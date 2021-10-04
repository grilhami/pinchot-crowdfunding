import           Control.Applicative  (Applicative (pure))
import           Data.Maybe           (catMaybes)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Default         (Default (def))
import           Control.Monad        (void)

import           Ledger               (Address, ScriptContext, POSIXTime, PubKeyHash, pubKeyHash, Datum (Datum), TxOutRef)
import qualified Ledger
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         (Value)
import           Ledger.Tx            (ChainIndexTxOut (..))
import qualified Ledger.Interval      as Interval
import qualified Ledger.TimeSlot      as TimeSlot
import           Playground.Contract
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Applicative (..))
import qualified Plutus.Contract.Typed.Tx as Typed
import qualified Prelude              as Haskell

-- | Campaign Datum that will be included in the transaction
data CampaignDatum = 
            CampaignDatum 
            {
                campaignId :: Integer
            ,   campaignDeadline :: POSIXTime
            ,   campaignOwner :: PubKeyHash
            ,   campaignGoal :: Value
            } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''CampaignDatum
PlutusTx.makeLift ''CampaignDatum

-- | Campaign params for "create campaign" endpoint
data CampaignParams = 
                CampaignParams 
                {
                    uniqueId :: Integer
                ,    goal :: Value
                ,   deadline :: POSIXTime
                ,   stake :: Value
                } deriving stock (Haskell.Eq, Haskell.Show, Generic)
                  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Contribute params for "contribute" endpoint
data ContributeParams = 
                ContributeParams 
                {
                    campaignChoiceId :: Integer
                ,   value :: Value
                } deriving stock (Haskell.Eq, Haskell.Show, Generic)
                  deriving anyclass (FromJSON, ToJSON, ToSchema)
-- | Crowdfunding validator type
data Crowdfunding
instance Scripts.ValidatorTypes Crowdfunding where
    type instance RedeemerType Crowdfunding = ()
    type instance DatumType Crowdfunding = CampaignDatum

-- | Compile validator
mkValidator :: Scripts.TypedValidator Crowdfunding
mkValidator = Scripts.mkTypedValidator @Crowdfunding
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @CampaignDatum

-- | Validator to check if transaction is submitted after campaign deadline (Refund and Withdrawal)

validate :: CampaignDatum -> () -> ScriptContext -> Bool
validate cmp _ sc = 
        (Interval.from (campaignDeadline cmp) `Interval.contains` (Ledger.txInfoValidRange (Ledger.scriptContextTxInfo sc)))



-- | The address of the contract (the hash of its validator script).
contractAddress :: Address
contractAddress = Scripts.validatorAddress mkValidator

-- | Schema for endpoints: "create campaign" and "contribute"
type CrowdfundingSchema = 
            Endpoint "create campaign" CampaignParams
            .\/ Endpoint "contribute" ContributeParams

contract :: AsContractError e => Contract () CrowdfundingSchema e ()
contract = selectList [createCampaign, contribute]

-- | Implement "create campaign" endpoint
createCampaign :: AsContractError e => Promise () CrowdfundingSchema e ()
createCampaign = endpoint @"create campaign" $ \CampaignParams{..} -> do
    ckh <- pubKeyHash <$> ownPubKey

    let c = CampaignDatum 
            {
                campaignId = uniqueId,
                campaignGoal = goal,
                campaignDeadline = (TimeSlot.scSlotZeroTime def) + deadline,
                campaignOwner = ckh
            }
        tx = Constraints.mustPayToTheScript c stake
        
    -- | Submit Campaign as transaction 
    txid <- fmap Ledger.txId (submitTxConstraints mkValidator tx)

    logInfo @Haskell.String "WAITING FOR THE CAMPAIGN DEADLINE ..."

    utxo <- watchAddressUntilTime contractAddress ((campaignDeadline c) + 1)

    logInfo @Haskell.String "CAMPAIGN DEALINE PASSED, PROCEED WITHDRAWAL"

    let flt Ledger.TxOutRef{txOutRefId} _ = txid Haskell.== txOutRefId
        tx' = Typed.collectFromScriptFilter flt utxo ()
                <> Constraints.mustValidateIn (Interval.from (campaignDeadline c))
                <> Constraints.mustBeSignedBy ckh

    logInfo @Haskell.String "WITHDRAWING"
    
    if Constraints.modifiesUtxoSet tx'
    then void (submitTxConstraintsSpending mkValidator utxo tx')
    else pure () 

contribute :: AsContractError e => Promise () CrowdfundingSchema e ()
contribute = endpoint @"contribute" $ \ContributeParams{..} -> do
    contributor <- pubKeyHash <$> ownPubKey
    utxos <- utxosAt contractAddress

    let os = findCampaign utxos
        cmp = [x | x <- os, (campaignId x) == campaignChoiceId]!!0
        tx = Constraints.mustPayToTheScript cmp value
                <> Constraints.mustValidateIn (Interval.to (campaignDeadline cmp))
    void $ submitTxConstraints mkValidator tx

    
findCampaign :: Map TxOutRef ChainIndexTxOut -> [CampaignDatum]
findCampaign = 
    catMaybes . Map.elems . Map.map getCampaign 

getCampaign :: ChainIndexTxOut -> Maybe CampaignDatum
getCampaign o = do
    Datum d <- either (const Nothing) Just (_ciTxOutDatum o)
    PlutusTx.fromBuiltinData d




-- | The "publish" contract endpoint.
-- publish :: AsContractError e => Promise () Schema e ()
-- publish = endpoint @"publish" $ \(i, lockedFunds) -> do
--     let tx = Constraints.mustPayToTheScript (MyDatum i) lockedFunds
--     void $ submitTxConstraints starterInstance tx

-- | The "redeem" contract endpoint.
-- redeem :: AsContractError e => Promise () Schema e ()
-- redeem = endpoint @"redeem" $ \myRedeemerValue -> do
--     unspentOutputs <- utxosAt contractAddress
--     let redeemer = MyRedeemer myRedeemerValue
--         tx       = collectFromScript unspentOutputs redeemer
--     void $ submitTxConstraintsSpending starterInstance unspentOutputs tx

endpoints :: AsContractError e => Contract () CrowdfundingSchema e ()
endpoints = contract

mkSchemaDefinitions ''CrowdfundingSchema

$(mkKnownCurrencies [])
