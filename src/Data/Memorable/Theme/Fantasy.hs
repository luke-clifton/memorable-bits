{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Data.Memorable.Theme.Fantasy where

import Data.Memorable

type TransitiveVerb = ToTree
	'[ "blending"
	 , "breaking"
	 , "bullying"
	 , "burning"
	 , "charming"
	 , "dominating"
	 , "fighting"
	 , "freezing"
	 , "growing"
	 , "kicking"
	 , "killing"
	 , "liquifying"
	 , "loving"
	 , "melting"
	 , "paralyzing"
	 , "peeling"
	 , "petrifying"
	 , "piercing"
	 , "pinching"
	 , "poking"
	 , "punching"
	 , "singeing"
	 , "slicing"
	 , "splattering"
	 , "squishing"
	 , "tearing"
	 , "teasing"
	 , "throwing"
	 , "tormenting"
	 , "tossing"
	 , "wooing"
	 , "zapping"
	 ]

type MedievalWeapons = ToTree '["axe", "bow", "sword", "dagger"]

type MedievalArmour = ToTree
	'[ "helm"
	 , "boots"
	 , "vest"
	 , "gauntlets"
	 , "great helm"
	 , "cuirasse"
	 , "plackard"
	 , "pants"
	 , "shirt"
	 , "greaves"
	 , "hat"
	 , "leggings"
	 , "robes"
	 , "sandals"
	 , "amulet"
	 , "ring"
	 ]

type Aura = ToTree '["unholy", "holy", "sacred", "vile"]

type Monster = ToTree '["orc", "goblin", "gnoll", "troll", "vampire", "zombie", "bugbear", "dragon"]

type ArmourMaterial = ToTree
	'[ "bronze"
	 , "cloth"
	 , "golden"
	 , "iron"
	 , "leather"
	 , "silk"
	 , "studded leather"
	 , "wooden"
	 , "crystal"
	 , "mythril"
	 , "bone"
	 , "stone"
	 , "silver"
	 , "glass"
	 , "paper"
	 , "fur"
	 ]

type Buff = ToTree
	'[ "climbing"
	 , "constitution"
	 , "dexterity"
	 , "fear"
	 , "hope"
	 , "paralysis"
	 , "swimming"
	 , "wisdom"
	 ]

type RpgWeapons = MedievalWeapons :<> " of " :<> Monster :<> " " :<> TransitiveVerb

type RpgArmour = ArmourMaterial :<> " " :<> MedievalArmour :<> " of " :<> Buff

type RpgThings
	= Aura
	:<> " "
	:<> (RpgWeapons :<|> RpgArmour)

rpgThings :: Proxy RpgThings
rpgThings = Proxy
