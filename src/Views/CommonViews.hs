{-
  Copyright © 2022 Josep Bigorra

  This file is part of Tablatura.
  Tablatura is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation,
    either version 3 of the License, or (at your option) any later version.

  Tablatura is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  See the GNU General Public License for more details.
  You should have received a copy of the GNU General Public License along with Foobar.
  If not, see <https://www.gnu.org/licenses/>. 
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Views.CommonViews where

import Import

baseInputStyle :: Text
baseInputStyle = "width-max-mobile width-three-quarters padding-medium"

codeInputStyle :: Text
codeInputStyle = baseInputStyle ++ " monospace"

baseTextAreaStyle :: Text
baseTextAreaStyle = "width-max min-height-large padding-small"

codeTextAreaStyle :: Text
codeTextAreaStyle = baseTextAreaStyle ++ " monospace"

emptyRequiredInputField :: MForm Handler (FormResult Text, FieldView App)
emptyRequiredInputField = requiredInputField Nothing

requiredInputField :: Maybe Text -> MForm Handler (FormResult Text, FieldView App)
requiredInputField =
  mreq
    textField
    FieldSettings
      { fsLabel = "",
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Nothing,
        fsAttrs = [("class", baseInputStyle)]
      }

emptyOptionalInputField :: MForm Handler (FormResult (Maybe Text), FieldView App)
emptyOptionalInputField = optionalInputField Nothing

optionalInputField :: Maybe (Maybe Text) -> MForm Handler (FormResult (Maybe Text), FieldView App)
optionalInputField =
  mopt
    textField
    FieldSettings
      { fsLabel = "",
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Nothing,
        fsAttrs = [("class", baseInputStyle)]
      }



emptyRequiredCodeInputField :: MForm Handler (FormResult Text, FieldView App)
emptyRequiredCodeInputField = requiredCodeInputField Nothing

requiredCodeInputField :: Maybe Text -> MForm Handler (FormResult Text, FieldView App)
requiredCodeInputField =
  mreq
    textField
    FieldSettings
      { fsLabel = "",
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Nothing,
        fsAttrs = [("class", codeInputStyle)]
      }

emptyOptionalCodeInputField :: MForm Handler (FormResult (Maybe Text), FieldView App)
emptyOptionalCodeInputField = optionalCodeInputField Nothing

optionalCodeInputField :: Maybe (Maybe Text) -> MForm Handler (FormResult (Maybe Text), FieldView App)
optionalCodeInputField =
  mopt
    textField
    FieldSettings
      { fsLabel = "",
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Nothing,
        fsAttrs = [("class", codeInputStyle)]
      }

emptyOptionalImageFileField :: MForm Handler (FormResult (Maybe FileInfo), FieldView App)
emptyOptionalImageFileField = optionalImageFileField Nothing

optionalImageFileField :: Maybe (Maybe FileInfo) -> MForm Handler (FormResult (Maybe FileInfo), FieldView App)
optionalImageFileField =
  mopt
    fileField
    FieldSettings
      { fsLabel = "",
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Nothing,
        fsAttrs = [("class", baseInputStyle), ("accept", "image/webp, image/png, image/jpeg, image/jpg, image/gif")]
      }

emptyOptionalFileField :: MForm Handler (FormResult (Maybe FileInfo), FieldView App)
emptyOptionalFileField = optionalFileField Nothing

optionalFileField :: Maybe (Maybe FileInfo) -> MForm Handler (FormResult (Maybe FileInfo), FieldView App)
optionalFileField =
  mopt
    fileField
    FieldSettings
      { fsLabel = "",
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Nothing,
        fsAttrs = [("class", baseInputStyle)]
      }

emptyRequiredTextareaField :: MForm Handler (FormResult Textarea, FieldView App)
emptyRequiredTextareaField = requiredTextareaField Nothing

requiredTextareaField :: Maybe Textarea -> MForm Handler (FormResult Textarea, FieldView App)
requiredTextareaField =
  mreq
    textareaField
    FieldSettings
      { fsLabel = "",
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Nothing,
        fsAttrs = [("class", baseTextAreaStyle)]
      }

emptyRequiredCodeTextareaField :: MForm Handler (FormResult Textarea, FieldView App)
emptyRequiredCodeTextareaField = requiredCodeTextareaField Nothing

requiredCodeTextareaField :: Maybe Textarea -> MForm Handler (FormResult Textarea, FieldView App)
requiredCodeTextareaField =
  mreq
    textareaField
    FieldSettings
      { fsLabel = "",
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Nothing,
        fsAttrs = [("class", codeTextAreaStyle)]
      }

emptyOptionalTextareaField :: MForm Handler (FormResult (Maybe Textarea), FieldView App)
emptyOptionalTextareaField = optionalTextareaField Nothing

optionalTextareaField :: Maybe (Maybe Textarea) -> MForm Handler (FormResult (Maybe Textarea), FieldView App)
optionalTextareaField =
  mopt
    textareaField
    FieldSettings
      { fsLabel = "",
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Nothing,
        fsAttrs = [("class", baseTextAreaStyle)]
      }

emptyOptionalCodeTextareaField :: MForm Handler (FormResult (Maybe Textarea), FieldView App)
emptyOptionalCodeTextareaField = optionalCodeTextareaField Nothing

optionalCodeTextareaField :: Maybe (Maybe Textarea) -> MForm Handler (FormResult (Maybe Textarea), FieldView App)
optionalCodeTextareaField =
  mopt
    textareaField
    FieldSettings
      { fsLabel = "",
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Nothing,
        fsAttrs = [("class", codeTextAreaStyle)]
      }

emptyRequiredPositiveIntField :: MForm Handler (FormResult Int, FieldView App)
emptyRequiredPositiveIntField = requiredPositiveIntField Nothing

requiredPositiveIntField :: Maybe Int -> MForm Handler (FormResult Int, FieldView App)
requiredPositiveIntField =
  mreq
    intField
    FieldSettings
      { fsLabel = "",
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Nothing,
        fsAttrs =
          [ ("class", baseInputStyle),
            ("min", "1")
          ]
      }
