package main

import (
	"fmt"
	"encoding/json"
	"github.com/nicksnyder/go-i18n/v2/i18n"
	"golang.org/x/text/language"
)

func main() {
	bundle := i18n.NewBundle(language.English)
	bundle.RegisterUnmarshalFunc("json", json.Unmarshal)
	bundle.LoadMessageFile("active.en.json")
	bundle.LoadMessageFile("active.fi.json")
	localizer := i18n.NewLocalizer(bundle, "fi")

  fmt.Println(
		localizer.MustLocalize(&i18n.LocalizeConfig{
			DefaultMessage: &i18n.Message{
				ID: "HelloWorld",
				Other: "Hello World"},
		}))

	fmt.Println(
		localizer.MustLocalize(&i18n.LocalizeConfig{
			DefaultMessage: &i18n.Message{
				ID: "HelloName",
				Other: "Hello {{.name}}!",
			},
			TemplateData: map[string]interface{}{
				"name": "John",
			},
		}))

  fmt.Println(
		localizer.Localize(&i18n.LocalizeConfig{
			DefaultMessage: &i18n.Message{
				ID: "Skis",
				One: "I have {{.ski}} ski.",
				Other: "I have {{.ski}} skis.",
			},
			TemplateData: map[string]interface{}{
				"ski": 2,
			},
			PluralCount: 2,
	}))
}