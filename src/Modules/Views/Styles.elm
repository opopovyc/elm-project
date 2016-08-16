module Modules.Views.Styles exposing(styles)

styles : String
styles = 
    """#container { display: flex; }
      
      .contentContainer {
        background: #efefef;
        padding: 20px;
        max-width: 350px;
        min-width: 150px;
        margin: 15vh auto;
        border-radius: 10px;
        border: solid 5px #dbdbdb;
      }

      .progressBar {
        margin-bottom: 26px;
        margin-bottom: 1.66em;
      }

      .progressBar h4 {
        font-size: 21px;
        font-size: 1.33em;
        text-transform: none;
        font-family: Arial, Helvetica, sans-serif;
        font-weight: bold;
        margin-bottom: 7px;
        margin-bottom: .33em;
      }

      .progressBarContainer {
        width: 100%;
        max-width: 350px;
        height: 26px;
        height: 1.66em;
        background: #e6eae3;
        background: rgba(8,102,220,.2);
        overflow: hidden;
        border-radius: 5px;
      }

      .progressBarValue {
        height: 1.66em;
        float: left;
        background: #0866dc;
        background: rgba(8,102,220,.75);
      }
    """