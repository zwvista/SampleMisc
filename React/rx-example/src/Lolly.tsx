import {createSlice} from "@reduxjs/toolkit";
import * as React from "react";
import {useDispatch, useSelector} from "react-redux";
import {useEffect} from "react";

function LangList(props: any) {
  return (
    <table className="table table-striped table-bordered">
      <tbody>
      <tr>
        <th>id</th>
        <th>language</th>
      </tr>
      {
        props.langList.map((lang: any) =>
          <tr key={lang.ID}>
            <td>{lang.ID}</td>
            <td>{lang.NAME}</td>
          </tr>
        )
      }
      </tbody>
    </table>
  )
}

const initialState = {
  langList: [],
  dictList: [],
  selectedLang: undefined,
  selectedDict: undefined,
  word: '一人',
  redirectSearch: false,
  dictUrl: 'about:blank',
}

const lollySlice = createSlice({
  name: 'lolly',
  initialState,
  reducers: {
    setLangList: (state, {payload}) => { state.langList = payload; },
    setDictList: (state, {payload}) => { state.dictList = payload; },
    setSelectedLang: (state, {payload}) => { state.selectedLang = payload; },
    setSelectedDict: (state, {payload}) => { state.selectedDict = payload; },
    setWord: (state, {payload}) => { state.word = payload; },
    setRedirectSearch: (state, {payload}) => { state.redirectSearch = payload; },
    setDictUrl: (state, {payload}) => { state.dictUrl = payload; },
  },
});
const {setLangList, setDictList, setSelectedLang, setSelectedDict, setWord, setRedirectSearch, setDictUrl} = lollySlice.actions;
const lollySelector = (state: any) => state.lolly;
export const lollyReducer = lollySlice.reducer;
function langChange(lang: any) {
  return async (dispatch: any) => {
    dispatch(setSelectedLang(lang));
    const response = await fetch('https://zwvista.com/lolly/api.php/records/DICTIONARIES?filter=LANGIDFROM,eq,' + JSON.parse(lang).ID);
    const data = await response.json();
    dispatch(setDictList(data.records));
    dispatch(setSelectedDict(JSON.stringify(data.records[0])));
  }
}
function init() {
  return async (dispatch: any) => {
    const response = await fetch('https://zwvista.com/lolly/api.php/records/LANGUAGES?filter=ID,neq,0');
    const data = await response.json();
    dispatch(setLangList(data.records));
    dispatch(langChange(JSON.stringify(data.records[0])));
  }
}

function Lolly() {
  const dispatch = useDispatch()
  const { langList, dictList, selectedLang, selectedDict, word, redirectSearch, dictUrl } = useSelector(lollySelector)

  function formSubmit(event: any) {
    event.preventDefault();
    let url = JSON.parse(selectedDict).URL;
    url = url.replace('{0}', encodeURIComponent(word));
    if (redirectSearch)
      window.location = url;
    else
      dispatch(setDictUrl(url));
  }

  useEffect(() => {
    dispatch(init());
  }, [dispatch]);

  return (
    <div>
      <LangList langList={langList} />
      <form className="form-horizontal" onSubmit={formSubmit} action="search" method="post">
        <div className="form-group">
          <label className="col-sm-1 control-label" htmlFor='lang'>Language:</label>
          <div className="col-sm-3">
            <select className="form-control" onChange={event => dispatch(langChange(event.target.value))} value={selectedLang} id='lang' name='selectedLang'>
              {
                langList.map((lang: any) =>
                  <option key={lang.ID} value={JSON.stringify(lang)}>{lang.NAME}</option>
                )
              }
            </select>
          </div>
          <label className="col-sm-1 control-label" htmlFor='dict'>Dictionary:</label>
          <div className="col-sm-3">
            <select className="form-control" onChange={event => dispatch(setSelectedDict(event.target.value))} value={selectedDict} id='dict' name='selectedDict'>
              {
                dictList.map((dict: any) =>
                  <option key={dict.ID} value={JSON.stringify(dict)}>{dict.NAME}</option>
                )
              }
            </select>
          </div>
        </div>
        <div className="form-group">
          <label className="col-sm-1 control-label" htmlFor='word'>Word:</label>
          <div className="col-sm-3">
            <input className="form-control" value={word} onChange={event => dispatch(setWord(event.target.value))} id="word" name='word' />
          </div>
          <input type="submit" className="btn btn-primary" value='Search' id='search' onClick={() => dispatch(setRedirectSearch(false))} />
          <input type="submit" className="btn btn-primary" value='Search(redirect)' id='redirectSearch' onClick={() => dispatch(setRedirectSearch(true))} />
        </div>
      </form>
      <iframe id='dictframe' src={dictUrl} />
    </div>
  );
}

export default Lolly;
