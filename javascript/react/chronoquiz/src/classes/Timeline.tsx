interface TimelineInput {
  id: number;
  title: string;
  description: string;
  keywords: Array<string>;
  creator: string;
  facts: FactList;
}

export default class Timeline {
  id: number;
  title: string;
  description: string;
  keywords: Array<string>;
  creator: string;
  facts: Array<Fact>;

  constructor({ 
    id = -1,
    title = "", 
    description = "", 
    keywords = [], 
    creator = "", 
    facts = []
  }: TimelineInput = {}) {
    this.id = id;
    this.title = title;
    this.description = description;
    this.keywords = keywords;
    this.creator = creator;
    this.facts = facts;
  }

  static parseKeywords(inputStr: string = ""): Array<string> {
    if (inputStr) {
      return inputStr.split(";").map(s => s.trim());
    } else {
      return [];
    }
  }

  get keywordString() {
    return this.keywords.join("; ");
  }

  static newFromKeywordString({ id, title, description, keywords, creator, facts }) {
    return new Timeline({
      id: id,
      title: title,
      description: description,
      keywords: keywords ? Timeline.parseKeywords(keywords) : [],
      creator: creator,
      facts: facts
    });
  }

  json() {
    return JSON.stringify({
      id: this.id,
      title: this.title,
      description: this.description,
      keywords: this.keywordString,
      creator: this.creator,
      facts: this.facts.map(e => e.json())
     });
  }

  sortByDate() {
    this.facts.sort((c1, c2) => c1.date - c2.date);
    return this;
  }

  removeFact(fact) {
    let filtered = this.facts.filter(f => f !== fact);
    return new Timeline({
      ...this,
      facts: filtered
    });
  }
}


