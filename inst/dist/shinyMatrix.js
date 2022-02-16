function toTSV (d) {
    let data = _.cloneDeep(d);

    let values = data.values;
    values.unshift(data.colnames);

    let lines = _.map(values, o => o.join("\t"))

    data.rownames.unshift('');
    lines = _.zip(data.rownames, lines);

    lines = _.map(lines, o => o.join("\t"))

    return lines.join("\n");
}