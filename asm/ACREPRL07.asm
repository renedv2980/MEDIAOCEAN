*          DATA SET ACREPRL07  AT LEVEL 112 AS OF 08/12/20                      
*PHASE ACRL07C                                                                  
*                                                                               
* PID  LVl DATE    COMMENTS                                                     
* ---------------------------------                                             
* SMAN 102 10NOV11 PR000122 Estimate Phase 1                                    
* SMAN 103 16DEC11 PR002242 UK/US merge                                         
* YNGX 107 30SEP14 PCA01185 RELINK TO INCLUDE NEW ACREPRLWRK                    
* GHOA 109 30SEP18          RELINK TO INCLUDE NEW ACREPRLWRK                    
* GHOA 110 30SEP18 SPC33734 RELINK TO INCLUDE NEW ACREPRLWRK                    
* VGUP 111 02APR19 SPEC-43802 Relink due to new ACREPRLWRK                      
* GHOA 113 23Jul20 SPEC-46231 Relink due to new ACREPRLWRK                      
*                                                                               
ACRL07   CSECT                                                                  
         TITLE 'Print out specs of format'                                      
         PRINT NOGEN                                                            
         USING ACWORKD,RC          Map   ACWORKD   dsect                        
         USING ACRLD,RA            Map   Scribe    work area                    
         USING LWSD,R8             Map   Local     work area                    
         USING BOXD,R6             Map   box  definitions                       
         USING BIGPRNTD,R4         Map   wide print     areas                   
         NMOD1 LWSDX,**SPEC**,R9                                                
*                                                                               
         LR    R8,RC               ->    Local     work area                    
         L     RC,RLBASEC          ->    ACWORKD   dsect                        
         L     R4,VBIGPRNT         ->    wide print     areas                   
         L     R6,ADBXAREA         ->    box  work area                         
         ST    R5,RPT@FMTR         Save  addr of   format    record             
*                                                                               
         CLI   QOPT1,C' '          Down-load  requested ?                       
         BE    START10             No,   continue                               
         CLI   QOPT8,ONLY          Only  format    report ?                     
         BNE   EXIT                No,   return                                 
*                                                                               
START10  BAS   RE,FRMTINIT         Format     initialization                    
         BAS   RE,PHEAD            Process    headers                           
         BAS   RE,PROWS            Process    rows                              
         BAS   RE,PCOLS            Process    columns                           
         BAS   RE,PGPROF           Process    general   profiles                
         BAS   RE,PRPROF           Process    report    profiles                
*                                                                               
         MVC   XHEAD1(L'SPACES),SPACES   Clear XHEAD1-3  areas                  
         MVC   XHEAD2(L'SPACES),SPACES                                          
         MVC   XHEAD3(L'SPACES),SPACES                                          
         MVI   RCSUBPRG,0                Clear subprogram number                
         NI    BOXDDCTL,TURNOFF-BOXDDLC  Clear lower case chararcters           
*                                                                               
EXIT     XIT1  ,                   Return to caller                             
*                                                                               
EXITR7   XIT1  REGS=(R7)           Return R7 to caller                          
         DROP  R4,R6                                                            
         EJECT ,                                                                
***********************************************************************         
*  Format initialization                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING BOXD,R6             Map   box  definitions                       
         SPACE 1                                                                
FRMTINIT NTR1  ,                   Format     initialization                    
         L     R6,ADBXAREA         ->    box  work area                         
         L     RF,=A(BASESTAB)     ->    Base addresses table                   
         MVC   ABASES,0(RF)        Copy  base addresses to   LWSD               
*                                                                               
*                                  Translate  dictionary     list               
*                                        using     lower     case               
         GOTO1 ADDICTAT,DMCB,C'LL  ',A(DICI),DICO                               
         GOTO1 ADDICTAT,DMCB,C'LU  ',A(DICIU),DICOU                             
*                                                                               
         MVI   COMMA,C','          Comma                                        
         CLI   RCLANG,LANGGER      German ?                                     
         BNE   *+8                 No,   skip                                   
         MVI   COMMA,C'#'          Use   C'#'                                   
*                                                                               
         MVI   RPTTYPES,0          Clear report    types     found              
         MVI   RPTSWS,0            Clear switches                               
*                                                                               
         L     RF,HEADHOOK         ->    BXHOOK                                 
         BASR  RE,RF               Call  BXHOOK                                 
*                                                                               
         MVI   BOXPGDF#,1          Page  def  #    1                            
         MVI   BOXFONT,0           Pitch 10                                     
         MVI   MAXLINES,LPI08LS    Landscape  8    lines     per  inch          
         MVI   RPTPGWID,PGWDRL10   Page  width     for  pitch     10            
         CLI   QPROG+1,PORTRAIT    Portrait   printing ?                        
         BNE   FRMT10              No,   skip                                   
         MVI   MAXLINES,LPI08PT    Portrait   8    lines     per  inch          
         MVI   RPTPGWID,PGWDRP10   Page  width     for  pitch     10            
*                                                                               
FRMT10   DS    0H                                                               
         OI    BOXDDCTL,BOXDDLC    Lower case chararcters                       
*                                                                               
*                                  ***** Get   report    profile  *****         
*                                  *****       DD   number        *****         
         L     R1,=A(PROFNTAB)     ->    profile   name table                   
*                                                                               
FRMT20   CLI   0(R1),EOT           End   of   table ?                           
         BNE   *+6                 No,   continue                               
         DC    H'00'               Error                                        
*                                                                               
         CLC   2(1,R1),QPROG       Same  report    type ?                       
         BE    FRMT30              Yes,  continue                               
         LA    R1,PROFNTBQ(,R1)    Find  next entry                             
         B     FRMT20              Test  next entry                             
*                                                                               
FRMT30   MVC   RPTPRF#,0(R1)       Save  report    profile   DD   num           
         MVI   RPTBXTYP,RPTBNONE   Say   not  in   a    box                     
         GOTO1 =A(NEWPG)           Force new  page                              
         DROP  R6                                                               
*                                  ***** determine if   we   have *****         
*                                  *****      any C1,  C2,  C3   *****          
*                                  *****      elements           *****          
         SR    R3,R3                                                            
         L     R2,AIO2             ->    record                                 
         AH    R2,DATADISP         ->    1st  element                           
*                                                                               
FRMT40   DS    0H                  Next  element                                
         CLI   0(R2),0             End   of   record ?                          
         BE    FRMTXIT                                                          
         CLI   0(R2),RHDELQ        X'C1' header    element ?                    
         BNE   *+8                                                              
         OI    RPTTYPES,RPTHEAD                                                 
         CLI   0(R2),RRWELQ        X'C2' row       element ?                    
         BNE   *+8                                                              
         OI    RPTTYPES,RPTROWS                                                 
         CLI   0(R2),RCLELQ        X'C3' column    element ?                    
         BNE   *+8                                                              
         OI    RPTTYPES,RPTCOLS                                                 
         IC    R3,1(,R2)           Get   next element                           
         AR    R2,R3                                                            
         B     FRMT40                                                           
*                                                                               
FRMTXIT  B     EXIT                Return     to   caller                       
         EJECT ,                                                                
***********************************************************************         
*  Process Headers                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R4         Map   wide print     areas                   
PHEAD    NTR1  ,                   Process    headers                           
         TM    RPTTYPES,RPTHEAD    Any   headers ?                              
         BZ    PHEADEX             No,   exit headers                           
         MVC   RPTSCRNN,SPACES     Clear the  screen    name                    
         MVC   RPTSCRNN(L'$HEADS),$HEADS     "HEADERS"                          
         GOTO1 =A(PRTSCRNN)        Print the  screen    name                    
*                                                                               
         MVI   RPTBXTYP,RPTBHEAD   Open  a    headers   box                     
         GOTO1 =A(OBOXTYPE)        Open  box  based     on   box  type          
*                                                                               
         USING BXHEAD,R3           Map   headings  print     line               
         LA    R3,XP                                                            
*                                                                               
         USING RHDELD,R2           Map   headings  data element                 
         L     R2,AIO2             ->    record                                 
         AH    R2,DATADISP         ->    1st  element                           
*                                  ************************************         
PHEAD10  DS    0H                  *     Process   header    element  *         
*                                  ************************************         
         CLI   0(R2),0             End   of   record ?                          
         BE    PHEAD70             Yes,  End  heading   el   report             
         CLI   0(R2),RHDELQ        X'C1' Heading   element ?                    
         BNE   PHEADNXT            No,   get  next element                      
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
*                                  ***** Type of   header:        *****         
         LA    R7,BXHTYPE          ->    boxes     type area for  hdrs          
         CLI   RHDTYPE,RHDTITL     Title ?                                      
         BNE   *+10                No,   skip                                   
         MVC   0(L'@TITLE,R7),@TITLE     "Title"                                
*                                                                               
         CLI   RHDTYPE,RHDCNTR     Center     head-line ?                       
         BNE   *+10                No,   skip                                   
         MVC   0(L'@CENTR,R7),@CENTR     "Center"                               
*                                                                               
         CLI   RHDTYPE,RHDLFTH     Left  head-line ?                            
         BNE   *+10                No,   skip                                   
         MVC   0(L'@LEFT,R7),@LEFT       "Left"                                 
*                                                                               
         CLI   RHDTYPE,RHDRHTH     Right head-line ?                            
         BNE   *+10                No,   skip                                   
         MVC   0(L'@RIGHT,R7),@RIGHT     "Right"                                
*                                                                               
         CLI   RHDTYPE,RHDFTLN     Footline ?                                   
         BNE   *+10                No,   skip                                   
         MVC   0(L'@FTLN,R7),@FTLN       "Footline"                             
         LA    R1,L'BXHTYPE        Length     of   text                         
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
*                                                                               
*                                  ***** Number:                  *****         
         CLI   RHDTYPE,RHDTITL     Title ?                                      
         BE    PHEAD20             Yes,  skip                                   
         CLI   RHDSEQ,0            Any   sequence  number ?                     
         BE    PHEAD20             No,   skip                                   
         MVC   1(L'BXHSEQ#,R7),RHDSEQ    Print     sequence  number             
         OI    1(R7),X'F0'         Convert    BIN  TO   Display  (1-3)          
*                                                                               
*                                  ***** Keyword:                 *****         
PHEAD20  DS    0H                  Print keyword   or   text                    
         SR    RF,RF               Get   length    of   element                 
         IC    RF,RHDLN                                                         
         AHI   RF,-(RHDLNQ+1)      Get   length    of   data                    
         TM    RHDFRM,RHDFREE      Free  form data ?                            
         BO    PHEAD30             Yes,  insert    data                         
         TM    RHDFRM,RHDDICT      Need  to   translate ?                       
         BO    PHEAD40             Yes,  translate the  data                    
*                                                                               
PHEAD30  DS    0H                  Insert     the  data                         
         EXMVC RF,BXHKEYWD,RHDDATA                                              
         B     PHEAD50             Output     the  data                         
*                                                                               
PHEAD40  DS    0H                  Translate  the  data                         
         MVI   BXHKEYWD,C'&&'                 the  data                         
         MVI   BXHKEYWD+1,ESC#LFJT                                              
         MVC   BXHKEYWD+2(2),RHDDATA                                            
         MVI   BXHKEYWD+4,6                                                     
         LA    R7,BXHKEYWD+1       ->    keyword   area                         
         GOTO1 =A(TRANFREE)        Translate  and  find 1st  free byte          
*                                                                               
*                                  *---- Keyword   BLKn           ----*         
         CLC   =AL2(AC#RSBLK),RHDDATA    "BLK"n ?                               
         BNE   PHEAD50                   No,  output    the  keyword            
         MVC   0(1,R7),RHDXDATA    get   the  number                            
         OI    0(R7),X'F0'         Convert    BIN  TO   Display  (1-9))         
*                                                                               
*                                  ***** Print     the  data      *****         
PHEAD50  DS    0H                                                               
*                                  Make  sure line will fit                     
         GOTO1 =A(CHKNWRIT)              and  print     the  data               
*                                                                               
*                                  ***** Next element             *****         
PHEADNXT DS    0H                  Bump  to   next heading   element            
         SR    R1,R1                                                            
         IC    R1,RHDLN                                                         
         AR    R2,R1                                                            
         B     PHEAD10             Test  next element                           
         DROP  R2,R3                                                            
*                                                                               
PHEAD70  DS    0H                  End   the  headings  box                     
         GOTO1 =A(CLOSEBOX)        Close the  current   box                     
*                                                                               
PHEADEX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  Process rows                                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R4         Map   wide print     areas                   
PROWS    NTR1  ,                                                                
         TM    RPTTYPES,RPTROWS    Any   rows ?                                 
         BZ    PROWSEX             No,   exit rows                              
*                                  Force new  page if   the  following          
*                                        will not  fit:                         
*                                  1.    blank     line                         
*                                  2.    screen    name line                    
*                                  3.    box  title     line                    
*                                  4.    box  headers                           
*                                  5.    box  midline                           
*                                  6-9.  one  entry    (4    lines)             
*                                  10.   box  close     line                    
         MVI   RPTLINES,10         ..                                           
         GOTO1 =A(CHKSPACE)        Make  sure lines     will fit                
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
         TM    RPTSWS,RPTSNPED     Started    a    new  page ?                  
         BO    PROWS00             Yes,  skip                                   
         GOTO1 ACREPORT            Skip  a    line                              
*                                                                               
         USING BXROWD,R3           Map   rows print     line                    
PROWS00  DS    0H                                                               
         MVC   RPTSCRNN,SPACES     Clear the  screen    name                    
         MVC   RPTSCRNN(L'$ROWS),$ROWS   "ROWS"                                 
         GOTO1 =A(PRTSCRNN)        Print the  screen    name                    
*                                                                               
         LA    R3,XP                                                            
         LA    R7,1(,R3)                                                        
*                                                                               
         MVC   0(L'@ROWS,R7),@ROWS "Rows"                                       
         LA    R1,L'@ROWS          Length     of   text                         
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVC   1(6,R7),=C'(1-12)'                                               
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
         MVI   RPTBXTYP,RPTBROWS   Open  a    rows box                          
         GOTO1 =A(OBOXTYPE)        Open  box  based     on   box  type          
*                                                                               
         MVC   RPTRPG,=C' 0'       Init  new  page on   row  number             
*                                                                               
         L     R2,AIO2             ->    record                                 
*                                                                               
         USING RRWELD,R2           Map   rows data element                      
         AH    R2,DATADISP         ->    1st  element                           
         SPACE 1                                                                
*                                  ************************************         
PROWS10  DS    0H                  *     Process   row  element       *         
*                                  ************************************         
         CLI   0(R2),0             End   of   record ?                          
         BE    PROWS40             Yes,  end  rows processing                   
         CLI   0(R2),RRWELQ        X'C2',     row  element ?                    
         BNE   PROWSNXT            No,   get  next element                      
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
*                                  ***** Row  number:             *****         
         SR    R1,R1               Get   sequence  number                       
         IC    R1,RRWSEQ                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'         Clear the  sign                              
         UNPK  BXRNUM+1(L'BXRNUM-1),DUB                                         
         CLI   BXRNUM+1,C'0'       Remove     leading   zero if   any           
         BNE   *+8                                                              
         MVI   BXRNUM+1,C' '                                                    
*                                                                               
         TM    RRWOPT2,RRWPGNO     Start new  page on   this row ?              
         BZ    PROWS20             No,   skip                                   
         MVC   RPTRPG,BXRNUM+1     Save  the  row  number                       
         MVI   BXRNUM,C'*'         Flag  this row                               
*                                                                               
PROWS20  DS    0H                  ***** Keyword:                 *****         
         GOTO1 GETKEYWD,(R2)       Get   keyword                                
*                                                                               
*                                  ***** Attributes:              *****         
         BAS   RE,PROWSATT         Get   Attributes                             
*                                                                               
         SR    RF,RF               ***** Prefix:                  *****         
         IC    RF,RRWDATLN         Get   length    of   RRWNDATA                
         LA    RE,RRWNDATA(RF)     ->    start     of   prefix                  
         ICM   RF,1,RRWPFXLN       Get   length    of   prefix                  
         BZ    PROWS30             Zero, no   prefix    attached                
         CHI   RF,L'BXRPRFX        Will  it   fit ?                             
         BNH   *+6                 Yes,  continue                               
         DC    H'00'               Prefix     too  big  for  field              
*                                                                               
         BCTR  RF,0                Minus one  for  execute                      
         EXMVC RF,BXRPRFX,0(RE)    Insert     prefix                            
*                                                                               
PROWS30  DS    0H                  ***** Type:                    *****         
         CLI   RRWTYPE,RRWMID      Mid-line ?                                   
         BNE   *+10                                                             
         MVC   BXRTYPE(L'@MIDS),@MIDS    "Mid-line"                             
*                                                                               
         CLI   RRWTYPE,RRWLHEAD    Left      row  heading ?                     
         BNE   *+10                                                             
         MVC   BXRTYPE(L'@LEFT),@LEFT    "Left"                                 
*                                                                               
         CLI   RRWTYPE,RRWRHEAD    Right     row  heading ?                     
         BNE   *+10                                                             
         MVC   BXRTYPE(L'@RIGHT),@RIGHT  "Right"                                
*                                                                               
         CLI   RRWTYPE,RRWCHEAD    Center    row  heading ?                     
         BNE   *+10                                                             
         MVC   BXRTYPE(L'@CENTR),@CENTR  "Center"                               
*                                                                               
*                                  ***** Totals:                  *****         
         MVC   BXRTOTL(L'@NO),@NO  Default   to   "No"                          
         TM    RRWOPT,RRWTOT       Any   totals ?                               
         BZ    *+16                                                             
         MVC   BXRTOTL(L'@NO),SPACES     Clear    default                       
         MVC   BXRTOTL(L'@YES),@YES      "Yes"                                  
*                                                                               
         TM    RRWOPT,RRWTOTSP     Separate  page ?                             
         BZ    *+10                                                             
         MVC   BXRTOTL(L'@SPRT),@SPRT    "Separate"                             
*                                                                               
         TM    RRWOPT2,RRWBTM      Bottom    of   page ?                        
         BZ    *+10                                                             
         MVC   BXRTOTL(L'@CBOTT),@CBOTT  "Bottom"                               
*                                                                               
*                                  ***** Print     the  data      *****         
*                                  Make  sure line(s)   will fit                
         GOTO1 =A(CHKNWRIT)              and  print     the  data               
*                                                                               
*                                  ***** Next element             *****         
PROWSNXT DS    0H                  Bump  to   next rows element                 
         SR    R1,R1                                                            
         IC    R1,RRWLN                                                         
         AR    R2,R1                                                            
         B     PROWS10             Test  next element                           
         DROP  R2,R3                                                            
*                                                                               
PROWS40  DS    0H                  End   the  rows box                          
         GOTO1 =A(CLOSEBOX)        Close the  current   box                     
*                                                                               
         MVI   RPTLINES,2          Say   print     two  lines                   
         GOTO1 =A(CHKSPACE)        Make  sure lines     will fit                
         MVI   RPTLINES,1          Say   print     one  lines                   
*                                                                               
         TM    RPTSWS,RPTSNPED     Started    a    new  page ?                  
         BO    PROWS50             Yes,  skip                                   
         GOTO1 ACREPORT            Skip  a    line                              
*                                                                               
PROWS50  DS    0H                  "Start     new  page number  on row"         
         GOTO1 =A(TXTGET),DMCB,5664,(40,XP+1),(L'RPTRPG,RPTRPG),0               
         GOTO1 ACREPORT            Print the  line                              
*                                                                               
PROWSEX  DS    0H                  Exit                                         
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  Process columns                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R4         Map   wide print     areas                   
         SPACE 1                                                                
PCOLS    NTR1  ,                                                                
         TM    RPTTYPES,RPTCOLS    Any   columns ?                              
         BZ    PCOLSEX             No,   exit columns                           
*                                  Force new  page if   the  following          
*                                        will not  fit:                         
*                                  1.    blank     line                         
*                                  2.    screen    name line                    
*                                  3.    box  title     line                    
*                                  4.    box  headers                           
*                                  5.    box  midline                           
*                                  6-9.  one  entry    (4    lines)             
*                                  10.   box  close     line                    
         MVI   RPTLINES,10         ..                                           
         GOTO1 =A(CHKSPACE)        Make  sure lines     will fit                
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
         TM    RPTSWS,RPTSNPED     Started    a    new  page ?                  
         BO    PCOLS00             Yes,  skip                                   
         GOTO1 ACREPORT            Skip  a    line                              
*                                                                               
         USING BXCOLD,R3           Map   columns   print     line               
PCOLS00  DS    0H                                                               
         LA    R3,XP                                                            
         LA    R7,1(,R3)                                                        
*                                                                               
         MVC   RPTSCRNN,SPACES     Clear the  screen    name                    
         MVC   RPTSCRNN(L'$COLS),$COLS   "COLUMNS"                              
         GOTO1 =A(PRTSCRNN)        Print the  screen    name                    
*                                                                               
         MVC   0(L'@COLS,R7),@COLS "Columns"                                    
         LA    R1,L'@COLS          Length     of   text                         
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVC   1(6,R7),=C'(1-48)'                                               
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
         MVI   RPTBXTYP,RPTBCOLS   Open  a    columns   box                     
         GOTO1 =A(OBOXTYPE)        Open  box  based     on   box  type          
*                                                                               
         L     R2,AIO2             ->    record                                 
*                                                                               
         USING RCLELD,R2           Map   columns   data element                 
         AH    R2,DATADISP         ->    1st  element                           
         SPACE 1                                                                
*                                  ************************************         
PCOLS10  DS    0H                  *     Process   col  element       *         
*                                  ************************************         
         CLI   0(R2),0             End   of   record ?                          
         BE    PCOLS70             Yes,  end  columns                           
         CLI   0(R2),RCLELQ        X'C3' column    element ?                    
         BNE   PCOLSNXT            No,   get  next element                      
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
*                                  ***** Skip internally          *****         
*                                  *****      generated element   *****         
         CLI   RCLDATLN,4          Data  input     length    =    4 ?           
         BL    PCOLS20             No,   skip                                   
         CLC   RCLNDATA(4),=C'zero'                                             
         BE    PCOLSNXT            Yes,  skip this element                      
*                                                                               
PCOLS20  DS    0H                  ***** Column    number:        *****         
         ZIC   R1,RCLSEQ           Get   col  sequence  number                  
         CVD   R1,DUB              Convert    binary    to   display            
         OI    DUB+7,X'0F'         Clear the  sign                              
         UNPK  BXCNUM+1(L'BXCNUM-1),DUB                                         
         CLI   BXCNUM+1,C'0'       Change     leading   zero                    
         BNE   *+8                       to                                     
         MVI   BXCNUM+1,C' '                  blanks                            
*                                                                               
*                                  ***** Keyword:                 *****         
         GOTO1 GETKEYWD,(R2)       Get   keyword                                
*                                                                               
*                                  ***** Attributes:              *****         
         BAS   RE,PCOLSATT         Get   Attributes                             
*                                                                               
*                                  ***** Range:                   *****         
         BAS   RE,PCOLSRNG         Get   range                                  
*                                                                               
*                                  ***** Width:                   *****         
         ZIC   R1,RCLWDTH          Get   col  width                             
         CVD   R1,DUB              Convert    binary    to   display            
         OI    DUB+7,X'0F'         Clear the  sign                              
         UNPK  BXCWDTH+1(2),DUB                                                 
         CLI   BXCWDTH+1,C'0'      Change     leading   zero                    
         BNE   *+8                       to                                     
         MVI   BXCWDTH+1,C' '                 blanks                            
*                                                                               
*                                  ***** Totals:                  *****         
         TM    RCLOPT,RCLACCM      Amount    keyword ?                          
         BO    PCOLS30             Yes,  skip                                   
         MVC   BXCTOTL(L'@NO),@NO  Default   to   "No"                          
         TM    RCLOPT2,RCLTOT      Any   totals ?                               
         BZ    PCOLS30                                                          
         MVC   BXCTOTL(L'@NO),SPACES     Clear    default                       
         MVC   BXCTOTL(L'@YES),@YES      "Yes"                                  
*                                                                               
PCOLS30  DS    0H                                                               
         TM    RCLOPT,RCLSUPP      Suppress  detail ?                           
         BZ    *+10                                                             
         MVC   BXCTOTL(L'@ONLY),@ONLY    "Only"                                 
*                                                                               
         TM    RCLOPT,RCLNOTOT     Suppress  total ?                            
         BZ    *+10                                                             
         MVC   BXCTOTL(L'@DTLSO),@DTLSO "Details  only"                         
*                                                                               
*                                  ***** Headings:                *****         
         ZIC   RF,RCLDATLN         ->    headings                               
         LA    RE,RCLNDATA(RF)                                                  
         ICM   RF,1,RCLHD1LN       Get   length    of   1st  heading            
         BZ    PCOLS40             None, try  heading   2                       
         BCTR  RF,0                                                             
         EXMVC RF,BXCHEAD1,0(RE)   Insert     heading   1                       
         LA    RE,1(RF,RE)         ->    end  of   heading                      
*                                                                               
PCOLS40  DS    0H                                                               
         ICM   RF,1,RCLHD2LN       Get   length    of   2nd  heading            
         BZ    PCOLS50             None, skip                                   
         BCTR  RF,0                                                             
         EXMVC RF,BXCHEAD2,0(RE)   Insert     heading   2                       
*                                                                               
PCOLS50  DS    0H                  ***** Filters:                 *****         
         BAS   RE,PCOLSFLT         Get   Filters                                
*                                                                               
*                                  ***** Print     the  data      *****         
*                                  Make  sure line(s)   will fit                
         GOTO1 =A(CHKNWRIT)              and  print     the  data               
*                                                                               
*                                  ***** Next element             *****         
PCOLSNXT DS    0H                  Bump  to   next cols element                 
         SR    R1,R1                                                            
         IC    R1,RCLLN                                                         
         AR    R2,R1                                                            
         B     PCOLS10             Test  next element                           
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
PCOLS70  DS    0H                                                               
         GOTO1 =A(CLOSEBOX)        Close the  current   box                     
*                                                                               
         MVI   RPTLINES,2          Say   print     two  lines                   
         GOTO1 =A(CHKSPACE)        Make  sure lines     will fit                
         MVI   RPTLINES,1          Say   print     one  lines                   
*                                                                               
         TM    RPTSWS,RPTSNPED     Started    a    new  page ?                  
         BO    PCOLS75             Yes,  skip                                   
         GOTO1 ACREPORT            Skip  a    line                              
*                                                                               
PCOLS75  DS    0H                  Report     width                             
         LA    R7,XP+1                                                          
         MVCDD 0(17,R7),AC#RPTWD   "Report    width"                            
         GOTO1 =A(TRANFREE)        Translate  and  find 1st  free byte          
         MVI   1(R7),C'='          Equal sign                                   
         LA    R7,3(,R7)           Point past the  '='  and  a    blank         
*                                                                               
         SR    R3,R3               Clear register                               
         L     R2,AIO2             ->    record                                 
         AH    R2,DATADISP         ->    1st  element                           
*                                                                               
PCOLS80  DS    0H                                                               
         CLI   0(R2),0             End   of   record ?                          
         BNE   *+6                 No,   continue                               
         DC    H'00'               Bad   scribe    record                       
*                                                                               
         CLI   0(R2),STYELQ        X'25' Scribe    free form element ?          
         BE    PCOLS85             Yes,  done                                   
         IC    R3,1(,R2)           Bump  to   next element                      
         AR    R2,R3                                                            
         B     PCOLS80                                                          
*                                                                               
         USING STYELD,R2           Map   free form element                      
PCOLS85  DS    0H                  Found free form element                      
         EDIT  STYWIDTH,(5,(R7)),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 ACREPORT            Print the  line                              
         DROP  R2                                                               
*                                                                               
PCOLSEX  BAS   RE,CLRXPSAV         Clear XPSAVE    area                         
         B     EXIT                Return     to   caller                       
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  Process general profile                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R4         Map   wide print     areas                   
         SPACE 1                                                                
PGPROF   NTR1  ,                                                                
*                                  Force new  page if   the  following          
*                                        will not  fit:                         
*                                  1.    blank     line                         
*                                  2.    screen    name line                    
*                                  3.    box  title     line                    
*                                  4.    box  headers                           
*                                  5.    box  midline                           
*                                  6-7.  two  entries                           
*                                  8.    box  close     line                    
         MVI   RPTLINES,8          ..                                           
         GOTO1 =A(CHKSPACE)        Make  sure lines     will fit                
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
         TM    RPTSWS,RPTSNPED     Started    a    new  page ?                  
         BO    PGPROF00            Yes,  skip                                   
         GOTO1 ACREPORT            Skip  a    line                              
*                                                                               
PGPROF00 DS    0H                                                               
         MVC   RPTSCRNN,SPACES     Clear the  screen    name                    
         OI    RPTSWS,RPTSUCTR     Upper case translate                         
         LA    R7,RPTSCRNN         ->    the  screen    name area               
*                                  "General   profile"                          
         GOTO1 =A(TXTGET),DMCB,3885,(L'RPTSCRNN,(R7)),0,0                       
         GOTO1 =A(PRTSCRNN)        Print the  screen    name                    
*                                                                               
*                                  "Print     options"                          
         MVC   XP+1(L'@PROPT),@PROPT                                            
         GOTO1 ACREPORT                                                         
*                                                                               
         MVI   RPTBXTYP,RPTBPROF   Open  a    general   profile   box           
         GOTO1 =A(OBOXTYPE)        Open  box  based     on   box  type          
*                                                                               
BASE     USING BXGPROFD,R3         Map   general   profile   print line         
         USING BXGPROFD,R5         Map   general   profile   print line         
         LA    R3,XP                                                            
         LR    R5,R3               Addr  of   current   profile   col           
*                                                                               
         L     R2,AIO2             ->    record                                 
*                                                                               
         USING RPFELD,R2           Map   profile   data element                 
         AH    R2,DATADISP         ->    1st  element                           
*                                                                               
*                                  ************************************         
PGPRO10  DS    0H                  *     Process   profile   element  *         
*                                  ************************************         
         CLI   0(R2),0             End   of   record ?                          
         BNE   PGPRO20             No,   continue                               
         L     R2,=A(MISSC4EL)     Missing    profile   element                 
         OI    RPTSWS,RPTNOC4E     Turn  on   no   X'C4'     element            
*                                                                               
PGPRO20  DS    0H                                                               
         CLI   0(R2),RPFELQ        X'C4' profile   element ?                    
         BE    PGPRO30             Yes,  process   element                      
         SR    R1,R1               Bump  to   next element                      
         IC    R1,RPFLN                                                         
         AR    R2,R1                                                            
         B     PGPRO10             Test  next element                           
         SPACE 1                                                                
*                                  ************************************         
PGPRO30  DS    0H                  *     start     print     options  *         
*                                  ************************************         
         ST    R2,RPT@PRFE         Save  address   of   profile   el            
         GOTO1 =A(PGPROPRT)        Process    print     options                 
*                                                                               
         SPACE 1                                                                
*                                  ************************************         
*                                  *     End  print     options       *         
*                                  ************************************         
         TM    RPTOPT#,X'01'       Odd   number    of   options ?               
         BZ    PGPRO40             No,   end  print     options                 
*                                  Make  sure line will fit                     
         GOTO1 =A(CHKNWRIT)              and  print     the  data               
*                                                                               
PGPRO40  DS    0H                                                               
         GOTO1 =A(CLOSEBOX)        Close the  current   box                     
         SPACE 1                                                                
*                                  ************************************         
*                                  *     Start     rounding  options  *         
*                                  ************************************         
*                                  Force new  page if   the  following          
*                                        will not  fit:                         
*                                  1.    blank     line                         
*                                  2.    box  title     line                    
*                                  3.    box  headers                           
*                                  4.    box  midline                           
*                                  5.    one  line                              
*                                  6.    box  close     line                    
         MVI   RPTLINES,6          ..                                           
         GOTO1 =A(CHKSPACE)        Make  sure lines     will fit                
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
         TM    RPTSWS,RPTSNPED     Started    a    new  page ?                  
         BO    PGPRO50             Yes,  skip                                   
         GOTO1 ACREPORT            Skip  a    line                              
*                                                                               
PGPRO50  DS    0H                  "Rounding  options"                          
         MVC   XP+1(L'@RNDTO),@RNDTO                                            
         GOTO1 ACREPORT                                                         
*                                                                               
         MVI   RPTBXTYP,RPTBPROF   Open  a    general   profile   box           
         GOTO1 =A(OBOXTYPE)        Open  box  based     on   box  type          
*                                                                               
         GOTO1 =A(PGPRORND)        Process    rounding  options                 
*                                                                               
         SPACE 1                                                                
*                                  ************************************         
*                                  *     End  rounding  options       *         
*                                  ************************************         
         TM    RPTOPT#,X'01'       Odd   number    of   options ?               
         BZ    PGPRO60             No,   end  rounding  options                 
*                                  Make  sure line will fit                     
         GOTO1 =A(CHKNWRIT)              and  print     the  data               
*                                                                               
PGPRO60  DS    0H                                                               
         GOTO1 =A(CLOSEBOX)        Close the  current   box                     
         DROP  R5,BASE                                                          
         SPACE 1                                                                
*                                  ************************************         
*                                  *     Start     ranking   options  *         
*                                  ************************************         
*                                  Map   general   profile   print line         
         USING BXGRPROD,R3               for  ranking   format                  
*                                  Force new  page if   the  following          
*                                        will not  fit:                         
*                                  1.    blank     line                         
*                                  2.    box  title     line                    
*                                  3.    box  headers                           
*                                  4.    box  midline                           
*                                  5.    one  line                              
*                                  6.    box  close     line                    
         MVI   RPTLINES,6          ..                                           
         GOTO1 =A(CHKSPACE)        Make  sure lines     will fit                
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
         TM    RPTSWS,RPTSNPED     Started    a    new  page ?                  
         BO    PGPRO70             Yes,  skip                                   
         GOTO1 ACREPORT            Skip  a    line                              
*                                                                               
PGPRO70  DS    0H                  "Ranking"                                    
         MVC   XP+1(L'@RNKNG),@RNKNG                                            
         GOTO1 ACREPORT                                                         
*                                                                               
*                                  Open  a    general   profile                 
         MVI   RPTBXTYP,RPTBPROR              ranking   box                     
         GOTO1 =A(OBOXTYPE)        Open  box  based     on   box  type          
*                                                                               
         GOTO1 =A(PGPRORNK)        Process    ranking   options                 
*                                                                               
         SPACE 1                                                                
*                                  ************************************         
*                                  *     End  ranking   options       *         
*                                  ************************************         
*                                  Make  sure lines     will fit                
         GOTO1 =A(CHKNWRIT)              and  print     the  data               
*                                                                               
         GOTO1 =A(CLOSEBOX)        Close the  current   box                     
         DROP  R3                                                               
         SPACE 1                                                                
*                                  ************************************         
*                                  *     Start     down-load options  *         
*                                  ************************************         
*                                  Force new  page if   the  following          
*                                        will not  fit:                         
*                                  1.    blank     line                         
*                                  2.    screen    name line                    
*                                  3.    box  headers                           
*                                  4.    box  midline                           
*                                  5-6.  two  entries                           
*                                  7.    box  close     line                    
         MVI   RPTLINES,7          ..                                           
         GOTO1 =A(CHKSPACE)        Make  sure lines     will fit                
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
         TM    RPTSWS,RPTSNPED     Started    a    new  page ?                  
         BO    PGPRO80             Yes,  skip                                   
         GOTO1 ACREPORT            Skip  a    line                              
*                                                                               
PGPRO80  DS    0H                                                               
         MVC   RPTSCRNN,SPACES     Clear the  screen    name                    
         MVC   RPTSCRNN(L'$DL),$DL "DOWN-LOAD"                                  
         GOTO1 =A(PRTSCRNN)        Print the  screen    name                    
*                                                                               
*                                  Open  a    general   profile                 
         MVI   RPTBXTYP,RPTBPROD              down-load box                     
         GOTO1 =A(OBOXTYPE)        Open  box  based     on   box  type          
*                                                                               
         GOTO1 =A(PGPRODWN)        Process    down-load options                 
         SPACE 1                                                                
*                                  ************************************         
*                                  *     End  down-load options       *         
*                                  ************************************         
*                                  Make  sure line will fit                     
         GOTO1 =A(CHKNWRIT)              and  print     the  data               
*                                                                               
         GOTO1 =A(CLOSEBOX)        Close the  current   box                     
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  Process report profiles                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R4         Map   wide print     areas                   
         SPACE 1                                                                
PRPROF   NTR1  ,                                                                
         CLI   QPROG,MEDIA         Media ?                                      
         BE    PRPROFEX            Yes,  exit                                   
         MVI   RPTSCNUM,1          Section    number    1                       
*                                  Start a    report    profile                 
         GOTO1 =A(SRPRFSEC)              section                                
*                                                                               
         LA    R3,XP               ->    report    line                         
*                                                                               
         GOTO1 =A(PRPRF)           Process    report    profile                 
*                                                                               
*                                  Make  sure line will fit                     
         GOTO1 =A(CHKNWRIT)              and  print     the  data               
*                                                                               
         GOTO1 =A(CLOSEBOX)        Close the  current   box                     
*                                                                               
PRPROFEX DS    0H                  Exit                                         
         B     EXIT                Return     to   caller                       
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  Insert row attributes                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING RRWELD,R2           Map   rows data element                      
         USING BXROWD,R3           Map   rows print     line                    
         SPACE 1                                                                
PROWSATT NTR1  ,                                                                
         LA    R7,BXRKEYWD         ->    text build     area                    
         LA    R1,L'BXRKEYWD       Length     of   field                        
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         TM    RRWOPT2,RRWDICT     Is    this el   translated ?                 
         BZ    PROWSA00            No,   continue                               
         CLI   RRWDATLN,2          Do    we   have any  attributes ?            
         BNH   PROWSAEX            No,   skip attributes                        
*                                                                               
PROWSA00 DS    0H                  Output     the  attributes                   
         TM    RRWOPT,RRWADR       Address ?                                    
         BZ    PROWSA10                                                         
         LA    R1,$RSADD           "ADR"                                        
         BAS   RE,ADDATTR          Add the attribute                            
*                                                                               
PROWSA10 TM    RRWOPT,RRWCODE+RRWNAME    Both ?                                 
         BNO   PROWSA20                                                         
         LA    R1,$BOTH            "BOTH"                                       
         BAS   RE,ADDATTR          Add the attribute                            
         B     PROWSA40                                                         
*                                                                               
PROWSA20 TM    RRWOPT,RRWCODE      Code ?                                       
         BZ    PROWSA30                                                         
         LA    R1,$RSCOD           "CODE"                                       
         BAS   RE,ADDATTR          Add the attribute                            
*                                                                               
PROWSA30 TM    RRWOPT,RRWNAME      Name ?                                       
         BZ    PROWSA40                                                         
         LA    R1,$RSNAM           "NAME"                                       
         BAS   RE,ADDATTR          Add the attribute                            
*                                                                               
PROWSA40 TM    RRWOPT2,RRWSHRT     Description ?                                
         BZ    PROWSA50                                                         
         LA    R1,$RSDES           "DESC"                                       
         BAS   RE,ADDATTR          Add the attribute                            
*                                                                               
PROWSA50 TM    RRWOPT2,RRWLVL      Level ?                                      
         BZ    PROWSA60                                                         
         LA    R1,$RSLVL           "LVL"                                        
         BAS   RE,ADDATTR          Add the attribute                            
*                                                                               
PROWSA60 BAS   RE,DTEBASIS         Get date bases attributes                    
*                                                                               
PROWSAEX B     EXIT                Return to caller                             
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Get keyword for rows and columns                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R4         Map   wide print     areas                   
         USING BXROWD,R7           Map   rows print     line                    
GETKEYWD NTR1  ,                                                                
         L     R4,VBIGPRNT         ->    wide print     areas                   
*                                  ************************************         
*                                  *     Row  element                 *         
*                                  ************************************         
         USING RRWELD,R1           Map   row  element                           
         CLI   RRWEL,RRWELQ        Got   row  element ?                         
         BNE   GETKEY10            No,   go   to   column    element            
         LA    R2,RRWNDATA         ->    input     kw/att    area               
         ZIC   R3,RRWDATLN         Get   input     kw/att    len                
         LA    R5,RRWSPCL          ->    special   indicator                    
         LA    R6,RRWXDATA         ->    extra     data                         
         LA    R7,XP               ->    print     line                         
         LA    R7,BXRKEYWD         ->    rows keyword   area                    
         TM    RRWOPT2,RRWDICT     Translate  required ?                        
         BZ    GETKEYNT            No,   process   without   translate          
         B     GETKEYTR            Yes,  process   with      translate          
         DROP  R1,R7                                                            
*                                  ************************************         
GETKEY10 DS    0H                  *     Column    element            *         
*                                  ************************************         
         USING RCLELD,R1           Map   column    element                      
         USING BXCOLD,R7           Map   column    print     line               
         LA    R2,RCLNDATA         ->    input     kw/att    area               
         ZIC   R3,RCLDATLN         Get   input     kw/att    len                
         LA    R5,RCLSPCL          ->    special   indicator                    
         LA    R6,RCLXDATA         ->    extra     data                         
         LA    R7,XP               ->    print     line                         
         LA    R7,BXCKEYWD         ->    column    keyword   area               
         TM    RCLOPT2,RCLDICT     Translate  required ?                        
         BO    GETKEYTR            Yes,  process   with      translate          
         DROP  R1,R7                                                            
*                                  ************************************         
GETKEYNT DS    0H                  *     No   translation    required *         
*                                  ************************************         
         CLI   0(R2),C','          At    end  of   keyword                      
         BE    GETKEYXT            Yes,  end  this loop                         
         MVC   0(1,R7),0(R2)       Insert     this char into keyword            
         LA    R2,1(,R2)           Next  input     byte                         
         LA    R7,1(,R7)           Next  print     byte                         
         BCT   R3,GETKEYNT         Check next      byte                         
         B     GETKEYXT                                                         
*                                  ************************************         
GETKEYTR DS    0H                  *     Translation    required      *         
*                                  ************************************         
         LR    R3,R7               Save  register                               
*                                  Clear the keyword field                      
         MVC   0(L'BXRKEYWD,R7),SPACES                                          
         MVI   0(R7),ESC#LFJT      Escape character                             
         MVC   1(2,R7),0(R2)       Keyword number                               
         MVI   3(R7),6             Max keyword length                           
         OI    RPTSWS,RPTSUCTR     Upper case translate                         
         GOTO1 =A(TRANFREE)        Translate and find 1st free byte             
*                                                                               
         CLI   0(R5),RCLSPCTL      Jobber keyword with #                        
         BE    GETKEY30            Yes, replace #                               
         CLI   0(R5),RCLSPCT#      Non-jobber keyword with #                    
         BE    GETKEY30            Yes, replace #                               
         CLI   0(R5),RCLSPDTE      Date1-Date2 calculation                      
         BE    GETKEY80            Yes, find Date2                              
         CLI   0(R5),RCLSPLV1      Account levels 1-9, replace #                
         BL    GETKEYXT                                                         
         CLI   0(R5),RCLSPLV9                                                   
         BH    GETKEYXT            No, done                                     
         LR    R7,R3                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK+10(1),0(R5)                                                 
         MVC   WORK(6),0(R7)       Move keyword with #                          
         B     GETKEY38            No, done                                     
*                                                                               
GETKEY30 DS    0H                  Special # replacement code                   
         LR    R7,R3               Restore R7                                   
         MVC   WORK,SPACES         Clear work area                              
         MVC   WORK(6),0(R7)       Move keyword with #                          
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,0(R6)          Any extra data ?                             
         BNZ   *+6                 Yes, continue                                
         DC    H'00'               Should have extra data                       
*                                                                               
         CVD   R1,DUB              Convert it to packed                         
         OI    DUB+7,X'0F'         Clear the sign                               
         UNPK  WORK+10(3),DUB      Convert it to display                        
*                                                                               
GETKEY38 LA    R0,6                Max keyword length                           
         LA    R1,WORK             ->    found keyword area                     
         LR    RE,R7               ->    build keyword area                     
*                                                                               
GETKEY40 DS    0H                  Replace # with a number                      
         CLI   0(R1),C'#'          Is char # ?                                  
         BNE   GETKEY60            No, just move char                           
         MVC   0(3,RE),WORK+10     Insert num with leadings 0's                 
*                                  Remove leading zeros                         
         LA    RF,3                Three bytes of data                          
         CLI   WORK+10,C'0'        Leading zero ?                               
         BNE   GETKEY50            No, continue                                 
         BCTR  RF,0                Two bytes of data                            
*                                  Insert the num & make sure                   
         MVC   0(3,RE),WORK+11           last byte blank                        
         CLI   WORK+11,C'0'        Another leading zero ?                       
         BNE   GETKEY50            No, continue                                 
         BCTR  RF,0                One byte of data                             
*                                  Insert the number and                        
         MVC   0(2,RE),WORK+12           make sure last byte blank              
*                                                                               
GETKEY50 DS    0H                  Got   the  number                            
         AR    RE,RF               ->    byte following number                  
         BCTR  RE,0                ->    previous  byte                         
         B     GETKEY70            Find  next characters                        
*                                                                               
GETKEY60 DS    0H                  Move  one  character                         
         MVC   0(1,RE),0(R1)             from the  found     keyword            
*                                                                               
GETKEY70 DS    0H                  Bump  to   next character                    
         LA    RE,1(,RE)           Next  build     keyword   character          
         LA    R1,1(,R1)           Next  found     keyword   character          
         BCT   R0,GETKEY40         Test  next found     character               
         B     GETKEYXT            Exit                                         
*                                                                               
GETKEY80 DS    0H                  Date1-Date2     calculation                  
*                                  R7    ->   1st  free byte                    
         MVI   0(R7),C'-'          Insert     minus     sign                    
         LA    R7,1(,R7)           Get   next character                         
*                                  Code  assumes   total     len  <= 12         
         MVI   0(R7),ESC#LFJT      Escape     character                         
         MVC   1(2,R7),2(R2)       Date2 Keyword   number                       
         MVI   3(R7),6             Max   keyword   length                       
         OI    RPTSWS,RPTSUCTR     Upper case translate                         
         GOTO1 =A(TRANSLAT)        Translate  date2     keyword                 
*                                                                               
GETKEYXT B     EXIT                Return     to   caller                       
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  Insert column attributes                                           *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      R3       = Current columns print line                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         USING BXCOLD,R3           Map   columns   print     line               
         SPACE 1                                                                
PCOLSATT NTR1  ,                                                                
         TM    RCLOPT2,RCLDICT     Is    this el   translated ?                 
         BZ    PCOLSA00            No,   continue                               
         CLI   RCLDATLN,2          Do    we   have any  attributes ?            
         BNH   PCOLSAEX            No,   skip attributes                        
*                                                                               
PCOLSA00 DS    0H                  Output     the  attributes                   
         LA    R7,BXCKEYWD         ->    text build     area                    
         LA    R1,L'BXCKEYWD       Length     of   field                        
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
         ST    R7,RPT@ATTR         Save  start     of   attributes              
*                                                                               
         L     R4,=A(POPT2TAB)     ->    RCLOPT2   table                        
*                                                                               
PCOLSA10 DS    0H                                                               
         CLI   0(R4),EOT           End   of   table ?                           
         BE    PCOLSA30            Yes,  continue                               
         MVC   BYTE,0(R4)          Get        opt  tbl  entry     bits          
         NC    BYTE,RCLOPT2        See   if   opt  tbl  bits set                
         CLC   BYTE,0(R4)          Same  bits still     on ?                    
         BNE   PCOLSA20            No,   next entry                             
         LA    R1,1(,R4)           ->    the attribute  number                  
         BAS   RE,ADDATTR#         Add   the  attribute                         
*                                  Both  ?                                      
         CLI   BYTE,RCLCODE+RCLNAME                                             
         BNE   PCOLSA20            No,   continue                               
         L     R4,=A(POPT2ATB)     Skip  code and  name separately              
*                                                                               
PCOLSA20 DS    0H                                                               
         LA    R4,POPT2TBQ(,R4)    ->    next entry                             
         B     PCOLSA10            check next entry                             
*                                                                               
PCOLSA30 DS    0H                                                               
         L     RF,RPT@ATTR         ->    text build     area                    
         CR    R7,RF               Any   attributes     so   far ?              
         BNE   PCOLSA90            Yes,  done with attributes                   
*                                  ***** Date bases     attributes:             
         BAS   RE,DTEBASIS               Get  date bases     attributes         
*                                                                               
         CLI   RCLDATES,0          Any   date basis     attributes ?            
         BE    PCOLSA90            No,   skip                                   
         CLI   RCLDTEFG,0          Any   date basis ?                           
         BNE   PCOLSA40            Yes,  continue                               
         CLC   RCLNDATA(2),=AL2(AC#RSRMO)     MON ?                             
         BE    PCOLSA90            Yes,  no   period                            
         CLC   RCLNDATA(2),=AL2(AC#RSRY1)     YEAR ?                            
         BE    PCOLSA90            Yes,  no   period                            
         LA    R1,$RSRPE           "PER"                                        
         BAS   RE,ADDATTR          Add   the  attribute                         
         B     PCOLSA90            Done  with attributes                        
*                                                                               
PCOLSA40 DS    0H                                                               
         L     R4,=A(DTEFGTAB)     ->    RCLDTEFG  table                        
*                                                                               
PCOLSA50 DS    0H                                                               
         CLI   0(R4),EOT           End   of   table ?                           
         BNE   *+6                 No,   continue                               
         DC    H'00'               DTEFG table     error                        
*                                                                               
         CLC   RCLDTEFG,0(R4)      Found matching  entry ?                      
         BE    PCOLSA60            Yes,  use  this entry                        
         LA    R4,DTEFGTBQ(,R4)    Bump  to   next entry                        
         B     PCOLSA50            Try   the  next entry                        
*                                                                               
PCOLSA60 DS    0H                  Found the  entry                             
         LA    R1,1(,R4)           ->    the attribute number                   
         BAS   RE,ADDATTR#         Add   the  attribute                         
*                                                                               
         TM    RCLDTEFG,RCLNROLL   Non-rolling     parameter ?                  
         BZ    PCOLSA90            Yes,  finished  parameters                   
         LA    R4,LATRB            Get   length    of   an   attribute          
*                                                                               
PCOLSA70 DS    0H                  Found the  entry                             
*                                  Find  the  question  marks                   
         CLI   0(R7),C'?'          Is    it   question  mark ?                  
         BE    PCOLSA80            Yes,  replace   it                           
         LA    R7,1(,R7)           Get   next character                         
         BCT   R4,PCOLSA70         Test  next charcter                          
*                                                                               
PCOLSA80 DS    0H                  Found the  question marks                    
         BCTR  R4,0                Minus 1    for  execute                      
         EXMVC R4,0(R7),SPACES     Clear the  field                             
         SR    RF,RF               Get   the                                    
         ICM   RF,3,RCLENDT                   period                            
         CVD   RF,DUB              Convert    to   packed                       
         OI    DUB+7,X'0F'         Clear the  sign                              
         SR    R4,R4               Get   length    for  unpack                  
         CHI   RF,9                More  than one  byte ?                       
         BNH   *+8                 No,   skip                                   
         LA    R4,X'10'            Unpack     two  bytes                        
         CHI   RF,99               More  than two  bytes ?                      
         BNH   *+8                 No,   skip                                   
         LA    R4,X'20'            Unpack     3    bytes                        
         EX    R4,*+8              Insert     the  data                         
         B     *+10                Skip  unpack                                 
         UNPK  0(,R7),DUB          Convert    to   display                      
*                                                                               
         SRL   R4,4                Get   num  of   bytes     -1 (0,1,2)         
         LA    R7,1(R4,R7)         Bump  to   next byte                         
*                                                                               
PCOLSA90 DS    0H                  Found all  the  parameters                   
*                                                                               
PCOLSAEX DS    0H                  Exit                                         
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert column range into BXCRNGE                                   *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      R3       = Current columns print line                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         USING BXCOLD,R3           Map   columns   print     line               
         SPACE 1                                                                
PCOLSRNG NTR1  ,                                                                
         LA    R7,WORK             ->    build     area                         
         MVC   WORK,SPACES         Clear build     area                         
*                                                                               
*                                  ***** 'from'    or   only value              
*                                  Date  range     used in                      
         CLI   RCLSPCL,RCLSPKY1          non$ keyword  (i.e. PDAY)              
         BNE   PCOLSR10            No,   skip special   logic                   
         ZIC   RF,RCLPDN1          Get   calendar  period    number             
         CVD   RF,DUB              Convert    to   packed                       
         OI    DUB+7,X'0F'         Clear sign                                   
         SR    RE,RE               Unpack     into one  byte                    
         CHI   RF,9                Less  than 10 ?                              
         BNH   *+8                 Yes,  skip                                   
         LA    RE,X'10'            Unpack     into two  bytes                   
         EX    RE,*+8              Execute    unpack                            
         B     PCOLSR90            Move  range     to   output                  
         UNPK  0(0,R7),DUB         Convert    range     to   display            
*                                                                               
PCOLSR10 DS    0H                  Not   PDAY                                   
         CLI   RCLDATES,0          Any   date range     used ?                  
         BE    PCOLSREX            No,   exit                                   
         TM    RCLOPT,RCLACCM      Is    this an   accumulated column ?         
         BZ    PCOLSREX            No,   exit                                   
         TM    RCLDTEFG,X'FF'-RCLTODTE   Any  period    range ?                 
         BZ    PCOLSREX            No,   exit                                   
         CLC   RCLSTDT,=XL2'8000'  Delimiter  for  'Prior' ?                    
         BNE   PCOLSR20            No,   skip                                   
         MVCDD 0(5,R7),AC#PRIOR    "Prior"                                      
         OI    RPTSWS,RPTSUCTR     Upper case translate                         
         GOTO1 =A(TRANSLAT)        Translate                                    
         B     PCOLSR80            Find  the  'to' value                        
*                                                                               
PCOLSR20 DS    0H                  Not   prior                                  
         MVC   HALF,RCLSTDT        Move  for  boundary  alignment               
         LH    RF,HALF             Get   start     date                         
*                                  PMON  or   PM1-PM12  parameter ?             
         TM    RCLDTEFG,RCLPERD+RCLMON                                          
         BNO   PCOLSR30            No,   skip                                   
         SR    RF,RF               Clear register                               
         ICM   RF,1,RCLPDN1        Get   calendar  period    number             
         TM    RCLPDYR,X'80'       Do    we   want to   negate    it ?          
         BZ    *+6                 No,   skip                                   
         LNR   RF,RF               Negate     it                                
*                                                                               
PCOLSR30 DS    0H                                                               
         BAS   RE,PCOLREDT         Edit  the  value                             
*                                                                               
*                                  PMON  or   PM1-PM12  parameter ?             
         TM    RCLDTEFG,RCLPERD+RCLMON                                          
         BNO   PCOLSR40            No,   continue  normal                       
         CLI   RCLSTDT,X'01'       Forward    one  year ?                       
         BNE   *+8                 No,   skip                                   
         MVI   0(R1),C'+'          Yes,  move in   a    plus sign               
         TM    RCLDTEFG,RCLNROLL   PM1-PM12   parameter ?                       
         BO    PCOLSR90            Yes,  insert    this value                   
*                                  PMON  parameter                              
         OC    RCLENDT,RCLENDT     Any   end  value ?                           
         BNZ   PCOLSR80            Yes,  find it                                
         B     PCOLSR90            No,   output    this value                   
*                                                                               
PCOLSR40 DS    0H                  Not   PMON nor  PM1-PM12  parameter          
         TM    RCLSTDT,X'80'       Negative   'from'    date ?                  
         BO    PCOLSR50            Yes,  must have 'to' date                    
         TM    RCLDTEFG,RCLDAY     DAY   parameter ?                            
         BO    PCOLSR60            Yes,  must have 'to' date                    
         OC    RCLENDT,RCLENDT     Any   end  date ?                            
         BZ    PCOLSR90            No,   output    the  value                   
*                                                                               
PCOLSR50 DS    0H                                                               
         TM    RCLDTEFG,RCLPERD    Calendar   periods   parameter ?             
         BZ    PCOLSR60            No,   find 'to' date                         
         TM    RCLDTEFG,RCLNROLL   P1-P999    parameter ?                       
         BO    PCOLSR90            Yes,  skip 'to' date                         
*                                  Must  be   PED  parameter ?                  
         CLC   RCLSTDT,RCLENDT           'From'    date =    'to' date?         
         BE    PCOLSR90            Yes,  do   not  display   'to' date          
*                                                                               
PCOLSR60 DS    0H                                                               
         CLC   RCLENDT,=XL2'8000'  Delimiter  for  'after'                      
         BNE   PCOLSR70            No,   skip                                   
         MVC   7(1,R7),COMMA       Insert     comma                             
         LA    R7,8(,R7)           ->    'to' date in   output                  
         MVCDD 0(5,R7),AC#AFTER    "After"                                      
         OI    RPTSWS,RPTSUCTR     Upper case translate                         
         GOTO1 =A(TRANSLAT)        Translate                                    
         SHI   R7,8                ->    start     of   build     area          
         B     PCOLSR90            Output     the  range                        
*                                                                               
PCOLSR70 DS    0H                                                               
*                                  M1-M12,    Q1-Q4,    MON  parameter?         
         TM    RCLDTEFG,RCLMON+RCLNROLL                                         
         BNZ   PCOLSR90            Yes,  skip 'to' date                         
         TM    RCLDTEFG,RCLPERD    PED   parameter ?                            
         BO    PCOLSR80            Yes,  must have 'to' date                    
         TM    RCLDTEFG,RCLDAY     DAY   parameter ?                            
         BZ    PCOLSR90            No,   skip 'to' date                         
*                                                                               
PCOLSR80 DS    0H                  Add   'to' date                              
         MVC   7(1,R7),COMMA       Insert     comma                             
         MVC   HALF,RCLENDT        Move  for  boundary  alignment               
         LH    RF,HALF                                                          
         LA    R7,8(,R7)           ->    area to   store     'to' date          
         BAS   RE,PCOLREDT         Edit  the  value                             
*                                                                               
PCOLSR90 DS    0H                  Move  to   range     field                   
         MVC   BXCRNGE,SPACES      Clear range     field                        
         LA    R0,15               Length     of   field                        
         LA    R1,WORK             ->    build     area                         
         LA    RF,BXCRNGE          ->    range     field                        
*                                                                               
PCOLSR93 DS    0H                  Do    not  insert    spaces                  
         CLI   0(R1),C' '          Blank ?                                      
         BNH   PCOLSR95            Yes,  skip this byte                         
         MVC   0(1,RF),0(R1)       Insert     this byte                         
         LA    RF,1(,RF)           ->    next output    byte                    
*                                                                               
PCOLSR95 DS    0H                                                               
         LA    R1,1(,R1)           ->    next build     byte                    
         BCT   R0,PCOLSR93         Test  next build     byte                    
*                                                                               
PCOLSREX DS    0H                  Exit                                         
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         SPACE 1                                                                
*                                  ************************************         
PCOLREDT DS    0H                  *     Range     edit               *         
*                                  ************************************         
         CVD   RF,DUB              Convert    to   packed                       
         MVC   0(7,R7),=XL7'40202020202060'   Edit mask                         
         LA    R1,5(,R7)           Set   R1   since     DUB  may  = 0           
         EDMK  0(7,R7),DUB+5       Edit  and  mark the  value                   
*                                  R1    ->   1st     significant digit         
         CLI   5(R7),C' '          Value =    0 ?                               
         BNE   *+8                 No,   skip                                   
         MVI   5(R7),C'0'          Force a    0    to   print                   
         BCTR  R1,0                ->    before    1st  byte                    
*                                  If    negative  value,    insert             
         MVC   0(1,R1),6(R7)             a    "-"  in   front                   
         MVI   6(R7),C' '          Clear sign byte at   end                     
*                                                                               
         BSM   0,RE                Return     to   caller                       
         EJECT ,                                                                
***********************************************************************         
*  Find date basis attributes and add them to the work area           *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      R7       = Current address in work area                        *         
*                                                                     *         
*    Output:                                                          *         
*      R7       = New     address in work area                        *         
***********************************************************************         
         SPACE 1                                                                
DTEBASIS NTR1  ,                                                                
*                                  ************************************         
*                                  *     Row       element            *         
*                                  ************************************         
*                                                                               
         USING RRWELD,R2           Map   row  element                           
         CLI   RRWEL,RRWELQ        Got   row  element ?                         
         BNE   DTEBAS10            No,   go   to   column    element            
         LA    R3,RRWDATES         ->    row  dates     switches                
         B     DTEBAS20                                                         
         DROP  R2                                                               
         SPACE 1                                                                
*                                  ************************************         
DTEBAS10 DS    0H                  *     Column    element            *         
*                                  ************************************         
*                                                                               
         USING RCLELD,R2           Map   column    element                      
         LA    R3,RCLDATES         ->    column    dates     switches           
         DROP  R2                                                               
         SPACE 1                                                                
*                                  ************************************         
DTEBAS20 DS    0H                  *     Find      date bases         *         
*                                  *               attributes         *         
*                                  ************************************         
         CLI   0(R3),0             Any   date bases     attributes ?            
         BE    DTEBASEX            No,   skip                                   
*                                                                               
         CLI   0(R3),RRWBMOA       Billed     MOA ?                             
         BNE   *+8                                                              
         LA    R1,=AL2(AC#RSBMO)   "BMOA"     -    billed    MOA                
*                                                                               
         CLI   0(R3),RRWBLDT       Billed     date ?                            
         BNE   *+8                                                              
         LA    R1,=AL2(AC#RSBDT)   "BDTE"     -    bill      date               
*                                                                               
         CLI   0(R3),RRWMODT       MOA   date ?                                 
         BNE   *+8                                                              
         LA    R1,=AL2(AC#RSMOA)   "MOA"      -    MOA       date               
*                                                                               
         CLI   0(R3),RRWADTE       Activity   date ?                            
         BNE   *+8                                                              
         LA    R1,=AL2(AC#RSADT)   "ADTE"     -    added     date               
*                                                                               
         CLI   0(R3),RRWDUDT       Due   date ?                                 
         BNE   *+8                                                              
         LA    R1,=AL2(AC#RSADU)   "DUE"      -    due  date                    
*                                                                               
         CLI   0(R3),RRWTRDT       Transaction     date ?                       
         BNE   DTEBAS50                                                         
         LA    R1,=AL2(AC#RSTDT)   "TADT"     -    transaction    date          
*                                                                               
         CLI   QPROG,RECEIVABLE    RCV   scribe ?                               
         BNE   *+8                                                              
         LA    R1,=AL2(AC#RSABL)   "BILL"     -    bill date                    
*                                                                               
         CLI   QPROG,INCOME        Income     scribe ?                          
         BNE   *+8                                                              
         LA    R1,=AL2(AC#RSIDT)   "IDTE"     -    invoice   date               
*                                                                               
         CLI   QPROG,MANPOWER      Manpower   scribe ?                          
         BNE   *+8                                                              
         LA    R1,=AL2(AC#RSCMO)   "CMO"      -    calendar  month              
*                                                                               
DTEBAS50 DS    0H                                                               
         BAS   RE,ADDATTR#         Add   the  attribute                         
*                                                                               
DTEBASEX DS    0H                  Exit                                         
         B     EXITR7              Return     R7   to   caller                  
         EJECT ,                                                                
***********************************************************************         
*  Add an attribute when the attribute is known                       *         
*                                                                     *         
*    Input:                                                           *         
*      R1       = Address   of   attribute to   be   added            *         
*      R7       = First     free byte                                 *         
*                                                                     *         
*    Output:                                                          *         
*      R7       = First     free byte                                 *         
***********************************************************************         
         SPACE 1                                                                
ADDATTR  NTR1  ,                   Add   an   attribute                         
         MVC   0(1,R7),COMMA       ","                                          
         LA    R7,1(,R7)           First free byte                              
         MVC   0(LATRB,R7),0(R1)   Insert     the  attribute                    
         LA    R1,LATRB            Length     of   data field                   
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
         B     EXITR7              Return     R7   to   caller                  
         EJECT ,                                                                
***********************************************************************         
*  Add an attribute when the attribute number is known                *         
*                                                                     *         
*    Input:                                                           *         
*      R1       = Address   of   attribute number                     *         
*      R1       = Attribute to   be   added                           *         
*      R7       = First     free byte                                 *         
*                                                                     *         
*    Output:                                                          *         
*      R7       = First     free byte                                 *         
***********************************************************************         
         SPACE 1                                                                
ADDATTR# NTR1  ,                   Add   an   attribute                         
         MVC   0(1,R7),COMMA       ","                                          
         LA    R7,1(,R7)           First free byte                              
         MVI   0(R7),ESC#LFJT      Insert                                       
         MVC   1(2,R7),0(R1)             the                                    
         MVI   3(R7),LATRB                    attribute                         
         OI    RPTSWS,RPTSUCTR     Upper case translate                         
         GOTO1 =A(TRANSLAT)        Translate  the  entry                        
         LA    R1,LATRB            Length     of   data field                   
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
         B     EXITR7              Return     R7   to   caller                  
         EJECT ,                                                                
***********************************************************************         
*  Insert column filter information                                   *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      R3       = Current columns print line                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         USING BXCOLD,R3           Map   columns   print     line               
         USING BIGPRNTD,R4         Map   wide print     areas                   
         USING RFLELD,R6           Map   filters   data element                 
         SPACE 1                                                                
PCOLSFLT NTR1  ,                   Find  any  column    filters                 
         L     R4,VBIGPRNT         ->    wide print     areas                   
         BAS   RE,CLRXPSAV         Clear XPSAVE    area                         
         MVC   SVREG,XPSAVE        Start at   saved     XP   print area         
         MVI   RPTLINE#,1          Init  curr line being     formatted          
         BAS   RE,PCOLSFAD         Find  additional     column     opts         
         LR    R6,R2               Start from current   column     el           
*                                                                               
PCOLSF10 DS    0H                                                               
         CLI   0(R6),0             End   of   record ?                          
         BE    PCOLSF65            Yes,  done with filters   search             
         CLI   0(R6),RFLELQ        X'C5' filters   elements                     
         BNE   PCOLSF60            No,   find next element                      
         CLC   RFLSEQ,RCLSEQ       Seq   num  matches   column     num?         
         BNE   PCOLSF60            No,   find next element                      
         TM    RFLIND,RFLINTRL     Internally generated element ?               
         BO    PCOLSF60            Yes,  find next element                      
*                                                                               
         USING FLTTABD,R5          Map   column    filters   table              
         L     R5,=A(FLTTAB)       ->    filters   table                        
*                                                                               
PCOLSF15 DS    0H                                                               
         CLI   0(R5),EOT           End   of   table ?                           
         BE    PCOLSF60            Yes,  find next element                      
         CLC   RFLTYPE,FLTTNUM     Match on   type ?                            
         BNE   PCOLSF18            No,   get  next entry                        
         CLI   FLTTJOB,0           For   all  jobs ?                            
         BE    PCOLSF20            Yes,  continue                               
         CLC   FLTTJOB,QPROG       Same  job ?                                  
         BE    PCOLSF20            Yes,  continue                               
*                                                                               
PCOLSF18 DS    0H                  Get   next entry                             
         LA    R5,FLTTABLQ(,R5)    ->    next tbl  entry                        
         B     PCOLSF15            Test  next tbl  entry                        
*                                                                               
PCOLSF20 DS    0H                  Found tbl  entry                             
         L     R7,SVREG            ->    current   XP   saved     addr          
         TM    FLTTSWS,FLTTPCTD    PCT   denominator ?                          
         BZ    PCOLSF25            No,   skip                                   
         MVC   0(17,R7),=C'PCT Denominator: '                                   
*                                  Insert     PCT  denominator                  
         LA    R7,17(,R7)          ->    next text location                     
*                                                                               
PCOLSF25 DS    0H                  Insert     entry                             
         MVC   0(L'FLTTTXT,R7),FLTTTXT                                          
*                                                                               
         CLI   RFLTYPE,RFL14DLB    14    pointer ?                              
         BNE   PCOLSF26            No,   skip                                   
         LA    R7,3(,R7)           Yes,  skip over the  '14 '                   
         GOTO1 =A(TRANFREE)        Translate  and  find 1st  free byte          
         B     PCOLSF28                                                         
*                                                                               
PCOLSF26 DS    0H                  Normal     case                              
         CLI   0(R7),ESC#HIGH      Do    we   need translation ?                
         BH    PCOLSF27            No,   skip                                   
         GOTO1 =A(TRANSLAT)        Translate  the  text                         
*                                                                               
PCOLSF27 DS    0H                                                               
         LA    R1,L'FLTTTXT        Length     of   text                         
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
PCOLSF28 DS    0H                                                               
         MVI   0(R7),C'='          Insert     equal     sign                    
         LA    R7,1(,R7)           Move  after      equal     sign              
*                                                                               
         CLI   RFLTYPE,RFLTTYPE    Transaction     type ?                       
         BNE   PCOLSF30            No,   skip                                   
*                                  Convert    type                              
         GOTO1 =A(CNVTTYPE),DMCB,(R6),('LENFFULL',(R7))                         
         LA    R1,LENFFULL         Max   on-line   full-line                    
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
         B     PCOLSF55            Add   brake     point                        
*                                                                               
PCOLSF30 DS    0H                                                               
         TM    RFLIND,RFLXCLD      Exclude    option    on ?                    
         BZ    PCOLSF35            No,   skip                                   
         MVI   0(R7),C'*'          Enter C'*' to   indicate  exclude            
         LA    R7,1(,R7)           Move  after     C'*'                         
*                                                                               
PCOLSF35 DS    0H                                                               
         TM    FLTTSWS,FLTTSTA     Status ?                                     
         BZ    PCOLSF40            No,   skip                                   
         BAS   RE,PCOLSFST         Process    status                            
         B     PCOLSF55            Add   brake     point                        
         DROP  R5                                                               
*                                                                               
PCOLSF40 DS    0H                                                               
         CLI   RFLTYPE,RFLMTHD     Method     type ?                            
         BNE   PCOLSF45            No,   skip                                   
         BAS   RE,PCOLSFMT         Insert     method                            
         B     PCOLSF55            Add   brake     point                        
*                                                                               
PCOLSF45 DS    0H                                                               
         CLI   RFLTYPE,RFLBUDGT    Budget     type ?                            
         BNE   PCOLSF48            No,   skip                                   
         BAS   RE,PCOLSFBD         Insert     budget                            
         B     PCOLSF55            Add   brake     point                        
*                                                                               
PCOLSF48 DS    0H                                                               
*&&UK                                                                           
         CLI   RFLTYPE,RFLAUTH     Auth     status ?                            
         BNE   PCOLSF50            No,   skip                                   
         BAS   RE,PCOLAUTH         Insert     Auth status                       
         B     PCOLSF55            Add   brake     point                        
*&&                                                                             
PCOLSF50 DS    0H                                                               
         ZIC   R1,RFLLN            Get   element   length                       
         AHI   R1,-(RFLLNQ+1)      Find  data length    -1                      
         EXMVC R1,0(R7),RFLDATA    Add   the  data                              
         LA    R7,1(R1,R7)         Move  after     the  data                    
*                                                                               
PCOLSF55 DS    0H                                                               
         MVI   0(R7),BRAKE         Mark  brake     point                        
         LA    R7,1(,R7)           ->    next text location                     
         ST    R7,SVREG            Save  XP   saved     address                 
*                                                                               
*                                  ***** Next element             *****         
PCOLSF60 DS    0H                  Bump  to   next filter    element            
         ZIC   RF,RFLLN                                                         
         AR    R6,RF                                                            
         B     PCOLSF10            Test  next element                           
*                                                                               
         DROP  R6                                                               
*                                                                               
PCOLSF65 DS    0H                  Finished   reading   col  filters            
         L     R7,SVREG            ->    End       of   XP   data               
         L     R5,XPSAVE           ->    Start     of   XP   data               
         LR    RF,R7               Save  current   data address                 
         SR    R7,R5               Length     of   total     data               
         BNP   PCOLSFEX            None, skip filters,  i.e. exit               
*                                                                               
         CHI   R7,MAXXPLN*STDPGWD  Too   much data ?                            
         BNH   *+6                 No,   continue                               
         DC    H'00'               Too   much column    filter    data          
*                                                                               
         LR    R6,R5               ->    start     of   XP   data               
*                                                                               
PCOLSF70 DS    0H                  Find  brake     point                        
         CLI   0(R6),BRAKE         A     brake     point ?                      
         BE    PCOLSF75            Yes,  continue                               
         LA    R6,1(,R6)           ->    next byte                              
         BCT   R7,PCOLSF70         Test  next byte                              
*                                  Never fall through                           
*                                                                               
PCOLSF75 DS    0H                  Found brake     point                        
         SR    R6,R5               Get   size of   data                         
         CHI   R6,L'BXCCFLT        Will  it   fit  on   one  line ?             
         BH    PCOLSF80            No,   work it   out                          
         BCTR  R6,0                Minus one  for  execute                      
         EXMVC R6,BXCCFLT,0(R5)    Move  the  data                              
         B     PCOLSF95            Continue                                     
*                                                                               
PCOLSF80 DS    0H                  More  than one  line needed                  
         LR    RE,R5               Get   curr XP   data address                 
         LA    RF,L'BXCCFLT-1      Get   num  bytes     to   be   moved         
         EXMVC RF,BXCCFLT,0(RE)    Move  the  data                              
*                                  Is    there     enough    space              
         CLI   RPTLINE#,MAX#XPLN-1       to   print     both lines ?            
         BH    PCOLSF85            No,   print     what is   formatted          
         LA    R3,STDPGWD(,R3)     ->    next print     line                    
         ZIC   R1,RPTLINE#         Get   curr line number                       
         LA    R1,1(,R1)           Bump  it   by   one                          
         STC   R1,RPTLINE#         Save  curr line number                       
         B     PCOLSF90            Insert     the  rest of   the  data          
*                                                                               
PCOLSF85 DS    0H                  Write then move required                     
         MVI   RPTLINES,MAX#XPLN   Lines to   print                             
*                                  Make  sure line(s)   will fit  and           
         GOTO1 =A(CHKNWRIT)              print     what is   formatted          
         MVI   RPTLINES,0          Lines previously     formatted               
         MVI   RPTLINE#,1          Curr  line number                            
         LA    R3,XP               Reset curr col  line                         
*                                                                               
PCOLSF90 DS    0H                  Insert     the  rest of   the  data          
         LR    RF,R6               Get   num  bytes     left to   move          
         SHI   RF,L'BXCCFLT+1            minus     one                          
         AHI   RE,L'BXCCFLT        ->    from where          to   move          
         EXMVC RF,BXCCFLT+2,0(RE)  Move  the  data (indented by   2)            
         BCTR  R6,0                                                             
*                                                                               
PCOLSF95 DS    0H                  The   data was  moved                        
         LA    R5,2(R6,R5)         ->    to   next data in   XP   area          
*                                  Minus one  for  X'FF',    test if            
         SHI   R7,1                any   more data to   be   printed ?          
         BZ    PCOLSFEX            No,   exit                                   
         LA    R3,STDPGWD(,R3)     ->    next print     line                    
         ZIC   R1,RPTLINE#         Get   curr line number                       
         LA    R1,1(,R1)           Bump  it   by   one                          
         STC   R1,RPTLINE#         Save  curr line number                       
         LR    R6,R5               ->    next XP   data start                   
*                                  Any   more print     lines                   
         CLC   RPTLINE#,RPTLINES         previously     formatted ?             
         BNH   PCOLSF70            Yes,  find next brake     point              
         BCTR  R1,0                Find  lines     to   print                   
         STC   R1,RPTLINES                                                      
*                                  Make  sure line(s)   will fit  and           
         GOTO1 =A(CHKNWRIT)              print     what is   formatted          
         MVI   RPTLINES,0          Lines previously     formatted               
         MVI   RPTLINE#,1          Curr  line number                            
         LA    R3,XP               Reset curr col  line                         
         B     PCOLSF70            Find  next brake     point                   
*                                                                               
PCOLSFEX DS    0H                  ***** Exit                                   
         CLC   RPTLINES,RPTLINE#   Prev  lines     >=   curr lines              
         BNL   *+10                Yes,  skip                                   
         MVC   RPTLINES,RPTLINE#   Print curr #    of   lines                   
         LA    R3,XP               Reset curr col  line                         
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2,R3,R4                                                         
         EJECT ,                                                                
***********************************************************************         
*  Clear XPSAVE area                                                  *         
*                                                                     *         
*    Output:                                                          *         
*      XPSAVE   = XPSAVE  area                                        *         
***********************************************************************         
         SPACE 1                                                                
CLRXPSAV NTR1  ,                                                                
         LA    R0,SPACES           Clear XPSAVE    area                         
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         L     RE,XPSAVE                                                        
         LHI   RF,(MAXXPLN*STDPGWD)                                             
         MVCL  RE,R0                                                            
*                                                                               
         B     EXIT                Return     to   caller                       
         EJECT ,                                                                
***********************************************************************         
*  Display column filter status                                       *         
*                                                                     *         
*    Input:                                                           *         
*      R6       = filters data element                                *         
*      R7       = place   data here                                   *         
*                                                                     *         
*    Output:                                                          *         
*      R7       = first   free byte                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R6           Map   filters   data element                 
         SPACE 1                                                                
PCOLSFST NTR1  ,                                                                
         CLI   RFLDATA,YES                                                      
         BNE   *+10                                                             
         MVC   0(L'@ONLY,R7),@ONLY Yes,  means     "Only"                       
*                                                                               
         CLI   RFLDATA,NO                                                       
         BNE   *+10                                                             
         MVC   0(L'@NO,R7),@NO     No,   means     "No"                         
*                                                                               
         LA    R1,L'@ONLY          Use   len  of   max  data                    
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         B     EXITR7              Return     R7   to   caller                  
*                                                                               
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*  Display column filter method                                       *         
*                                                                     *         
*    Input:                                                           *         
*      R6       = filters data element                                *         
*      R7       = place   data here                                   *         
*                                                                     *         
*    Output:                                                          *         
*      R7       = first   free byte                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R6           Map   filters   data element                 
         SPACE 1                                                                
PCOLSFMT NTR1  ,                                                                
         MVC   0(1,R7),RFLDATA     Insert     number    of   method             
         GOTO1 =A(DISMTHD),DMCB,RFLDATA,(R7)                                    
*                                                                               
         LA    R1,L'METCODE        Method     code length    (3)                
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         B     EXITR7              Return     R7   to   caller                  
*                                                                               
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*  Display column filter budget                                       *         
*                                                                     *         
*    Input:                                                           *         
*      R6       = filters data element                                *         
*      R7       = place   data here                                   *         
*                                                                     *         
*    Output:                                                          *         
*      R7       = first   free byte                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING BUDRECD,R2          Map   budget    records                      
         USING RFLELD,R6           Map   filters   data element                 
         SPACE 1                                                                
PCOLSFBD NTR1  ,                                                                
         LA    R2,IOKEY            ->    I/O  key  area                         
         XC    BUDKEY,BUDKEY       Zero  out  the  area                         
         MVI   BUDKTYP,BUDKTYPQ    X'1B' budget    record    type               
         MVC   BUDKCPY,RCCOMPFL    Company    code                              
         MVC   BUDKNO1,RFLDATA     Move  in   budget    number                  
         MVC   HALF,RFLDATA        Save  the  budget    number                  
*                                  Read  the  record                            
         L     R1,=AL4(IOHIGH+IOACDIR+IOBUD)                                    
         GOTO1 AIO                                                              
*                                                                               
         L     R2,ABUDIO           ->    i/o  area                              
         CLC   HALF,BUDKNO1        Did   the  budget    key  change ?           
         BE    PCOLSFB5            No,   continue                               
*                                                                               
*                                  This  column    budget    might              
*                                        not  exist     because   it            
*                                        was  obsoleted -                       
*                                  Display    the  budget    as   a             
*                                        number    instead   of   as            
*                                        a    budget    ID   name               
         SR    R1,R1               Clear register                               
         ICM   R1,3,HALF           Get   the  budget    number                  
         CVD   R1,DUB              Convert    it   to   packed                  
         OI    DUB+7,X'0F'         Clear the  sign                              
         UNPK  0(5,R7),DUB         Convert    it   to   display                 
         MVC   5(2,R7),=C' ?'      Say   something is   wrong                   
         B     PCOLSFBX            Exit                                         
*                                                                               
PCOLSFB5 DS    0H                  Display    the  budget     ID                
         MVC   0(L'BUDKCOD,R7),BUDKCOD                                          
*                                                                               
PCOLSFBX DS    0H                                                               
         LA    R1,L'BUDKCOD        Budget     ID   length    (10)               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         B     EXITR7              Return     R7   to   caller                  
*                                                                               
         DROP  R2,R6                                                            
         EJECT ,                                                                
***********************************************************************         
*  Display column filter auth status                                  *         
*                                                                     *         
*    Input:                                                           *         
*      R6       = filters data element                                *         
*      R7       = place   data here                                   *         
*                                                                     *         
*    Output:                                                          *         
*      R7       = first   free byte                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R6           Map   filters   data element                 
         SPACE 1                                                                
*&&UK                                                                           
PCOLAUTH NTR1  ,                                                                
         MVCDD 0(6,R7),AC#BOTH     Insert "BOTH" - Default                      
         CLI   RFLDATA,C'A'        "Auth" ?                                     
         BNE   PCOLAU10                                                         
         MVCDD 0(6,R7),AC#ATHED    Insert "AUTH"                                
         B     PCOLAU20                                                         
*                                                                               
PCOLAU10 CLI   RFLDATA,C'U'        "Unauth" ?                                   
         BNE   PCOLAU20                                                         
         MVCDD 0(6,R7),AC#UATH     Insert "AUTH"                                
*                                                                               
PCOLAU20 GOTO1 =A(TRANFREE)        Translate  and  find 1st  free byte          
         B     EXITR7              Return     R7   to   caller                  
*&&                                                                             
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*  Add to column filters additional column options                    *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      SVREG    = current XP   saved address                          *         
*                                                                     *         
*    Output:                                                          *         
*      SVREG    = current XP   saved address                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         SPACE 1                                                                
PCOLSFAD NTR1  ,                   Find  additional     col  options            
*                                                                               
         BAS   RE,COLFAP           ***** Print     Y/N            *****         
         BAS   RE,COLFSO           ***** Sort order               *****         
         BAS   RE,COLFSU           ***** Stack     under          *****         
         BAS   RE,COLFDR           ***** Decimal   round          *****         
         BAS   RE,COLFRC           ***** Repeat    constant       *****         
         BAS   RE,COLFPR           ***** Print     redundant      *****         
         BAS   RE,COLFNA           ***** Negative  amounts        *****         
         BAS   RE,COLFPC           ***** Print     commas         *****         
         BAS   RE,COLFPZ           ***** Print     zero amounts   *****         
         BAS   RE,COLFUT           ***** Negative  amounts        *****         
         BAS   RE,COLFZT           ***** Print     zero totals    *****         
*                                                                               
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  Add to column filters additional column options - Print Y/N        *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      SVREG    = current XP   saved address                          *         
*                                                                     *         
*    Output:                                                          *         
*      SVREG    = current XP   saved address                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         SPACE 1                                                                
COLFAP   NTR1  ,                   ***** Print     Y/N                          
         L     R7,SVREG            ->    current   XP   saved     addr          
*                                  Hidden     column    or                      
*                                        merge     data                         
*                                             if   $COLS     are 0 ?            
         TM    RCLOPT,RCLHIDE+RCLMERGE                                          
         BNZ   COLFAP10            Yes,  print     option                       
         TM    RCLOPT2,RCLCZERO                                                 
         BZ    COLFAPEX            No,   skip print     option                  
*                                                                               
COLFAP10 DS    0H                                                               
         MVCDD 0(8,R7),AC#PRINT    "Print"                                      
         GOTO1 =A(TRANFREE)        Translate  and  find 1st  free byte          
*                                                                               
         MVI   0(R7),C'='          Insert     equals    sign                    
         LA    R7,1(,R7)           Move  after     equal     sign               
*                                                                               
         TM    RCLOPT,RCLHIDE      Hidden     column ?                          
         BZ    COLFAP20            No    skip                                   
         MVC   0(L'@NO,R7),@NO     "No"                                         
*                                                                               
         TM    RCLOPT2,RCLCZERO    Eliminate  rcd  if   col  =    0 ?           
         BZ    COLFAP30            No    skip                                   
         MVC   0(L'@NO,R7),SPACES                                               
         MVI   0(R7),C'H'          Hide                                         
         B     COLFAP40                                                         
*                                                                               
COLFAP20 DS    0H                                                               
         TM    RCLOPT2,RCLCZERO    Eliminate  rcd  if   col  =    0 ?           
         BZ    COLFAP30            No    skip                                   
         MVI   0(R7),C'C'          Hide                                         
*                                                                               
COLFAP30 DS    0H                                                               
         TM    RCLOPT,RCLMERGE     Merge data if    $COLS    are  0 ?           
         BZ    COLFAP40            No    skip                                   
         MVC   0(L'@NO,R7),SPACES                                               
         MVI   0(R7),C'M'          Merge                                        
*                                                                               
COLFAP40 DS    0H                                                               
         LA    R1,L'@NO            Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
         MVI   0(R7),BRAKE         Add   brake     point                        
         LA    R7,1(,R7)           Move  after     brake     point              
*                                                                               
COLFAPEX DS    0H                                                               
         ST    R7,SVREG            Return     curr XP   saved     addr          
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  Add to column filters additional column options - Sort order       *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      SVREG    = current XP   saved address                          *         
*                                                                     *         
*    Output:                                                          *         
*      SVREG    = current XP   saved address                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         SPACE 1                                                                
COLFSO   NTR1  ,                   ***** Sort  order                            
         L     R7,SVREG            ->    current   XP   saved     addr          
         SR    R3,R3               Clear register                               
         ICM   R3,1,RCLSORTN       Any   value ?                                
         BZ    COLFSOEX            No,   skip sort order                        
         MVCDD 0(15,R7),AC#SRTOR   "Sort order"                                 
         GOTO1 =A(TRANFREE)        Translate  and  find 1st  free byte          
*                                                                               
         MVI   0(R7),C'='          Insert     equals    sign                    
         LA    R7,1(,R7)           Move  after     equal     sign               
*                                                                               
         CVD   R3,DUB              Convert    to   packed                       
         OI    DUB+7,X'0F'         Clear sign                                   
         UNPK  0(2,R7),DUB         Convert    to   display                      
         CHI   R3,9                Number     <=   9                            
         BH    COLFSO10            No,   skip                                   
         MVC   0(1,R7),1(R7)       Left  justify   the  number                  
         MVI   1(R7),C' '          Clear trailing  byte                         
         LA    R7,1(,R7)           Adjust     current   address                 
         B     COLFSO20                                                         
*                                                                               
COLFSO10 DS    0H                                                               
         LA    R7,2(,R7)           Adjust     current   address                 
*                                                                               
COLFSO20 DS    0H                                                               
         MVI   0(R7),BRAKE         Add   brake     point                        
         LA    R7,1(,R7)           Move  after     brake     point              
*                                                                               
COLFSOEX DS    0H                                                               
         ST    R7,SVREG            Return     curr XP   saved     addr          
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  Add to column filters additional column options - Stack under      *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      SVREG    = current XP   saved address                          *         
*                                                                     *         
*    Output:                                                          *         
*      SVREG    = current XP   saved address                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         SPACE 1                                                                
COLFSU   NTR1  ,                   ***** Stack     under                        
         L     R7,SVREG            ->    current   XP   saved     addr          
         SR    R3,R3               Clear register                               
         ICM   R3,1,RCLSTACK       Any   value ?                                
         BZ    COLFSUEX            No,   skip stack     under                   
         MVCDD 0(20,R7),AC#STCKU   "Stack     under"                            
         GOTO1 =A(TRANFREE)        Translate  and  find 1st  free byte          
*                                                                               
         MVI   0(R7),C'='          Insert     equals    sign                    
         LA    R7,1(,R7)           Move  after     equal     sign               
*                                                                               
         CVD   R3,DUB              Convert    to   packed                       
         OI    DUB+7,X'0F'         Clear sign                                   
         UNPK  0(2,R7),DUB         Convert    to   display                      
         CHI   R3,9                Number     <=   9                            
         BH    COLFSU10            No,   skip                                   
         MVC   0(1,R7),1(R7)       Left  justify   the  number                  
         MVI   1(R7),C' '          Clear trailing  byte                         
         LA    R7,1(,R7)           Adjust     current   address                 
         B     COLFSU20                                                         
*                                                                               
COLFSU10 DS    0H                                                               
         LA    R7,2(,R7)           Adjust     current   address                 
*                                                                               
COLFSU20 DS    0H                                                               
         MVI   0(R7),BRAKE         Add   brake     point                        
         LA    R7,1(,R7)           Move  after     brake     point              
*                                                                               
COLFSUEX DS    0H                                                               
         ST    R7,SVREG            Return     curr XP   saved     addr          
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  Add to column filters additional column options - Decimal round    *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      SVREG    = current XP   saved address                          *         
*                                                                     *         
*    Output:                                                          *         
*      SVREG    = current XP   saved address                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         SPACE 1                                                                
COLFDR   NTR1  ,                   ***** Decimal   round                        
         L     R7,SVREG            ->    current   XP   saved     addr          
         TM    RCLOPT,RCLACCM      Is    this an   accumulated column ?         
         BZ    COLFDREX            No,   skip decimal   rounding                
         CLI   RCLDCMLS,0          Any   decimal   rounding ?                   
         BE    COLFDREX            No,   skip decimal   rounding                
         CLI   RCLDCMLS,C' '       Any   decimal   rounding ?                   
         BE    COLFDREX            No,   skip decimal   rounding                
*                                                                               
         USING FMTRECD,R5          Map   format    record                       
         L     R5,RPT@FMTR         Get   addr of   format    record             
         TM    RCLOPT,RCLPCT       Percentage column ?                          
         BO    COLFDR10            No,   skip                                   
         CLC   RCLDCMLS,FMTRNDTY   Same  as   percent   rounding  amt ?         
         BE    COLFDREX            Yes,  skip decimal   rounding                
         B     COLFDR20            Continue                                     
*                                                                               
COLFDR10 DS    0H                                                               
         CLC   RCLDCMLS,FMTPCT     Same  as   main rounding  amount ?           
         BE    COLFDREX            Yes,  skip decimal   rounding                
         DROP  R5                                                               
*                                                                               
COLFDR20 DS    0H                                                               
         L     R3,=A(DECMRTAB)     ->    decimal   rounding  table              
*                                                                               
COLFDR30 DS    0H                  Find  entry     in   the  table              
         CLI   0(R3),X'00'         End   of   table ?                           
         BE    COLFDR50            Yes,  skip                                   
         CLC   RCLDCMLS,0(R3)      Found match ?                                
         BE    COLFDR40            Insert     value                             
         LA    R3,DECMRTBQ(,R3)    Find  next entry                             
         B     COLFDR30            Check next entry                             
*                                                                               
COLFDR40 DS    0H                  Found match                                  
         LH    R4,=H'5670'         "Decimals"                                   
         CLI   1(R3),C'-'          Minus                                        
         BNE   *+8                 No,   skip                                   
         LH    R4,=H'5672'         "Rounding"                                   
*                                                                               
*                                  Get   the  text                              
         GOTO1 =A(TXTGET),DMCB,(R4),(L'BXCCFLT-4,(R7)),0,0                      
*                                                                               
         LA    R1,L'BXCCFLT-4      Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVI   0(R7),C'='          Insert     equals    sign                    
         LA    R7,1(,R7)           Move  after     equal     sign               
*                                                                               
         MVC   0(2,R7),1(R3)       Insert     value                             
*                                                                               
COLFDR50 DS    0H                                                               
         LA    R1,2                Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVI   0(R7),BRAKE         Add   brake     point                        
         LA    R7,1(,R7)           Move  after     brake     point              
*                                                                               
COLFDREX DS    0H                                                               
         ST    R7,SVREG            Return     curr XP   saved     addr          
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  Add to column filters additional column options - Repeat constant  *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      SVREG    = current XP   saved address                          *         
*                                                                     *         
*    Output:                                                          *         
*      SVREG    = current XP   saved address                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         SPACE 1                                                                
COLFRC   NTR1  ,                   ***** Repeat    constant                     
         L     R7,SVREG            ->    current   XP   saved     addr          
         TM    RCLOPT,RCLACCM      Is    this an   accumulated column ?         
         BZ    COLFRCEX            No,   skip repeat    constant                
*                                  Repeat     constant  data                    
         TM    RCLOPT3,RCLRDATY+RCLRDATN      yes  or   no ?                    
         BZ    COLFRCEX            No,   skip repeat    constant                
*                                  "Repeat    constant"                         
         GOTO1 =A(TXTGET),DMCB,5671,(L'BXCCFLT-4,(R7)),0,0                      
*                                                                               
         LA    R1,L'BXCCFLT-4      Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVI   0(R7),C'='          Insert     equals    sign                    
         LA    R7,1(,R7)           Move  after     equal     sign               
*                                                                               
         TM    RCLOPT3,RCLRDATY    Repeat     constant  data =    yes ?         
         BZ    COLFRC10            No,   skip                                   
         MVC   0(L'@YES,R7),@YES   "Yes"                                        
         B     COLFRC20            Continue                                     
*                                                                               
COLFRC10 DS    0H                                                               
         MVC   0(L'@NO,R7),@NO     "No"                                         
*                                                                               
COLFRC20 DS    0H                                                               
         LA    R1,L'@NO            Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVI   0(R7),BRAKE         Add   brake     point                        
         LA    R7,1(,R7)           Move  after     brake     point              
*                                                                               
COLFRCEX DS    0H                                                               
         ST    R7,SVREG            Return     curr XP   saved     addr          
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  Add to column filters additional column options - Print redundant  *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      SVREG    = current XP   saved address                          *         
*                                                                     *         
*    Output:                                                          *         
*      SVREG    = current XP   saved address                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         SPACE 1                                                                
COLFPR   NTR1  ,                   ***** Print     redundant                    
         L     R7,SVREG            ->    current   XP   saved     addr          
         TM    RCLOPT,RCLACCM      Is    this an   accumulated column ?         
         BO    COLFPREX            Yes,  skip print     redundant               
*                                  Print redundant data yes  or   no ?          
         TM    RCLOPT3,RCLRDATY+RCLRDATN                                        
         BZ    COLFPREX            No,   skip print     redundant               
*                                  "Print     redundant"                        
         GOTO1 =A(TXTGET),DMCB,5676,(L'BXCCFLT-4,(R7)),0,0                      
*                                                                               
         LA    R1,L'BXCCFLT-4      Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVI   0(R7),C'='          Insert     equals    sign                    
         LA    R7,1(,R7)           Move  after     equal     sign               
*                                                                               
         TM    RCLOPT3,RCLRDATY    Print redundant data =    yes ?              
         BZ    COLFPR10            No,   skip                                   
         MVC   0(L'@YES,R7),@YES   "Yes"                                        
         B     COLFPR20            Continue                                     
*                                                                               
COLFPR10 DS    0H                                                               
         MVC   0(L'@NO,R7),@NO     "No"                                         
*                                                                               
COLFPR20 DS    0H                                                               
         LA    R1,L'@NO            Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVI   0(R7),BRAKE         Add   brake     point                        
         LA    R7,1(,R7)           Move  after     brake     point              
*                                                                               
COLFPREX DS    0H                                                               
         ST    R7,SVREG            Return     curr XP   saved     addr          
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  Add to column filters additional column options - Negative amounts *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      SVREG    = current XP   saved address                          *         
*                                                                     *         
*    Output:                                                          *         
*      SVREG    = current XP   saved address                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         SPACE 1                                                                
COLFNA   NTR1  ,                   ***** Negative  amounts                      
         L     R7,SVREG            ->    current   XP   saved     addr          
         TM    RCLOPT,RCLACCM      Is    this an   accumulated column ?         
         BZ    COLFNAEX            No,   skip repeat    constant                
*                                  Any   negative  amounts   option ?           
         TM    RCLEDOPT,RCLEDTRL+RCLEDBKT+RCLEDLED+RCLEDCR                      
         BZ    COLFNAEX            No,   skip                                   
         MVCDD 0(18,R7),AC#NEGAM   "Negative  amounts"                          
         GOTO1 =A(TRANFREE)        Translate  and  find 1st  free byte          
*                                                                               
         MVI   0(R7),C'='          Insert     equals    sign                    
         LA    R7,1(,R7)           Move  after     equal     sign               
*                                                                               
         TM    RCLEDOPT,RCLEDTRL   Trailing   minus     sign ?                  
         BZ    COLFNA10            No,   skip                                   
         MVI   0(R7),C'T'          "T"                                          
         B     COLFNA50            Continue                                     
*                                                                               
COLFNA10 DS    0H                                                               
         TM    RCLEDOPT,RCLEDBKT   Minus sign as   brackets ?                   
         BZ    COLFNA20            No,   skip                                   
         MVI   0(R7),C'B'          "B"                                          
         B     COLFNA50            Continue                                     
*                                                                               
COLFNA20 DS    0H                                                               
         TM    RCLEDOPT,RCLEDLED   Leading    minus     sign ?                  
         BZ    COLFNA30            No,   skip                                   
         MVI   0(R7),C'L'          "L"                                          
         B     COLFNA50            Continue                                     
*                                                                               
COLFNA30 DS    0H                  Print minus     as   credit                  
         MVI   0(R7),C'S'                                                       
*                                                                               
COLFNA50 DS    0H                                                               
         MVI   1(R7),BRAKE         Add   brake     point                        
         LA    R7,2(,R7)           Move  after     brake     point              
*                                                                               
COLFNAEX DS    0H                                                               
         ST    R7,SVREG            Return     curr XP   saved     addr          
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  Add to column filters additional column options - Print commas     *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      SVREG    = current XP   saved address                          *         
*                                                                     *         
*    Output:                                                          *         
*      SVREG    = current XP   saved address                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         SPACE 1                                                                
COLFPC   NTR1  ,                   ***** Print     commas                       
         L     R7,SVREG            ->    current   XP   saved     addr          
         TM    RCLOPT,RCLACCM      Accumulated     column ?                     
         BZ    COLFPCEX            No,   skip                                   
*                                  Print using     commas    yes/no             
         TM    RCLEDOPT,RCLEDCMY+RCLEDCMN                                       
         BZ    COLFPCEX            No,   skip                                   
*                                  "Print     amounts   using   commas"         
         GOTO1 =A(TXTGET),DMCB,1606,(L'BXCCFLT-4,(R7)),0,0                      
*                                                                               
         LA    R1,L'BXCCFLT-4      Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVI   0(R7),C'='          Insert     equals    sign                    
         LA    R7,1(,R7)           Move  after     equal     sign               
*                                                                               
         TM    RCLEDOPT,RCLEDCMY   Print using     commas    =    yes ?         
         BZ    COLFPC10            No,   skip                                   
         MVC   0(L'@YES,R7),@YES   "Yes                                         
         B     COLFPC20            Continue                                     
*                                                                               
COLFPC10 DS    0H                                                               
         MVC   0(L'@NO,R7),@NO     "No"                                         
*                                                                               
COLFPC20 DS    0H                                                               
         LA    R1,L'@NO            Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVI   0(R7),BRAKE         Add   brake     point                        
         LA    R7,1(,R7)           Move  after     brake     point              
*                                                                               
COLFPCEX DS    0H                                                               
         ST    R7,SVREG            Return     curr XP   saved     addr          
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  Add to column filters additional column options - Print zero amount*         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      SVREG    = current XP   saved address                          *         
*                                                                     *         
*    Output:                                                          *         
*      SVREG    = current XP   saved address                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         SPACE 1                                                                
COLFPZ   NTR1  ,                   ***** Print     zero amounts                 
         L     R7,SVREG            ->    current   XP   saved     addr          
         TM    RCLOPT,RCLACCM      Accumulated     column ?                     
         BZ    COLFPZEX            No,   skip                                   
*                                  Print zeros     yes  or   no ?               
         TM    RCLEDOPT,RCLEDZRY+RCLEDZRN                                       
         BZ    COLFPZEX            No,   skip                                   
*                                                                               
         MVCDD 0(22,R7),AC#PRZAM   "Print     zero amounts"                     
         GOTO1 =A(TRANFREE)        Translate  and  find 1st  free byte          
*                                                                               
         MVI   0(R7),C'='          Insert     equals    sign                    
         LA    R7,1(,R7)           Move  after     equal     sign               
*                                                                               
         TM    RCLEDOPT,RCLEDZRY   Print zeros     =    yes ?                   
         BZ    COLFPZ10            No,   skip                                   
         MVC   0(L'@YES,R7),@YES   "Yes                                         
         B     COLFPZ20            Continue                                     
*                                                                               
COLFPZ10 DS    0H                                                               
         MVC   0(L'@NO,R7),@NO     "No"                                         
*                                                                               
COLFPZ20 DS    0H                                                               
         LA    R1,L'@NO            Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVI   0(R7),BRAKE         Add   brake     point                        
         LA    R7,1(,R7)           Move  after     brake     point              
*                                                                               
COLFPZEX DS    0H                                                               
         ST    R7,SVREG            Return     curr XP   saved     addr          
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  Add to column filters additional column options - Underline totals *         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      SVREG    = current XP   saved address                          *         
*                                                                     *         
*    Output:                                                          *         
*      SVREG    = current XP   saved address                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         SPACE 1                                                                
COLFUT   NTR1  ,                   ***** Underline totals                       
         L     R7,SVREG            ->    current   XP   saved     addr          
         TM    RCLOPT,RCLACCM      Is    this an   accumulated column ?         
         BZ    COLFUTEX            No,   skip underline totals                  
*                                  Any   underline totals    option ?           
         TM    RCLOPT3,RCLUNLNA+RCLUNLNB+RCLUNLNN                               
         BZ    COLFUTEX            No,   skip                                   
*                                  "Underline totals"                           
         GOTO1 =A(TXTGET),DMCB,1618,(L'BXCCFLT-4,(R7)),0,0                      
*                                                                               
         LA    R1,L'BXCCFLT-4      Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVI   0(R7),C'='          Insert     equals    sign                    
         LA    R7,1(,R7)           Move  after     equal     sign               
*                                                                               
         TM    RCLOPT3,RCLUNLNA    Underline  totals    above ?                 
         BZ    COLFUT10            No,   skip                                   
         MVI   0(R7),C'A'          "A"                                          
         TM    RCLOPT3,RCLUNLDB    Double     underline ?                       
         BZ    COLFUT50            No,   continue                               
         MVI   0(R7),C'D'          "D"                                          
         B     COLFUT50            Continue                                     
*                                                                               
COLFUT10 DS    0H                                                               
         TM    RCLOPT3,RCLUNLNB    Underline  totals    below ?                 
         BZ    COLFUT20            No,   skip                                   
         MVI   0(R7),C'B'          "B"                                          
         TM    RCLOPT3,RCLUNLDB    Double     underline ?                       
         BZ    COLFUT50            No,   continue                               
         MVI   0(R7),C'E'          "E"                                          
         B     COLFUT50            Continue                                     
*                                                                               
COLFUT20 DS    0H                                                               
         MVC   0(L'@NO,R7),@NO     "No"                                         
*                                                                               
COLFUT50 DS    0H                                                               
         LA    R1,L'@NO            Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVI   1(R7),BRAKE         Add   brake     point                        
         LA    R7,2(,R7)           Move  after     brake     point              
*                                                                               
COLFUTEX DS    0H                                                               
         ST    R7,SVREG            Return     curr XP   saved     addr          
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  Add to column filters additional column options - Print zero totals*         
*                                                                     *         
*    Input:                                                           *         
*      R2       = Current element                                     *         
*      SVREG    = current XP   saved address                          *         
*                                                                     *         
*    Output:                                                          *         
*      SVREG    = current XP   saved address                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R2           Map   columns   data element                 
         SPACE 1                                                                
COLFZT   NTR1  ,                   ***** Print     zero totals                  
         L     R7,SVREG            ->    current   XP   saved     addr          
         TM    RCLOPT,RCLACCM      Accumulated     column ?                     
         BZ    COLFZTEX            No,   skip                                   
*                                  Print zero totals    yes  or   no ?          
         TM    RCLOPT4,RCLZRTTY+RCLZRTTN                                        
         BZ    COLFZTEX            No,   skip                                   
*                                                                               
         MVCDD 0(22,R7),AC#PRZTO   "Print     zero totals"                      
         GOTO1 =A(TRANFREE)        Translate  and  find 1st  free byte          
*                                                                               
         MVI   0(R7),C'='          Insert     equals    sign                    
         LA    R7,1(,R7)           Move  after     equal     sign               
*                                                                               
         TM    RCLOPT4,RCLZRTTY    Print zeros     totals    =    yes ?         
         BZ    COLFZT10            No,   skip                                   
         MVC   0(L'@YES,R7),@YES   "Yes                                         
         B     COLFZT20            Continue                                     
*                                                                               
COLFZT10 DS    0H                                                               
         MVC   0(L'@NO,R7),@NO     "No"                                         
*                                                                               
COLFZT20 DS    0H                                                               
         LA    R1,L'@NO            Maximum    length    of   text               
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
*                                                                               
         MVI   0(R7),BRAKE         Add   brake     point                        
         LA    R7,1(,R7)           Move  after     brake     point              
*                                                                               
COLFZTEX DS    0H                                                               
         ST    R7,SVREG            Return     curr XP   saved     addr          
         B     EXIT                Return     to   caller                       
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DROP  R9,RB                                                            
         TITLE 'Print out specs of format - general profile'                    
***********************************************************************         
*  Insert general profile - print options                             *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*                                                                     *         
*    Uses:                                                            *         
*      RPTOPT#  - Option    number                                    *         
*      RPTLINES - Number    of   lines      to      be     printed    *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROPRT NMOD1 0,**PGPRO*                                                       
         L     RC,RLBASEC          ->    ACWORKD   dsect                        
         LR    R5,R3               ->    print     line                         
         MVI   RPTLINES,1          Say   print     one  line                    
         MVI   RPTOPT#,0           Start option    number                       
*                                                                               
*                                  Map   general   profile -                    
         USING GPPRTABD,R4               print     options   table              
*                                  ->    general   profile -                    
         L     R4,=A(GPPRTAB)            print     options   table              
*                                                                               
PGPROP10 DS    0H                  Process    next option                       
*                                                                               
         BAS   RE,PGPRONUM         Insert     number                            
*                                                                               
         TM    GPPRSWS,GPPRMSG     Message    text                              
         BO    PGPROP20            Yes,  process   message   text               
*                                  Process    dictionary     entry -            
*                                  .     emulate   a    MVCDD                   
         MVI   BXGOPT1,ESC#LFJT    Escape     left justified character          
*                                  Insert     dictionary     number             
         MVC   BXGOPT1+1(2),GPPRNUM                                             
         MVI   BXGOPT1+3,L'BXGOPT1 Insert     max  output    length             
         B     PGPROP30            Continue                                     
*                                                                               
PGPROP20 DS    0H                  Process    message   text                    
         SR    R1,R1               Get   message   number                       
         ICM   R1,3,GPPRNUM                                                     
         BAS   RE,PGPROTXT         Get   the  text                              
*                                                                               
PGPROP30 DS    0H                  Process    the  options                      
         ICM   RF,15,GPPRRTN       Get   the  routine   address                 
         BASR  RE,RF               Go    to   the  routine                      
*                                                                               
         BAS   RE,PGPRONXT         Find  next area                              
*                                                                               
         LA    R4,GPPRTABQ(,R4)    ->    next table     entry                   
         CLI   GPPRSWS,EOT         End   of   table ?                           
         BNE   PGPROP10            No,   process   next entry                   
         B     GPEXIT              Return     to   caller                       
*                                                                               
         DROP  R2,R4,R5                                                         
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "left justify"                            *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROLJT DS    0H                                                               
         BAS   R6,PGPRO@NO         Default    =    "No"                         
         TM    RPFPOPT,RPFLFJT     Left  justify ?                              
         BZ    *+8                                                              
         BAS   R6,PGPRO@YS         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print redundant totals"                  *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPRORDT DS    0H                                                               
         BAS   R6,PGPRO@NO         Default    =    "No"                         
         TM    RPFPOPT,RPFRTOT     Print redundant totals ?                     
         BZ    *+8                                                              
         BAS   R6,PGPRO@YS         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print boxes"                             *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROBOX DS    0H                                                               
         BAS   R6,PGPRO@YS         Default    =    "Yes"                        
*                                                                               
         TM    RPFPOPT3,RPFXTBOX   Print extra     box  detail ?                
         BZ    PGPROBX5                                                         
         MVC   BXGVAL1(L'@YES),SPACES                                           
         MVI   BXGVAL1,C'X'        Yes                                          
*                                                                               
PGPROBX5 DS    0H                                                               
         TM    RPFPOPT,RPFBOX      Print boxes ?                                
         BO    PGPROBXX                                                         
         TM    RPTSWS,RPTNOC4E     Any   C4   element ?                         
         BO    PGPROBXX            No,   skip                                   
         BAS   R6,PGPRO@NO         "No"                                         
*                                                                               
PGPROBXX DS    0H                                                               
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print redundant info. in columns"        *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPRORDC DS    0H                                                               
         BAS   R6,PGPRO@NO         Default    =    "No"                         
         TM    RPFPOPT,RPFRDAT     Print redundant detail ?                     
         BZ    *+8                                                              
         BAS   R6,PGPRO@YS         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print inactive accounts"                 *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROINA DS    0H                                                               
         BAS   R6,PGPRO@NO         Default    =    "No"                         
*                                                                               
         TM    RPFPOPT,RPFIACT     Print inactive  accounts ?                   
         BZ    *+8                                                              
         BAS   R6,PGPRO@YS         "Yes"                                        
*                                                                               
*                                  All   levels    plus                         
         TM    RPFIND,RPFACLVL           inactive  accounts ?                   
         BZ    PGPROIN2                                                         
         MVC   BXGVAL1(L'@NO),SPACES                                            
         MVI   BXGVAL1,C'A'        All   inactive  accounts                     
*                                                                               
PGPROIN2 DS    0H                  Eliminate  rcd  if                           
         TM    RPFIND,RPFICZRO           all  age  cols =    zero ?             
         BZ    PGPROIN4                                                         
         MVC   BXGVAL1(L'@NO),SPACES                                            
         MVI   BXGVAL1,C'C'        Check aging     cols only                    
*                                                                               
PGPROIN4 DS    0H                  Show  accounts  with                         
         TM    RPFPOPT2,RPFIABUD         budgets   only ?                       
         BZ    PGPROIN6                                                         
         MVC   BXGVAL1(L'@NO),SPACES                                            
         MVI   BXGVAL1,C'B'        Yes                                          
*                                                                               
PGPROIN6 DS    0H                  Show  accounts  with                         
         TM    RPFPOPT2,RPFIATRN         transactions   only ?                  
         BZ    PGPROIN8                                                         
         MVC   BXGVAL1(L'@NO),SPACES                                            
         MVI   BXGVAL1,C'T'        Yes                                          
*                                                                               
PGPROIN8 DS    0H                  Exit                                         
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print amounts using commas"              *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROCMA DS    0H                                                               
         BAS   R6,PGPRO@YS         Default    =    "Yes"                        
         TM    RPTSWS,RPTNOC4E     Any   C4   element ?                         
         BO    PGPROCMX            No,   skip                                   
         TM    RPFEDOPT,RPFEDCMA   Print commas ?                               
         BO    *+8                                                              
         BAS   R6,PGPRO@NO         "No"                                         
*                                                                               
PGPROCMX DS    0H                                                               
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print zero amounts"                      *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROZAM DS    0H                                                               
         BAS   R6,PGPRO@NO         Default    =    "No"                         
         TM    RPFEDOPT,RPFEDZRO   Print zeros     as   nonblank ?              
         BZ    *+8                                                              
         BAS   R6,PGPRO@YS         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print zero totals"                       *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROZTO DS    0H                                                               
         BAS   R6,PGPRO@NO         Default    =    "No"                         
         TM    RPFPOPT,RPFZEROT    Print zeros     totals ?                     
         BZ    *+8                                                              
         BAS   R6,PGPRO@YS         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "negative amounts"                        *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPRONGM DS    0H                                                               
         MVI   BXGVAL1,C'T'        Default =  trailing minus      sign          
         TM    RPTSWS,RPTNOC4E     Any   C4   element ?                         
         BO    PGPRONGX            No,   skip                                   
         TM    RPFEDOPT,RPFEDTRL   Print trailing  minus     signs ?            
         BO    PGPRONGX            Yes,  continue                               
         MVI   BXGVAL1,C'L'        Default =  leading  minus      sign          
         TM    RPFEDOPT,RPFEDLED   Print leading   minus     signs ?            
         BO    PGPRONGX            Yes,  continue                               
         MVI   BXGVAL1,C'B'        Default =  bracketed      minus sign         
         TM    RPFEDOPT,RPFEDBKT   Print bracketed minus     signs ?            
         BO    PGPRONGX            Yes,  continue                               
         MVI   BXGVAL1,C'S'        Print minus     as   "CR"                    
*                                                                               
PGPRONGX DS    0H                  Exit                                         
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print landscape or portrait"             *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
*&&US                                                                           
PGPROLOP DS    0H                                                               
         MVI   BXGVAL1,C'L'        Default =  landscape                         
*                                                                               
         TM    RPFPOPT,RPFPORT     Print portrait ?                             
         BZ    *+8                                                              
         MVI   BXGVAL1,C'P'        Print portrait                               
*                                                                               
         TM    RPFDNOPT,RPFDDOWN   Down  load only ?                            
         BZ    *+8                                                              
         MVI   BXGVAL1,C'D'        Down  load                                   
*                                                                               
         BR    RE                  Return     to   caller                       
*&&                                                                             
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print headings in upper case"            *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROUPR DS    0H                                                               
         BAS   R6,PGPRO@YS         Default    =    "Yes"                        
         TM    RPFPOPT,RPFMCASE    Print using     mixed     case ?             
         BZ    *+8                                                              
         BAS   R6,PGPRO@NO         "No"                                         
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print mids/tots across columns"          *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROXCL DS    0H                                                               
         BAS   R6,PGPRO@NO         Default    =    "No"                         
         TM    RPFPOPT2,RPFEXCOL   Print mids/tots across    columns ?          
         BZ    *+8                                                              
         BAS   R6,PGPRO@YS         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "stripes or shade in/out of boxes"        *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
*&&US                                                                           
PGPROSOS DS    0H                                                               
         BAS   R6,PGPRO@NO         Default    =    "No"                         
*                                                                               
         TM    RPFPOPT2,RPFSTRPE   Print on   striped   paper ?                 
         BZ    PGPROSO2                                                         
         MVC   BXGVAL1(L'@NO),SPACES                                            
         MVI   BXGVAL1,C'S'        Striped                                      
*                                                                               
PGPROSO2 DS    0H                                                               
         TM    RPFPOPT2,RPFISHDE   Print shade     inside    box ?              
         BZ    PGPROSO4                                                         
         MVC   BXGVAL1(L'@NO),SPACES                                            
         MVI   BXGVAL1,C'I'        Shade inside                                 
*                                                                               
PGPROSO4 DS    0H                                                               
         TM    RPFPOPT2,RPFOSHDE   Print shade     outside   box ?              
         BZ    PGPROSO6                                                         
         MVC   BXGVAL1(L'@NO),SPACES                                            
         MVI   BXGVAL1,C'O'        Shade outside                                
*                                                                               
PGPROSO6 DS    0H                                                               
         BR    RE                  Return     to   caller                       
*&&                                                                             
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print request totals"                    *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPRORQT DS    0H                                                               
         BAS   R6,PGPRO@YS         Default    =    "Yes"                        
*                                                                               
         TM    RPFPOPT3,RPFNORQT   Don't print     request   totals ?           
         BZ    *+8                                                              
         BAS   R6,PGPRO@NO         "No"                                         
*                                                                               
*                                  Print     request   total on                 
         TM    RPFPOPT2,RPFTOTSP         separate  page ?                       
         BZ    PGPRORQX                                                         
         MVC   BXGVAL1(L'@NO),SPACES                                            
         MVI   BXGVAL1,C'S'        Separate  page                               
*                                                                               
PGPRORQX DS    0H                  Exit                                         
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print division errors as "E""            *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPRODVE DS    0H                                                               
         BAS   R6,PGPRO@NO         Default    =    "No"                         
         TM    RPFIND,RPFIDERR     Print division  error     as   "E" ?         
         BZ    *+8                                                              
         BAS   R6,PGPRO@YS         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print foreign currency with prefix"      *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
*&&UK                                                                           
PGPROFCP DS    0H                                                               
         BAS   R6,PGPRO@NO         Default    =    "No"                         
         TM    RPFIND,RPFIPRFX     Print foreign   currency  prefix ?           
         BZ    *+8                                                              
         BAS   R6,PGPRO@YS         "Yes"                                        
         BR    RE                  Return     to   caller                       
*&&                                                                             
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "underline totals"                        *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROUTO DS    0H                                                               
*                                  Default    =    underline                    
         MVI   BXGVAL1,C'B'                        below     total              
*                                                                               
         TM    RPFPOPT3,RPFTPUL    Underline  above     total ?                 
         BZ    *+8                                                              
         MVI   BXGVAL1,C'A'        Underline  above                             
*                                                                               
         TM    RPFPOPT3,RPFNOUL    Don't underline totals ?                     
         BZ    *+8                                                              
         BAS   R6,PGPRO@NO         "No"                                         
*                                                                               
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "exclude non-foreign currency trans"      *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
*&&UK                                                                           
PGPROXNF DS    0H                                                               
         BAS   R6,PGPRO@NO         Default    =    "No"                         
         TM    RPFIND,RPFIXNFC     Exclude    non-foreign    curr tran?         
         BZ    *+8                                                              
         BAS   R6,PGPRO@YS         "Yes"                                        
         BR    RE                  Return     to   caller                       
*&&                                                                             
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "Number of address lines"                 *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPRO#AL DS    0H                                                               
*&&US*&& MVI   BXGVAL1,C'V'        Default =  variable                          
*&&UK*&& MVI   BXGVAL1,C'4'        Default =  4    lines                        
*                                                                               
         TM    RPFNALPR,RPFNALPV   Variable   number ?                          
         BZ    PGPRO#A5                                                         
         MVI   BXGVAL1,C'V'        Variable                                     
         B     PGPRO#AX            End   this option                            
*                                                                               
PGPRO#A5 DS    0H                  Not   variable  number                       
         CLI   RPFNALPR,X'00'      Initialized     field                        
         BE    PGPRO#AX            No,   use  the  defaults                     
         MVC   BXGVAL1(1),RPFNALPR Get   the  number   (1-4)                    
         OI    BXGVAL1,C'0'        Convert    to   display                      
*                                                                               
PGPRO#AX DS    0H                  End   option                                 
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "print column headings"                   *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROPCH DS    0H                                                               
         BAS   R6,PGPRO@YS         Default    =    "Yes"                        
         TM    RPFPOPT3,RPFNOCHD   Print column    headings ?                   
         BZ    *+8                                                              
         BAS   R6,PGPRO@NO         "No"                                         
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - "pitch size for DDS printing"             *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - "In       use"                                      *         
*      R4       - "In       use"                                      *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROPIT DS    0H                                                               
*                                  Default    =    "Auto"                       
         MVC   BXGVAL1(L'@AUTO),@AUTO                                           
         SR    R1,R1                                                            
         ICM   R1,1,RPFPITCH       Any   pitch     value ?                      
         BZ    PGPROPIX            No,   skip                                   
         MVC   BXGVAL1(L'@AUTO),SPACES                                          
         CVD   R1,DUB              Convert    to   packed                       
         OI    DUB+7,X'0F'         Clear sign                                   
         UNPK  BXGVAL1(2),DUB      Convert    to   display                      
*                                                                               
PGPROPIX DS    0H                  Exit                                         
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile "No" text into BXGVAL1                      *         
*                                                                     *         
*    Input:                                                           *         
*      R5       - Address   of   current    profile column            *         
*      R6       - return    address                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPRO@NO DS    0H                                                               
*        MVC   BXGVAL1(L'@YES),SPACES    Clear     field                        
         MVC   BXGVAL1(L'@NO),@NO        Insert    "No"                         
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile "Yes" text into BXGVAL1                     *         
*                                                                     *         
*    Input:                                                           *         
*      R5       - Address   of   current    profile column            *         
*      R6       - return    address                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPRO@YS DS    0H                                                               
         MVC   BXGVAL1(L'@NO),SPACES     Clear     field                        
         MVC   BXGVAL1(L'@YES),@YES      Insert    "Yes"                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - rounding options                          *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*                                                                     *         
*    Uses:                                                            *         
*      RPTOPT#  - Option    number                                    *         
*      RPTLINES - Number    of   lines      to      be     printed    *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPRORND NTR1  BASE=APGPRO                                                      
         LR    R5,R3               ->    print     line                         
         MVI   RPTLINES,1          Say   print     one  line                    
         MVI   RPTOPT#,0           Start option    number                       
*                                                                               
*                                  ***** Round     cash amounts                 
         BAS   RE,PGPRONUM         Insert     number                            
*                                  "Round     cash amounts"                     
         MVC   BXGOPT1(L'@RNDCA),@RNDCA                                         
*                                                                               
         L     RE,=A(RNDTAB)       ->    rounding  table                        
*                                                                               
PGPRON10 DS    0H                  Find  rounding  table     entry              
         CLI   0(RE),LANGENG       Default    language  (EOT) ?                 
         BE    PGPRON20            Yes,  stop the  loop                         
         CLC   RCLANG,0(RE)        Matching   language ?                        
         BE    PGPRON20            Yes,  use  this entry                        
         LA    RE,5(,RE)           Bump  to   next entry                        
         B     PGPRON10            Check next entry                             
*                                                                               
*                                  The   round     table   entry                
PGPRON20 DS    0H                        address   is   in RE                   
         MVC   BXGVAL1(1),1(RE)    Default,   print     to   the  penny         
         TM    RPTSWS,RPTNOC4E     Any   C4   element ?                         
         BO    PGPRON30            No,   skip                                   
         CLI   RPFRND,CPENNY       Round to   the  penny ?                      
         BE    PGPRON30            Yes,  end  this option                       
         MVC   BXGVAL1(1),2(RE)    Say   round     to   the  dollar             
         CLI   RPFRND,CDOLLAR      Round to   the  dollar ?                     
         BE    PGPRON30            Yes,  end  this option                       
         MVC   BXGVAL1(1),3(RE)    Say   round     to   $1,000 ?                
         CLI   RPFRND,CTHOUSND     Round to   the  thousands ?                  
         BE    PGPRON30            Yes,  end  this option                       
         MVC   BXGVAL1(1),4(RE)    Say   round     to   the  $1,000,000         
*                                                                               
PGPRON30 DS    0H                  End   option                                 
         BAS   RE,PGPRONXT         Find  next area                              
*                                                                               
*                                  ***** Percent   decimal   places             
         BAS   RE,PGPRONUM         Insert     number                            
*                                  "Print     %    with 0,   1,   or            
         LA    R1,1620                        2    decimal   places"            
         BAS   RE,PGPROTXT         Get   the  text                              
         MVI   BXGVAL1,C'2'        Default    to   2                            
         TM    RPTSWS,RPTNOC4E     Any   C4   element ?                         
         BO    PGPRON40            No,   skip                                   
         MVC   BXGVAL1(1),RPFPCTS  0/1/2 decimal   places                       
*                                                                               
PGPRON40 DS    0H                  End   option                                 
         BAS   RE,PGPRONXT         Find  next area                              
*                                                                               
         B     GPEXIT              Return     to   caller                       
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile option number                               *         
*                                                                     *         
*    Input:                                                           *         
*      RPTOPT#  - Option    number                                    *         
*      R5       - Address   of   current    profile column            *         
*                                                                     *         
*    Output:                                                          *         
*      RPTOPT#  - New       option    number                          *         
***********************************************************************         
         SPACE 1                                                                
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPRONUM NTR1  BASE=APGPRO                                                      
         ZIC   R2,RPTOPT#          Get   option    number                       
         LA    R2,1(,R2)           Bump  it                                     
         STC   R2,RPTOPT#          Save  it                                     
*                                  Insert     number    in   column             
         EDIT  (R2),(L'BXGNUM1,BXGNUM1),0                                       
*                                                                               
         B     GPEXIT              Return     to   caller                       
*                                                                               
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile message text                                *         
*                                                                     *         
*    Input:                                                           *         
*      R1       - Message   text number                               *         
*      R5       - Address   of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
         USING BXGPROFD,R5         Map   general   profile   print line         
         SPACE 1                                                                
PGPROTXT NTR1  BASE=APGPRO                                                      
         LA    R5,BXGOPT1          ->    message   area                         
         MVI   RPTTXTL,L'BXGOPT1   Get   message   area length                  
         BAS   RE,PGPROGTX         Output     the  text                         
*                                                                               
         B     GPEXIT              Return     to   caller                       
*                                                                               
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
*  Find next general profile option                                   *         
*                                                                     *         
*    Input:                                                           *         
*      R3       - Address   of   start of   profile columns           *         
*      R5       - Address   of   current    profile column            *         
*                                                                     *         
*    Output:                                                          *         
*      R5       - New  addr of   current    profile column            *         
***********************************************************************         
         SPACE 1                                                                
BASE     USING BXGPROFD,R3         Map   general   profile   print line         
         SPACE 1                                                                
PGPRONXT NTR1  BASE=APGPRO                                                      
         CR    R5,R3               At    1st  option    area ?                  
         BNE   PGPRONX5            No,   skip                                   
         LA    R5,BASE.BXGCOL3     ->    2nd  option    area                    
         B     PGPRONXX            Exit                                         
*                                                                               
PGPRONX5 DS    0H                  Finished   option    line                    
*                                  Make  sure line will fit                     
         GOTO1 =A(CHKNWRIT)              and  print     the  data               
         LR    R5,R3               ->    1st  option    area                    
*                                                                               
PGPRONXX DS    0H                  Exit                                         
         B     GPEXITR5            Return     R5   to   caller                  
*                                                                               
         DROP  BASE                                                             
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - ranking options                           *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*                                                                     *         
*    Uses:                                                            *         
*      RPTLINES - Number    of   lines      to      be     printed    *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGRPROD,R3               for  ranking   format                  
         SPACE 1                                                                
PGPRORNK NTR1  BASE=APGPRO                                                      
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
         LA    R5,BXGROPT1         ->    text address                           
         MVI   RPTTXTL,L'BXGROPT1  Text  length                                 
         LA    R1,1625             "Rank on   row/col"                          
         BAS   RE,PGPROGTX         Output     the  text                         
*                                                                               
         LA    R5,BXGROPT2         ->    text address                           
         MVI   RPTTXTL,L'BXGROPT2  Text  length                                 
         LA    R1,1626             "using     col"                              
         BAS   RE,PGPROGTX         Output     the  text                         
*                                                                               
         LA    R5,BXGROPT3         ->    text address                           
         MVI   RPTTXTL,L'BXGROPT3  Text  length                                 
         LA    R1,1627             "Ascending/Descending"                       
         BAS   RE,PGPROGTX         Output     the  text                         
*                                                                               
         CLI   RPFRKON,0           Any   ranking ?                              
         BZ    PGPROREX            No,   skip                                   
         MVC   BXGRVAL1(1),RPFRKON Rank  on   row/column                        
         MVC   BYTE,RPFRKON+1      Get   row/column     number                  
         CLI   RPFRKON,C'R'        Row   ?                                      
         BNE   PGPRORN5            Skip                                         
         ZIC   R1,BYTE             Subtract   one  for  row                     
         BCTR  R1,0                                                             
         STC   R1,BYTE                                                          
*                                                                               
PGPRORN5 DS    0H                  Row/Col    to   rank on                      
         EDIT  (B1,BYTE),(2,BXGRVAL1+1),0,ALIGN=LEFT                            
*                                                                               
         ZIC   R1,RPFRKCL          Using column                                 
         CVD   R1,DUB              Convert    to   packed                       
         OI    DUB+7,X'0F'         Clear sign                                   
         UNPK  BXGRVAL2(2),DUB     Convert    to   display                      
*                                                                               
*                                  Ascending/Descending                         
         MVC   BXGRVAL3(L'RPFRKOR),RPFRKOR                                      
*                                                                               
PGPROREX DS    0H                  Exit                                         
         B     GPEXIT              Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile - down-load options                         *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*                                                                     *         
*    Uses:                                                            *         
*      RPTSWS   - Report    switches                                  *         
*      RPTLINES - Number    of   lines      to      be     printed    *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPRODWN NTR1  BASE=APGPRO                                                      
         MVI   RPTLINES,1          Say   print     one  line                    
*                                  Turn  off  processing     columns            
         NI    RPTSWS,TURNOFF-RPTSCOL1-RPTSCOL2                                 
*                                                                               
*                                  Map   general   profile -                    
         USING GPDWTABD,R4               down-load options   table              
*                                  ->    general   profile -                    
         L     R4,=A(GPDWTAB)            down-load options   table              
*                                                                               
PGPROD10 DS    0H                  Process    next option                       
         TM    GPDWSWS,GPDWSKPL    Skip  a    line ?                            
         BZ    PGPROD20            No,   continue                               
*                                  Make  sure line will fit                     
         GOTO1 =A(CHKNWRIT)              and  print     the  data               
         MVI   RPTLINES,2          Say   print     two  lines                   
         GOTO1 =A(CHKSPACE)        Make  sure line both will fit                
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
         TM    RPTSWS,RPTSNPED     Started    a    new  page ?                  
         BO    PGPROD15            Yes,  skip                                   
         GOTO1 =A(PMIDLINE)        Print a    midline                           
*                                                                               
PGPROD15 DS    0H                  Turn  off  processing     columns            
         NI    RPTSWS,TURNOFF-RPTSCOL1-RPTSCOL2                                 
         LA    R4,1(,R4)           ->    next entry                             
         B     PGPROD70            Check for  end  of   table                   
*                                                                               
PGPROD20 DS    0H                  Process    column    1    or   2             
         TM    RPTSWS,RPTSCOL2     Last  time in   col  2 ?                     
         BO    PGPROD30            Yes,  need to   print     last line          
         TM    RPTSWS,RPTSCOL1     Last  time in   col  1 ?                     
         BZ    PGPROD40            No,   continue  with this line               
         TM    GPDWSWS,GPDWCOL1    This  time add  to   col  1 ?                
         BZ    PGPROD40            No,   continue  with this line               
*                                                                               
PGPROD30 DS    0H                  Print last line                              
*                                  Make  sure line will fit                     
         GOTO1 =A(CHKNWRIT)              and  print     the  data               
*                                                                               
PGPROD40 DS    0H                  Set   correct   column    indicator          
*                                  Turn  off  processing     columns            
         NI    RPTSWS,TURNOFF-RPTSCOL1-RPTSCOL2                                 
*                                                                               
         TM    GPDWSWS,GPDWCOL1    Column     1 ?                               
         BZ    *+8                 No,   skip                                   
         OI    RPTSWS,RPTSCOL1     Say   col  1                                 
*                                                                               
         TM    GPDWSWS,GPDWCOL2    Column     2 ?                               
         BZ    *+8                 No,   skip                                   
         OI    RPTSWS,RPTSCOL2     Say   col  2                                 
*                                                                               
         TM    GPDWSWS,GPDWMSG     Message    text                              
         BO    PGPROD50            Yes,  process   message   text               
*                                  Process    dictionary     entry -            
*                                  .     emulate   a    MVCDD                   
         LA    R1,BXGDOPT1         ->    option    1                            
         TM    RPTSWS,RPTSCOL1     Column     1 ?                               
         BO    *+8                 Yes,  skip                                   
         LA    R1,BXGDOPT2         ->    option    2                            
         MVI   0(R1),ESC#LFJT      Escape     left justified character          
         MVC   1(2,R1),GPDWNUM     Insert     dictionary     number             
         MVI   3(R1),L'BXGDOPT1    Insert     col  1    output    len           
         TM    RPTSWS,RPTSCOL1     Column     1 ?                               
         BO    *+8                 Yes,  skip                                   
         MVI   3(R1),L'BXGDOPT2    Insert     col  2    output    len           
         B     PGPROD60            Continue                                     
*                                                                               
PGPROD50 DS    0H                  Process    message   text                    
         SR    R1,R1               Get   message   number                       
         ICM   R1,3,GPDWNUM                                                     
         BAS   RE,PGPRDTXT         Get   the  text                              
*                                                                               
PGPROD60 DS    0H                  Process    the  options                      
         ICM   RF,15,GPDWRTN       Get   the  routine   address                 
         BASR  RE,RF               Go    to   the  routine                      
*                                                                               
         LA    R4,GPPRTABQ(,R4)    ->    next table     entry                   
*                                                                               
PGPROD70 DS    0H                                                               
         CLI   GPDWSWS,EOT         End   of   table ?                           
         BNE   PGPROD10            No,   process   next entry                   
*                                                                               
         B     GPEXIT              Return     to   caller                       
*                                                                               
         DROP  R2,R3,R4                                                         
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load option message text               *         
*                                                                     *         
*    Input:                                                           *         
*      R1       - Message   text number                               *         
*      R3       - Address   of   print      line                      *         
*      RPTSWS   - Report    switches                                  *         
*                                                                     *         
*    Uses:                                                            *         
*      RPTTXTL  - Length    of   message    text                      *         
***********************************************************************         
         SPACE 1                                                                
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPRDTXT NTR1  ,                                                                
         LA    R5,BXGDOPT1         ->    message   area 1                       
         MVI   RPTTXTL,L'BXGDOPT1  Get   message   area 1    length             
         TM    RPTSWS,RPTSCOL2     Column     2 ?                               
         BZ    PGPRDT10            No,   continue                               
         LA    R5,BXGDOPT2         ->    message   area 2                       
         MVI   RPTTXTL,L'BXGDOPT2  Get   message   area 2    length             
*                                                                               
PGPRDT10 DS    0H                                                               
         BAS   RE,PGPROGTX         Output     the  text                         
*                                                                               
         B     GPEXIT              Return     to   caller                       
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "date format"                   *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         USING DTFORMD,R6          Map   date format    table                   
         SPACE 1                                                                
PGPDWDFM DS    0H                                                               
         TM    RPTSWS,RPTNOC4E     Any   C4   element ?                         
         BO    PGPDWDFX            No,   skip                                   
*                                                                               
*&&US*&& MVI   BYTE,LANGEUS        Default    language  is   English            
*&&UK*&& MVI   BYTE,LANGEUK        Default    language  is   English            
         CLI   RCLANG,LANGGER      German ?                                     
         BNE   *+8                 No,   skip                                   
         MVI   BYTE,LANGGER        Use   German                                 
*                                                                               
         L     R6,=A(DATETAB)      ->    date format    table                   
         SR    RF,RF               Clear register                               
*                                                                               
PGPDWDF2 DS    0H                  Top   of   loop                              
         CLI   0(R6),EOT           End   of   table ?                           
         BNE   PGPDWDF4            No,   skip                                   
*                                  Use   1st  table     entry                   
         LR    R6,RF                     for  language (English/German)         
         B     PGPDWDF8            Get   date format                            
*                                                                               
PGPDWDF4 DS    0H                  Check table     entry                        
         CLC   DTFLANG,BYTE        Correct    language ?                        
         BNE   PGPDWDF6            No,   try  next entry                        
         LTR   RF,RF               First entry     for  language ?              
         BNZ   *+6                 No,   skip                                   
         LR    RF,R6               Save  1st  entry     for  language           
         CLC   RPFDTFMT,DTFCODE    Right format ?                               
         BE    PGPDWDF8            Get   date format                            
*                                                                               
PGPDWDF6 DS    0H                  Get   next entry                             
         LA    R6,DTFLNQ(,R6)      Bump  to   next entry                        
         B     PGPDWDF2            Test  the  next entry                        
*                                                                               
PGPDWDF8 DS    0H                  Found the  entry                             
*                                  Get   the  date format                       
         MVC   BXGDVAL1(L'DTFDTFMT),DTFDTFMT                                    
*                                                                               
PGPDWDFX DS    0H                  Exit                                         
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3,R6                                                         
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "decimal"                       *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWDCM DS    0H                                                               
         BAS   R6,PGPD@YES         Default    =    "Yes"                        
         TM    RPFDNOPT,RPFDNDEC   Any   decimals ?                             
         BZ    *+8                                                              
         BAS   R6,PGPD@NO          "No"                                         
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "field seperator"               *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWFLD DS    0H                                                               
*                                  Default    =    "Blank"                      
         MVC   BXGDVAL2(L'@BLNK),@BLNK                                          
         LA    R1,RPFFLDD          ->    field     separator                    
         BAS   R6,PGPDWCHR         Print field     separator                    
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "amounts in text format"        *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWTXT DS    0H                                                               
         BAS   R6,PGPD@NO          Default    =    "No"                         
         TM    RPFDNOPT,RPFDTXT#   Amounts    in   text format ?                
         BZ    *+8                                                              
         BAS   R6,PGPD@YES         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "text delimiter"                *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWEOT DS    0H                                                               
         MVI   BXGDVAL2,C'"'       Default    =    quote     sign               
         LA    R1,RPFTXTD          ->    text delimiter                         
         BAS   R6,PGPDWCHR         Print text delimiter                         
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "pad amounts w/leading zeros"   *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWPAD DS    0H                                                               
         BAS   R6,PGPD@NO          Default    =    "No"                         
         TM    RPFDNOPT,RPFDNPAD   Pad   amounts   with leading zeros ?         
         BZ    *+8                                                              
         BAS   R6,PGPD@YES         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "end of line delimiter"         *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWEOL DS    0H                                                               
         MVI   BXGDVAL2,X'5E'      Default    =    ";"                          
         LA    R1,RPFEOLD          ->    end  of   line delimiter               
         BAS   R6,PGPDWCHR         Print end  of   line delimiter               
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "end of report delimiter"       *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWEOR DS    0H                                                               
         MVI   BXGDVAL2,C':'       Default    =    ":"                          
         LA    R1,RPFEORD          ->    end  of   report    delimiter          
         BAS   R6,PGPDWCHR         Print end  of   report    delimiter          
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "fixed width fields"            *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWFXW DS    0H                                                               
         BAS   R6,PGPD@NO          Default    =    "No"                         
         TM    RPFDNOPT,RPFDNFXW   Fixed column    width ?                      
         BZ    *+8                                                              
         BAS   R6,PGPD@YES         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "format can exceed 198 chars"   *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWXMX DS    0H                                                               
         BAS   R6,PGPD@NO          Default    =    "No"                         
         TM    RPFDNOPT,RPFDDOWN   Force down-load ?                            
         BZ    *+8                                                              
         BAS   R6,PGPD@YES         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "include column headings"       *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWCOL DS    0H                                                               
         BAS   R6,PGPD@YES         Default    =    "Yes"                        
         TM    RPFDNOPT,RPFDNCOL   Include    column   headings ?               
         BZ    *+8                                                              
         BAS   R6,PGPD@NO          "No"                                         
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "include rows"                  *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWROW DS    0H                                                               
         BAS   R6,PGPD@NO          Default    =    "No"                         
         TM    RPFDNOPT,RPFDNROW   Include    rows ?                            
         BZ    *+8                                                              
         BAS   R6,PGPD@YES         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "include totals"                *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWTOT DS    0H                                                               
         BAS   R6,PGPD@NO          Default    =    "No"                         
         TM    RPFDNOPT,RPFDNTOT   Include    totals ?                          
         BZ    *+8                                                              
         BAS   R6,PGPD@YES         "Yes"                                        
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "number of address lines to     *         
*                                      down-load"                     *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWD#A DS    0H                                                               
         MVI   BXGDVAL1,C'0'       Default    =    "0"                          
         CLI   RPFNALDW,X'04'      More  than 4 ?                               
         BH    PGPDWD#X            Yes,  ignore    the  data                    
*                                  Get   num  of   address    lines             
         MVC   BXGDVAL1(L'RPFNALDW),RPFNALDW                                    
         OI    BXGDVAL1,C'0'       Convert    to   display                      
*                                                                               
PGPDWD#X DS    0H                                                               
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "transmission type"             *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWEDI DS    0H                                                               
         CLI   RPFLN,RPFLNQ        Old   element ?                              
         BE    PGPDWEDX            Yes,  skip                                   
         TM    RPFXMIT,RPFXFTP     FTP ?                                        
         BZ    PGPDWEDX            No,   skip                                   
         MVC   BXGDVAL1(3),=C'FTP'                                              
*                                                                               
PGPDWEDX DS    0H                                                               
         BR    RE                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "No" text into BXGDVAL1         *         
*                                                                     *         
*    Input:                                                           *         
*      R3       - Address   of   print      line                      *         
*      R6       - return    address                                   *         
***********************************************************************         
         SPACE 1                                                                
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPD@NO  DS    0H                                                               
*        MVC   BXGDVAL1(L'@YES),SPACES   Clear     field                        
         MVC   BXGDVAL1(L'@NO),@NO       Insert    "No"                         
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - "Yes" text into BXGDVAL1        *         
*                                                                     *         
*    Input:                                                           *         
*      R3       - Address   of   print      line                      *         
*      R6       - return    address                                   *         
***********************************************************************         
         SPACE 1                                                                
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPD@YES DS    0H                                                               
         MVC   BXGDVAL1(L'@NO),SPACES    Clear     field                        
         MVC   BXGDVAL1(L'@YES),@YES     Insert    "Yes"                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  Insert general profile down-load - character for:                  *         
*                                     "field separator",              *         
*                                     "text  delimiter",              *         
*                                     "end   of line   delimiter", or *         
*                                     "end   of report delimiter"     *         
*                                                                     *         
*    Input:                                                           *         
*      R1       - Address   of   data                                 *         
*      R2       - Address   of   profile    element                   *         
*      R3       - Address   of   print      line                      *         
*      R4       - "In       use"                                      *         
*      R6       - return    address                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load options                 
         SPACE 1                                                                
PGPDWCHR DS    0H                                                               
         CLI   RPFLN,RPFLNQ        Old   element ?                              
         BE    PGPDWCHX            Yes,  skip                                   
         MVC   BXGDVAL2,SPACES     Clear the  value                             
*                                                                               
         CLI   0(R1),0             Any   value     specified ?                  
         BNE   PGPDWC10                                                         
         MVC   BXGDVAL2(L'@NONE),@NONE   "None"                                 
         B     PGPDWCHX                                                         
*                                                                               
PGPDWC10 DS    0H                                                               
         CLI   0(R1),X'40'         Blank value     specified ?                  
         BNE   PGPDWC20                                                         
         MVC   BXGDVAL2(L'@BLNK),@BLNK   Default   =    "Blank"                 
         B     PGPDWCHX                                                         
*                                                                               
PGPDWC20 DS    0H                                                               
         MVC   BXGDVAL2(1),0(R1)   Insert     value                             
*                                                                               
PGPDWCHX DS    0H                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Get general profile message text                                   *         
*                                                                     *         
*    Input:                                                           *         
*      R1       - Message   text number                               *         
*      R5       - Address   of   message    text                      *         
*      RPTTXTL  - Length    of   message    text                      *         
***********************************************************************         
         SPACE 1                                                                
PGPROGTX NTR1  BASE=APGPRO                                                      
         LR    R2,R1               Get   message   number                       
         GOTO1 =A(TXTGET),DMCB,(R2),(RPTTXTL,(R5)),0,0                          
*                                                                               
         B     GPEXIT              Return     to   caller                       
         EJECT ,                                                                
         SPACE 1                                                                
GPEXIT   DS    0H                                                               
         XIT1  ,                   Return     to   caller                       
*                                                                               
GPEXITR5 DS    0H                  Exit                                         
         XIT1  REGS=(R5)           Return     R5   to   caller                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
         DROP  RB                                                               
         TITLE 'Print out specs of format - report profiles'                    
***********************************************************************         
*  Process report profiles                                            *         
*                                                                     *         
*    Input:                                                           *         
*      R3       - Address   of   print     line                       *         
*                                                                     *         
*    Uses:                                                            *         
*      RPTOPT#  - Option  number                                      *         
*      RPTLINES - Number  of   lines  to      be    printed           *         
*      R4       - Current report      profile entry                   *         
*      R5       - Address of   report profile field numbers entry     *         
***********************************************************************         
         SPACE 1                                                                
         USING BXRPROFD,R3         Map   report    profile   print line         
         USING PRPRF,RB            Base  reg  for  report    profile            
         SPACE 1                                                                
PRPRF    NMOD1 0,**PRPRF*                                                       
         L     RC,RLBASEC          ->    ACWORKD   dsect                        
         MVI   RPTLINES,1          Say   print     one  line                    
         MVI   RPTOPT#,0           Start option    number                       
*                                                                               
*                                  ->    report    profiles  -                  
         L     RE,=A(RPRTTAB)            report    types     table              
*                                                                               
PRPRF10  DS    0H                  Find  profile   report    table              
         CLI   0(RE),EOT           End   of   table ?                           
         BE    PRPRFEX             Yes,  exit                                   
         CLC   QPROG(1),0(RE)      Right report    type ?                       
         BE    PRPRF20             Yes,  continue                               
         LA    RE,4(,RE)           Bump  to   next entry                        
         B     PRPRF10             Try   the  next entry                        
*                                                                               
         USING RPRFTABD,R4         Map   report    profiles  table              
PRPRF20  DS    0H                  Found profile   report    table              
         SR    R4,R4                                                            
         ICM   R4,7,1(RE)          ->    profile   report    table              
         OI    RPTSWS,RPTBXLN1     First line of   the  box                     
*                                                                               
*                                  Process    profile   report                  
PRPRF30  DS    0H                        table     entries                      
         CLI   RPRFFLD#,EOT        End   of   table ?                           
         BE    PRPRFEX             Yes,  exit                                   
         TM    RPTSWS,RPTBXLN1     First line in   the  box ?                   
         BO    PRPRF40             Yes,  nothing   to   output                  
*                                  Make  sure last line will fit                
         GOTO1 =A(CHKNWRIT)              and  print     the  data               
*                                                                               
         TM    RPRFSWS1,RPRFSKIP   Skip  one  line before    field ?            
         BZ    PRPRF40             No,   skip this code                         
         MVI   RPTLINES,2          Say   print     two  lines                   
         GOTO1 =A(CHKSPACE)        Make  sure line both will fit                
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
         TM    RPTSWS,RPTSNPED     Started    a    new  page ?                  
         BO    PRPRF40             Yes,  skip                                   
         GOTO1 ACREPORT            Print a    blank     line                    
*                                                                               
PRPRF40  DS    0H                                                               
*                                  Finished   1st  line in   box                
         NI    RPTSWS,TURNOFF-RPTBXLN1                                          
*                                                                               
         TM    RPRFSWS1,RPRFRNUM   Restart    numbering ?                       
         BZ    PRPRF50             No,   skip                                   
         GOTO1 =A(CLOSEBOX)        Close the  current   box                     
         ZIC   RF,RPTSCNUM         Bump  to   next section   number             
         LA    RF,1(,RF)                                                        
         STC   RF,RPTSCNUM                                                      
*                                  Start next report    profile                 
         GOTO1 =A(SRPRFSEC)              section                                
         MVI   RPTOPT#,0           Start option    number                       
*                                                                               
PRPRF50  DS    0H                                                               
         TM    RPRFSWS1,RPRFNNUM   Do    not  number    the  field ?            
         BO    *+8                 Yes,  skip                                   
         BAS   RE,RPRFNOIT         Number     the  field                        
*                                                                               
*                                                                               
*                                  Map   report    profile -                    
         USING RPR#TABD,R5               field     numbers   table              
*                                  ->    report    profile -                    
         L     R5,=A(RPR#TAB)            field     numbers   table              
*                                                                               
*                                  Find  report    profile -                    
PRPRF60  DS    0H                        field     number    table              
         CLI   RPR#FLD#,EOT        End   of   table ?                           
         BE    PRPRF80             Yes,  skip this entry                        
         CLC   RPR#FLD#,RPRFFLD#   Right field     number    entry ?            
         BE    PRPRF70             Yes,  continue                               
         LA    R5,RPR#TABQ(,R5)    Bump  to   next entry                        
         B     PRPRF60             Try   the  next entry                        
*                                                                               
*                                  Found report    profile -                    
PRPRF70  DS    0H                        field     number    entry              
*                                  Note: when we   will print     more          
*                                        than one  text/value     in            
*                                        the  column    area,     then          
*                                        the  text num  will be   zero.         
         OC    RPR#NUM,RPR#NUM     Any   text number ?                          
         BZ    *+8                 No    text,     skip                         
         BAS   RE,RPRFTEXT         Generate   text                              
*                                                                               
*&&DO                                                                           
                                                                                
         TM    RPR#SWS,RPR#TXTO    Text  only field ?                           
         BO    PRPRF80             Yes,  skip                                   
*&&                                                                             
         BAS   RE,RPRFCALL         Call  report    profile subroutine           
*                                                                               
PRPRF80  DS    0H                                                               
         LA    R4,RPRFTABQ(,R4)    ->    next report    profile   field         
         B     PRPRF30             Check next report    profile   field         
*                                                                               
PRPRFEX  DS    0H                  Exit                                         
         B     RPEXIT              Return     to   caller                       
*                                                                               
         DROP  R3,R4,R5                                                         
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile option number                                *         
*                                                                     *         
*    Input:                                                           *         
*      RPTOPT#  - Option    number                                    *         
*      RPTSWS   - Report    switches                                  *         
*      R3       - Address   of   print     line                       *         
*                                                                     *         
*    Output:                                                          *         
*      RPTOPT#  - New       option    number                          *         
***********************************************************************         
         SPACE 1                                                                
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRFNOIT NTR1  ,                                                                
         ZIC   R2,RPTOPT#          Get   option    number                       
         LA    R2,1(,R2)           Bump  it                                     
         STC   R2,RPTOPT#          Save  it                                     
*                                                                               
         LA    R4,BXRPNUM          ->    number    column                       
*                                  Insert     number    in   column             
         EDIT  (R2),(L'BXRPNUM,(R4)),0                                          
*                                                                               
         B     RPEXIT              Return     to   caller                       
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  Generate the report profile option text                            *         
*                                                                     *         
*    Input:                                                           *         
*      RPTSWS   - Report  switches                                    *         
*      R3       - Address of   print  line                            *         
*      R5       - Address of   report profile field numbers entry     *         
***********************************************************************         
         SPACE 1                                                                
         USING BXRPROFD,R3         Map   report    profile   print line         
*                                  Map   report    profile -                    
         USING RPR#TABD,R5               field     numbers   table              
         SPACE 1                                                                
RPRFTEXT NTR1  ,                                                                
         LA    R7,BXRPOPT          ->    option    column                       
*                                                                               
         LA    R6,L'BXRPOPT        Text  length                                 
         CLI   RPR#NUML,0          Any   text length    specified ?             
         BE    *+8                 No,   skip                                   
         IC    R6,RPR#NUML         Get   text length                            
*                                                                               
*                                  Message    text or   LWSD offset             
*                                        or   HARDCTAB  offset                  
         TM    RPR#SWS,RPR#MSG+RPR#LWS+RPR#HARD                                 
         BNZ   RPRFTX20            Yes,  then not  dictionary     entry         
*                                  Process    dictionary     entry -            
*                                  .     emulate   a    MVCDD                   
         MVI   0(R7),ESC#LFJT      Escape     left justified character          
         MVC   1(2,R7),RPR#NUM     Insert     dictionary     number             
         STC   R6,3(R7)            Insert     output    length                  
         B     RPRFTXEX            Exit                                         
*                                                                               
RPRFTX20 DS    0H                  Process    message   text                    
         SR    R2,R2               Get   number    or   offset                  
         ICM   R2,3,RPR#NUM                                                     
         TM    RPR#SWS,RPR#MSG     Message    text ?                            
         BZ    RPRFTX30            No,   skip                                   
*                                  Get   message   number                       
         GOTO1 =A(TXTGET),DMCB,(R2),((R6),(R7)),0,0                             
         B     RPRFTXEX            Exit                                         
*                                                                               
RPRFTX30 DS    0H                  Process    LWSD text                         
         TM    RPR#SWS,RPR#LWS     LWSD  offset ?                               
         BO    RPRFTX40            No,   skip                                   
         AR    R2,R8               ->    LWSD entry                             
         B     RPRFTX50            Exit                                         
*                                                                               
RPRFTX40 DS    0H                  Process    HARDCTAB  text                    
         A     R2,=A(HARDCTAB)     ->    hard-code table                        
*                                                                               
RPRFTX50 DS    0H                  Insert     text                              
         BCTR  R6,0                Len   minus     one  for  EX                 
         EXMVC R6,0(R7),0(R2)      Get   the  text                              
*                                                                               
RPRFTXEX DS    0H                  Exit                                         
         B     RPEXIT              Return     to   caller                       
*                                                                               
         DROP  R3,R5                                                            
         EJECT ,                                                                
***********************************************************************         
*  Call the report profile option subroutine                          *         
*                                                                     *         
*    Input:                                                           *         
*      RPTSWS   - Report  switches                                    *         
*      RPT@PRFE - Address of   profile element                        *         
*      R3       - Address of   print   line                           *         
*      R4       - Address of   report  profile table entry            *         
*      R5       - Address of   report  profile field numbers entry    *         
***********************************************************************         
         SPACE 1                                                                
         USING BXRPROFD,R3         Map   report    profile   print line         
         USING RPRFTABD,R4         Map   report    profiles  table              
*                                  Map   report    profile -                    
         USING RPR#TABD,R5               field     numbers   table              
         SPACE 1                                                                
RPRFCALL NTR1  ,                                                                
         CLI   RPR#FTYP,0          Any   report    filter    type ?             
         BNE   RPRFCA10            Yes,  pass filters   data element            
         SPACE 1                                                                
*                                  ************************************         
*                                  *     Pass profile   data element  *         
*                                  ************************************         
         L     R2,RPT@PRFE         ->    profile   element                      
         LA    R7,BXRPVAL1         ->    value     in   col  1                  
         B     RPRFCA20            Continue                                     
         SPACE 1                                                                
*                                  ************************************         
RPRFCA10 DS    0H                  *     Pass  filters   data element *         
*                                  ************************************         
*                                                                               
         MVC   BYTE,RPR#FTYP       Save  desired   filter    type               
*                                  Find  desired   filter    element,           
         BAS   R6,RPRFFILT               element   returned  in   R2            
         LTR   R2,R2               Element    found ?                           
         BZ    RPRFCAEX            No,   exit                                   
         SPACE 1                                                                
*                                  ************************************         
RPRFCA20 DS    0H                  *     Find the  routine            *         
*                                  ************************************         
         CLI   RPR#RTN#,RPR#MAX    Routine    number    too  high ?             
         BH    RPRFCAEX            Yes,  exit                                   
*                                                                               
*                                  Map   report    profile -                    
         USING RPRRTNTD,R4               routine   numbers   table              
*                                  ->    report    profile -                    
         L     R4,=A(RPRRTNT)            routine   numbers   table              
*                                                                               
*                                  Find  report    profile -                    
RPRFCA30 DS    0H                        field     number    table              
         CLI   RPRRTN#,EOT         End   of   table ?                           
         BE    RPRFCAEX            Yes,  exit                                   
         CLC   RPRRTN#,RPR#RTN#    Right routine   number    entry ?            
         BE    RPRFCA40            Yes,  call the  routine                      
         LA    R4,RPRRTNTQ(,R4)    Bump  to   next entry                        
         B     RPRFCA30            Try   the  next entry                        
*                                                                               
*                                  Found report    profile -                    
RPRFCA40 DS    0H                        routine   number    entry              
         SPACE 1                                                                
*                                  ************************************         
*                                  *     Call the  routine            *         
*                                  ************************************         
         SR    RF,RF               Get   the                                    
         ICM   RF,7,RPR@RTN              routine   address                      
         BASR  R6,RF               Call  the  routine                           
         SPACE 1                                                                
*                                  ************************************         
RPRFCAEX DS    0H                  *     Exit                         *         
*                                  ************************************         
         B     RPEXIT              Return     to   caller                       
*                                                                               
         DROP  R3,R4,R5                                                         
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - type of report                             *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   filters data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTTYPR DS    0H                                                               
         ST    R6,SVR6             Save  R6                                     
         BAS   R6,RPRTNAME         Call  get  name field     routine            
         L     R6,SVR6             Restore    R6                                
*                                                                               
         USING RPRTTYRD,R4         Map   type of   report    table              
         L     R4,=A(RPRTTYRT)     ->    type of   report    table              
*                                                                               
RPRTTY10 DS    0H                                                               
         CLI   RPRTTYQP,EOT        End   of   table ?                           
         BE    RPRTTYEX            Yes,  exit                                   
         CLC   RPRTTYQP,QPROG      Same  JCL  character ?                       
         BNE   RPRTTY20            No,   continue  search                       
         CLC   RPRTTYUL,BXRPVAL    Same  unit/ledger                            
         BE    RPRTTY30            Yes,  found     match                        
*                                                                               
RPRTTY20 DS    0H                  Find  next entry                             
         LA    R4,RPRTTYRQ(,R4)    ->    next entry                             
         B     RPRTTY10            Check next entry                             
*                                                                               
RPRTTY30 DS    0H                  Found type of   report                       
         MVC   RPTSVRUL,RPRTTYUL   Save  report    unit/ledger                  
         MVI   BXRPVAL,ESC#LFJT    Escape     character                         
*                                  Get   dictionary     number                  
         MVC   BXRPVAL+1(L'RPRTTYD#),RPRTTYD#                                   
         MVI   BXRPVAL+3,12        Length     of   output                       
*                                                                               
RPRTTYEX DS    0H                  Exit                                         
         BR    R6                  Return     to   caller                       
         DROP  R2,R3,R4                                                         
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - specific income account(s) for INC         *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   filters data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2           Map   filters   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
RPRTSINA DS    0H                                                               
         ST    R6,SVR6             Save  return    address                      
*                                  Account    profile   data element            
*                                        for  report    types                   
         MVI   BYTE,RFLACC               income    and  suspense                
         CLC   RPTSVRUL,=AL2(UL1C) Report     type =    costing ?               
         BNE   *+8                 No,   skip                                   
         MVI   BYTE,RFLCOST        Cost  profile   data element                 
*                                                                               
         BAS   R6,RPRFFILT         Find  filter    element                      
         LTR   R2,R2               Element    found ?                           
         BZ    RPRTSINX            No,   skip                                   
         BAS   R6,RPRTNAME         Call  get  name field     routine            
*                                                                               
RPRTSINX L     R6,SVR6             Restore    return    address                 
         BR    R6                  Return     to   caller                       
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - specific cost account(s) for INC           *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   filters data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2           Map   filters   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
RPRTSCSA DS    0H                                                               
         ST    R6,SVR6             Save  return    address                      
*                                  Cost  profile   data element                 
*                                        for  report    types                   
         MVI   BYTE,RFLCOST              income    and  suspense                
         CLC   RPTSVRUL,=AL2(UL1C) Report     type =    costing ?               
         BNE   *+8                 No,   skip                                   
         MVI   BYTE,RFLACC         Account    profile   data element            
*                                                                               
         BAS   R6,RPRFFILT         Find  filter    element                      
         LTR   R2,R2               Element    found ?                           
         BZ    RPRTSCSX            No,   skip                                   
*                                                                               
         BAS   R6,RPRTNAME         Call  get  name field     routine            
*                                                                               
RPRTSCSX DS    0H                  Exit                                         
         L     R6,SVR6             Restore    return    address                 
         BR    R6                  Return     to   caller                       
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - variable type of office text               *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
*                                                                     *         
*    Output:                                                          *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
RPRTOFFV DS    0H                                                               
         ST    R6,SVR6             Save  return    address                      
*                                                                               
*                                  Determine  what text to   use                
         LA    R4,1740             Default    to   "Person   office"            
         CLI   RPFCLIOF,C'C'       Contra     security ?                        
         BNE   *+8                 No,   skip                                   
         LA    R4,1741             "Contra    office"                           
         CLI   RPFCLIOF,C'B'       Person     or   contra    security ?         
         BNE   *+8                 No,   skip                                   
         LA    R4,1742             "Person    or   contra    office"            
*                                                                               
         LA    R6,L'BXRPOPT        Text  length                                 
*                                  Get   the  text                              
         GOTO1 =A(TXTGET),DMCB,(R4),((R6),BXRPOPT),0,0                          
*                                                                               
         USING RFLELD,R2           Map   filters   data element                 
         MVI   BYTE,RFLOFF         Office     profile   data element            
*                                                                               
         BAS   R6,RPRFFILT         Find  filter    element                      
         LTR   R2,R2               Element    found ?                           
         BZ    RPRTOFVX            No,   skip                                   
         BAS   R6,RPRTNAME         Call  get  name field     routine            
*                                                                               
RPRTOFVX DS    0H                  Exit                                         
         L     R6,SVR6             Restore    return    address                 
         BR    R6                  Return     to   caller                       
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - name field                                 *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   filters data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
*                                                                     *         
*    Output:                                                          *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2           Map   filters   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
RPRTNAME DS    0H                                                               
*                                                                               
         ZIC   RF,RFLLN            Get   element   length                       
         SH    RF,=Y(RFLLNQ+1)                                                  
         BM    RPRTNAEX            Skip  if   element   too  small              
         LA    R7,BXRPVAL          ->    Value     field                        
         TM    RFLIND,RFLXCLD      Exclude    bit  on ?                         
         BZ    RPRTNA10            No,   move the  data                         
         MVI   0(R7),C'*'          Output     C'*'                              
         LA    R7,1(,R7)           Bump  the  data location                     
*                                                                               
RPRTNA10 DS    0H                  Insert     the  data                         
         EXMVC RF,0(R7),RFLDATA                                                 
*                                                                               
RPRTNAEX DS    0H                  Exit                                         
         BR    R6                  Return     to   caller                       
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - filter data element to value1 or value2    *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   filters data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
*                                                                     *         
*    Output:                                                          *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2           Map   filters   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
RPRTVALN DS    0H                                                               
*                                                                               
         ZIC   RF,RFLLN            Get   element   length                       
         SH    RF,=Y(RFLLNQ+1)                                                  
         BM    RPRTVAEX            Skip  if   element   too  small              
         LA    R7,BXRPVAL1         ->    Val  1    field                        
*                                                                               
         TM    RFLIND,RFLXCLD      Exclude    bit  on ?                         
         BZ    RPRTVA10            No,   move the  data                         
         MVI   0(R7),C'*'          Output     C'*'                              
         LA    R7,1(,R7)           Bump  the  data location                     
*                                                                               
RPRTVA10 DS    0H                  Insert     the  data                         
         EXMVC RF,0(R7),RFLDATA                                                 
*                                                                               
RPRTVAEX DS    0H                  Exit                                         
         BR    R6                  Return     to   caller                       
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - Auth status                                *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   filters data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
*                                                                     *         
*    Output:                                                          *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2           Map   filters   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
*&&UK                                                                           
RPRTAUTH DS    0H                                                               
*                                                                               
         ZIC   RF,RFLLN            Get   element   length                       
         SHI   RF,RFLLNQ+1                                                      
         BM    RPRTAUEX            Skip  if   element   too  small              
         LA    R7,BXRPVAL1         ->    Value     field                        
         MVCDD 0(L'BXRPVAL1,R7),AC#BOTH  Default - "BOTH"                       
         CLI   RFLDATA,C'A'              "Auth"?                                
         BNE   RPRTAU10                                                         
         MVCDD 0(L'BXRPVAL1,R7),AC#ATHED                                        
         B     RPRTAUEX                                                         
*                                                                               
RPRTAU10 CLI   RFLDATA,C'U'              "Unauth"?                              
         BNE   RPRTAUEX                                                         
         MVCDD 0(L'BXRPVAL1,R7),AC#UATH                                         
*                                                                               
RPRTAUEX DS    0H                  Exit                                         
         BR    R6                  Return     to   caller                       
*&&                                                                             
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - filters                                    *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
RPRTFLTS DS    0H                                                               
         ST    R6,SVR6             Save  R6                                     
*                                                                               
         LA    R7,BXRPVAL          ->    value     in   col  2                  
*                                                                               
*                                  Map   report    profile -                    
         USING BXRPFLTD,R7               filter    scheme    print area         
         MVC   BXRPF1TX,@FLT1      Get   F1   text                              
         MVC   BXRPF2TX,@FLT2      Get   F2   text                              
         MVC   BXRPF3TX,@FLT3      Get   F3   text                              
         MVC   BXRPF4TX,@FLT4      Get   F4   text                              
         MVC   BXRPF5TX,@FLT5      Get   F5   text                              
*                                                                               
         LA    R0,5                Five  filters                                
*                                  ->    filter    1    data                    
         LA    R4,RPFFLT1                in   profile   element                 
         LA    R6,BXRPF1TX         ->    filter    1    text                    
         DROP  R7                                                               
*                                                                               
RPRTFL10 DS    0H                                                               
         LR    R7,R6               ->    filter    n    text                    
         LA    R1,L'BXRPF1TX       Max   length    of   text                    
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
         MVI   0(R7),C'='          Insert     equal     sign                    
         LA    R7,1(,R7)           ->    value     area                         
         CLI   0(R4),X'40'         Any   character data ?                       
         BNH   RPRTFL20            No,   get  next filter                       
         MVC   0(1,R7),0(R4)       Insert     the  character                    
         TM    0(R4),X'40'         Lower case data ?                            
         BNZ   RPRTFL20            No,   upper     case,     get  next          
         MVI   0(R7),C'*'          Exclude    filter                            
         MVC   1(1,R7),0(R4)       Insert     the  character                    
         OI    1(R7),X'40'         Make  upper     case                         
*                                                                               
RPRTFL20 DS    0H                                                               
*                                  ->    next text field                        
         LA    R6,BXRPF2TX-BXRPF1TX(,R6)                                        
*                                  ->    next filter    data                    
         LA    R4,1(,R4)                 in   profile   element                 
         CLM   R0,1,=AL1(2)        Next  is   F5 ?                              
         BNE   *+8                 No,   skip                                   
         LA    R4,RPFFLT5          Yes,  ->   F5   value                        
         BCT   R0,RPRTFL10         Check next filter                            
*                                                                               
RPRTFLEX DS    0H                  Exit                                         
         L     R6,SVR6             Restore    R6                                
         BR    R6                  Return     to   caller                       
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - contra  filters                            *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2           Map   filters   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
RPRTCFLT DS    0H                                                               
         ST    R6,SVR6             Save  R6                                     
*                                                                               
         LA    R7,BXRPVAL          ->    value     in   col  2                  
*                                                                               
*                                  Map   report    profile -                    
         USING BXRPFLTD,R7               filter    scheme    print area         
         MVC   BXRPF1TX,@CTRF1     Get   CF1  text                              
         MVC   BXRPF2TX,@CTRF2     Get   CF2  text                              
         MVC   BXRPF3TX,@CTRF3     Get   CF3  text                              
         MVC   BXRPF4TX,@CTRF4     Get   CF4  text                              
         MVC   BXRPF5TX,@CTRF5     Get   CF5  text                              
*                                                                               
         LA    R0,5                Five  contra    filters                      
         MVI   BYTE,RFLCFLT1       Get   contra    filter    1    code          
         LA    R4,BXRPF1TX         ->    contra    filter    1    text          
         DROP  R7                                                               
*                                                                               
RPRTCF10 DS    0H                                                               
         LR    R7,R4               ->    filter    n    text                    
         LA    R1,L'BXRPF1TX       Max   length    of   text                    
         GOTO1 =A(FINDFREE)        Find  1st  free byte                         
         MVI   0(R7),C'='          Insert     equal     sign                    
         LA    R7,1(,R7)           ->    value     area                         
         BAS   R6,RPRFFILT         Find  filter    element                      
         LTR   R2,R2               Element    found ?                           
         BZ    RPRTCF30            No,   skip                                   
         ZIC   RF,RFLLN            Get   element   length                       
         SH    RF,=Y(RFLLNQ+1)                                                  
         BM    RPRTCF30            Skip  if   element   too  small              
         TM    RFLIND,RFLXCLD      Exclude    bit  on ?                         
         BZ    RPRTCF20            No,   move the  data                         
         MVI   0(R7),C'*'          Output     C'*'                              
         LA    R7,1(,R7)           Bump  the  data location                     
*                                                                               
RPRTCF20 DS    0H                  Insert     the  data                         
         EXMVC RF,0(R7),RFLDATA                                                 
*                                                                               
RPRTCF30 DS    0H                                                               
*                                  ->    next text field                        
         LA    R4,BXRPF2TX-BXRPF1TX(,R4)                                        
         ZIC   RF,BYTE             Bump  to   next filter    type               
         LA    RF,1(,RF)                 i.e. next contra    filter             
         STC   RF,BYTE                                                          
         BCT   R0,RPRTCF10         Check next contra    filter                  
*                                                                               
RPRTCFLX DS    0H                  Exit                                         
         L     R6,SVR6             Restore    R6                                
         BR    R6                  Return     to   caller                       
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - transaction type                           *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   filters data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2           Map   filters   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
RPRTTRAN DS    0H                                                               
         ST    R6,SVR6             Save  return    address                      
         LA    R7,BXRPVAL          ->    value     in   col  2                  
         GOTO1 =A(CNVTTYPE),DMCB,(R2),(L'BXRPVAL,(R7))                          
         L     R6,SVR6             Restore    return    address                 
         BR    R6                  Return     to   caller                       
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - method                                     *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   filters data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2           Map   filters   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
RPRTMTHD DS    0H                                                               
         ST    R6,SVR6             Save  return    address                      
         LA    R7,BXRPVAL          ->    value     in   col  2                  
         GOTO1 =A(DISMTHD),DMCB,RFLDATA,(R7)                                    
         L     R6,SVR6             Restore    return    address                 
         BR    R6                  Return     to   caller                       
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - location status                            *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   filters data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2           Map   filters   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
RPRTLOCS DS    0H                                                               
         CLI   RFLDATA,C' '        Any   data ?                                 
         BNH   RPRTLOCX            No,   exit                                   
*                                                                               
         USING LOCSTATD,R4         Map   location  status    table              
         L     R4,=A(LOCSTATB)     ->    location  status    table              
*                                                                               
RPRTLO10 DS    0H                                                               
         CLI   LOCSTACD,EOT        End   of   table ?                           
         BE    RPRTLOCX            Yes,  exit                                   
         CLC   LOCSTACD,RFLDATA    Same  code ?                                 
         BE    RPRTLO20            Yes,  get  the  value                        
         LA    R4,LOCSTALQ(,R4)    Get   next entry                             
         B     RPRTLO10            Check next entry                             
*                                                                               
RPRTLO20 DS    0H                  Found location  table     entry              
         LA    R7,BXRPVAL          ->    output    value     area               
         TM    RFLIND,RFLXCLD      Exclude    bit  on ?                         
         BZ    RPRTLO30            No,   move the  data                         
         MVI   0(R7),C'*'          Output     C'*'                              
         LA    R7,1(,R7)           Bump  the  data location                     
*                                                                               
RPRTLO30 DS    0H                  Insert     the  data                         
         MVC   0(L'LOCSTADD,R7),LOCSTADD                                        
*                                                                               
RPRTLOCX DS    0H                  Exit                                         
         BR    R6                  Return     to   caller                       
         DROP  R2,R3,R4                                                         
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - payroll code                               *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   filters data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      RPTSWS   - Report  switches                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2           Map   filters   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
RPRTPAYC DS    0H                                                               
         ST    R6,SVR6             Save  return    address                      
*                                                                               
         ZIC   R0,RFLLN            Get   num  of   codes     in                 
         AHI   R0,-RFLLNQ                the  element                           
         BNP   RPRTPYEX            Skip  if   element   too  small              
         LA    R7,BXRPVAL          ->    Value     field                        
         TM    RFLIND,RFLXCLD      Exclude    bit  on ?                         
         BZ    RPRTPY10            No,   get  the  data                         
         MVI   0(R7),C'*'          Output     C'*'                              
         LA    R7,1(,R7)           Bump  the  data location                     
*                                                                               
RPRTPY10 DS    0H                                                               
         LA    R4,RFLDATA          ->    Data                                   
         L     R5,PYCBASE          ->    payroll   table                        
         LR    RE,R5               Get   number    of   entries                 
         AHI   RE,-2                                                            
*                                        The  num  of   entries   are           
*                                        in   the  last part of   the           
         LH    R6,0(,RE)                 table's   label                        
*                                                                               
RPRTPY20 DS    0H                  Check a    filter    data entry              
         LR    RF,R6               Number     of   entries   in   table         
         LR    R1,R5               ->    payroll   table                        
*                                                                               
         USING PYRD,R1             Map   payroll   table     entry              
RPRTPY30 DS    0H                  Find  data entry     in   table              
         CLC   PYRNUM,0(R4)        Same  number ?                               
         BE    RPRTPY40            Yes,  found     match                        
         LA    R1,PYRENLNQ(,R1)    Get   next table     entry                   
         BCT   RF,RPRTPY30         Check next table     entry                   
         B     RPRTPY50            Get   next filter    data entry              
*                                                                               
RPRTPY40 DS    0H                  Found data entry     in   table              
*                                  Copy  payroll   code to   output             
         MVC   0(L'PYRCODE,R7),PYRCODE                                          
         DROP  R1                                                               
*                                                                               
         LA    R1,L'PYRCODE        Get   length    of   payroll   code          
*                                  Find  1st  free byte and  add  comma         
         GOTO1 =A(PUTCOMMA)              R7   passed    and  returned           
*                                                                               
RPRTPY50 DS    0H                  Get   next filter    data entry              
         LA    R4,1(,R4)           ->    next filter    data entry              
         BCT   R0,RPRTPY20         Process    the  data entry                   
*                                                                               
*                                  Finished   all  entries                      
         BCTR  R7,0                Clear the  extra     comma                   
         MVI   0(R7),C' '          Note: if   we   did  not  find               
*                                        any  entries   we   just               
*                                        cleared   an   only asterisk           
*                                        (if  exclude   on)  or                 
*                                        the  space     before                  
*                                        BXRPVAL.                               
*                                                                               
RPRTPYEX DS    0H                  Exit                                         
         L     R6,SVR6             Restore    return    address                 
         BR    R6                  Return     to   caller                       
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include reversals                          *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
RPRTIREV DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFROPT,RPFXRVRS    Include    reversals ?                       
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFROPT,RPFORVRS    Reversals  only ?                            
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include accrual reversals                  *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTIACR DS    0H                                                               
         BAS   R5,RPRT@NO          Default    =    "No"                         
         TM    RPFROPT,RPFTYP56    Include    reversals type 56 ?               
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include locked accounts                    *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTILCK DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFROPT,RPFXLOCK    Exclude    locked    accounts ?              
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFROPT,RPFOLOCK    Only  locked    accounts ?                   
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - only show outstanding items                *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTSHWO DS    0H                                                               
         BAS   R5,RPRT@NO          Default    =    "No"                         
         TM    RPFROPT,RPFBALO     Outstanding     balance   only ?             
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - cash receipts &  cash disbursement         *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTCSHR DS    0H                                                               
         BAS   R5,RPRT@NO          Default    =    "No"                         
*                                  Specs for  cash disbursement                 
         TM    RPFOPT2,RPFSPEC                receipts ?                        
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include draft transactions                 *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTDRFT DS    0H                                                               
         BAS   R5,RPRT@NO          Default    =    "No"                         
*                                                                               
         TM    RPFOPT1,RPFIDRFT    Include    draft     transactions ?          
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
         TM    RPFOPT1,RPFODRFT    Only       draft     transactions ?          
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include held items                         *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTIHLD DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT1,RPFXHELD    Include    held/queried   items ?            
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT1,RPFOHELD    Only       held items ?                      
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include held/queried items                 *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTIHLQ DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT1,RPFXHELD    Include    held/queried   items ?            
         BZ    RPRTIHQ3                                                         
         BAS   R5,RPRT@NO          "No"                                         
         B     RPRTIHQX                                                         
*                                                                               
RPRTIHQ3 DS    0H                                                               
         SR    R1,R1               Clear      register                          
         TM    RPFOPT1,RPFOHELD    Only       held items ?                      
         BZ    RPRTIHQ5                                                         
         LA    R1,1(,R1)                                                        
         MVC   0(L'@NO,R7),SPACES  Clear field                                  
         MVI   0(R7),C'H'          "H"                                          
*                                                                               
RPRTIHQ5 DS    0H                                                               
         TM    RPFOPT2,RPFOQRED    Only       queried   items ?                 
         BZ    RPRTIHQ7                                                         
         LA    R1,1(,R1)                                                        
         MVC   0(L'@NO,R7),SPACES  Clear field                                  
         MVI   0(R7),C'Q'          "Q"                                          
*                                                                               
RPRTIHQ7 DS    0H                                                               
         CHI   R1,2                Both  held and  queried ?                    
         BL    RPRTIHQX                                                         
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
RPRTIHQX DS    0H                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include cleared items                      *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
*&&UK                                                                           
RPRTICLR DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT3,RPFXCLRD    Exclude    cleared    items ?                
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT3,RPFOCLRD    Only  cleared   items ?                      
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*&&                                                                             
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include approved items                     *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTIAPR DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT1,RPFXAPRV    Exclude    approved   items ?                
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT1,RPFOAPRV    Only  approved  items ?                      
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - only show credit payees                    *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTOCRP DS    0H                                                               
         BAS   R5,RPRT@NO          Default    =    "No"                         
*                                                                               
*                                  Show  credit   (negative) balances           
         TM    RPFROPT,RPFBALCR          only ?                                 
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - only show cash discount items              *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTOCD$ DS    0H                                                               
         BAS   R5,RPRT@NO          Default    =    "No"                         
*                                                                               
         TM    RPFOPT2,RPFOCD$     Show  only cash discount  items ?            
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include offset items                       *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTIOFF DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT1,RPFXOFFS    Exclude    offset    items ?                 
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT1,RPFOOFFS    Include    only offset    items ?            
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include PAY=NO vendors                     *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTPVND DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT2,RPFXVNDR    Exclude    PAY=NO    vendors ?               
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT2,RPFOVNDR    Include    only PAY=NO    vendors ?          
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include urgent items                       *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTURGN DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT2,RPFXUGNT    Exclude    urgent    items ?                 
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT2,RPFOUGNT    Include    only urgent    items ?            
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include accounts analyzed by department    *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTINLD DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT3,RPFXADPT    Exclude    analysis  department ?            
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT3,RPFOADPT    Include    only analysis department?         
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include accounts analyzed by staff         *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTINLS DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT3,RPFXASTF    Exclude    analysis  staff ?                 
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT3,RPFOASTF    Include    only analysis  staff ?            
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include accounts analyzed by client        *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTINLC DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT3,RPFXACLI    Exclude    analysis  by   client ?           
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT3,RPFOACLI    Include    only analysis  by client?         
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - estimate status                            *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTESTA DS    0H                                                               
*                                  Estimate   status                            
         MVC   0(L'RPFESTST,R7),RPFESTST                                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - type of balance                            *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTTBAL DS    0H                                                               
*                                  Type  of   balance   =                       
         TM    RPFROPT,RPFBALCR          credit    balances  only ?             
         BZ    *+10                No,   skip                                   
         MVCDD 0(L'BXRPVAL1,R7),AC#CR    "Credit"                               
*                                                                               
*                                  Type  of   balance   =                       
         TM    RPFROPT,RPFBALDR           debit    balances  only ?             
         BZ    *+10                No,   skip                                   
         MVCDD 0(L'BXRPVAL1,R7),AC#DR    "Debit"                                
*                                                                               
*                                  Type  of   balance   =                       
         TM    RPFROPT,RPFBALO           +    or   -    balances  only?         
         BZ    RPRTTBA5            No,   skip                                   
         MVC   0(L'BXRPVAL1,R7),SPACES                                          
         MVC   0(5,R7),=C'+ | -'                                                
*                                                                               
RPRTTBA5 DS    0H                                                               
*                                  Type  of   balance   =                       
         TM    RPFROPT,RPFBALZR          zero balances       only ?             
         BZ    *+10                No,   skip                                   
         MVCDD 0(L'BXRPVAL1,R7),AC#ZERO  "Zero"                                 
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include utilized transactions              *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTIUTR DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         CLI   RPFBLTRN,C' '       Include    utilized  transactions ?          
         BNH   RPRTIUTX            Yes,  exit                                   
         CLI   RPFBLTRN,C'Y'       Test  for  yes                               
         BE    RPRTIUTX            Yes,  exit                                   
         CLI   RPFBLTRN,C'N'       Test  for  no                                
         BNE   RPRTIUT3            No,   skip                                   
         BAS   R5,RPRT@NO          "No"                                         
         B     RPRTIUTX            Exit                                         
*                                                                               
RPRTIUT3 DS    0H                                                               
         CLI   RPFBLTRN,C'O'       Test  for  only                              
         BNE   RPRTIUT6            No,   skip                                   
         BAS   R5,RPRT@ONY         "Only"                                       
         B     RPRTIUTX            Exit                                         
*                                                                               
RPRTIUT6 DS    0H                                                               
         MVC   0(L'BXRPVAL1,R7),SPACES                                          
*                                  Bill  transaction                            
         MVC   0(L'RPFBLTRN,R7),RPFBLTRN                                        
*                                                                               
RPRTIUTX DS    0H                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include closed jobs                        *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTICLS DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT4,RPFXCLSE    Exclude    closed    jobs ?                  
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT4,RPFOCLSE    Include    only closed    jobs ?             
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include expense jobs                       *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
*&&US                                                                           
RPRTIEXP DS    0H                                                               
         BAS   R5,RPRT@NO          Default = "No"                               
         TM    RPFOPT4,RPFIEXPS    Include    expense   jobs ?                  
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
         CLI   QPROG,PAYABLES      Pay scrbie ?                                 
         BNE   RPRTIX10            No                                           
         BAS   R5,RPRT@YES         Yes, Default = "Yes"                         
         TM    RPFOPT4,RPFXEXPS    Exclude expense jobs ?                       
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
RPRTIX10 TM    RPFOPT4,RPFOEXPS    Include    only expense   jobs ?             
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
         BR    R6                  Return     to   caller                       
*&&                                                                             
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include CBIL W/Os and transfers            *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTICWO DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT4,RPFXCWOT    Exclude    W/Os and  transfers ?             
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
*                                  Include    only CBIL W/Os and                
         TM    RPFOPT4,RPFOCWOT               transfers ?                       
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include estimates                          *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTIEST DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT4,RPFXESTO    Exclude    jobs with estimates ?             
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT4,RPFOESTO    Include    only job  with estimates?         
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include purchase orders                    *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTIPOS DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT5,RPFXPORD    Exclude    purchase  orders ?                
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT5,RPFOPORD    Include    purchase  orders ?                
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - merge purchase orders                      *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTMPOS DS    0H                                                               
         BAS   R5,RPRT@NO          Default    =    "No"                         
*                                                                               
         TM    RPFOPT5,RPFMRGPO    Merge purchase  orders ?                     
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - job ageing method                          *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTAGMD DS    0H                                                               
*                                  Default    =    "DEFAULT"                    
         MVCDD 0(L'BXRPVAL1,R7),AC#RSAGD                                        
*                                                                               
         CLI   RPFAGMTH,RPFAGADT              Activity  date  ageing ?          
         BNE   *+10                                                             
         MVCDD 0(L'BXRPVAL1,R7),AC#RSAGB      "BALANCE"                         
*                                                                               
         CLI   RPFAGMTH,RPFAGFIF              FIFO            ageing ?          
         BNE   *+10                                                             
         MVCDD 0(L'BXRPVAL1,R7),AC#RSAGG      "FIFO"                            
*                                                                               
         CLI   RPFAGMTH,RPFAGOPN              Open item       ageing ?          
         BNE   *+10                                                             
         MVCDD 0(L'BXRPVAL1,R7),AC#RSAGO      "OPEN"                            
*                                                                               
         CLI   RPFAGMTH,RPFAGUBL              Unbilled  item  ageing ?          
         BNE   *+10                                                             
         MVCDD 0(L'BXRPVAL1,R7),AC#RSAGU      "UNBILLED"                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include 99 with WC group or type filters   *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRT99WC DS    0H                                                               
         BAS   R5,RPRT@NO          Default    =    "No"                         
*                                                                               
*                                  Include    WC   99   with group or           
         TM    RPFOPT5,RPFIWC99               type filters ?                    
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - report  bank reconciled items              *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTRCNC DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
*                                  Include    bank reconciled                   
         TM    RPFOPT5,RPFXBRI                items ?                           
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
*                                  Only       bank reconciled                   
         TM    RPFOPT5,RPFOBRI                items ?                           
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - time sheet worksheet                       *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTTMWK DS    0H                                                               
         BAS   R5,RPRT@NO          Default    =    "No"                         
*                                                                               
         TM    RPFPOPT3,RPFTEDIT   Include    time worksheet ?                  
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - include saved time sheets                  *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTSVTM DS    0H                                                               
         BAS   R5,RPRT@NO          Default    =    "No"                         
*                                                                               
         TM    RPFOPT5,RPFISVTM    Include    saved     time sheets ?           
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
*                                  Include    only saved                        
         TM    RPFOPT5,RPFOSVTM               time sheets ?                     
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - office security                            *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTOFSC DS    0H                                                               
*                                  Default    =    "Person"                     
         MVCDD 0(L'BXRPVAL1,R7),AC#PRSN                                         
*                                                                               
         CLI   RPFCLIOF,C'C'       Client     office ?                          
         BNE   *+10                                                             
         MVCDD 0(L'BXRPVAL1,R7),AC#CLINT "Client"                               
*                                                                               
         CLI   RPFCLIOF,C'B'       Both  office    and  client ?                
         BNE   *+10                                                             
         MVCDD 0(L'BXRPVAL1,R7),AC#BOTH  "Both"                                 
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
*&&US                                                                           
***********************************************************************         
*  Insert report profile - Time by Day                                *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTFPDD DS    0H                                                               
         BAS   R5,RPRT@NO          Default    =    "No"                         
*                                                                               
         TM    RPFOPT7,RPFFPDRD    Filter Period date range by day?             
         BZ    *+8                                                              
         BAS   R5,RPRT@YES         "Yes"                                        
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
*&&                                                                             
*&&UK                                                                           
***********************************************************************         
*  Insert report profile - Include Jobs locked from Estimates         *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTJLES DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT9,RPFXJLES    Exclude    jobs locked Estimates?            
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT8,RPFOJLES    Include    jobs locked Estimates?            
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - Include Jobs locked from Orders            *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTJLOR DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT9,RPFXJLOR    Exclude    jobs locked Orders?               
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT8,RPFOJLOR    Include    jobs locked Orders?               
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - Include Jobs locked from Billing           *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTJLBI DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT9,RPFXJLBI    Exclude    jobs locked Billing?              
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT8,RPFOJLBI    Include    jobs locked Billing?              
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - Include Jobs locked from Time              *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTJLTI DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT9,RPFXJLTI    Exclude    jobs locked Time?                 
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT8,RPFOJLTI    Include    jobs locked Time?                 
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - Include Jobs locked from Adjustments       *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTJLAD DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT9,RPFXJLAD    Exclude    jobs locked Adjustments?          
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT8,RPFOJLAD    Include    jobs locked Adjustments?          
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - Include Jobs locked from 3rd Party         *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTJLEX DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT9,RPFXJLEX    Exclude    jobs locked 3rd Party?            
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT8,RPFOJLEX    Include    jobs locked 3rd Party?            
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  Insert report profile - Include Jobs locked from Sub-job           *         
*                                                                     *         
*    Input:                                                           *         
*      R2       - Address of   profile data element                   *         
*      R3       - Address of   print   line                           *         
*      R6       - return  address                                     *         
*      R7       - Address of   value   field                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R2           Map   profile   data element                 
         USING BXRPROFD,R3         Map   report    profile   print line         
         SPACE 1                                                                
RPRTSUBJ DS    0H                                                               
         BAS   R5,RPRT@YES         Default    =    "Yes"                        
*                                                                               
         TM    RPFOPT9,RPFXSUBJ    Exclude    jobs locked Sub-job?              
         BZ    *+8                                                              
         BAS   R5,RPRT@NO          "No"                                         
*                                                                               
         TM    RPFOPT8,RPFOSUBJ    Include    jobs locked Sub-job?              
         BZ    *+8                                                              
         BAS   R5,RPRT@ONY         "Only"                                       
*                                                                               
         BR    R6                  Return     to   caller                       
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
*&&                                                                             
***********************************************************************         
*  Insert report profile "No" text into value field                   *         
*                                                                     *         
*    Input:                                                           *         
*      R5       - Return    address                                   *         
*      R7       - Address   of   current    value   field             *         
***********************************************************************         
         SPACE 1                                                                
RPRT@NO  DS    0H                                                               
*        MVC   0(L'@YES,R7),SPACES Clear field                                  
         MVC   0(L'@NO,R7),@NO     Insert     "No"                              
*                                                                               
         BR    R5                  Return     to   caller                       
         SPACE 3                                                                
***********************************************************************         
*  Insert report profile "Yes" text into value field                  *         
*                                                                     *         
*    Input:                                                           *         
*      R5       - Return    address                                   *         
*      R7       - Address   of   current    value   field             *         
***********************************************************************         
         SPACE 1                                                                
RPRT@YES DS    0H                                                               
         MVC   0(L'@NO,R7),SPACES  Clear field                                  
         MVC   0(L'@YES,R7),@YES   Insert     "Yes"                             
*                                                                               
         BR    R5                  Return     to   caller                       
         SPACE 3                                                                
***********************************************************************         
*  Insert report profile "Only" text into value field                 *         
*                                                                     *         
*    Input:                                                           *         
*      R5       - Return    address                                   *         
*      R7       - Address   of   current    value   field             *         
***********************************************************************         
         SPACE 1                                                                
RPRT@ONY DS    0H                                                               
*        MVC   0(L'@NO,R7),SPACES  Clear field                                  
         MVC   0(L'@ONLY,R7),@ONLY Insert     "Only"                            
*                                                                               
         BR    R5                  Return     to   caller                       
         EJECT ,                                                                
***********************************************************************         
*  Find the filter data element                                       *         
*                                                                     *         
*    Input:                                                           *         
*      AIO2     - Address of   record                                 *         
*      BYTE     - Filters data element     number                     *         
*      R6       - Return  address                                     *         
*                                                                     *         
*    Output                                                           *         
*      R2       - address of   filters     data element or            *         
*               - zero    if   not  found                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2           Map   filters   data element                 
         SPACE 1                                                                
RPRFFILT DS    0H                                                               
         L     R2,AIO2             ->    record                                 
*                                                                               
         USING RFLELD,R2           Map   filters   data element                 
         AH    R2,DATADISP         ->    1st  element                           
*                                                                               
RPRFFI10 DS    0H                  Find  filters   data element                 
         CLI   0(R2),0             End   of   record ?                          
         BE    RPRFFING            Yes,  not  found     exit                    
         CLI   0(R2),RFLELQ        X'C5' filters   data element ?               
         BNE   RPRFFI20            No,   skip this element                      
         CLI   RFLSEQ,0            High  level     filter ?                     
         BNE   RPRFFI20            No,   skip this element                      
         CLC   RFLTYPE,BYTE        Desired    filter    type ?                  
         BE    RPRFFIEX            Yes,  exit found                             
*                                                                               
RPRFFI20 DS    0H                  Find  the  next element                      
         SR    RF,RF               Bump  to   next element                      
         IC    RF,RFLLN                                                         
         AR    R2,RF                                                            
         B     RPRFFI10            Test  the  next element                      
         SPACE 1                                                                
*                                                                               
RPRFFING DS    0H                  Not   found     exit                         
         SR    R2,R2               Clear                                        
*                                                                               
RPRFFIEX DS    0H                  Exit                                         
         BR    R6                  Return     R2   to   caller                  
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
         SPACE 1                                                                
RPEXIT   DS    0H                  Exit                                         
         XIT1  ,                   Return     to   caller                       
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
         DROP  RB                                                               
         TITLE 'Print out specs of format - Subroutines'                        
         SPACE 1                                                                
*                                  Dummy subroutine     -                       
SUBROS   NMOD1 0,**SUBRO*                base for  the  other                   
*        B     SUBREXIT                  subroutines                            
         SPACE 3                                                                
***********************************************************************         
*  Display the method code                                            *         
*                                                                     *         
*    Input:                                                           *         
*      Parm 1:                                                        *         
*        Bytes 0 - 3 = address of   method number                     *         
*      Parm 2:                                                        *         
*        Bytes 0 - 3 = address to   store  the    method code         *         
***********************************************************************         
         SPACE 1                                                                
*                                  Map   cost allocation     history            
         USING CAHRECD,R2                record                                 
         SPACE 1                                                                
DISMTHD  NTR1  BASE=ASUBROS                                                     
         L     R3,0(,R1)           ->    method    number                       
         LA    R3,0(,R3)           Clear the  high order     byte               
         L     R7,4(,R1)           ->    addr to   store     the  code          
         LA    R7,0(,R7)           Clear the  high order     byte               
*                                                                               
         LA    R2,IOKEY            ->    I/O  key  area                         
         MVC   CAHKEY,SPACES                                                    
*                                  X'3E' cost allocation     history            
         MVI   CAHKTYP,CAHKTYPQ          record    type                         
*                                  X'01' cost allocation     history            
         MVI   CAHKSUB,CAHKSUBQ          sub-record                             
         MVC   CAHKCPY,RCCOMPFL    Company    code                              
         MVC   CAHKMTHD,0(R3)      Move  in   method                            
         XC    CAHKOFC,CAHKOFC     Clear office/office  group                   
*                                  Read  the  record                            
         L     R1,=AL4(IOREAD+IOACFIL+IOBUD)                                    
         GOTO1 AIO                                                              
         BNE   DISMTHEX            Not   found,    exit                         
*                                                                               
         USING METELD,R1           Map   methods   element                      
         L     R1,ABUDIO           ->    i/o  area                              
         AH    R1,DATADISP         ->    1st  element                           
*                                                                               
DISMTH10 DS    0H                                                               
         CLI   0(R1),0             End   of   record ?                          
         BE    DISMTHEX            Yes,  exit                                   
         CLI   METEL,METELQ        X'82' method    of   allocation el ?         
         BE    DISMTH20            Yes,  found                                  
         ZIC   RE,METLN            Get   element   Length                       
         AR    R1,RE               Get   next element                           
         B     DISMTH10            Test  next element                           
*                                                                               
DISMTH20 DS    0H                  Move  in   method    code                    
         MVC   0(L'METCODE,R7),METCODE                                          
*                                                                               
DISMTHEX DS    0H                                                               
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
*  Start a report profile section                                     *         
*                                                                     *         
*    Input:                                                           *         
*      QPROG    - Scribe  JCL       type                              *         
*      RPTSCNUM - boxes   section   number                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R4         Map   wide print     areas                   
         SPACE 1                                                                
*                                  Start a    report    profile                 
SRPRFSEC NTR1  BASE=ASUBROS              box  section                           
         L     R4,VBIGPRNT         ->    wide print     areas                   
*                                  Force new  page if   the  following          
*                                        will not  fit:                         
*                                  1.    blank     line                         
*                                  2.    screen    name line                    
*                                  3.    box  title     line                    
*                                  4.    box  headers                           
*                                  5.    box  midline                           
*                                  6-7.  two  entries                           
*                                  8.    box  close     line                    
         MVI   RPTLINES,8          ..                                           
         BAS   RE,CHKSPACE         Make  sure lines     will fit                
         MVI   RPTLINES,1          Say   print     one  line                    
*                                                                               
         TM    RPTSWS,RPTSNPED     Started    a    new  page ?                  
         BO    SRPRFS10            Yes,  skip                                   
         GOTO1 ACREPORT            Skip  a    line                              
*                                                                               
SRPRFS10 DS    0H                                                               
         CLI   QPROG,PRODUCTION    Production ?                                 
         BNE   SRPRFS20            No,   continue                               
         CLI   RPTSCNUM,1          First screen ?                               
         BE    SRPRFS20            Yes,  continue                               
         MVC   RPTPRF#,=AL2(AC#RS492)    "PROD2PRO"                             
*                                                                               
SRPRFS20 DS    0H                                                               
         MVC   RPTSCRNN,SPACES     Clear the  screen    name                    
         OI    RPTSWS,RPTSUCTR     Upper case translate                         
         LA    R7,RPTSCRNN         ->    the  screen    name field              
         MVI   0(R7),ESC#LFJT      Insert     profile   name                    
         MVC   1(2,R7),RPTPRF#     Get   report    profile   DD   num           
         MVI   3(R7),8                                                          
         BAS   RE,TRANFREE         Translate  and  find 1st  free byte          
         BAS   RE,PRTSCRNN         Print the  screen    name                    
*                                                                               
         LA    R3,XP               ->    report    line                         
         LA    R7,1(,R3)           ->    start     of   text                    
*                                                                               
*                                  "Report    profile"                          
         GOTO1 =A(TXTGET),DMCB,5673,(40,(R7)),0,0                               
         CLI   QPROG,PRODUCTION    Production ?                                 
         BE    SRPRFS30            Yes,  add  section   number                  
*                                  Note: there     will be   more               
         B     SRPRFS40            Continue                                     
*                                                                               
SRPRFS30 DS    0H                  Add   section   number                       
         LA    R1,40               Max   num  of   bytes     used               
         BAS   RE,FINDFREE         Find  1st  free byte                         
         MVI   1(R7),C'-'          Insert     dash                              
         LA    R7,3(,R7)           ->    start     of   text                    
*                                  "Section   number"                           
         MVC   0(L'@SCNUM,R7),@SCNUM                                            
         LA    R1,L'@SCNUM         Max   num  of   bytes     used               
         BAS   RE,FINDFREE         Find  1st  free byte                         
*                                  Insert     section   number                  
         MVC   1(L'RPTSCNUM,R7),RPTSCNUM                                        
*                                  Convert    BIN  TO   Display  (1-9)          
         OI    1+L'RPTSCNUM-1(R7),C'0'                                          
*                                                                               
SRPRFS40 DS    0H                  Exit                                         
         GOTO1 ACREPORT            Print the  section   heading                 
*                                                                               
         MVI   RPTBXTYP,RPTBRPRO   Open  a    report    profile   box           
         BAS   RE,OBOXTYPE         Open  box  based     on   box  type          
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  Print the screen name (centered)                                   *         
*                                                                     *         
*    Input:                                                           *         
*      RPTSCRNN - Screen  name                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R4         Map   wide print     areas                   
         SPACE 1                                                                
*                                  Start a    report    profile                 
PRTSCRNN NTR1  BASE=ASUBROS              box  section                           
         L     R4,VBIGPRNT         ->    wide print     areas                   
         MVC   XP(2),=C'**'        "**"                                         
         LA    R7,XP+3             Get   screen    name                         
         MVC   0(L'RPTSCRNN,R7),RPTSCRNN                                        
         LA    R1,L'RPTSCRNN       Length     of   text                         
         BAS   RE,FINDFREE         Find  1st  free byte                         
*                                                                               
         LA    R7,1(,R7)           Skip  one  space                             
         MVC   0(L'$SCRN,R7),$SCRN "SCREEN"                                     
         LA    R1,L'$SCRN          Length     of   text                         
         BAS   RE,FINDFREE         Find  1st  free byte                         
*                                                                               
         MVC   1(2,R7),=C'**'      "**"                                         
         LA    R7,XP               ->    line                                   
         ZIC   R2,RPTPGWID         Get   page width                             
*                                  Center     the  screen    name               
         GOTO1 CENTER,DMCB,(R7),(R2)                                            
         GOTO1 ACREPORT            Print the  screen    name                    
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  Check if there is enough space on the page to print the next set   *         
*  of lines, and then print the data.                                 *         
***********************************************************************         
         SPACE 1                                                                
*                                  See   if   there     is   space              
*                                        on   the  page to   print              
*                                        the  line(s) and  then                 
CHKNWRIT NTR1  BASE=ASUBROS              print     them                         
         BAS   RE,CHKSPACE         Make  sure line will fit                     
         GOTO1 ACREPORT            Output     the  data                         
         B     SUBREXIT            Return     to   caller                       
         EJECT ,                                                                
***********************************************************************         
*  Check if there is enough space on the page to print the next set   *         
*  of lines.                                                          *         
*                                                                     *         
*    Input:                                                           *         
*      RPTLINES - Number of   lines to    print                       *         
*      RPTBXTYP - Type   of   box   being printed                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R4         Map   wide print     areas                   
         SPACE 1                                                                
*                                  See   if   there     is   space              
*                                        on   the  page to   print              
CHKSPACE NTR1  BASE=ASUBROS              the  line(s)                           
         L     R4,VBIGPRNT         ->    wide print     areas                   
*                                                                               
*                                  Turn  off  started   a    new  page          
         NI    RPTSWS,TURNOFF-RPTSNPED                                          
*                                                                               
         ZIC   RE,LINE             Get   current   line number                  
         ZIC   RF,RPTLINES         Add   num  of   lines     to   print         
         AR    RE,RF                                                            
         CLI   RPTBXTYP,RPTBNONE   In    a    box ?                             
         BE    *+8                 No,   skip                                   
         LA    RE,1(,RE)           Add   one  for  close     box                
         CLM   RE,1,MAXLINES       Will  they fit ?                             
         BNH   CHKSPAEX            Yes,  continue                               
         MVC   SVXP1,XP            Save  XP   lines                             
         MVC   SVXP2,XPSECOND                                                   
         MVC   SVXP3,XPTHIRD                                                    
         MVC   SVXP4,XPFOURTH                                                   
         MVC   XP,XSPACES          Clear XP   lines                             
         MVC   XPSECOND,XSPACES    Clear XP   lines                             
         MVC   XPTHIRD,XSPACES     Clear XP   lines                             
         MVC   XPFOURTH,XSPACES    Clear XP   lines                             
*                                                                               
         MVC   RPTSVBXT,RPTBXTYP   Save  current   box  type                    
*                                                                               
         CLI   RPTBXTYP,RPTBNONE   In    a    box ?                             
         BE    CHKSPA10            No,   skip                                   
         BAS   RE,CLOSEBOX         Close the  current   box                     
*                                                                               
CHKSPA10 DS    0H                                                               
         BAS   RE,NEWPG            Force new  page                              
*                                                                               
         OI    RPTSWS,RPTSNPED     Started    a    new  page                    
*                                                                               
         CLI   RPTSVBXT,RPTBNONE   In    a    box ?                             
         BE    CHKSPA50            No,   restore   XP   lines                   
         CLI   RPTSVBXT,RPTBXTMX   Past  last open box  routine ?               
         BH    CHKSPA50            Yes,  skip                                   
         LA    R7,XP+1                                                          
*                                  Insert     screen    name                    
         MVC   0(L'RPTSCRNN,R7),RPTSCRNN                                        
         LA    R1,L'RPTSCRNN       Length     of   text                         
         BAS   RE,FINDFREE         Find  1st  free byte                         
         LA    R7,1(,R7)           Blank                                        
         MVC   0(L'@CONTD,R7),@CONTD     "Continued"                            
         GOTO1 ACREPORT            Print Screen    name Continued               
*                                                                               
         MVC   RPTBXTYP,RPTSVBXT   Restore    current   box  type               
         BAS   RE,OBOXTYPE         Open  box  based     on   box  type          
*                                                                               
CHKSPA50 DS    0H                                                               
         MVC   XP,SVXP1            Restore    XP   lines                        
         MVC   XPSECOND,SVXP2                                                   
         MVC   XPTHIRD,SVXP3                                                    
         MVC   XPFOURTH,SVXP4                                                   
*                                                                               
CHKSPAEX DS    0H                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  Start new page                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R4         Map   wide print     areas                   
         USING FMTRECD,R5          Map   format    record                       
         USING BOXD,R6             Map   box  definitions                       
         SPACE 1                                                                
NEWPG    NTR1  BASE=ASUBROS        Start a    new  page                         
         L     R4,VBIGPRNT         ->    wide print     areas                   
         L     R5,RPT@FMTR         Get   addr of   format    record             
         L     R6,ADBXAREA         ->    box  work area                         
         MVI   FORCEHED,YES        Force new  page                              
         MVC   XHEAD1,XSPACES      Clear header    area                         
         MVC   XHEAD2,XSPACES      Clear header    area                         
         MVC   XHEAD3,XSPACES      Clear header    area                         
*                                                                               
         MVI   RCSUBPRG,1          Indicate   subprogram     number             
*                                  Note: ACREPRL01 inserts   into               
*                                        col  2    of   XHEAD1:                 
*                                             Run on (date) at (time)           
*                                                                               
         MVI   BOXFONT,1           Pitch 12                                     
         CLI   QPROG+1,PORTRAIT    Portrait   printing ?                        
         BNE   *+8                 No,   skip                                   
         MVI   BOXFONT,2           Pitch 15                                     
         SPACE 1                                                                
*                                  ************************************         
*                                  *     Header    line 1             *         
*                                  ************************************         
*                                  *---- H1   center              ----*         
*                                  *     Get  col  for  center        *         
         LA    R2,PGWDRL12         Length     of   landscape line               
         CLI   QPROG+1,PORTRAIT    Portrait   printing ?                        
         BNE   *+8                 No,   skip                                   
         LA    R2,PGWDRP15         Length     of   portrait  line               
         SRL   R2,1                Length     divided   by   two                
*                                                                               
*                                  *     report    name               *         
         LA    R7,XHEAD1(R2)       ->    header    column                       
         AHI   R7,-25              Allow for  50   byte report    name          
*                                                                               
*                                  "Scribe    Format    Design  Report"         
         GOTO1 =A(TXTGET),DMCB,5674,(50,(R7)),0,0                               
*                                                                               
         GOTO1 CENTER,DMCB,(R7),50 Center     the  report    name               
*                                                                               
*                                  *---- H1   right               ----*         
*                                  *     Get  col  for  right     side*         
*                                  Landscape  col  for  right     side          
*                                  Note:                                        
*                                  *     we   need 17   bytes     for:          
*                                  *          Print within=No                   
*                                  *     with                                   
*                                  *          12   for  Print-within            
*                                  *           1   for  =                       
*                                  *           4   for  No                      
         LA    R3,PGWDRL12-17                                                   
         CLI   QPROG+1,PORTRAIT    Portrait   printing ?                        
         BNE   *+8                 No,   skip                                   
         LA    R3,PGWDRP15-17      Portrait   col  for  right side              
*                                                                               
*                                  *     page number                  *         
         LA    R7,XHEAD1(R3)       ->    header    column                       
*                                                                               
         MVC   0(L'@PAGE,R7),@PAGE "Page"                                       
         EDIT  (B2,PAGE),(3,L'@PAGE+1(R7)),0,ZERO=BLANK                         
         SPACE 1                                                                
*                                  ************************************         
*                                  *     Header    line 2             *         
*                                  ************************************         
*                                  *---- H2   left                ----*         
*                                  *     Company   name               *         
         LA    R7,XHEAD2+1         ->    3rd  header    line                    
         MVCDD 0(7,R7),AC#CPY      "Company"                                    
         BAS   RE,TRANFREE         Translate  and  find 1st  free byte          
         MVI   0(R7),C':'          ":"                                          
         LA    R7,XHEAD2+10        Bump  over colon     and  a    space         
*                                                                               
         USING ACRL2D,RE           Map   Scribe    work area 2                  
         L     RE,AACRL2D          ->    Scribe    work area 2                  
*                                  Get   company   name                         
         MVC   0(L'COMPNAME,R7),COMPNAME                                        
         DROP  RE                                                               
*                                  *---- H2   center              ----*         
*                                  *     report    type               *         
         LA    R7,XHEAD2(R2)       ->    header    column                       
         SHI   R7,15               Allow for  30   byte report    type          
*                                                                               
         TM    RECAPSW,RECAPON     Any   recapping ?                            
         BZ    NEWPG30             No,   skip recap     data                    
         CLI   FMTNUM,1            Main  format ?                               
         BNE   NEWPG10             No,   skip                                   
*                                  "Main report"                                
         GOTO1 =A(TXTGET),DMCB,5668,(30,(R7)),0,0                               
         B     NEWPG20             Continue                                     
*                                                                               
NEWPG10  DS    0H                  Recap report                                 
         MVC   BYTE,FMTNUM         Get   format    number                       
         OI    BYTE,C'0'           Make  numeric                                
*                                  "Recap     report    number"   n             
         GOTO1 =A(TXTGET),DMCB,5669,(30,(R7)),(L'BYTE,BYTE),0                   
*                                                                               
NEWPG20  DS    0H                                                               
         GOTO1 CENTER,DMCB,(R7),30 Center     the  report    type               
*                                                                               
*                                  *---- H2   right               ----*         
*                                  *     print     within             *         
         CLI   FMTNUM,1            Main  format ?                               
         BE    NEWPG30             Yes,  skip                                   
         LA    R7,XHEAD2(R3)       ->    header    column                       
*                                                                               
         MVCDD 0(12,R7),AC#PRWIN   "Print     Within"                           
         BAS   RE,TRANFREE         Translate  and  find 1st  free byte          
*                                                                               
         MVI   0(R7),C'='          Insert     equal     sign                    
         LA    R7,1(,R7)                                                        
*                                                                               
         MVC   0(L'@YES,R7),@YES   Default    =    "Yes"  (interleaved)         
         TM    FMTRCAP,FMTRSEP     Interleaved ?                                
         BZ    *+10                Yes,  skip                                   
         MVC   0(L'@NO,R7),@NO     "No"                                         
*                                                                               
         SPACE 1                                                                
*                                  ************************************         
*                                  *     Header    line 3             *         
*                                  ************************************         
*                                  *---- H3   left                ----*         
NEWPG30  DS    0H                  *     Format    code               *         
         LA    R7,XHEAD3+1         ->    3rd  header    line                    
         MVCDD 0(6,R7),AC#FRMAT    "Format"                                     
         BAS   RE,TRANFREE         Translate  and  find 1st  free byte          
         MVI   0(R7),C':'          ":"                                          
         LA    R7,XHEAD3+10        Bump  over colon     and  a    space         
*                                  Get   format    code                         
         MVC   0(L'FMTCODE,R7),FMTCODE                                          
         CLI   FMTNAME,X'40'       Any   format    name ?                       
         BL    NEWPG50             No,   skip                                   
         LA    R1,L'FMTCODE        Max   num  of   bytes     used               
         BAS   RE,FINDFREE         Find  1st  free byte                         
         MVI   1(R7),C'-'          Insert     a    blank/dash/blank             
         LA    R7,3(,R7)           Bump  over the  blank/dash/blank             
*                                                                               
*                                  Get   format    name                         
         MVC   0(L'FMTNAME,R7),FMTNAME                                          
*                                                                               
         LR    RE,R7               ->    format    name                         
         LA    RF,L'FMTNAME        Length     of   name                         
*                                                                               
NEWPG40  DS    0H                  Check character                              
         CLI   0(RE),X'40'         Null  ?                                      
         BNL   *+8                 No,   continue                               
         MVI   0(RE),X'40'         Use   space                                  
         LA    RE,1(,RE)           ->    next character                         
         BCT   RF,NEWPG40          Test  next character                         
*                                                                               
*                                  *---- H3   right               ----*         
NEWPG50  DS    0H                  *     profile   type                         
         CLI   FMTNUM,1            Main  format ?                               
         BE    NEWPGEX             Yes,  skip                                   
         LA    R7,XHEAD3(R3)       ->    header    column                       
*                                                                               
         MVI   0(R7),ESC#LFJT      Insert     profile   name                    
         MVC   1(2,R7),RPTPRF#     Get   report    profile   DD   num           
         MVI   3(R7),8                                                          
         BAS   RE,TRANFREE         Translate  and  find 1st  free byte          
*                                                                               
         MVI   0(R7),C'='          Insert     equal     sign                    
         LA    R7,1(,R7)                                                        
*                                                                               
         MVCDD 0(6,R7),AC#RSMAN    Default    =    "Main"                       
         TM    FMTRCAP,FMTRPROF    Use   recap     program's profile ?          
         BZ    *+10                No,   skip                                   
         MVCDD 0(6,R7),AC#RSRPT    "Report"                                     
         BAS   RE,TRANSLAT         Translate                                    
*                                                                               
NEWPGEX  DS    0H                                                               
         GOTO1 ACREPORT            Write header    line                         
         MVI   BOXFONT,0           Pitch 10                                     
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R4,R5,R6                                                         
         EJECT ,                                                                
***********************************************************************         
*  Open a box based on the box type                                   *         
*                                                                     *         
*    Input:                                                           *         
*      RPTLINES - Number of   lines to    print                       *         
***********************************************************************         
         SPACE 1                                                                
*                                  Open  a    box  based     on                 
OBOXTYPE NTR1  BASE=ASUBROS              box  type                              
         L     RE,=A(OBOXRTAB)     ->    open box  routine   table              
         ZIC   RF,RPTBXTYP         Get   saved     box  type                    
         BCTR  RF,0                Minus 1                                      
         SLL   RF,2                Times 4                                      
         L     RF,0(RF,RE)         ->    open box  routine                      
         BASR  RE,RF               Open  the  box                               
*                                                                               
         B     SUBREXIT            Return     to   caller                       
         EJECT ,                                                                
***********************************************************************         
*  Open a headers box                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING BXHEAD,R3           Map   headers   print     line               
         USING BIGPRNTD,R4         Map   wide print     areas                   
         USING BOXD,R6             Map   box  definitions                       
         SPACE 1                                                                
OHEADBOX NTR1  ,                   Open  a    headers   box                     
         L     R6,ADBXAREA         ->    box  work area                         
         LA    R3,BOXCOLS          ->    box  definitions                       
         L     R4,VBIGPRNT         ->    wide print     areas                   
*                                                                               
         MVI   BOXFONT,0           Pitch 10                                     
         CLI   QPROG+1,PORTRAIT    Portrait   printing ?                        
         BNE   *+8                 No,   skip                                   
         MVI   BOXFONT,1           Pitch 12                                     
*                                                                               
         BAS   RE,OPENBOX          Open  a    box                               
*                                                                               
         MVI   BXHLEFT,C'L'                                                     
         MVI   BXHCOL1,C'C'                                                     
         MVI   BXHRGHT,C'R'                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R3,XP                                                            
         MVC   BXHTYPE(L'@TYPE1),@TYPE1   "Type"                                
         MVC   BXHKEYWD(L'@KEYWD),@KEYWD "Keyword"                              
         GOTO1 ACREPORT                                                         
*                                                                               
         BAS   RE,PMIDLINE         Print a    midline                           
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R3,R4,R6                                                         
         EJECT ,                                                                
***********************************************************************         
*  Open a rows  box                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING BXROWD,R3           Map   rows print     line                    
         USING BIGPRNTD,R4         Map   wide print     areas                   
         USING BOXD,R6             Map   box  definitions                       
         SPACE 1                                                                
OROWSBOX NTR1  ,                   Open  a    rows box                          
         L     R6,ADBXAREA         ->    box  work area                         
         LA    R3,BOXCOLS          ->    box  definitions                       
         L     R4,VBIGPRNT         ->    wide print     areas                   
*                                                                               
         MVI   BOXFONT,0           Pitch 10                                     
*                                                                               
         BAS   RE,OPENBOX          Open  a    box                               
*                                                                               
         MVI   BXRLEFT,C'L'                                                     
         MVI   BXRCOL1,C'C'                                                     
         MVI   BXRCOL2,C'C'                                                     
         MVI   BXRCOL3,C'C'                                                     
         MVI   BXRCOL4,C'C'                                                     
         MVI   BXRRGHT,C'R'                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R3,XP                                                            
         MVC   BXRNUM(L'@NUM),@NUM       "Num"                                  
         LA    R7,BXRKEYWD                                                      
         MVC   0(L'@KEYWD,R7),@KEYWD     "Keyword"                              
         LA    R1,L'@KEYWD               Length     of   data                   
         BAS   RE,FINDFREE               Find  1st  free byte                   
         MVC   0(L'COMMA,R7),COMMA       ","                                    
         MVC   1(L'@ATTR,R7),@ATTR       "Attribute"                            
         MVC   BXRTYPE(L'@TYPE1),@TYPE1  "Type"                                 
         MVC   BXRTOTL(L'@TOTLS),@TOTLS  "Totals"                               
         MVC   BXRPRFX(L'@PREFX),@PREFX  "Prefix"                               
         GOTO1 ACREPORT                                                         
*                                                                               
         BAS   RE,PMIDLINE         Print a    midline                           
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R3,R4,R6                                                         
         EJECT ,                                                                
***********************************************************************         
*  Open a columns box                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING BXCOLD,R3           Map   columns   print     line               
         USING BIGPRNTD,R4         Map   wide print     areas                   
         USING BOXD,R6             Map   box  definitions                       
         SPACE 1                                                                
OCOLSBOX NTR1  ,                   Open  a    columns   box                     
         L     R6,ADBXAREA         ->    box  work area                         
         LA    R3,BOXCOLS          ->    box  definitions                       
         L     R4,VBIGPRNT         ->    wide print     areas                   
*                                                                               
         MVI   BOXFONT,2           Pitch 15                                     
         CLI   QPROG+1,PORTRAIT    Portrait   printing ?                        
         BNE   OCOLSB20            No,   skip                                   
         MVI   BOXPGDF#,2          Page  def  #    2                            
         MVI   BOXFONT,0           Pitch 18                                     
*                                                                               
OCOLSB20 DS    0H                                                               
         BAS   RE,OPENBOX          Open  a    box                               
*                                                                               
         MVI   BXCLEFT,C'L'                                                     
         MVI   BXCCOL1,C'C'                                                     
         MVI   BXCCOL2,C'C'                                                     
         MVI   BXCCOL3,C'C'                                                     
         MVI   BXCCOL4,C'C'                                                     
         MVI   BXCCOL5,C'C'                                                     
         MVI   BXCCOL6,C'C'                                                     
         MVI   BXCCOL7,C'C'                                                     
         MVI   BXCRGHT,C'R'                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R3,XP                                                            
         MVC   BXCNUM(L'@NUM),@NUM       "Num"                                  
         LA    R7,BXCKEYWD                                                      
         MVC   0(L'@KEYWD,R7),@KEYWD     "Keyword"                              
         LA    R1,L'@KEYWD               Length     of   data                   
         BAS   RE,FINDFREE               Find  1st  free byte                   
         MVC   0(L'COMMA,R7),COMMA       ","                                    
         MVC   1(L'@ATTR,R7),@ATTR       "Attribute"                            
         MVC   BXCRNGE(L'@RANGE),@RANGE  "Range"                                
         MVC   BXCTOTL(L'@TOTLS),@TOTLS  "Totals"                               
         MVC   BXCWDTH(L'@WIDE),@WIDE    "Wide"                                 
         MVC   BXCHEAD1(L'@HEAD1),@HEAD1 "Heading  1"                           
         MVC   BXCHEAD2(L'@HEAD2),@HEAD2 "Heading  2"                           
*                                        "Options  and  filters"                
         GOTO1 =A(TXTGET),DMCB,5675,(L'BXCCFLT,BXCCFLT),0,0                     
         GOTO1 ACREPORT                                                         
*                                                                               
         BAS   RE,PMIDLINE         Print a    midline                           
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R3,R4,R6                                                         
         EJECT ,                                                                
***********************************************************************         
*  Open a general profile box                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING BXGPROFD,R3         Map   general   profile   print line         
         USING BIGPRNTD,R4         Map   wide print     areas                   
         USING BOXD,R6             Map   box  definitions                       
         SPACE 1                                                                
OGPROBOX NTR1  ,                   Open  a    general   profile   box           
         L     R6,ADBXAREA         ->    box  work area                         
         LA    R3,BOXCOLS          ->    box  definitions                       
         L     R4,VBIGPRNT         ->    wide print     areas                   
*                                                                               
         MVI   BOXFONT,0           Pitch 10                                     
         CLI   QPROG+1,PORTRAIT    Portrait   printing ?                        
         BNE   *+8                 No,   skip                                   
         MVI   BOXFONT,2           Pitch 15                                     
*                                                                               
         BAS   RE,OPENBOX          Open  a    box                               
*                                                                               
         MVI   BXGLEFT,C'L'                                                     
         MVI   BXGCOL1,C'C'                                                     
         MVI   BXGCOL2,C'C'                                                     
         MVI   BXGCOL3,C'C'                                                     
         MVI   BXGCOL4,C'C'                                                     
         MVI   BXGCOL5,C'C'                                                     
         MVI   BXGRGHT,C'R'                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R3,XP                                                            
         MVC   BXGNUM1(L'@NUM),@NUM      "Num"                                  
         MVC   BXGOPT1(L'@OPT),@OPT      "Option"                               
         MVC   BXGVAL1(L'@VALUE),@VALUE  "Value"                                
         MVC   BXGNUM2(L'@NUM),@NUM      "Num"                                  
         MVC   BXGOPT2(L'@OPT),@OPT      "Option"                               
         MVC   BXGVAL2(L'@VALUE),@VALUE  "Value"                                
         GOTO1 ACREPORT                                                         
*                                                                               
         BAS   RE,PMIDLINE         Print a    midline                           
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R3,R4,R6                                                         
         EJECT ,                                                                
***********************************************************************         
*  Open a general profile ranking  box                                *         
***********************************************************************         
         SPACE 1                                                                
*                                  Map   general   profile   print line         
         USING BXGRPROD,R3               for  ranking   format                  
         USING BIGPRNTD,R4         Map   wide print     areas                   
         USING BOXD,R6             Map   box  definitions                       
         SPACE 1                                                                
*                                  Open  a    general   profile                 
OGPRRBOX NTR1  ,                         ranking   box                          
         L     R6,ADBXAREA         ->    box  work area                         
         LA    R3,BOXCOLS          ->    box  definitions                       
         L     R4,VBIGPRNT         ->    wide print     areas                   
*                                                                               
         MVI   BOXFONT,0           Pitch 10                                     
*                                                                               
         BAS   RE,OPENBOX          Open  a    box                               
*                                                                               
         MVI   BXGRLEFT,C'L'                                                    
         MVI   BXGRCOL1,C'C'                                                    
         MVI   BXGRCOL2,C'C'                                                    
         MVI   BXGRCOL3,C'C'                                                    
         MVI   BXGRCOL4,C'C'                                                    
         MVI   BXGRCOL5,C'C'                                                    
         MVI   BXGRRGHT,C'R'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R3,XP                                                            
         MVC   BXGROPT1(L'@OPT),@OPT     "Option"                               
         MVC   BXGRVAL1(L'@VALUE),@VALUE "Value"                                
         MVC   BXGROPT2(L'@OPT),@OPT     "Option"                               
         MVC   BXGRVAL2(L'@NUM),@NUM     "Num"                                  
         MVC   BXGROPT3(L'@OPT),@OPT     "Option"                               
         MVC   BXGRVAL3(L'@AORD),@AORD   "A/D"                                  
         GOTO1 ACREPORT                                                         
*                                                                               
         BAS   RE,PMIDLINE         Print a    midline                           
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R3,R4,R6                                                         
         EJECT ,                                                                
***********************************************************************         
*  Open a general profile down-load box                               *         
***********************************************************************         
         SPACE 1                                                                
*                                  Map   general   profile   print line         
         USING BXGDPROD,R3               for  down-load format                  
         USING BIGPRNTD,R4         Map   wide print     areas                   
         USING BOXD,R6             Map   box  definitions                       
         SPACE 1                                                                
*                                  Open  a    general   profile                 
OGPRDBOX NTR1  ,                         down-load box                          
         L     R6,ADBXAREA         ->    box  work area                         
         LA    R3,BOXCOLS          ->    box  definitions                       
         L     R4,VBIGPRNT         ->    wide print     areas                   
*                                                                               
         MVI   BOXFONT,0           Pitch 10                                     
         CLI   QPROG+1,PORTRAIT    Portrait   printing ?                        
         BNE   *+8                 No,   skip                                   
         MVI   BOXFONT,1           Pitch 12                                     
*                                                                               
         BAS   RE,OPENBOX          Open  a    box                               
*                                                                               
         MVI   BXGDLEFT,C'L'                                                    
         MVI   BXGDCOL1,C'C'                                                    
         MVI   BXGDCOL2,C'C'                                                    
         MVI   BXGDCOL3,C'C'                                                    
         MVI   BXGDRGHT,C'R'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R3,XP                                                            
         MVC   BXGDOPT1(L'@OPT),@OPT     "Option"                               
         MVC   BXGDVAL1(L'@VALUE),@VALUE "Value"                                
         MVC   BXGDOPT2(L'@OPT),@OPT     "Option"                               
         MVC   BXGDVAL2(L'@VALUE),@VALUE "Value"                                
         GOTO1 ACREPORT                                                         
*                                                                               
         BAS   RE,PMIDLINE         Print a    midline                           
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R3,R4,R6                                                         
         EJECT ,                                                                
***********************************************************************         
*  Open a report profile box                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING BXRPROFD,R3         Map   report    profile   print line         
         USING BIGPRNTD,R4         Map   wide print     areas                   
         USING BOXD,R6             Map   box  definitions                       
         SPACE 1                                                                
ORPRFBOX NTR1  ,                   Open  a    report    profile   box           
         L     R6,ADBXAREA         ->    box  work area                         
         LA    R3,BOXCOLS          ->    box  definitions                       
         L     R4,VBIGPRNT         ->    wide print     areas                   
*                                                                               
         MVI   BOXFONT,0           Pitch 10                                     
*                                                                               
         BAS   RE,OPENBOX          Open  a    box                               
*                                                                               
         MVI   BXRPLEFT,C'L'                                                    
         MVI   BXRPCOL1,C'C'                                                    
         MVI   BXRPCOL2,C'C'                                                    
         MVI   BXRPRGHT,C'R'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R3,XP                                                            
*                                                                               
         MVC   BXRPNUM(L'@NUM),@NUM      "Num"                                  
         MVC   BXRPOPT(L'@OPT),@OPT      "Option"                               
         MVC   BXRPVAL1(L'@VALUE),@VALUE "Value"                                
         GOTO1 ACREPORT                                                         
*                                                                               
         BAS   RE,PMIDLINE         Print a    midline                           
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R3,R4,R6                                                         
         EJECT ,                                                                
***********************************************************************         
*  Open a box                                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING BOXD,R6             Map   box  definitions                       
         SPACE 1                                                                
OPENBOX  NTR1  ,                   Open  a    box                               
         L     R6,ADBXAREA         ->    box  work area                         
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,YES                                                      
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'T'                                                       
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*  Print a midline                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING BOXD,R6             Map   box  definitions                       
         SPACE 1                                                                
PMIDLINE NTR1  BASE=ASUBROS        Print a    midline                           
         L     R6,ADBXAREA         ->    box  work area                         
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'M'                                                       
         GOTO1 ACREPORT            Print the  midline                           
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*  Close the current box                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING BOXD,R6             Map   box  definitions                       
         SPACE 1                                                                
CLOSEBOX NTR1  BASE=ASUBROS        Close the  current   box                     
         L     R6,ADBXAREA         ->    box  work area                         
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         GOTO1 ACREPORT            Print last line of   box                     
*                                                                               
         MVI   RPTBXTYP,RPTBNONE   Say   not  in   a    box                     
*                                                                               
         MVC   BOXCOLS,SPACES      Clear box  definitions                       
         MVC   BOXCOLSR,SPACES                                                  
         MVC   BOXROWS,SPACES                                                   
*                                                                               
         MVI   BOXPGDF#,1          Page  def  #    1                            
         MVI   BOXFONT,0           Pitch 10                                     
*                                                                               
         B     SUBREXIT            Return     to   caller                       
*                                                                               
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*  Put comma after the last character in the data                     *         
*                                                                     *         
*    Input:                                                           *         
*      R1       = max   length  of   the  data                        *         
*      R7       = start         of   the  data                        *         
*                                                                     *         
*    Output:                                                          *         
*      R7       = first free    byte                                  *         
***********************************************************************         
         SPACE 1                                                                
PUTCOMMA NTR1  BASE=ASUBROS                                                     
         BAS   RE,FINDFREE         Find  1st  free byte                         
*                                                                               
         MVC   0(1,R7),COMMA       Insert     ","  into next byte               
         LA    R7,1(,R7)           Bump  past comma                             
*                                                                               
         B     SUBREXR7            Return     R7   to   caller                  
         EJECT ,                                                                
***********************************************************************         
*  Translate and find first free byte after the translated data       *         
*                                                                     *         
*    Input:                                                           *         
*      R7       = start of   data to   be   translated                *         
*                                                                     *         
*    Output:                                                          *         
*      R7       = first free byte                                     *         
***********************************************************************         
         SPACE 1                                                                
TRANFREE NTR1  BASE=ASUBROS                                                     
         ZIC   R2,3(,R7)           Get   length    of   dictionary item         
         BAS   RE,TRANSLAT         Translate  the  field                        
*                                                                               
         LR    R1,R2               Get   length    of   item                    
         BAS   RE,FINDFREE         Find  1st  free byte                         
*                                                                               
         B     SUBREXR7            Return     R7   to   caller                  
         EJECT ,                                                                
***********************************************************************         
*  Find first available (byte)                                        *         
*                                                                     *         
*    Input:                                                           *         
*      R1       = number  of bytes to consider                        *         
*      R7       = address of field                                    *         
*                                                                     *         
*    Output:                                                          *         
*      R7       = first free byte                                     *         
***********************************************************************         
         SPACE 1                                                                
FINDFREE NTR1  BASE=ASUBROS        Find  last byte in   field                   
         AR    R7,R1                                                            
         BCTR  R7,0                                                             
*                                                                               
FINDFR10 DS    0H                  Find  last character                         
         CLI   0(R7),C' '                                                       
         BH    FINDFR20                                                         
         BCTR  R7,0                                                             
         BCT   R1,FINDFR10                                                      
*                                                                               
FINDFR20 DS    0H                  First free byte                              
         LA    R7,1(,R7)                                                        
*                                                                               
         B     SUBREXR7            Return     R7   to   caller                  
         EJECT ,                                                                
***********************************************************************         
*  Convert transaction types to strings                               *         
*                                                                     *         
*    Input:                                                           *         
*      Parm 1:                                                        *         
*        Bytes 0-3 = Address of   RFLEL                               *         
*      Parm 2:                                                        *         
*        Byte  0   = Length  of   output area                         *         
*        Bytes 1-3 = Address of   output area                         *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2           Map   filters   data element                 
         SPACE 1                                                                
CNVTTYPE NTR1  BASE=ASUBROS        Convert    transaction    to strings         
         L     R2,0(,R1)           ->    input     area                         
         LA    R2,0(,R2)           Strip off  high order     byte               
         L     R6,4(,R1)           ->    output                                 
         LA    R6,0(,R6)           Strip off  high order     byte               
         ST    R6,FULL             Save  output    address                      
         MVC   BYTE,4(R1)          Save  output    length                       
*                                                                               
         ZIC   R3,BYTE             Get   output    length                       
         BCTR  R3,0                Minus one  for  execute                      
         EXMVC R3,0(R6),SPACES     Clear the  output    area                    
*                                                                               
         ZIC   R3,RFLLN            Get   unconverted    string    len           
         SHI   R3,RFLLNQ           Find  data length                            
*                                                                               
         L     R6,ABLOCK           ->    scan block     area                    
         MVC   0(132,R6),SPACES    Clear scan block     area                    
         MVC   132(60,R6),SPACES         6*32 bytes =   192                     
*                                                                               
         LA    R4,RFLDATA          ->    input     data                         
         TM    RFLIND,RFLXCLD      Exclusion ?                                  
         BZ    CNVTYP10            No,   skip                                   
         MVI   0(R6),C'*'          Insert   asterisk                            
         LA    R6,1(,R6)                                                        
*                                                                               
CNVTYP10 DS    0H                  Check for  special   type                    
         CLI   0(R4),TYFIRST       Special    type ?                            
         BL    CNVTYP60            No,   skip                                   
         CLI   0(R4),TYLAST        Special    type 30 ?                         
         BH    CNVTYP60            No,   skip                                   
         L     R1,=A(TTYPETAB)     ->    transaction    type table              
*                                                                               
CNVTYP20 DS    0H                  Test  table     element                      
         CLI   0(R1),0             End   of   table ?                           
         BNE   *+6                 No,   continue                               
         DC    H'00'               Invalid    type                              
*                                                                               
         CLC   0(1,R1),0(R4)       Same  transaction    type ?                  
         BE    CNVTYP30            Yes,  continue                               
         LA    R1,5(,R1)           ->    next table     entry                   
         B     CNVTYP20            Check next table     entry                   
*                                                                               
CNVTYP30 DS    0H                  Found special   type                         
         MVC   WORD,1(R1)          Save  type code                              
         CLI   WORD,ESC#HIGH       Test  for  escape    sequence                
         BNL   CNVTYP40            No,   continue                               
         LA    R7,WORD             ->    type code                              
         BAS   RE,TRANSLAT         Translate  type code                         
*                                                                               
CNVTYP40 DS    0H                  Insert     into scan block                   
         MVC   0(3,R6),WORD        Get   type code ID                           
         LA    R0,3                Max   3    characters                        
         LA    RE,2(,R6)           ->    last character                         
*                                                                               
CNVTYP50 DS    0H                  Put   num  of   chars     in   R0            
         CLI   0(RE),C' '          Space ?                                      
         BNE   CNVTYP70            No,   continue                               
         BCTR  RE,0                ->    previous                               
         BCT   R0,CNVTYP50         Test  previous  character                    
         B     CNVTYP70            All   spaces,   continue                     
*                                                                               
CNVTYP60 DS    0H                  Not   special   type,     so   edit          
         EDIT  (B1,(R4)),(3,(R6)),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
CNVTYP70 DS    0H                  R0    has  num  of   characters              
         AR    R6,R0               Find  next free byte                         
         LA    R4,1(,R4)           ->    next transaction    type               
         CHI   R3,1                Last  transaction    type ?                  
         BE    CNVTYP80            Yes,  skip                                   
         MVC   0(1,R6),COMMA       Insert     comma                             
         LA    R6,1(,R6)           ->    next output    byte                    
*                                                                               
CNVTYP80 DS    0H                  Process    next transaction    type          
         BCT   R3,CNVTYP10               if   any                               
*                                                                               
         L     RF,ABLOCK           ->    scan block     area                    
         LR    RE,RF               Save  scan block     area address            
         SR    R6,RF               Find  length    of   data                    
         SR    RF,RF               Get   length    of   output    field         
         ICM   RF,1,BYTE                                                        
         BZ    CNVTYP90            None, skip                                   
         CR    R6,RF               Will  the  output    fit ?                   
         BNH   *+6                 Yes,  continue                               
         DC    H'00'               Output     field     is   too  small         
*                                                                               
CNVTYP90 DS    0H                  Move  the  data to   output    field         
         SHI   R6,1                Data  len  minus     one  for  EX            
         BM    CNVTYPEX            No    data,     exit                         
         L     RF,FULL             ->    output    address                      
         EXMVC R6,0(RF),0(RE)      Move  data to   output    field              
         DROP  R2                                                               
*                                                                               
CNVTYPEX DS    0H                  Exit                                         
         B     SUBREXIT            Return     to   caller                       
         EJECT ,                                                                
***********************************************************************         
*  Translate a field                                                  *         
*                                                                     *         
*    Input:                                                           *         
*      R7       = start  of   data to   be   translated               *         
*      RPTSWS   = report switches                                     *         
*                                                                     *         
*    Output:                                                          *         
*      RPTSWS   = report switches                                     *         
***********************************************************************         
         SPACE 1                                                                
TRANSLAT NTR1  BASE=ASUBROS        Translate  the  field                        
         MVC   DMCB,=C'SL  '       Single     field,    upper     case          
         TM    RPTSWS,RPTSUCTR     Upper case required ?                        
         BZ    *+8                 No,   skip                                   
         MVI   DMCB+1,C'U'         Say   upper     case                         
         GOTO1 ADDICTAT,DMCB,,(R7),0                                            
*                                  Clear upper     case                         
         NI    RPTSWS,TURNOFF-RPTSUCTR                                          
*                                                                               
         B     SUBREXIT            Return     to   caller                       
         EJECT ,                                                                
***********************************************************************         
*  Standardized text get routine                                      *         
*                                                                     *         
*    Input:                                                           *         
*      Parm1:                                                         *         
*        Byte  0   = Type    (default    "S"    screen       type)    *         
*        Bytes 1-3 = Text    number                                   *         
*      Parm2:                                                         *         
*        Byte  0   = Length  of   output area                         *         
*        Bytes 1-3 = Address of   output area                         *         
*      Parm3:                                                         *         
*        Byte  0   = Length  of   extra  output data                  *         
*                    or      zero if     no     extra data            *         
*        Bytes 1-3 = Address of   extra  output data                  *         
*      Parm4                                                          *         
*        Byte  0   = Zero                                             *         
*        Bytes 1-3 = Address of   substitution  table                 *         
*                    or      zero if     no     substitution table    *         
*      RPTSWS   = report switches                                     *         
*                                                                     *         
*    Output:                                                          *         
*      RPTSWS   = report switches                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING GETTXTD,R2          Map   GETTXT    DSECT                        
         USING MASTD,R6            Map   DDMASTER  DSECT                        
         SPACE 1                                                                
TXTGET   NTR1  BASE=ASUBROS                                                     
         L     R6,ADMASTC          ->    DDMASTC   area                         
         LA    R2,DMCB1            ->    2nd  DMCB                              
         LR    R3,R1               ->    parameter list                         
         XC    GTBLOCK,GTBLOCK     Clear work block                             
         MVC   GTMSGNO,2(R3)       Get   message   number                       
         MVI   GTMTYP,GTMSCR       Set   default   type (screen)                
         CLI   0(R3),C' '          Any   message   type ?                       
         BNH   *+10                No,   skip                                   
         MVC   GTMTYP,0(R3)        Get   message   type                         
         MVI   GTMSYS,6            Account    system                            
         MVC   GTMAXL(4),4(R3)     output     addr and  length                  
         CLI   8(R3),0             Any   extra     output    area ?             
         BE    TXTGET10            No,   skip                                   
         MVC   GTLTXT,8(R3)        Length     of   extra     output             
         MVC   GTATXT,9(R3)        A(extra    output    data)                   
*                                                                               
TXTGET10 DS    0H                                                               
         MVC   GTMLANG,RCLANG      Insert     language                          
*                                                                               
         OC    13(3,R3),13(R3)     Any   substitution   table ?                 
         BZ    *+10                No,   skip                                   
         MVC   GTASUBST,13(R3)     Yes,  substitution   table                   
*                                                                               
         OI    GT1INDS,GT1OWRK+GT1NOREF                                         
*                                                                               
         SR    R5,R5                                                            
         IC    R5,4(,R3)           Get   length    of   output    area          
         L     R4,4(,R3)           Get   address   of   output    area          
         SH    R5,=H'01'           Any   space ?                                
         BM    TXTGETEX            No,   exit                                   
         EXMVC R5,0(R4),SPACES     Clear the  field                             
*                                                                               
         GOTO1 MCVGETXT,GETTXTD    Get   the  message   text                    
*                                                                               
         TM    RPTSWS,RPTSUCTR     Upper case required ?                        
         BZ    TXTGETEX            No,   skip                                   
         L     R1,AUPCASE                                                       
         EX    R5,TXTGETTR         Upper case the  message                      
*                                                                               
TXTGETEX DS    0H                  Exit                                         
*                                  Clear upper     case                         
         NI    RPTSWS,TURNOFF-RPTSUCTR                                          
         B     SUBREXIT            Return     to   caller                       
*                                                                               
TXTGETTR TR    0(0,R4),0(R1)       Translate  to   upper     case               
*                                                                               
         DROP  R2,R6                                                            
         EJECT ,                                                                
         SPACE 1                                                                
SUBREXIT DS    0H                  Exit                                         
         XIT1  ,                   Return     to   caller                       
*                                                                               
SUBREXR7 DS    0H                  Exit                                         
*                                                                               
         XIT1  REGS=(R7)           Return     R7   to   caller                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         TITLE 'Print out specs of format - constants'                          
***********************************************************************         
*  Constants                                                          *         
***********************************************************************         
         SPACE 1                                                                
MISSC4EL DS    0C                  Missing    X'C4'     el  (RPFEL)             
         DC    AL1(RPFELQ)         X'C4' element                                
         DC    AL1(RPFLN2Q)        Base  element   length                       
         DC    XL(RPFLN2Q-2)'00'   Rest  of   element   is   null               
         DC    XL1'00'             End   of   record                            
*                                                                               
         TITLE 'Print out specs of format - Tables'                             
***********************************************************************         
*  Tables                                                             *         
***********************************************************************         
         SPACE 1                                                                
*                                  ************************************         
BASESTAB DS    0A                  *     Base addresses table         *         
*                                  *          (see ABASES)            *         
*                                  ************************************         
*                                  Address    of:                               
         DC    A(PGPROPRT)         .     General   profile                      
         DC    A(PRPRF)            .     Report    profile                      
         DC    A(SUBROS)           .     Subroutines                            
BASESTBQ EQU   *-BASESTAB          Length     of   table                        
         EJECT ,                                                                
         SPACE 1                                                                
*                                  ************************************         
PROFNTAB DS    0C                  *     Profile   name table         *         
*                                  ************************************         
*                                                                               
         DC    AL2(AC#RS491),AL1(PRODUCTION)  Production                        
*                                                                               
PROFNTBQ EQU   *-PROFNTAB          Table entry     length                       
*                                                                               
         DC    AL2(AC#RSPPR),AL1(PAYABLES)    Pay                               
         DC    AL2(AC#RSRPR),AL1(RECEIVABLE)  Receivables                       
         DC    AL2(AC#RSIPR),AL1(INCOME)      Income                            
         DC    AL2(AC#RSXPR),AL1(EXPENSE)     Expense                           
         DC    AL2(AC#RS499),AL1(MANPOWER)    Person                            
         DC    AL2(AC#RSLPR),AL1(PNL)         P&L                               
         DC    AL2(AC#RSCPR),AL1(CASH)        Cash (Bank)                       
         DC    AL2(AC#RSGPR),AL1(GENERAL)     G/L                               
         DC    AL2(AC#RSMPR),AL1(MEDIA)       Media                             
*                                                                               
         DC    AL1(EOT)            End   of   table                             
         EJECT ,                                                                
         SPACE 1                                                                
*                                  ************************************         
OBOXRTAB DS    0A                  *     Open box  routine   table    *         
*                                  ************************************         
         DC    A(OHEADBOX)         Open  header    box                          
         DC    A(OROWSBOX)         Open  rows      box                          
         DC    A(OCOLSBOX)         Open  columns   box                          
         DC    A(OGPROBOX)         Open  general   profile   box                
         DC    A(OGPRRBOX)         Open  "         "    range     box           
         DC    A(OGPRDBOX)         Open  "         "    down-load box           
         DC    A(ORPRFBOX)         Open  report    profile   box                
         EJECT ,                                                                
         SPACE 1                                                                
*                                  ************************************         
POPT2TAB DS    0C                  *     RCLOPT2   switches           *         
*                                  ************************************         
*                                                                               
*                                  ***** Notes:                   *****         
*                                  ***** 1.   these     entries   *****         
*                                  *****      are  sequence       *****         
*                                  *****      dependent           *****         
*                                  ***** 2.   do   not  add  any  *****         
*                                  *****      new  attributes     *****         
*                                  *****      before    POPT2ATB  *****         
*                                                                               
         DC    AL1(RCLCODE+RCLNAME)                                             
         DC    AL2(AC#RSCAN)       "BOTH"     code                              
*                                                                               
POPT2TBQ EQU   *-POPT2TAB          Table entry     length                       
*                                                                               
         DC    AL1(RCLCODE)                                                     
         DC    AL2(AC#RSCOD)       "CODE"     code                              
*                                                                               
         DC    AL1(RCLNAME)                                                     
         DC    AL2(AC#RSNAM)       "NAME"     name                              
*                                                                               
POPT2ATB DS    0C                                                               
         DC    AL1(RCLSHRT)                                                     
         DC    AL2(AC#RSDES)       "DESC"     description                       
*                                                                               
         DC    AL1(RCLLVL)                                                      
         DC    AL2(AC#RSLVL)       "LVL "     level                             
*                                                                               
         DC    AL1(EOT)            End   of   table                             
         EJECT ,                                                                
         SPACE 1                                                                
*                                  ************************************         
DTEFGTAB DS    0C                  *     RCLDTEFG  switches           *         
*                                  ************************************         
         DC    AL1(RCLBBF)                                                      
         DC    AL2(AC#RSBBF)       "BBF "     balance   forward                 
*                                                                               
DTEFGTBQ EQU   *-DTEFGTAB          Table entry     length                       
*                                                                               
         DC    AL1(RCLDAY)                                                      
         DC    AL2(AC#RSRDA)       "DAY "     day                               
*                                                                               
         DC    AL1(RCLMON)                                                      
         DC    AL2(AC#RSRMO)       "MON "     month                             
*                                                                               
         DC    AL1(RCLMON+RCLNROLL)                                             
         DC    AL2(AC#RSMNO)       "M???"     calendar  month                   
*                                                                               
         DC    AL1(0)                                                           
         DC    AL2(AC#RSRPE)       "PER "                                       
*                                                                               
         DC    AL1(RCLPERD)                                                     
         DC    AL2(AC#RSPED)       "PED "     period    end  date               
*                                                                               
         DC    AL1(RCLPERD+RCLMON)                                              
         DC    AL2(AC#RSPMO)       "PMON"     period    by   month              
*                                                                               
         DC    AL1(RCLPERD+RCLNROLL)                                            
         DC    AL2(AC#RSPNO)       "P???"     calender  period                  
*                                                                               
         DC    AL1(RCLPERD+RCLMON+RCLNROLL)                                     
         DC    AL2(AC#RSPMN)       "PM??"     calender  period by month         
*                                                                               
         DC    AL1(RCLQTR)                                                      
         DC    AL2(AC#RSRQ1)       "QTR "     quarter                           
*                                                                               
         DC    AL1(RCLQTR+RCLTODTE)                                             
         DC    AL2(AC#RSRQ2)       "QTD "     quarter   by   date               
*                                                                               
         DC    AL1(RCLQTR+RCLNROLL)                                             
         DC    AL2(AC#RSQNO)       "Q???"     calendar  quarter                 
*                                                                               
         DC    AL1(RCLQTR+RCLBBF)                                               
         DC    AL2(AC#RSRQ3)       "Q???"     calendar  quarter                 
*                                                                               
         DC    AL1(RCLYEAR)                                                     
         DC    AL2(AC#RSRY1)       "YEAR"     year                              
*                                                                               
         DC    AL1(RCLYEAR+RCLTODTE)                                            
         DC    AL2(AC#RSRY2)       "YTD "     year by   date                    
*                                                                               
         DC    AL1(RCLYEAR+RCLBBF)                                              
         DC    AL2(AC#RSYR4)       "+YR "     year by   date                    
*                                                                               
         DC    AL1(EOT)            End   of   table                             
         EJECT ,                                                                
         SPACE 1                                                                
*                                  ************************************         
DECMRTAB DS    0C                  *     Decimal   rounding  table    *         
*                                  ************************************         
*                                                                               
         DC    XL1'05',CL2'4 '                                                  
*                                                                               
DECMRTBQ EQU   *-DECMRTAB          Entry length                                 
*                                                                               
         DC    XL1'04',CL2'3 '                                                  
         DC    XL1'03',CL2'2 '     Default:   do   not  display                 
         DC    XL1'02',CL2'1 '                                                  
         DC    XL1'01',CL2'0 '                                                  
         DC    XL1'FF',CL2'-1'                                                  
         DC    XL1'FE',CL2'-2'                                                  
         DC    XL1'FD',CL2'-3'                                                  
         DC    XL1'FC',CL2'-4'                                                  
         DC    XL1'FB',CL2'-5'                                                  
         DC    XL1'FA',CL2'-6'                                                  
*                                                                               
         DC    XL1'00'             End   of   table                             
         EJECT ,                                                                
         SPACE 1                                                                
*                                  ************************************         
FLTTAB   DS    0C                  *     Filters   table              *         
*                                  ************************************         
*                                                                               
         DC    AL1(RFLACC)         2                                            
         DC    AL1(0,0)                                                         
         DCDD  AC#RSACC,L'FLTTTXT  "Account"                                    
*                                                                               
         DC    AL1(RFLOFF)         3                                            
         DC    AL1(MANPOWER,0)                                                  
*                                  "Person    Office"                           
         DC    CL(L'FLTTTXT)'Person Office'                                     
*                                                                               
         DC    AL1(RFLOFF)         3                                            
         DC    AL1(0,0)                                                         
         DCDD  AC#OFF,L'FLTTTXT    "Office"                                     
*                                                                               
         DC    AL1(RFLCLI)         4                                            
         DC    AL1(PNL,0)                                                       
         DCDD  AC#CLINT,L'FLTTTXT  "Client"                                     
*                                                                               
         DC    AL1(RFLCLI)         4                                            
         DC    AL1(MANPOWER,0)                                                  
         DCDD  AC#CPJ,L'FLTTTXT    "Cli/Prd/Job"                                
*                                                                               
         DC    AL1(RFLCLI)         4                                            
         DC    AL1(0,0)                                                         
         DCDD  AC#CLIPJ,L'FLTTTXT  "Cl/Pr/Jo(Es)"                               
*                                                                               
         DC    AL1(RFLBSR)         6                                            
         DC    AL1(0,0)                                                         
         DCDD  AC#BLGSR,L'FLTTTXT  "Billing   source"                           
*                                                                               
         DC    AL1(RFLCOST)        7                                            
         DC    AL1(0,0)                                                         
         DCDD  AC#CSTA,L'FLTTTXT   "Costing   A/c"                              
*                                                                               
         DC    AL1(RFL11BL)        8                                            
         DC    AL1(0,0)                                                         
         DCDD  AC#BLG,L'FLTTTXT    "Billing"                                    
*                                                                               
         DC    AL1(RFL12RV)        9                                            
         DC    AL1(0,0)                                                         
         DCDD  AC#RSREV,L'FLTTTXT  "Revenue"                                    
*                                                                               
         DC    AL1(RFLDEPT)        12                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#DPT,L'FLTTTXT    "Department"                                 
*                                                                               
         DC    AL1(RFLPRSN)        13                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#PRSN,L'FLTTTXT   "Person"                                     
*                                                                               
         DC    AL1(RFLVNDR)        14                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#VNDR,L'FLTTTXT   "Vendor"                                     
*                                                                               
         DC    AL1(RFLXCAT)        15                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#EXP,L'FLTTTXT    "Expense"                                    
*&&UK                                                                           
         DC    AL1(RFLFCUR)        16                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#RCURR,L'FLTTTXT  "Currency"                                   
*&&                                                                             
         DC    AL1(RFLPRTP)        17                                           
         DC    AL1(0,0)                                                         
*                                  "Participant"                                
         DC    CL(L'FLTTTXT)'Participant'                                       
*                                                                               
         DC    AL1(RFLCNTR)        18                                           
         DC    AL1(MANPOWER,0)                                                  
         DCDD  AC#CSTNC,L'FLTTTXT  "Cost/Non-cli"                               
*                                                                               
         DC    AL1(RFLCNTR)        18                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#RSCOC,L'FLTTTXT  "Contra Account"                             
*                                                                               
         DC    AL1(RFLBLGP)        19                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#BILGP,L'FLTTTXT  "Billgroup"                                  
*                                                                               
         DC    AL1(RFLWCGP)        20                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#WGRP,L'FLTTTXT   "Work group"                                 
*                                                                               
         DC    AL1(RFLWC)          21                                           
         DC    AL1(MANPOWER,0)                                                  
         DCDD  AC#TASK,L'FLTTTXT   "Task"                                       
*                                                                               
         DC    AL1(RFLWC)          21                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#WC,L'FLTTTXT     "Work code"                                  
*                                                                               
         DC    AL1(RFLBTYP)        22                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#BLTYP,L'FLTTTXT  "Billtype"                                   
*                                                                               
         DC    AL1(RFLOFGP)        23                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#OFFGP,L'FLTTTXT  "Office    group"                            
*                                                                               
         DC    AL1(RFLMDGP)        24                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#MEDGP,L'FLTTTXT  "Media     group"                            
*                                                                               
         DC    AL1(RFLMED)         25                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#MEDC,L'FLTTTXT   "Media     code"                             
*                                                                               
         DC    AL1(RFLSTTY)        26                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#STDIO,L'FLTTTXT  "Studio"                                     
*                                                                               
         DC    AL1(RFLUFLD)        27                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#RSUSF,L'FLTTTXT  "User Field"                                 
*                                                                               
         DC    AL1(RFLXCPT)        28                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#XCN,L'FLTTTXT    "Exception"                                  
*                                                                               
*&&DO                                                                           
         DC    AL1(RFLTWTP)        29    Not  used in   ACSCR12                 
         DC    AL1(0,0)                                                         
*                                  "Time WC"                                    
         DC    CL(L'FLTTTXT)'Time WC'                                           
*&&                                                                             
*                                                                               
         DC    AL1(RFLWCTYP)       29                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#RSWCT,L'FLTTTXT  "WC   Type"                                  
*                                                                               
         DC    AL1(RFLOPTP)        30                                           
         DC    AL1(0,0)                                                         
         DC    CL(L'FLTTTXT)'OOP'  "OOP"                                        
*                                                                               
         DC    AL1(RFLTTIME)       31                                           
         DC    AL1(MANPOWER,0)                                                  
*                                  "Type of   time"                             
         DC    CL(L'FLTTTXT)'Type of time'                                      
*                                                                               
         DC    AL1(RFLTTIME)       31                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#RSTYT,L'FLTTTXT  "Time Type"                                  
*                                                                               
         DC    AL1(RFLTTYPE)       32                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#RSTTY,L'FLTTTXT  "Type"                                       
*                                                                               
         DC    AL1(RFLBUDGT)       33                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#BGT,L'FLTTTXT    "Budget"                                     
*                                                                               
         DC    AL1(RFLFILT1)       35                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#FLT1,3           "F1"                                         
         DC    CL(L'FLTTTXT-3)' '                                               
*                                                                               
         DC    AL1(RFLFILT2)       36                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#FLT2,3           "F2"                                         
         DC    CL(L'FLTTTXT-3)' '                                               
*                                                                               
         DC    AL1(RFLFILT3)       37                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#FLT3,3           "F3"                                         
         DC    CL(L'FLTTTXT-3)' '                                               
*                                                                               
         DC    AL1(RFLFILT4)       38                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#FLT4,3           "F4"                                         
         DC    CL(L'FLTTTXT-3)' '                                               
*                                                                               
         DC    AL1(RFLFILT5)       39                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#FLT5,3           "F5"                                         
         DC    CL(L'FLTTTXT-3)' '                                               
*                                                                               
         DC    AL1(RFLCOFF)        40                                           
         DC    AL1(0,0)            Status                                       
         DCDD  AC#RSOCC,L'FLTTTXT  "Client    office"                           
*                                                                               
         DC    AL1(RFLAOFF)        41                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#ANOFF,L'FLTTTXT  "Analysis  off"                              
*                                                                               
         DC    AL1(RFLAPPRV)       42                                           
         DC    AL1(0,FLTTSTA)      Status                                       
         DCDD  AC#APRVD,L'FLTTTXT  "Approved"                                   
*                                                                               
         DC    AL1(RFLOFFST)       43                                           
         DC    AL1(0,FLTTSTA)      Status                                       
         DCDD  AC#OFFST,L'FLTTTXT  "Offset"                                     
*                                                                               
         DC    AL1(RFLURGNT)       44                                           
         DC    AL1(0,FLTTSTA)      Status                                       
         DCDD  AC#URG,L'FLTTTXT    "Urgent"                                     
*                                                                               
         DC    AL1(RFLMTHD)        45                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#METH,L'FLTTTXT   "Method"                                     
*                                                                               
         DC    AL1(RFLLOCS)        46                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#LOC,L'FLTTTXT    "Location"                                   
*                                                                               
         DC    AL1(RFLPRCLV)       47                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#RSPCL,L'FLTTTXT  "Price     Level"                            
*                                                                               
         DC    AL1(RFL14DLB)       48                                           
         DC    AL1(0,0)                                                         
         DC    CL3'14'             "14                                          
         DCDD  AC#PTR,L'FLTTTXT-3       Pointer"                                
*                                                                               
         DC    AL1(RFLDCNTR)       54                                           
*                                  PCT  Denominator                             
         DC    AL1(MANPOWER,FLTTPCTD)                                           
         DCDD  AC#CSTNC,L'FLTTTXT  "Cost/Non-cli"                               
*                                                                               
         DC    AL1(RFLDCNTR)       54                                           
         DC    AL1(0,FLTTPCTD)     PCT  Denominator                             
         DCDD  AC#RSCOC,L'FLTTTXT  "Contra     account"                         
*                                                                               
         DC    AL1(RFLDTTME)       55                                           
         DC    AL1(0,FLTTPCTD)     PCT  Denominator                             
*                                  "Type  of   time"                            
         DC    CL(L'FLTTTXT)'Type of Time'                                      
*                                                                               
         DC    AL1(RFLDCLI)        56                                           
         DC    AL1(PNL,FLTTPCTD)   PCT  Denominator                             
         DCDD  AC#CLINT,L'FLTTTXT  "Client"                                     
*                                                                               
         DC    AL1(RFLDCLI)        56                                           
*                                  PCT  Denominator                             
         DC    AL1(MANPOWER,FLTTPCTD)                                           
         DCDD  AC#CPJ,L'FLTTTXT    "Cli/Prd/Job"                                
*                                                                               
         DC    AL1(RFLDCLI)        56                                           
         DC    AL1(0,FLTTPCTD)     PCT  Denominator                             
         DCDD  AC#CLIPJ,L'FLTTTXT  "Cl/Pr/Jo(Es)"                               
*                                                                               
         DC    AL1(RFLDWC)         57                                           
         DC    AL1(0,FLTTPCTD)     PCT  Denominator                             
         DCDD  AC#TASK,L'FLTTTXT   "Task"                                       
*                                                                               
         DC    AL1(RFLRECON)       60                                           
         DC    AL1(0,FLTTSTA)      Status                                       
         DCDD  AC#RCND,L'FLTTTXT   "Reconciled"                                 
*&&UK                                                                           
         DC    AL1(RFLAUTH)        62                                           
         DC    AL1(0,0)                                                         
         DCDD  AC#RSSTA,L'FLTTTXT  "Auth status"                                
*&&                                                                             
         DC    AL1(EOT)            End   of   table                             
         EJECT ,                                                                
         SPACE 1                                                                
*                                  ************************************         
TTYPETAB DS    0C                  *     Transaction    type table    *         
*                                  ************************************         
         DC    AL1(TY30DI)         Type  30 - difference                        
         DC    CL4'DI'                                                          
*                                                                               
         DC    AL1(TY06MN)         Type  06 - manual    billing                 
         DC    CL4'M'                                                           
*                                                                               
         DC    AL1(TY30CH)         Type  30 - check                             
         DCDD  AC#RSTCH,4                                                       
*                                                                               
         DC    AL1(TY30OF)         Type  30 - offset                            
         DCDD  AC#RSTOF,4                                                       
*                                                                               
         DC    AL1(TY30WO)         Type  30 - write-off                         
         DCDD  AC#RSTWO,4                                                       
*                                                                               
         DC    AL1(TY30TT)         Type  30 - transfer  to                      
         DCDD  AC#RSTTT,4                                                       
*                                                                               
         DC    AL1(TY30TF)         Type  30 - transfer  from                    
         DCDD  AC#RSTTF,4                                                       
*                                                                               
         DC    AL1(TY30IA)         Type  30 - inter-agency                      
         DC    CL4'IA'                                                          
*                                                                               
         DC    AL1(EOT)            End   of   table                             
         EJECT ,                                                                
         SPACE 1                                                                
*                                  ************************************         
*                                  *     General   profile   -        *         
GPPRTAB  DS    0C                  *          print    option         *         
*                                  *          table                   *         
*                                  ************************************         
*                                                                               
*                                  ***** Left justify                           
         DC    AL1(0)              Dictionary                                   
         DC    AL2(AC#LJUST)       Text                                         
         DC    AL4(PGPROLJT)       Insert     value                             
*                                                                               
*                                  ***** Print     redundant totals             
         DC    AL1(0)              Dictionary                                   
         DC    AL2(AC#PRRDT)                                                    
         DC    AL4(PGPRORDT)                                                    
*                                                                               
*                                  ***** Print     boxes                        
         DC    AL1(0)              Dictionary                                   
         DC    AL2(AC#PRBOX)                                                    
         DC    AL4(PGPROBOX)                                                    
*                                                                               
*                                  ***** Print     redundant info               
*                                  *****      in   col                          
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1604)                                                        
         DC    AL4(PGPRORDC)                                                    
*                                                                               
*                                  ***** Print     inactive  accounts           
         DC    AL1(0)              Dictionary                                   
         DC    AL2(AC#PRINA)                                                    
         DC    AL4(PGPROINA)                                                    
*                                                                               
*                                  ***** Print     amounts   using              
*                                  *****      commas                            
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1606)                                                        
         DC    AL4(PGPROCMA)                                                    
*                                                                               
*                                  ***** Print     zero amounts                 
         DC    AL1(0)              Dictionary                                   
         DC    AL2(AC#PRZAM)                                                    
         DC    AL4(PGPROZAM)                                                    
*                                                                               
*                                  ***** Print     zero totals                  
         DC    AL1(0)              Dictionary                                   
         DC    AL2(AC#PRZTO)                                                    
         DC    AL4(PGPROZTO)                                                    
*                                                                               
*                                  ***** Print     negative  amounts            
         DC    AL1(0)              Dictionary                                   
         DC    AL2(AC#NEGAM)                                                    
         DC    AL4(PGPRONGM)                                                    
*                                                                               
*&&US                                                                           
*                                  ***** Print     landscape or                 
*                                  *****      portrait                          
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1610)                                                        
         DC    AL4(PGPROLOP)                                                    
*                                                                               
*                                  ***** Print     headings  in                 
*                                  *****      upper     case                    
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1611)                                                        
         DC    AL4(PGPROUPR)                                                    
*                                                                               
*                                  ***** Print     mids/tots across             
*                                  *****      columns                           
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1612)                                                        
         DC    AL4(PGPROXCL)                                                    
*                                                                               
*                                  ***** Stripes   or   shade                   
*                                  *****      in/out    of   boxes              
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1613)                                                        
         DC    AL4(PGPROSOS)                                                    
*&&                                                                             
*                                                                               
*&&UK                                                                           
*                                  ***** Print     headings  in                 
*                                  *****      upper     case                    
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1611)                                                        
         DC    AL4(PGPROUPR)                                                    
*                                                                               
*                                  ***** Print     mids/tots across             
*                                  *****      columns                           
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1612)                                                        
         DC    AL4(PGPROXCL)                                                    
*&&                                                                             
*                                                                               
*                                  ***** Print     request   totals             
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1607)                                                        
         DC    AL4(PGPRORQT)                                                    
*                                                                               
*&&US                                                                           
*                                  ***** Print     division  errors             
*                                  *****      as   "E"                          
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1617)                                                        
         DC    AL4(PGPRODVE)                                                    
*&&                                                                             
*                                                                               
*&&UK                                                                           
*                                  ***** Print     foreign   currency           
*                                  *****      with prefix                       
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1615)                                                        
         DC    AL4(PGPROFCP)                                                    
*&&                                                                             
*                                                                               
*&&US                                                                           
*                                  ***** Underline totals                       
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1618)                                                        
         DC    AL4(PGPROUTO)                                                    
*&&                                                                             
*                                                                               
*&&UK                                                                           
*                                  ***** Exclude   non-foreign                  
*                                  *****      trans                             
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1616)                                                        
         DC    AL4(PGPROXNF)                                                    
*                                                                               
*                                  ***** Print     division  errors             
*                                  *****      as   "E"                          
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1617)                                                        
         DC    AL4(PGPRODVE)                                                    
*                                                                               
*                                  ***** Underline totals                       
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1618)                                                        
         DC    AL4(PGPROUTO)                                                    
*&&                                                                             
*                                                                               
*                                  ***** Number    of   address   lines         
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1619)                                                        
         DC    AL4(PGPRO#AL)                                                    
*                                                                               
*                                  ***** Print     column    headings           
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1628)                                                        
         DC    AL4(PGPROPCH)                                                    
*                                                                               
*                                  ***** Pitch     control                      
         DC    AL1(GPPRMSG)        Message                                      
         DC    AL2(1624)                                                        
         DC    AL4(PGPROPIT)                                                    
*                                                                               
         DC    AL1(EOT)            End   of   table                             
         EJECT ,                                                                
         SPACE 1                                                                
*                                  ************************************         
*                                  *     General   profile   -        *         
GPDWTAB  DS    0C                  *          down-load option        *         
*                                  *          table                   *         
*                                  ************************************         
*                                                                               
*                                  ***** Date format                            
         DC    AL1(GPDWCOL1)       Dictionary,     column    1                  
         DC    AL2(AC#DTFMT)       Text                                         
         DC    AL4(PGPDWDFM)       Insert     value                             
*                                                                               
*                                  *     Skip a    line                         
         DC    AL1(GPDWSKPL)       Skip  line                                   
*                                                                               
*                                  ***** Decimal                                
         DC    AL1(GPDWCOL1)       Dictionary,     column    1                  
         DC    AL2(AC#DCML)                                                     
         DC    AL4(PGPDWDCM)                                                    
*                                                                               
*                                  ***** Field     separator                    
*                                  Message,   column    2                       
         DC    AL1(GPDWMSG+GPDWCOL2)                                            
         DC    AL2(1730)                                                        
         DC    AL4(PGPDWFLD)                                                    
*                                                                               
*                                  ***** Amounts   in   text format             
*                                  Message,   column    1                       
         DC    AL1(GPDWMSG+GPDWCOL1)                                            
         DC    AL2(1630)                                                        
         DC    AL4(PGPDWTXT)                                                    
*                                                                               
*                                  ***** Text delimiter                         
*                                  Message,   column    2                       
         DC    AL1(GPDWMSG+GPDWCOL2)                                            
         DC    AL2(1731)                                                        
         DC    AL4(PGPDWEOT)                                                    
*                                                                               
*                                  ***** Pad  amounts   w/leading zeros         
*                                  Message,   column    1                       
         DC    AL1(GPDWMSG+GPDWCOL1)                                            
         DC    AL2(1631)                                                        
         DC    AL4(PGPDWPAD)                                                    
*                                                                               
*                                  ***** End  of   line delimiter               
*                                  Message,   column    2                       
         DC    AL1(GPDWMSG+GPDWCOL2)                                            
         DC    AL2(1732)                                                        
         DC    AL4(PGPDWEOL)                                                    
*                                                                               
*                                  ***** End  of   report    delimiter          
*                                  Message,   column    2                       
         DC    AL1(GPDWMSG+GPDWCOL2)                                            
         DC    AL2(1733)                                                        
         DC    AL4(PGPDWEOR)                                                    
*                                                                               
*                                  ***** Fixed     width     fields             
*                                  Message,   column    1                       
         DC    AL1(GPDWMSG+GPDWCOL1)                                            
         DC    AL2(1632)                                                        
         DC    AL4(PGPDWFXW)                                                    
*                                                                               
*                                  ***** Format    can  exceed                  
*                                  *****      198  characters                   
*                                  Message,   column    1                       
         DC    AL1(GPDWMSG+GPDWCOL1)                                            
         DC    AL2(1633)                                                        
         DC    AL4(PGPDWXMX)                                                    
*                                                                               
*                                  *     Skip a    line                         
         DC    AL1(GPDWSKPL)       Skip  line                                   
*                                                                               
*                                  ***** Include   column    headings           
*                                  Message,   column    1                       
         DC    AL1(GPDWMSG+GPDWCOL1)                                            
         DC    AL2(1634)                                                        
         DC    AL4(PGPDWCOL)                                                    
*                                                                               
*                                  ***** Include   rows                         
*                                  Message,   column    1                       
         DC    AL1(GPDWMSG+GPDWCOL1)                                            
         DC    AL2(1635)                                                        
         DC    AL4(PGPDWROW)                                                    
*                                                                               
*                                  ***** Include   totals                       
*                                  Message,   column    1                       
         DC    AL1(GPDWMSG+GPDWCOL1)                                            
         DC    AL2(1636)                                                        
         DC    AL4(PGPDWTOT)                                                    
*                                                                               
*                                  *     Skip a    line                         
         DC    AL1(GPDWSKPL)       Skip  line                                   
*                                                                               
*                                  ***** Number    of   address   lines         
*                                  *****      to   down-load                    
*                                  Message,   column    1                       
         DC    AL1(GPDWMSG+GPDWCOL1)                                            
         DC    AL2(1621)                                                        
         DC    AL4(PGPDWD#A)                                                    
*                                                                               
*                                  *     Skip a    line                         
         DC    AL1(GPDWSKPL)       Skip  line                                   
*                                                                               
*                                  ***** Transmission   type                    
*                                  Message,   column    1                       
         DC    AL1(GPDWMSG+GPDWCOL1)                                            
         DC    AL2(1637)                                                        
         DC    AL4(PGPDWEDI)                                                    
*                                                                               
         DC    AL1(EOT)            End   of   table                             
         EJECT ,                                                                
         SPACE 1                                                                
*                                  ************************************         
*                                  *     Rounding table     by        *         
RNDTAB   DS    0C                  *          language                *         
*                                  *                                  *         
*                                  *     Each entry    contains  the  *         
*                                  *     char used to  display   the  *         
*                                  *     cash round    by   amount    *         
*                                  *     options.      Sample         *         
*                                  *     currency  equivalents   are  *         
*                                  *     included.                    *         
*                                  ************************************         
*                                                                               
*                                  For  the  US  English    we   have:          
*                                  .    language                                
*                                  .    fraction-penny  - $ .00                 
*                                  .    whole   -dollar - $1.00                 
*                                  .    thousand-     $1,000.00                 
*                                  .    million - $1,000,000.00                 
         DC    AL1(LANGEUS),CL4'PDTM'                                           
*                                                                               
*                                  For  the  UK  English    we   have:          
*                                  .    language                                
*                                  .    fraction-pence   -  .00                 
*                                  .    whole   -shilling- 1.00 pound           
*                                  .    thousand-      1,000.00 pound           
*                                  .    million -  1,000,000.00 pound           
         DC    AL1(LANGEUK),CL4'PSTM'                                           
*                                                                               
*                                  For  Germany  we   have:                     
*                                  .    language                                
*                                  .    fraction-pfennig -  .00                 
*                                  .    whole   -DM      - 1.00                 
*                                  .    tousand -      1,000.00 DM              
*                                  .    million -  1,000,000.00 DM              
         DC    AL1(LANGGER),CL4'PDTM'                                           
*                                                                               
*                                  Note:ACSCR05  has  no   code for:            
*                                  .    France:  centimes  and  francs          
*                                  .    Europe:  euros                          
*                                                                               
*                                  Default   characters                         
         DC    AL1(LANGENG),CL4'PDTM'                                           
         EJECT ,                                                                
***********************************************************************         
*     Down-load option date table                                     *         
***********************************************************************         
         SPACE 1                                                                
DATETAB  DC    AL1(LANGEUS,00),CL10'MMMDD/YY'      DATCON    8   ( 9)           
         DC    AL1(LANGEUS,01),CL10'YYMMDD'                  0                  
         DC    AL1(LANGEUS,20),CL10'YYYYMMDD'               20                  
         DC    AL1(LANGEUS,21),CL10'MMMDD/YYYY'             21                  
         DC    AL1(LANGEUS,23),CL10'YYYY-MM-DD'             23                  
         DC    AL1(LANGEUS,10),CL10'MM/DD/YY'               10                  
         DC    AL1(LANGEUS,30),CL10'DDMMMYY'                NONE                
*                                                                               
         DC    AL1(LANGEUK,00),CL10'DDMMMYY'       DATCON    8                  
         DC    AL1(LANGEUK,01),CL10'YYMMDD'                  0                  
         DC    AL1(LANGEUK,20),CL10'YYYYMMDD'               20                  
         DC    AL1(LANGEUK,21),CL10'DDMMMYYYY'              21                  
         DC    AL1(LANGEUK,23),CL10'YYYY-MM-DD'             23                  
         DC    AL1(LANGEUK,10),CL10'DD/MM/YY'               10                  
         DC    AL1(LANGEUK,11),CL10'MMMDD/YY'               11                  
         DC    AL1(LANGEUK,05),CL10'DD MMM YY'               5                  
         DC    AL1(LANGEUK,13),CL10'DD.MM.YY'               13                  
*                                                                               
         DC    AL1(LANGGER,00),CL10'DD.MM.YY'      DATCON    8                  
         DC    AL1(LANGGER,01),CL10'YYMMDD'                  0                  
         DC    AL1(LANGGER,20),CL10'YYYYMMDD'               20                  
         DC    AL1(LANGGER,21),CL10'DD.MM.YYYY'             21                  
         DC    AL1(LANGGER,23),CL10'YYYY-MM-DD'             23                  
         DC    AL1(LANGGER,10),CL10'DD/MM/YY'               10                  
         DC    AL1(LANGGER,11),CL10'MMMDD/YY'               11                  
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*     Report  profiles report type table                              *         
***********************************************************************         
         SPACE 1                                                                
RPRTTAB  DC    AL1(PRODUCTION),AL3(RPPRODT)   Production                        
         DC    AL1(PAYABLES),AL3(RPPAYT)      Pay                               
         DC    AL1(RECEIVABLE),AL3(RPRCVT)    Receivables                       
         DC    AL1(INCOME),AL3(RPINCT)        Income                            
         DC    AL1(EXPENSE),AL3(RPEXPT)       Expense                           
         DC    AL1(MANPOWER),AL3(RPPERST)     Person                            
         DC    AL1(PNL),AL3(RPPNLT)           P&L                               
         DC    AL1(CASH),AL3(RPCASHT)         Cash (Bank)                       
         DC    AL1(GENERAL),AL3(RPGNLT)       G/L                               
*        DC    AL1(MEDIA),AL3(RPMEDT)         Media                             
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*     List of   'Production' report profile fields                    *         
***********************************************************************         
         SPACE 1                                                                
RPPRODT  DC    AL1(F#UL,0)             Unit and ledger                          
         DC    AL1(F#SPCACT,0)         Specific account(s)                      
         DC    AL1(F#SCONTR,0)         Specific contra(s)                       
         DC    AL1(F#BLGGRP,0)         Billing group                            
         DC    AL1(F#FLTSCM,RPRFNNUM)  Filter scheme                            
         DC    AL1(F#OFFGRP,0)         Office group                             
         DC    AL1(F#OFFICE,0)         Office                                   
         DC    AL1(F#MEDGRP,0)         Media group                              
         DC    AL1(F#MEDIA,0)          Media                                    
         DC    AL1(F#WCGRP,0)          Work code group                          
         DC    AL1(F#WC,0)             Work code                                
         DC    AL1(F#BLGTYP,0)         Billing type                             
         DC    AL1(F#ESTSTA,0)         Estimate status                          
*&&US*&& DC    AL1(F#STUDTY,0)         Studio type                              
         DC    AL1(F#USRFLD,0)         User field                               
         DC    AL1(F#TYTIME,0)         Type of time                             
         DC    AL1(F#TYPBAL,0)         Type of balance                          
         DC    AL1(F#TRNTYP,0)         Transaction type(s)                      
         DC    AL1(F#INCUTR,0)         Include utilized transactions            
         DC    AL1(F#INCCLS,0)         Include closed jobs                      
*&&US*&& DC    AL1(F#INCEXP,0)         Include expense jobs                     
         DC    AL1(F#INCREV,0)         Include reversals                        
         DC    AL1(F#INCLCK,RPRFRNUM)  Include locked accounts                  
         DC    AL1(F#IDRAFT,0)         Include draft transactions               
         DC    AL1(F#INCHLT,0)         Include held transactions                
         DC    AL1(F#ICBLWO,0)         Include CBIL W/Os and  transfers         
         DC    AL1(F#INCEST,0)         Include estimates                        
         DC    AL1(F#INCPOS,0)         Include purchase  orders                 
         DC    AL1(F#MRGPOS,0)         Merge   purchase  orders                 
         DC    AL1(F#AGMTHD,0)         Ageing  method                           
         DC    AL1(F#IN99WC,0)         Include 99 w/WC grp or type flt          
*&&US*&& DC    AL1(F#TIMWCT,0)         Time  work code types                    
*&&US*&& DC    AL1(F#OOPWCT,0)         Out of pocket WC types                   
         DC    AL1(F#EXCRSN,0)         Exception  reasons                       
*&&US*&& DC    AL1(F#PRCLVL,0)         Price levels                             
*&&UK*&& DC    AL1(F#AUTHST,0)         Auth status                              
*&&UK*&& DC    AL1(F#INJLES,0)         Include jobs locked - Estimates          
*&&UK*&& DC    AL1(F#INJLOR,0)         Include jobs locked - Orders             
*&&UK*&& DC    AL1(F#INJLBI,0)         Include jobs locked - Billing            
*&&UK*&& DC    AL1(F#INJLTI,0)         Include jobs locked - Time               
*&&UK*&& DC    AL1(F#INJLAD,0)         Include jobs locked - Adjust             
*&&UK*&& DC    AL1(F#INJLEX,0)         Include jobs locked - 3rd Party          
*&&UK*&& DC    AL1(F#INSUBJ,0)         Include sub-job                          
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*     List of 'Payables' report profile fields                        *         
***********************************************************************         
         SPACE 1                                                                
RPPAYT   DC    AL1(F#UL,0)             Unit and ledger                          
         DC    AL1(F#SPCACT,0)         Specific account(s)                      
         DC    AL1(F#FLTSCM,0)         Filter scheme                            
         DC    AL1(F#CPJEST,0)         Cli/Pro/Job(Est)                         
         DC    AL1(F#OFFICE,0)         Office                                   
         DC    AL1(F#TRNTYP,0)         Transaction type(s)                      
         DC    AL1(F#INCAPR,RPRFSKIP)  Include approved items                   
         DC    AL1(F#ONYCRP,0)         Only show credit payees                  
         DC    AL1(F#INCHLD,0)         Include held items                       
         DC    AL1(F#ONYCD$,0)         Only show cash discount items            
         DC    AL1(F#INCOFF,0)         Include offset items                     
         DC    AL1(F#INCLCK,0)         Include locked accounts                  
         DC    AL1(F#INCREV,0)         Include reversals                        
         DC    AL1(F#IPVNDR,0)         Include PAY+NO vendors                   
         DC    AL1(F#IDRAFT,0)         Include draft transactions               
         DC    AL1(F#CSHDIS,0)         Cash disbursement                        
         DC    AL1(F#INCURG,0)         Include urgent items                     
*&&US*&& DC    AL1(F#INCEXP,0)         Include expense jobs                     
*&&UK*&& DC    AL1(F#AUTHST,0)         Auth status                              
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*   List of  'Receivables' report profile fields                      *         
***********************************************************************         
         SPACE 1                                                                
RPRCVT   DC    AL1(F#TYPRPT,0)         Type of report                           
         DC    AL1(F#SPCACT,0)         Specific account(s)                      
         DC    AL1(F#FLTSCM,0)         Filter  scheme                           
         DC    AL1(F#BLGSRC,0)         Billing source                           
*&&US*&& DC    AL1(F#CPJEST,0)         Cli/Pro/Job(Est)                         
         DC    AL1(F#OFFICE,0)         Office                                   
         DC    AL1(F#TRNTYP,0)         Transaction type(s)                      
         DC    AL1(F#INCREV,RPRFSKIP)  Include reversals                        
*&&US*&& DC    AL1(F#INCACR,0)         Include accrual reversals                
         DC    AL1(F#INCLCK,0)         Include locked  accounts                 
         DC    AL1(F#SHWOUT,0)         Only show outstanding items              
         DC    AL1(F#CSHRCT,0)         Cash receipts                            
*&&US*&& DC    AL1(F#IDRAFT,0)         Include draft transactions               
         DC    AL1(F#INCHLQ,0)         Include held/queried items               
*&&UK*&& DC    AL1(F#INCCLR,0)         Include cleared items                    
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*     List of 'Income' report profile fields                          *         
***********************************************************************         
         SPACE 1                                                                
RPINCT   DC    AL1(F#TYPRPT,0)         Type  of   report                        
         DC    AL1(F#SINCAC,0)         Specific   income    accounts            
         DC    AL1(F#SCSTAI,0)         Specific   cost account(s)               
         DC    AL1(F#SCONTR,0)         Specific   contra(s)                     
         DC    AL1(F#FLTSCM,0)         Filter     scheme                        
         DC    AL1(F#CPJEST,0)         Cli/Pro/Job(Est)                         
         DC    AL1(F#OFFICE,0)         Office                                   
         DC    AL1(F#TRNTYP,0)         Transaction     type(s)                  
         DC    AL1(F#INCREV,RPRFSKIP)  Include    reversals                     
         DC    AL1(F#INCACR,0)         Include accrual reversals                
         DC    AL1(F#INCLCK,0)         Include locked accounts                  
         DC    AL1(F#IDRAFT,0)         Include draft transactions               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*     List of 'Expense' report profile fields                         *         
***********************************************************************         
         SPACE 1                                                                
RPEXPT   DC    AL1(F#UL,0)             Unit  and  ledger                        
         DC    AL1(F#SPCACT,0)         Specific   account(s)                    
         DC    AL1(F#SCONTR,0)         Specific   contra(s)                     
         DC    AL1(F#FLTSCM,0)         Filter     scheme                        
         DC    AL1(F#TRNTYP,0)         Transaction     type(s)                  
         DC    AL1(F#SCSTAC,0)         Specific   cost account(s)               
         DC    AL1(F#OFFICE,0)         Office                                   
         DC    AL1(F#DEPTS,0)          Departments                              
         DC    AL1(F#PERSON,0)         Person                                   
         DC    AL1(F#DIREXP,0)         Direct expense account(s)                
         DC    AL1(F#VENDOR,0)         Vendor                                   
         DC    AL1(F#INCLCK,RPRFSKIP)  Include locked accounts                  
         DC    AL1(F#INCREV,0)         Include reversals                        
         DC    AL1(F#IDRAFT,0)         Include draft transactions               
         DC    AL1(F#INCNLD,0)         Include accounts  anayzed  by            
         DC    AL1(F#INCNLS,0)         Include accounts  anayzed  by            
         DC    AL1(F#INCNLC,0)         Include accounts  anayzed  by            
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*     List of 'Person' report profile fields                          *         
***********************************************************************         
         SPACE 1                                                                
RPPERST  DC    AL1(F#TYPRPT,0)         Type  of   report                        
         DC    AL1(F#SPCACT,0)         Specific   account(s)                    
         DC    AL1(F#FLTSCM,0)         Filter scheme                            
         DC    AL1(F#CFLTSC,0)         Contra filter scheme                     
         DC    AL1(F#CSTNCL,0)         Cost/Non-cli (contra   acc)              
*&&US*&& DC    AL1(F#CPJ,0)            Cli/Prd/Job                              
         DC    AL1(F#TASK,0)           Task                                     
         DC    AL1(F#OFFVAR,0)         Variable type of office text             
         DC    AL1(F#CLIOFF,0)         Client   office                          
         DC    AL1(F#LOCSTA,0)         Location status                          
         DC    AL1(F#METHOD,0)         Method                                   
         DC    AL1(F#PAYCDE,0)         Payroll code                             
         DC    AL1(F#INCLCK,RPRFSKIP)  Include locked accounts                  
         DC    AL1(F#TIMEWK,0)         Time sheet worksheet                     
         DC    AL1(F#SVTIME,0)         Include saved time sheets                
         DC    AL1(F#OFFSEC,0)         Office security                          
         DC    AL1(F#FILDAY,0)         Filter period date range by day          
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*     List of 'P&L' report profile fields                             *         
***********************************************************************         
         SPACE 1                                                                
RPPNLT   DC    AL1(F#TYPRPT,0)         Type  of   report                        
         DC    AL1(F#SPCACT,0)         Specific   account(s)                    
         DC    AL1(F#FLTSCM,0)         Filter     scheme                        
         DC    AL1(F#CLIENT,0)         Client                                   
         DC    AL1(F#OFFICE,0)         Office                                   
         DC    AL1(F#METHOD,0)         Method                                   
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*     List of 'Cash' report profile fields                            *         
***********************************************************************         
         SPACE 1                                                                
RPCASHT  DC    AL1(F#TYPRPT,0)         Type  of report                          
         DC    AL1(F#SPCACT,0)         Specific account(s)                      
         DC    AL1(F#SCONTR,0)         Specific contra(s)                       
         DC    AL1(F#FLTSCM,0)         Filter scheme                            
         DC    AL1(F#OFFICE,0)         Office                                   
         DC    AL1(F#TRNTYP,0)         Transaction type(s)                      
         DC    AL1(F#RCONCL,RPRFSKIP)  Report bank reconciled items             
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*     List of 'G/L' report profile fields                             *         
***********************************************************************         
         SPACE 1                                                                
RPGNLT   DC    AL1(F#UL,0)             Unit  and  ledger                        
         DC    AL1(F#SPCACT,0)         Specific   account(s)                    
         DC    AL1(F#SCONTR,0)         Specific   contra(s)                     
         DC    AL1(F#FLTSCM,0)         Filter     scheme                        
         DC    AL1(F#OFFICE,0)         Office                                   
         DC    AL1(F#TRNTYP,0)         Transaction     type(s)                  
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*     List of 'Media' report profile fields                           *         
***********************************************************************         
         SPACE 1                                                                
RPMEDT   DC    AL1(EOT)            End   of   table                             
         EJECT ,                                                                
***********************************************************************         
*     Report profiles field number table                              *         
***********************************************************************         
         SPACE 1                                                                
RPR#TAB  DC    AL1(F#TYPRPT,RFLLDG)           Type of   report                  
         DC    AL2(AC#TYPRP),AL1(0,0)                                           
         DC    AL1(RPR#TYPR)                                                    
*                                                                               
         DC    AL1(F#UL,RFLLDG)               Unit and  ledger                  
         DC    AL2(AC#UNTLD),AL1(0,0)                                           
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#SPCACT,RFLACC)           Specific  account(s)              
         DC    AL2(1652),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#SINCAC,0)                Specific  income                  
         DC    AL2(1653),AL1(0,RPR#MSG)                 account(s)              
         DC    AL1(RPR#SINA)                            (INC  only)             
*                                                                               
         DC    AL1(F#SCSTAI,0)                Specific  cost                    
         DC    AL2(1654),AL1(0,RPR#MSG)                 account(s)              
         DC    AL1(RPR#SCSA)                            (INC  only)             
*                                                                               
         DC    AL1(F#SCSTAC,RFLCOST)          Specific  cost                    
         DC    AL2(1654),AL1(0,RPR#MSG)                 account(s)              
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#BLGSRC,RFLBSR)           Billing   source                  
         DC    AL2(AC#BLGSR),AL1(0,0)                                           
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#BLGGRP,RFLBLGP)          Billing   group                   
         DC    AL2(AC#BLGGP),AL1(0,0)                                           
         DC    AL1(RPR#VALN)                                                    
*                                                                               
         DC    AL1(F#BLGTYP,RFLBTYP)          Billing   type                    
         DC    AL2(AC#BLGTY),AL1(0,0)                                           
         DC    AL1(RPR#VALN)                                                    
*                                                                               
         DC    AL1(F#SCONTR,RFLCNTR)          Specific  contra(s)               
         DC    AL2(1671),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#CSTNCL,RFLCNTR)          Cost/Non-cli                      
         DC    AL2(AC#CSTNC),AL1(0,0)                  (contra    acc)          
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#FLTSCM,0)                Filter    scheme                  
         DC    AL2(AC#FLTSC),AL1(0,0)                                           
         DC    AL1(RPR#FLTS)                                                    
*                                                                               
         DC    AL1(F#CFLTSC,0)                Contra    filter                  
         DC    AL2(AC#CFLTS),AL1(0,0)                   scheme                  
         DC    AL1(RPR#CFLT)                                                    
*                                                                               
         DC    AL1(F#TRNTYP,RFLTTYPE)         Transaction    type               
         DC    AL2(AC#TRNTP),AL1(0,0)                                           
         DC    AL1(RPR#TRAN)                                                    
*                                                                               
         DC    AL1(F#CLIENT,RFLCLI)           Client                            
         DC    AL2(AC#CLINT),AL1(0,0)                                           
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#CPJ,RFLCLI)              Cli/Prd/Job                       
         DC    AL2(AC#CPJ),AL1(0,0)                                             
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#CPJEST,RFLCLI)           Cli/Pro/Job(Est)                  
         DC    AL2(AC#CLIPJ),AL1(0,0)                                           
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#CLIOFF,RFLCOFF)          Client    office                  
         DC    AL2(AC#RSOCC),AL1(0,0)                                           
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#OFFICE,RFLOFF)           Office                            
         DC    AL2(AC#OFF),AL1(0,0)                                             
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#OFFVAR,0)                Variable  type of                 
         DC    AL2(0),AL1(0,0)                          office   text           
         DC    AL1(RPR#OFFV)                                                    
*                                                                               
         DC    AL1(F#OFFGRP,RFLOFGP)          Office    group                   
         DC    AL2(AC#OFFGP),AL1(0,0)                                           
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#DEPTS,RFLDEPT)           Departments                       
         DC    AL2(AC#DPTS),AL1(0,0)                                            
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#PERSON,RFLPRSN)          Person                            
         DC    AL2(AC#PRSN),AL1(0,0)                                            
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#DIREXP,RFLXCAT)          Direct    expense                 
         DC    AL2(1655),AL1(0,RPR#MSG)                 account(s)              
         DC    AL1(RPR#NAME)                  (Expense  category  13)           
*                                                                               
         DC    AL1(F#VENDOR,RFLVNDR)          Vendor                            
         DC    AL2(AC#VNDR),AL1(0,0)                                            
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#MEDIA,RFLMED)            Media                             
         DC    AL2(AC#MED),AL1(0,0)                                             
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#MEDGRP,RFLMDGP)          Media     group                   
         DC    AL2(AC#MEDGP),AL1(0,0)                                           
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#TASK,RFLWC)              Task                              
         DC    AL2(AC#TASK),AL1(0,0)                                            
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#WC,RFLWC)                Work code                         
         DC    AL2(AC#WC),AL1(0,0)                                              
         DC    AL1(RPR#NAME)                                                    
*                                                                               
         DC    AL1(F#WCGRP,RFLWCGP)           Work code group                   
         DC    AL2(AC#WCGRP),AL1(0,0)                                           
         DC    AL1(RPR#NAME)                                                    
*                                                                               
*&&US                                                                           
         DC    AL1(F#STUDTY,RFLSTTY)          Studio    type                    
         DC    AL2(AC#STUTY),AL1(0,0)                                           
         DC    AL1(RPR#VALN)                                                    
*&&                                                                             
*                                                                               
         DC    AL1(F#USRFLD,RFLUFLD)          User field                        
         DC    AL2(AC#RSUSF),AL1(0,0)                                           
         DC    AL1(RPR#VALN)                                                    
*                                                                               
         DC    AL1(F#TYTIME,RFLTTIME)         Type of   time                    
         DC    AL2(5659),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#VALN)                                                    
*                                                                               
*&&US                                                                           
         DC    AL1(F#TIMWCT,RFLTWTP)          Time work code types              
         DC    AL2(5653),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#NAME)                                                    
*&&                                                                             
*                                                                               
*&&US                                                                           
         DC    AL1(F#OOPWCT,RFLOPTP)          Out  of   pocket                  
         DC    AL2(5654),AL1(0,RPR#MSG)            work code types              
         DC    AL1(RPR#NAME)                                                    
*&&                                                                             
*                                                                               
         DC    AL1(F#EXCRSN,RFLXCPT)          Exception reasons                 
         DC    AL2(5652),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#NAME)                                                    
*                                                                               
*&&US                                                                           
         DC    AL1(F#PRCLVL,RFLPRCLV)         Price     levels                  
         DC    AL2(5656),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#NAME)                                                    
*&&                                                                             
*                                                                               
         DC    AL1(F#METHOD,RFLMTHD)          Method                            
         DC    AL2(AC#METH),AL1(0,0)                                            
         DC    AL1(RPR#MTHD)                                                    
*                                                                               
         DC    AL1(F#LOCSTA,RFLLOCS)          Location  status                  
         DC    AL2(AC#RSLST),AL1(0,0)                                           
         DC    AL1(RPR#LOCS)                                                    
*                                                                               
         DC    AL1(F#PAYCDE,RFLPCDE)          Payroll   code                    
         DC    AL2(AC#RSPYC),AL1(0,0)                                           
         DC    AL1(RPR#PAYC)                                                    
*&&UK                                                                           
         DC    AL1(F#AUTHST,RFLAUTH)          Auth     status                   
         DC    AL2(5683),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#AUTH)                                                    
*&&                                                                             
         DC    AL1(F#INCREV,0)                Include   reversals               
         DC    AL2(1647),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#IREV)                                                    
*                                                                               
         DC    AL1(F#INCACR,0)                Include   accrual                 
         DC    AL2(1656),AL1(0,RPR#MSG)                 reversals               
         DC    AL1(RPR#IACR)                                                    
*                                                                               
         DC    AL1(F#INCLCK,0)                Include   locked                  
         DC    AL2(1646),AL1(0,RPR#MSG)                 accounts                
         DC    AL1(RPR#ILCK)                                                    
*                                                                               
         DC    AL1(F#SHWOUT,0)                Only show outstanding             
         DC    AL2(1640),AL1(0,RPR#MSG)                 items                   
         DC    AL1(RPR#SHWO)                                                    
*                                                                               
         DC    AL1(F#CSHRCT,0)                Cash receipts                     
         DC    AL2(1657),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#CSHR)                                                    
*                                                                               
         DC    AL1(F#CSHDIS,0)                Cash disbursement                 
         DC    AL2(1650),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#CSHD)                                                    
*                                                                               
         DC    AL1(F#IDRAFT,0)                Include   draft                   
         DC    AL2(1649),AL1(0,RPR#MSG)                 transactions            
         DC    AL1(RPR#DRFT)                                                    
*                                                                               
         DC    AL1(F#INCHLD,0)                Include   held items              
         DC    AL2(1643),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#IHLD)                                                    
*                                                                               
         DC    AL1(F#INCHLT,0)                Include   held                    
         DC    AL2(5647),AL1(0,RPR#MSG)                 transactions            
         DC    AL1(RPR#IHLD)                                                    
*                                                                               
         DC    AL1(F#INCHLQ,0)                Include   held/queried            
         DC    AL2(1659),AL1(0,RPR#MSG)                 items                   
         DC    AL1(RPR#IHLQ)                                                    
*                                                                               
*&&UK                                                                           
         DC    AL1(F#INCCLR,0)                Include   cleared   items         
         DC    AL2(1690),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#ICLR)                                                    
*&&                                                                             
*                                                                               
         DC    AL1(F#INCAPR,0)                Include   approved  items         
         DC    AL2(1641),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#IAPR)                                                    
*                                                                               
         DC    AL1(F#ONYCRP,0)                Only show credit                  
         DC    AL2(1642),AL1(0,RPR#MSG)                 payees                  
         DC    AL1(RPR#OCRP)                                                    
*                                                                               
         DC    AL1(F#ONYCD$,0)                Only show cash discount           
         DC    AL2(1644),AL1(0,RPR#MSG)                 items                   
         DC    AL1(RPR#OCD$)                                                    
*                                                                               
         DC    AL1(F#INCOFF,0)                Include   offset                  
         DC    AL2(1645),AL1(0,RPR#MSG)                 items                   
         DC    AL1(RPR#IOFF)                                                    
*                                                                               
         DC    AL1(F#IPVNDR,0)                Include   PAY=NO                  
         DC    AL2(1648),AL1(0,RPR#MSG)                 vendors                 
         DC    AL1(RPR#PVND)                                                    
*                                                                               
         DC    AL1(F#INCURG,0)                Include   urgent                  
         DC    AL2(1651),AL1(0,RPR#MSG)                 items                   
         DC    AL1(RPR#URGN)                                                    
*                                                                               
         DC    AL1(F#INCNLD,0)                Include   accounts                
         DC    AL2(1668),AL1(0,RPR#MSG)                 analyzed by             
         DC    AL1(RPR#INLD)                            department              
*                                                                               
         DC    AL1(F#INCNLS,0)                Include   accounts                
         DC    AL2(1669),AL1(0,RPR#MSG)                 analyzed by             
         DC    AL1(RPR#INLS)                            staff                   
*                                                                               
         DC    AL1(F#INCNLC,0)                Include   accounts                
         DC    AL2(1670),AL1(0,RPR#MSG)                 analyzed by             
         DC    AL1(RPR#INLC)                            client                  
*                                                                               
         DC    AL1(F#ESTSTA,0)                Estimate  status                  
         DC    AL2(5658),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#ESTA)                                                    
*                                                                               
         DC    AL1(F#TYPBAL,0)                Type of   balance                 
         DC    AL2(5660),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#TBAL)                                                    
*                                             Include   utilized                
         DC    AL1(F#INCUTR,0)                     transactions                 
         DC    AL2(5661),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#IUTR)                                                    
*                                                                               
         DC    AL1(F#INCCLS,0)                Include   closed    jobs          
         DC    AL2(5662),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#ICLS)                                                    
*                                                                               
         DC    AL1(F#INCEXP,0)                Include   expense   jobs          
         DC    AL2(5663),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#IEXP)                                                    
*                                                                               
         DC    AL1(F#ICBLWO,0)                Include   CBIL W/Os and           
         DC    AL2(5648),AL1(0,RPR#MSG)                 transfers               
         DC    AL1(RPR#ICWO)                                                    
*                                                                               
         DC    AL1(F#INCEST,0)                Include   estimates               
         DC    AL2(5649),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#IEST)                                                    
*                                                                               
         DC    AL1(F#INCPOS,0)                Include   purchase                
         DC    AL2(5650),AL1(0,RPR#MSG)                 orders                  
         DC    AL1(RPR#IPOS)                                                    
*                                                                               
         DC    AL1(F#MRGPOS,0)                Merge     purchase                
         DC    AL2(5651),AL1(0,RPR#MSG)                 orders                  
         DC    AL1(RPR#MPOS)                                                    
*                                                                               
         DC    AL1(F#AGMTHD,0)                Job       ageing                  
*        DC    AL2(5655),AL1(0,RPR#MSG)                 method                  
         DC    AL2(5900),AL1(0,RPR#MSG)       For       future    use           
         DC    AL1(RPR#AGMD)                                                    
*                                                                               
         DC    AL1(F#IN99WC,0)                Include   99   with WC            
         DC    AL2(5667),AL1(0,RPR#MSG)                 group     or            
         DC    AL1(RPR#99WC)                            type filters            
*                                                                               
         DC    AL1(F#RCONCL,0)                Report    bank reconciled         
         DC    AL2(1698),AL1(0,RPR#MSG)                 items                   
         DC    AL1(RPR#RCNC)                                                    
*                                                                               
         DC    AL1(F#TIMEWK,0)                Time      sheet                   
         DC    AL2(1673),AL1(0,RPR#MSG)                 worksheet               
         DC    AL1(RPR#TMWK)                                                    
*                                                                               
         DC    AL1(F#SVTIME,0)                Include   saved                   
         DC    AL2(1674),AL1(0,RPR#MSG)                 time sheets             
         DC    AL1(RPR#SVTM)                                                    
*                                                                               
         DC    AL1(F#OFFSEC,0)                Office    security                
         DC    AL2(1675),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#OFSC)                                                    
*&&US                                                                           
         DC    AL1(F#FILDAY,0)                Filter    period date             
         DC    AL2(1677),AL1(0,RPR#MSG)                 range by day            
         DC    AL1(RPR#FPDD)                                                    
*&&                                                                             
*&&UK                                                                           
         DC    AL1(F#INJLES,0)                Include   jobs locked             
         DC    AL2(5684),AL1(0,RPR#MSG)                 from Estimates          
         DC    AL1(RPR#JLES)                                                    
*                                                                               
         DC    AL1(F#INJLOR,0)                Include   jobs locked             
         DC    AL2(5685),AL1(0,RPR#MSG)                 from Orders             
         DC    AL1(RPR#JLOR)                                                    
*                                                                               
         DC    AL1(F#INJLBI,0)                Include   jobs locked             
         DC    AL2(5686),AL1(0,RPR#MSG)                 from Billing            
         DC    AL1(RPR#JLBI)                                                    
*                                                                               
         DC    AL1(F#INJLTI,0)                Include   jobs locked             
         DC    AL2(5687),AL1(0,RPR#MSG)                 from Time               
         DC    AL1(RPR#JLTI)                                                    
*                                                                               
         DC    AL1(F#INJLAD,0)                Include   jobs locked             
         DC    AL2(5688),AL1(0,RPR#MSG)                 from Adjust             
         DC    AL1(RPR#JLAD)                                                    
*                                                                               
         DC    AL1(F#INJLEX,0)                Include   jobs locked             
         DC    AL2(5689),AL1(0,RPR#MSG)                 from 3rd Party          
         DC    AL1(RPR#JLEX)                                                    
*                                                                               
         DC    AL1(F#INSUBJ,0)                Include   sub-job                 
         DC    AL2(5690),AL1(0,RPR#MSG)                                         
         DC    AL1(RPR#SUBJ)                                                    
*                                                                               
         DC    AL1(F#FILDAY,0)                Filter    period date             
         DC    AL2(0055),AL1(0,RPR#MSG)                 range by day            
         DC    AL1(RPR#FPDD)                                                    
*&&                                                                             
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*     Report profiles routine numbers table                           *         
***********************************************************************         
         SPACE 1                                                                
RPRRTNT  DC    AL1(RPR#TYPR),AL3(RPRTTYPR)    Type of   report                  
*                                             Special   income                  
         DC    AL1(RPR#SINA),AL3(RPRTSINA)         accounts  (INC only)         
*                                             Special   cost                    
         DC    AL1(RPR#SCSA),AL3(RPRTSCSA)         accounts  (INC only)         
*                                             Variable  type of                 
         DC    AL1(RPR#OFFV),AL3(RPRTOFFV)         office    text               
         DC    AL1(RPR#NAME),AL3(RPRTNAME)    Name field                        
         DC    AL1(RPR#VALN),AL3(RPRTVALN)    Value     1st/2nd   field         
         DC    AL1(RPR#FLTS),AL3(RPRTFLTS)    Filters                           
         DC    AL1(RPR#CFLT),AL3(RPRTCFLT)    Contra    filters                 
         DC    AL1(RPR#TRAN),AL3(RPRTTRAN)    Transaction                       
         DC    AL1(RPR#MTHD),AL3(RPRTMTHD)    Method                            
         DC    AL1(RPR#LOCS),AL3(RPRTLOCS)    Location  status                  
         DC    AL1(RPR#PAYC),AL3(RPRTPAYC)    Payroll   code                    
*&&UK*&& DC    AL1(RPR#AUTH),AL3(RPRTAUTH)    Auth      status                  
*                                                                               
         DC    AL1(RPR#IREV),AL3(RPRTIREV)    Include   reversals               
*                                             Include   accrual                 
         DC    AL1(RPR#IACR),AL3(RPRTIACR)              reversals               
*                                             Include   locked                  
         DC    AL1(RPR#ILCK),AL3(RPRTILCK)              accounts                
*                                             Only show outstanding             
         DC    AL1(RPR#SHWO),AL3(RPRTSHWO)              items                   
*                                             Cash receipts  &                  
         DC    AL1(RPR#CSHR),AL3(RPRTCSHR)    Cash disbursement                 
*                                             Include   draft                   
         DC    AL1(RPR#DRFT),AL3(RPRTDRFT)              transactions            
         DC    AL1(RPR#IHLD),AL3(RPRTIHLD)    Include   held items              
*                                             Include   held/queried            
         DC    AL1(RPR#IHLQ),AL3(RPRTIHLQ)              items                   
*&&UK*&& DC    AL1(RPR#ICLR),AL3(RPRTICLR)    Include   cleared items           
*                                             Include   approved                
         DC    AL1(RPR#IAPR),AL3(RPRTIAPR)              items                   
*                                             Only show credit                  
         DC    AL1(RPR#OCRP),AL3(RPRTOCRP)              payees                  
*                                             Only show cash discount           
         DC    AL1(RPR#OCD$),AL3(RPRTOCD$)              items                   
*                                             Include   offset                  
         DC    AL1(RPR#IOFF),AL3(RPRTIOFF)              items                   
*                                             Include   PAY=NO                  
         DC    AL1(RPR#PVND),AL3(RPRTPVND)              vendors                 
*                                             Include   urgent                  
         DC    AL1(RPR#URGN),AL3(RPRTURGN)              items                   
*                                             Include   accounts                
*                                                       analyzed  by            
         DC    AL1(RPR#INLD),AL3(RPRTINLD)              department              
*                                             Include   accounts                
*                                                       analyzed  by            
         DC    AL1(RPR#INLS),AL3(RPRTINLS)              staff                   
*                                             Include   accounts                
*                                                       analyzed  by            
         DC    AL1(RPR#INLC),AL3(RPRTINLC)              client                  
         DC    AL1(RPR#ESTA),AL3(RPRTESTA)    Estimate  status                  
         DC    AL1(RPR#TBAL),AL3(RPRTTBAL)    Type of   balance                 
*                                             Include   utilized                
         DC    AL1(RPR#IUTR),AL3(RPRTIUTR)              transactions            
         DC    AL1(RPR#ICLS),AL3(RPRTICLS)    Include   closed    jobs          
*&&US*&& DC    AL1(RPR#IEXP),AL3(RPRTIEXP)    Include   expense   jobs          
*                                             Include   CBIL W/Os and           
         DC    AL1(RPR#ICWO),AL3(RPRTICWO)              transfers               
         DC    AL1(RPR#IEST),AL3(RPRTIEST)    Include   estimates               
*                                             Include   purchase                
         DC    AL1(RPR#IPOS),AL3(RPRTIPOS)              orders                  
*                                             Merge     purchase                
         DC    AL1(RPR#MPOS),AL3(RPRTMPOS)              orders                  
*                                             Job       ageing                  
         DC    AL1(RPR#AGMD),AL3(RPRTAGMD)              method                  
*                                             Include   99   with WC            
*                                                       group     or            
         DC    AL1(RPR#99WC),AL3(RPRT99WC)              type filters            
*                                             Report    bank reconciled         
         DC    AL1(RPR#RCNC),AL3(RPRTRCNC)              items                   
*                                             Time      sheet                   
         DC    AL1(RPR#TMWK),AL3(RPRTTMWK)              worksheet               
*                                             Include   saved                   
         DC    AL1(RPR#SVTM),AL3(RPRTSVTM)              time sheets             
         DC    AL1(RPR#OFSC),AL3(RPRTOFSC)    Office    security                
*&&US*&& DC    AL1(RPR#FPDD),AL3(RPRTFPDD)    TIME BY DAY                       
*&&UK*&& DC    AL1(RPR#JLES),AL3(RPRTJLES)    Include   JL - Estimates          
*&&UK*&& DC    AL1(RPR#JLOR),AL3(RPRTJLOR)    Include   JL - Orders             
*&&UK*&& DC    AL1(RPR#JLBI),AL3(RPRTJLBI)    Include   JL - Billing            
*&&UK*&& DC    AL1(RPR#JLTI),AL3(RPRTJLTI)    Include   JL - Time               
*&&UK*&& DC    AL1(RPR#JLAD),AL3(RPRTJLAD)    Include   JL - Adjust             
*&&UK*&& DC    AL1(RPR#JLEX),AL3(RPRTJLEX)    Include   JL - 3rd Party          
*&&UK*&& DC    AL1(RPR#SUBJ),AL3(RPRTSUBJ)    Include   sub-job                 
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*     Report profiles type of report table                            *         
***********************************************************************         
         SPACE 1                                                                
RPRTTYRT DC    AL1(RECEIVABLE)     RCV   Receivables                            
         DC    CL2'SR'                                                          
         DC    AL2(AC#RSRCV)       Receivable                                   
*                                                                               
         DC    AL1(RECEIVABLE)     RCV   Receivables                            
         DC    CL2'SA'                                                          
         DC    AL2(AC#ADVNS)       Advances                                     
*                                                                               
         DC    AL1(RECEIVABLE)     RCV   Receivables                            
         DC    CL2'SB'                                                          
         DC    AL2(AC#BLMSC)       Balance/Misc.                                
*                                                                               
         DC    AL1(INCOME)         INC   Income                                 
         DC    AL2(ULSI)                                                        
         DC    AL2(AC#INCM)        Income                                       
*                                                                               
         DC    AL1(INCOME)         INC   Income                                 
         DC    AL2(ULSK)                                                        
         DC    AL2(AC#SUSP)        Suspense                                     
*                                                                               
         DC    AL1(INCOME)         INC   Income                                 
         DC    AL2(UL1C)                                                        
         DC    AL2(AC#CSG)         Costing                                      
*                                                                               
         DC    AL1(CASH)           CASH                                         
         DC    CL2'SC'                                                          
         DC    AL2(AC#RS540)       Cash                                         
*                                                                               
         DC    AL1(PNL)            P&L                                          
         DC    CL2'1C'                                                          
         DC    AL2(AC#RS544)       Financial                                    
*                                                                               
         DC    AL1(EOT)            End   of   table                             
         EJECT ,                                                                
***********************************************************************         
*     Report profiles location status table                           *         
***********************************************************************         
         SPACE 1                                                                
LOCSTATB DC    C'A'                Active                                       
         DCDD  AC#ACTV,L'LOCSTADD                                               
         DC    C'T'                Terminate                                    
         DCDD  AC#TRM,L'LOCSTADD                                                
         DC    C'L'                Leave of   absense                           
         DCDD  AC#LOA,L'LOCSTADD                                                
         DC    C'L'                Leave                                        
         DCDD  AC#LEAVE,L'LOCSTADD                                              
         DC    C'M'                Transfer                                     
         DCDD  AC#XFR,L'LOCSTADD                                                
         DC    C'O'                Other                                        
         DCDD  AC#OTHER,L'LOCSTADD                                              
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  Hard-coded text fields                                             *         
***********************************************************************         
         SPACE 1                                                                
HARDCTAB DS    0C                                                               
*                                                                               
         DC    AL1(EOT)            End   of   table                             
         EJECT ,                                                                
***********************************************************************         
*  Dictionary  List                                                   *         
***********************************************************************         
         SPACE 1                                                                
DICI     DS    0C                  Dictionary list lower     case               
         DCDDL AC#AORD,L'@AORD     A/D                                          
         DCDDL AC#ATTR,L'@ATTR     Attribute                                    
         DCDDL AC#AUTO,L'@AUTO     Auto                                         
         DCDDL AC#BLNK,L'@BLNK     Blank                                        
         DCDDL AC#CBOTT,L'@CBOTT   Bottom                                       
         DCDDL AC#CENTR,L'@CENTR   Center                                       
         DCDDL AC#COLS,L'@COLS     Columns                                      
         DCDDL AC#CONTD,L'@CONTD   Continued                                    
         DCDDL AC#CTRF1,L'@CTRF1   CF1                                          
         DCDDL AC#CTRF2,L'@CTRF2   CF2                                          
         DCDDL AC#CTRF3,L'@CTRF3   CF3                                          
         DCDDL AC#CTRF4,L'@CTRF4   CF4                                          
         DCDDL AC#CTRF5,L'@CTRF5   CF5                                          
         DCDDL AC#DESC,L'@DESC     Description                                  
         DCDDL AC#DTLSO,L'@DTLSO   Details    only                              
         DCDDL AC#FLT1,L'@FLT1     F1                                           
         DCDDL AC#FLT2,L'@FLT2     F2                                           
         DCDDL AC#FLT3,L'@FLT3     F3                                           
         DCDDL AC#FLT4,L'@FLT4     F4                                           
         DCDDL AC#FLT5,L'@FLT5     F5                                           
         DCDDL AC#FTLN,L'@FTLN     Footline                                     
         DCDDL AC#HEAD1,L'@HEAD1   Heading    1                                 
         DCDDL AC#HEAD2,L'@HEAD2   Heading    2                                 
         DCDDL AC#KEYWD,L'@KEYWD   Keyword                                      
         DCDDL AC#LEFT,L'@LEFT     Left                                         
         DCDDL AC#MIDS,L'@MIDS     Mid-line                                     
         DCDDL AC#NEGAM,L'@NEGAM   Negative   amounts                           
         DCDDL AC#NO,L'@NO         No                                           
         DCDDL AC#NONE,L'@NONE     None                                         
         DCDDL AC#NUM,L'@NUM       Num                                          
         DCDDL AC#ONLY,L'@ONLY     Only                                         
         DCDDL AC#OPT,L'@OPT       Option                                       
         DCDDL AC#OR,L'@OR         Or                                           
         DCDDL AC#PAGE,L'@PAGE     Page                                         
         DCDDL AC#PREFX,L'@PREFX   Prefix                                       
         DCDDL AC#PROPT,L'@PROPT   Print      options                           
         DCDDL AC#RANGE,L'@RANGE   Range                                        
         DCDDL AC#RIGHT,L'@RIGHT   Right                                        
         DCDDL AC#RNDCA,L'@RNDCA   Round      cash amounts                      
         DCDDL AC#RNDTO,L'@RNDTO   Rounding   options                           
         DCDDL AC#RNKNG,L'@RNKNG   Ranking                                      
         DCDDL AC#ROWS,L'@ROWS     Rows                                         
         DCDDL AC#SCNUM,L'@SCNUM   Section    number                            
         DCDDL AC#SPRT,L'@SPRT     Separate                                     
         DCDDL AC#TITLE,L'@TITLE   Title                                        
         DCDDL AC#TOTLS,L'@TOTLS   Totals                                       
         DCDDL AC#TYPE1,L'@TYPE1   Type                                         
         DCDDL AC#VALUE,L'@VALUE   Value                                        
         DCDDL AC#WIDE,L'@WIDE     Wide                                         
         DCDDL AC#YES,L'@YES       Yes                                          
         DC    AL1(0)                                                           
         SPACE 3                                                                
DICIU    DS    0C                  Dictionary list upper     case               
         DCDDL AC#RSCAN,L'$BOTH    Both                                         
         DCDDL AC#COLS,L'$COLS     Columns                                      
         DCDDL AC#DL,L'$DL         Down-load                                    
         DCDDL AC#HEADS,L'$HEADS   Headers                                      
         DCDDL AC#ROWS,L'$ROWS     Rows                                         
         DCDDL AC#RSADD,L'$RSADD   Address                                      
         DCDDL AC#RSCOD,L'$RSCOD   Code                                         
         DCDDL AC#RSDES,L'$RSDES   Description                                  
         DCDDL AC#RSLVL,L'$RSLVL   Level                                        
         DCDDL AC#RSNAM,L'$RSNAM   Name                                         
         DCDDL AC#RSRPE,L'$RSRPE   Period                                       
         DCDDL AC#SCRN,L'$SCRN     Screen                                       
         DC    AL1(0)                                                           
         TITLE 'Print out specs of format - Equates'                            
***********************************************************************         
*  Equates                                                            *         
***********************************************************************         
         SPACE 1                                                                
LATRB    EQU   4                   Length     of   attribute                    
LATRLONG EQU   8                   Length     of   attribute long               
LENFFULL EQU   62                  Maximum    on-line   full line               
*                                                                               
MAX#XPLN EQU   (XFOOT1-XP)/L'XP    Maximum    number    of   XP   lines         
*                                                                               
CPENNY   EQU   C'P'                                                             
CDOLLAR  EQU   C'D'                                                             
CTHOUSND EQU   C'T'                                                             
CMILLION EQU   C'M'                                                             
*                                                                               
ULSI     EQU   C'SI'               Unit  ledger    SI                           
ULSK     EQU   C'SK'               Unit  ledger    SK                           
UL1C     EQU   C'1C'               Unit  ledger    1C                           
*                                                                               
BRAKE    EQU   X'FF'               Brake point                                  
*                                                                               
*                                  ***** Field     number    equates            
F#TYPRPT EQU   01                  Type  of   report                            
F#UL     EQU   02                  Unit  and  ledger                            
F#SPCACT EQU   03                  Specific   account(s)                        
F#SINCAC EQU   04                  Specific   income    acts (INC only)         
F#SCSTAI EQU   05                  Specific   cost      acts (INC only)         
F#SCSTAC EQU   06                  Specific   cost      accounts                
F#BLGSRC EQU   07                  Billing    source                            
F#BLGGRP EQU   08                  Billing    group                             
F#BLGTYP EQU   09                  Billing    type                              
F#SCONTR EQU   10                  Specific   contra(s)                         
F#CSTNCL EQU   11                  Cost/Non-cli        (Contra    Acc)          
F#FLTSCM EQU   12                  Filter     scheme                            
*F#FILTRS EQU  13                  Filters                                      
F#CFLTSC EQU   14                  Contra     filter    scheme                  
*F#CFLTRS EQU  15                  Contra     filters                           
F#TRNTYP EQU   16                  Transaction     type                         
F#CLIENT EQU   17                  Client                                       
F#CPJ    EQU   18                  Cli/Prd/Job                                  
F#CPJEST EQU   19                  Client/product/job   est                     
F#CLIOFF EQU   20                  Client     office                            
F#OFFICE EQU   21                  Office                                       
F#OFFVAR EQU   22                  Variable   type of   office    text          
F#OFFGRP EQU   23                  Office     group                             
F#DEPTS  EQU   24                  Departments                                  
F#PERSON EQU   25                  Person                                       
F#DIREXP EQU   26                  Direct     expense   account(s)              
F#VENDOR EQU   27                  Vendor                                       
F#MEDIA  EQU   28                  Media                                        
F#MEDGRP EQU   29                  Media      group                             
F#TASK   EQU   30                  Task                                         
F#WC     EQU   31                  Work  code                                   
F#WCGRP  EQU   32                  Work  code group                             
F#STUDTY EQU   33                  Studio     type                              
F#USRFLD EQU   34                  User  field                                  
F#TYTIME EQU   35                  Type  of   time                              
F#TIMWCT EQU   36                  Time  work code types                        
F#OOPWCT EQU   37                  Out   of   pocket     WC   types             
F#EXCRSN EQU   38                  Exception  reasons                           
F#PRCLVL EQU   39                  Price      levels                            
F#METHOD EQU   40                  Method                                       
F#LOCSTA EQU   41                  Location   status                            
F#PAYCDE EQU   42                  Payroll    code                              
*                                                                               
F#INCREV EQU   51                  Include    reversals                         
F#INCACR EQU   52                  Include    accrual   reversals               
F#INCLCK EQU   53                  Include    locked    accounts                
F#SHWOUT EQU   54                  Only  show outanding items                   
F#CSHRCT EQU   55                  Cash  receipts                               
F#CSHDIS EQU   56                  Cash  disbursement                           
F#IDRAFT EQU   57                  Include    draft     transactions            
F#INCHLD EQU   58                  Include    held items                        
F#INCHLT EQU   59                  Include    held transactions                 
F#INCHLQ EQU   60                  Include    held/queried   items              
F#INCCLR EQU   61                  Include    cleared   items                   
F#INCAPR EQU   62                  Include    aproved   items                   
F#ONYCRP EQU   63                  Only  show credit    payees                  
F#ONYCD$ EQU   64                  Only  show cash discount  items              
F#INCOFF EQU   65                  Include    offset    items                   
F#IPVNDR EQU   66                  Include    PAY=NO    vendors                 
F#INCURG EQU   67                  Include    urgent    items                   
*                                  Include    accounts  analyzed  by            
F#INCNLD EQU   68                             department                        
*                                  Include    accounts  analyzed  by            
F#INCNLS EQU   69                             staff                             
*                                  Include    accounts  analyzed  by            
F#INCNLC EQU   70                             client                            
F#ESTSTA EQU   71                  Estimate   status                            
F#TYPBAL EQU   72                  Type  of   balance                           
F#INCUTR EQU   73                  Include    utilized  transactions            
F#INCCLS EQU   74                  Include    closed    jobs                    
F#INCEXP EQU   75                  Include    expense   jobs                    
F#ICBLWO EQU   76                  Include    CBIL W/Os and  transfers          
F#INCEST EQU   77                  Include    estimates                         
F#INCPOS EQU   78                  Include    purchase  orders                  
F#MRGPOS EQU   79                  Merge      purchase  orders                  
F#AGMTHD EQU   80                  Job        ageing    method                  
*                                  Include    99   with WC   group              
F#IN99WC EQU   81                             or   type filters                 
F#RCONCL EQU   82                  Report     bank reconciled     items         
F#TIMEWK EQU   83                  Time       sheet     worksheet               
F#SVTIME EQU   84                  Include    saved     time sheets             
F#OFFSEC EQU   85                  Office     security                          
F#AUTHST EQU   86                  Auth       status                            
F#INJLES EQU   87                  Include    jobs locked - Estimates           
F#INJLOR EQU   88                  Include    jobs locked - Orders              
F#INJLBI EQU   89                  Include    jobs locked - Billing             
F#INJLTI EQU   90                  Include    jobs locked - Time                
F#INJLAD EQU   91                  Include    jobs locked - Adjustments         
F#INJLEX EQU   92                  Include    jobs locked - 3rd Party           
F#INSUBJ EQU   93                  Include    sub-job                           
F#FILDAY EQU   94                  Filter     period date range by day          
*                                                                               
*                                  ***** Report    profile   routines           
RPR#TYPR EQU   1                   Type  of   report                            
RPR#SINA EQU   2                   Specific   income    acts (INC only)         
RPR#SCSA EQU   3                   Specific   cost      acts (INC only)         
RPR#OFFV EQU   4                   Variable   type of   office    text          
RPR#NAME EQU   5                   Name  field                                  
RPR#VALN EQU   6                   Value 1    or   2    field                   
RPR#FLTS EQU   7                   Filters                                      
RPR#CFLT EQU   8                   Contra     filters                           
RPR#TRAN EQU   9                   Transaction                                  
RPR#MTHD EQU   10                  Method                                       
RPR#LOCS EQU   11                  Location   status                            
RPR#PAYC EQU   12                  Payroll    code                              
*                                                                               
RPR#IREV EQU   31                  Include    reversals                         
RPR#IACR EQU   32                  Include    accrual   reversals               
RPR#ILCK EQU   33                  Include    locked    accounts                
RPR#SHWO EQU   34                  Show  outstanding    items                   
RPR#CSHR EQU   35                  Cash  receipts                               
RPR#CSHD EQU   RPR#CSHR            Cash  disbursement                           
RPR#DRFT EQU   36                  Include    draft     transactions            
RPR#IHLD EQU   37                  Include    held items                        
RPR#IHLQ EQU   38                  Include    held/queried   items              
RPR#ICLR EQU   39                  Include    cleared   items                   
RPR#IAPR EQU   40                  Include    approved  items                   
RPR#OCRP EQU   41                  Only  show credit    payees                  
RPR#OCD$ EQU   42                  Only  show cash discount  items              
RPR#IOFF EQU   43                  Include    offset    items                   
RPR#PVND EQU   44                  Include    PAY=NO    vendors                 
RPR#URGN EQU   45                  Include    urgent    items                   
*                                  Include    accounts  analyzed  by            
RPR#INLD EQU   46                             department                        
*                                  Include    accounts  analyzed  by            
RPR#INLS EQU   47                             staff                             
*                                  Include    accounts  analyzed  by            
RPR#INLC EQU   48                             client                            
RPR#ESTA EQU   49                  Estimate   status                            
RPR#TBAL EQU   50                  Type  of   balance                           
RPR#IUTR EQU   51                  Include    utilized  transactions            
RPR#ICLS EQU   52                  Include    closed    jobs                    
RPR#IEXP EQU   53                  Include    expense   jobs                    
RPR#ICWO EQU   54                  Include    CBIL W/Os and  transfPATC         
RPR#IEST EQU   55                  Include    estimates                         
RPR#IPOS EQU   56                  Include    purchase  orders                  
RPR#MPOS EQU   57                  Merge      purchase  orders                  
RPR#AGMD EQU   58                  Job        ageing    method                  
*                                  Include    99   with WC   group              
RPR#99WC EQU   59                             or   type filters                 
RPR#RCNC EQU   60                  Report     bank reconciled     items         
RPR#TMWK EQU   61                  Time       sheet     worksheet               
RPR#SVTM EQU   62                  Include    saved     time sheets             
RPR#OFSC EQU   63                  Office     security                          
RPR#AUTH EQU   64                  Auth       status                            
RPR#JLES EQU   65                  Include    jobs locked - Estimates           
RPR#JLOR EQU   66                  Include    jobs locked - Orders              
RPR#JLBI EQU   67                  Include    jobs locked - Billing             
RPR#JLTI EQU   68                  Include    jobs locked - Time                
RPR#JLAD EQU   69                  Include    jobs locked - Adjustments         
RPR#JLEX EQU   70                  Include    jobs locked - 3rd Part            
RPR#SUBJ EQU   71                  Include    sub-job                           
RPR#FPDD EQU   72                  Filter     period date range by day          
*                                                                               
RPR#MAX  EQU   72                  Max   routine   number                       
         TITLE 'Print out specs of format - DSECTS'                             
***********************************************************************         
*  Local working storage area                                         *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
ABASES   DS    0XL(BASESTBQ)       Base  addresses (see BASESTAB)               
*                                  Address    of:                               
APGPRO   DS    A                   .     General   profile                      
APRPRO   DS    A                   .     Report    profile                      
ASUBROS  DS    A                   .     Subroutines                            
*                                                                               
DMCB1    DS    6F                  Extra linkage   area                         
*                                                                               
RPT@FMTR DS    A                   Addr  of   format    record                  
RPT@PRFE DS    A                   Addr  of   profile   element                 
RPT@ATTR DS    A                   Addr  of   keyword   attributes              
*                                                                               
RPTTYPES DS    XL1                 Report     print     types found             
RPTHEAD  EQU   X'80'               .     headers                                
RPTROWS  EQU   X'40'               .     rows                                   
RPTCOLS  EQU   X'20'               .     columns                                
*                                                                               
RPTSWS   DS    XL1                 Report     switches                          
RPTSUCTR EQU   X'80'               .     upper     case translate               
RPTSCOL1 EQU   X'40'               .     processing     column    1             
RPTSCOL2 EQU   X'20'               .     processing     column    2             
RPTSNPED EQU   X'10'               .     started   a    new  page               
RPTNOC4E EQU   X'02'               .     no   X'C4'     element                 
RPTBXLN1 EQU   X'01'               .     1st  line of   a    box                
*                                                                               
RPTBXTYP DS    AL1                 Current    report    box  type               
RPTBNONE EQU   0                   .     none                                   
RPTBHEAD EQU   1                   .     headers                                
RPTBROWS EQU   2                   .     rows                                   
RPTBCOLS EQU   3                   .     columns                                
RPTBPROF EQU   4                   .     general   profile                      
RPTBPROR EQU   5                   .     general   profile   ranking            
RPTBPROD EQU   6                   .     general   profile   down-load          
RPTBRPRO EQU   7                   .     report    profile                      
RPTBXTMX EQU   7                   .     max  report    box  type               
*                                                                               
RPTSVBXT DS    AL1(L'RPTBXTYP)     Save  current   report    box  type          
*                                                                               
RPTSCNUM DS    XL1                 Boxes section   number    (1-9)              
*                                                                               
RPTLINES DS    XL1                 Num   of   lines     to   print              
RPTLINE# DS    XL1                 Number     of   lines     formatted          
*                                                                               
RPTOPT#  DS    XL1                 Option     number                            
*                                                                               
RPTTXTL  DS    XL1                 Length     of   text for  PGPROTXT           
*                                                                               
RPTRPG   DS    CL2                 Start new  page on   row  number             
*                                                                               
RPTSVRUL DS    CL2                 Save  report    unit/ledger                  
*                                                                               
COMMA    DS    CL1                 Comma or   C'#' in   German                  
*                                                                               
RPTPGWID DS    AL1                 BOXFONT    0    page width                   
*                                                                               
RPTPRF#  DS    XL2                 Report     profile   DD   number             
*                                                                               
RPTSCRNN DS    CL25                Screen     name                              
*                                                                               
SVXP1    DS    CL(L'XP)            Save  XP        line                         
SVXP2    DS    CL(L'XP)            Save  XPSECOND  line                         
SVXP3    DS    CL(L'XP)            Save  XPTHIRD   line                         
SVXP4    DS    CL(L'XP)            Save  XPFOURTH  line                         
         SPACE 1                                                                
*                                  ************************************         
*                                  *     Local     dictionary         *         
*                                  ************************************         
DICO     DS    0C                  Local dictionary     lower      case         
@AORD    DS    CL3                 A/D                                          
@ATTR    DS    CL10                Attribute                                    
@AUTO    DS    CL6                 Auto                                         
@BLNK    DS    CL8                 Blank                                        
@CBOTT   DS    CL6                 Bottom                                       
@CENTR   DS    CL7                 Center                                       
@COLS    DS    CL8                 Columns                                      
@CONTD   DS    CL10                Continued                                    
@CTRF1   DS    CL3                 CF1                                          
@CTRF2   DS    CL3                 CF2                                          
@CTRF3   DS    CL3                 CF3                                          
@CTRF4   DS    CL3                 CF4                                          
@CTRF5   DS    CL3                 CF5                                          
@DESC    DS    CL11                Description                                  
@DTLSO   DS    CL12                Details    only                              
@FLT1    DS    CL3                 F1                                           
@FLT2    DS    CL3                 F2                                           
@FLT3    DS    CL3                 F3                                           
@FLT4    DS    CL3                 F4                                           
@FLT5    DS    CL3                 F5                                           
@FTLN    DS    CL17                Footline                                     
@HEAD1   DS    CL9                 Heading    1                                 
@HEAD2   DS    CL10                Heading    2                                 
@KEYWD   DS    CL13                Keyword                                      
@LEFT    DS    CL8                 Left                                         
@MIDS    DS    CL8                 Mid-line                                     
@NEGAM   DS    CL18                Negative   amounts                           
@NO      DS    CL4                 No                                           
@NONE    DS    CL5                 None                                         
@NUM     DS    CL3                 Num                                          
@ONLY    DS    CL9                 Only                                         
@OPT     DS    CL6                 Option                                       
@OR      DS    CL5                 Or                                           
@PAGE    DS    CL6                 Page                                         
@PREFX   DS    CL6                 Prefix                                       
@PROPT   DS    CL17                Print      options                           
@RANGE   DS    CL8                 Range                                        
@RIGHT   DS    CL8                 Right                                        
@RNDCA   DS    CL20                Round      cash amounts                      
@RNDTO   DS    CL21                Rounding   options                           
@RNKNG   DS    CL12                Ranking                                      
@ROWS    DS    CL7                 Rows                                         
@SCNUM   DS    CL14                Section    number                            
@SPRT    DS    CL8                 Separate                                     
@TITLE   DS    CL5                 Title                                        
@TOTLS   DS    CL6                 Totals                                       
@TYPE1   DS    CL5                 Type                                         
@VALUE   DS    CL6                 Value                                        
@WIDE    DS    CL5                 Wide                                         
@YES     DS    CL3                 Yes                                          
*                                                                               
DICOU    DS    0C                  Local dictionary     upper      case         
$BOTH    DS    CL(LATRB)         4 Both                                         
$COLS    DS    CL8                 Columns                                      
$DL      DS    CL11                Down-load                                    
$HEADS   DS    CL10                Headers                                      
$ROWS    DS    CL7                 Rows                                         
$RSADD   DS    CL(LATRB)         4 Address        (ADR)                         
$RSCOD   DS    CL(LATRB)         4 Code                                         
$RSDES   DS    CL(LATRB)         4 Description    (DESC)                        
$RSLVL   DS    CL(LATRB)         4 Level          (LVL)                         
$RSNAM   DS    CL(LATRB)         4 Name                                         
$RSRPE   DS    CL(LATRB)         4 Period         (PER)                         
$SCRN    DS    CL10                Screen                                       
*                                                                               
LWSDX    EQU   *-LWSD                                                           
         EJECT ,                                                                
***********************************************************************         
*  Headings print line format                                         *         
***********************************************************************         
         SPACE 1                                                                
BXHEAD   DSECT                                                                  
BXHLEFT  DS    CL1                 Boxes type left                              
BXHTYPE  DS    CL9                 Title/Center/Left/Right/Footline             
         DS    CL1                                                              
BXHSEQ#  DS    CL1                 Sequence   number                            
BXHCOL1  DS    CL1                 Boxes type center                            
BXHKEYWD DS    CL72                Keyword                                      
BXHRGHT  DS    CL1                 Boxes type right                             
         ORG   BXHEAD                                                           
         DS    CL(STDPGWD)                                                      
         ORG   ,                                                                
BXHLNQ   EQU   *-BXHEAD                                                         
         EJECT ,                                                                
***********************************************************************         
*  Rows print line format                                             *         
***********************************************************************         
         SPACE 1                                                                
BXROWD   DSECT                                                                  
BXRLEFT  DS    CL1                 Boxes type left                              
BXRNUM   DS    CL3                 Number                                       
BXRCOL1  DS    CL1                 Boxes type center                            
BXRKEYWD DS    CL23                Keyword,   attribute                         
BXRCOL2  DS    CL1                 Boxes type center                            
BXRTYPE  DS    CL8                 Type                                         
BXRCOL3  DS    CL1                 Boxes type center                            
BXRTOTL  DS    CL8                 Total type                                   
BXRCOL4  DS    CL1                 Boxes type center                            
BXRPRFX  DS    CL14                Prefix                                       
BXRRGHT  DS    CL1                 Boxes type right                             
         ORG   BXROWD                                                           
         DS    CL(STDPGWD)                                                      
         ORG   ,                                                                
BXRLNQ   EQU   *-BXROWD                                                         
         EJECT ,                                                                
***********************************************************************         
*  Columns print line format                                          *         
*    Note: keep the width of the boxes to <= 163                      *         
***********************************************************************         
         SPACE 1                                                                
BXCOLD   DSECT                                                                  
BXCLEFT  DS    CL1                 Boxes type left                              
BXCNUM   DS    CL3                 Number                                       
BXCCOL1  DS    CL1                 Boxes type center                            
BXCKEYWD DS    CL(L'BXRKEYWD)   23 Keyword,   attribute                         
BXCCOL2  DS    CL1                 Boxes type center                            
BXCRNGE  DS    CL10                Range                                        
BXCCOL3  DS    CL1                 Boxes type center                            
BXCWDTH  DS    CL5                 Width                                        
BXCCOL4  DS    CL1                 Boxes type center                            
BXCTOTL  DS    CL12                Total                                        
BXCCOL5  DS    CL1                 Boxes type center                            
BXCHEAD1 DS    CL12                Head  1                                      
BXCCOL6  DS    CL1                 Boxes type center                            
BXCHEAD2 DS    CL12                Head  2                                      
BXCCOL7  DS    CL1                 Boxes type center                            
BXCCFLT  DS    CL60                Options and filters                          
BXCRGHT  DS    CL1                 Boxes type right                             
         ORG   BXCOLD                                                           
         DS    CL(STDPGWD)                                                      
         ORG   ,                                                                
BXCLNQ   EQU   *-BXCOLD                                                         
         EJECT ,                                                                
***********************************************************************         
*  General profile print line format                                  *         
***********************************************************************         
         SPACE 1                                                                
BXGPROFD DSECT                                                                  
BXGLEFT  DS    CL1                 Boxes type left                              
BXGNUM1  DS    CL3                 Number                                       
BXGCOL1  DS    CL1                 Boxes type center                            
BXGOPT1  DS    CL36                Option                                       
BXGCOL2  DS    CL1                 Boxes type center                            
BXGVAL1  DS    CL6                 Value                                        
BXGCOL3  DS    CL1                 Boxes type center                            
BXGNUM2  DS    CL(L'BXGNUM1)     3 Number                                       
BXGCOL4  DS    CL1                 Boxes type center                            
BXGOPT2  DS    CL(L'BXGOPT1)    36 Option                                       
BXGCOL5  DS    CL1                 Boxes type center                            
BXGVAL2  DS    CL(L'BXGVAL1)     6 Value                                        
BXGRGHT  DS    CL1                 Boxes type right                             
         ORG   BXGPROFD                                                         
         DS    CL(STDPGWD)                                                      
         ORG   ,                                                                
BXGLNQ   EQU   *-BXGPROFD                                                       
         EJECT ,                                                                
***********************************************************************         
*  General profile print line for ranking format                      *         
***********************************************************************         
         SPACE 1                                                                
BXGRPROD DSECT                                                                  
BXGRLEFT DS    CL1                 Boxes type left                              
BXGROPT1 DS    CL15                Option                                       
BXGRCOL1 DS    CL1                 Boxes type center                            
BXGRVAL1 DS    CL5                 Value                                        
BXGRCOL2 DS    CL1                 Boxes type center                            
BXGROPT2 DS    CL10                Option                                       
BXGRCOL3 DS    CL1                 Boxes type center                            
BXGRVAL2 DS    CL3                 Number                                       
BXGRCOL4 DS    CL1                 Boxes type center                            
BXGROPT3 DS    CL20                Option                                       
BXGRCOL5 DS    CL1                 Boxes type center                            
BXGRVAL3 DS    CL3                 A/D                                          
BXGRRGHT DS    CL1                 Boxes type right                             
         ORG   BXGRPROD                                                         
         DS    CL(STDPGWD)                                                      
         ORG   ,                                                                
BXGRLNQ  EQU   *-BXGRPROD                                                       
         EJECT ,                                                                
***********************************************************************         
*  General profile print line for down-load options                   *         
***********************************************************************         
         SPACE 1                                                                
BXGDPROD DSECT                                                                  
BXGDLEFT DS    CL1                 Boxes type left                              
BXGDOPT1 DS    CL36                Option                                       
BXGDCOL1 DS    CL1                 Boxes type center                            
BXGDVAL1 DS    CL10                Value                                        
BXGDCOL2 DS    CL1                 Boxes type center                            
BXGDOPT2 DS    CL29                Option                                       
BXGDCOL3 DS    CL1                 Boxes type center                            
BXGDVAL2 DS    CL8                 Value                                        
BXGDRGHT DS    CL1                 Boxes type right                             
         ORG   BXGDPROD                                                         
         DS    CL(STDPGWD)                                                      
         ORG   ,                                                                
BXGDLNQ  EQU   *-BXGDPROD                                                       
         EJECT ,                                                                
***********************************************************************         
*  Report profile print line                                          *         
***********************************************************************         
         SPACE 1                                                                
BXRPROFD DSECT                                                                  
BXRPLEFT DS    CL1                 Boxes type left                              
BXRPNUM  DS    CL3                 Number                                       
BXRPCOL1 DS    CL1                 Boxes type center                            
BXRPOPT  DS    CL32                Option                                       
BXRPCOL2 DS    CL1                 Boxes type center                            
BXRPVAL  DS    CL40                Value                                        
         ORG   BXRPVAL                                                          
BXRPVAL1 DS    CL9                 Value                                        
         ORG   ,                                                                
BXRPRGHT DS    CL1                 Boxes type right                             
         ORG   BXRPROFD                                                         
         DS    CL(STDPGWD)                                                      
         ORG   ,                                                                
BXRPLNQ  EQU   *-BXRPROFD                                                       
         EJECT ,                                                                
***********************************************************************         
*  Report profile print line - filter scheme print area               *         
***********************************************************************         
         SPACE 1                                                                
BXRPFLTD DSECT                                                                  
BXRPF1TX DS    CL3                 F1    text                                   
         DS    CL1                 =                                            
BXRPF1VL DS    CL2                 F1    value                                  
         DS    CL2                                                              
BXRPF2TX DS    CL3                 F2    text                                   
         DS    CL1                 =                                            
BXRPF2VL DS    CL2                 F2    value                                  
         DS    CL2                                                              
BXRPF3TX DS    CL3                 F3    text                                   
         DS    CL1                 =                                            
BXRPF3VL DS    CL2                 F3    value                                  
         DS    CL2                                                              
BXRPF4TX DS    CL3                 F4    text                                   
         DS    CL1                 =                                            
BXRPF4VL DS    CL2                 F4    value                                  
         DS    CL2                                                              
BXRPF5TX DS    CL3                 F5    text                                   
         DS    CL1                 =                                            
BXRPF5VL DS    CL2                 F5    value                                  
BXRPFLNQ EQU   *-BXRPFLTD                                                       
         EJECT ,                                                                
***********************************************************************         
*  Columns filter table DSECT                                         *         
***********************************************************************         
         SPACE 1                                                                
FLTTABD  DSECT                     Map   of   column    filter    table         
*                                                                               
FLTTNUM  DS    XL1                 Filter     number                            
FLTTJOB  DS    XL1                 For   job  IDs  (or  zero)                   
*                                                                               
FLTTSWS  DS    XL1                 Filter     table     switches                
FLTTSTA  EQU   X'80'               .     Status                                 
FLTTPCTD EQU   X'40'               .     PCT  denominator                       
*                                                                               
FLTTTXT  DS    CL14                Filter     text                              
*                                                                               
FLTTABLQ EQU   *-FLTTABD           Length     of   entry                        
         EJECT ,                                                                
***********************************************************************         
*  DSECT to process general profile - print option                    *         
***********************************************************************         
         SPACE 1                                                                
*                                  Map   general   profile   -                  
GPPRTABD DSECT                           print     option    table              
*                                                                               
*                                  General    profile   print    option         
GPPRSWS  DS    XL1                       table     switches                     
GPPRMSG  EQU   X'80'               .     message   number                       
*                                                                               
GPPRNUM  DS    AL2                 Dictionary or   message   number             
*                                                                               
*                                  Address    of   routine   to                 
GPPRRTN  DS    AL4                       insert    the  value                   
GPPRTABQ EQU   *-GPPRTABD          Length     of   entry                        
         EJECT ,                                                                
***********************************************************************         
*  DSECT to process general profile - down-load option                *         
***********************************************************************         
         SPACE 1                                                                
*                                  Map   general   profile   -                  
GPDWTABD DSECT                           down-load option    table              
*                                                                               
*                                  General    profile   down-load               
GPDWSWS  DS    XL1                       option    table     switches           
GPDWMSG  EQU   X'80'               .     message   number                       
GPDWSKPL EQU   X'40'               .     skip      line                         
GPDWCOL1 EQU   X'08'               .     column    1                            
GPDWCOL2 EQU   X'04'               .     column    2                            
*                                                                               
GPDWNUM  DS    AL2                 Dictionary or   message   number             
*                                                                               
*                                  Address    of   routine   to                 
GPDWRTN  DS    AL4                       insert    the  value                   
*                                                                               
GPDWTABQ EQU   *-GPDWTABD          Length     of   entry                        
         EJECT ,                                                                
***********************************************************************         
*  DSECT to process general profile - down-load date format option    *         
***********************************************************************         
         SPACE 1                                                                
DTFORMD  DSECT                     Date  format    table                        
*                                                                               
DTFLANG  DS    AL1                 Language                                     
DTFCODE  DS    AL1                 DATCON     code                              
DTFDTFMT DS    CL10                Date  format                                 
*                                                                               
DTFLNQ   EQU   *-DTFORMD           Length     of   entry                        
         EJECT ,                                                                
***********************************************************************         
*  DSECT to process report profile table entries                      *         
***********************************************************************         
         SPACE 1                                                                
*                                  Map   report    profile   -                  
RPRFTABD DSECT                           table     entries                      
*                                                                               
RPRFFLD# DS    AL1                 Field number                                 
*                                                                               
RPRFSWS1 DS    XL1                 Field switches  1                            
RPRFSKIP EQU   X'80'               .     skip one  line before    field         
*                                  .     restart   numbering before             
RPRFRNUM EQU   X'40'                          field     (new box)               
RPRFNNUM EQU   X'04'               .     do   not  number    the  field         
*RPRFRSTO EQU   X'01'              .     stereo    field     only               
*                                                                               
RPRFTABQ EQU   *-RPRFTABD          Length     of   entry                        
         EJECT ,                                                                
***********************************************************************         
*  DSECT to process report profile field numbers                      *         
***********************************************************************         
         SPACE 1                                                                
*                                  Map   report    profile   -                  
RPR#TABD DSECT                           field     numbers   entries            
RPR#FLD# DS    AL(L'RPRFFLD#)      Field number                                 
*                                  Report     filter    type                    
RPR#FTYP DS    XL1                       (see RFLTYPE   in   ACGENFILE)         
*                                                                               
*                                  Dictionary or   message   number             
*                                        or   LWSD offset or                    
RPR#NUM  DS    AL2                       HARDCTAB  offset                       
*                                  Message    length                            
RPR#NUML DS    AL1                      (zero =    max  available)              
*                                                                               
RPR#SWS  DS    XL1                 Switches                                     
RPR#MSG  EQU   X'80'               .     message   number                       
RPR#LWS  EQU   X'40'               .     LWSD offset                            
RPR#HARD EQU   X'20'               .     HARDCTAB  offset                       
*RPR#TXTO EQU  X'08'               .     text only field                        
*                                                                               
RPR#RTN# DS    AL(L'RPRRTN#)       Processing routine   number                  
*                                                                               
RPR#TABQ EQU   *-RPR#TABD          Length     of   entry                        
         EJECT ,                                                                
***********************************************************************         
*  DSECT to process report profile routine numbers                    *         
***********************************************************************         
         SPACE 1                                                                
*                                  Map   report    profile   -                  
RPRRTNTD DSECT                           routine   number    entries            
*                                                                               
RPRRTN#  DS    AL1                 Routine    number                            
*                                                                               
RPR@RTN  DS    AL3                 Address    of   routine                      
*                                                                               
RPRRTNTQ EQU   *-RPRRTNTD          Length     of   entry                        
         EJECT ,                                                                
***********************************************************************         
*  Report profile - Type of report table DSECT                        *         
***********************************************************************         
         SPACE 1                                                                
RPRTTYRD DSECT                                                                  
RPRTTYQP DS    AL1                 JCL   character in   QPROG                   
RPRTTYUL DS    CL2                 Ledger                                       
RPRTTYD# DS    AL2                 Dictionary entry                             
*                                                                               
RPRTTYRQ EQU   *-RPRTTYRD          Length     of   entry                        
         EJECT ,                                                                
***********************************************************************         
*  Report profile - Location status table DSECT                       *         
***********************************************************************         
         SPACE 1                                                                
LOCSTATD DS    0C                                                               
*                                  Location   status    code in                 
LOCSTACD DS    CL1                       RFLLOCS   element                      
LOCSTADD DS    CL8                 Dictionary entry                             
*                                                                               
LOCSTALQ EQU   *-LOCSTATD          Length     of   entry                        
         EJECT ,                                                                
         SPACE 1                                                                
*ACREPRLWRK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPRLWRK                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'112ACREPRL07 08/12/20'                                      
         END                                                                    
