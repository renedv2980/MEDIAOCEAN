*          DATA SET PRWRI16    AT LEVEL 009 AS OF 05/28/20                      
*PHASE T40516B,*                                                                
*********************************************************************           
*                                                                   *           
*        PRWRI16 (T40516) - SC JOHNSON PRINT ESTIMATE INTERFACE     *           
*                                                                   *           
*-------------------------------------------------------------------*           
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-32038  05/28/20 NEW FISICAL CALENDAR FOR SCJ 2020-2021    *         
* AKAT SPEC-22216  11/05/18 RELINK FOR NEW DISPLACEMENT OF PBQMEDLS   *         
* AKAT SPEC-26418  08/01/18 NEW FISICAL CALENDAR FOR SCJ 2018-2020    *         
* AKAT ITMF-8802   07/28/16 NEW FISICAL CALENDAR FOR SCJ 2016-2018    *         
***********************************************************************         
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 24JUL15    BPLA -- FISCAL CALENDAR UPDATE                                     
* 25JUL14    BPLA -- FISCAL CALENDAR UPDATE                                     
* 15MAR12 01 AKAT -- BIG BANG                                       *           
* 02APR12 02 AKAT -- SCJD SUPPORT                                   *           
*                                                                   *           
*********************************************************************           
         TITLE 'T40516 - SC JOHNSON PRINT ESTIMATE INTERFACE'                   
T40516   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T40516,RR=RE                                               
         LR    R5,RC                                                            
         USING WORKD,R5                                                         
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPINPUT      INPUT                                        
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1  ,                                                                
*                                                                               
* INITIALIZATION                                                                
*                                                                               
INIT     MVI   PRINTH,C'N'         DIDN'T PRINT HEADER YET                      
         OI    PBQFLOPT,PBQFLOUT   SEND OUTPUT TO A FILE                        
         OI    PBQFLOPT,PBQFLSCJ   SC JOHNSON ESTIMATE FILE                     
         OI    PBQREAD,PBQRDBUY    READ BUYS                                    
         OI    PBQREAD,PBQRDOAN    READ OAN RECORDS                             
         OI    PBQPOPT,PBQPOYYM+PBQPOCTY                                        
         MVI   WIDEOPT,C'Y'        SET FOR WIDE REPORT                          
         MVI   DIGITAL,C'N'        DEFAULT TO NO DIGITAL                        
         CLC   =C'IS',PBQMEDLS+1   DIGITAL FILE REQUEST?                        
         BE    *+14                YES                                          
         CLC   =C'SI',PBQMEDLS+1   DIGITAL FILE REQUEST?                        
         BNE   *+8                 NO                                           
         MVI   DIGITAL,C'Y'        DIGITAL FILE                                 
*                                                                               
         L     RF,AFLTTAB          UPDATE FOR UCOM=E1                           
         AHI   RF,FLTAREAL         POINT TO UCOM=E1                             
         MVC   0(7,RF),=X'0110001C021C21'                                       
         CLC   =C'SCJED',CONREC    SCJED FILE?                                  
         BNE   XIT                 NO                                           
         MVI   DIGITAL,C'Y'        YES - ALWAYS A DIGITAL FILE                  
         MVC   0(7,RF),=X'0110000C020C03'   UDEF=E1                             
*                                                                               
         B     XIT                 EXIT                                         
*                                                                               
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
INPUT    L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
         B     XIT                                                              
*                                                                               
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL          R4 = GLOBAL                                  
         USING GLOBALD,R4          GLOBALD DSECT                                
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
*                                                                               
DRHOOKX  B     XIT                 EXIT                                         
*                                                                               
* DRIVER INIT                                                                   
*                                                                               
DRVINIT  OI    GLINDS,GLPALDET     PRINT ALL DETAILS                            
         B     XIT                 EXIT                                         
*                                                                               
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,ROUTLIST         SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),X'FF'         END OF ROUTINE LIST?                         
         BE    XIT                 YES                                          
         CLC   0(8,R1),GLLABEL     MATCH ON LABEL?                              
         BE    *+12                YES                                          
         LA    R1,12(R1)           NO - BUMP TO NEXT ENTRY                      
         B     RESOLVE2            LOOP BACK AND CHECK NEXT ENTRY               
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                 EXIT                                         
*                                                                               
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                  RETURN TO CALLER                             
*                                                                               
* DRIVER INPUT/OUTPUT ROUTINES                                                  
*                                                                               
IHEADER  MVI   0(R2),1             GENERATE DATA                                
         B     XIT                 EXIT                                         
*                                                                               
OHEADER  CLI   PRINTH,C'Y'         PRINTED HEADER ALREADY?                      
         BE    XIT                 YES - ONLY PRINT ONCE                        
         MVC   P+1(6),=C'HEADER'   HEADER                                       
         MVC   P+11(15),=C'PRINT ESTIMATES'                                     
         CLI   DIGITAL,C'Y'        DIGITAL?                                     
         BNE   *+10                NO                                           
         MVC   P+11(17),=C'DIGITAL ESTIMATES'                                   
         GOTO1 DATCON,DMCB,(3,PBBTODAY),(20,P+31)                               
*                                                                               
         LA    R6,SCFIST           SC'S FISCAL CALENDAR TABLE                   
OHEAD10  CLC   PBBTODAY,0(R6)      CHK VS. PERIOD START                         
         BL    DATEERR                                                          
         CLC   PBBTODAY,3(R6)      CHK VS. PERIOD END                           
         BNH   OHEAD20             FOUND MY PERIOD                              
         LA    R6,6(R6)            NEXT PERIOD                                  
         CLI   0(R6),X'FF'         CHK END OF TABLE                             
         BNE   OHEAD10                                                          
*                                                                               
DATEERR  DC    H'0'                UPDATE FISCAL TABLE CANT FIND PERIOD         
*                                                                               
OHEAD20  GOTO1 DATCON,DMCB,(3,0(R6)),(20,P+39)                                  
         GOTO1 DATCON,DMCB,(3,3(R6)),(20,P+47)                                  
*                                                                               
         ICM   R1,15,AOUTPDCB      HAVE OUTPUT DCB?                             
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
         PUT   (R1),P+1            OUTPUT TO FILE                               
***      GOTO1 VPRINT,DMCB,P,=C'BL01'                                           
         MVC   P,SPACES            CLEAR PRINT LINE                             
         MVI   PRINTH,C'Y'         PRINTED HEADER                               
         B     XIT                 EXIT                                         
*                                                                               
OBIL     GOTO1 DATCON,DMCB,(3,0(R2)),(22,DUB)                                   
         MVC   0(3,R3),DUB         MON                                          
         MVC   3(4,R3),DUB+4       YEAR                                         
         B     XIT                 EXIT                                         
*                                                                               
PRINT    L     RF,GLADTAB          START SEARCH FROM BEGINING                   
         USING DRFLD,RF            OF DRIVE TABLE                               
         XR    RE,RE               CLEAR RE                                     
*                                                                               
PRINT10  CLI   0(RF),0             END OF TABLE?                                
         BE    XIT                 YES - NO DATA                                
         CLI   0(RF),X'48'         TOTAL ENTRY?                                 
         BE    PRINT20             YES                                          
         ICM   RE,1,1(RF)          ZERO ENTRY LENGTH?                           
         BNZ   *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
         AR    RF,RE               BUMP TO NEXT DRIVER ENTRY                    
         B     PRINT10             CHECK NEXT ENTRY                             
*                                                                               
PRINT20  ICM   R3,15,DRFLAPOS      START OF PRINT POSITION                      
         BZ    XIT                 IF ZERO THEN EXIT                            
         USING DETAILD,P+1                                                      
         USING SCJPRNTD,R3         SCJ PRINT LINE DSECT                         
*                                                                               
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE?                        
         BNZ   PRINT25             YES                                          
*                                                                               
         MVC   DETDET(6),=C'DETAIL'                                             
*                                                                               
         CLI   SCJMEDIA,X'40'      HAVE MEDIA?                                  
         BNH   *+10                NO                                           
         MVC   SAVEMED,SCJMEDIA    YES - SAVE THE MEDIA                         
         MVC   SCJMEDIA,SAVEMED    RESTORE THE MEDIA                            
*                                                                               
         CLC   SCJUCOM,SPACES      HAVE UCOM?                                   
         BNH   *+10                NO                                           
         MVC   SAVEUCOM,SCJUCOM    YES - SAVE THE UCOM                          
         MVC   SCJUCOM,SAVEUCOM    RESTORE UCOM                                 
*                                                                               
         CLC   SCJPROD,SPACES      HAVE PRD?                                    
         BNH   *+10                NO                                           
         MVC   SAVEPRD,SCJPROD     YES - SAVE THE PRD                           
         MVC   SCJPROD,SAVEPRD     RESTORE THE PRD                              
*                                                                               
         CLC   SCJESTMT,SPACES     HAVE EST?                                    
         BNH   *+10                NO                                           
         MVC   SAVEEST,SCJESTMT    YES - SAVE THE EST                           
         MVC   SCJESTMT,SAVEEST    RESTORE THE EST                              
*                                                                               
         CLC   SCJESTDT,SPACES     HAVE EST START DATE?                         
         BNH   *+10                NO                                           
         MVC   SAVEESD,SCJESTDT    YES - SAVE THE EST START DATE                
         MVC   SCJESTDT(17),SAVEESD RESTORE THE EST START DATE                  
*                                                                               
         MVC   DETUCOM,SCJUCOM     UCOM                                         
         OC    DETUCOM,SPACES      SPACE PAD IN CASE NULLS                      
         MVC   DETPRD,SCJPROD      PRODUCT                                      
         MVC   DETEST,SCJESTMT     ESTIMATE                                     
         MVC   DETEDATE,SCJESTDT   ESTIMATE START DATE                          
         MVC   DETEDPRD,SCJPROD    PRODUCT                                      
         MVC   DETEDMOS,SCJBLMON   MOS                                          
         MVC   DETEDMED,SCJMEDIA   MEDIA                                        
         MVI   DETEDSYS,C'N'       SYSTEM (FOR DIGITAL)                         
         MVC   DETETOT,SCJONCD     ORDERED NET + TAX - CASH DISCOUNT            
         MVC   DETGROSS,SCJOGRS    ORDERED GROSS                                
         MVC   DETNET,SCJONET      ORDERED NET                                  
         MVC   DETTAX,SCJOTAX      ORDERED TAX                                  
         CLI   DIGITAL,C'Y'        DIGITAL?                                     
         BE    *+14                YES                                          
         MVC   DETCASHD,SCJOCD     CASH DICOUNT                                 
         MVI   DETEDSYS,C'A'       SYSTEM (NON-DIGITAL)                         
*                                                                               
         LA    RF,DETETOT+L'DETETOT-1                                           
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         LA    RF,DETGROSS+L'DETGROSS-1                                         
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         LA    RF,DETNET+L'DETNET-1                                             
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         LA    RF,DETTAX+L'DETTAX-1                                             
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         CLI   DIGITAL,C'Y'        DIGITAL?                                     
         BE    *+12                YES                                          
         LA    RF,DETCASHD+L'DETCASHD-1                                         
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         LH    R1,LINECNT          LINE COUNT                                   
         AHI   R1,1                BUMP LINE COUNT                              
         STH   R1,LINECNT          NEW LINE COUNT                               
         B     PRINT30             END OF DETAIL LINE                           
*                                                                               
PRINT25  MVC   P+1(6),=C'FOOTER'   FOOTER                                       
         LH    R1,LINECNT          LINE COUNT                                   
         AHI   R1,2                PLUS HEADER AND FOOTER                       
         EDIT  (R1),(10,P+11),FILL=0                                            
*                                                                               
         MVC   P+21(16),SCJOGRS    TOTAL GROSS                                  
         LA    RF,P+36             LAST POSITION OF TOTAL GROSS                 
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         MVC   P+37(16),SCJONET    TOTAL NET                                    
         LA    RF,P+52             LAST POSITION OF TOTAL NET                   
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         MVC   P+53(16),SCJOTAX    TOTAL TAX                                    
         LA    RF,P+68             LAST POSITION OF TOTAL TAX                   
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
         CLI   DIGITAL,C'Y'        DIGITAL?                                     
         BE    PRINT30             YES                                          
         MVC   P+69(16),SCJOCD     TOTAL CASH DISCOUNT                          
         LA    RF,P+84             LAST POSITION OF CASH DISCOUNT               
         BAS   RE,FMTNUM           FORMAT NUMBER                                
*                                                                               
PRINT30  ICM   R1,15,AOUTPDCB      HAVE OUTPUT DCB?                             
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
         PUT   (R1),P+1            OUTPUT TO FILE                               
****     GOTO1 VPRINT,DMCB,P,=C'BL01'                                           
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE?                        
         BZ    PRINT40             NO                                           
         ICM   R1,15,AOUTPDCB      OUTPUT DCB                                   
         CLOSE ((R1),)             CLOSE TAPE                                   
*                                                                               
PRINT40  MVC   P,SPACES            CLEAR PRINT LINE                             
         MVC   P2,SPACES           CLEAR PRINT LINE                             
         B     XIT                 EXIT                                         
         DROP  R3                                                               
*                                                                               
* FORMAT A 16 CHAR NUMBER BY LEADING WITH +/- SIGN AND ZERO PADDING             
*                                                                               
FMTNUM   LA    R6,16               FIELD LENGTH = 16                            
*                                                                               
FNUM00   CLI   0(RF),X'40'         SPACE?                                       
         BE    FNUM10              YES - NUMBER ENDS HERE                       
         BCTR  RF,0                DECREMENT POINTER                            
         BCT   R6,FNUM00           LOOP BACK & CHECK PREVIOUS POSITION          
         BR    RE                  SOMEHOW HAVE A 14 DIGIT NUMBER!              
*                                                                               
FNUM10   MVI   BYTE,C'+'           DEFAULT TO POSITIVE                          
         CLI   1(RF),C'-'          NEGATIVE NUMBER                              
         BNE   *+8                 NO                                           
         MVI   BYTE,C'-'           YES - MAKE IT NEGATIVE                       
*                                                                               
FNUM20   BCTR  R6,0                -1                                           
         SR    RF,R6               MOVE SIGN HERE                               
         MVC   0(1,RF),BYTE        +/- SIGN                                     
         LA    RF,1(RF)            PAD WITH 0'S STARTING HERE                   
         BCTR  R6,0                -1 FOR EX                                    
         EX    R6,*+8              ** EXECUTE **                                
         B     *+10                BRANCH FOR IDF                               
         MVC   0(0,RF),ZEROS       LEADING ZEROS BEFORE THE NUMBER              
         BCTR  RF,0                FIRST POSITION                               
         BR    RE                  RETURN TO CALLER                             
*                                                                               
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                             
*                                                                               
         LTORG                                                                  
ZEROS    DC    CL16'0000000000000000'                                           
RPFIRST  EQU   4                   REQUEST FIRST                                
*                                                                               
SCFIST   DC   XL3'6F0702',XL3'6F0805' JUL02/11 - AUG05/11                       
         DC   XL3'6F0806',XL3'6F0902' AUG06/11 - SEP02/11                       
         DC   XL3'6F0903',XL3'6F091E' SEP03/11 - SEP30/11                       
         DC   XL3'6F0A01',XL3'6F0B04' OCT01/11 - NOV04/11                       
         DC   XL3'6F0B05',XL3'6F0C02' NOV05/11 - DEC02/11                       
         DC   XL3'6F0C03',XL3'6F0C1E' DEC03/11 - DEC30/11                       
         DC   XL3'6F0C1F',XL3'700203' DEC31/11 - FEB03/12                       
         DC   XL3'700204',XL3'700302' FEB04/12 - MAR02/12                       
         DC   XL3'700303',XL3'70031E' MAR03/12 - MAR30/12                       
         DC   XL3'70031F',XL3'700504' MAR31/12 - MAY04/12                       
         DC   XL3'700505',XL3'700601' MAY05/12 - JUN01/12                       
         DC   XL3'700602',XL3'70061D' JUN02/12 - JUN29/12                       
*                                                                               
*        2012-2013                                                              
*                                                                               
         DC   XL3'70061E',XL3'700803' JUN30/12 - AUG03/12                       
         DC   XL3'700804',XL3'70081F' AUG04/12 - AUG31/12                       
         DC   XL3'700901',XL3'70091C' SEP01/12 - SEP28/12                       
         DC   XL3'70091D',XL3'700B02' SEP29/12 - NOV02/12                       
         DC   XL3'700B03',XL3'700B1E' NOV03/12 - NOV30/12                       
         DC   XL3'700C01',XL3'700C1C' DEC01/12 - DEC28/12                       
         DC   XL3'700C1D',XL3'710201' DEC29/12 - FEB01/13                       
         DC   XL3'710202',XL3'710301' FEB02/13 - MAR01/13                       
         DC   XL3'710302',XL3'71031D' MAR02/13 - MAR29/13                       
         DC   XL3'71031E',XL3'710503' MAR30/13 - MAY03/13                       
         DC   XL3'710504',XL3'71051F' MAY04/13 - MAY31/13                       
         DC   XL3'710601',XL3'71061C' JUN01/13 - JUN28/13                       
*                                                                               
*        2013-2014                                                              
*                                                                               
         DC   XL3'71061D',XL3'710802' JUN29/13 - AUG02/13                       
         DC   XL3'710803',XL3'71081E' AUG03/13 - AUG30/13                       
         DC   XL3'71081F',XL3'71091B' AUG31/13 - SEP27/13                       
         DC   XL3'71091C',XL3'710B01' SEP28/13 - NOV01/13                       
         DC   XL3'710B02',XL3'710B1D' NOV02/13 - NOV29/13                       
         DC   XL3'710B1E',XL3'710C1B' NOV30/13 - DEC27/13                       
         DC   XL3'710C1C',XL3'72011F' DEC28/13 - JAN31/14                       
         DC   XL3'720201',XL3'72021C' FEB01/14 - FEB28/14                       
         DC   XL3'720301',XL3'72031C' MAR01/14 - MAR28/14                       
         DC   XL3'72031D',XL3'720502' MAR29/14 - MAY02/14                       
         DC   XL3'720503',XL3'72051E' MAY03/14 - MAY30/14                       
         DC   XL3'72051F',XL3'72061B' MAY31/14 - JUN27/14                       
*                                                                               
*        2014-2015                                                              
*                                                                               
         DC   XL3'72061C',XL3'720801' JUN28/14 - AUG01/14                       
         DC   XL3'720802',XL3'72081D' AUG02/14 - AUG29/14                       
         DC   XL3'72081E',XL3'72091A' AUG30/14 - SEP26/14                       
         DC   XL3'72091B',XL3'720A1F' SEP27/14 - OCT31/14                       
         DC   XL3'720B01',XL3'720B1C' NOV01/14 - NOV28/14                       
         DC   XL3'720B1D',XL3'730102' NOV29/14 - JAN02/15                       
         DC   XL3'730103',XL3'730206' JAN03/15 - FEB06/15                       
         DC   XL3'730207',XL3'730306' FEB07/15 - MAR06/15                       
         DC   XL3'730307',XL3'730403' MAR07/15 - APR03/15                       
         DC   XL3'730404',XL3'730508' APR04/15 - MAY08/15                       
         DC   XL3'730509',XL3'730605' MAY09/15 - JUN05/15                       
         DC   XL3'730606',XL3'730703' JUN06/15 - JUL03/15                       
*                                                                               
*        2015-2016                                                              
*                                                                               
         DC   XL3'730704',XL3'730807' JUL04/15 - AUG07/15                       
         DC   XL3'730808',XL3'730904' AUG08/15 - SEP04/15                       
         DC   XL3'730905',XL3'730A02' SEP05/15 - OCT02/15                       
         DC   XL3'730A03',XL3'730B06' OCT03/15 - NOV06/15                       
         DC   XL3'730B07',XL3'730C04' NOV07/15 - DEC04/15                       
         DC   XL3'730C05',XL3'740101' DEC05/15 - JAN01/16                       
         DC   XL3'740102',XL3'740205' JAN02/16 - FEB05/16                       
         DC   XL3'740206',XL3'740304' FEB06/16 - MAR04/16                       
         DC   XL3'740305',XL3'740401' MAR05/16 - APR01/16                       
         DC   XL3'740402',XL3'740506' APR02/16 - MAY06/16                       
         DC   XL3'740507',XL3'740603' MAY07/16 - JUN03/16                       
         DC   XL3'740604',XL3'740701' JUN04/16 - JUL01/16                       
*                                                                               
*        2016-2017                                                              
*                                                                               
         DC   XL3'740702',XL3'740805' JUL02/16 - AUG05/16                       
         DC   XL3'740806',XL3'740902' AUG06/16 - SEP02/16                       
         DC   XL3'740903',XL3'74091E' SEP03/16 - SEP30/16                       
         DC   XL3'740A01',XL3'740B04' OCT01/16 - NOV04/16                       
         DC   XL3'740B05',XL3'740C02' NOV05/16 - DEC02/16                       
         DC   XL3'740C03',XL3'740C1E' DEC03/16 - DEC30/16                       
         DC   XL3'740C1F',XL3'750203' DEC31/16 - FEB03/17                       
         DC   XL3'750204',XL3'750303' FEB04/17 - MAR03/17                       
         DC   XL3'750304',XL3'75031F' MAR04/17 - MAR31/17                       
         DC   XL3'750401',XL3'750505' APR01/17 - MAY05/17                       
         DC   XL3'750506',XL3'750602' MAY06/17 - JUN02/17                       
         DC   XL3'750603',XL3'75061E' JUN03/17 - JUN30/17                       
*                                                                               
*        2017-2018                                                              
*                                                                               
         DC   XL3'750701',XL3'750804' JUL01/17 - AUG04/17                       
         DC   XL3'750805',XL3'750901' AUG05/17 - SEP01/17                       
         DC   XL3'750902',XL3'75091D' SEP02/17 - SEP29/17                       
         DC   XL3'75091E',XL3'750B03' SEP30/17 - NOV03/17                       
         DC   XL3'750B04',XL3'750C01' NOV04/17 - DEC01/17                       
         DC   XL3'750C02',XL3'750C1D' DEC02/17 - DEC29/17                       
         DC   XL3'750C1E',XL3'760202' DEC30/17 - FEB02/18                       
         DC   XL3'760203',XL3'760302' FEB03/18 - MAR02/18                       
         DC   XL3'760303',XL3'76031E' MAR03/18 - MAR30/18                       
         DC   XL3'76031F',XL3'760504' MAR31/18 - MAY04/18                       
         DC   XL3'760505',XL3'760601' MAY05/18 - JUN01/18                       
         DC   XL3'760602',XL3'76061D' JUN02/18 - JUN29/18                       
*                                                                               
*        2018-2019                                                              
*                                                                               
         DC   XL3'76061E',XL3'760803' JUN30/18 - AUG03/18                       
         DC   XL3'760804',XL3'76081F' AUG04/18 - AUG31/18                       
         DC   XL3'760901',XL3'76091C' SEP01/18 - SEP28/18                       
         DC   XL3'76091D',XL3'760B02' SEP29/18 - NOV02/18                       
         DC   XL3'760B03',XL3'760B1E' NOV03/18 - NOV30/18                       
         DC   XL3'760C01',XL3'760C1C' DEC01/18 - DEC28/18                       
         DC   XL3'760C1D',XL3'770201' DEC29/18 - FEB01/19                       
         DC   XL3'770202',XL3'770301' FEB02/19 - MAR01/19                       
         DC   XL3'770302',XL3'77031D' MAR02/19 - MAR29/19                       
         DC   XL3'77031E',XL3'770503' MAR30/19 - MAY03/19                       
         DC   XL3'770504',XL3'77051F' MAY04/19 - MAY31/19                       
         DC   XL3'770601',XL3'77061C' JUN01/19 - JUN28/19                       
*                                                                               
*        2019-2020                                                              
*                                                                               
         DC   XL3'77061D',XL3'770802' JUN29/19 - AUG02/19                       
         DC   XL3'770803',XL3'77081E' AUG03/19 - AUG30/19                       
         DC   XL3'77081F',XL3'77091B' AUG31/19 - SEP27/19                       
         DC   XL3'77091C',XL3'770B01' SEP28/19 - NOV01/19                       
         DC   XL3'770B02',XL3'770B1D' NOV02/19 - NOV29/19                       
         DC   XL3'770B1E',XL3'770C1B' NOV30/19 - DEC27/19                       
         DC   XL3'770C1C',XL3'78011F' DEC28/19 - JAN31/20                       
         DC   XL3'780201',XL3'78021C' FEB01/20 - FEB28/20                       
         DC   XL3'78021D',XL3'78031B' FEB29/20 - MAR27/20                       
         DC   XL3'78031C',XL3'780501' MAR28/20 - MAY01/20                       
         DC   XL3'780502',XL3'78051D' MAY02/20 - MAY29/20                       
***      DC   XL3'78051E',XL3'78061A' MAY30/20 - JUN26/20                       
         DC   XL3'78051E',XL3'780703' MAY30/20 - JUL03/20                       
*                                                                               
*        2020-2021                                                              
*                                                                               
         DC   XL3'780704',XL3'780807' JUL04/20 - AUG07/20                       
         DC   XL3'780808',XL3'780904' AUG08/20 - SEP04/20                       
         DC   XL3'780905',XL3'780A02' SEP05/20 - OCT02/20                       
         DC   XL3'780A03',XL3'780B06' OCT03/20 - NOV06/20                       
         DC   XL3'780B07',XL3'780C04' NOV07/20 - DEC04/20                       
         DC   XL3'780C05',XL3'790101' DEC05/20 - JAN01/21                       
         DC   XL3'790102',XL3'790205' JAN02/21 - FEB05/21                       
         DC   XL3'790206',XL3'790305' FEB06/21 - MAR05/21                       
         DC   XL3'790306',XL3'790402' MAR06/21 - APR02/21                       
         DC   XL3'790403',XL3'790507' APR03/21 - MAY07/21                       
         DC   XL3'790508',XL3'790604' MAY08/21 - JUN04/21                       
         DC   XL3'790605',XL3'790702' JUN05/21 - JUL02/21                       
*                                                                               
         DC   XL3'FFFFFF'         END OF TABLE                                  
                                                                                
*                                                                               
ROUTLIST DS    0F                                                               
         DC    CL8'IHEADER ',A(IHEADER)                                         
         DC    CL8'OHEADER ',A(OHEADER)                                         
         DC    CL8'OBIL    ',A(OBIL)                                            
         DC    X'FF'                                                            
*                                                                               
SAVEMED  DS    CL1                                                              
SAVEUCOM DS    CL8                                                              
SAVEPRD  DS    CL3                                                              
SAVEEST  DS    CL3                                                              
SAVEESD  DS    CL17                                                             
PRINTH   DS    CL1                                                              
LINECNT  DS    H                                                                
* WORKING STORAGE                                                               
*                                                                               
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
WORKL    EQU   *-WORKD                                                          
*                                                                               
SCJPRNTD DSECT                                                                  
SCJMEDIA DS    CL1                 MEDIA                                        
         DS    CL1                 IGNORE                                       
SCJUCOM  DS    CL8                 UCOM                                         
         DS    CL1                 IGNORE                                       
SCJPROD  DS    CL3                 PRODUCT                                      
         DS    CL3                 IGNORE                                       
SCJESTMT DS    CL3                 ESTIMATE                                     
         DS    CL3                 IGNORE                                       
SCJESTDT DS    CL8                 ESTIMATE START DATE                          
         DS    CL10                IGNORE                                       
SCJBLMON DS    CL7                 BILLABLE MONTH                               
         DS    CL1                 IGNORE                                       
SCJONCD  DS    CL16                ORDERED NET-CASH DISCOUNT                    
         DS    CL1                 IGNORE                                       
SCJOGRS  DS    CL16                ORDERED GROSS                                
         DS    CL1                 IGNORE                                       
SCJONET  DS    CL16                ORDERED NET                                  
         DS    CL1                 IGNORE                                       
SCJOTAX  DS    CL16                ORDERED TAX                                  
         DS    CL1                 IGNORE                                       
SCJOCD   DS    CL16                ORDERED CASH DISC                            
*                                                                               
DETAILD  DSECT                                                                  
DETDET   DS    CL10                DETAIL LINE                                  
DETUCOM  DS    CL8                 UCOM                                         
DETPRD   DS    CL3                 PRODUCT                                      
DETEST   DS    CL3                 ESTIMATE                                     
         DS    CL5                 ESTIMATE                                     
DETEDATE DS    CL8                 ESTIMATE START DATE                          
DETEDPRD DS    CL3                 ESTIMATE DESCRIPTION PRODUCT                 
DETEDMOS DS    CL7                 ESTIMATE DESCRIPTION MOS                     
DETEDMED DS    CL1                 ESTIMATE DESCRIPTION MEDIA                   
DETEDSYS DS    CL1                 ESTIMATE DESCRIPTION SYSTEM                  
DETEDBLN DS    CL18                ESTIMATE DESCRIPTION BLANKS                  
DETETOT  DS    CL16                ORDERED NET + TAX - CASH DISCOUNT            
DETGROSS DS    CL16                ORDERED GROSS                                
DETNET   DS    CL16                ORDERED NET                                  
DETTAX   DS    CL16                ORDERED TAX                                  
DETCASHD DS    CL16                CASH DICOUNT                                 
DETLENQ  EQU   *-DETDET                                                         
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE PRWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE PJOBREC                                                        
       ++INCLUDE DRGLOBAL                                                       
         ORG   GLOBALD+1144                                                     
AOUTPDCB DS    A                                                                
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
       ++INCLUDE PRWRIFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE PRWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRWRIEAD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE PPCLRST                                                        
       ++INCLUDE PGENGRP                                                        
       ++INCLUDE PPDDEQUS                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009PRWRI16   05/28/20'                                      
         END                                                                    
