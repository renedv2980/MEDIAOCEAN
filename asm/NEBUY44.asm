*          DATA SET NEBUY44    AT LEVEL 076 AS OF 07/09/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE T31144B                                                                  
         TITLE 'NETPAK BUY PROGRAM - STEWARD ADD MODULE - T31144'               
T31144   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MBUY**,RA,RR=RE                                              
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK          R7 POINTS TO LOCAL WORKING STORAGE           
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         L     R5,ABUYVALS                                                      
         USING BUYVALD,R5                                                       
         USING TSARD,TSARBLK                                                    
         SPACE                                                                  
         TWAXC MULUNITH,CLRINPUTLEN=Y                                           
         SPACE                                                                  
MULT     BAS   RE,ACTED                                                         
         BNE   MULT2               INPUT IN ACTIN FIELD                         
         TM    MODE,FIRST          TEST FOR FIRST TIME                          
         BO    MULT4               YES-PUT OUT MESSAGE                          
         GOTO1 VBLDRQST            GENERATE TURN AROUND REQUEST                 
         BAS   RE,EDIT             EDIT INDIVIDUAL UNIT FIELDS                  
         BAS   RE,TSARINIT          INITIALIZE TSAR BUFFER                      
         BAS   RE,BUILD            BUILD THE RECORDS                            
         B     MULT4                                                            
         SPACE                                                                  
MULT2    BAS   RE,LIST             BUILD SCHEDULE LIST                          
         GOTO1 VBLDRQST            GENERATE TURN AROUND REQUEST                 
         BAS   RE,TSARINIT          INITIALIZE TSAR BUFFER                      
         BAS   RE,BUILD            FOR BM, CONSTRUCT UNITS AND ADD              
         SPACE                                                                  
MULT4    LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         TM    MODE,FIRST                                                       
         BZ    MULT8                                                            
         CLI   INPNTRY,0                                                        
         BNE   MULT8                                                            
         BAS   RE,BUILDTE                                                       
         NI    MODE,X'FF'-FIRST                                                 
         B     MULT                                                             
         SPACE                                                                  
MULT8    MVC   BUYMSG(18),=C'UNITS BELOW BOUGHT'                                
         MVI   BUYMSG+19,DASH                                                   
         MVC   BUYMSG+21(17),=C'ENTER NEXT ACTION'                              
         SPACE                                                                  
MULTX    NI    MODE,X'FF'-DISPLAY                                               
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO BUILD THE REQUESTED DATES FROM STEWARD                         
*                                                                               
BUILDTE  NTR1                                                                   
         LA    R2,MULUNITH                                                      
         LA    R3,STEWDATE                                                      
         LA    R4,52                                                            
*                                                                               
BDATE10  CLI   0(R3),X'40'                                                      
         BNH   BDATEEX                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(2,(R3)),(4,8(R2))                                  
         MVI   5(R2),5                                                          
         LA    R3,2(R3)                                                         
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                BUMP TO NEXT FIELD                          
         BCT   R4,BDATE10                                                       
*                                                                               
BDATEEX  B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* BUILD TSAR BUFFER FOR TRANSFER OF UNIT INFORMATION                            
*                                                                               
TSARINIT NTR1                                                                   
         MVI   TSACTN,TSAINI                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,DRFKEYX-DRFKEY                                            
         MVI   TSPAGN,3            REQUEST 3 PAGES                              
         MVI   TSPAGL,2            START PAGE NUMBER                            
*****    OI    TSINDS,TSINODSK     TEMPEST IS IN USE BY FALINK !!!              
         OI    TSINDS,TSIXTTWA     AND IT HAS BIG PAGES !                       
         LHI   R0,DRFRECLN                                                      
         STH   R0,TSRECL                                                        
         MVC   TSAREC,AIOAREA4                                                  
         BAS   RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXXMOD                                                           
         SPACE 2                                                                
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT ACTION FIELD FOR DATE EXPRESSIONS                         
*                                                                               
ACTED    NTR1                                                                   
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         TM    CLIOPT2,X'08'       TEST FOR FROZEN CLIENT                       
         BZ    ACTEDA                                                           
         LA    R2,BUYACTH          CABLE LOCK ERROR                             
         ST    R2,FADDR                                                         
         MVI   FERN,CLIFRERR                                                    
         B     ERROR                                                            
*                                                                               
ACTEDA   L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'20'      TEST FOR LOCKED PACKAGE                      
         BO    ACTEDB                                                           
         TM    NPAKCNTL,X'08'      TEST FOR CABLE LOCKED                        
         BO    ACTEDC                                                           
         B     ACTEDF                                                           
ACTEDB   MVI   FERN,PAKLERR        NO INPUT ALLOWED (LOCKED)                    
         B     ACTERR                                                           
ACTEDC   MVI   FERN,UCBLKERR       NO INPUT ALLOWED (CABLE LOCKED)              
         B     ACTERR                                                           
         DROP  RE                                                               
*                                                                               
ACTEDF   XC    FLAST,FLAST         START ALL OVER FOR ACTION FIELD              
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         SEARCH FOR COMMA AFTER ACTION CODE           
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,COMMA         TEST IF COMMA FOUND                          
         BE    ACTED1              YES                                          
         CLI   ACTION,CAL          NO-ACTION CAL REQUIRES DATES                 
         BNE   ACTEDX                                                           
         MVI   FERN,MISERR         SET ERROR MESSAGE                            
         MVC   XTRA(7),=C'DATE(S)'                                              
         B     ACTERR                                                           
         SPACE                                                                  
ACTED1   GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST IF ANYTHING FOLLOWS COMMA               
         BE    ACTED1R             NO                                           
         MVI   FNDX,2              YES-INITIALIZE FIELD INDEX                   
         L     R3,AIOAREA3         START INPUT ENTRY LIST IN IO3                
         USING INPD,R3                                                          
         B     ACTED3                                                           
         SPACE                                                                  
ACTED1R  MVI   FERN,MISERR                                                      
         MVC   XTRA(7),=C'DATE(S)'                                              
         B     ACTERR                                                           
         SPACE                                                                  
ACTED2   XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         LOOK FOR A COMMA                             
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY MORE INPUT                      
         BE    ACTEDX                                                           
         ZIC   RE,FNDX                                                          
         LA    RE,1(RE)                                                         
         STC   RE,FNDX                                                          
         SPACE                                                                  
ACTED3   ZIC   RE,FLDH+5           CHECK FOR BUY BOUGHT UNFROZEN                
         BCTR  RE,0                                                             
         EX    RE,FROZCOMP                                                      
         BNE   *+12                                                             
         MVI   BUYUNFR,C'Y'                                                     
         B     ACTED2                                                           
*                                                                               
         MVI   FERN,INVERR                                                      
         CLC   FLD(3),=C'RS='      CHECK REASON CODE                            
         BNE   ACTED3A                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),0                                    
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   WORK+1,4                                                         
         BH    ERROR                                                            
         MVC   AUDREASN,WORK+22    MOVE REASON CODE                             
         GOTO1 VCKREASN,DMCB,AUDREASN                                           
         B     ACTED2                                                           
*                                                                               
ACTED3A  CLI   FLDH+5,3            TEST FOR LENGTH OF 3                         
         BL    ACTED20             IF L.T. THEN IT IS EITHER DAY OR LEN         
         BH    ACTED3C             IF G.T. 3 CAN ONLY BE A DATE                 
         GOTO1 VDATVAL,DMCB,(1,FLD),DUB2                                        
         OC    0(4,R1),0(R1)       PRE-SCREEN FOR VALID DATE                    
         BZ    ACTED20             NO-CHECK FOR DAY OR LEN                      
*                                                                               
ACTED3C  XC    FTERM,FTERM                                                      
         MVC   FTERM(4),=C'-*,='                                                
         MVI   FLEN,0              RE-SCAN FIELD                                
         GOTO1 AFVAL,0                                                          
         MVI   FERN,INVERR                                                      
         CLI   FSTOP,C'='                                                       
         BNE   ACTED3E                                                          
         ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         EX    RE,ACTCOMP                                                       
         BNE   ACTERR                                                           
         B     ACTED30                                                          
*                                                                               
ACTED3E  DS    0H                                                               
         CLI   FSTOP,C'*'          TWO LENGTHS                                  
         BNE   *+12                                                             
         TM    FLDH+4,X'08'                                                     
         BO    ACTED20                                                          
         MVI   FERN,DATERR                                                      
         GOTO1 VDATVAL,DMCB,(1,FLD),DUB                                         
         OC    0(4,R1),0(R1)       TEST FOR ERROR                               
         BZ    ACTEDR                                                           
         CLC   FLDH+5(1),3(R1)     DOES DATE MAKE UP ALL OF FIELD               
         BNE   ACTEDR              NO                                           
         MVC   DUB(2),ESTSTART     ESTIMATE START YEAR                          
         CLC   ESTSTART(2),ESTEND  TEST IF EST ST/END IN SAME YEAR              
         BE    ACTED4                                                           
         CLC   DUB+2(4),ESTSTART+2 TEST IF INPUT MMDD LT EST ST MMDD            
         BNL   *+10                NO-SAME YEAR AS EST START                    
         MVC   DUB(2),ESTEND       YES-MUST BE YEAR OF EST END                  
         SPACE                                                                  
ACTED4   BAS   RE,CHKFEB29                                                      
         OC    INPST,INPST         TEST FOR FIRST DATE                          
         BZ    *+8                 YES                                          
         LA    R3,INPNTRL(R3)      NO-BUMP INPUT ENTRY POINTER                  
         BAS   RE,CHKDEFRT         CHECK CLIENT ESTIMATE RATE TYPE              
         MVC   INPST,DUB           SET START DATE                               
         ZIC   RE,INPNTRY                                                       
         LA    RE,1(RE)            INCREMENT LIST ENTRY COUNT                   
         STC   RE,INPNTRY                                                       
         SPACE                                                                  
ACTED6   CLI   FSTOP,C'-'          TEST IF DASH STOPPED SCAN                    
         BE    ACTED7              YES                                          
         MVI   INPCTL,SINGLE       NO-HAD TO BE SINGLE DATE                     
         CLI   FSTOP,C'*'          TEST FOR STAR                                
         BE    ACTED12             LOOK FOR NUMBER PER WEEK OVERRIDE            
         B     ACTED15             ALL DONE WITH DATE                           
         SPACE                                                                  
ACTED7   XC    FTERM,FTERM                                                      
         MVC   FTERM(3),=C'(*,'    EXTRACT SECOND PART OF DATE EXPR.            
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR NOTHING BETWEEN '-' AND END         
         BNE   *+12                NO-SOMETHING THERE                           
         MVI   FERN,MISERR         YES-ERROR                                    
         B     ACTEDR                                                           
         CLI   FLDH+5,1            TEST FOR LENGTH OF 1                         
         BNE   ACTED8                                                           
         MVI   FERN,INVERR                                                      
         CLI   FLD,C'E'                                                         
         BNE   ACTEDR                                                           
         MVC   INPEND,ESTEND       SET ESTIMATE END DATE                        
         MVI   INPCTL,DATE                                                      
         B     ACTED8B                                                          
         SPACE                                                                  
ACTED8   GOTO1 VDATVAL,DMCB,(1,FLD),DUB                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    ACTED9              NOT A DATE                                   
         MVI   FERN,DATERR                                                      
         CLC   FLDH+5(1),3(R1)                                                  
         BNE   ACTEDR                                                           
         MVC   DUB(2),ESTSTART                                                  
         CLC   ESTSTART(2),ESTEND                                               
         BE    ACTED8A                                                          
         CLC   DUB+2(4),ESTSTART+2                                              
         BNL   *+10                                                             
         MVC   DUB(2),ESTEND                                                    
         SPACE                                                                  
ACTED8A  BAS   RE,CHKFEB29                                                      
         MVC   INPEND,DUB                                                       
         CLC   INPST,INPEND                                                     
         BL    *+12                                                             
         MVI   FERN,SEQERR                                                      
         B     ACTEDR                                                           
         MVI   INPCTL,DATE                                                      
         B     ACTED8B                                                          
         SPACE                                                                  
ACTED8B  CLI   FSTOP,LPAREN        TEST LEFT PAREN FOUND                        
         BNE   ACTED8C             NO                                           
         XC    FTERM,FTERM         LOOK FOR DAILY BUYING IF DATE-DATE           
         MVC   FTERM(2),=C'*,'     GET REST OF EXPRESSION W/IN PARENS           
         GOTO1 AFVAL,0                                                          
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,2                                                         
         BNE   ACTEDR                                                           
         CLC   FLD(2),=C'D)'       MUST BE '(D)' FOR DAILY BUYING               
         BNE   ACTEDR                                                           
         OI    INPCTL,DAILY                                                     
         SPACE 1                                                                
ACTED8C  CLI   FSTOP,C'*'                                                       
         BE    ACTED12             DO NUMBER PER WEEK CHECK                     
         B     ACTED15             ALL DONE                                     
         SPACE 1                                                                
ACTED9   XC    FTERM,FTERM         ONLY POSSIBILITY IS 'N' WEEKS                
         MVC   FTERM,=C'W*,'                                                    
         MVI   FLEN,0              START EDIT AGAIN AT DASH                     
         GOTO1 AFVAL,0                                                          
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,0            TEST FOR SOMETHING BETWEEN '-' AND           
         BE    ACTEDR              END-NO                                       
         CLI   FSTOP,C'W'          TEST IF 'W' HALTED IT                        
         BNE   ACTEDR                                                           
         TM    FLDH+4,X'08'        TEST FOR NUMERIC PREFIX                      
         BZ    ACTEDR                                                           
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ACTEDR                                                           
         CH    R0,=H'52'           NO MORE THAN 52 WEEKS                        
         BH    ACTEDR                                                           
         STC   R0,INPWEEKS                                                      
         MVI   INPIND,EVERY                                                     
         MVI   INPCTL,NWEEKS                                                    
*                                                                               
         XC    FTERM,FTERM                                                      
         MVC   FTERM(2),=C'*,'                                                  
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR SOMETHING AFTER 'W'                 
         BNE   ACTED9A             YES-GO EDIT IT                               
         ICM   R1,7,FLAST          WHEN RE-EDITING AND EMPTY FIELD              
         LA    R1,1(R1)            FOUND-INCREMENT FLAST TO RESUME              
         STCM  R1,7,FLAST          SCAN AFTER FIELD ENDING STOP CHAR.           
         B     ACTED10                                                          
*                                                                               
ACTED9A  CLI   FLDH+5,1                                                         
         BNE   ACTEDR              CAN ONLY BE 1 BYTE                           
         CLI   FLD,C'A'            ALTERNATE WEEKS                              
         BNE   *+12                                                             
         MVI   INPIND,ALT                                                       
         B     ACTED10                                                          
         CLI   FLD,C'T'            THREE WEEK INTERVAL                          
         BNE   *+12                                                             
         MVI   INPIND,THIRD                                                     
         B     ACTED10                                                          
         CLI   FLD,C'F'            EVERY FOURTH WEEK                            
         BNE   ACTEDR                                                           
         MVI   INPIND,FOURTH                                                    
*                                                                               
ACTED10  CLI   FSTOP,C'*'                                                       
         BE    ACTED12                                                          
         B     ACTED15             ALL DONE                                     
         SPACE                                                                  
* COMMON NUMBER PER WEEK EDIT                                                   
*                                                                               
ACTED12  XC    FTERM,FTERM         LOOK FOR A COMMA AFTER NUMBER PER            
         MVI   FTERM,COMMA         WEEK OVERRIDE                                
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BNE   ACTED13                                                          
         MVI   FERN,MISERR                                                      
         MVC   XTRA(11),=C'NUMBER/WEEK'                                         
         B     ACTERR                                                           
         SPACE                                                                  
ACTED13  MVI   FERN,INVERR                                                      
         TM    FLDH+4,X'08'                                                     
         BZ    ACTEDR                                                           
         CLI   FLDH+5,2                                                         
         BH    ACTEDR                                                           
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ACTERR                                                           
         CH    R0,=Y(FIELDS)                                                    
         BH    ACTERR                                                           
         STC   R0,INPNUM           NUMBER PER WEEK OVERRIDE                     
         SPACE                                                                  
ACTED15  B     ACTED2              END OF DATE EDIT-GET NEXT FIELD              
         SPACE                                                                  
* LENGTH AND DAY EDITS                                                          
*                                                                               
ACTED20  TM    FLDH+4,X'08'        TEST FOR NUMERIC INPUT                       
         BZ    ACTED25             NO-LOOK FOR DAY                              
         CVB   R0,DUB                                                           
         MVI   FERN,INVERR                                                      
         LTR   R0,R0                                                            
         BZ    ACTERR                                                           
         CH    R0,=H'255'                                                       
         BH    ACTERR                                                           
         CLI   INPLEN,0                                                         
         BE    *+14                                                             
         MVC   XTRA(13),=C'DUPLICATE LEN'                                       
         B     ACTERR                                                           
         STC   R0,INPLEN                                                        
         OC    INPST,INPST         TEST FOR PRECEDING DATE                      
         BNZ   ACTED22                                                          
         MVC   XTRA(12),=C'MISSING DATE'                                        
         B     ACTERR                                                           
         SPACE                                                                  
ACTED22  CLI   FSTOP,C'*'          TWO LENGTHS                                  
         BNE   ACTED40                                                          
         XC    FTERM,FTERM                                                      
         MVC   FTERM(1),=C','      EXTRACT SECOND LENGTH                        
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR NOTHING BETWEEN '*' AND ','         
         BNE   *+12                NO-SOMETHING THERE                           
         MVI   FERN,MISERR         YES-ERROR                                    
         B     ACTEDR                                                           
         SPACE                                                                  
         TM    FLDH+4,X'08'        TEST FOR NUMERIC INPUT                       
         BZ    ACTERR              NO-LOOK FOR DAY                              
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ACTERR                                                           
         CH    R0,=H'255'                                                       
         BH    ACTERR                                                           
         STC   R0,INPLEN2          STORE SECOND LENGTH                          
         B     ACTED40                                                          
         SPACE                                                                  
ACTED25  ZIC   R0,FLDH+5                                                        
         GOTO1 VDAYVAL,DMCB,((R0),FLD),BYTE,DUB                                 
         MVI   FERN,INVERR                                                      
         CLI   BYTE,0                                                           
         BE    ACTEDR                                                           
         MVI   FERN,DAYERR                                                      
         LA    R0,7                COUNTER                                      
         LA    R1,X'40'            INITIAL BIT MASK                             
         SR    RE,RE               CLEAR COUNT OF ON BITS                       
ACTED26  EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0                                                           
         BZ    *+8                                                              
         LA    RE,1(RE)            INCREMENT COUNT OF ON BITS                   
         SRL   R1,1                NEXT DAY BIT POSITION                        
         BCT   R0,ACTED26                                                       
         CH    RE,=H'1'            TEST FOR MORE THAN ONE DAY                   
         BH    ACTEDR                                                           
         TM    INPCTL,DAILY        CANNOT HAVE DAY OVERRIDE                     
         BZ    *+12                AND DAILY BUYING                             
         MVI   FERN,INVERR                                                      
         B     ACTEDR                                                           
         SPACE                                                                  
ACTED27  CLI   INPDAY,0                                                         
         BE    *+14                                                             
         MVC   XTRA(13),=C'DUPLICATE DAY'                                       
         B     ACTERR                                                           
         MVC   INPDAY,BYTE                                                      
         MVC   INPDAYNO,DUB                                                     
         OC    INPST,INPST                                                      
         BNZ   ACTED40                                                          
         MVC   XTRA(12),=C'MISSING DATE'                                        
         B     ACTERR                                                           
         SPACE                                                                  
ACTED30  CLI   FSTOP,C'='          ACTUAL COST FIELD                            
         BNE   ACTED40                                                          
         XC    FTERM,FTERM                                                      
         MVC   FTERM(1),=C','      EXTRACT SECOND LENGTH                        
         XC    DUB,DUB                                                          
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR NOTHING BETWEEN '*' AND ','         
         BNE   *+12                NO-SOMETHING THERE                           
         MVI   FERN,MISERR         YES-ERROR                                    
         B     ACTEDR                                                           
*                                                                               
         BAS   RE,CHKBUYRT                                                      
         ZIC   R0,FLDH+5           DATA LENGTH                                  
         LTR   R0,R0                                                            
         BZ    BADCOST                                                          
         GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'                                                      
         BE    BADCOST                                                          
*                                                                               
         MVC   INPAMT,4(R1)                                                     
         OC    INPAMT,INPAMT                                                    
         BNZ   *+8                                                              
         MVI   INPZAMT,C'Y'                                                     
ACTED40  B     ACTED2              LOOK FOR NEXT FIELD                          
         SPACE                                                                  
ACTEDR   B     ACTERR                                                           
         SPACE                                                                  
ACTEDX   MVI   FERN,AUDITERR                                                    
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'02'      IS REASON CODE REQUIRED                      
         BO    ACTEDX10            YES                                          
         OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    ACTEDX30                                                         
         B     ACTERR                                                           
ACTEDX10 OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    ACTERR                                                           
*                                                                               
ACTEDX30 MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         CLI   INPNTRY,0           SET CC ON EXIT                               
         B     EXXMOD                                                           
         DROP  R3                                                               
ACTCOMP  CLC   FLD(0),=CL6'ACTUAL'                                              
FROZCOMP CLC   FLD(0),=CL8'UNFROZEN'                                            
*                                                                               
BADCOST  LA    R2,BUYACTH                                                       
         ST    R2,FADDR            SET ERROR CURSOR POSITION                    
         MVC   BUYMSG(L'INVCOST),INVCOST                                        
         MVI   FERN,USERERR                                                     
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     ACTERR                                                           
         MVC   BUYMSG+36(0),FLD                                                 
         EJECT                                                                  
*                                                                               
* RATE TYPE CLIENT ESTIMATE VALIDATOR                                           
*                                                                               
CHKDEFRT NTR1                                                                   
         XC    HALF,HALF                                                        
*--CHECK ESTIMATE RECORD                                                        
         GOTO1 FLDCHK,DMCB2,RTRECTAB,ESTRATE,HALF      CHECK RATE               
         CLI   ESTRATEC,X'40'                                                   
         BL    *+10                                                             
         MVC   HALF+1(1),ESTRATEC                                               
         CLI   HALF,0                                                           
         BNE   CHK200                                                           
*--CHECK CLIENT RECORD                                                          
CHK40    MVC   BYTE,CLIPRO+14                                                   
         GOTO1 FLDCHK,DMCB2,RTRECTAB,BYTE,HALF         CHECK RATE               
         CLI   CLIEXTRA+14,X'40'                                                
         BL    *+10                                                             
         MVC   HALF+1(1),CLIEXTRA+14                                            
         CLI   HALF,0                                                           
         BE    EXXMOD                                                           
         B     CHK200                                                           
*                                                                               
*--CHECK BUY RATE                                                               
*  R3=INPUT RATE LOCATION                                                       
CHKBUYRT NTR1                                                                   
         USING INPD,R3                                                          
         XC    HALF,HALF                                                        
         GOTO1 FLDCHK,DMCB2,RTFLDTAB,FLD,HALF          CHECK RATE               
         CLI   HALF,0                                                           
         BE    EXXMOD                                                           
         BAS   RE,CUTFLD                                                        
         GOTO1 FLDCHK,DMCB2,CVFLDTAB,FLD,HALF+1        CHECK COVERAGE           
         CLI   HALF+1,0                                                         
         BE    CHK200              BRANCH TO WRITE TO ELEMENT                   
         BAS   RE,CUTFLD                                                        
*                                                                               
CHK200   MVC   INPRATE,HALF        MOVE IN RATE                                 
         MVI   INPCVRG,0                                                        
         CLI   HALF+1,C'A'         DONT MOVE DEFAULT VALUE OUT                  
         BE    *+10                                                             
         MVC   INPCVRG,HALF+1      MOVE IN COVERAGE                             
         B     EXXMOD                                                           
*                                                                               
*****    REMOVES FIRST CHARACTER FROM FIELD                                     
CUTFLD   NTR1                                                                   
         MVC   FLD(9),FLD+1                                                     
         MVI   FLD+9,X'40'                                                      
         ZIC   R3,FLDH+5                                                        
         BCTR  R3,0                                                             
         STC   R3,FLDH+5                                                        
         B     EXXMOD                                                           
*                                                                               
*****   INPUT          P1=A(TABLE)                                              
*                      P2=RATE                                                  
*       OUTPUT         P3=1 BYTE OUTPUT SET TO ZERO NOT FOUND                   
FLDCHK   NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
FLDCH50  CLI   0(R2),C' '                                                       
         BE    FLDCH100                                                         
         CLC   0(1,R2),0(R3)                                                    
         BE    FLDCH70                                                          
         LA    R2,2(R2)                                                         
         B     FLDCH50                                                          
FLDCH70  MVC   0(1,R4),1(R2)                                                    
         B     FLDCHEX                                                          
FLDCH100 MVI   0(R4),0                                                          
FLDCHEX  B     EXXMOD                                                           
*                                                                               
RTFLDTAB DC    CL17'FFCCWWYYHHTTRRJJ '                                          
RTRECTAB DC    CL9'2F8C9WYY '                                                   
CVFLDTAB DC    CL7'AAIITT '                                                     
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GENERATE LIST OF UNITS FROM INPUT LIST                         
*                                                                               
LIST     NTR1                                                                   
         ZIC   R0,INPNTRY          COUNT OF ENTRIES IN INPUT LIST               
         L     R3,AIOAREA3         R3 POINTS TO INPUT LIST                      
         USING INPD,R3                                                          
         LA    R4,SCHEDULE         R4 POINTS TO SCHEDULE LIST                   
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         SPACE                                                                  
LIST2    TM    INPCTL,SINGLE       TEST FOR SINGLE DATE FORMAT                  
         BZ    LIST4                                                            
*                                                                               
         GOTO1 PROGCHK,INPST                                                    
         GOTO1 CALEN,DMCB,INPST,(R3)                                            
         GOTO1 PROGCHK,DUB                                                      
         BAS   RE,DATCHK           CHECK IF DATE W/IN EST AND SEQUENCE          
         SR    R2,R2                                                            
         ICM   R2,1,INPNUM                                                      
         BNZ   *+8                                                              
         LA    R2,1                NUMBER PER WEEK IS 1                         
LIST3    GOTO1 ADDENTRY,DMCB,DUB,INPST                                          
         BCT   R2,LIST3                                                         
         B     LISTX               ALL DONE                                     
         SPACE                                                                  
LIST4    TM    INPCTL,NWEEKS                                                    
         BZ    LIST8                                                            
*                                                                               
         GOTO1 PROGCHK,INPST                                                    
         GOTO1 CALEN,DMCB,INPST,(R3)                                            
         GOTO1 PROGCHK,DUB                                                      
         BAS   RE,DATCHK                                                        
         MVC   WEEKS,INPWEEKS      COUNT OF WEEKS                               
         LA    RE,WEEKTAB                                                       
LIST4A   CLI   0(RE),X'FF'         TEST FOR END-OF-TABLE                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   INPIND,0(RE)        TEST FOR MATCH AGAINST TABLE                 
         BE    *+12                                                             
         LA    RE,L'WEEKTAB(RE)    NEXT TABLE ENTRY                             
         B     LIST4A                                                           
         ZIC   R6,1(RE)            GET NUMBER OF DAYS BETWEEN WEEKS             
         SPACE                                                                  
LIST5    SR    R2,R2                                                            
         ICM   R2,1,INPNUM                                                      
         BNZ   *+8                                                              
         LA    R2,1                                                             
LIST6    GOTO1 ADDENTRY,DMCB,DUB,INPST                                          
         BCT   R2,LIST6                                                         
*                                                                               
         ZIC   R1,WEEKS                                                         
         SH    R1,=H'1'            DECREMENT COUNT                              
         JM    *+2                 COUNTER HAS GONE NEGATIVE!                   
         BZ    LISTX               ALL DONE                                     
         STC   R1,WEEKS                                                         
         GOTO1 VADDAY,DMCB,DUB,DUB,(R6)                                         
         BAS   RE,DATCHK           CHECK NEXT DATE                              
         GOTO1 PROGCHK,DUB                                                      
         B     LIST5                                                            
         SPACE                                                                  
LIST8    TM    INPCTL,DATE                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 PROGCHK,INPST                                                    
         GOTO1 CALEN,DMCB,INPST,(R3)                                            
         GOTO1 PROGCHK,DUB                                                      
         MVC   STDATE,DUB                                                       
         GOTO1 CALEN,DMCB,INPEND,(R3)                                           
         MVC   ENDATE,DUB                                                       
*                                                                               
         MVI   DAILYFLG,NO                                                      
         TM    INPCTL,DAILY        TEST DAILY BUYING ACTIVE                     
         BZ    LIST8B              NO                                           
         L     RE,APROGEL                                                       
         USING NPGELEM,RE                                                       
         MVC   HALF(1),NPGDAY                                                   
         CLI   NPGROT,0                                                         
         BE    *+10                                                             
         MVC   HALF(1),NPGROT      IF ROTATION EXISTS USE THAT AS DAY           
*                                                                               
         LA    RE,DAYS             CHECK FOR SINGLE DAY OR ROTATION             
         LA    RF,DAYTAB                                                        
LIST8A   CLC   0(1,RF),HALF                                                     
         BE    LIST8B                                                           
         LA    RF,L'DAYTAB(RF)                                                  
         BCT   RE,LIST8A                                                        
*                                                                               
         MVI   DAILYFLG,YES        SET DAILY BUYING FLAG                        
         MVC   STDATE,INPST        TAKE THE FLIGHT SPAN AS GIVEN                
         GOTO1 COVTST,STDATE                                                    
         MVC   INPDAY,BYTE                                                      
         MVC   ENDATE,INPEND       AND DO NOT CALENDARIZE IT                    
         DROP  RE                                                               
*                                                                               
LIST8B   XC    DUB,DUB                                                          
         MVC   DUB(6),STDATE                                                    
         SPACE                                                                  
LIST9    BAS   RE,DATCHK                                                        
         SR    R2,R2                                                            
         ICM   R2,1,INPNUM                                                      
         BNZ   *+8                                                              
         LA    R2,1                                                             
LIST10   GOTO1 ADDENTRY,DMCB,DUB,INPST                                          
         BCT   R2,LIST10                                                        
*                                                                               
         CLI   DAILYFLG,YES        TEST DAILY BUYING ACTIVE                     
         BNE   LIST11                                                           
         BAS   RE,NEXTDAT          GET NEXT DATE                                
         MVC   INPDAY,BYTE         AND ITS DAY VALUE                            
         B     LIST12                                                           
*                                                                               
LIST11   GOTO1 VADDAY,DMCB,DUB,DUB,7                                            
*                                                                               
LIST12   CLC   DUB(6),ESTEND       TEST IF PAST ESTIMATE END                    
         BH    LISTX               YES-ALL DONE                                 
         CLC   DUB(6),ENDATE       TEST IF PAST END DATE                        
         BH    LISTX               YES-ALL DONE                                 
         GOTO1 PROGCHK,DUB                                                      
         B     LIST9               YES-ALL DONE                                 
         SPACE                                                                  
LISTX    LA    R3,INPNTRL(R3)                                                   
         BCT   R0,LIST2                                                         
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CALENDARIZE DATE                                               
*                                                                               
* AT ENTRY, P1=A(DATE)                                                          
*           P2=A(INPUT LIST ENTRY)                                              
* AT EXIT, DUB CONTAINS FINAL DATE                                              
*                                                                               
CALEN    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         USING INPD,R3                                                          
         GOTO1 VGETDAY,DMCB2,(R2),THREE                                         
         MVC   BYTE,0(R1)          DAY NUMBER                                   
         L     RE,APROGEL                                                       
         USING NPGELEM,RE                                                       
         MVC   HALF(1),NPGDAY                                                   
         MVC   HALF+1(1),NPGDAYNO                                               
         CLI   NPGROT,0                                                         
         BE    *+16                                                             
         MVC   HALF(1),NPGROT      IF ROTATION EXISTS USE THAT AS DAY           
         MVC   HALF+1(1),NPGROTNO                                               
*                                                                               
         MVC   DAYNO,HALF+1        DEFAULT TO PROGRAM DAY NUM                   
         CLI   INPDAY,0            TEST FOR DAY OVERRIDE                        
         BE    CAL2                NO                                           
         MVC   DAYNO,INPDAYNO                                                   
         MVC   DAYB,INPDAY                                                      
         NC    DAYB,HALF                                                        
         CLC   DAYB,INPDAY         TEST PROGRAM DAY COVERS INPUT DAY            
         BE    CAL2                                                             
         MVI   FERN,COVERR                                                      
         B     ERROR                                                            
         SPACE                                                                  
CAL2     ZIC   R0,DAYNO            DAY NUMBER OF DAY                            
         SRL   R0,4                                                             
         ZIC   R1,BYTE             DAY NUMBER OF DATE                           
         MVC   DUB(6),0(R2)                                                     
         SR    R0,R1                                                            
         BZ    CALX                                                             
         CLI   ACTION,CAL          ACTION CAL - SET CORRECT DATES               
         BE    CAL4                                                             
         CLI   BUYPROF+12,YES      DAY CHANGE ALLOWED                           
         BNE   CAL4                                                             
         CLI   INPDAY,0            IF DATE INPUT ERROR                          
         BE    CALX                                                             
         MVI   FERN,DAYDTEMT                                                    
         B     ERROR                                                            
         SPACE                                                                  
CAL4     GOTO1 VADDAY,DMCB2,(R2),DUB,(R0)                                       
         SPACE                                                                  
CALX     B     EXXMOD                                                           
         DROP  RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK DATE FOR FIT WITHIN ESTIMATE AND FOR SEQUENCE            
* AT ENTRY DUB CONTAINS DATE, R3 POINTS TO START DATE (INPUT LIST)              
*                                                                               
DATCHK   ST    RE,SAVEREG                                                       
         CLC   DUB(6),ESTSTART                                                  
         BL    DAT1                                                             
         CLC   DUB(6),ESTEND                                                    
         BH    DAT2                                                             
         OC    LASTDATE,LASTDATE                                                
         BZ    *+14                                                             
         CLC   DUB(6),LASTDATE                                                  
         BL    DAT3                                                             
         MVC   LASTDATE,DUB                                                     
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE                                                                  
DAT1     MVI   FERN,STERR                                                       
         B     DATR                                                             
         SPACE                                                                  
DAT2     MVI   FERN,ENDERR                                                      
         B     DATR                                                             
         SPACE                                                                  
DAT3     MVI   FERN,SEQERR                                                      
         SPACE                                                                  
DATR     GOTO1 VDATCON,DMCB,(0,(R3)),(4,XTRA)                                   
         B     ACTERR                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO ADD AN ENTRY TO SCHEDULE LIST.  AT ENTRY, R4 POINTS            
* TO LIST, P1=A(DATE), P2=A(INPUT LIST ENTRY)                                   
*                                                                               
ADDENTRY NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         USING LISTD,R4                                                         
         GOTO1 VDATCON,DMCB2,(0,(R2)),(2,LISTDATE)                              
         MVC   LISTDAY,INPDAY                                                   
         MVC   LISTLEN,INPLEN                                                   
         MVC   LISTLEN2,INPLEN2                                                 
         MVC   LISTAMT,INPAMT                                                   
         MVC   LISTZAMT,INPZAMT                                                 
         MVC   LISTRATE,INPRATE                                                 
         MVC   LISTCVRG,INPCVRG                                                 
         ZIC   R1,UNITS                                                         
         LA    R1,1(R1)                                                         
         STC   R1,UNITS                                                         
         LA    R4,LISTNTRL(R4)                                                  
         CLI   UNITS,FIELDS                                                     
         BH    ADDENTR                                                          
         XIT1  REGS=(R4)                                                        
*                                                                               
ADDENTR  MVC   BUYMSG(L'TOOMANY),TOOMANY                                        
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
         DROP  R3,R4                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK IF PROGRAM RECORD COVERS A DATE                          
*                                                                               
* AT ENTRY, R1 CONTAINS A(DATE) AND ROUTINE ASSUMES FADDR IS SET                
*                                                                               
PROGCHK  NTR1                                                                   
         LR    R2,R1                                                            
         GOTO1 VDATCON,DMCB2,(0,(R2)),(2,HALF2)                                 
         GOTO1 VGETPROG,(R1),HALF2                                              
         CLI   FERN,0              TEST FOR UNCOVERED DATE                      
         BNE   PROGCHKR                                                         
         GOTO1 VDISPROG                                                         
         B     EXXMOD                                                           
         SPACE                                                                  
PROGCHKR GOTO1 VDATCON,(R1),(0,(R2)),(4,XTRA)                                   
         B     ERROR                                                            
         EJECT                                                                  
* SUB TO CHECK FEB29 (LEAP YEAR) DATE VALID                                     
CHKFEB29 NTR1                                                                   
         CLC   =C'FEB29',FLD                                                    
         BNE   EXXMOD                                                           
         MVI   FERN,DATERR                                                      
         GOTO1 VDATCON,DMCB2,(0,DUB),(5,WORK)                                   
         GOTO1 VDATVAL,DMCB2,(0,WORK),(5,WORK+10)                               
         OC    DMCB2(4),DMCB2                                                   
         BZ    ERROR                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO CHECK IF DATE IS COVERED BY PROGRAM'S DAYS                     
* ON ENTRY, R1=A(DATE).  ON EXIT, BYTE CONTAINS ITS DAY BIT VALUE               
*                                                                               
COVTST   NTR1                                                                   
         LR    R2,R1               R2=A(DATE)                                   
         GOTO1 VGETDAY,DMCB2,(R2),THREE                                         
         ZIC   RF,0(R1)            RF=DAY NUMBER                                
         LA    R1,X'80'            DAY BIT MASK                                 
         SRL   R1,0(RF)            SHIFT TO DAY POSITION                        
         L     RE,APROGEL                                                       
         USING NPGELEM,RE                                                       
         MVC   HALF(1),NPGDAY                                                   
         CLI   NPGROT,0                                                         
         BE    *+10                                                             
         MVC   HALF(1),NPGROT      IF ROTATION EXISTS USE THAT AS DAY           
         STC   R1,BYTE                                                          
         NC    BYTE,HALF                                                        
         CLM   R1,1,BYTE           TEST IF PROGRAM DAYS COVERED DATE            
         BE    EXXMOD              YES                                          
         MVI   FERN,COVERR                                                      
         GOTO1 VDATCON,DMCB2,(R2),(4,XTRA)                                      
         B     ERROR                                                            
         DROP  RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET NEXT DATE FOR DAILY BUYING FUNCTION                        
* AT ENTRY, DUB CONTAINS LAST UNIT DATE. ON EXIT DUB CONTAINS                   
* NEXT DATE COVERED BY PROGRAM DAYS AND BYTE CONTAINS ITS DAY BIT VALUE         
*                                                                               
NEXTDAT  NTR1                                                                   
         L     R2,APROGEL                                                       
         USING NPGELEM,R2                                                       
         MVC   HALF(1),NPGDAY                                                   
         CLI   NPGROT,0                                                         
         BE    *+10                                                             
         MVC   HALF(1),NPGROT      IF ROTATION EXISTS USE THAT AS DAY           
*                                                                               
NEXTDAT2 GOTO1 VADDAY,DMCB2,DUB,DUB,1                                           
         GOTO1 VGETDAY,(R1),DUB,THREE                                           
         ZIC   RF,0(R1)            DAY NUMBER                                   
         LA    R1,X'80'                                                         
         SRL   R1,0(RF)            POSITION DAY BIT                             
         STC   R1,BYTE                                                          
         NC    BYTE,HALF                                                        
         CLM   R1,1,BYTE           TEST PROGRAM DAYS COVER UNIT DAY             
         BE    EXXMOD              YES                                          
         B     NEXTDAT2            NO-TRY NEXT DAY                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT UNIT FIELD INPUT UNDER ACTION BM                          
*                                                                               
EDIT     NTR1                                                                   
         LA    R2,MULUNITH                                                      
         XC    LASTDATE,LASTDATE                                                
         LA    R4,SCHEDULE                                                      
         USING LISTD,R4                                                         
         SPACE                                                                  
EDIT1    ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BE    EDIT20                                                           
         CLI   FLD,STAR            TEST FOR SKIP EDIT CHARACTER                 
         BE    EDIT20                                                           
*                                                                               
EDIT2    XC    ENTRY,ENTRY                                                      
         LA    R3,ENTRY            BUILD A DUMMY INPUT LIST ENTRY               
         USING INPD,R3                                                          
         MVC   FTERM(2),=C'*,'     LOOK FOR A DATE FIRST                        
         MVI   FLEN,0              RE-SCAN FIELD                                
         GOTO1 AFVAL,0                                                          
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,0            DEFEND AGAINST SOLE COMMA                    
         BE    ERROR                                                            
         MVI   FERN,DATERR                                                      
         GOTO1 VDATVAL,DMCB,(1,FLD),DUB                                         
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         CLC   FLDH+5(1),DMCB+3                                                 
         BNE   ERROR                                                            
         MVC   DUB(2),ESTSTART                                                  
         CLC   ESTSTART(2),ESTEND                                               
         BE    EDIT3                                                            
         CLC   DUB+2(4),ESTSTART+2                                              
         BNL   *+10                                                             
         MVC   DUB(2),ESTEND                                                    
         SPACE                                                                  
EDIT3    BAS   RE,CHKFEB29                                                      
         MVC   INPST,DUB           SET START DATE                               
         CLI   FSTOP,STAR          TEST FOR STAR                                
         BNE   EDIT4                                                            
*                                                                               
         XC    FTERM,FTERM         LOOK FOR A NUMBER                            
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0            TEST IF ANYTHING FOUND                       
         BE    EDIT3R              YES                                          
         MVI   FERN,NUMERR                                                      
         TM    FLDH+4,X'08'        TEST FOR NUMERIC FIELD                       
         BZ    EDIT3R                                                           
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,2            TEST FOR TWO DIGITS OR LESS                  
         BH    EDIT3R                                                           
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    EDIT3R                                                           
         CH    R0,=Y(FIELDS)       TEST FOR SCREEN LIMIT                        
         BH    EDIT3R              NO-OVER                                      
         STC   R0,INPNUM           NUMBER PER WEEK                              
         B     EDIT4                                                            
         SPACE                                                                  
EDIT3R   MVC   XTRA(11),=C'NUMBER/WEEK'                                         
         B     ERROR                                                            
         SPACE                                                                  
EDIT4    LA    R0,3                TWO MORE FIELDS AFTER DATE                   
         SPACE                                                                  
EDIT5    XC    FTERM,FTERM                                                      
         MVC   FTERM(3),=C',*='                                                 
         GOTO1 AFVAL,0                                                          
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   EDIT5A              YES                                          
         CLI   FSTOP,C'*'          TEST FOR SOLE ASTERIKS                       
         BE    ERROR                                                            
         CLI   FSTOP,C'='          TEST FOR SOLE EQUALS                         
         BE    ERROR                                                            
         CLI   FSTOP,COMMA         TEST FOR SOLE COMMA                          
         BE    ERROR                                                            
         B     EDIT13              ONLY A DATE INPUT IN FIELD                   
*                                                                               
EDIT5A   CLI   FSTOP,C'='          TEST FOR ACTUAL COST INPUT                   
         BE    EDIT8                                                            
         TM    FLDH+4,X'08'        TEST FOR NUMERIC INPUT                       
         BZ    EDIT6                                                            
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    ERROR                                                            
         CH    R1,=H'255'                                                       
         BH    ERROR                                                            
         CLI   INPLEN,0            DEFEND AGAINST DUPLICATES                    
         BNE   ERROR                                                            
         STC   R1,INPLEN                                                        
         SPACE                                                                  
         CLI   FSTOP,C'*'          TWO LENGTHS                                  
         BNE   EDIT11                                                           
         XC    FTERM,FTERM                                                      
         MVC   FTERM(1),=C','      EXTRACT SECOND LENGTH                        
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR NOTHING BETWEEN '*' AND ','         
         BNE   *+12                NO-SOMETHING THERE                           
         MVI   FERN,MISERR         YES-ERROR                                    
         B     ERROR                                                            
         SPACE                                                                  
         TM    FLDH+4,X'08'        TEST FOR NUMERIC INPUT                       
         BZ    ERROR               NO-LOOK FOR DAY                              
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    ERROR                                                            
         CH    R1,=H'255'                                                       
         BH    ERROR                                                            
         STC   R1,INPLEN2          STORE SECOND LENGTH                          
         B     EDIT11                                                           
         SPACE                                                                  
EDIT6    TM    FLDH+4,X'04'        TEST FOR VALID ALPHA                         
         BZ    ERROR                                                            
         CLI   FLDH+5,3                                                         
         BH    ERROR                                                            
         ZIC   R6,FLDH+5                                                        
         GOTO1 VDAYVAL,DMCB,((R6),FLD),BYTE,DAYNO                               
         CLI   BYTE,0                                                           
         BE    ERROR                                                            
         CLI   INPDAY,0                                                         
         BNE   ERROR                                                            
         MVC   INPDAY,BYTE                                                      
         MVC   INPDAYNO,DAYNO                                                   
         B     EDIT11                                                           
         SPACE                                                                  
         CLI   FSTOP,C'='          ACTUAL AMOUNT                                
         BNE   EDIT11                                                           
*                                                                               
EDIT8    ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         EX    RE,ACTCOMP                                                       
         BNE   ERROR                                                            
         XC    FTERM,FTERM                                                      
         MVC   FTERM(1),=C','      EXTRACT SECOND LENGTH                        
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR NOTHING BETWEEN '*' AND ','         
         BNE   *+12                NO-SOMETHING THERE                           
         MVI   FERN,MISERR         YES-ERROR                                    
         B     ERROR                                                            
         SPACE                                                                  
*                                                                               
         BAS   RE,CHKDEFRT                                                      
         BAS   RE,CHKBUYRT                                                      
         ZIC   R0,FLDH+5           DATA LENGTH                                  
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
*                                                                               
         MVC   INPAMT,4(R1)                                                     
         OC    INPAMT,INPAMT                                                    
         BNZ   *+8                                                              
         MVI   INPZAMT,C'Y'                                                     
         B     EDIT11                                                           
         SPACE                                                                  
EDIT11   BCT   R0,EDIT5                                                         
*                                                                               
EDIT13   GOTO1 PROGCHK,INPST                                                    
         CLI   INPDAY,0            TEST FOR DAY OVERRIDE                        
         BNE   EDIT15              YES-USER INPUT ONE                           
*                                                                               
         L     RE,APROGEL                                                       
         USING NPGELEM,RE                                                       
         MVC   HALF(1),NPGDAY                                                   
         CLI   NPGROT,0                                                         
         BE    *+10                                                             
         MVC   HALF(1),NPGROT      IF ROTATION EXISTS USE THAT AS DAY           
*                                                                               
         LA    RE,DAYS             CHECK FOR SINGLE DAY OR ROTATION             
         LA    RF,DAYTAB                                                        
EDIT14   CLC   0(1,RF),HALF                                                     
         BE    EDIT15                                                           
         LA    RF,L'DAYTAB(RF)                                                  
         BCT   RE,EDIT14                                                        
         GOTO1 VGETDAY,DMCB,INPST,FULL                                          
         MVC   DAYNO,0(R1)         FOR A ROTATION, TAKE THE DATE                
         ZIC   R0,DAYNO            AS INPUT AND MAKE THE DAY AGREE              
         SLL   R0,4                WITH THE DATE                                
         STC   R0,INPDAYNO                                                      
         LA    R0,DAYS                                                          
         LA    RE,DAYTAB                                                        
         CLC   DAYNO,4(RE)         TEST DAY NUMBER VS. TABLE                    
         BE    *+14                                                             
         LA    RE,L'DAYTAB(RE)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         MVC   INPDAY,0(RE)        EXTRACT DAY BITS FROM TABLE                  
         DROP  RE                                                               
         SPACE                                                                  
EDIT15   GOTO1 CALEN,DMCB,INPST,(R3)                                            
         GOTO1 PROGCHK,DUB                                                      
         BAS   RE,DATCHK                                                        
         SR    R0,R0                                                            
         ICM   R0,1,INPNUM         NUMBER PER WEEK                              
         BNZ   *+8                                                              
         LA    R0,1                                                             
EDIT15A  GOTO1 ADDENTRY,DMCB,DUB,(R3)                                           
         BCT   R0,EDIT15A                                                       
         SPACE                                                                  
EDIT20   L     R2,FADDR                                                         
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    RE,MULTABH                                                       
         CR    R2,RE                                                            
         BL    EDIT1                                                            
         SPACE                                                                  
EDITX    CLI   UNITS,0                                                          
         BNE   EXXMOD                                                           
*                                                                               
         LA    R2,MULUNITH                                                      
         ST    R2,FADDR            SET ERROR CURSOR POSITION                    
         MVC   BUYMSG(L'NOUNITS),NOUNITS                                        
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A UNIT RECORD FOR EACH ENTRY IN SCHEDULE LIST            
*                                                                               
BUILD    NTR1                                                                   
         GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB                                                       
*                                                                               
         MVI   SEQNUM,1                                                         
         MVC   COUNT,UNITS         INITIALIZE COUNT OF UNITS                    
         LA    R2,SCHEDULE                                                      
         USING LISTD,R2                                                         
*                                                                               
         LA    R6,BLOCK            ** R6 = UNIT BLOCK **                        
         USING UNBLOCKD,R6                                                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,AIOAREA1                                                  
         MVC   UNALOCAL,AIOAREA2                                                
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'80'      TEST IF INTEGRATION TABLE LOOK UP            
         BNO   BUILD2                                                           
         DROP  RE                                                               
         LA    RF,INTGTBL                                                       
         ST    RF,UNINGTBL         PASS BACK INTG RATES HERE                    
         LA    RF,SVINTTBL                                                      
         ST    RF,INTHLDIT                                                      
         MVI   INTGREAD,0          DO 1ST READ                                  
         MVC   INTGAIO,AIOAREA3                                                 
         MVC   INTGSTDT,0(R2)                                                   
         ZIC   RE,UNITS                                                         
         BCTR  RE,0                                                             
         MH    RE,=H'13'                                                        
         AR    RE,R2                                                            
         MVC   INTGEDDT,0(RE)                                                   
         SPACE                                                                  
BUILD2   L     R4,AIOAREA1                                                      
         LR    RE,R4                                                            
         LA    RF,PAGELEN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING NURECD,R4                                                        
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,AGYMED                                                     
         MVC   NUKCLT,CLIPK                                                     
         MVC   NUKDATE,LISTDATE                                                 
         MVC   NUKNET,NET                                                       
         MVC   NUKPROG,PROG                                                     
         MVC   NUKEST,EST                                                       
         CLC   NUKDATE,LSTDATE     TEST FOR SAME DATE AS LAST UNIT              
         BE    *+12                YES                                          
         BAS   RE,GETSUB           NEXT SUB-LINE                                
         B     BUILD3                                                           
*                                                                               
         MVC   SVOLDDTE,NUKDATE                                                 
         ZIC   R1,LSTSUB           SAME DATE SO INCREMENT THE                   
         LA    R1,1(R1)            LAST SUB-LINE FOR DATE                       
         CH    R1,=H'255'                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   R1,NUKSUB                                                        
         SPACE                                                                  
BUILD3   MVC   LISTSUB,NUKSUB                                                   
         CLI   NUKSUB,SUBMAX       TEST SUB-LINE LIMIT BLOWN                    
         BNH   *+12                NO                                           
         MVI   FERN,SUBERR                                                      
         B     BUILDR                                                           
*                                                                               
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NUKDP,NPAKDP                                                     
         SPACE                                                                  
         GOTO1 VEDIT,DMCB,(C'I',(R6))                                           
         SPACE                                                                  
         GOTO1 VGETPROG,DMCB,NUKDATE                                            
         CLI   FERN,0                                                           
         BNE   BUILDR              ERROR ROUTINE                                
         SPACE                                                                  
         MVC   INTGSTDT,NUKDATE                                                 
         GOTO1 VEDIT,DMCB,(C'N',(R6))                                           
         CLI   UNERROR,0                                                        
         BE    *+14                                                             
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*                                                                               
         CLI   LISTZAMT,C'Y'       ZERO AMOUNT OVERRIDE                         
         BE    BUILD5                                                           
         OC    LISTAMT,LISTAMT     TEST IF USER OVERRODE ACTUAL AMOUNT          
         BZ    BUILD7              NO                                           
         MVC   NUACTUAL,LISTAMT                                                 
BUILD5   OI    NUUNITST,X'20'                                                   
*                                                                               
BUILD7   CLI   LISTDAY,0           TEST IF USER OVERRODE CALENDARIZING          
         BE    BUILD8              NO                                           
         MVC   NUDAY,LISTDAY                                                    
         MVC   NUKDATE,LISTDATE                                                 
         MVC   UNDATE,LISTDATE                                                  
         SPACE                                                                  
BUILD8   CLI   LISTLEN,0                                                        
         BE    BUILD9                                                           
         MVC   NULEN,LISTLEN                                                    
         SR    RE,RE                                                            
         ICM   RE,1,LISTLEN2                                                    
         BZ    BUILD9                                                           
         ZIC   RF,LISTLEN                                                       
         STC   RF,NULEN1                                                        
         AR    RE,RF                                                            
         STC   RE,NULEN                                                         
*                                                                               
BUILD9   GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',AIOAREA1),0                    
         CLI   12(R1),0            SET RATE INFO ON SECONDARY ELEMENT           
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         MVC   NUSDSRT,LISTRATE                                                 
         MVC   NUSDRTCV,LISTCVRG                                                
         DROP  RE                                                               
*                                                                               
*  GET DEMO INFO                                                                
*                                                                               
         GOTO1 VEDIT,DMCB,(C'D',(R6))                                           
         CLI   UNERROR,0                                                        
         BE    *+14                                                             
         MVC   FERN,UNERROR                                                     
         B     BUILDR                                                           
         GOTO1 (RF),(R1),(C'F',(R6))                                            
         SPACE                                                                  
BUILD10  CLC   NUKDATE,SVOLDDTE    SEE IF EDIT CHANGE DATE                      
         BE    *+8                                                              
         BAS   RE,GETSUB           YES REASSIGN SUB-LINE                        
*                                                                               
*  IF ACTION IS STDRAF BYPASS WRITTING THE RECORD                               
*  TO THE FILE. GO TO A ROUTINE TO CALCULATE THE                                
*  REQUESTED DEMOS TABLE THE OUTPUT AND EXIT.                                   
*                                                                               
         CLC   BUYACT(6),=CL6'STDRAF'                                           
         BE    BUILD100                                                         
*                                                                               
         GOTO1 AIO,DMCB,UNT+FILE+ADDREC,(R4)                                    
         MVC   UNITDA,NDXDA        SAVE RETURNED DISK ADDRESS                   
         MVC   LSTDATE,NUKDATE     SAVE UNIT DATE/SUB-LINE                      
         MVC   LSTSUB,NUKSUB                                                    
         GOTO1 VEDIT,DMCB,(C'P',(R6)),AIOAREA4                                  
         L     R3,AIOAREA4                                                      
         LA    R1,NDIRPTRS         N'PASSIVE POINTERS                           
         L     RE,NPTRS            N'POINTERS IN TABLE                          
         LR    RF,RE                                                            
         AR    RF,R1               UPDATE POINTER COUNT                         
         CH    RF,MAXPTRS          TEST FOR TABLE OVERFLOW                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         ST    RF,NPTRS                                                         
         MH    RE,=Y(NDIRLEN)      INDEX INTO POINTER TABLE                     
         LA    RE,PTRTAB(RE)       POINT TO NEXT ENTRY POSITION                 
         SPACE                                                                  
BUILD11  MVC   0(NDIRLEN,RE),0(R3) EXTRACT PASSIVE POINTER                      
         MVI   NDIRCTL(RE),0       ZERO CONTROL BYTE                            
         LA    RE,NDIRLEN(RE)      NEXT TABLE POSITION                          
         LA    R3,NDIRLEN(R3)      NEXT POINTER                                 
         BCT   R1,BUILD11                                                       
         SPACE                                                                  
* APR/2016: THIS CODE IS BEING COMMENTED OUT. THE BRAS INSTRUCTION              
*  HAD BEEN COMMENTED OUT AGES AGO, BUT THE BNE *+8 REMAINED. THAT              
*  COULD NOT POSSIBLY BE RIGHT, BECAUSE IT MEANT THAT WE WERE NOT               
*  INCREMENTING THE TABLE POINTER IN R2.                                        
*&&DO                                                                           
BUILD12  CLI   NUKSUB,2                                                         
         BNE   *+8                                                              
*&&                                                                             
*        BRAS  RE,SUBPRT                                                        
*                                                                               
         LA    R2,LISTNTRL(R2)                                                  
         ZIC   R0,COUNT                                                         
         SH    R0,=H'1'                                                         
         JM    *+2                 COUNTER HAS GONE NEGATIVE!                   
         BZ    BUILD15             ALL DONE                                     
         STC   R0,COUNT            UPDATE COUNTER                               
         B     BUILD2                                                           
         SPACE                                                                  
BUILD15  L     R2,NPTRS            SORT POINTERS IN DESCENDING SEQUENCE         
         GOTO1 VXSORT,DMCB,(X'FF',PTRTAB),(R2),NDIRLEN,L'NUKEY,0                
         LA    R3,PTRTAB           R3=POINTS TO PASSIVE POINTER TABLE           
         BAS   RE,NEWPTR           ADD NEW POINTER                              
         LA    R3,NDIRLEN(R3)      NEXT POINTER                                 
         BCT   R2,*-8                                                           
         B     EXXMOD                                                           
         SPACE 2                                                                
*                                                                               
*  IF ACTION IS STDRAF BYPASS WRITTING THE RECORD                               
*  TO THE FILE. GO TO A ROUTINE TO CALCULATE THE                                
*  REQUESTED DEMOS TABLE THE OUTPUT AND EXIT.                                   
*                                                                               
*                                                                               
* MOVE IN OVERRIDE BUY INFORMATION IF NEEDED                                    
BUILD100 CLI   STEWLENG,0                                                       
         BE    *+10                                                             
         MVC   NULEN,STEWLENG                                                   
         CLI   STEWDAY,0                                                        
         BE    *+10                                                             
         MVC   NUDAY,STEWDAY                                                    
         CLI   STEWTIML,0            IF FIELD < 0 NO INPUT                      
         BE    *+10                                                             
         MVC   NUTIME,STEWTIME                                                  
*                                                                               
         LA    RE,1                 DUPLICATE DATE COUNTER                      
         MVC   FULL(2),0(R2)        CURRENT DATE                                
BUILD110 LA    R2,LISTNTRL(R2)      GET NEXT DATE                               
         ZIC   R0,COUNT                                                         
         SH    R0,=H'1'                                                         
         JM    *+2                 COUNTER HAS GONE NEGATIVE!                   
         STC   R0,COUNT            UPDATE COUNTER                               
         CLI   COUNT,0                                                          
         BE    BUILD120            ALL DONE                                     
         CLC   FULL(2),0(R2)       ARE DATE THE SAME                            
         BNE   BUILD120            NO EXIT                                      
         LA    RE,1(RE)            ADD TO COUNTER                               
         B     BUILD110            GET NEXT DATE                                
*                                                                               
BUILD120 STC   RE,DUPDATES                                                      
         LA    R3,NEBLOCKA                                                      
         USING NEBLOCKD,R3                                                      
*                                                                               
*  CHECK DEMOS NEEDED FOR RETURN                                                
         OI    NBINDS9,NBI9GDM     REQUESTING GUARANTEES                        
         TM    STEWDSTA,X'20'      CHECK FOR NO GUARANTEES                      
         BZ    *+12                                                             
         MVI   NBDEMRAW,C'Y'        RETURN RAW DEMOS                            
         NI    NBINDS9,X'FF'-NBI9GDM                                            
*                                                                               
*  CHECK OVERRIDE EQUIVALENCING OPTIONS                                         
         TM    STEWDSTA,X'08'                                                   
         BZ    *+8                                                              
         MVI   NBUSER2,30                                                       
         TM    STEWDSTA,X'04'                                                   
         BZ    *+8                                                              
         MVI   NBUSER2,0                                                        
         TM    STEWDSTA,X'02'                                                   
         BZ    *+8                                                              
         MVI   NBUSER+1,30                                                      
         TM    STEWDSTA,X'01'                                                   
         BZ    *+8                                                              
         MVI   NBUSER+1,0                                                       
*                                                                               
         TM    STEWDSTA,X'10'        WAS ACTUAL DEMOS REQUESTED                 
         BZ    *+16                                                             
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBESTOPT,C'N'                                                    
         MVI   NBNOWRIT,C'N'                                                    
*                                                                               
         ST    R4,NBAIO                                                         
         MVI   NBFUNCT,NBFVAL                                                   
         L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         ST    RE,NBADEM                                                        
         MVI   NBESTOPT,C'M'                                                    
*  FIND DEMO PRECISSION                                                         
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',AIOAREA1),0                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         CLI   NUPOSTYP,C'S'                                                    
         BE    BUILD140                                                         
         CLI   NUPOSTYP,C'H'                                                    
         BE    BUILD140                                                         
         CLI   NUPOSTYP,C'N'                                                    
         BE    BUILD140                                                         
         MVI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         DROP  RE                                                               
*                                                                               
BUILD140 L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         LA    RF,1000                                                          
         XCEF                                                                   
*                                                                               
         L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         USING NETDEMOD,RE                                                      
         MVC   NDUSRNMS,ESTUSNS                                                 
         MVC   NDDEMOS,STEWDEMS                                                 
         LA    R1,25                                                            
         LA    RF,NDDEMOS                                                       
*                                                                               
BUILD160 CLI   0(RF),X'FF'                                                      
         BE    BUILD170                                                         
         LA    RF,3(RF)                                                         
         BCT   R1,BUILD160                                                      
BUILD170 MVI   0(RF),0                                                          
*                                                                               
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         BAS   RE,BLDRECS           BUILD STEWARD RECORDS                       
*                                                                               
         CLI   COUNT,0                                                          
         BNE   BUILD2              ALL DONE                                     
*  SAVE DATA                                                                    
         MVI   TSACTN,TSASAV                                                    
         BAS   RE,CALLTSAR                                                      
*  RETURN TSAR BLOCK TO THE STEWARD OVERLAY                                     
         GOTO1 VGLOBBER,DMCB,=C'PUTD',TSARBLK,48,GLVTSAR                        
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* ERROR EXIT                                                                    
*                                                                               
BUILDR   ZIC   R1,UNITS                                                         
         ZIC   R0,COUNT                                                         
         SR    R1,R0               UNITS BEFORE ERROR                           
         LA    R1,1(R1)            PLUS ONE FOR ERROR                           
         LA    R2,MULUNITH         NOW POSITION POINTER TO ERROR                
         BCTR  R1,0                FIELD HEADER                                 
         LTR   R1,R1                                                            
         BZ    BUILDR1                                                          
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,*-6                                                           
*                                                                               
BUILDR1  ST    R2,FADDR                                                         
         MVC   WORK(L'MULUNIT),8(R2)                                            
         MVC   8(L'MULUNIT,R2),SPACES                                           
         MVC   8(L'MULUNIT-1,R2),WORK                                           
         B     ERROR                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO BUILD RECORDS FOR STEWARD                                      
*                                                                               
BLDRECS  NTR1                                                                   
         LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         L     R4,AIOAREA4                                                      
         USING DRAFRECD,R4                                                      
*                                                                               
         MVC   DRFSEQN,SEQNUM                                                   
         ZIC   RE,SEQNUM                                                        
         LA    RE,1(RE)                                                         
         STCM  RE,1,SEQNUM                                                      
*                                                                               
         MVC   DRFNUM,DUPDATES                                                  
         MVC   DRFDATE,NBACTDAT                                                 
         MVC   DRFDAY(1),NBDAY                                                  
         MVC   DRFTIME,NBTIME                                                   
         MVC   DRFLEN,NBLEN                                                     
         MVC   DRFROT,NBSDROT                                                   
         MVC   DRFDPT,NBACTDP                                                   
         MVC   DRFSQTR,NBACTSQH                                                 
         MVC   DRFINT,NBINTEG                                                   
         MVC   DRFPNAM,NBPROGNM                                                 
         MVC   DRFNTI,NBNTI                                                     
         MVC   DRFMEDTP,NBSTATYP                                                
         MVC   DRFPSTTP,NBPOSTYP                                                
* DEMO GET PRECISION                                                            
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'5D',AIOAREA1),0                    
         CLI   12(R1),0            SET RATE INFO ON SECONDARY ELEMENT           
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         L     RE,12(R1)                                                        
         CLC   5(2,RE),=XL2'5901'   2 DECIMAL PRECISSION                        
         BL    *+8                                                              
         OI    DRFSTAT,X'40'        2 DECIMAL INDICATOR                         
* HOMES DEMOS                                                                   
         MVC   DRFHOMSH,NBESTSHR                                                
         MVC   DRFHOMHT,NBESTHUT                                                
         MVC   DRFHOMRT,NBESTHOM+2                                              
         MVC   DRFHOMIM,NBESTHOM+4                                              
*                                                                               
         XC    DRFRESLT,DRFRESLT                                                
         TM    STEWDSTA,X'10'      WAS ACTUAL DEMOS REQUESTED                   
         BZ    BLDR40                                                           
         MVC   DRFHOMSH,NBACTSHR                                                
         MVC   DRFHOMHT,NBACTHUT                                                
         MVC   DRFHOMRT,NBACTHOM+2                                              
         MVC   DRFHOMIM,NBACTHOM+4                                              
* MOVE OUT THE RESULT CODE (ACTUAL DEMOS ONLY)                                  
         CLI   NBRESULT,0                                                       
         BE    *+10                                                             
         MVC   DRFRESLT(1),NBRESULT                                             
*--MOVE CABLE LEVEL INFO                                                        
         CLI   NBPOSTYP,C'C'                                                    
         BNE   BLDR40                                                           
         CLI   NBUSER2+5,C'Y'                                                   
         BE    BLDR40                                                           
         CLI   NBUSER2+5,C'N'                                                   
         BE    BLDR40                                                           
         CLI   NBUSER2+5,X'40'                                                  
         BNH   BLDR40                                                           
         MVC   DRFRESLT+1(1),NBUSER2+5                                          
         DROP  R6                                                               
*                                                                               
BLDR40   GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'60',AIOAREA1),(1,=C'F')            
         CLI   12(R1),0                                                         
         BNE   BLDR60                                                           
         L     RE,12(R1)                                                        
         USING NUOTH,RE                                                         
         MVC   DRFBTYP,NUOTHER                                                  
         DROP  RE                                                               
*                                                                               
BLDR60   L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         USING NETDEMOD,RE                                                      
         MVC   DRFDEMS,NDESTDEM                                                 
*                                                                               
         TM    STEWDSTA,X'10'      WAS ACTUAL DEMOS REQUESTED                   
         BZ    *+10                                                             
         MVC   DRFDEMS,NDACTDEM     MOVE OUT ACTUAL DEMOS                       
****************************************                                        
BLDR120  LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         TM    STEWDST2,X'07'       CHECK 2 SETS OF DEMOS                       
         BZ    BLDR400                                                          
         MVI   DRFDEMO2,C'Y'                                                    
*                                                                               
         TM    STEWDST2,X'01'       CHECK ACTUALS                               
         BZ    *+20                                                             
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBESTOPT,C'N'                                                    
         MVI   NBDEMRAW,C'N'                                                    
         B     BLDR180                                                          
*                                                                               
         TM    STEWDST2,X'02'       ESTIMATED RAW                               
         BZ    *+20                                                             
         MVI   NBACTOPT,C'N'                                                    
         MVI   NBESTOPT,C'Y'                                                    
         MVI   NBDEMRAW,C'Y'                                                    
         B     BLDR180                                                          
*                                                                               
         TM    STEWDST2,X'04'       ESTIMATED GUARANTEES                        
         BZ    BLDR180                                                          
         MVI   NBACTOPT,C'N'                                                    
         MVI   NBESTOPT,C'Y'                                                    
         MVI   NBDEMRAW,C'N'                                                    
         OI    NBINDS9,NBI9GDM     REQUESTING GUARANTEES                        
         DROP  RE                                                               
*                                                                               
*  CALL NETVALUE GET SECOND SET OF DEMOS                                        
BLDR180  GOTO1 VNETVAL,DMCB,NEBLOCKD                                            
*                                                                               
         L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         USING NETDEMOD,RE                                                      
*                                                                               
         TM    STEWDST2,X'01'       WAS ACTUAL DEMOS REQUESTED                  
         BO    BLDR200                                                          
* HOMES DEMOS ESTIMATED                                                         
         MVC   DRFHOMS2,NBESTSHR                                                
         MVC   DRFHOMH2,NBESTHUT                                                
         MVC   DRFHOMR2,NBESTHOM+2                                              
         MVC   DRFHOMI2,NBESTHOM+4                                              
         MVC   DRFDEMS2,NDESTDEM                                                
         B     BLDR400                                                          
* HOMES DEMOS ACTUAL                                                            
BLDR200  MVC   DRFHOMS2,NBACTSHR                                                
         MVC   DRFHOMH2,NBACTHUT                                                
         MVC   DRFHOMR2,NBACTHOM+2                                              
         MVC   DRFHOMI2,NBACTHOM+4                                              
         MVC   DRFDEMS2,NDACTDEM     MOVE OUT ACTUAL DEMOS                      
         DROP  RE,R6                                                            
*                                                                               
*  WRITE TO TSAR                                                                
BLDR400  MVI   TSACTN,TSAADD                                                    
         L     R0,AIOAREA4                                                      
         ST    R0,TSAREC                                                        
         BAS   RE,CALLTSAR                                                      
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO GET NEXT SUB-LINE NUMBER                                       
*                                                                               
GETSUB   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         L     R1,AIOAREA1                                                      
         MVI   NUKPTYPE,X'84'      USE PASSIVE KEY TO FIND NEXT NUMBER          
         MVC   NUKPAM,NUKAM-NUKEY(R1)                                           
         MVC   NUKPCLT,NUKCLT-NUKEY(R1)                                         
         MVC   NUKPNET,NUKNET-NUKEY(R1)                                         
         MVC   NUKPPROG,NUKPROG-NUKEY(R1)                                       
         MVC   NUKPDATE,NUKDATE-NUKEY(R1)                                       
         MVC   NUKPEST,NUKEST-NUKEY(R1)                                         
         LA    R0,UPDATE+PASSDEL+UNT+DIR+HIGH                                   
         SPACE                                                                  
GETSUB2  GOTO1 AIO,DMCB,(R0)                                                    
         CLC   KEY(NUKPSUB-NUKPKEY),KEYSAVE                                     
         BNE   GETSUB4                                                          
         LA    R0,UPDATE+PASSDEL+UNT+DIR+SEQ                                    
         B     GETSUB2                                                          
         SPACE                                                                  
GETSUB4  LA    R4,KEYSAVE                                                       
         ZIC   R1,NUKPSUB                                                       
         LA    R1,1(R1)                                                         
         L     R4,AIOAREA1         POINT BACK TO RECORD                         
         USING NURECD,R4                                                        
         STC   R1,NUKSUB                                                        
         B     EXXMOD                                                           
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR NEW POINTERS (AT ENTRY, R3 ADDRESSES POINTER)                 
*                                                                               
NEWPTR   ST    RE,SAVEREG                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R3)  READ FOR NEW POINTER                         
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+DIR+HIGH                             
         CLC   KEY(L'NUKEY),KEYSAVE TEST IF KEY FOUND                           
         BE    NEWPTR2                                                          
         MVC   KEY(NDIRLEN),0(R3)  RESET ENTIRE POINTER                         
         BAS   RE,SETSTAT          SET STATUS BYTE                              
         GOTO1 (RF),(R1),UNT+DIR+ADD                                            
         B     NEWPTRX                                                          
         SPACE                                                                  
NEWPTR2  MVC   KEY(NDIRLEN),0(R3)                                               
         BAS   RE,SETSTAT          SET STATUS BYTE                              
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         SPACE                                                                  
NEWPTRX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
*--SET POSTING TYPE IN THE STATUS FIELD                                         
SETSTAT  LA    R1,NEBLOCKA                                                      
         USING NEBLOCKD,R1                                                      
         CLI   NBPOSTYP,C'N'       NETWORK                                      
         BE    SETSTEX                                                          
         CLI   NBPOSTYP,C'C'       CABLE                                        
         BNE   *+12                                                             
         OI    KEY+20,X'01'                                                     
         B     SETSTEX                                                          
         CLI   NBPOSTYP,C'S'       SYNDICATION                                  
         BNE   *+12                                                             
         OI    KEY+20,X'02'                                                     
         B     SETSTEX                                                          
         OI    KEY+20,X'03'        OTHER                                        
SETSTEX  BR    RE                                                               
         DROP  R1                                                               
*                                                                               
         EJECT                                                                  
* MODULE AND ROUTINE EXIT                                                       
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ACTERR    GOTO1 VERROR                                                          
         EJECT                                                                  
* ERROR - SET ERROR MESSAGE AND EXIT                                            
*                                                                               
* ON ENTRY                                                                      
*        FADDR = A(FIELD HEADER OF FIELD IN ERROR)                              
*        FERN  = SYSTEM ERROR NUMBER OR X'FF' FOR USER MESSAGE                  
*        FNDX  = OPTIONALLY SET FIELD INDEX FOR MULTI-FIELD TWA FIELD           
*        XTRA  = OPTIONALLY CONTAINS EXTRA MESSAGE CONCATENATED TO              
*                SYSTEM ERROR MESSAGE                                           
*                                                                               
ERROR    GOTO1 VERROR                                                           
***ERROR    CLI   FERN,USERERR        TEST FOR USER MESSAGE                     
***         BE    ERROR6                                                        
***         GOTO1 VGETMSG,DMCB+12,(FERN,BUYMSG),(X'FF',DMCB),(7,0)              
***         LA    R2,BUYMSG+L'BUYMSG-1 R2 POINTS TO END OF MSG FLD              
***         LA    R3,L'BUYMSG         CALCULATE MESSAGE LENGTH                  
***         CLI   0(R2),C' '          TEST FOR BLANK                            
***         BNE   *+10                                                          
***         BCTR  R2,0                BACK UP POINTER                           
***         BCT   R3,*-10                                                       
***         LA    R2,1(R2)            POINT TO BLANK AFTER LAST CHAR            
****                                                                            
***ERROR2   CLI   FNDX,0              TEST FOR INDEX NUMBER                     
***         BE    ERROR4                                                        
***         LA    R0,L'BUYMSG                                                   
***         LA    R3,8(R3)            ADD ON LENGTH OF INDEX MSG +1             
***         CR    R3,R0               TEST FOR FIT IN FIELD                     
***         BH    ERROR6              NO - EXIT                                 
***         LA    R2,BUYMSG-7(R3)                                               
***         MVC   0(7,R2),=C'- FLD#N'                                           
***         EDIT  (B1,FNDX),(1,6(R2))                                           
***         LA    R2,7(R2)            POINT TO BLANK AFTER INDEX MSG            
****                                                                            
***ERROR4   CLC   XTRA,SPACES         TEST FOR ANY EXTRA MESSAGE                
***         BE    ERROR6                                                        
***         LA    RE,XTRA+L'XTRA-1                                              
***         LA    R1,L'XTRA           CALCULATE LENGTH OF EXTRA MESSAGE         
***         CLI   0(RE),C' '                                                    
***         BNE   *+10                                                          
***         BCTR  RE,0                                                          
***         BCT   R1,*-10                                                       
****                                                                            
***         LA    R0,L'BUYMSG                                                   
***         LA    R3,1(R1,R3)         COMPUTE TOTAL MESSAGE LENGTH              
***         CR    R3,R0               TEST FOR FIT                              
***         BH    ERROR6                                                        
***         BCTR  R1,0                LESS ONE FOR EXECUTE                      
***         EX    R1,*+8              MOVE XTRA TO MESSAGE FIELD                
***         B     ERROR6                                                        
***         MVC   1(0,R2),XTRA        EXECUTED                                  
****                                                                            
***ERROR6   L     R2,FADDR            SET CURSOR                                
***         OI    6(R2),X'C0'         CURSOR AND TRANSMIT BITS                  
***         L     RD,AWORK            RESTORE ROOT'S RD                         
***         DC    H'0',C'$ABEND'      UNWIND THE IO'S                           
         EJECT                                                                  
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
UNTFILE  DC    CL8'UNTFILE'                                                     
MAXPTRS  DC    Y(FIELDS*NDIRPTRS)  MAX ENTRIES IN POINTER TABLE                 
TOOMANY  DC    C'** ERROR - TOO MANY UNITS TO FIT ON SCREEN **'                 
NOUNITS  DC    C'** ERROR - NO UNITS TO GENERATE ON SCREEN **'                  
INVCOST  DC    C'** ERROR - BAD ACTUAL COST INPUT **'                           
         SPACE 2                                                                
* TABLE OF WEEK INDICATORS AND THE NUMBER OF DAYS OF INTERVAL                   
*                                                                               
WEEKTAB  DS    0XL2                                                             
         DC    AL1(EVERY),AL1(7)                                                
         DC    AL1(ALT),AL1(14)                                                 
         DC    AL1(THIRD),AL1(21)                                               
         DC    AL1(FOURTH),AL1(28)                                              
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE OF DAY BITS AND THEIR CHARACTER REPRESENTATIONS                         
*                                                                               
DAYTAB   DS    0CL5                                                             
         DC    X'40',C'MON',X'01'                                               
         DC    X'20',C'TUE',X'02'                                               
         DC    X'10',C'WED',X'03'                                               
         DC    X'08',C'THU',X'04'                                               
         DC    X'04',C'FRI',X'05'                                               
         DC    X'02',C'SAT',X'06'                                               
         DC    X'01',C'SUN',X'07'                                               
DAYS     EQU   (*-DAYTAB)/L'DAYTAB                                              
         SPACE 2                                                                
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
         DROP  RB,RA                                                            
* SUB-ROUTINE TO UPDATE FIRST UNIT FOR DATE WHEN SECOND UNIT IS ADDED           
*                                                                               
SUBPRT   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,AGYMED                                                    
         MVC   NUKPCLT,CLIPK                                                    
         MVC   NUKPNET,NET                                                      
         MVC   NUKPPROG,PROG                                                    
         MVC   NUKPDATE,NUKDATE-NUKEY(R1)                                       
         MVC   NUKPEST,EST                                                      
         MVI   NUKPSUB,1            SUB-LINE 1                                  
         L     R1,AIOAREA1                                                      
         MVC   NUKPDP,NUKDP-NUKEY(R1) EXTRACT DAYPART                           
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(NUKPDP-NUKPKEY+1),KEYSAVE                                    
         BNE   SUBPRTX             COULD NOT FIND IT-DELETED                    
         SPACE                                                                  
SUBPRT2  L     R4,AIOAREA4         GET THE RECORD                               
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,(R4)                                
         USING NURECD,R4                                                        
         MVI   NUSUBPRT,1                                                       
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         SPACE                                                                  
SUBPRTX  XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* MULTIPLE BUY SCREEN                                                           
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYFAD                                                       
         SPACE 2                                                                
FIELDS   EQU   (MULTABH-MULUNITH)/(L'MULUNITH+L'MULUNIT)                        
         SPACE 2                                                                
* PASSIVE POINTER TABLE (MAX UNITS*L'DIRECTORY ENTRY*N'PASSIVE PTRS.)           
*                                                                               
         ORG   TWAD+PAGELEN                                                     
PTRTAB   DS    (FIELDS*NDIRLEN*NDIRPTRS)X                                       
         EJECT                                                                  
* DSECT TO COVER INPUT LIST ENTRIES                                             
*                                                                               
INPD     DSECT                                                                  
INPST    DS    CL6                 START DATE (YYMMDD)                          
INPEND   DS    CL6                 END DATE (YYMMDD)                            
INPWEEKS DS    X                   NUMBER OF WEEKS (NWEEKS FORMAT)              
INPIND   DS    X                   WEEKS INDICATOR (E.G. ALTERNATE)             
INPDAY   DS    X                   OVERRIDE DAY                                 
INPDAYNO DS    X                   OVERRIDE DAY NUMBER                          
INPLEN   DS    X                   OVERRIDE LENGTH                              
INPLEN2  DS    X                   OVERRIDE LENGTH (2)                          
INPNUM   DS    X                   NUMBER PER WEEK OVERRIDE                     
INPCTL   DS    X                   INPUT SYNTAX                                 
INPAMT   DS    XL4                 ACTUAL AMOUNT                                
INPZAMT  DS    X                   ZERO AMOUNT INPUTTED                         
INPRATE  DS    X                   RATE TYPE INPUTTED                           
INPCVRG  DS    X                   RATE COVERAGE INPUTTED                       
INPNTRL  EQU   *-INPD                                                           
         SPACE 2                                                                
* DSECT TO COVER SCHEDULE LIST ENTRIES                                          
*                                                                               
LISTD    DSECT                                                                  
LISTDATE DS    XL2                 UNIT DATE (COMPRESSED)                       
LISTSUB  DS    X                   SUB-LINE NUMBER                              
LISTDAY  DS    X                   OVERRIDE DAY                                 
LISTLEN  DS    X                   OVERRIDE LENGTH                              
LISTLEN2 DS    X                   OVERRIDE LENGTH (2)                          
LISTAMT  DS    XL4                 OVERRIDE ACTUAL AMOUNT                       
LISTZAMT DS    X                   ZERO AMOUNT INPUTTED                         
LISTRATE DS    X                   RATE TYPE                                    
LISTCVRG DS    X                   RATE COVERAGE                                
LISTNTRL EQU   *-LISTD             ENTRY LENGTH                                 
         SPACE 2                                                                
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
STEWDEMS DS    CL75                 STEWARD REQUEST DEMOS (FROM BASE)           
STEWDATE DS    CL104                STEWARD REQUEST DATES (FROM BASE)           
STEWTIME DS    CL4                  STEWARD REQUEST TIMES                       
STEWTIML DS    CL1                  TIME INPUT INDICATOR                        
STEWDAY  DS    CL1                  STEWARD REQUEST DAYS                        
STEWLENG DS    CL1                  STEWARD REQUEST LENGTH                      
STEWDSTA DS    CL1                  STEWARD DEMO STATUS BITS                    
*                                   X'20' = DEMOS ESTIMATE NO GUARANTEE         
*                                   X'10' = DEMOS ACTUAL                        
*                                   X'08' = RTG 30 SECOND EQUIV                 
*                                   X'04' = RTG NO EQUIV                        
*                                   X'02' = IMP 30 SECOND EQUIV                 
STEWDST2 DS    CL1                  SECOND STATUS BYTE                          
*                                   X'01' = ACTUALS COLUMN 2                    
*                                   X'02' = ESTIMATES COLUMN 2                  
*                                   X'04' = EST GUARANTEE COLUMN 2              
TSARBLK  DS    6D                   STORAGE BLOCK FOR TSAR                      
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
VEDIT    DS    V                   V(GENERAL UNIT EDIT)                         
UNITDA   DS    XL4                 SAVED DISK ADDRESS                           
*                                                                               
NPTRS    DS    F                   PASSIVE POINTER COUNTER                      
UNITS    DS    X                   SCHEDULE LIST ENTRY COUNT                    
COUNT    DS    X                                                                
INPNTRY  DS    X                   INPUT LIST ENTRY COUNT                       
WEEKS    DS    X                                                                
DAYNO    DS    X                                                                
SEQNUM   DS    X                                                                
DAYB     DS    B                   DAY BITS                                     
DAILYFLG DS    C                   Y=DAILY BUYING                               
LASTDATE DS    CL6                 CONTROL FOR DATE SEQUENCE                    
STDATE   DS    CL6                                                              
ENDATE   DS    CL6                                                              
*                                                                               
SVOLDDTE DS    XL2                 SAVE DATE BEFORE TRANVERSING EDIT            
LSTDATE  DS    XL2                 LAST UNIT'S DATE                             
LSTSUB   DS    X                   LAST UNIT'S SUB-LINE                         
*                                                                               
DUPDATES DS    X                   DUPLICATE DATE COUNTER                       
*                                                                               
ENTRY    DS    XL(INPNTRL)         DUMMY INPUT ENTRY (EDIT ROUTINE)             
BLOCK    DS    CL256               GENERAL WORK AREA                            
SCOUT    DS    CL60                SCREEN OUTPUT AREA                           
SVINTTBL DS    CL70                SAVED DKA FOR INTEG RECORDS                  
SCHEDULE DS    (FIELDS)XL(LISTNTRL)                                             
TEMPDEND EQU   *-TEMPD                                                          
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
EVERY    EQU   X'80'                                                            
ALT      EQU   X'40'                                                            
THIRD    EQU   X'20'                                                            
FOURTH   EQU   X'10'                                                            
SINGLE   EQU   X'01'                                                            
NWEEKS   EQU   X'02'                                                            
DATE     EQU   X'04'                                                            
DAILY    EQU   X'08'                                                            
LPAREN   EQU   C'('                                                             
RPAREN   EQU   C')'                                                             
         SPACE 2                                                                
* NETDEMOD (DSECT COVERING NETWORK DEMO BLOCK)                                  
*                                                                               
         PRINT OFF                                                              
NETDEMOD DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
* NETDEMOE (DSECT COVERING NETWORK DEMO BLOCK SUPPORTS 50 DEMOS)                
*                                                                               
NETDEMOE DSECT                                                                  
       ++INCLUDE NETDEMOP                                                       
*                                                                               
       ++INCLUDE NAVDSECTS                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076NEBUY44   07/09/19'                                      
         END                                                                    
