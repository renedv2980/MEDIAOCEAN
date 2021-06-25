*          DATA SET SPREPZ102  AT LEVEL 074 AS OF 04/01/03                      
*PHASE SPZ102A                                                                  
*INCLUDE COVAIL                                                                 
         TITLE 'SPREPZ102 - CLIENT SUMMARY REPORT'                              
         PRINT NOGEN                                                            
SPZ102   CSECT                                                                  
         NMOD1 0,SPZ102,RA                                                      
*                                                                               
         L     R8,0(R1)                                                         
         LA    R9,2048(R8)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,R8,R9                                                    
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SI100                                                            
         CLI   MODE,CLTFRST                                                     
         BE    SP100                                                            
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* RUN FIRST                                                                     
*                                                                               
SI100    L     R6,=A(BUFFALOC)     GET BUFFALO BUFFER                           
         GOTO1 COVAIL,DMCB,C'SETB',100000,0,(R6)                                
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFBUFF,DMCB+12                                                 
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF     INIT BUFFALO                   
         SPACE                                                                  
* READ AGENCY'S AND BUILD 2 BYTE AGY TABLE FOR PRINTING                         
         XC    KEY,KEY                                                          
         MVI   KEY,6               AGY KEY                                      
         GOTO1 HIGH                                                             
         B     SI220                                                            
*                                                                               
SI200    GOTO1 SEQ                                                              
SI220    CLI   KEY,6                                                            
         BNE   EXIT                                                             
*                                                                               
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         LR    R2,R6                                                            
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   SI200                                                            
*                                                                               
         ZIC   RE,3(R6)            MOVE IN AGY/MED BYTE                         
         SRL   RE,4                JUST AGY                                     
         AR    RE,RE                                                            
         LA    RE,AGYTBL-2(RE)                                                  
         MVC   0(2,RE),1(R2)       MOVE IN 2 CHAR AGY                           
         B     SI200                                                            
         EJECT                                                                  
*                                                                               
* REQUEST FIRST                                                                 
*                                                                               
         SPACE                                                                  
* READ CLIENT HEADERS                                                           
SP100    XC    SVCLTKY,SVCLTKY                                                  
         CLI   QOPT1,C'Y'                                                       
         BNE   SP120                                                            
         MVC   SVCLTKY+1(1),BAGYMD                                              
SP120    CLC   QCLT,=C'ALL'                                                     
         BE    SP200                                                            
         GOTO1 CLPACK,DMCB,QCLT,SVCLTKY+2                                       
         B     SP220                                                            
         SPACE                                                                  
* RETURN HERE TO READ NEXT AGY/MEDIA CLIENT                                     
SP200    CLC   QCLT,=C'ALL'                                                     
         BNE   PREND                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(4),SVCLTKY                                                   
         MVC   KEY+4(2),=H'-1'     HIGHEST POSSIBLE PRODUCT                     
SP220    GOTO1 HIGH                                                             
         CLI   KEY,0                                                            
         BNE   PREND                                                            
         ZIC   RE,KEY+1            GET CURRENT AGENCY                           
         SRL   RE,4                REMOVE MEDIA                                 
         AR    RE,RE               X 2                                          
         LA    RE,AGYTBL-2(RE)     ADD IN TABLE ADDRESS                         
         MVC   CURAGY,0(RE)                                                     
         MVC   SVCLTKY,KEY         SAVE LAST CLIENT                             
         OC    KEY+4(2),KEY+4      MUST BE A NEW A/M CLIENT                     
         BNZ   SP200                                                            
         GOTO1 GETCLT                                                           
         L     R6,ADCLT                                                         
         LA    R6,CLIST-CLTHDR(R6)                                              
         ST    R6,CURPRD                                                        
         LA    R7,880(R6)                                                       
         ST    R7,ENDPDLST                                                      
         B     SP320                                                            
         SPACE                                                                  
* RETURN HERE TO GET NEXT PRODUCT                                               
SP300    L     R6,CURPRD                                                        
         LA    R6,4(R6)            NEXT PRODUCT                                 
SP320    CLI   0(R6),0             LAST PRODUCT IN LIST                         
         BE    SP200                                                            
         C     R6,ENDPDLST         END OF LIST                                  
         BNL   SP200                                                            
*                                                                               
         MVI   NEWPRDSW,0          NEW PRODUCT                                  
         ST    R6,CURPRD                                                        
         CLC   0(3,R6),=C'AAA'                                                  
         BE    SP300                                                            
         CLC   0(3,R6),=C'POL'                                                  
         BE    SP300               PROCESS THIS PRODUCT                         
         SPACE                                                                  
* SET UP KEY TO READ 1ST STATION BILL RECORD                                    
SP400    XC    KEY(13),KEY                                                      
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(1),SVCLTKY+1                                               
         MVC   KEY+3(2),SVCLTKY+2                                               
         ZIC   R0,3(R6)                                                         
         STC   R0,KEY+5            PRD FROM PRD LIST                            
         GOTO1 HIGH                                                             
         MVI   OUTOFSTB,1          NO STATION BILL RECORDS                      
         CLC   KEY(6),KEYSAVE                                                   
         BNE   SP420                                                            
         MVI   OUTOFSTB,0                                                       
         MVC   NXTSTBKY,KEY        SAVE KEY FOR COMPARES                        
         SPACE                                                                  
* NOW READ FOR 1ST ESTIMATE/EST BILL                                            
SP420    XC    KEY(13),KEY                                                      
         MVC   KEY+1(1),SVCLTKY+1                                               
         MVC   KEY+2(2),SVCLTKY+2                                               
         L     R6,CURPRD                                                        
         MVC   KEY+4(3),0(R6)                                                   
         MVI   KEY+7,1             1ST ESTIMATE                                 
         GOTO1 HIGH                                                             
         MVI   OUTOFESB,1          NO ESTIMATE BILL RECORDS                     
         CLC   KEY(7),KEYSAVE                                                   
         BNE   SP500                                                            
*                                                                               
         MVI   OUTOFESB,0                                                       
         MVC   NXTESBKY,KEY        SAVE KEY FOR COMPARES                        
         EJECT                                                                  
* CHECK FOR ANY MORE EST/EST BILLS/STATION BILLS FOR THIS PRD                   
SP500    CLI   OUTOFSTB,1          ANY STATION BILLS LEFT?                      
         BNE   SP520               - YES                                        
         CLI   OUTOFESB,1          ANY ESTIMATE BILLS LEFT?                     
         BE    SP300               - YES                                        
         B     SP540               NO STA BILLS JUST PROCESS EST BILLS          
*                                                                               
SP520    MVI   NOSTBEST,0          0 - IF STA & EST BILLS                       
         CLI   OUTOFESB,0          ANY ESTIMATE BILLS LEFT?                     
         BNE   SB100               - NO JUST STA BILLS                          
*                                                                               
         CLC   NXTSTBES,NXTESBES   IF STA LOWER PROCESS IT                      
         BL    SB100                                                            
         BE    SP540                                                            
         MVI   NOSTBEST,1          NO STA BILLS                                 
*                                                                               
SP540    MVI   NOEST,0             HAVE AN EST                                  
         MVI   EDEST,0             NO ESTIMATE # IN CASE JUST ESTIMATE          
         CLI   NXTESBYS,0          IF NOT 0 THAN NO EST EXISTS                  
         BE    EB100                                                            
         MVI   NEWESTSW,0          DON,T ATTEMPT TO READ EST                    
         MVC   EDEST,NXTESBES                                                   
         MVI   NOEST,1             NO ESTIMATE                                  
         MVC   KEY(13),NXTESBKY    LOOK FOR EST BILL RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EB220               PROGRAM SAID I HAD MORE EST BILLS            
         DC    H'0'                                                             
         EJECT                                                                  
* PROCESS ESTIMATE BILL RECORDS FOR 1 ESTIMATE                                  
EB100    MVC   KEY(13),NXTESBKY    LOOK FOR EST BILL RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+6                 PROGRAM SAID I HAD MORE EST                  
         DC    H'0'                                                             
*                                                                               
         XC    BFKEY,BFKEY                                                      
         ZAP   BFBGRS,=P'0'                                                     
         ZAP   BFBNET,=P'0'                                                     
         ZAP   BFSBGRS,=P'0'                                                    
         ZAP   BFSBNET,=P'0'                                                    
*                                                                               
EB200    GOTO1 SEQ                 NEXT STA BILL                                
EB210    CLC   KEY(8),KEYSAVE      AN EST BREAK?                                
         BNE   EB500               - YES                                        
EB220    CLC   =X'5300',KEY+8      CHECK FOR JAN83 OR LATER                     
         BNH   EB240                                                            
         MVC   KEY+8(2),=X'5300'                                                
         XC    KEY+10(3),KEY+10    CLEAR REST OF KEY                            
         GOTO1 HIGH                                                             
         B     EB210                                                            
EB240    MVC   NEWESTSW,KEY+7                                                   
         MVC   BFKEY,KEY+8                                                      
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         USING ESBHDRD,R6                                                       
         GOTO1 GET                                                              
         CLI   BRETAIL,X'41'       DETAIL SUMMARY BILL - IGNORE IT              
         BE    EB200                                                            
*                                                                               
         AP    BFBGRS,BGRSP        ADD IN CASE OF CORPORATE BILL                
         AP    BFBNET,BNETP                                                     
         CLI   BRETAIL,0           NORMAL BILL                                  
         BE    EB300                                                            
         CLI   BRETAIL,X'01'       CORPORATE - ADD TO BUFFALO                   
         BE    EB300                                                            
         CLI   BRETAIL,X'81'       CORPORATE - ADD TO BUFFALO                   
         BE    EB300                                                            
         MVI   CORBIL,1            MAKE SURE YOU GET A COR BILL                 
         B     EB200               READ NEXT BILL                               
*                                                                               
EB300    BAS   RE,PUTBUFF                                                       
*                                                                               
         XC    BFKEY,BFKEY                                                      
         ZAP   BFBGRS,=P'0'                                                     
         ZAP   BFBNET,=P'0'                                                     
         ZAP   BFSBGRS,=P'0'                                                    
         ZAP   BFSBNET,=P'0'                                                    
         MVI   CORBIL,0                                                         
         B     EB200                                                            
         SPACE                                                                  
* END OF 1 ESTIMATES BILLS                                                      
EB500    CLI   CORBIL,0                                                         
         BE    *+8                                                              
         BAS   RE,PUTBUFF          DISTBUTION - NO COR BILL                     
*                                                                               
         CLC   KEY(7),KEYSAVE                                                   
         BE    EB520                                                            
         MVI   OUTOFESB,1                                                       
         B     SB100                                                            
*                                                                               
EB520    MVC   NXTESBKY,KEY                                                     
         EJECT                                                                  
* NOW PROCESS STATION BILL RECORDS                                              
SB100    CLI   OUTOFSTB,1          NO STA BILLS FOR THIS PRD                    
         BE    PR100                                                            
         CLI   NOSTBEST,1          NO STA BILL FOR THIS EST                     
         BE    PR100                                                            
         MVC   KEY(13),NXTSTBKY    RE-READ 1ST STA BILL                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     MUST BE SAME                                 
         BE    SB220                                                            
         DC    H'0'                                                             
*                                                                               
SB200    GOTO1 SEQ                                                              
         CLC   KEY(7),KEYSAVE      LAST STA BILL THIS EST                       
         BNE   SB500               - YES                                        
*                                                                               
SB220    MVC   NEWESTSW,KEY+6                                                   
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         USING STBHDRD,R6                                                       
         GOTO1 GET                                                              
         MVI   ELCODE,X'0E'                                                     
         BAS   RE,GETEL                                                         
         B     SB260                                                            
*                                                                               
SB240    BAS   RE,NEXTEL                                                        
SB260    BNE   SB200                                                            
         CLC   =X'5300',STABPER-STABELEM(R6)                                    
         BH    SB240               ONLY EXCEPT IF AFTER JAN83                   
         SPACE                                                                  
* EITHER ADD OR CREATE A BUFFALO RECORD                                         
         MVC   BFYS(2),STABPER-STABELEM(R6)                                     
         LH    R2,STABBDT-STABELEM(R6)                                          
         SLL   R2,16               CLEAR HIGH ORDER BITS                        
         SRDL  R2,25               GET MONTH/DAY IN R3 (9 BITS)                 
         SH    R2,=H'80'           BASE YEAR                                    
         SLDL  R2,4                SET UP YYMM DATE                             
         STC   R2,BFBM                                                          
         MVC   BFINV,STABINV-STABELEM(R6)                                       
         NI    BFINV,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)                  
         ZAP   BFBGRS,=P'0'        CLEAR BILL DOLLARS                           
         ZAP   BFBNET,=P'0'                                                     
         ICM   RE,15,STABGRS-STABELEM(R6)                                       
         CVD   RE,DUB                                                           
         ZAP   BFSBGRS,DUB                                                      
         ICM   RE,15,STABNET-STABELEM(R6)                                       
         CVD   RE,DUB                                                           
         ZAP   BFSBNET,DUB                                                      
         BAS   RE,PUTBUFF                                                       
         B     SB240                                                            
         SPACE                                                                  
* END OF THIS ESTIMATE STATION BILLS                                            
SB500    CLC   KEY(6),KEYSAVE      SAME A/A/CLT/PRD                             
         BE    SB520               - YES                                        
         MVI   OUTOFSTB,1                                                       
         B     PR100                                                            
SB520    MVC   NXTSTBKY,KEY        SAVE STA BILL KEY                            
         EJECT                                                                  
* READ BACK BUFFALO RECS & COMPARE FOR DIFFERENCES                              
PR100    BAS   RE,HIGHBUFF         READ 1ST BUFFALO REC                         
         B     PR220                                                            
*                                                                               
PR200    BAS   RE,SEQBUFF                                                       
PR220    BNZ   PR900                                                            
*                                                                               
         CLI   NOEST,0             NO ESTIMATE FORCE PRINT                      
         BNE   PR240                                                            
         CP    BFBGRS,BFSBGRS      GROSS NET BOTH =                             
         BNE   PR240                                                            
         CP    BFBNET,BFSBNET                                                   
         BE    PR200                                                            
PR240    MVI   ANERR,1                                                          
         CLI   NEWPRDSW,1                                                       
         BE    PR300                                                            
         SPACE                                                                  
* 1ST ERROR FOR THIS PRODUCT                                                    
         XC    KEY(13),KEY                                                      
         MVC   KEY+1(1),SVCLTKY+1                                               
         MVC   KEY+2(2),SVCLTKY+2                                               
         L     R6,CURPRD                                                        
         MVC   KEY+4(3),0(R6)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETPRD                                                           
         MVI   NEWPRDSW,1                                                       
         SPACE                                                                  
* NOW READ FOR ESTIMATE ONCE                                                    
PR300    CLI   NEWESTSW,0                                                       
         BE    PR400                                                            
         XC    KEY(13),KEY                                                      
         MVC   KEY+1(1),SVCLTKY+1                                               
         MVC   KEY+2(2),SVCLTKY+2                                               
         L     R6,CURPRD                                                        
         MVC   KEY+4(3),0(R6)                                                   
         MVC   KEY+7(1),NEWESTSW                                                
         GOTO1 HIGH                                                             
         MVI   NEWESTSW,0                                                       
         CLC   KEY(13),KEYSAVE                                                  
         BE    PR320                                                            
PR302    MVC   EDEST,KEYSAVE+7                                                  
         B     PR400                                                            
PR320    GOTO1 GETEST                                                           
         SPACE                                                                  
* IF NO ESTIMATE PRINT MESSAGE & EDIT ESTIMATE NUMBER                           
PR400    CLI   EDEST,0                                                          
         BE    PR420                                                            
         EDIT  (1,EDEST),(3,P1),ALIGN=LEFT                                      
*                                                                               
         MVC   P1+4(NOESTXLN),NOESTX                                            
         GOTO1 REPORT                                                           
         MVI   EDEST,0                                                          
PR420    LA    R6,P1                                                            
         USING PLINE,R6                                                         
         MVC   PLNAGY,CURAGY                                                    
         MVC   WORK(2),BFYS        YY/MM                                        
         MVI   WORK+2,1            DAYS                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,PLNYS)                                   
         ZIC   R2,BFBM                                                          
         SRDL  R2,4                                                             
         AH    R2,=H'80'           BASE YEAR                                    
         SLL   R2,4                                                             
         SLDL  R2,4                SET UP YYMM DATE                             
         STH   R2,WORK                                                          
         GOTO1 DATCON,DMCB,(3,WORK),(6,PLNBM)                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BFINV          EDIT WITH LEADING 00'S                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLNINV(4),DUB                                                    
*                                                                               
         CP    BFBGRS,=P'0'                                                     
         BE    PR500                                                            
         EDIT  BFBGRS,PLNBGRS,2,ALIGN=LEFT,MINUS=YES                            
*                                                                               
PR500    CP    BFBNET,=P'0'                                                     
         BE    PR520                                                            
         EDIT  BFBNET,PLNBNET,2,ALIGN=LEFT,MINUS=YES                            
*                                                                               
PR520    CP    BFSBGRS,=P'0'                                                    
         BE    PR540                                                            
         EDIT  BFSBGRS,(12,PLNSBGRS),2,ALIGN=LEFT,MINUS=YES                     
*                                                                               
PR540    CP    BFSBNET,=P'0'                                                    
         BE    PR560                                                            
         EDIT  BFSBNET,PLNSBNET,2,ALIGN=LEFT,MINUS=YES                          
*                                                                               
PR560    ZAP   DOUBLE,BFBGRS                                                    
         SP    DOUBLE,BFSBGRS                                                   
         BZ    PR580                                                            
         MP    DOUBLE,=P'-1'                                                    
         EDIT  DOUBLE,PLNDFGRS,2,ALIGN=LEFT,MINUS=YES                           
*                                                                               
PR580    ZAP   DOUBLE,BFBNET                                                    
         SP    DOUBLE,BFSBNET                                                   
         BZ    PR600                                                            
         MP    DOUBLE,=P'-1'                                                    
         EDIT  DOUBLE,PLNDFNET,2,ALIGN=LEFT,MINUS=YES                           
         DROP  R6                                                               
PR600    GOTO1 REPORT                                                           
         B     PR200                                                            
*                                                                               
PR900    MVI   FORCEHED,C'Y'                                                    
         BAS   RE,RSETBUFF                                                      
         B     SP500                                                            
         EJECT                                                                  
* TEST TO SEE IF ANY ERRORS                                                     
PREND    CLI   ANERR,0                                                          
         BNE   PE100                                                            
         MVC   P1(NOERRLN),NOERR                                                
         GOTO1 REPORT                                                           
PE100    GOTO1 AENDREQ             END REPORT                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        BUFFALO ROUTINES                                                       
*                                                                               
HIGHBUFF XC    BFKEY,BFKEY                                                      
         LA    R1,=C'HIGH'                                                      
         B     BUFFX                                                            
*                                                                               
SEQBUFF  LA    R1,=C'SEQ'                                                       
         B     BUFFX                                                            
*                                                                               
PUTBUFF  LA    R1,=C'PUT'                                                       
         B     BUFFX                                                            
*                                                                               
RSETBUFF LA    R1,=C'RESET'                                                     
         B     BUFFX                                                            
*                                                                               
BUFFX    NTR1                                                                   
         ST    R1,DMCB                                                          
         GOTO1 BUFFALO,DMCB,,BUFFBUFF,BFREC,1                                   
         TM    DMCB+8,X'80'                                                     
         B     EXIT                                                             
         SPACE                                                                  
GETEL    GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
COVAIL   DC    V(COVAIL)                                                        
SVCLTKY  DS    CL4                 LAST CLIENT KEY READ                         
CURPRD   DS    A                   A(CURRENT PRODUCT IN USES)                   
ENDPDLST DS    A                   A(END OF CLIENT PRODUCT LIST)                
*                                                                               
OUTOFIND DS    0H                                                               
OUTOFSTB DS    XL1                 OUT OF STATION BILLS FOR A PRD               
OUTOFESB DS    XL1                 OUT OF ESTIMATE BILLS FOR A PRD              
*                                                                               
NXTESBKY DS    0XL13                                                            
NXTESBID DS    XL1                                                              
NXTESBAM DS    XL1                                                              
NXTESBCT DS    XL2                                                              
NXTESBPD DS    XL3                                                              
NXTESBES DS    XL1                                                              
NXTESBYS DS    XL1                                                              
NXTESBMS DS    XL1                                                              
NXTESBBM DS    XL1                                                              
NXTESBIV DS    XL2                                                              
*                                                                               
NXTSTBKY DS    0XL13                                                            
NXTSTBID DS    XL2                                                              
NXTSTBAM DS    XL1                                                              
NXTSTBCT DS    XL2                                                              
NXTSTBPD DS    XL1                                                              
NXTSTBES DS    XL1                                                              
NXTSTBMK DS    XL2                                                              
NXTSTBST DS    XL3                                                              
         DS    XL1                 SPARE                                        
*                                                                               
NOSTBEST DS    XL1                 EST BILLS BUT NO STA BILLS                   
ELCODE   DS    XL1                                                              
NEWPRDSW DS    XL1                 READ A NEW PRODUCT IF ERROR                  
NEWESTSW DS    XL1                 ESTIMATE - READ NEW EST IF ERROR             
NOEST    DS    XL1                 NO EST BUT BILLS FOR EST                     
EDEST    DS    XL1                 ESTIMATE NUMBER TO EDIT                      
ANERR    DS    XL1                 FOUND AN ERROR                               
CORBIL   DS    XL1                 1 IF DISTRABUTION BILLS INCOUNTERED          
CURAGY   DS    CL2                 AGENCY 2 CHAR CODE                           
AGYTBL   DS    15CL2               MAX 15 AGENCIES PER FILE                     
*                                                                               
BFREC    DS    0C                  BUFFALO RECORD LAYOUT                        
BFKEY    DS    0CL5                                                             
BFYS     DS    XL1                                                              
BFMS     DS    XL1                                                              
BFBM     DS    XL1                                                              
BFINV    DS    XL2                                                              
*                                                                               
BFBGRS   DS    PL6                                                              
BFBNET   DS    PL6                                                              
BFSBGRS  DS    PL6                                                              
BFSBNET  DS    PL6                                                              
*                                                                               
NOESTX   DC    C'THE FOLLOWING BILLS NO LONGER HAVE AN ESTIMATE'                
NOESTXLN EQU   *-NOESTX                                                         
NOERR    DC    C'NO BILLING ERRORS WERE DETECTED'                               
NOERRLN  EQU   *-NOERR                                                          
*                                                                               
*                                                                               
         BUFF  LINES=1,ROWS=1,COLUMNS=4,FLAVOR=PACKED,KEYLIST=(5,A)             
*                                                                               
         EJECT                                                                  
PLINE    DSECT                                                                  
PLNAGY   DS    CL2                                                              
         DS    XL1                 SPACE                                        
PLNYS    DS    CL8                                                              
         DS    XL1                 SPACE                                        
PLNBM    DS    CL10                                                             
         DS    XL1                 SPACE                                        
PLNINV   DS    CL7                                                              
         DS    XL1                 SPACE                                        
PLNBGRS  DS    CL12                                                             
         DS    XL1                 SPACE                                        
PLNBNET  DS    CL12                                                             
         DS    XL1                 SPACE                                        
PLNSBGRS DS    CL13                                                             
         DS    XL1                 SPACE                                        
PLNSBNET DS    CL12                                                             
         DS    XL1                 SPACE                                        
PLNDFGRS DS    CL12                                                             
         DS    XL1                 SPACE                                        
PLNDFNET DS    CL12                                                             
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
ESBHDRD  DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
STBHDRD  DSECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE DDBUFFALOD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074SPREPZ102 04/01/03'                                      
         END                                                                    
