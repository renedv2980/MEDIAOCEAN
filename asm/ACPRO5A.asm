*          DATA SET ACPRO5A    AT LEVEL 008 AS OF 08/10/00                      
*PHASE T60B5AA                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B5A - JOB ESTIMATE - TIME ESTIMATING MODULE'                 
*                                                                               
* FKON 2     - AMEND SRCHTBL TO ACCOMODATE GLOBAL VALUES                        
* FKON 3     - AMEND SRCHTBL                                                    
*                                                                               
T60B5A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B5A**,R7,R5,RR=R2                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R2,RELO                                                          
         LA    RE,CTRTBGBR         RE=A(TABLE OF CTRY SPECIFIC VALUES)          
         CLI   CTRY,CTRYGER                                                     
         BNE   *+12                                                             
         LA    RE,CTRTBGER                                                      
         B     INIT06                                                           
         CLI   CTRY,CTRYHOL                                                     
         BNE   INIT06                                                           
         LA    RE,CTRTBHOL                                                      
INIT06   MVC   CTRVALUE(CTRVALNQ),0(RE) SET COUNTRY SPECIFIC VALUES             
*                                                                               
         CLI   MODE,NEWSCR                                                      
         BNE   INIT10                                                           
         CLI   CALLER,0            ARE WE CALLED FROM BASE SCREEN?              
         BNE   NSCR                NO                                           
         GOTO1 VTRANSF,WORK,RECNJOB,ACTNEST,0  GO AND CALL JOB/EST              
*                                                                               
INIT10   GOTO1 DICTATE,DMCB,C'LU  ',DDIN,DDOUT                                  
         L     RF,=A(SELACCS-T60BFFD)                                           
         AR    RF,RA                                                            
         ST    RF,ASELACCS         ADDRESSE OF SELECTED ACCOUNTS AREA           
*        CLI   ACTNUM,ACTNSTAF                                                  
*        BE    JOST                GO TO JOB/STAFF ROUTINE                      
         B     JOTI                GO TO JOB/TIME ROUTINE                       
         EJECT                                                                  
**********************************************************************          
* NEW SCREEN MODE ROUTINE                                            *          
**********************************************************************          
NSCR     MVI   RACHANGE,C'Y'       RECORD/ACTION HAS CHANGED                    
         MVI   PFKEY,0                                                          
         CLI   CALLER,X'32'        ARE WE CALLED FROM JOB/EST CONTROLER         
         BNE   NSCR10              NO                                           
         MVI   SVFLAG,0            INITIALIZE FLAG                              
         MVC   SAVECLI,CLICODE     SAVE JOB AND ESTIMATE VALUES SET             
         MVC   SAVEPRO,PRODCODE     BY ACPRO43                                  
         MVC   SAVEJOB,JOBNUM                                                   
         MVC   SAVEST,ESTIMATE     ESTIMATE TYPE/VERSION                        
         MVC   SAVEWC,WCSUBWC      WORK CODE INCL. SUB CODE                     
         ZAP   SAVETOTA,ESTITOTA   JOB ESTIMATE TOTAL AMOUNT                    
         XC    NSAVELS,NSAVELS     INIT NUMBER OF SAVED ELEMENTS                
NSCR10   BAS   RE,GETCLI           GET CLIENT LENGTH AND OFFICE CODE            
         BAS   RE,GETLDG           GET PERSON LEDGER                            
*        CLI   ACTNUM,ACTNSTAF     TEST JOB STAFF SCREEN                        
*        BNE   NSCR14              NO                                           
         B     NSCR14              NO                                           
         USING ACLELD,R6                                                        
         MVC   PRSLV1H,ACLVDESC    R6 STILL POINTS TO ACC LENGTH EL             
         MVC   PRSLV2H,ACLVDESC+(L'ACLVALS*1)                                   
         MVC   PRSLV3H,ACLVDESC+(L'ACLVALS*2)                                   
         MVC   PRSLV4H,ACLVDESC+(L'ACLVALS*3)                                   
         CLI   LDGLV4,0                                                         
         BNE   NSCR12                                                           
         OI    PRSLEV4H+1,X'20'    PROTECT LEVEL 4                              
         CLI   LDGLV3,0                                                         
         BNE   NSCR12                                                           
         OI    PRSLEV3H+1,X'20'    PROTECT LEVEL 3                              
         CLI   LDGLV2,0                                                         
         BNE   NSCR12                                                           
         OI    PRSLEV2H+1,X'20'    PROTECT LEVEL 2                              
         CLI   LDGLV1,0                                                         
         BNE   NSCR12                                                           
         DC    H'0'                NO LEVEL - SOMETHING WRONG                   
NSCR12   BAS   RE,GETTSR           GET TIME SHEET REC IF THERE IS ONE           
         LA    R2,PRSPFAH                                                       
         LA    RF,AS$TEPF1         TIME ESTIMATING PFKEY LINE 1                 
         B     NSCR16                                                           
*                                                                               
NSCR14   LA    R2,PRTPFAH                                                       
         LA    RF,AS$TEPF2         TIME ESTIMATING PFKEY LINE 2                 
*                                                                               
NSCR16   XC    WORK,WORK                                                        
         GOTO1 GETTXT,WORK,(RF),('PFLMAX',(R2)),(C'S',0)                        
         OI    6(R2),X'80'         TRANSMIT PFKEY LINE                          
*                                                                               
NSCRX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* JOB/STAFF ROUTINE                                                  *          
**********************************************************************          
JOST     CLI   MODE,VALKEY                                                      
         BE    JOST4                                                            
         CLI   MODE,VALREC                                                      
         BE    JOST20                                                           
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALKEY LOGIC JOB/STAFF                                             *          
**********************************************************************          
JOST4    MVI   INTMODE,DISLIST     INITIALIZE INTERNAL MODE                     
         LA    R2,PRSLEV1H                                                      
         TM    PRSLEV1H+4,X'20'    TEST CHANGE IN KEY FIELDS                    
         BNO   JOST6                                                            
         TM    PRSLEV2H+4,X'20'                                                 
         BNO   JOST6                                                            
         TM    PRSLEV3H+4,X'20'                                                 
         BNO   JOST6                                                            
         TM    PRSLEV4H+4,X'20'                                                 
         BNO   JOST6                                                            
         B     JOST7                                                            
JOST6    MVI   KEYCHG,C'Y'         KEY HAS CHANGED                              
JOST7    CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+12                                                             
         MVI   RACHANGE,C'N'                                                    
         B     JOST8                                                            
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELD                 
         BNE   JOST12              NO                                           
         MVI   INTMODE,FSTLIST                                                  
         B     JOST10              YES - SO DON'T CLEAR PREV SEL ACCS           
JOST8    MVI   INTMODE,FSTLIST                                                  
         L     RE,ASELACCS         CLEAR SELECTED ACCOUNTS SAVE AREA            
         LA    R0,MAXSEL                                                        
         XC    0(L'ACTKACT,RE),0(RE)                                            
         LA    RE,L'ACTKACT(RE)                                                 
         BCT   R0,*-10                                                          
         MVI   NSELACCS,0          RESET NUMBER OF SELECTED ACCOUNTS            
JOST10   LH    RE,=Y(SAVEKEYS-T60BFFD)  CLEAR BACKWARD SCREEN SAVE AREA         
         AR    RE,RA                                                            
         LA    R0,MAXKEYS                                                       
         XC    0(L'ACTKACT,RE),0(RE)                                            
         LA    RE,L'ACTKACT(RE)                                                 
         BCT   R0,*-10                                                          
         MVI   LNLISTS,0           RESET NUMBER OF LISTS ON SCREEN              
         MVI   NSAVSCRS,0          RESET NUMBER OF SAVED SCREENS                
         XC    LASTACC,LASTACC     CLEAR SAVED LAST ACCOUNT ON SCREEN           
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELD                 
         BNE   JOST12              NO                                           
         BAS   RE,VHEAD            VALIDATE THE START KEY FIELDS                
*                                                                               
JOST12   B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALREC LOGIC JOB/STAFF                                             *          
**********************************************************************          
         USING ACTRECD,R4                                                       
JOST20   TM    SVFLAG,SVFLTSRE     ARE WE DEALING WITH TIME SHEET RECS?         
         BNO   JOST22              NO                                           
         L     R4,AIO2             READ TIME SHEET LIST RECORD INTO IO2         
         MVC   0(L'TSLKSAVE,R4),TSLKSAVE RESTORE SAVED KEY                      
         GOTO1 DATAMGR,DMCB,=C'DMREAD',SYSFIL,(R4),(R4),0                       
*                                                                               
JOST22   CLI   INTMODE,FSTLIST                                                  
         BE    JOST24                                                           
         BAS   RE,ANYSEL           HANDLE ANY SELECTIONS ON THE SCREEN          
         BE    XIT                                                              
         BAS   RE,PROCPFST         PROCESS PFKEYS                               
         B     JOST24                                                           
*                                                                               
JOST24   LA    RF,LIST             READ AND LIST THE ACCOUNTS                   
         TM    SVFLAG,SVFLTSRE     ARE WE DEALING WITH TIME SHEET RECS?         
         BNO   *+8                 NO                                           
         LA    RF,TSLIST           LIST THE ACCOUNTS FROM TSL RECORD            
         BASR  RE,RF                                                            
         LA    R2,PRSACT1H         PUT CURSER AT FIRST ACTION FIELD             
         LA    R0,AP$LDMOR         MORE TO COME                                 
         CLI   LNLISTS,NLINES      TEST IF SCREEN FILLED                        
         BE    SCINFMSG            YES                                          
         XC    LASTACC,LASTACC     NO-MUST BE AT END-OF-LIST                    
         LA    R0,AP$LDISP         END OF LIST                                  
         B     SCINFMSG                                                         
*                                                                               
JOSTX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO READ AND LIST THE PERSON ACCOUNTS ON THE SCREEN     *          
* ON ENTRY KEY CONTAINS THE START ACCOUNT KEY                        *          
**********************************************************************          
LIST     NTR1  ,                                                                
         GOTO1 VCLEARF,DMCB,(0,PRSACT1H),PRSPFAH CLEAR SCREEN                   
         GOTO1 (RF),(R1),(1,PRSACT1H),PRSPFAH                                   
         MVI   LNLISTS,0           RESET NUMBER OF LINES ON SCREEN              
         LA    R2,PRSACT1H         R2=A(FIRST SELECT FIELD)                     
         USING LNJOSTD,R2                                                       
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),CTRPLDG                             
LIST10   GOTO1 HIGH                                                             
         CLC   KEY(L'ACTKCPY+L'ACTKUNT+L'ACTKLDG),KEYSAVE TEST SAME CUL         
         BNE   LIST38              ALL DONE                                     
         MVI   ELCODE,RSTELQ       TEST ACCOUNT STATUS                          
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R6                                                        
         TM    RSTSTAT1,RSTSACIC+RSTSACIL                                       
         BNZ   LIST30              DON'T LIST IF ACCOUNT IS LOCKED              
         MVI   ELCODE,ABLELQ       TEST ACCOUNT HAS BALANCE ELEMENT             
         BAS   RE,GETELIO                                                       
         BNE   LIST30              NO BALANCE ELEMENT - HIGHER LEV ACC          
         MVI   ELCODE,NAMELQ       LOOK FOR ACCOUNT NAME                        
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NAMELD,R6                                                        
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         LTR   RE,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                BAD NAME ELEMENT                             
         EX    RE,*+4                                                           
         MVC   LSTNAME(0),NAMEREC                                               
         MVC   LSTCODE,ACTKACT                                                  
         OI    LSTACTH+4,X'20'     MARK ACT FIELD FOR PREV VALIDATION           
         L     RE,ASELACCS         TEST IF ACCOUNT IS ALREADY SELECTED          
         SR    R0,R0                                                            
         ICM   R0,1,NSELACCS                                                    
         BZ    LIST20              NO SELECTED ACCOUNTS YET                     
         CLC   0(L'ACTKACT,RE),ACTKACT                                          
         BE    *+16                                                             
         LA    RE,L'ACTKACT(RE)                                                 
         BCT   R0,*-14                                                          
         B     LIST20              ACCOUNT IS NOT SELECTED YET                  
         MVI   LSTACT,C'*'         MARK AS SELECTED                             
         MVC   LSTACT+1(1),AC@SEL                                               
LIST20   SR    RE,RE                                                            
         IC    RE,LNLISTS                                                       
         LA    RE,1(RE)            INCREMENT LIST LINES COUNT                   
         STC   RE,LNLISTS                                                       
         CLI   LNLISTS,NLINES      TEST SCREEN FILLED                           
         BNL   LIST34                                                           
         LA    R2,LSTLEN(R2)                                                    
*                                                                               
LIST30   MVI   KEY+ACTKEND,X'FF'   NEXT ACCOUNT                                 
         B     LIST10                                                           
*                                                                               
LIST34   MVC   LASTACC,ACTKACT     SAVE LAST ACCOUNT ON SCREEN                  
         B     LISTX                                                            
*                                                                               
LIST38   LA    R4,KEYSAVE                                                       
         MVC   LASTACC,ACTKACT                                                  
*                                                                               
LISTX    B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO READ AND LIST THE PERSON ACCOUNTS ON THE SCREEN     *          
* ON ENTRY IO2 CONTAINS THE TIME SHEET LIST RECORD                   *          
*          KEY CONTAINS THE START ACCOUNT CODE (SPACES MEANS START   *          
*                                               FROM THE BEGINNING)  *          
**********************************************************************          
         USING LNJOSTD,R2                                                       
         USING ACTRECD,R4                                                       
TSLIST   NTR1  ,                                                                
         GOTO1 VCLEARF,DMCB,(0,PRSACT1H),PRSPFAH CLEAR SCREEN                   
         GOTO1 (RF),(R1),(1,PRSACT1H),PRSPFAH                                   
         MVI   LNLISTS,0           RESET NUMBER OF LINES ON SCREEN              
         LA    R2,PRSACT1H         R2=A(FIRST SELECT FIELD)                     
         L     R3,AIO2             GET ADDRESSE OF TIME SHEET LIST REC          
         LA    R3,ACCORFST(R3)                                                  
         ST    R3,ADRELEM          SAVE ADDRESSE OF ELEMENT                     
*                                                                               
TSLIST04 CLI   0(R3),0             TEST END OF RECORD                           
         BE    TSLIST34                                                         
         CLI   0(R3),TSLELQ        TEST FOR TIME SHEET LIST ELEMENT             
         BNE   TSLIST30            NO - NEXT ELEMENT                            
*                                                                               
         LA    R3,TSLLN1Q(R3)      POINT R3 TO ELEMENT DATA                     
         LA    R4,KEY                                                           
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),CTRPLDG                             
TSLIST05 TM    PFLAG,PFLFSTLI      TEST FIRST LINE                              
         BNO   TSLIST06            NO                                           
         CLC   ACTKACT,SPACES      START FROM THE BEGINNING?                    
         BNE   TSLIST08            NO                                           
*                                                                               
TSLIST06 SR    RF,RF                                                            
         IC    RF,0(R3)            GET LENGTH OF DATA                           
         CH    RF,=H'3'                                                         
         BL    TSLIST28            NO DATA                                      
         SH    RF,=H'3'            1 FOR LENGTH 1 FOR STATUS 1 FOR EX           
         MVC   ACTKACT,SPACES                                                   
         MVI   KEY+ACTKEND,0                                                    
         EX    RF,*+4                                                           
         MVC   ACTKACT(0),TSLLN2Q(R3) EXTRACT ELEMENT DATA                      
         B     TSLIST10            GO AND READ NEXT ACCOUNT                     
*                                                                               
TSLIST08 SR    RF,RF                                                            
         IC    RF,0(R3)            GET LENGTH OF DATA                           
         CH    RF,=H'3'                                                         
         BL    TSLIST28            NO DATA                                      
         SH    RF,=H'3'            1 FOR LENGTH 1 FOR STATUS 1 FOR EX           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   ACTKACT(0),TSLLN2Q(R3) MATCH ON ELEMENT ENTRY?                   
         BNE   TSLIST28            NO - GET NEXT TSL ENTRY                      
         NI    PFLAG,255-PFLFSTLI  SWITCH OFF FIRST LINE FLAG                   
         B     TSLIST10            READ NEXT ACCOUNT                            
*                                                                               
TSLIST10 GOTO1 HIGH                                                             
         CLC   KEY(L'ACTKCPY+L'ACTKUNT+L'ACTKLDG),KEYSAVE TEST SAME CUL         
         BNE   TSLIST28            NEXT TIME SHEET EL ENTRY                     
         SR    RF,RF                                                            
         IC    RF,0(R3)            GET DATA LENGTH                              
         SH    RF,=H'3'            1 FOR LENGTH 1 FOR STATUS 1 FOR EX           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   ACTKACT(0),TSLLN2Q(R3) MATCH ON TSL ENTRY?                       
         BNE   TSLIST28            NO - NEXT TIME SHEET EL ENTRY                
         MVI   ELCODE,RSTELQ       TEST ACCOUNT STATUS                          
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R6                                                        
         TM    RSTSTAT1,RSTSACIC+RSTSACIL                                       
         BNZ   TSLIST24            DON'T LIST IF ACCOUNT IS LOCKED              
         MVI   ELCODE,ABLELQ       TEST ACCOUNT HAS BALANCE ELEMENT             
         BAS   RE,GETELIO                                                       
         BNE   TSLIST24            NO BALANCE ELEMENT - HIGHER LEV ACC          
         MVI   ELCODE,NAMELQ       LOOK FOR ACCOUNT NAME                        
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NAMELD,R6                                                        
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         LTR   RE,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                BAD NAME ELEMENT                             
         EX    RE,*+4                                                           
         MVC   LSTNAME(0),NAMEREC                                               
         MVC   LSTCODE,ACTKACT                                                  
         OI    LSTACTH+4,X'20'     MARK ACT FIELD FOR PREV VALIDATION           
         L     RE,ASELACCS         TEST IF ACCOUNT IS ALREADY SELECTED          
         SR    R0,R0                                                            
         ICM   R0,1,NSELACCS                                                    
         BZ    TSLIST20            NO SELECTED ACCOUNTS YET                     
         CLC   0(L'ACTKACT,RE),ACTKACT                                          
         BE    *+16                                                             
         LA    RE,L'ACTKACT(RE)                                                 
         BCT   R0,*-14                                                          
         B     TSLIST20            ACCOUNT IS NOT SELECTED YET                  
         MVI   LSTACT,C'*'         MARK AS SELECTED                             
         MVC   LSTACT+1(1),AC@SEL                                               
TSLIST20 SR    RE,RE                                                            
         IC    RE,LNLISTS                                                       
         LA    RE,1(RE)            INCREMENT LIST LINES COUNT                   
         STC   RE,LNLISTS                                                       
         CLI   LNLISTS,NLINES      TEST SCREEN FILLED                           
         BNL   TSLIST34                                                         
         LA    R2,LSTLEN(R2)                                                    
*                                                                               
TSLIST24 MVI   KEY+ACTKEND,X'FF'   NEXT ACCOUNT                                 
         B     TSLIST10                                                         
*                                                                               
TSLIST28 TM    PFLAG,PFLFSTLI      TEST IF FIRST LINE                           
         BO    *+10                YES                                          
         MVC   KEY,KEYSAVE         RESTORE LAST ACCOUNT KEY                     
         SR    RE,RE                                                            
         IC    RE,0(R3)                                                         
         AR    R3,RE               BUMP R3 TO NEXT ENTRY                        
         CLI   0(R3),0                                                          
         BE    TSLIST30            ZERO MEANS END OF ELEMENT                    
         B     TSLIST05                                                         
*                                                                               
TSLIST30 SR    RE,RE                                                            
         L     R3,ADRELEM          RESTORE ADDRESSE OF ELEMENT                  
         IC    RE,1(R3)                                                         
         AR    R3,RE               AND BUMP TO NEXT ELEMENT                     
         ST    R3,ADRELEM          SAVE ADDRESSE OF ELEMENT                     
         B     TSLIST04                                                         
*                                                                               
TSLIST34 MVC   LASTACC,ACTKACT     SAVE LAST ACCOUNT ON SCREEN                  
         B     TSLISTX                                                          
*                                                                               
TSLISTX  B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*********************************************************************           
* SUB-ROUTINE TO EDIT THE LIST SCREEN AND SAVE THE SELECTED         *           
* ACCOUNTS UP TO MAXSEL                                             *           
*********************************************************************           
ANYSEL   SR    R0,R0                                                            
         ICM   R0,1,LNLISTS        R0=NUMBER OF LINES ON SCREEN                 
         BZ    ANYSELX             NOTHING ON SCREEN                            
         LA    R2,PRSACT1H         R2=A(SELECT FIELD)                           
         USING LNJOSTD,R2                                                       
         L     R6,ASELACCS         R6=SAVE AREA FOR SELECTED ACCOUNTS           
         SR    RF,RF                                                            
         IC    RF,NSELACCS         NUMBER OF SELECTED ACCOUNTS                  
         MH    RF,=Y(L'ACTKACT)                                                 
         AR    R6,RF               POINT R6 TO FREE PLACE                       
         NI    PFLAG,255-PFLSELAL  INITIALIZE PROGRAM FLAG                      
*                                                                               
ANYSEL10 TM    PFLAG,PFLSELAL      TEST SELECT ALL                              
         BZ    *+16                NO                                           
         CLI   LSTACT,C'*'         YES - TEST ALREADY EDITED                    
         BE    ANYSEL20            NEXT LIST ENTRY                              
         B     ANYSEL12            HANDLE AS SELECTED                           
         TM    LSTACTH+4,X'20'     TEST CHANGE IN ACTION FIELD                  
         BO    ANYSEL20            NO - NEXT LIST ENTRY                         
         CLI   LSTACTH+5,0         TEST ANY SELECT INPUT                        
         BE    ANYSEL14            NO - MEANS PREV INPUT WAS DELETED            
         CLI   LSTACT,C'*'         TEST ALREADY EDITED                          
         BE    ANYSEL20            YES - NEXT LIST ENTRY                        
         MVI   ERROR,INVALID                                                    
         CLC   LSTACT(1),AC@SEL    SELECT?                                      
         BNE   ERREND                                                           
         CLI   LSTACTH+5,2         TEST INPUT LENGTH                            
         BH    ERREND                                                           
         BL    ANYSEL12                                                         
         CLI   LSTACT+1,C'+'       TEST SELECT ALL                              
         BNE   ERREND              NO                                           
         OI    PFLAG,PFLSELAL      SELECT ALL FROM THIS LINE ON                 
ANYSEL12 CLI   NSELACCS,MAXSEL                                                  
         BL    *+12                                                             
         MVI   ERROR,MAXLIUSE      MAXIMUM ACCOUNTS SELECTED                    
         B     ERREND                                                           
         MVI   LSTACT,C'*'         MARK AS EDITED                               
         MVC   LSTACT+1(1),AC@SEL                                               
         MVC   0(L'ACTKACT,R6),LSTCODE                                          
         LA    R6,L'ACTKACT(R6)                                                 
         SR    RF,RF                                                            
         IC    RF,NSELACCS                                                      
         LA    RF,1(RF)            INCREASE NUMBER OF SELECTED ACCOUNTS         
         STC   RF,NSELACCS                                                      
         B     ANYSEL18                                                         
*                                                                               
ANYSEL14 L     RF,ASELACCS         LOOK FOR ACCOUNT IN SAVE AREA                
         SR    R4,R4                                                            
         ICM   R4,1,NSELACCS                                                    
         BZ    ANYSEL18            NO SELECTED ACCOUNTS YET                     
         CLC   0(L'ACTKACT,RF),LSTCODE                                          
         BE    *+16                                                             
         LA    RF,L'ACTKACT(RF)                                                 
         BCT   R4,*-14                                                          
         B     ANYSEL18            ACCOUNT IS NOT SELECTED YET                  
         XC    0(L'ACTKACT,RF),0(RF)  DELETE ACCOUNT FROM SAVE AREA             
         SH    R6,=Y(L'ACTKACT)    SET R6 ONE ENTRY BACK                        
         SR    R1,R1                                                            
         IC    R1,NSELACCS                                                      
         BCTR  R1,0                SUPTRACT ONE SELECTED ACCOUNT                
         STC   R1,NSELACCS                                                      
         BCT   R4,ANYSEL16         MOVE UP THE FOLLOWING ACCS IF SOME           
         B     ANYSEL18            IT WAS THE LAST ONE                          
ANYSEL16 MVC   0(L'ACTKACT,RF),L'ACTKACT(RF)                                    
         LA    RF,L'ACTKACT(RF)                                                 
         BCT   R4,ANYSEL16                                                      
         XC    0(L'ACTKACT,RF),0(RF)  CLEAR THE LAST ONE                        
*                                                                               
ANYSEL18 OI    LSTACTH+6,X'80'     TRANSMIT ACTION FIELD                        
         OI    LSTACTH+4,X'20'                                                  
         MVI   INTMODE,EDTLIST                                                  
*                                                                               
ANYSEL20 LA    R2,LSTLEN(R2)       BUMP TO NEXT LINE                            
         BCT   R0,ANYSEL10                                                      
*                                                                               
ANYSELX  CLI   INTMODE,EDTLIST     SET CC                                       
         MVI   INTMODE,DISLIST                                                  
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO PROCESS ANY PFKEY FROM JOB/STAFF SCREEN             *          
**********************************************************************          
PROCPFST CLI   PFKEY,0                                                          
         BE    STPFK8              NO PFKEY SCROLL FORWARD                      
         CLI   PFKEY,PF6                                                        
         BE    STPFK6              END AND UPDATE                               
         CLI   PFKEY,PF7                                                        
         BE    STPFK7              SCROLL BACKWARD                              
         CLI   PFKEY,PF8                                                        
         BE    STPFK8              SCROLL FORWARD                               
         CLI   PFKEY,PF12                                                       
         BE    STPFK12             END NO UPDATE                                
         B     STPFK8              NO VALID PFKEY SCROLL FORWARD                
*                                                                               
STPFK6   GOTO1 VTRANSF,WORK,RECNJOB,ACTNTIME,0  GO TO JOB/TIME                  
*                                                                               
         USING ACTRECD,R4                                                       
STPFK7   LA    R4,KEY              SCROLL ONE SCREEN BACK                       
         MVC   KEY,SPACES                                                       
         LA    RF,SAVEKEYS                                                      
         SR    R1,R1                                                            
         ICM   R1,1,NSAVSCRS       GET NUMBER OF SAVED SCREENS                  
         BZ    XIT                 NO SAVED SCREEN                              
         BCTR  R1,0                                                             
         STC   R1,NSAVSCRS                                                      
         MH    R1,=Y(L'ACTKACT)                                                 
         AR    RF,R1                                                            
         MVC   ACTKACT,0(RF)                                                    
         XC    0(L'ACTKACT,RF),0(RF)                                            
         OI    PFLAG,PFLFSTLI                                                   
         B     STPFX                                                            
*                                                                               
STPFK8   LA    R4,KEY              SCROLL ONE SCREEN FORWARD                    
         OI    PFLAG,PFLFSTLI                                                   
         CLI   LNLISTS,NLINES      TEST IF SCREEN IS FILLED                     
         BL    XIT                 NO MORE TO COME                              
         MVC   ACTKACT,LASTACC                                                  
         MVI   KEY+ACTKEND,X'FF'   NEXT ACCOUNT                                 
         LA    R2,PRSACT1H         R2=A(SELECT FIELD)                           
         USING LNJOSTD,R2                                                       
         LA    RF,SAVEKEYS                                                      
         SR    R1,R1                                                            
         IC    R1,NSAVSCRS         GET NUMBER OF SAVED SCREENS                  
         CH    R1,=Y(MAXKEYS)      IF MAXKEYS SAVED                             
         BL    *+8                 NO                                           
         LA    R1,MAXKEYS-1        YES - OVERWRITE THE LAST ONE                 
         MH    R1,=Y(L'ACTKACT)                                                 
         AR    RF,R1               RF=A(END OF SAVED KEY LIST)                  
         MVC   0(L'ACTKACT,RF),LSTCODE  SAVE FIRST KEY ON SCREEN                
         CLI   NSAVSCRS,MAXKEYS    IF MAXKEYS SAVED                             
         BNL   STPFX               DON'T INCREASE NUMBER OF SAVED SCR           
         SR    R1,R1                                                            
         IC    R1,NSAVSCRS                                                      
         LA    R1,1(R1)            INCREASE NUMBER OF SAVED SCREENS             
         STC   R1,NSAVSCRS                                                      
         B     STPFX                                                            
*                                                                               
STPFK12  L     RF,ASELACCS         CLEAR SELECTED ACCOUNTS SAVE AREA            
         LA    R0,MAXSEL                                                        
         XC    0(L'ACTKACT,RF),0(RF)                                            
         LA    RF,L'ACTKACT(RF)                                                 
         BCT   R0,*-10                                                          
         MVI   NSELACCS,0          RESET NUMBER OF SELECTED ACCOUNTS            
         GOTO1 VTRANSF,WORK,RECNJOB,ACTNTIME,0  AND EXIT TO JOB/TIME            
*                                                                               
STPFX    BR    RE                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* JOB/TIME ROUTINE                                                   *          
**********************************************************************          
JOTI     GOTO1 CALLOV,DMCB,0,X'D9000A5D'  GET V(TSAR)                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTSAR,0(R1)                                                      
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    JOTI2                                                            
         CLI   MODE,VALREC                                                      
         BE    JOTI40                                                           
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALKEY LOGIC JOB/TIME                                              *          
**********************************************************************          
JOTI2    MVI   INTMODE,DISLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   JOTI38                                                           
         MVI   INTMODE,FSTLIST     SET TO FIRST TIME THROUGH                    
         MVI   NSAVSCRS,0          RESET NUMBER OF SAVED SCREENS                
         MVI   RACHANGE,C'N'                                                    
         MVC   PRTWC(2),SAVEWC     PUT WORK CODE ON SCREEN                      
         SR    R0,R0                                                            
         ICM   R0,1,SAVEWC+2       DO WE HAVE A SUB WORK CODE?                  
         BZ    JOTI4               NO                                           
         CURED (R0),(2,PRTWC+2),0,ALIGN=LEFT                                    
JOTI4    BAS   RE,GETWC            GET WORK CODE NAME                           
*                                                                               
         TM    SVFLAG,SVFLWJTB     TEST IF WE WERE AT JOB/TIME BEFORE           
         BZ    *+12                NO                                           
         BAS   RE,REST             RESTORE THE TSAR DATASET                     
         B     JOTI20                                                           
         BAS   RE,INIT             INITIALIZE THE TSAR DATASET                  
         ZAP   WCTOTAL,=P'0'       INITIALIZE WORK CODE TOTAL AMOUNT            
         ZAP   TOTHOURS,=P'0'      INITIALIZE WORK CODE TOTAL HOURS             
*                                                                               
         USING EVERECD,R4                                                       
         LA    R4,KEY              LOOK FOR EXISTING TIME EST RECORDS           
         XC    KEY,KEY                                                          
         MVI   EVEKTYP,EVEKTYPQ                                                 
         MVI   EVEKSUB,EVEKSUBQ                                                 
         MVC   EVEKCPY(L'CUL),CUL                                               
         MVC   EVEKCLI,SAVECLI                                                  
         MVC   EVEKPRO,SAVEPRO                                                  
         MVC   EVEKJOB,SAVEJOB                                                  
         MVC   EVEKTYPE(L'SAVEST),SAVEST                                        
         MVC   EVEKWC(L'SAVEWC),SAVEWC                                          
         MVI   EVEKSEQ,0                                                        
         GOTO1 HIGH                READ TIME ESTIMATING RECORD                  
         CLC   KEY(L'EVEKEY),KEYSAVE                                            
         BNE   JOTI20                                                           
*                                                                               
         USING EPTELD,R6                                                        
         SR    R0,R0                                                            
         L     R6,AIO                                                           
         LA    R6,ACCORFST(R6)                                                  
JOTI6    CLI   EPTEL,0             TEST FOR EOR                                 
         BE    JOTI12              LOOK FOR ANOTHER RECORD                      
         CLI   EPTEL,EPTELQ        TEST FOR PERSON TIME ELEMENT                 
         BE    JOTI10                                                           
*                                                                               
JOTI8    IC    R0,EPTLN            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         B     JOTI6                                                            
*                                                                               
JOTI10   XC    ELEMENT,ELEMENT                                                  
         LA    RF,ELEMENT                                                       
         SR    RE,RE                                                            
         IC    RE,EPTLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),EPTEL                                                    
         MVI   0(RF),0             FIRST 2 BYTES=LENGTH OF TSAR RECORD          
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAADD                                                    
         ST    RF,TSAREC                                                        
         GOTO1 VTSAR,TSARD         ADD ELEMENT TO TSAR DATASET                  
         ZAP   DUB,EPTRATE                                                      
         MP    DUB,EPTHOURS                                                     
         SRP   DUB,64-2,5                                                       
         AP    WCTOTAL,DUB         ADD UP AMOUNTS                               
         AP    TOTHOURS,EPTHOURS     AND HOURS                                  
         B     JOTI8                                                            
*                                                                               
JOTI12   MVC   NSAVELS,TSARBLK+(TSPRECN-TSARD) STORE NUMBER OF ELS              
         GOTO1 SEQ                 LOOK FOR ANOTHER TIME EST RECORD             
         CLC   KEY(EVEKSEQ-EVEKEY),KEYSAVE                                      
         BNE   JOTI20                                                           
         SR    R0,R0                                                            
         L     R6,AIO                                                           
         LA    R6,ACCORFST(R6)                                                  
JOTI14   CLI   EPTEL,0             TEST FOR EOR                                 
         BE    JOTI12              LOOK FOR ANOTHER RECORD                      
         CLI   EPTEL,EPTELQ        TEST FOR PERSON TIME ELEMENT                 
         BE    JOTI18                                                           
*                                                                               
JOTI16   IC    R0,EPTLN            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         B     JOTI14                                                           
*                                                                               
JOTI18   XC    ELEMENT,ELEMENT                                                  
         LA    RF,ELEMENT                                                       
         SR    RE,RE                                                            
         IC    RE,EPTLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),EPTEL                                                    
         MVI   0(RF),0             FIRST 2 BYTES=LENGTH OF TSAR RECORD          
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAADD                                                    
         ST    RF,TSAREC                                                        
         GOTO1 VTSAR,TSARD         ADD ELEMENT TO TSAR DATASET                  
         ZAP   DUB,EPTRATE                                                      
         MP    DUB,EPTHOURS                                                     
         SRP   DUB,64-2,5                                                       
         AP    WCTOTAL,DUB         ADD UP AMOUNTS                               
         AP    TOTHOURS,EPTHOURS     AND HOURS                                  
         B     JOTI16                                                           
*                                                                               
JOTI20   SR    R0,R0                                                            
         ICM   R0,1,NSELACCS       TAKE NUM OF SEL ACCS FROM JOB/STAFF          
         BZ    JOTI26              NO SEL ACCS                                  
*                                                                               
         XC    ELEMENT,ELEMENT     BUILD TSAR RECORD IN ELEMENT                 
         LA    R6,ELEMENT                                                       
         L     R2,ASELACCS                                                      
         MVI   EPTLN,EPTLN1Q       FIRST 2 BYTES=LENGTH OF TSAR RECORD          
         MVC   EPTUNT(L'EPTUNT+L'EPTLDG),CTRPLDG                                
         ZAP   EPTHOURS,=P'0'                                                   
         ZAP   EPTRATE,=P'0'                                                    
JOTI22   CLI   NSAVELS+1,MAXELS                                                 
         BNL   JOTI26              MAX ELEMENTS LEAVE THE REST ACCS             
         MVC   EPTACT,0(R2)                                                     
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAADD                                                    
         LA    RF,ELEMENT                                                       
         ST    RF,TSAREC                                                        
         GOTO1 VTSAR,TSARD         ADD ELEMENT TO TSAR DATASET                  
         MVC   NSAVELS,TSPRECN     SAVE NUMBER OF RECORDS                       
         LA    R2,L'ACTKACT(R2)                                                 
         BCT   R0,JOTI22           TAKE NEXT SELECTED ACCOUNT                   
JOTI26   STC   R0,NSELACCS         SAVE NUMBER OF LEFT SEL ACCOUNTS             
         TM    SVFLAG,SVFLWJTB     TEST IF WE WERE HERE BEFORE                  
         BO    JOTI36              YES - SO ALL DONE                            
*                                                                               
         OI    SVFLAG,SVFLWJTB     MARK WE WERE AT JOB/TIME                     
         XC    KEY,KEY                                                          
         MVC   KEY(EVEKWC-EVEKEY),KEYSAVE                                       
         GOTO1 HIGH                READ ESTIMATE VERSION RECORD                 
         CLC   KEY(L'EVEKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                ESTIMATE RECORD NOT FOUND                    
         L     R6,AIO                                                           
         LA    R6,ACCORFST(R6)                                                  
         USING EDAELD,R6                                                        
JOTI30   CLI   EDAEL,0             TEST FOR END OF RECORD                       
         BE    JOTI36              WORK CODE ELEMENT NOT FOUND                  
         CLI   EDAEL,EDAELQ        LOOK FOR ESTIMATE DATA ELEMENT               
         BNE   JOTI32                                                           
         CLC   EDAWORK,SAVEWC      MATCH ON WORK CODE                           
         BNE   JOTI32                                                           
         CLC   EDASUBC,SAVEWC+L'EDAWORK  AND SUB WORK CODE                      
         BNE   JOTI32                                                           
         SP    SAVETOTA,EDACOMM    SUBTRACT WC AMOUNT FROM EST TOTAL            
         B     JOTI36                                                           
*                                                                               
JOTI32   IC    R0,EDALN            BUMP TO NEXT ESTIMATE DATA ELEMENT           
         AR    R6,R0                                                            
         B     JOTI30                                                           
*                                                                               
JOTI36   BAS   RE,SAVETSAR         SAVE THE TSAR DATASET                        
JOTI38   B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALREC LOGIC JOB/TIME                                              *          
**********************************************************************          
JOTI40   CLI   INTMODE,FSTLIST     TEST FIRST TIME THROUGH                      
         BE    JOTI44                                                           
         BAS   RE,REST             RESTORE THE TSAR DATASET                     
         BAS   RE,EDTSCR           EDIT THE SCREEN                              
         BE    JOTI44              SOMETHING EDIT                               
         BAS   RE,PROCPFTI         PROCES PFKEYS                                
*                                                                               
JOTI44   BAS   RE,DISP             DISPLAY THE SCREEN                           
         LA    R2,PRTACT1H         PUT CURSER AT FIRST ACTION FIELD             
         ST    R2,ACURFORC                                                      
         CLI   INTMODE,FSTLIST     TEST FIRST TIME THROUGH                      
         BNE   JOTI46              NO                                           
         SR    R0,R0                                                            
         ICM   R0,1,NSELACCS                                                    
         BZ    JOTI48              NO ACCOUNTS LEFT                             
         XC    LISTAR,LISTAR                                                    
         MVI   LISTAR,3                                                         
         CURED (R0),(2,LISTAR+1),0,ALIGN=LEFT                                   
         LA    R0,AP$ELADD         ACCOUNTS LEFT                                
         SR    RF,RF                                                            
         ICM   RF,4,GETMSYS                                                     
         GOTO1 GETTXT,DMCB,(R0),(0,CONHEADH),(C'I',0),0,LISTAR,(RF)             
         B     JOTIX                                                            
*                                                                               
JOTI46   CLI   PFKEY,PF10          PRINT REPORT?                                
         BE    JOTIX                                                            
         CLI   PFKEY,PF6           RECORD UPDATED?                              
         BE    JOTIX                                                            
         CLI   INTMODE,EDTLIST     WAS SOMETHING EDIT                           
         BNE   JOTI48                                                           
         LA    R0,AP$CHDUN         CHANGE COMPLETED                             
         CLI   LNLISTS,NLINES      TEST IF SCREEN FILLED                        
         BE    SCINFMSG            YES                                          
         LA    R0,AP$DCHND         CHANGED NO MORE DATA TO DISPLAY              
         B     SCINFMSG                                                         
JOTI48   LA    R0,AP$DISCM         MORE TO COME ENTER CHANGES                   
         CLI   LNLISTS,NLINES      TEST IF SCREEN FILLED                        
         BE    SCINFMSG            YES                                          
         LA    R0,AP$DISCH         DISPLAYED ENTER CHANGES                      
         B     SCINFMSG                                                         
*                                                                               
JOTIX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO DISPLAY THE PERSON TIME ELEMENTS FROM THE DATASET   *          
* ON THE SCREEN                                                      *          
* ON ENTRY ELEMENT CONTAINS THE FIRST ELEMENT TO DISPLAY             *          
**********************************************************************          
         USING LNJOTID,R2                                                       
         USING EPTELD,R6                                                        
DISP     NTR1  ,                                                                
         GOTO1 VCLEARF,DMCB,(0,PRTACT1H),PRTETOHH CLEAR SCREEN                  
         GOTO1 (RF),(R1),(1,PRTACT1H),PRTETOHH                                  
         LA    R2,PRTACT1H         R2=A(FIELD HEADER)                           
         LA    R1,PRTETOHH-1       R1=BXLE LIMIT                                
         SR    R0,R0               R0=INCREMENT=FIELD LENGTH                    
         OI    4(R2),X'20'         TURN ON ALL PREV VALID BITS                  
         NI    1(R2),X'FF'-X'20'   UNPROTECT ALL FIELDS                         
         IC    R0,0(R2)                                                         
         BXLE  R2,R0,*-12                                                       
         LA    R2,PRTACT1H         R2=A(FIRST ACTION FIELD)                     
         MVI   LNLISTS,0           RESET NUMBER OF LINES ON SCREEN              
         XC    FSTEL,FSTEL         CLEAR FIRST ELEMENT ON SCREEN                
         CLI   NSAVSCRS,0          ARE WE ON THE FIRST SCREEN?                  
         BH    *+10                NO                                           
         XC    ELEMENT,ELEMENT     YES - SO START AT THE VERY BEGINNING         
         CLI   INTMODE,FSTLIST     TEST FIRST TIME THROUGH                      
         BNE   DISP4                                                            
*                                                                               
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH       ACTION = READ HIGH                           
         LA    R6,ELEMENT                                                       
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         READ FIRST TSAR REC INTO ELEMENT             
         CLI   TSERRS,0                                                         
         BE    DISP8                                                            
         TM    TSERRS,TSEEOF       EMPTY DATASET?                               
         BO    DISPX                                                            
         CLI   TSERRS,TSERNF                                                    
         BE    DISP8                                                            
         DC    H'0'                                                             
*                                                                               
DISP4    LA    R6,ELEMENT          ELEMENT CONTAINS THE START ELEMENT           
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH       ACTION = READ HIGH                           
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         READ TSAR RECORD INTO ELEMENT                
         CLI   TSERRS,0                                                         
         BE    DISP8                                                            
         TM    TSERRS,TSEEOF       TEST END OF DATASET                          
         BO    DISPX                                                            
         CLI   TSERRS,TSERNF                                                    
         BE    DISP8                                                            
         DC    H'0'                                                             
*                                                                               
DISP8    TM    EPTINDS,INSLINE     DO WE HAVE TO INSERT A BLANK LINE?           
         BZ    DISP9               NO                                           
         SR    RF,RF               YES - BUMP TO NEXT LINE ON SCREEN            
         IC    RF,LNLISTS                                                       
         LA    RF,1(RF)                                                         
         STC   RF,LNLISTS          INCREASE NUMBER OF LINES ON SCREEN           
         CLI   LNLISTS,NLINES      TEST SCREEN FILLED                           
         BNL   DISPX               YES                                          
         LA    R2,LTILEN(R2)       BUMP TO NEXT LINE                            
         NI    EPTINDS,255-INSLINE SWITCH OFF INSERT MARKER                     
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAWRT       ACTION = WRITE                               
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         WRITE BACK THE UNMARKED ELEMENT              
*                                                                               
DISP9    MVC   LTICODE,EPTACT      PERSON ACCOUNT CODE                          
         OI    LTICODEH+1,X'20'    PROTECT FIELD                                
         CLI   EPTLN,EPTLN1Q       IS PERSON NAME OVERWRITEN?                   
         BH    *+18                YES                                          
         BAS   RE,GETACCNM         GET PERSON ACCOUNT NAME                      
         MVC   LTINAME,PACCNAME                                                 
         B     DISP10                                                           
         SR    RE,RE                                                            
         IC    RE,EPTLN                                                         
         SH    RE,=Y(EPTLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   LTINAME(0),EPTDESC  EXTRACT OVERWRITEN PERSON ACC NAME           
DISP10   OC    EPTRDATE,EPTRDATE   DO WE HAVE A DATE?                           
         BZ    DISP12              NO                                           
         GOTO1 DATCON,DMCB,(1,EPTRDATE),(10,LTIDATE)                            
         OI    LTIDATEH+1,X'20'    PROTECT FIELD                                
DISP12   CURED (P4,EPTHOURS),(L'LTIHOUR,LTIHOUR),2,ZERO=BLANK                   
         CURED (P4,EPTRATE),(L'LTIRATE,LTIRATE),2,ZERO=BLANK                    
         OC    FSTEL,FSTEL                                                      
         BNZ   *+10                                                             
         MVC   FSTEL,EPTUNT        SAVE FIRST ELEMENT ON SCREEN                 
         MVC   LASTEL,EPTUNT       SAVE LAST ELEMENT ON SCREEN                  
         SR    RF,RF                                                            
         IC    RF,LNLISTS                                                       
         LA    RF,1(RF)                                                         
         STC   RF,LNLISTS          INCREASE NUMBER OF LINES ON SCREEN           
         CLI   LNLISTS,NLINES      TEST SCREEN FILLED                           
         BNL   DISPX               YES                                          
         LA    R2,LTILEN(R2)       BUMP TO NEXT LINE ON SCREEN                  
         LA    RF,TSARKLEN         SET NEW KEY FOR READ HIGH TSAR               
         LA    RF,ELEMENT+1(RF)                                                 
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)                                                         
         B     DISP4                                                            
*                                                                               
DISPX    CURED (P8,WCTOTAL),(L'PRTWCTO,PRTWCTO),2,ALIGN=LEFT                    
         CURED (P6,TOTHOURS),(L'PRTTITO,PRTTITO),2,ALIGN=LEFT                   
         ZAP   DUB,SAVETOTA                                                     
         AP    DUB,WCTOTAL                                                      
         CURED (P8,DUB),(L'PRTETOT,PRTETOT),2,ALIGN=LEFT                        
         OI    PRTWCTOH+6,X'80'                                                 
         OI    PRTETOTH+6,X'80'                                                 
         OI    PRTTITOH+6,X'80'                                                 
         BAS   RE,SAVETSAR                                                      
         B     XIT                                                              
         SPACE 1                                                                
GETACCNM NTR1  ,                                                                
         MVC   PACCNAME,SPACES                                                  
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKULA,EPTULA                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACTKEY),KEYSAVE                                            
         BE    *+14                                                             
         MVC   PACCNAME,AC@MSNG                                                 
         B     GETACNMX                                                         
         MVI   ELCODE,NAMELQ       LOOK FOR ACCOUNT NAME                        
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NAMELD,R6                                                        
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         LTR   RE,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                BAD NAME ELEMENT                             
         EX    RE,*+4                                                           
         MVC   PACCNAME(0),NAMEREC                                              
GETACNMX B     XIT                                                              
         DROP  R1,R2,R4,R6                                                      
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO EDIT JOB/TIME SCREEN                                *          
**********************************************************************          
EDTSCR   NTR1  ,                                                                
         LA    R2,PRTACT1H         R2=A(FIRST ACTION FIELD)                     
         LA    R1,PRTETOHH                                                      
         BCTR  R1,0                R1=BXLE LIMIT                                
         SR    R0,R0               R0=INCREMENT REGISTER                        
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BZ    EDTSC02             YES                                          
         IC    R0,0(R2)            GET FIELD LENGTH                             
         BXLE  R2,R0,*-12                                                       
         B     NOXIT               NOTHING CHANGED                              
*                                                                               
         USING LNJOTID,R2                                                       
EDTSC02  LA    R2,PRTACT1H         R2=A(FIRST ACTION FIELD)                     
         SR    R0,R0                                                            
         ICM   R0,1,LNLISTS        R0=NUMBER OF LINES ON SCREEN                 
         BZ    EDTSC30             NOTHING ON SCREEN                            
*                                  AT FIRST CHECK THE SYNTAX                    
EDTSC04  TM    LTIACTH+4,X'20'     TEST CHANGE IN ACTION FIELD                  
         BO    EDTSC08             NO - NEXT LIST ENTRY                         
         CLI   LTIACTH+5,0         TEST ANY INPUT                               
         BE    EDTSC08             NO - NEXT LIST ENTRY                         
         MVI   ERROR,INVALID                                                    
         CLI   LTIACTH+5,2         TEST INPUT LENGTH                            
         BH    ERRENDTI                                                         
         BL    EDTSC06                                                          
         CLC   LTIACT(2),AC@CLEAR  CLEAR ALL?                                   
         BE    EDTSC18                                                          
         CLC   LTIACT(1),AC@DEL    DELETE ALL UNDERNEETH THIS LINE?             
         BNE   ERRENDTI                                                         
         CLI   LTIACT+1,C'+'                                                    
         BE    EDTSC10                                                          
         B     ERRENDTI                                                         
EDTSC06  CLC   LTIACT(1),AC@DEL    DELETE?                                      
         BE    EDTSC08                                                          
         CLC   LTIACT(1),AC@INSRT  INSERT?                                      
         BE    EDTSC08                                                          
         CLC   LTIACT(1),AC@RPLIC  REPLICATE?                                   
         BE    EDTSC08                                                          
         B     ERRENDTI                                                         
*                                                                               
EDTSC08  LA    R2,LTILEN(R2)       BUMP TO NEXT LINE                            
         BCT   R0,EDTSC04                                                       
*                                  SYNTAX IS OK SO DO THE CHANGE                
EDTSC10  LA    R2,PRTACT1H         R2=A(FIRST ACTION FIELD)                     
         SR    R0,R0                                                            
         IC    R0,LNLISTS          R0=NUMBER OF LINES ON SCREEN                 
*                                                                               
EDTSC12  TM    LTIACTH+4,X'20'     TEST CHANGE IN ACTION FIELD                  
         BO    EDTSC16             NO - NEXT LIST ENTRY                         
         CLI   LTIACTH+5,0         TEST ANY INPUT                               
         BE    EDTSC16             NO - NEXT LIST ENTRY                         
         TM    LTICODEH+1,X'20'    TEST ACCOUNT FIELD IS PROTECTED              
         BO    EDTSC14             YES - SO SUB ACTION IS POSSIBLE              
         CLC   LTIACT(1),AC@DEL    ALLOW DELETE AN ERROR LINE?                  
         BE    EDTSC16             LEAVE DEL ACTION IN SUB ACT COLUMN           
         MVC   LTIACT,SPACES       DELETE INPUT                                 
         OI    LTIACTH+4,X'20'     MARK LINE AS PROCESSED                       
         OI    LTIACTH+6,X'80'     TRANSMIT THE FIELD                           
         B     EDTSC16             NEXT LIST ENTRY                              
EDTSC14  CLC   LTIACT(1),AC@DEL    DELETE?                                      
         BE    EDTSC20                                                          
         CLC   LTIACT(1),AC@INSRT  INSERT?                                      
         BE    EDTSC20                                                          
         CLC   LTIACT(1),AC@RPLIC  REPLICATE?                                   
         BE    EDTSC28                                                          
         B     ERRENDTI                                                         
*                                                                               
EDTSC16  LA    R2,LTILEN(R2)       BUMP TO NEXT LINE                            
         BCT   R0,EDTSC12                                                       
         B     EDTSC30                                                          
*                                                                               
EDTSC18  BAS   RE,INIT             CLEAR ALL                                    
         MVI   NSAVSCRS,0          RESET NUMBER OF SAVED SCREENS                
         XC    NSAVELS,NSAVELS     RESET NUMBER OF SAVED ELEMENTS               
         XC    FSTEL,FSTEL                                                      
         XC    LASTEL,LASTEL                                                    
         ZAP   WCTOTAL,=P'0'       INITIALIZE WORK CODE TOTAL AMOUNT            
         ZAP   TOTHOURS,=P'0'      INITIALIZE WORK CODE TOTAL HOURS             
         B     EDTSCRX                                                          
*                                                                               
         USING EPTELD,R6                                                        
EDTSC20  LA    R6,ELEMENT          GET ELEMENT FOR INSERT/DELETE                
         XC    ELEMENT,ELEMENT     BUILD TSAR RECORD IN ELEMENT                 
         MVC   EPTUNT(L'EPTUNT+L'EPTLDG),CTRPLDG                                
         MVC   EPTACT,LTICODE      TAKE ACCOUNT CODE FROM SCREEN                
         OC    EPTACT,SPACES                                                    
         TM    LTIDATEH+1,X'20'    TEST DATE FIELD PROTECTED                    
         BZ    EDTSC22             NO - SO WE HAD NO DATE BEFORE                
         GOTO1 DATCON,DMCB,(4,LTIDATE),(1,EPTRDATE)                             
EDTSC22  LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH       ACTION = READ HIGH                           
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         READ THE ELEMENT                             
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    LTIACTH+4,X'20'     MARK LINE AS PROCESSED                       
         CLC   LTIACT(1),AC@INSRT  INSERT?                                      
         BE    EDTSC26                                                          
EDTSC24  ZAP   DUB,EPTRATE         DELETE A LINE                                
         MP    DUB,EPTHOURS                                                     
         SRP   DUB,64-2,5                                                       
         SP    WCTOTAL,DUB         SUPTRACT AMOUNTS                             
         SP    TOTHOURS,EPTHOURS     AND HOURS                                  
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSADEL       ACTION = DELETE                              
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         DELETE THE ELEMENT                           
         MVC   NSAVELS,TSPRECN     SAVE NUMBER OF RECORDS                       
         CLI   LTIACTH+5,2         TEST FOR 'D+'                                
         BL    EDTSC16             NO - BUMP TO NEXT LINE                       
         LA    RF,TSARKLEN         SET NEW KEY FOR READ HIGH TSAR               
         LA    RF,ELEMENT+1(RF)                                                 
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)                                                         
         LA    R6,ELEMENT                                                       
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH       ACTION = READ HIGH                           
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         READ TSAR RECORD INTO ELEMENT                
         CLI   TSERRS,0                                                         
         BE    EDTSC24                                                          
         CLI   TSERRS,TSERNF                                                    
         BE    EDTSC24                                                          
         TM    TSERRS,TSEEOF       TEST END OF DATASET                          
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   LTIACT(1),AC@DEL    MARK LINES AS DELETED                        
         OI    LTIACTH+4,X'20'     MARK LINE AS PROCESSED                       
         LA    R2,LTILEN(R2)       BUMP TO NEXT LINE                            
         BCT   R0,*-14                                                          
         B     EDTSC30                                                          
*                                                                               
EDTSC26  OI    EPTINDS,INSLINE     INSERT A LINE                                
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAWRT       ACTION = WRITE                               
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         WRITE BACK THE MARKED ELEMENT                
         B     EDTSC16             BUMP TO NEXT LINE                            
*                                                                               
EDTSC28  CLI   NSAVELS+1,MAXELS    TEST MAXIMUM OF ELEMENTS ON RECORD           
         BL    *+12                NO                                           
         MVI   ERROR,MAXLIUSE      YES - CAN NOT REPLICATE ONE                  
         B     ERRENDTI                                                         
         TM    LTIDATEH+1,X'20'    TEST DATE FIELD PROTECTED                    
         BO    *+12                                                             
         MVI   ERROR,RECXIST       NO - SO IT IS A BLANK ONE ALLREADY           
         B     ERRENDTI                                                         
         LA    R6,ELEMENT          REPLICATE A LINE                             
         XC    ELEMENT,ELEMENT     BUILD TSAR RECORD IN ELEMENT                 
         MVC   EPTUNT(L'EPTUNT+L'EPTLDG),CTRPLDG                                
         MVC   EPTACT,LTICODE      TAKE ACCOUNT CODE FROM SCREEN                
         OC    EPTACT,SPACES                                                    
         GOTO1 DATCON,DMCB,(4,LTIDATE),(1,EPTRDATE)                             
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH       ACTION = READ HIGH                           
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         READ THE ELEMENT                             
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   EPTHOURS,=P'0'      SET HOURS                                    
         ZAP   EPTRATE,=P'0'         AND RATE TO ZERO                           
         XC    EPTRDATE,EPTRDATE   CLEAR DATE                                   
         MVI   EPTINDS,0             AND INDICATOR                              
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAADD                                                    
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         ADD NEW ELEMENT TO DATASET                   
         MVC   NSAVELS,TSPRECN     SAVE NUMBER OF RECORDS                       
         LA    RF,PRTACT1H                                                      
         CR    R2,RF               TEST IF WE ARE IN THE FIRST LINE             
         BH    *+10                NO                                           
         MVC   FSTEL,EPTUNT        YES - RESET FIRST ELEMENT ON SCREEN          
         B     EDTSC16             BUMP TO NEXT LINE                            
*                                                                               
EDTSC30  MVI   PFLAG,0             INITIALIZE PROGRAM FLAG                      
         LA    R2,PRTACT1H         SET TO FIRST LINE                            
         LA    R0,NLINES           TAKE NUMBER OF SCREEN LINES                  
*                                                                               
EDTSC32  CLC   LTIACT(1),AC@DEL    TEST IF LINE WAS DELETED                     
         BE    EDTSC90             YES - BUMP TO NEXT LINE                      
         SR    RE,RE                                                            
         LA    R1,FIELDS           NO OF INPUT FIELDS ON SCREEN LINE            
         LA    RF,LTICODEH         TEST FOR ANY CHANGES IN LINE                 
         TM    4(RF),X'20'                                                      
         BZ    EDTSC33                                                          
         IC    RE,0(RF)                                                         
         AR    RF,RE                                                            
         BCT   R1,*-14                                                          
         B     EDTSC90             NO CHANGES IN LINE BUMP TO NEXT              
*                                                                               
EDTSC33  TM    LTICODEH+1,X'20'    TEST FIELD IS PROTECTED                      
         BO    EDTSC34             YES - NO CHANGE POSSIBLE                     
         TM    LTICODEH+4,X'20'    TEST CHANGE IN ACC CODE FIELD                
         BO    EDTSC34             NO                                           
         CLI   LTICODEH+5,0        TEST ACCOUNT CODE WAS DELETED                
         BE    EDTSC90             YES - SO IGNORE HOLE LINE                    
         CLI   NSAVELS+1,MAXELS    TEST MAXIMUM OF ELEMENTS ON RECORD           
         BL    *+16                YES -  CAN NOT ADD ANOTHER ONE               
         LA    R2,LTICODEH                                                      
         MVI   ERROR,MAXLIUSE                                                   
         B     ERRENDTI                                                         
         BAS   RE,CHEPACC          CHECK PERSON ACCOUNT                         
         OI    PFLAG,PFLNEWEL      MARK AS NEW ELEMENT                          
*                                                                               
EDTSC34  TM    LTINAMEH+4,X'20'    TEST CHANGE IN DESCRIPTION FIELD             
         BO    EDTSC38             NO                                           
         CLI   LTINAMEH+5,0                                                     
         BNE   EDTSC36                                                          
         TM    PFLAG,PFLNEWEL      TEST FOR NEW ELEMENT                         
         BO    EDTSC38             YES - ALL DONE                               
         OI    PFLAG,PFLDELDO      DELETE OVERWRITEN DESCRIPTION                
         B     EDTSC38                                                          
EDTSC36  CLC   LTICODE,SPACES      TEST IF WE HAVE AN ACCOUNT                   
         BNH   ERRNOACC            NO - ERROR ACCOUNT MISSING                   
         OI    PFLAG,PFLDESOV      MARK AS DESCIPTION OVERWRITEN                
*                                                                               
EDTSC38  CLI   LTIDATEH+5,0        TEST ANY DATE INPUT                          
         BE    *+14                NO - SO USE TODAYS DATE                      
         CLC   LTICODE,SPACES      TEST IF WE HAVE AN ACCOUNT                   
         BNH   ERRNOACC            NO - ERROR ACCOUNT MISSING                   
         MVC   BYTE,AGYLANG        LANGUAGE                                     
         OI    BYTE,PVINSGLO       SINGLE DATE ONLY                             
         OI    BYTE,PVINSGLS       TREAT SINGLE DATE AS SINGLE                  
         LA    RF,L'LTIDATE        GET LENGTH OF DATE FIELD                     
         TM    LTIDATEH+1,X'20'    TEST DATE FIELD IS PROTECTED                 
         BO    *+8                                                              
         IC    RF,LTIDATEH+5       NO - GET INPUT LENGTH                        
         GOTO1 PERVAL,DMCB,((RF),LTIDATE),(BYTE,WORK)                           
         CLI   4(R1),PVRCINV1      TEST IF DATE NOT OK                          
         BNE   *+16                                                             
         MVI   ERROR,INVDATE                                                    
         LA    R2,LTIDATEH                                                      
         B     ERRENDTI                                                         
         MVC   NDATE,WORK+(PVALPSTA-PERVALD)  SAVE DATE                         
*                                                                               
EDTSC42  ZAP   NHOUR,=P'0'         RESET NEW HOURS                              
         TM    LTIHOURH+4,X'20'    TEST CHANGE IN HOURS FIELD                   
         BO    EDTSC46             NO                                           
         OI    PFLAG,PFLHOUWI      HOURS WERE INPUT                             
         SR    RF,RF                                                            
         ICM   RF,1,LTIHOURH+5     RF = LENGTH OF INPUT                         
         BZ    EDTSC46             NO INPUT                                     
         CLC   LTICODE,SPACES      TEST IF WE HAVE AN ACCOUNT                   
         BNH   ERRNOACC            NO - ERROR ACCOUNT MISSING                   
         GOTO1 CASHVAL,DMCB,(X'82',LTIHOUR),(RF)                                
         CLI   0(R1),X'FF'         TEST FOR ERROR                               
         BNE   *+16                                                             
         MVI   ERROR,INVALID                                                    
         LA    R2,LTIHOURH                                                      
         B     ERRENDTI                                                         
         MVI   ERROR,NOTOK                                                      
         ZAP   DUB,4(8,R1)                                                      
         CP    DUB,=P'0'           TEST NEGATIV INPUT                           
         BL    *+14                YES - NOT ALLOWED                            
         CP    DUB,MAXHOURS        TEST MAXIMUM HOURS                           
         BNH   *+12                                                             
         LA    R2,LTIHOURH                                                      
         B     ERRENDTI                                                         
         ZAP   NHOUR,DUB                                                        
*                                                                               
EDTSC46  ZAP   NRATE,=P'0'         RESET NEW RATE                               
         TM    LTIRATEH+4,X'20'    TEST CHANGE IN RATE FIELD                    
         BO    EDTSC50             NO                                           
         OI    PFLAG,PFLRATWI      RATE WAS INPUT                               
         SR    RF,RF                                                            
         ICM   RF,1,LTIRATEH+5     RF = LENGTH OF INPUT                         
         BZ    EDTSC50             NO INPUT                                     
         CLC   LTICODE,SPACES      TEST IF WE HAVE AN ACCOUNT                   
         BNH   ERRNOACC            NO - ERROR ACCOUNT MISSING                   
         TM    PFLAG,PFLHOUWI      TEST IF WE HAVE INPUT HOURS                  
         BO    *+12                YES                                          
         CLI   LTIHOURH+5,0        TEST IF WE ALLREADY HAD HOURS                
         B     *+10                                                             
         CP    NHOUR,=P'0'         TEST IF WE HAVE HOURS                        
         BE    ERRNOHRS            NO - ERROR HOURS MISSING                     
         GOTO1 CASHVAL,DMCB,(X'82',LTIRATE),(RF)                                
         CLI   0(R1),X'FF'         TEST FOR ERROR                               
         BNE   *+16                                                             
         MVI   ERROR,INVALID                                                    
         LA    R2,LTIRATEH                                                      
         B     ERRENDTI                                                         
         MVI   ERROR,NOTOK                                                      
         ZAP   DUB,4(8,R1)                                                      
         CP    DUB,=P'0'           TEST NEGATIV INPUT                           
         BL    *+14                YES - NOT ALLOWED                            
         CP    DUB,MAXRATE         TEST MAXIMUM RATE                            
         BNH   *+12                                                             
         LA    R2,LTIRATEH                                                      
         B     ERRENDTI                                                         
         ZAP   NRATE,DUB                                                        
*                                                                               
         USING EPTELD,R6                                                        
EDTSC50  LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVC   EPTUNT(L'EPTUNT+L'EPTLDG),CTRPLDG                                
         MVC   EPTACT,LTICODE      TAKE ACCOUNT CODE FROM SCREEN                
         OC    EPTACT,SPACES                                                    
         TM    PFLAG,PFLNEWEL      TEST FOR NEW ELEMENT                         
         BO    EDTSC70                                                          
         TM    LTIDATEH+1,X'20'    TEST DATE FIELD PROTECTED                    
         BZ    *+10                NO - SO WE HAD NO DATE BEFORE                
         MVC   EPTRDATE,NDATE                                                   
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH                                                    
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         READ ELEMENT                                 
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    LTIDATEH+1,X'20'    TEST DATE FIELD PROTECTED                    
         BO    EDTSC56             YES - SO WE HAD A DATE BEFORE                
         CP    NHOUR,=P'0'         TEST IF WE HAVE HOURS                        
         BNE   EDTSC70             YES - SO ADD A NEW ELEMENT                   
*                                  NOW WE ARE DEALING WITH A CHANGE IN          
*                                   THE DESCRIPTION OR DATE FIELD OF            
*                                    AN EMPTY ELEMENT                           
         TM    LTINAMEH+4,X'20'    TEST CHANGE IN DESCRIPTION FIELD             
         BO    EDTSC54             NO - SO A DATE WAS INPUT                     
         SR    RF,RF                                                            
         ICM   RF,1,LTINAMEH+5     GET LENGTH OF DESCIPTION                     
         BNZ   *+18                                                             
         XC    EPTDESC,EPTDESC     NO INPUT SO DELETE THE PREV ONE              
         LA    RF,EPTLN1Q          AND SET THE SHORT LENGTH                     
         B     EDTSC52                                                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   EPTDESC(0),LTINAME  TAKE NEW DESCRIPTION                         
         LA    RF,EPTLN1Q+1(RF)    AND SET LONG LENGTH                          
EDTSC52  STC   RF,EPTLN                                                         
EDTSC54  CLC   LTIDATE,SPACES      TEST INPUT IN DATE FIELD                     
         BNH   EDTSC66             NO                                           
         MVC   EPTRDATE,NDATE      YES - GET NEW DATE                           
         B     EDTSC80             ADD NEW ELEMENT AND DEL THE OLD ONE          
*                                                                               
EDTSC56  TM    PFLAG,PFLHOUWI      TEST IF HOURS WERE INPUT                     
         BO    *+10                YES                                          
         ZAP   NHOUR,EPTHOURS      NO - SO TAKE OLD NUMBER OF HOURS             
         CP    NHOUR,=P'0'         TEST IF WE HAVE HOURS                        
         BNE   *+18                YES                                          
         ZAP   NRATE,=P'0'         NO - SET HOURS AND RATE TO ZERO              
         OI    PFLAG,PFLRATWR      SIMULATE RATE WAS READ                       
         B     EDTSC58                                                          
         TM    PFLAG,PFLRATWI      TEST IF A RATE WAS INPUT                     
         BZ    *+18                NO                                           
         CP    NRATE,=P'0'         TEST IF WE HAVE TO READ THE RATE             
         BNE   EDTSC58             NO - RATE WAS INPUT                          
         B     EDTSC57             YES - READ THE RATE                          
         TM    EPTINDS,EPTIOVER    TEST IF RATE WAS INPUT PREVIOSLY             
         BZ    EDTSC57             NO - SO READ THE RATE                        
         ZAP   NRATE,EPTRATE       YES - TAKE OLD RATE FROM ELEMENT             
         B     EDTSC58                                                          
EDTSC57  MVC   RATEACC,LTICODE                                                  
         BAS   RE,GETRATE          GET THE PERSON RATE                          
         CP    NRATE,=P'0'                                                      
         BE    ERRNORAT            NO RATE FOUND                                
EDTSC58  TM    PFLAG,PFLDESOV      TEST FOR DESCRIPTION OVERWRITE               
         BZ    EDTSC60             NO                                           
         SR    RF,RF                                                            
         IC    RF,LTINAMEH+5       GET LENGTH OF DESCIPTION                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   EPTDESC(0),LTINAME  TAKE NEW DESCRIPTION                         
         LA    RF,EPTLN1Q+1(RF)    AND SET LONG LENGTH                          
         B     EDTSC62                                                          
EDTSC60  TM    PFLAG,PFLDELDO      TEST FOR DELETE DESC. OVERWRITEN             
         BZ    EDTSC64             NO - SO LEAVE LENGTH AS IT WAS               
         XC    EPTDESC,EPTDESC     NO INPUT SO DELETE THE PREV ONE              
         LA    RF,EPTLN1Q          AND SET THE SHORT LENGTH                     
EDTSC62  STC   RF,EPTLN                                                         
*                                                                               
EDTSC64  ZAP   DUB,EPTRATE         SUPTRACT OLD VALUES FROM TOTALS              
         MP    DUB,EPTHOURS                                                     
         SRP   DUB,64-2,5                                                       
         SP    WCTOTAL,DUB         SUPTRACT AMOUNT                              
         SP    TOTHOURS,EPTHOURS     AND HOURS                                  
         ZAP   EPTHOURS,NHOUR      TAKE HOURS                                   
         ZAP   EPTRATE,NRATE       AND RATE                                     
         ZAP   DUB,EPTRATE         ADD NEW VALUES TO TOTALS                     
         MP    DUB,EPTHOURS                                                     
         SRP   DUB,64-2,5                                                       
         AP    WCTOTAL,DUB         ADD AMOUNT                                   
         AP    TOTHOURS,EPTHOURS     AND HOURS                                  
         TM    PFLAG,PFLRATWR      TEST RATE WAS READ                           
         BO    *+12                YES                                          
         OI    EPTINDS,EPTIOVER    MARK AS RATE WAS OVERWRITEN                  
         B     EDTSC66                                                          
         NI    EPTINDS,255-EPTIOVER  SWITCH OFF RATE OVERWRITEN BIT             
*                                                                               
EDTSC66  LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAWRT       ACTION = WRITE                               
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         WRITE BACK THE CHANGED ELEMENT               
         B     EDTSC88             MARK LINE AS EDITED AND BUMP TO NEXT         
*                                                                               
EDTSC70  CP    NHOUR,=P'0'         TEST IF WE HAVE HOURS                        
         BNE   *+12                YES - SO DATE IS REQUIRED                    
         CLI   LTIDATEH+5,0        TEST IF A DATE WAS INPUT                     
         BE    *+10                NO - SO IT IS A NEW BLANK ELEMENT            
         MVC   EPTRDATE,NDATE                                                   
         TM    PFLAG,PFLDESOV      TEST IF DESCRIPTION WAS OVERWRITEN           
         BZ    EDTSC72             NO                                           
         SR    RF,RF                                                            
         IC    RF,LTINAMEH+5       GET LENGTH OF DESCIPTION                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   EPTDESC(0),LTINAME  TAKE NEW DESCRIPTION                         
         LA    RF,EPTLN1Q+1(RF)    AND SET LONG LENGTH                          
         STC   RF,EPTLN                                                         
         B     EDTSC74                                                          
EDTSC72  TM    PFLAG,PFLNEWEL      TEST FOR NEW ELEMENT                         
         BO    *+12                YES - GO FURTHER                             
         TM    PFLAG,PFLDELDO      TEST FOR DELETE DESC. OVERWRITEN             
         BZ    *+8                 NO - SO TAKE THE OLD LENGTH                  
         MVI   EPTLN,EPTLN1Q                                                    
EDTSC74  CP    NHOUR,=P'0'         DO WE HAVE HOURS?                            
         BE    EDTSC80             NO - SO IT IS A NEW BLANK ELEMENT            
         CP    NRATE,=P'0'         TEST IF WE HAVE TO READ THE RATE             
         BNE   EDTSC78                                                          
         MVC   RATEACC,LTICODE                                                  
         BAS   RE,GETRATE          READ PERSON CHARGE RATE                      
         CP    NRATE,=P'0'                                                      
         BE    ERRNORAT            NO RATE FOUND                                
EDTSC78  TM    PFLAG,PFLRATWR      TEST RATE WAS READ                           
         BO    *+12                YES                                          
         OI    EPTINDS,EPTIOVER    MARK AS RATE WAS OVERWRITEN                  
         B     EDTSC80                                                          
         NI    EPTINDS,255-EPTIOVER  SWITCH OFF RATE OVERWRITEN BIT             
*                                                                               
         USING TSARD,R1                                                         
EDTSC80  ZAP   EPTHOURS,NHOUR      TAKE HOURS                                   
         ZAP   EPTRATE,NRATE       AND RATE                                     
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAADD       ACTION = ADD                                 
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         ADD NEW ELEMENT                              
         TM    TSERRS,TSEDUP       TEST FOR DUPLICATE KEY ON ADD                
         BZ    *+16                NO                                           
         LA    R2,LTIDATEH                                                      
         MVI   ERROR,RECXIST       ELEMENT ALREADY EXISTS                       
         B     ERRENDTI                                                         
         MVC   NSAVELS,TSPRECN     SAVE NUMBER OF RECORDS                       
         ZAP   DUB,EPTRATE         ADD NEW VALUES TO TOTALS                     
         MP    DUB,EPTHOURS                                                     
         SRP   DUB,64-2,5                                                       
         AP    WCTOTAL,DUB         ADD AMOUNTS                                  
         AP    TOTHOURS,EPTHOURS     AND HOURS                                  
*                                                                               
         TM    PFLAG,PFLNEWEL      TEST FOR NEW ELEMENT                         
         BO    EDTSC88             YES - ALL DONE                               
         XC    EPTRDATE,EPTRDATE                                                
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSARDH       ACTION = READ HIGH                           
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         READ THE OLD EMPTY ELEMENT                   
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSADEL       ACTION = DELETE                              
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         DELETE THE OLD EMPTY ELEMENT                 
         MVC   NSAVELS,TSPRECN     SAVE NUMBER OF RECORDS                       
         B     EDTSC88             MARK LINE AS EDITED AND BUMP TO NEXT         
*                                                                               
EDTSC88  SR    RE,RE               MARK WHOLE LINE AS EDITED                    
         LA    R1,FIELDS           NO OF INPUT FIELDS ON SCREEN LINE            
         LA    RF,LTICODEH                                                      
         OI    4(RF),X'20'                                                      
         IC    RE,0(RF)                                                         
         AR    RF,RE                                                            
         BCT   R1,*-10                                                          
         B     EDTSC90                                                          
*                                                                               
EDTSC90  LA    R2,LTILEN(R2)       BUMP TO NEXT LINE                            
         BCT   R0,EDTSC32                                                       
         B     EDTSCRX                                                          
*                                                                               
EDTSCRX  MVI   INTMODE,EDTLIST     SOMETHING CHANGED                            
         XC    ELEMENT,ELEMENT     SET FIRST ELEMENT FOR REDISPLAY              
         MVC   ELEMENT+2(L'FSTEL),FSTEL                                         
         BAS   RE,SAVETSAR                                                      
         B     YESXIT                                                           
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO PROCESS ANY PFKEY FROM JOB/TIME SCREEN              *          
**********************************************************************          
PROCPFTI NTR1  ,                                                                
         CLI   PFKEY,0                                                          
         BE    TIPFK8              NO PFKEY SCROLL FORWARD                      
*        CLI   PFKEY,PF2                                                        
*        BE    TIPFK2              GO TO JOB/STAFF                              
         CLI   PFKEY,PF4                                                        
         BE    TIPFK4              REFRESH THE PERSON RATES                     
         CLI   PFKEY,PF6                                                        
         BE    TIPFK6              UPDATE THE RECORDS                           
         CLI   PFKEY,PF7                                                        
         BE    TIPFK7              SCROLL BACKWARD                              
         CLI   PFKEY,PF8                                                        
         BE    TIPFK8              SCROLL FORWARD                               
         CLI   PFKEY,PF10                                                       
         BE    TIPFK10             PRINT REPORT                                 
         CLI   PFKEY,PF12                                                       
         BNE   *+14                                                             
         MVC   BUFF(2),=X'5ABD'    SET FLAG FOR ACPRO32                         
         B     TIPFKX              GO BACK TO JOB EST                           
         MVI   ERROR,WRONGKEY      NO VALID PFKEY                               
         LA    R2,PRTACT1H                                                      
         B     ERRENDTI                                                         
*                                                                               
*IPFK2   GOTO1 VTRANSF,WORK,RECNJOB,ACTNSTAF,0  GO TO JOB/STAFF                 
*                                                                               
         USING EPTELD,R6                                                        
TIPFK4   XC    ELEMENT,ELEMENT     REFRESH THE PERSON CHARGE RATES              
         LA    R6,ELEMENT                                                       
         USING TSARD,R1                                                         
TIPFK4A  LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSARDH       ACTION = READ HIGH                           
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         READ TSAR RECORD INTO ELEMENT                
         TM    TSERRS,TSEEOF       TEST END OF DATASET                          
         BO    TIPFK4X             YES - ALL DONE                               
         TM    EPTINDS,EPTIOVER    TEST RATE WAS OVERWRITEN                     
         BO    TIPFK4B             YES - SO DON'T READ THE RATE                 
         CP    EPTHOURS,=P'0'      DO WE NEED A RATE?                           
         BE    TIPFK4B             NO - BUMP TO NEXT RECORD                     
*                                                                               
         ZAP   DUB,EPTRATE                                                      
         MP    DUB,EPTHOURS                                                     
         SRP   DUB,64-2,5                                                       
         SP    WCTOTAL,DUB         SUPTRACT AMOUNT FROM WC TOTAL                
         MVC   RATEACC,EPTACT      TAKE PERSON ACCOUNT CODE                     
         MVC   NDATE,EPTRDATE      DATE                                         
         ZAP   NRATE,EPTRATE       OLD RATE (IF WE DON'T FIND A NEW)            
         BAS   RE,GETRATE          LOOK FOR A RATE                              
         ZAP   EPTRATE,NRATE       GET NEW RATE IF ONE FOUND                    
         ZAP   DUB,EPTRATE                                                      
         MP    DUB,EPTHOURS                                                     
         SRP   DUB,64-2,5                                                       
         AP    WCTOTAL,DUB         ADD AMOUNT BACK TO WC TOTAL                  
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAWRT       ACTION = WRITE                               
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         WRITE BACK UPDATED RECORD                    
*                                                                               
TIPFK4B  LA    RF,TSARKLEN         SET NEW KEY FOR READ HIGH TSAR               
         LA    RF,ELEMENT+1(RF)                                                 
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)                                                         
         B     TIPFK4A                                                          
*                                                                               
TIPFK4X  BAS   RE,SAVETSAR         SAVE TSAR DATASET                            
         MVI   NSAVSCRS,0          START DISPLAY FROM BEGINNING                 
         B     TIPFKX                                                           
*                                                                               
TIPFK6   BAS   RE,UPDATE           UPDATE TIME EST RECORDS                      
         XC    LISTAR,LISTAR                                                    
         MVI   LISTAR,L'SAVEWC+2                                                
         MVC   LISTAR+1(2),SAVEWC  SET WORK CODE                                
         SR    R0,R0                                                            
         ICM   R0,1,SAVEWC+2       DO WE HAVE A SUB WORK CODE?                  
         BZ    TIPFK6A             NO                                           
         CURED (R0),(2,LISTAR+3),0,ALIGN=LEFT                                   
TIPFK6A  LA    R4,LISTAR+L'SAVEWC+3                                             
         MVC   0(1,R4),SAVEST                                                   
         CLI   SAVEST,EVEKTREV     TEST FOR REVISION ESTIMATE                   
         BNE   *+10                NO - NOTHING TO DO                           
         MVC   0(1,R4),CTRESTR     DIFFERENT LETTERS FOR REVISION EST           
         LA    R4,1(R4)                                                         
         SR    R0,R0                                                            
         IC    R0,SAVEST+1                                                      
         CURED (R0),(3,(R4)),0,ALIGN=LEFT                                       
         AR    R4,R0                                                            
         LA    RE,LISTAR+L'SAVEWC+2                                             
         SR    R4,RE                                                            
         STC   R4,LISTAR+L'SAVEWC+2                                             
         LA    R0,AP$WCCHA         WORKCODE AA CHANGED ON ESTIMATE YY           
         SR    RF,RF                                                            
         ICM   RF,4,GETMSYS                                                     
         GOTO1 GETTXT,DMCB,(R0),(0,CONHEADH),(C'I',0),0,LISTAR,(RF)             
         MVI   NSAVSCRS,0          START DISPLAY FROM BEGINNING                 
         B     TIPFKX                                                           
*                                                                               
TIPFK7   XC    ELEMENT,ELEMENT     SCROLL ONE SCREEN BACK                       
         SR    R1,R1                                                            
         ICM   R1,1,NSAVSCRS       GET NUMBER OF SAVED SCREENS                  
         BZ    TIPFKX              WE ARE ON THE FIRST SCREEN                   
         BCTR  R1,0                                                             
         STC   R1,NSAVSCRS                                                      
         MH    R1,=Y(L'LASTEL)                                                  
         LA    RF,SAVELS                                                        
         AR    RF,R1                                                            
         MVC   ELEMENT+2(L'LASTEL),0(RF)                                        
         XC    0(L'LASTEL,RF),0(RF)                                             
         B     TIPFKX                                                           
*                                                                               
TIPFK8   XC    ELEMENT,ELEMENT     SCROLL ONE SCREEN FORWARD                    
         CLI   LNLISTS,NLINES      TEST IF SCREEN IS FILLED                     
         BNL   *+14                YES                                          
         MVC   ELEMENT+2(L'FSTEL),FSTEL                                         
         B     TIPFKX              NO MORE TO COME                              
         MVC   ELEMENT+2(L'LASTEL),LASTEL                                       
         LA    RF,TSARKLEN+1       SET NEW KEY FOR READ HIGH TSAR               
         LA    RF,ELEMENT(RF)                                                   
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)                                                         
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH       ACTION = READ HIGH                           
         LA    R6,ELEMENT                                                       
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         READ TSAR RECORD INTO ELEMENT                
         SR    R1,R1                                                            
         IC    R1,NSAVSCRS         GET NUMBER OF SAVED SCREENS                  
         MH    R1,=Y(L'LASTEL)                                                  
         LA    RF,SAVELS                                                        
         AR    RF,R1               RF=A(END OF SAVED ELEMENT LIST)              
         MVC   0(L'FSTEL,RF),FSTEL SAVE FIRST ELEMENT ON SCREEN                 
         SR    R1,R1                                                            
         IC    R1,NSAVSCRS                                                      
         LA    R1,1(R1)            INCREASE NUMBER OF SAVED SCREENS             
         STC   R1,NSAVSCRS                                                      
         B     TIPFKX                                                           
*                                                                               
TIPFK10  BAS   RE,ACCNAME          GET CLIENT/PRODUCT/JOB NAMES                 
         MVC   REMUSER,TWAALIAS    SUPPLY REQUESTOR ID                          
         GOTO1 OPENPQ                                                           
         LA    R1,HOOK             PRINT THE TSAR RECORDS                       
         ST    R1,HEADHOOK                                                      
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
*                                                                               
         USING PRID,R4                                                          
         LA    R4,P                                                             
         USING EPTELD,R6                                                        
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING TSARD,R1                                                         
TIPFK10A LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSARDH       ACTION = READ HIGH                           
         ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD         READ TSAR RECORD INTO ELEMENT                
         TM    TSERRS,TSEEOF       TEST END OF DATASET                          
         BO    TIPFK10X            YES - ALL DONE                               
         CP    EPTHOURS,=P'0'      DO WE HAVE HOURS?                            
         BE    TIPFK10C            NO - SO DON'T PRINT IT                       
         MVC   PRICODE,EPTACT      TAKE PERSON ACCOUNT CODE                     
         CLI   EPTLN,EPTLN1Q       IS PERSON NAME OVERWRITEN?                   
         BH    *+18                YES                                          
         BAS   RE,GETACCNM         GET PERSON ACCOUNT NAME                      
         MVC   PRIDESC,PACCNAME                                                 
         B     TIPFK10B                                                         
         SR    RE,RE                                                            
         IC    RE,EPTLN                                                         
         SH    RE,=Y(EPTLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   PRIDESC(0),EPTDESC  EXTRACT OVERWRITEN PERSON ACC NAME           
TIPFK10B GOTO1 DATCON,DMCB,(1,EPTRDATE),(10,PRIDATE)                            
         CURED (P4,EPTHOURS),(L'PRIHOUR,PRIHOUR),2,ZERO=BLANK                   
         CURED (P4,EPTRATE),(L'PRIRATE,PRIRATE),2,ZERO=BLANK                    
         ZAP   DUB,EPTRATE                                                      
         MP    DUB,EPTHOURS                                                     
         SRP   DUB,64-2,5                                                       
         CURED (P8,DUB),(L'PRIAMT,PRIAMT),2,ZERO=BLANK                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
TIPFK10C LA    RF,TSARKLEN         SET NEW KEY FOR READ HIGH TSAR               
         LA    RF,ELEMENT+1(RF)                                                 
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)                                                         
         B     TIPFK10A                                                         
*                                                                               
TIPFK10X MVC   PRIDESC,AC@WCTOT    PRINT WORK CODE TOTALS                       
         CURED (P8,WCTOTAL),(L'PRIAMT,PRIAMT),2,ZERO=BLANK                      
         CURED (P6,TOTHOURS),(L'PRIHOUR,PRIHOUR),2,ZERO=BLANK                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SR    RF,RF                                                            
         ICM   RF,4,GETMSYS                                                     
         LA    R0,AP$SPOOL                                                      
         MVC   TEMPFLDH,CONHEADH                                                
         MVI   TEMPFLDH,L'TEMPFLDH+L'TEMPFLD                                    
         GOTO1 GETTXT,DMCB,(R0),(0,TEMPFLDH),(C'I',0),0,0,(RF)                  
         MVI   ERROR,0             CLEAR ERROR FOR NORMAL EXIT                  
         LA    R4,CONHEAD          R4=OUTPUT POINTER                            
         MVC   0(3,R4),SPOOLID                                                  
         MVI   3(R4),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,4(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         LA    R4,5(R4)                                                         
         MVC   0(L'TEMPFLD,R4),TEMPFLD                                          
         MVI   NSAVSCRS,0          START DISPLAY FROM BEGINNING                 
         B     TIPFKX                                                           
*                                                                               
TIPFKX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO GET PERSON LEDGER                                   *          
**********************************************************************          
         USING ACTRECD,R4                                                       
GETLDG   ST    RE,SAVERE                                                        
         LA    R4,KEY                                                           
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),CTRPLDG                             
         GOTO1 READ                READ PERSON LEDGER                           
         CLC   KEY(L'LDGKEY),KEYSAVE                                            
         BE    *+12                                                             
         MVI   ERROR,SECLKOUT                                                   
         B     ERREND              NO PERSON LEDGER FOUND                       
         MVI   ELCODE,ACLELQ       GET LEVEL LENGTHS                            
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACLELD,R6                                                        
         MVC   LDGLV1,ACLVALS                                                   
         MVC   LDGLV2,ACLVALS+(L'ACLVALS*1)                                     
         MVC   LDGLV3,ACLVALS+(L'ACLVALS*2)                                     
         MVC   LDGLV4,ACLVALS+(L'ACLVALS*3)                                     
*                                                                               
GETLDGX  L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO LOOK FOR A TIME SHEED LIST RECORD                   *          
**********************************************************************          
         USING TSLRECD,R4                                                       
GETTSR   ST    RE,SAVERE                                                        
         NI    SVFLAG,255-SVFLTSRE                                              
         MVC   AIO,AIO2                                                         
         LA    R4,KEY                                                           
         XC    TSLKEY,TSLKEY                                                    
         MVI   TSLKTYP,TSLKTYPQ    TIME SHEET LIST RECORD TYPE                  
         MVI   TSLKSUB,TSLKSUBQ    TIME SHEET LIST SUB RECORD TYPE              
         MVC   TSLKCPY(L'CUL),CUL                                               
         MVC   TSLKACT,SPACES                                                   
         MVC   TSLKACT(L'SAVECLI),SAVECLI FILL IN CLIENT CODE                   
         SR    RE,RE                                                            
         IC    RE,SVLCLI           GET CLIENT LENGTH                            
         LA    RE,TSLKACT(RE)                                                   
         MVC   0(L'SAVEPRO,RE),SAVEPRO FILL IN PRODUCT CODE                     
         SR    RE,RE                                                            
         IC    RE,SVLCLPRO         GET CLIENT+PRODUCT LENGTH                    
         LA    RE,TSLKACT(RE)                                                   
         MVC   0(L'SAVEJOB,RE),SAVEJOB AND JOB CODE                             
         GOTO1 HIGH                READ TIME SHEET LIST RECORD                  
         CLC   TSLKEY,KEYSAVE      RECORD FOUND?                                
         BE    GETTSR06                                                         
         MVC   KEY,KEYSAVE                                                      
         SR    RE,RE                                                            
         IC    RE,SVLCLPRO         GET CLIENT+PRODUCT LENGTH                    
         LA    RE,TSLKACT(RE)                                                   
         MVC   0(L'SAVEJOB,RE),SPACES LOOK FOR PRODUCT LEVEL RECORD             
         GOTO1 HIGH                READ TIME SHEET LIST RECORD                  
         CLC   TSLKEY,KEYSAVE      RECORD FOUND?                                
         BE    GETTSR06                                                         
         MVC   KEY,KEYSAVE                                                      
         SR    RE,RE                                                            
         IC    RE,SVLCLI           GET CLIENT LENGTH                            
         LA    RE,TSLKACT(RE)                                                   
         MVC   0(L'SAVEPRO,RE),SPACES LOOK FOR CLIENT LEVEL RECORD              
         GOTO1 HIGH                READ TIME SHEET LIST RECORD                  
         CLC   TSLKEY,KEYSAVE      RECORD FOUND?                                
         BNE   GETTSRX                                                          
*                                                                               
GETTSR06 OI    SVFLAG,SVFLTSRE     MARK THAT WE HAVE A TIME SHEET REC           
         MVC   TSLKSAVE,TSLKEY     SAVE RECORD KEY                              
*                                                                               
GETTSRX  MVC   AIO,AIO1            RESET IO AREA                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO VALIDATE START KEY FIELDS ON JOB STAFF SCREEN       *          
**********************************************************************          
         USING ACTRECD,R4                                                       
VHEAD    LA    R2,PRSLEV1H                                                      
         CLC   PRSLEV1H+5(1),LDGLV1                                             
         BH    VHEADHI             INPUT TO LONG                                
         LA    R4,KEY              BUILD START ACCOUNT KEY IN 'KEY'             
         MVC   KEY,SPACES                                                       
         SR    RF,RF                                                            
         ICM   RF,1,PRSLEV1H+5                                                  
         BZ    VHEAD2                                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ACTKACT(0),PRSLEV1  INSERT LEVEL 1                               
*                                                                               
VHEAD2   SR    RF,RF                                                            
         ICM   RF,1,LDGLV2                                                      
         BZ    VHEADX              ONLY 1 LEVEL - ALL DONE                      
         CLI   PRSLEV2H+5,0                                                     
         BE    VHEAD3                                                           
         CLI   PRSLEV1H+5,0                                                     
         BE    VHEADMIS                                                         
         SR    R1,R1                                                            
         IC    R1,LDGLV1                                                        
         SR    RF,R1                                                            
         CLM   RF,1,PRSLEV2H+5                                                  
         BNL   *+12                                                             
         LA    R2,PRSLEV2H                                                      
         B     VHEADHI             INPUT TO LONG                                
         LA    R1,ACTKACT(R1)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),PRSLEV2     INSERT LEVEL 2                               
*                                                                               
VHEAD3   SR    RF,RF                                                            
         ICM   RF,1,LDGLV3                                                      
         BZ    VHEADX              ONLY 2 LEVEL - ALL DONE                      
         CLI   PRSLEV3H+5,0                                                     
         BE    VHEAD4                                                           
         CLI   PRSLEV1H+5,0                                                     
         BE    VHEADMIS                                                         
         CLI   PRSLEV2H+5,0                                                     
         BNE   *+12                                                             
         LA    R2,PRSLEV2H                                                      
         B     VHEADMIS                                                         
         SR    R1,R1                                                            
         IC    R1,LDGLV2                                                        
         SR    RF,R1                                                            
         CLM   RF,1,PRSLEV3H+5                                                  
         BNL   *+12                                                             
         LA    R2,PRSLEV3H                                                      
         B     VHEADHI             INPUT TO LONG                                
         LA    R1,ACTKACT(R1)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),PRSLEV3     INSERT LEVEL 3                               
*                                                                               
VHEAD4   SR    RF,RF                                                            
         ICM   RF,1,LDGLV4                                                      
         BZ    VHEADX              ONLY 3 LEVEL - ALL DONE                      
         CLI   PRSLEV4H+5,0                                                     
         BE    VHEADX                                                           
         CLI   PRSLEV1H+5,0                                                     
         BE    VHEADMIS                                                         
         CLI   PRSLEV2H+5,0                                                     
         BNE   *+12                                                             
         LA    R2,PRSLEV2H                                                      
         B     VHEADMIS                                                         
         CLI   PRSLEV3H+5,0                                                     
         BNE   *+12                                                             
         LA    R2,PRSLEV3H                                                      
         B     VHEADMIS                                                         
         SR    R1,R1                                                            
         IC    R1,LDGLV3                                                        
         SR    RF,R1                                                            
         CLM   RF,1,PRSLEV4H+5                                                  
         BNL   *+12                                                             
         LA    R2,PRSLEV4H                                                      
         B     VHEADHI             INPUT TO LONG                                
         LA    R1,ACTKACT(R1)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),PRSLEV4     INSERT LEVEL 4                               
         B     VHEADX                                                           
*                                                                               
VHEADHI  MVI   ERROR,TOOLNG                                                     
         B     ERREND                                                           
*                                                                               
VHEADMIS MVI   ERROR,MISSING                                                    
         B     ERREND                                                           
*                                                                               
VHEADX   CLC   ACTKACT,SPACES      DO WE HAVE AN INPUT?                         
         BE    *+8                 NO                                           
         NI    SVFLAG,255-SVFLTSRE YES - SWITCH OFF TIME SHEET REC FLAG         
         OI    PRSLEV1H+4,X'20'    SET PREV VALID BITS                          
         OI    PRSLEV2H+4,X'20'                                                 
         OI    PRSLEV3H+4,X'20'                                                 
         OI    PRSLEV4H+4,X'20'                                                 
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO GET WORK CODE NAME                                  *          
**********************************************************************          
         USING WCORECD,R4                                                       
GETWC    NTR1  ,                                                                
         LA    R4,KEY                                                           
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY(L'CUL),CUL  COMPANY+UNIT+LEDGER                          
         MVC   WCOKWRK,SAVEWC      WORK CODE CODE                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                READ WORK CODE                               
         CLC   KEY(WCOKEND),KEYSAVE                                             
         BE    *+12                                                             
         MVI   ERROR,BADWORK                                                    
         B     ERREND                                                           
         SR    R0,R0                                                            
         L     R6,AIO                                                           
         LA    R6,ACCORFST(R6)                                                  
GETWC10  CLI   0(R6),0             TEST END OF RECORD                           
         BE    GETWCX                                                           
         CLI   0(R6),WCOELQ        WORK CODE ELEMENT?                           
         BE    GETWC14                                                          
         CLI   0(R6),NAMELQ        NAME ELEMENT?                                
         BE    GETWC16                                                          
         CLI   0(R6),FFTELQ        SUB WORK CODE?                               
         BE    GETWC18                                                          
*                                                                               
GETWC12  IC    R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         B     GETWC10                                                          
*                                                                               
         USING WCOELD,R6                                                        
GETWC14  MVC   PRTWC+5(L'WCODESC),WCODESC                                       
         B     GETWC12             LOOK FOR LONG NAME                           
*                                                                               
         USING NAMELD,R6                                                        
GETWC16  MVC   PRTWC+5(L'PRTWC-5),SPACES                                        
         SR    RF,RF                                                            
         IC    RF,NAMLN            GET LONG NAME LENGTH                         
         SH    RF,=Y(NAMLN1Q)                                                   
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PRTWC+5(0),NAMEREC                                               
         CLI   SAVEWC+2,0          ARE WE DEALING WITH A SUB WORK CODE          
         BE    GETWCX              NO - SO ALL DONE                             
         B     GETWC12             LOOK FOR SUB WORK CODE ELEMENT               
*                                                                               
         USING FFTELD,R6                                                        
GETWC18  CLI   FFTTYPE,FFTTFREE                                                 
         BNE   GETWC12                                                          
         CLC   FFTSEQ,SAVEWC+2     TEST MATCH ON SUB WORK CODE                  
         BNE   GETWC12                                                          
         MVC   PRTWC+5(L'PRTWC-5),SPACES                                        
         SR    RF,RF                                                            
         ICM   RF,1,FFTDLEN                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PRTWC+5(0),FFTDATA                                               
         B     GETWCX                                                           
*                                                                               
GETWCX   OI    PRTWCH+6,X'80'                                                   
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO READ THE PERSON CHARGE RATE                         *          
**********************************************************************          
         USING PCRRECD,R4                                                       
GETRATE  NTR1  ,                                                                
         B     GETRX                                                            
*&&DO                                                                           
         LA    R4,RATEKEY          BUILD KEY                                    
         XC    RATEKEY,RATEKEY                                                  
         MVC   PCRKPER,RATEACC     PERSON ACCOUNT CODE                          
         OC    PCRKPER,SPACES                                                   
         MVC   PCRKOFF,SVCLIOFF    CLIENT (OR PRODUCT) OFFICE CODE              
         MVC   PCRKJOB(L'SAVECLI),SAVECLI                                       
         SR    RE,RE                                                            
         IC    RE,SVLCLI           GET CLIENT LENGTH                            
         LA    RE,PCRKJOB(RE)                                                   
         MVC   0(L'SAVEPRO,RE),SAVEPRO                                          
         OC    PCRKJOB,SPACES                                                   
         MVC   PCRKTSK,SAVEWC      WORK CODE                                    
         DROP  R4                                                               
*                                                                               
         XC    INDEX,INDEX                                                      
         MVI   INDEX+8,X'FF'                                                    
         MVI   INDEX+0,2           FIXED WORK-CODE LENGTH                       
         LA    RE,L'PCRKJOB        LENGTH OF PRODUCTION KEYPART                 
         SR    RF,RF                                                            
         IC    RF,SVLCLI           LENGTH OF CLIENT                             
         STC   RF,INDEX+2                                                       
         SR    RE,RF                                                            
         STC   RE,INDEX+1          PRODUCT LENGTH (EXCL)                        
         MVI   INDEX+3,1           FIXED OFFICE CODE LENGTH                     
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         ICM   RF,1,LDGLV4                                                      
         BZ    *+14                NO LEVEL 4                                   
         IC    RE,LDGLV3                                                        
         SR    RF,RE                                                            
         STC   RF,INDEX+4          LENGTH OF LEVEL 4                            
         ICM   RE,1,LDGLV3                                                      
         BZ    *+14                NO LEVEL 3                                   
         IC    RF,LDGLV2                                                        
         SR    RE,RF                                                            
         STC   RE,INDEX+5          LENGTH OF LEVEL 3                            
         ICM   RF,1,LDGLV2                                                      
         BZ    *+14                NO LEVEL 2                                   
         IC    RE,LDGLV1                                                        
         SR    RF,RE                                                            
         STC   RF,INDEX+6          LENGTH OF LEVEL 2                            
         MVC   INDEX+7(1),LDGLV1   LENGTH OF LEVEL 1                            
*                                                                               
         LA    R6,SRCHTBL                                                       
         B     *+8                                                              
GETR10   LA    R6,1(R6)                                                         
         CLI   0(R6),0             TEST END OF TABLE                            
         BE    GETRX               NO RATE EXISTS                               
         XC    KEY,KEY             SET KEY                                      
         MVC   KEY(32),ALLF        SET KEY TO X'FF'                             
         LA    RE,KEY                                                           
         USING PCRRECD,RE                                                       
         MVI   PCRKTYP,PCRKTYPQ    PERSON CHARGE RATE RECORD TYPE               
         MVC   PCRKCPY,CUL                                                      
         MVC   PCRKPER,SPACES      CLEAR PERSON ACCOUNT                         
         DROP  RE                                                               
         LA    R4,KEY+PCRKTSK-PCRKEY R4=A(WORK CODE IN KEY)                     
         LA    R3,RATEKEY+PCRKTSK-PCRKEY                                        
         LA    R2,INDEX                                                         
         SR    R1,R1                                                            
         ICM   R1,8,0(R6)          GET SEARCH MASK                              
*                                                                               
GETR14   LTR   R1,R1               IS THIS BIT ON ?                             
         BNM   GETR18              NO, DON'T MOVE INPUT TO KEY                  
         SR    RF,RF                                                            
         ICM   RF,1,0(R2)          LENGTH OF KEY-COMPONENT                      
         BZ    GETR18                                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R4),0(R3)       MOVE COMPONENT TO KEY FROM RATEKEY           
*                                                                               
GETR18   LA    R2,1(R2)            GET NEXT INDEX LENGTH                        
         CLI   0(R2),X'FF'         WHEN ALL MOVES DONE                          
         BE    GETR20              TRY AND READ THE RECORD                      
         SLL   R1,1                GET NEXT BIT TO COMPARE TO                   
         SR    RF,RF                                                            
         ICM   RF,1,0(R2)                                                       
         BZ    GETR14                                                           
         SR    R4,RF               BUMP TO PREVIOUS FIELD IN KEY                
         SR    R3,RF                                                            
         B     GETR14              GET NEXT PART OF KEY                         
*                                                                               
GETR20   XC    MYDATE,MYDATE                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(PCRKTSK-PCRKEY),KEYSAVE                                      
         BNE   GETR10                                                           
         L     R4,AIO                                                           
         LA    R4,ACCORFST(R4)                                                  
GETR24   CLI   0(R4),TCIELQ                                                     
         BE    GETR28              FOUND A RATE ELEMENT                         
         CLI   0(R4),0                                                          
         BNE   GETR26              END OF RECORD                                
         OC    MYDATE,MYDATE                                                    
         BNZ   GETRX               GOOD RATE FOUND                              
         B     GETR10              GET NEXT RECORD                              
GETR26   SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETR24                                                           
*                                                                               
         USING TCIELD,R4                                                        
GETR28   CLC   TCIDTE,NDATE                                                     
         BH    GETR26              EFFECTIVE AFTER TRANSACTION DATE             
         CLC   TCIDTE,MYDATE                                                    
         BL    GETR26              NOT THE HIGHEST DATE                         
         MVC   MYDATE,TCIDTE       SAVE HIGHEST DATE                            
         ZAP   NRATE,TCIAMT        SAVE THIS RATE                               
         OI    PFLAG,PFLRATWR      RATE WAS READ                                
         B     GETR26              AND LOOK FOR ANOTHER                         
         DROP  R4                                                               
*&&                                                                             
GETRX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO UPDATE THE TIME ESTIMATE RECORDS                    *          
**********************************************************************          
         USING EVERECD,R4                                                       
         USING EPTELD,R6                                                        
UPDATE   NTR1  ,                                                                
         LA    R4,KEY              LOOK FOR EXISTING TIME EST RECORDS           
         XC    KEY,KEY                                                          
         MVI   EVEKTYP,EVEKTYPQ                                                 
         MVI   EVEKSUB,EVEKSUBQ                                                 
         MVC   EVEKCPY(L'CUL),CUL                                               
         MVC   EVEKCLI,SAVECLI                                                  
         MVC   EVEKPRO,SAVEPRO                                                  
         MVC   EVEKJOB,SAVEJOB                                                  
         MVC   EVEKTYPE(L'SAVEST),SAVEST                                        
         MVC   EVEKWC(L'SAVEWC),SAVEWC  WORK CODE + SUB WORK CODE               
         MVI   EVEKSEQ,0           SEQUENCE NUMBER                              
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                READ TIME ESTIMATING RECORD                  
         L     R4,AIO                                                           
         CLC   KEY(L'EVEKEY),KEYSAVE  DID WE FOUND ONE?                         
         BNE   UPD10               NO - ADD NEW ONES                            
         CLI   NSAVELS+1,0         DO WE HAVE SOMETHING TO STORE?               
         BE    UPD30               NO - DELETE ALL EXISTING T EST RECS          
UPD06    NI    ACCOSTAT(R4),X'FF'-X'80'                                         
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('EPTELQ',(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
         MVC   ADDORWRT,=C'DMWRT'                                               
         B     UPD14                                                            
*                                                                               
UPD10    CLI   NSAVELS+1,0         DO WE HAVE SOMETHING TO STORE?               
         BE    UPD40               NO - GO AND UPDATE ESTIMATE REC              
UPD12    MVC   0(L'KEYSAVE,R4),KEYSAVE  BUILD NEW EMPTY RECORD                  
         LA    RF,ACCORFST(R4)     RF=A(FIRST ELEMENT)                          
         MVI   0(RF),0             MARK END OF RECORD                           
         LA    RF,ACCORFST+1                                                    
         STCM  RF,3,ACCORLEN(R4)   SET RECORD LENGTH                            
         MVC   ADDORWRT,=C'DMADD'                                               
         B     UPD14                                                            
*                                                                               
UPD14    OC    ELEMENT,ELEMENT     TEST FIRST TIME THROUGH                      
         BNZ   UPD18               NO                                           
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSARDH       ACTION=READ HIGH FOR THE FIRST ONE           
UPD16    ST    R6,TSAREC                                                        
         GOTO1 VTSAR,TSARD                                                      
         TM    TSERRS,TSEEOF       TEST END OF FILE                             
         BO    UPD20               YES - WRITE/ADD THE RECORD                   
         SR    RE,RE                                                            
         ICM   RE,3,ACCORLEN(R4)   GET ACTUAL RECORD LENGTH                     
         SR    RF,RF                                                            
         IC    RF,EPTLN            GET ELEMENT LENGTH                           
         AR    RE,RF                                                            
         LA    RF,MAXRECLE                                                      
         CR    RE,RF               TEST IF ELEMENT FITS IN RECORD               
         BH    UPD20               WRITE/ADD THE RECORD                         
UPD18    MVI   ELEMENT,EPTELQ                                                   
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R4),(R6)                               
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSANXT       ACTION = READ NEXT                           
         B     UPD16                                                            
*                                                                               
UPD20    GOTO1 DATAMGR,DMCB,ADDORWRT,SYSFIL,(R4),(R4),0                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,0(R4)                                                        
         LA    R1,TSARBLK                                                       
         TM    TSERRS,TSEEOF       TEST END OF TSAR DATASET                     
         BO    UPD32               YES - DELETE EXISTING T EST RECS             
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         MVI   KEY+EVEKSEQ-EVEKEY+L'EVEKSEQ,X'FF'                               
         GOTO1 HIGH                                                             
         CLC   KEY(EVEKSEQ-EVEKEY),KEYSAVE  DID WE FOUND ONE?                   
         BE    UPD06               YES                                          
         SR    RF,RF                                                            
         IC    RF,KEYSAVE+EVEKSEQ-EVERECD                                       
         LA    RF,1(RF)            INCREASE SEQUENCE NUMBER                     
         STC   RF,KEYSAVE+EVEKSEQ-EVERECD                                       
         MVI   KEYSAVE+EVEKSEQ-EVEKEY+L'EVEKSEQ,0                               
         B     UPD12               GO AND ADD A NEW RECORD                      
*                                                                               
UPD30    GOTO1 HELLO,DMCB,(C'D',SYSFIL),('EPTELQ',(R4)),0                       
         OI    ACCOSTAT(R4),X'80'  DELETE THE RECORD                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',SYSFIL,(R4),(R4),0                        
UPD32    MVI   KEY+EVEKSEQ-EVEKEY+L'EVEKSEQ,X'FF'                               
         GOTO1 HIGH                                                             
         CLC   KEY(EVEKSEQ-EVEKEY),KEYSAVE  DID WE FOUND ONE?                   
         BE    UPD30               YES - DELETE IT                              
         B     UPD40                                                            
*                                                                               
UPD40    XC    KEY,KEY                                                          
         MVC   KEY(EVEKWC-EVEKEY),KEYSAVE                                       
         GOTO1 HIGH                READ ESTIMATE VERSION RECORD                 
         CLC   KEY(L'EVEKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                ESTIMATE RECORD NOT FOUND                    
         SR    R0,R0                                                            
         USING EDAELD,R6                                                        
         LA    R6,ACCORFST(R4)                                                  
UPD42    CLI   EDAEL,0             TEST FOR END OF RECORD                       
         BE    UPD46               WORK CODE ELEMENT NOT FOUND                  
         CLI   EDAEL,EDAELQ        LOOK FOR ESTIMATE DATA ELEMENT               
         BNE   UPD44                                                            
         CLC   EDAWORK,SAVEWC      MATCH ON WORK CODE                           
         BNE   UPD44                                                            
         CLC   EDASUBC,SAVEWC+L'EDAWORK  AND SUB WORK CODE                      
         BNE   UPD44                                                            
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         SR    RF,RF               DELETE ESTIMATE DATA ELEMENT                 
         IC    RF,EDALN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ELEMENT(0),0(R6)    SAVE ELEMENT                                 
         BCTR  RF,0                GET LENGTH FOR SEARCH ARGUMENT               
         LA    R6,ELEMENT                                                       
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('EDAELQ',(R4)),((RF),EDATYPE)          
         CP    WCTOTAL,=P'0'       DO WE HAVE TO STORE SOMETHING?               
         BE    UPD50               NO - GO AND UPDATE RECORD                    
         NI    EDATYPE,255-EDATPLIS SWITCH OFF PRICE LIST MARKER                
         XC    EDAPLIST,EDAPLIST                                                
         ZAP   EDACOMM,WCTOTAL     STORE WC TOTAL AMOUNT                        
         TM    EDATYPE,EDATCOHE    TEST COMMISSION HELD ON ELEMENT              
         BO    *+10                YES - SO VALUE IS ALREADY THERE              
         ZAP   EDANCOM,=P'0'                                                    
         ZAP   EDAHOURS,TOTHOURS   STORE TOTAL HOURS                            
         OI    EDATYPE,EDATTEST    MARK AS TIME ESTIMATE ELEMENT                
         MVI   EDALN,EDALN4Q                                                    
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R4),(R6)                               
         B     UPD50               WRITE BACK EST VERSION RECORD                
*                                                                               
UPD44    IC    R0,EDALN            BUMP TO NEXT ESTIMATE DATA ELEMENT           
         AR    R6,R0                                                            
         B     UPD42                                                            
*                                                                               
UPD46    CP    WCTOTAL,=P'0'       DO WE NEED TO STORE SOMETHING?               
         BE    UPDATEX             NO - ALL DONE                                
         LA    R6,ELEMENT          BUILD A NEW EST DATA ELEMENT                 
         XC    ELEMENT,ELEMENT                                                  
         MVI   EDAEL,EDAELQ                                                     
         MVI   EDALN,EDALN4Q                                                    
         CLI   SAVEWC+L'EDAWORK,0  TEST FOR SUB WORK CODE                       
         BE    UPD48               NO                                           
         MVC   EDASUBC,SAVEWC+L'EDAWORK                                         
         OI    EDATYPE,EDATSUB     MARK AS SUB WORK CODE ELEMENT                
UPD48    OI    EDATYPE,EDATWORK+EDATTEST  MARK AS TIME EST WORK CODE EL         
         MVC   EDAWORK,SAVEWC                                                   
         ZAP   EDACOMM,WCTOTAL     STORE WORK CODE TOTAL                        
         ZAP   EDANCOM,=P'0'                                                    
         ZAP   EDAHOURS,TOTHOURS   AND TOTAL HOURS                              
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R4),(R6)                               
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
         B     UPD50                                                            
*                                                                               
UPD50    SR    R0,R0                                                            
         USING EUPELD,R6                                                        
         LA    R6,ACCORFST(R4)                                                  
UPD52    CLI   EUPEL,0             TEST FOR END OF RECORD                       
         BE    UPD56                                                            
         CLI   EUPEL,EUPELQ        LOOK FOR ESTIMATE UPDATE ELEMENT             
         BE    UPD54                                                            
*                                                                               
         IC    R0,EUPLN            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         B     UPD52                                                            
*                                                                               
UPD54    MVC   EUPLAST,TODAYP      UPDATE THE ESTIMATE UPDATE ELEMENT           
         MVC   EUPERS,TWAALIAS                                                  
         B     UPD56                                                            
*                                                                               
UPD56    GOTO1 DATAMGR,DMCB,=C'DMWRT',SYSFIL,(R4),(R4),0                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDATEX  B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO GET THE ACCOUNT NAMES-CLIENT, PRODUCT, AND JOB      *          
**********************************************************************          
ACCNAME  NTR1  ,                                                                
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(L'SAVECLI),SAVECLI                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 SETNAME,DMCB,AIO,BLOCK                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SVLCLI                                                        
         LA    R1,KEY+3(R1)        POINT AT PRODUCT POSITION                    
         MVC   0(L'SAVEPRO,R1),SAVEPRO                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 SETNAME,DMCB,AIO,BLOCK+36                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SVLCLPRO                                                      
         LA    R1,KEY+3(R1)        POINT AT JOB                                 
         MVC   0(L'SAVEJOB,R1),SAVEJOB                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 SETNAME,DMCB,AIO,BLOCK+72                                        
*                                                                               
ACCNAMEX B     XIT                                                              
         SPACE 2                                                                
**********************************************************************          
* HOOK ROUTINE FOR REPORT PRINTING                                   *          
**********************************************************************          
HOOK     NTR1  ,                                                                
         MVC   H4+12(L'SAVECLI),SAVECLI                                         
         MVC   H4+20(36),BLOCK     CLIENT NAME                                  
         MVC   H5+12(L'SAVEPRO),SAVEPRO                                         
         MVC   H5+20(36),BLOCK+36  PRODUCT NAME                                 
         MVC   H6+12(L'SAVEJOB),SAVEJOB                                         
         MVC   H6+20(36),BLOCK+72  JOB NAME                                     
         MVC   H7+12(4),PRTWC      WORK CODE                                    
         MVC   H7+20(L'WCODESC),PRTWC+5  WORK CODE NAME                         
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO INITIALIZE THE TSAR DATASET                         *          
**********************************************************************          
INIT     ST    RE,SAVERE                                                        
         LA    RE,TSARBLK                                                       
         LA    RF,L'TSARBLK                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSNBUF,1            ONE CORE BUFFER                              
         LA    RE,BUFF             BUFF=A(CORE BUFFER)                          
         ST    RE,TSABUF                                                        
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSPAGL,2            USE TEMPSTR PAGE 2                           
         MVI   TSPAGN,1            ONLY 1 SAVE PAGE                             
         OI    TSRECI,TSRVAR       VARIABLE RECORD LENGTH                       
         MVI   TSKEYL,TSARKLEN     KEY LENGTH                                   
         MVC   TSRECL,=AL2(L'ELEMENT)  MAX RECORD LENGTH                        
         MVI   TSACTN,TSAINI       ACTION=INIT                                  
         GOTO1 VTSAR,TSARD         INITIALIZE TSAR DATASET                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INITX    L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
**********************************************************************          
* SUB ROUTINE TO SAVE THE TSAR DATASET                               *          
**********************************************************************          
SAVETSAR ST    RE,SAVERE                                                        
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSASAV       ACTION SAVE                                  
         GOTO1 VTSAR,TSARD         SAVE TSAR DATASET                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
**********************************************************************          
* SUB ROUTINE TO RESTORE THE TSAR DATASET                            *          
**********************************************************************          
REST     ST    RE,SAVERE                                                        
         LA    RE,TSARBLK                                                       
         LA    RF,L'TSARBLK                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSNBUF,1            NUMBER OF CORE BUFFER                        
         LA    RE,BUFF                                                          
         ST    RE,TSABUF           ADDRESSE OF BUFFER                           
         LA    RE,ELEMENT                                                       
         ST    RE,TSAREC           ADDRESSE OF RECORD                           
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSPAGL,2            USE TEMPSTR PAGE 2                           
         MVI   TSPAGN,1            N'TWA SAVE PAGES                             
         MVI   TSKEYL,TSARKLEN     PASS KEY LENGTH                              
         MVI   TSACTN,TSARES       ACTION=RESTORE                               
         GOTO1 VTSAR,TSARD         RESTORE THE TSAR DATASET                     
         BE    *+6                                                              
         DC    H'0'                BLOW UP ON ANY ERROR RESTORING               
*                                                                               
RESTX    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO LOOK UP CLIENT AND PRODUCT FOR OFFICE CODE          *          
**********************************************************************          
GETCLI   ST    RE,SAVERE           LOOK FOR CLIENT RECORD                       
         MVC   SVCLIOFF,SPACES                                                  
         GOTO1 SETHEIR                                                          
         MVC   SVLCLI,LCLI         SAVE LENGTH OF CLIENT CODE                   
         MVC   SVLCLPRO,LCLIPRO    SAVE LENGTH OF CLIENT+PRODUCT                
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY(L'CUL),CUL                                               
         MVC   ACTKACT(L'SAVECLI),SAVECLI                                       
         OC    ACTKACT,SPACES                                                   
         GOTO1 READ                READ CLIENT RECORD                           
         SR    R0,R0                                                            
         L     R6,AIO                                                           
         LA    R6,ACCORFST(R6)                                                  
GETCL10  CLI   0(R6),0             TEST END OF RECORD                           
         BE    GETCL20                                                          
         CLI   0(R6),PPRELQ        PRODUCTION PROFILE ELEMENT                   
         BE    GETCL14                                                          
         IC    R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         B     GETCL10                                                          
*                                                                               
         USING PPRELD,R6                                                        
GETCL14  MVC   SVCLIOFF,PPRGAOFF   SAVE CLIENT OFFICE CODE                      
*                                                                               
GETCL20  SR    RF,RF                                                            
         IC    RF,SVLCLI                                                        
         LA    RF,ACTKACT(RF)                                                   
         MVC   0(L'SAVEPRO,RF),SAVEPRO                                          
         OC    ACTKACT,SPACES                                                   
         GOTO1 READ                READ PRODUCT RECORD                          
         SR    R0,R0                                                            
         L     R6,AIO                                                           
         LA    R6,ACCORFST(R6)                                                  
GETCL22  CLI   0(R6),0             TEST END OF RECORD                           
         BE    GETCLX                                                           
         CLI   0(R6),PPRELQ        PRODUCTION PROFILE ELEMENT                   
         BE    GETCL24                                                          
         IC    R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         B     GETCL22                                                          
*                                                                               
GETCL24  CLC   PPRGAOFF,SPACES     DO WE HAVE AN OFFICE ON PRO LEVEL?           
         BNH   GETCLX              NO                                           
         MVC   SVCLIOFF,PPRGAOFF   YES - SO SAVE PRODUCT OFFICE CODE            
*                                                                               
GETCLX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* SUB ROUTINE TO LOOK UP PERSON ACCOUNT                              *          
**********************************************************************          
CHEPACC  ST    RE,SAVERE           LOOK FOR PERSON ACCOUNT                      
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),CTRPLDG                             
         MVC   ACTKACT,LTICODE                                                  
         OC    ACTKACT,SPACES                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACTKEY),KEYSAVE                                            
         BE    CHEPACCX                                                         
         LA    R2,LTICODEH                                                      
         MVI   ERROR,NOTFOUND                                                   
         B     ERRENDTI                                                         
*                                                                               
CHEPACCX L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
* SUPPORTING SUB ROUTINES                                            *          
**********************************************************************          
ERRNORAT LA    R2,LTIRATEH                                                      
         MVI   ERROR,NORATENT      NO VALID RATE FOUND - ENTER A RATE           
         B     ERRENDTI                                                         
ERRNOACC LA    R2,LTICODEH                                                      
         B     *+8                                                              
ERRNOHRS LA    R2,LTIHOURH                                                      
         MVI   ERROR,MISSING       MISSING INPUT FIELD                          
ERRENDTI BAS   RE,SAVETSAR                                                      
         LA    RF,CONHEADH         RF=A(FIELD HEADER)                           
         LA    RE,PRTPFAH-1        RE=BXLE LIMIT                                
         SR    R0,R0               R0=INCREMENT=FIELD LENGTH                    
         OI    6(RF),X'80'         TRANSMIT THE ENTIRE SCREEN                   
         IC    R0,0(RF)                                                         
         BXLE  RF,R0,*-8                                                        
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
BUMPTOUN SR    RF,RF               BUMP TO NEXT UNPROTECTED                     
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'         TEST PROTECTED FIELD                         
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
SCINFMSG SR    RF,RF                                                            
         ICM   RF,4,GETMSYS                                                     
         GOTO1 GETTXT,DMCB,(R0),(0,CONHEADH),(C'I',0),0,0,(RF)                  
         ST    R2,ACURFORC                                                      
         B     XIT                                                              
         SPACE 1                                                                
YESXIT   CLI   *+1,0               SET CC=EQ AND EXIT                           
         B     XIT                                                              
         SPACE 1                                                                
NOXIT    CLI   *,0                 SET CC=NEQ AND EXIT                          
         SPACE 1                                                                
XIT      MVI   ERROR,0                                                          
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* CONSTANS                                                           *          
**********************************************************************          
RELO     DC    A(0)                                                             
MAXHOURS DC    PL8'999999'         MAXIMUM HOURS PER LINE                       
MAXRATE  DC    PL8'9999999'        MAXIMUM RATE PER LINE                        
ALLF     DC    32X'FF'                                                          
         SPACE 1                                                                
**********************************************************************          
* TABLE OF COUNTRY SPECIFIC VALUES                                   *          
**********************************************************************          
CTRTBGBR DC    CL2'1P'             PERSONAL LEDGER UK                           
         DC    C'R'                REVISION ESTIMATE UK                         
CTRTBGER DC    CL2'1R'             PERSONAL LEDGER GERMANY                      
         DC    C'R'                REVISION ESTIMATE GERMANY                    
CTRTBHOL DC    CL2'1P'             PERSONAL LEDGER HOLLAND                      
         DC    C'H'                REVISION ESTIMATE HOLLAND                    
         SPACE 1                                                                
**********************************************************************          
* SEARCH TABLE TO SEARCH FOR CHARGE RATES                            *          
**********************************************************************          
SRCHTBL  DS    0CL1                                                             
         DC    B'11111111'     W-C/PROD/CLI/OFF/D/C/B/A                         
         DC    B'01111111'         PROD/CLI/OFF/D/C/B/A                         
         DC    B'10111111'     W-C      CLI/OFF/D/C/B/A                         
         DC    B'10101111'     W-C      CLI     D/C/B/A                         
         DC    B'00111111'              CLI/OFF/D/C/B/A                         
         DC    B'10011111'     W-C          OFF/D/C/B/A                         
         DC    B'00011111'                  OFF/D/C/B/A                         
         DC    B'10001111'     W-C              D/C/B/A                         
         DC    B'00001111'                      D/C/B/A                         
         DC    B'11110111'     W-C/PROD/CLI/OFF   C/B/A                         
         DC    B'01110111'         PROD/CLI/OFF   C/B/A                         
         DC    B'10110111'     W-C      CLI/OFF   C/B/A                         
         DC    B'10100111'     W-C      CLI       C/B/A                         
         DC    B'00110111'              CLI/OFF   C/B/A                         
         DC    B'10010111'     W-C          OFF   C/B/A                         
         DC    B'00010111'                  OFF   C/B/A                         
         DC    B'10000111'     W-C                C/B/A                         
         DC    B'00000111'                        C/B/A                         
         DC    B'11110011'     W-C/PROD/CLI/OFF     B/A                         
         DC    B'01110011'         PROD/CLI/OFF     B/A                         
         DC    B'10110011'     W-C      CLI/OFF     B/A                         
         DC    B'10100011'     W-C      CLI         B/A                         
         DC    B'00110011'              CLI/OFF     B/A                         
         DC    B'10010011'     W-C          OFF     B/A                         
         DC    B'00010011'                  OFF     B/A                         
         DC    B'10000011'     W-C                  B/A                         
         DC    B'00000011'                          B/A                         
         DC    B'11110001'     W-C/PROD/CLI/OFF       A                         
         DC    B'01110001'         PROD/CLI/OFF       A                         
         DC    B'10110001'     W-C      CLI/OFF       A                         
         DC    B'10100001'     W-C      CLI           A                         
         DC    B'00110001'              CLI/OFF       A                         
         DC    B'10010001'     W-C          OFF       A                         
         DC    B'00010001'                  OFF       A                         
         DC    B'10000001'     W-C                    A                         
         DC    B'00000001'                            A                         
         DC    B'11110000'     W-C/PROD/CLI/OFF                                 
         DC    B'01110000'         PROD/CLI/OFF                                 
         DC    B'10110000'     W-C      CLI/OFF                                 
         DC    B'10100000'     W-C      CLI                                     
         DC    B'00100000'              CLI                                     
         DC    B'00110000'              CLI/OFF                                 
         DC    B'10010000'     W-C          OFF                                 
         DC    B'00010000'                  OFF                                 
         DC    B'10000000'     W-C                                              
         DC    X'00'           E-O-T POSSIBLE SEARCHES                          
         SPACE 1                                                                
**********************************************************************          
* DICTANARY LIST                                                     *          
**********************************************************************          
DDIN     DS    0C                                                               
         DCDDL AC#SEL,3            SELECT/AUSW{HLEN                             
         DCDDL AC#CLEAR,2          CLEAR/                                       
         DCDDL AC#DEL,3            DELETE/LºSCHEN                               
         DCDDL AC#INSRT,3          INSERT/EINF}GEN                              
         DCDDL AC#RPLIC,3          REPLICATE/DUBLIZIEREN                        
         DCDDL AC#WCTOT,L'PRIDESC,R WORK CODE TOTAL/LA GESAMT                   
         DCDDL AC#MSNG,L'PACCNAME  MISSING/FEHLT                                
         DC    X'00'                                                            
         SPACE 1                                                                
**********************************************************************          
* SPECS FOR HEADINGS ETC                                             *          
**********************************************************************          
HEDSPECS DS    0D                                                               
         SSPEC H1,2,CREATED                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,45,AC#JOBER,19                                                
         SSPEC H2,45,AC#JOBER,19,LU                                             
         SSPEC H1,85,AGYNAME                                                    
         SSPEC H2,85,AGYADD                                                     
         SSPEC H4,85,REPORT                                                     
         SSPEC H4,98,PAGE                                                       
*                                                                               
         SSPEC H4,2,AC#CLINT,6                                                  
         SSPEC H5,2,AC#PRO,7                                                    
         SSPEC H6,2,AC#JOB,3                                                    
         SSPEC H7,2,AC#WC,9                                                     
*                                                                               
         SSPEC H9,3,AC#CODE,12                                                  
         SSPEC H9,16,AC#DESC,36                                                 
         SSPEC H9,53,AC#DATE,8                                                  
         SSPEC H9,62,AC#HOURS,8,R                                               
         SSPEC H9,71,AC#RATE,9,R                                                
         SSPEC H9,81,AC#AMT,13,R                                                
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
**********************************************************************          
* LITERAL POOL                                                       +          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* OTHER DSECTS ARE HIDDEN IN HERE                                    *          
**********************************************************************          
         SPACE 1                                                                
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
*ACMSGEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
*DDCTRYEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
*DDTSARD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
*DDPERVALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROBCD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROBDD                                                       
         SPACE 1                                                                
**********************************************************************          
* DSECT TO COVER SAVED WORKING STORAGE                               *          
**********************************************************************          
         ORG   PRTWORK                                                          
SVFLAG   DS    X                   SAVED FLAG                                   
SVFLWJTB EQU   X'80'               WAS AT JOB TIME BEFORE                       
SVFLTSRE EQU   X'40'               DEALING WITH TIME SHEED RECORDS              
SAVECLI  DS    CL(L'CLICODE)       SAVED CLIENT                                 
SAVEPRO  DS    CL(L'PRODCODE)            PRODUCT                                
SAVEJOB  DS    CL(L'JOBNUM)              JOB                                    
SAVEST   DS    XL(L'ESTIMATE)            ESTIMATE TYPE AND VERSION              
SAVEWC   DS    XL(L'WCSUBWC)             WC PLUS SUB WC                         
SAVETOTA DS    PL(L'ESTITOTA)      TOTAL ESTIMATE AMOUNT FOR JOB                
WCTOTAL  DS    PL8                 WORK CODE TOTAL AMOUNT                       
TOTHOURS DS    PL6                 WORK CODE TOTAL HOURS                        
SVCLIOFF DS    CL2                 CLIENT OFFICE CODE                           
SVLCLI   DS    X                   SAVED CLIENT CODE LENGTH                     
SVLCLPRO DS    X                   SAVED CLIENT+PRODUCT LENGTH                  
LDGLV1   DS    X                   PERS LDG LEV 1 LENGTH                        
LDGLV2   DS    X                   PERS LDG LEV 2 LENGTH                        
LDGLV3   DS    X                   PERS LDG LEV 3 LENGTH                        
LDGLV4   DS    X                   PERS LDG LEV 4 LENGTH                        
LNLISTS  DS    X                   NUMBER OF LINES ON SCREEN                    
NSAVSCRS DS    X                   NUMBER OF SAVED SCREENS                      
NSAVELS  DS    H                   NUMBER OF SAVED EPT ELEMENTS                 
NSELACCS DS    X                   NUMBER OF SELECTED ACCOUNTS                  
LASTACC  DS    CL(L'ACTKACT)       LAST ACCOUNT ON SCREEN                       
TSLKSAVE DS    CL(L'TSLKEY)        SAVED TIME SHEET LIST RECORD KEY             
         ORG   LASTACC                                                          
FSTEL    DS    XL(TSARKLEN)        FIRST ELEMENT ON SCREEN                      
LASTEL   DS    XL(L'FSTEL)         LAST ELEMENT ON SCREEN                       
SAVELS   DS    XL(MAXELS/NLINES*L'LASTEL) SAVED BACKWARDS SCREENS               
         ORG   T60BFFD+4000                                                     
SAVEKEYS DS    CL(MAXKEYS*L'ACTKACT)  SAVED BACKWARDS SCREENS                   
SELACCS  DS    CL(MAXSEL*L'ACTKACT)  SELECTED ACCOUNTS                          
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER LOCAL WORKING STORAGE                               *          
**********************************************************************          
SUBSYSD  DSECT                                                                  
         ORG   DRGEN                                                            
LOCAL    DS    0X                                                               
*        THE FOLLOWING THREE VALUES HAVE TO BE AT THIS PLACE BECAUSE            
*        THEY WERE SET BY ACPRO43 TO PASSE THEM TO THIS PROGRAM                 
ESTIMATE DS    XL2                 ESTIMATE TYPE AND VERSION                    
ESTITOTA DS    PL6                 TOTAL AMOUNT FOR ESTIMATE                    
WCSUBWC  DS    XL3                 WORK CODE INCL. SUB WC                       
*                                                                               
VTSAR    DS    V                                                                
SAVERE   DS    A                                                                
ADRELEM  DS    A                   ADDRESSE OF ELEMENT                          
ASELACCS DS    A                   ADDRESSE OF SELECTED ACCOUNTS AREA           
INTMODE  DS    X                                                                
*                                                                               
CTRVALUE DS    0X                  COUNTRY SPECIFIC VALUES                      
CTRPLDG  DS    CL2                 PERSON LEDGER                                
CTRESTR  DS    C                   LETTER FOR REVISION ESTIMATE                 
CTRVALNQ EQU   *-CTRVALUE                                                       
*                                                                               
KEYCHG   DS    C                                                                
INDEX    DS    CL9                                                              
RATEKEY  DS    CL49                PERSON CHARGE RATE KEY                       
MYDATE   DS    PL3                                                              
PACCNAME DS    CL36                PERSON ACCOUNT NAME                          
RATEACC  DS    CL(L'EPTACT)        PERSON ACC CODE FOR GETRATE ROUTINE          
NDATE    DS    PL(L'EPTRDATE)                                                   
NHOUR    DS    PL(L'EPTHOURS)                                                   
NRATE    DS    PL(L'EPTRATE)                                                    
TEMPFLDH DS    CL8                                                              
TEMPFLD  DS    CL20                                                             
ADDORWRT DS    CL5                 ADD OR WRITE DATAMGR COMAND                  
PFLAG    DS    X                   PROGRAM FLAG                                 
*                                  JOB/STAFF                                    
PFLSELAL EQU   X'80'               SELECT ALL ACCOUNTS ON THE SCREEN            
PFLFSTLI EQU   X'40'               FIRST LINE ON SCREEN                         
*                                  JOB/TIME                                     
PFLNEWEL EQU   X'01'               NEW ELEMENT                                  
PFLDELDO EQU   X'02'               DELETE DESCRIPTION OVERWRITEN                
PFLDESOV EQU   X'04'               DESCRIPTION OVERWRITEN                       
PFLHOUWI EQU   X'08'               HOURS WERE INPUT                             
PFLRATWI EQU   X'10'               RATE WAS INPUT                               
PFLRATWR EQU   X'20'               RATE WAS READ                                
TSARBLK  DS    XL(TSARDL)                                                       
DDOUT    DS    0C                                                               
         DSDDL PRINT=YES                                                        
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER LINE ON JOB/STAFF SCREEN                            *          
**********************************************************************          
LNJOSTD  DSECT                                                                  
LSTACTH  DS    CL8                  SCREEN                                      
LSTACT   DS    CL2                 ACTION COLUMN                                
         DS    CL8                                                              
LSTOUTH  DS    CL8                                                              
LSTOUT   DS    0CL50                                                            
LSTCODE  DS    CL12                ACCOUNT CODE                                 
         DS    CL2                                                              
LSTNAME  DS    CL36                ACCOUNT NAME                                 
LSTLEN   EQU   *-LNJOSTD                                                        
         SPACE 1                                                                
**********************************************************************          
* DSECT TO COVER LINE ON JOB/TIME SCREEN                             *          
**********************************************************************          
LNJOTID  DSECT                                                                  
LTIACTH  DS    CL8                  SCREEN                                      
LTIACT   DS    CL2                 ACTION COLUMN                                
         DS    CL8                                                              
LTICODEH DS    CL8                                                              
LTICODE  DS    CL12                ACCOUNT CODE                                 
LTINAMEH DS    CL8                                                              
LTINAME  DS    CL36                DESCRIPTION                                  
LTIDATEH DS    CL8                                                              
LTIDATE  DS    CL8                 DATE                                         
LTIHOURH DS    CL8                                                              
LTIHOUR  DS    CL7                 HOURS                                        
LTIRATEH DS    CL8                                                              
LTIRATE  DS    CL8                 PERSON RATE                                  
LTILEN   EQU   *-LNJOTID                                                        
         SPACE 1                                                                
**********************************************************************          
* DSECT TO COVER PRINT LINE (PF10 ON JOB/TIME SCREEN)                *          
**********************************************************************          
PRID     DSECT                                                                  
         DS    CL2                 SPARE                                        
PRICODE  DS    CL12                PERSON ACCOUNT CODE                          
         DS    CL1                                                              
PRIDESC  DS    CL36                PERSON DESCRIPTION/NAME                      
         DS    CL1                                                              
PRIDATE  DS    CL8                 DATE                                         
         DS    CL1                                                              
PRIHOUR  DS    CL8                 HOURS                                        
         DS    CL1                                                              
PRIRATE  DS    CL9                 RATE                                         
         DS    CL1                                                              
PRIAMT   DS    CL13                AMOUNT                                       
         SPACE 1                                                                
**********************************************************************          
* EQUATES                                                            *          
**********************************************************************          
INSLINE  EQU   X'08'               ELEMENT MARKER FOR INSERTING A LINE          
*                                  ATTENTION: IF THIS BIT WILL BE USED          
*                                  ON THE FILE (ACGENFILE EPTINDS)              
*                                  IT NEEDS TO BE CHANGED                       
FSTLIST  EQU   1                   INTERNAL MODES                               
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
NLINES   EQU   15                  MAX NUMBER OF LINES ON SCREENS               
FIELDS   EQU   5                   INPUT FIELDS ON ONE SCREEN LINE              
MAXSEL   EQU   50                  MAX SELECTED ACCOUNTS ON JOB/STAFF           
MAXKEYS  EQU   50                  MAX SAVED SCREENS JOB/STAFF                  
MAXELS   EQU   50                  MAX PERSON TIME ELEMENTS PER WC              
TSARKLEN EQU   (L'EPTULA+L'EPTRDATE)  LENGTH OF TSAR KEY                        
MAXRECLE EQU   2000-7              MAXIMUM RECORD LENGTH                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACPRO5A   08/10/00'                                      
         END                                                                    
