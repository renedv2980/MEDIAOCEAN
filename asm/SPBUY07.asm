*          DATA SET SPBUY07    AT LEVEL 061 AS OF 11/03/20                      
*PHASE T21107C    <===                                                          
         TITLE 'T21107 - SPOTPAK BUY - CHANGE FUNCTIONS'                        
T21107   CSECT                                                                  
*&&ONLIN SET   Y                  ONLINE-ONLY MODULE                            
         PRINT NOGEN                                                            
         NMOD1 0,T21107,RR=R8                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21107+4096,R9                                                   
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         C     R8,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R8,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
HAVERELO L     R8,AOVWORK                                                       
         USING LOCALD,R8                                                        
         LR    RE,R8               CLEAR LOCAL STORAGE                          
         LA    RF,L'OVWORK                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   BUDEMSW,0           RESET DEMO RESEQ SWITCH                      
*                                                                               
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         LA    R4,8(R2)            ON ENTRY R2 HAS A(FIRST ACTV FLDHDR)         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         B     CHA10                                                            
         SPACE 1                                                                
EQXIT    CR    RB,RB                                                            
         J     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
BADKERR  MVI   ERRCD,BADKEYWD                                                   
*                                                                               
BUYERR   GOTO1 ERROR                                                            
*                                                                               
CMERR    CLI   CMPASS,1            TEST FIRST PASS                              
         BE    *+6                                                              
         DC    H'0'                FORCE DUMP IN SECOND PASS                    
         MVI   ERRAREA,X'FF'                                                    
         LH    R0,THISLIN          GET LINE NUMBER                              
         MVC   BUYMSG(16),=C'** ERROR ON LINE'                                  
         LA    R6,BUYMSG+17                                                     
         EDIT  (R0),(3,(R6)),ALIGN=LEFT                                         
         AR    R6,R0                                                            
         MVC   1(2,R6),=C'**'                                                   
         XC    SVKEY+14(4),SVKEY+14  SET AS NO LINE RECALLED                    
         B     BUYERR                                                           
*                                                                               
* ORIGINAL DATA ERROR EXIT                                                      
*                                                                               
CMOERR   MVI   ERRAREA,X'FF'                                                    
         LH    R0,THISLIN          GET LINE NUMBER                              
         MVC   BUYMSG(7),=C'** LINE'                                            
         LA    R6,BUYMSG+8                                                      
         EDIT  (R0),(3,(R6)),ALIGN=LEFT                                         
         AR    R6,R0                                                            
         MVC   1(30,R6),=C'HAS DIFFERENT ORIGINAL DATA **'                      
         B     BUYERR                                                           
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
CHA10    DS    0H                                                               
         CLI   FLEN+1,2            TEST FOR ACTION CODE LENGTH=2                
         BNE   CHA12                                                            
         CLC   =C'CM',0(R4)        TEST FOR CHANGE MULTIPLE                     
         BE    CM                                                               
*                                                                               
CHA12    CLC   =C'RC=',0(R4)       TEST REASON CODE                             
         BNE   CHA14                                                            
         BRAS  RE,VALRSN                                                        
         B     CHA82                                                            
*                                                                               
CHA14    TM    UPSW,UPON+UPCHA     TEST UPLOADING                               
         BO    CHA20               YES-BUY IS ALREADY IN REC                    
         TM    WRKRUPSW,WRKRUPSW_NOIO OR WRKR UPLOAD                            
         BO    CHA20                                                            
*                                                                               
         MVI   ERRCD,NORECALL                                                   
         L     RE,ADRSVKEY                                                      
         OC    14(4,RE),14(RE)                                                  
         BZ    BUYERR                                                           
         MVC   KEY,0(RE)                                                        
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ERRCD,NOTFOUND                                                   
         TM    REC+15,X'80'        TEST DELETED                                 
         BO    BUYERR                                                           
*                                                                               
         L     RE,AREC                                                          
         CLC   SVKEY(10),0(RE)     CHECK SAME KEY                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SVKEY+11(2),10(RE)  CHECK SAME LINE NUMBER                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHA20    MVC   OLDBUYLN,BUYREC+13  SAVE BUYREC LENGTH                           
         TM    SVAFLAG1,X'22'      TEST CTA OR TRADE ACTIVE                     
         BZ    CHA22                                                            
         GOTO1 GOGETCTA,DMCB,('CIBCPYQ',AREC)                                   
CHA22    DS    0H                                                               
*                                                                               
CHA24    MVC   BUSTAT,BDSTAT                                                    
*                                                                               
         CLI   T211FFD+1,C'*'      TEST DDS TERMINAL                            
         BNE   CHA26                                                            
         CLC   =C'C,PKGFIX=',0(R4)                                              
         BNE   CHA26                                                            
         BRAS  RE,PKGFIX                                                        
         B     CHA80                                                            
*                                                                               
CHA26    MVI   SEPSPOT,0                                                        
         CLC   =C'C,SPLIT=',0(R4)   SPLIT BUYLINE?                              
         BE    CHA26SP                                                          
         CLC   =C'C,SEPSPOT=',0(R4) SEPARATE SPOT?                              
         BNE   CHA28                                                            
         MVI   SEPSPOT,C'Y'         SET SEPARATE SPOT FLAG FOR LATER            
CHA26SP  BRAS  RE,BUYSP                                                         
         B     CHA80                                                            
         EJECT                                                                  
CHA28    DS    0H                                                               
         CLC   =C'SD',0(R4)                                                     
         BE    CHA54                                                            
*                                                                               
         MVI   ERRCD,TRCDERR                                                    
         CLI   FLEN+1,1            C OR R                                       
         BNE   BUYERR                                                           
*                                                                               
         CLI   FSTOP,C','                                                       
         BE    CHA40                                                            
         CLI   0(R4),C'S'          SCHEDULE (NEW FORM)                          
         BNE   CHA30                                                            
         MVI   SKEDTYP,C'N'        SET NEW SKED FUNCTION                        
         CLI   SVRCLOPT,RCLDSK     TEST LAST RECALL DAILY SKED                  
         BE    CHA54                                                            
         B     CHA58                                                            
         SPACE 1                                                                
*================================================================*              
* COMMENTS AND ORBITS CAN BE CHANGED IN DISPLAY AREA                            
* IF PREVIOUSLY RECALLED                                                        
*================================================================*              
         SPACE 1                                                                
CHA30    DS    0H                                                               
         CLI   SVRCLOPT,RCLCOM                                                  
         BNE   *+12                                                             
         BAS   RE,COMD                                                          
         B     CHA80                                                            
         CLI   SVRCLOPT,RCLORB                                                  
         BNE   *+12                                                             
         BAS   RE,ORB                                                           
         B     CHA80                                                            
* CANADIAN NETWORK BUYLINE STATION PCTS                                         
         CLI   SVRCLOPT,RCLSTA                                                  
         BNE   *+12                                                             
         BAS   RE,SPCT                                                          
         B     CHA80                                                            
         CLI   SVRCLOPT,RCLDT      NTWK DAYS/TIMES                              
         BNE   *+12                                                             
         BAS   RE,SDT                                                           
         B     CHA80                                                            
         EJECT                                                                  
* CHECK FOR A KEYWORD                                                           
*                                                                               
CHA40    XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(2),=C',='                                                 
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,1             MISSING INPUT FLD                            
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         MVI   ERRCD,BADKEYWD                                                   
         CLC   =C'PUR=',0(R4)                                                   
         BNE   CHA41                                                            
         BRAS  RE,PUR                                                           
         B     CHA80                                                            
                                                                                
CHA41    CLC   =C'ID=',0(R4)                                                    
         BNE   CHA44                                                            
         BRAS  RE,ID                                                            
         MVC   KEY,SVKEY           RESTORE ORIGINAL BUY RECORD                  
         OC    KEY+14(4),KEY+14    TEST RECORD STILL AVAILABLE                  
         BZ    CHA82               NO                                           
         GOTO1 GETREC                                                           
         MVC   OLDBUYLN,BUYREC+13  SAVE BUYREC LENGTH                           
         B     CHA80                                                            
*                                                                               
CHA44    L     R1,=A(CHGTAB)                                                    
         A     R1,RELO                                                          
         USING CHGTABD,R1                                                       
         LA    RE,L'CHGTAB                                                      
         L     RF,=A(CHGTABX)                                                   
         A     RF,RELO                                                          
         BCTR  R5,0                                                             
CHA46    CLC   FLEN+1(1),CHGMINL                                                
         BL    CHA48                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   CHGNAME(0),0(R4) *EXECUTED*                                      
         BE    CHA64                                                            
CHA48    BXLE  R1,RE,CHA46                                                      
*                                                                               
         CLC   =C'PROG=-S',0(R4)   TEST FOR SPECIAL NOTATION                    
         BNE   *+12                                                             
         BAS   RE,SPEC                                                          
         B     CHA80                                                            
         CLC   =C'PROG=NS',0(R4)   TEST TO REMOVE SPECIAL NOTATION              
         BNE   *+12                                                             
         BAS   RE,REMSPEC                                                       
         B     CHA80                                                            
*                                                                               
* CHECK FOR SPECIAL PROCESSING KEYWORDS                                         
*                                                                               
         CLC   =C'PST/',0(R4)      TEST PST CHANGE                              
         BNE   *+12                                                             
         BAS   RE,CHPST                                                         
         B     CHA80                                                            
*                                                                               
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   CHA56                                                            
*                                                                               
         CLC   =C'SD',0(R4)        SCHEDULE DAYS                                
         BNE   CHA56                                                            
*                                                                               
CHA54    DS    0H                                                               
         BRAS  RE,DSKED                                                         
         MVI   RCLOPT,RCLDSK                                                    
         B     CHA82                                                            
*                                                                               
CHA56    CLC   =C'SKED',0(R4)                                                   
         BE    *+14                                                             
         CLC   =C'SK',0(R4)                                                     
         BNE   CHA60                                                            
         MVI   SKEDTYP,C'O'                                                     
*                                                                               
CHA58    BRAS  RE,BUYSKED                                                       
         MVI   RCLOPT,RCLSCH                                                    
*                                                                               
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   CHA80               PUTREC DONE IN SPBUY12                       
*                                                                               
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
*                                          TSTDARE IS CALLED IN SPBUY12         
         LA    RF,LKUP                                                          
         CLI   DEMLKSW,0           TEST DEMO LOOK-UP REQUIRED                   
         BNE   *+8                 YES                                          
         LA    RF,UPDREQ           NO-UPDATE BUY AND HANDLE REQUEST             
         BASR  RE,RF                                                            
         B     CHA80                                                            
*                                                                               
CHA60    B     BADKERR                                                          
         EJECT                                                                  
CHA64    MVC   BUCHGOV,CHGOV       SAVE OVLY                                    
         MVC   EDTVAL,CHGEDT       SET EDIT ROUTINE CODE                        
         LR    R7,R1               SAVE A(CHANGE TABLE ENTRY)                   
         CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   CHA66               NO                                           
         CLI   BUYMD,C'N'          TEST NTWK                                    
         BNE   CHA68                                                            
         LA    RE,EXP              CODES FOR EXP BUYS                           
         OC    SVNDEF(16),SVNDEF                                                
         BZ    *+8                                                              
         LA    RE,NBUY             CODES FOR NTWK BUYS                          
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    CHGCTL,0                                                         
         BZ    CHA60                                                            
         B     CHA68                                                            
*                                                                               
CHA66    TM    CHGCTL,CANAD        NOT CANAD AGY - TEST CANAD ONLY              
         BO    CHA60                                                            
*                                                                               
         EJECT                                                                  
CHA68    BRAS  RE,CHKRSN           CHECK RSNCODE PRESENT IF NEEDED              
         TM    CHGCTL,NOEDT        TEST IF EDIT NEEDED                          
         BO    CHA72               NO                                           
*                                                                               
CHA70    GOTO1 CALLEDT             EDIT INPUT DATA                              
*                                                                               
CHA72    DS    0H                                                               
         CLI   BUCHGOV,0           TEST CHANGE LOGIC IN THIS OVLY               
         BE    CHA78               YES                                          
*                                                                               
         TM    UPSW,UPON           SKIP CHANGE LOGIC IF UPLOADING AND           
         BZ    CHA74               IT'S A PERIOD CHANGE                         
         CLI   EDTVAL,PEREDT                                                    
         BNE   CHA74                                                            
         MVC   BDSTART(6),BUSTARTB SET BUY PERIOD FIELDS                        
         MVC   BDWKIND,BUWKIND                                                  
         MVC   BDDAY,BUDAYS                                                     
         MVC   BDSEDAY,BUDAYNUM                                                 
         MVC   BDINPUT,BUPERIND                                                 
         MVC   BDWKS,BUWKS                                                      
         B     CHA76                                                            
*                                                                               
CHA74    DS    0H                                                               
         GOTO1 CALLCHA             GET CHANGE LOGIC                             
*                                                                               
CHA76    BAS   RE,CHKMAXEL                                                      
*                                                                               
         CLI   DEMLKSW,C'Y'        TEST DEMO LOOK-UP REQ'D                      
         BNE   *+8                                                              
         BAS   RE,LKUP                                                          
         B     CHA80                                                            
*                                                                               
CHA78    DS    0H                                                               
         LR    R1,R7               RESTORE A(CHANGE TABLE ENTRY)                
         SR    RF,RF               GET DATA ROUTINE ADDRESS                     
         ICM   RF,7,CHGROUT                                                     
         GOTO1 (RF),RR=Y                                                        
         B     CHA80                                                            
         EJECT                                                                  
* DISPLAY LINE                                                                  
*                                                                               
CHA80    DS    0H                                                               
         GOTO1 CALLDSP                                                          
*                                                                               
* MOVE '*' TO FIRST POSITION OF INPUT LINE                                      
*                                                                               
CHA82    C     R2,FLAST            TEST IN DSPLY AREAS                          
         BNH   *+12                NO                                           
         LA    R2,BUYINP1H         YES-POINT TO COMMAND                         
         MVI   BUYINP2H+5,0         AND IGNORE FURTHER INPUT                    
*                                                                               
         MVC   ELEM(L'BUYINP1),8(R2)                                            
         MVI   8(R2),C'*'                                                       
         MVC   9(L'BUYINP1-1,R2),ELEM                                           
         FOUT  (R2)                                                             
*                                                                               
CHA90    CLI   BUYMSG,0                                                         
         BNE   *+10                                                             
         MVC   BUYMSG(22),=C'** ACTION COMPLETED **'                            
         B     EXIT                                                             
         EJECT                                                                  
*==================================================================             
* CHANGE MULTIPLE LOGIC - ASSUMES ALL CHANGE ROUTINES ARE IN                    
* SPBUY07 AND SPBUY12                                                           
*==================================================================             
         SPACE 1                                                                
CM       SR    R0,R0               INSURE NO MORE TRANSACTIONS                  
         LR    RF,R2               ON SCREEN                                    
*                                                                               
CM1      IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         C     RF,FLAST            TEST PAST LAST INPUT LINE                    
         BH    CM2                 YES                                          
         CLI   5(RF),0             TEST FOR BLANK LINE                          
         BE    CM2                 YES-ALL DONE                                 
         CLI   8(RF),C'*'          TEST NO-OPED LINE                            
         BE    CM1                 YES-CHECK NEXT ONE                           
         MVI   ERRAREA,X'FF'                                                    
         MVC   BUYMSG(L'MULTMSG),MULTMSG                                        
         B     BUYERR                                                           
*                                                                               
CM2      BAS   RE,EDLIN            EDIT LIST OF LINE NUMBERS                    
         MVI   CMPASS,1            SET FOR FIRST PASS                           
         XC    FSTOPS,FSTOPS       EDIT FOR DATA KEYWORD                        
         MVC   FSTOPS(2),=C',='                                                 
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,1                                                          
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
*                                                                               
CM3      MVI   ERRCD,BADKEYWD                                                   
         L     R1,=A(CMTAB)                                                     
         A     R1,RELO                                                          
         USING CHGTABD,R1                                                       
         LA    RE,L'CMTAB                                                       
         L     RF,=A(CMTABX)                                                    
         A     RF,RELO                                                          
         BCTR  R5,0                                                             
*                                                                               
CM4      CLC   FLEN+1(1),CHGMINL   TEST MEETS MINIMUM LEN FOR KEY               
         BL    CM5                 NO                                           
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   CHGNAME(0),0(R4)                                                 
         BE    CM6                                                              
CM5      BXLE  R1,RE,CM4                                                        
         L     R1,=A(SPECENT)                                                   
         A     R1,RELO                                                          
         CLC   =C'PROG=-S',0(R4)                                                
         BE    CM6                                                              
         L     R1,=A(REMSPENT)                                                  
         A     R1,RELO                                                          
         CLC   =C'PROG=NS',0(R4)                                                
         BE    CM6                                                              
         B     BUYERR                                                           
*                                                                               
CM6      CLI   SVAPROF+7,C'C'      TEST CANADIAN NETWORK                        
         BNE   CM6US                                                            
         CLI   BUYMD,C'N'                                                       
         BNE   CM7                                                              
         LA    RE,EXP                                                           
         OC    SVNDEF(16),SVNDEF                                                
         BZ    *+8                                                              
         LA    RE,NBUY                                                          
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    CHGCTL,0                                                         
         BZ    BUYERR                                                           
         B     CM7                                                              
CM6US    TM    CHGCTL,CANAD        NOT CANAD AGY - TEST CANAD ONLY              
         BO    BUYERR                                                           
*                                                                               
CM7      ST    R1,ACHGNTRY         SAVE A(TABLE ENTRY)                          
         STM   R4,R5,SAVER4        SAVE R4 AND R5                               
         MVC   SVFLDVAL,FADDR      SAVE FLDVAL VALUES                           
         LH    R6,NLINES           R6=COUNTER                                   
         LA    R7,LINLIST          R7=A(NEXT LINE TO PROCESS)                   
*                                                                               
CM8      BRAS  RE,GETNEXT          GET NEXT BUYLINE NUMBER                      
         BNE   CM20                DID NOT FIND IT                              
         MVC   THISLIN,0(R7)       SAVE LINE BEING PROCESSED                    
         MVC   SVKEY+11(2),0(R7)   SAVE LINE IN KEY AREA                        
         MVI   DEMLKSW,0                                                        
         MVC   FADDR(L'SVFLDVAL),SVFLDVAL                                       
         LM    R4,R5,SAVER4        RESTORE R4 AND R5                            
         L     R1,ACHGNTRY                                                      
         USING CHGTABD,R1                                                       
         MVC   BUCHGOV,CHGOV                                                    
         MVC   EDTVAL,CHGEDT                                                    
         SR    RF,RF                                                            
         ICM   RF,7,CHGORIG        GET A(ORIGINAL DATA CHECK)                   
         BZ    CM9                                                              
         GOTO1 (RF),RR=Y                                                        
*                                                                               
CM9      TM    CHGCTL,NOEDT                                                     
         BO    CM10                                                             
         GOTO1 CALLEDT                                                          
*                                                                               
CM10     CLI   BUCHGOV,0                                                        
         BE    CM15                                                             
         GOTO1 CALLCHA                                                          
         BNE   CMERR               ERROR FOUND IN CHANGE MODULE                 
         BAS   RE,CHKMAXEL                                                      
         CLI   DEMLKSW,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,LKUP                                                          
         B     CM20                                                             
*                                                                               
CM15     SR    RF,RF                                                            
         ICM   RF,7,CHGROUT                                                     
         GOTO1 (RF),RR=Y                                                        
*                                                                               
CM20     LA    R7,2(R7)            NEXT BUYLINE                                 
         BCT   R6,CM8                                                           
         CLI   CMPASS,1            TEST IF ON FIRST PASS                        
         BNE   CMX                 NO-ALL DONE                                  
         MVI   CMPASS,2            YES-RESET TO DO LIST                         
         LH    R6,NLINES                                                        
         LA    R7,LINLIST                                                       
         B     CM8                                                              
*                                                                               
CMX      CLI   BUYMSG,0            TEST BUY MESSAGE PREVIOUSLY SET              
         BNE   *+10                YES                                          
         MVC   BUYMSG(31),=C'** MULTIPLE CHANGE COMPLETED **'                   
         XC    SVKEY+14(4),SVKEY+14  LEAVE AS NO LINE RECALLED                  
         B     EXIT                                                             
         EJECT                                                                  
* ROUTINES TO CHECK THAT ORIGINAL DATA IS THE SAME ON CHANGE                    
* MULTIPLE BUY LINES                                                            
*                                                                               
CHKPER   STM   RE,R1,WORK                                                       
         MVC   DUB(6),BDSTART                                                   
         MVC   DUB+6(1),BDDAY                                                   
         LA    R1,7                                                             
         LA    RF,DUB                                                           
         BAS   RE,CHKORIG                                                       
         B     CHKX                                                             
*                                                                               
CHKPRO   STM   RE,R1,WORK                                                       
         LA    RF,BDPROGRM                                                      
         LA    R1,L'BDPROGRM                                                    
         BAS   RE,CHKORIG                                                       
         B     CHKX                                                             
*                                                                               
CHKADJ   STM   RE,R1,WORK                                                       
         LA    RF,BDPROGT                                                       
         LA    R1,L'BDPROGT                                                     
         BAS   RE,CHKORIG                                                       
         B     CHKX                                                             
*                                                                               
CHKTAX   STM   RE,R1,WORK                                                       
         LA    RF,BDNTAX                                                        
         LA    R1,L'BDNTAX                                                      
         BAS   RE,CHKORIG                                                       
         B     CHKX                                                             
*                                                                               
CHKDPT   STM   RE,R1,WORK                                                       
         LA    RF,BDDAYPT                                                       
         LA    R1,L'BDDAYPT                                                     
         BAS   RE,CHKORIG                                                       
         B     CHKX                                                             
*                                                                               
CHKSLN   STM   RE,R1,WORK                                                       
         LA    RF,BDSEC                                                         
         LA    R1,L'BDSEC                                                       
         BAS   RE,CHKORIG                                                       
         B     CHKX                                                             
*                                                                               
CHKCOS   STM   RE,R1,WORK                                                       
         MVC   DUB(3),BDCOST                                                    
         MVC   DUB+3(1),BDCIND                                                  
         MVC   DUB+4(1),BDCIND2                                                 
         LA    RF,DUB                                                           
         LA    R1,5                                                             
         BAS   RE,CHKORIG                                                       
         B     CHKX                                                             
*                                                                               
CHKTIM   STM   RE,R1,WORK                                                       
         LA    RF,BDTIMST                                                       
         LA    R1,4                L'BDTIMST+L'BDTIMEND                         
         BAS   RE,CHKORIG                                                       
         B     CHKX                                                             
*                                                                               
CHKREP   STM   RE,R1,WORK                                                       
         LA    RF,BDREP                                                         
         LA    R1,L'BDREP                                                       
         BAS   RE,CHKORIG                                                       
*                                                                               
CHKX     LM    RE,R1,WORK                                                       
         BR    RE                                                               
                                                                                
*===============================================================                
* FOLLOWING SUBR IS ENTERED BECAUSE IT CHANGES R6 !                             
*===============================================================                
                                                                                
CHKCOS2  NTR1                                                                   
         MVI   ELCDLO,X'71'                                                     
         MVI   ELCDHI,X'71'                                                     
         TM    SVCOPT1,COP1COSQ    TEST DOLLARS OR FACTOR                       
         BO    CHKCOS2A            FACTOR                                       
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL           IF THERE IS AN ELEMENT                       
         BNE   CHKCOS2X                                                         
         MVC   DUB(3),3(R6)                                                     
         LA    RF,DUB                                                           
         LA    R1,3                                                             
         BAS   RE,CHKORIG                                                       
         B     CHKCOS2X                                                         
*                                                                               
CHKCOS2A MVI   ELCDLO,X'73'                                                     
         MVI   ELCDHI,X'73'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL           IF THERE IS AN ELEMENT                       
         BNE   CHKCOS2X                                                         
         MVC   DUB(2),4(R6)                                                     
         LA    RF,DUB                                                           
         LA    R1,4                                                             
         BAS   RE,CHKORIG                                                       
*                                                                               
CHKCOS2X J     EXIT                                                             
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE FOR DEMOGRAPHIC UPGRADES *                                        
         SPACE 1                                                                
BUP      NTR1                                                                   
         CLI   FSTOP,C','          TEST STOP CHAR                               
         BE    BUPR                                                             
         GOTO1 DEMUPGD             CALL UPGRADE ROUTINES                        
         MVI   RCLOPT,X'07'        SET FOR DEMO DISPLAY                         
         MVI   BUWHY2,X'08'                                                     
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
         BAS   RE,CHGPUT                                                        
         B     EQXIT                                                            
*                                                                               
BUPR     B     BADKERR                                                          
         EJECT                                                                  
* PST ELEMENTS                                                                  
*                                                                               
CHPST    NTR1                                                                   
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
CHPST2   ICM   R0,1,1(R6)                                                       
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    CHPST04                                                          
         CLI   0(R6),6                                                          
         BL    CHPST2                                                           
         CLI   0(R6),13                                                         
         BH    CHPST2                                                           
         OC    4(2,R6),4(R6)                                                    
         BZ    CHPST2                                                           
         CLC   2(2,R6),=X'BB61'    TEST ELEM PRIOR TO NOV1/93                   
         BL    CHPST2                                                           
         MVI   ERRCD,BLLDPAID                                                   
         B     BUYERR                                                           
*                                                                               
CHPST04  LA    R4,4(R4)            POINT TO PROVINCE CODE                       
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         XC    FSTOPS,FSTOPS       GET ENTIRE STRING                            
         GOTO1 FLDVAL                                                           
         LR    R1,R5                                                            
*                                                                               
         XC    WORK,WORK           BLD A TEMP FLD HEADER                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(R4)     SET INPUT                                    
         LA    R1,1(R1)                                                         
         STC   R1,WORK+5           SET LENGTH                                   
         LA    R1,8(R1)                                                         
         STC   R1,WORK                                                          
*                                                                               
         L     R4,AREC2                                                         
         LA    R4,1000(R4)         WHO KNOWS WHY WE ARE USING +1000 ?           
         USING PSTBLKD,R4                                                       
         XC    0(200,R4),0(R4)     CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,WORK                                                          
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VCOMFACS    A(COMFACS)                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         CLI   PSTERR,0                                                         
         BNE   BADKERR                                                          
*                                                                               
         CLC   BDSTART,=X'6E061C'  TEST BUY FOR JUL/10 +                        
         BL    CHPST08                                                          
*                                                                               
         LA    R0,10                                                            
         LA    R1,PSTOUT                                                        
         SR    RF,RF                                                            
*                                                                               
CHPST06  CLI   0(R1),0                                                          
         BE    *+8                                                              
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,CHPST06                                                       
         CHI   RF,1                                                             
         BNH   CHPST08                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(ONLY1PST)                                              
         GOTO1 ERROR                                                            
*                                                                               
CHPST08  CLI   SVAPROF+7,C'C'      IF CANADIAN NETWORK                          
         BNE   CHPST10                                                          
         CLI   BUYMD,C'N'          TEST MEDIA N                                 
         BNE   CHPST10                                                          
         CLC   BUYKEY+4(2),=X'0000'   TEST EXPLODED BUY                         
         BE    CHPST50                                                          
*                                                                               
*        NOW FIX/BLD ELEMENT - PSTOUT HAS 10 CHAR                               
*                                                                               
CHPST10  BRAS  RE,SETPST                                                        
         BAS   RE,CHGPUT                                                        
         B     CHPSTX                                                           
*                                                                               
* SAVE REC IN REC 3                                                             
*                                                                               
CHPST50  DS    0H                  CANADIAN NETWORK                             
         CLC   BDSTART,=X'6E061C'  BUY START AFTER JUN28/10                     
         BL    CHPST52                                                          
         BRAS  RE,SETPST           PUT PST EL IN NETWORK BUY                    
         BAS   RE,CHGPUT                                                        
*                                                                               
CHPST52  MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         LR    R7,R6                                                            
*                                                                               
CHPST60  MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         LR    R6,R7               SAVE A(EXPLODED BUY)                         
         BAS   RE,NEXTEL                                                        
         BNE   CHPST70                                                          
         LR    R7,R6                                                            
*                                                                               
         BAS   RE,GETEXP           READ EXPLODED KEY/REC                        
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         BRAS  RE,SETPST                                                        
         BAS   RE,CHGPUT                                                        
         B     CHPST60                                                          
*                                                                               
CHPST70  MVC   DUB(4),AREC3        FROM                                         
         MVC   DUB+4(4),AREC1      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
CHPSTX   B     EQXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE FOR BOOK CHANGES                                                  
*                                                                               
BK       NTR1                                                                   
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    *+12                                                             
         BAS   RE,CNET                                                          
         B     BKX                                                              
*                                                                               
         MVI   DEMLKSW,C'Y'        SET DEMO LOOK-UP REQ'D                       
         MVI   RCLOPT,X'07'        SET FOR DEMO DISPLAY                         
         SPACE 1                                                                
* REMOVE UPGRADE ELEMENT IF PRESENT *                                           
         SPACE 1                                                                
         MVI   ELCDLO,X'62'                                                     
         MVI   ELCDHI,X'62'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   *+8                                                              
         BAS   RE,DELEL                                                         
*                                                                               
BK2      LA    R6,BDELEM                                                        
*                                                                               
BK4      MVI   ELCDLO,2                                                         
         MVI   ELCDHI,3                                                         
         XC    SVSPLMKT,SVSPLMKT                                                
         BAS   RE,NEXTEL                                                        
         BNE   BK8                                                              
         MVC   2(2,R6),BUBOOK      MOVE BOOK TO NEW ELEM                        
         CLI   0(R6),2                                                          
         BE    *+10                                                             
         MVC   SVSPLMKT,4(R6)                                                   
         MVI   BUDEMSW,0           RESET SWITCH                                 
         MVI   BUDLUSW,C'N'                                                     
         GOTO1 VBLDDEM             CONSTRUCT NEW DEMO ELEM                      
* RESET OVERRIDES                                                               
         ZIC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNP   BK4                                                              
         LA    R1,24(R6)                                                        
*                                                                               
BK6      SRL   R0,3                                                             
         XC    4(4,R1),4(R1)                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         B     BK4                                                              
*                                                                               
BK8      BAS   RE,LKUP                                                          
*                                                                               
BKX      B     EQXIT                                                            
         EJECT                                                                  
*=======================================================*                       
* SUB-ROUTINE FOR BOOK TYPE CHANGES                     *                       
*=======================================================*                       
                                                                                
BT       NTR1                                                                   
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
*                                                                               
         LA    R1,3(R4)              POINT TO BOOKTYPE VALUE                    
         LHI   RF,VEDITBTY-BUYSAVE   GET DSPL TO EDIT ROUTINE                   
         AR    RF,RA                 POINT TO ROUTINE ADDRESS                   
         L     RF,0(RF)                                                         
         GOTO1 (RF)                                                             
*                                                                               
         XC    SVSPLMKT,SVSPLMKT                                                
         MVI   DEMLKSW,C'Y'        SET DEMO LOOK-UP REQ'D                       
         MVI   RCLOPT,X'07'        SET FOR DEMO DISPLAY                         
         SPACE 1                                                                
* FIND EXISTING DEMO LOOK UP OVERRIDE ELEMENT *                                 
         SPACE 1                                                                
         MVI   ELCDLO,X'24'                                                     
         MVI   ELCDHI,X'24'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   BT2                                                              
         MVC   ELEM(64),0(R6)       SAVE EXISTING ELEMENT                       
         BAS   RE,DELEL                                                         
         B     BT4                                                              
*                                                                               
BT2      XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'2406'                                                 
*                                                                               
BT4      MVC   ELEM+2(1),BUBKTYPE                                               
*                                                                               
BTX      BAS   RE,ADDEL                                                         
         BAS   RE,LKUP                                                          
*                                                                               
         MVI   BUBKTYPE,0          CLEAR IT NOW SINCE WE DON'T NEED IT          
         B     EQXIT                                                            
         EJECT                                                                  
*==========================================================                     
* SUB-ROUTINE TO CHANGE COMMENTS IN DSP AREA                                    
*==========================================================                     
         SPACE 1                                                                
COMD     NTR1                                                                   
         MVI   EDTVAL,COMEDT                                                    
         MVI   RCLOPT,RCLCOM                                                    
         LA    R2,BUYOUTH                                                       
         BAS   RE,FNDUF                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
*                                                                               
COMD2    LA    R7,1(R7)                                                         
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 CALLEDT                                                          
* IF OLD COMMENT IN REC DELETE IT                                               
         LA    R6,BDELEM                                                        
COMD4    BAS   RE,NEXTEL                                                        
         BNE   COMD6                                                            
         CLC   ELEM+2(1),2(R6)                                                  
         BNE   COMD4                                                            
         BAS   RE,DELEL                                                         
COMD6    CLC   =C'DELETE',ELEM+3                                                
         BE    COMD7                                                            
* ADD NEW COMMENT                                                               
         BAS   RE,ADDEL                                                         
*                                                                               
COMD7    BAS   RE,FNDUF                                                         
         BNE   COMD8                                                            
         CLI   5(R2),0                                                          
         BE    *-12                                                             
         B     COMD2                                                            
*                                                                               
COMD8    DS    0H                                                               
*                                                                               
COMD10   MVI   BUWHY,X'08'                                                      
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
         BAS   RE,CHGPUT                                                        
         MVI   RCLOPT,RCLCOM                                                    
         B     EQXIT                                                            
         EJECT                                                                  
*==========================================================                     
* SUB-ROUTINE TO CHANGE COMMENTS IN INPUT AREA                                  
*==========================================================                     
         SPACE 1                                                                
COMI     NTR1                                                                   
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
         CLC   =C'FIXTRF',ELEM+3                                                
         BE    COMTRF                                                           
*                                                                               
         CLI   ELEM+1,8            INPUT FORMAT IS COM=1-GST=X                  
         BNE   *+14                                                             
         CLC   =C'GST=',ELEM+3                                                  
         BE    COMGST                                                           
*                                                                               
         CLI   ELEM+1,11           INPUT FORMAT IS COM=1-DAILY=NO               
         BNE   *+14                                                             
         CLC   =C'DAILY=NO',ELEM+3                                              
         BE    COMDAILY                                                         
*                                                                               
         CLC   =C'FIXLK',ELEM+3                                                 
         BE    COMFXLK                                                          
         CLC   =C'REBERIT',ELEM+3                                               
         BE    COMFXLK                                                          
*                                                                               
* SEARCH REC FOR THIS COMMENT (EDIT STORED NUM IN EL+2)                         
*                                                                               
COMI2    BAS   RE,NEXTEL                                                        
         BNE   COMI6                                                            
         CLC   ELEM+2(1),2(R6)                                                  
         BNE   COMI2                                                            
* FOUND - ONLY VALID IF LAST ACTION WAS COM DSPLY                               
         CLI   SVRCLOPT,RCLCOM                                                  
         BE    COMI4                                                            
COMI3    DS    0H                                                               
         MVI   RCLOPT,RCLCOM                                                    
         GOTO1 CALLDSP                                                          
         MVI   ERRCD,DSPCMNT       AND TELL HIM TO LOOK AT THEM                 
         B     BUYERR                                                           
COMI4    DS    0H                                                               
         BAS   RE,DELEL                                                         
COMI6    DS    0H                                                               
         CLC   =C'DELETE',ELEM+3                                                
         BE    *+8                                                              
* COMMENT NOT IN REC - ADD IT                                                   
COMIX    BAS   RE,ADDEL                                                         
*                                                                               
COMIX10  MVI   BUWHY,X'08'                                                      
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
         BAS   RE,CHGPUT                                                        
         MVI   RCLOPT,RCLCOM                                                    
         B     EQXIT                                                            
         EJECT                                                                  
*========================================================*                      
* VALIDATE AND SAVE REASON CODE                                                 
* SYNTAX IS RC=12345(,THIS IS MY REASON)                                        
*========================================================*                      
         SPACE 1                                                                
VALRSN   DS    0H                                                               
*&&DO                                                                           
* DO NOT DISPLAY REASON CODE AS IT MUST BE ENTERED FOR EACH INPUT               
         OI    BUYINFH+6,X'80'                                                  
         XC    BUYINF(20),BUYINF                                                
*&&                                                                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOREASON)                                              
         CLI   SVANYLOK,C'Y'       IS BUY LOCKED                                
         BNE   BUYERR              NO - SHOULD NOT BE ENTERED                   
*                                                                               
         LHI   R6,SVRSNEL-BUYSAVE                                               
         AR    R6,RA                                                            
         XC    0(69,R6),0(R6)      CLEAR PREVIOUS                               
*                                                                               
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C'='                                                      
         XC    FLEN,FLEN           SET TO RE-EDIT                               
         GOTO1 FLDVAL              REREAD RC=                                   
*                                                                               
         MVI   FSTOPS,C','         NOW GET LEN OF RC INPUT                      
         GOTO1 FLDVAL                                                           
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADRSN)                                                
         CLI   FLEN+1,6            MAX 6 CHAR REASON CODE                       
         BH    BUYERR                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
*                                                                               
         MVI   0(R6),X'90'         SET ELEMENT CODE                             
         MVI   1(R6),RCELLENQ      SET ELEM LEN W/O TEXT                        
         BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   2(0,R6),0(R4)       SAVE THE REASON CODE                         
         OC    2(6,R6),SPACES                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D18'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(6),2(R6)                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR                                                           
*                                                                               
         MVC   AREC,AREC1                                                       
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AREC                                                          
         USING RSNRECD,R6                                                       
*                                                                               
         CLI   RSNELINP,C'Y'       TEST TEXT REQUIRED                           
         BE    VALRSN10                                                         
*                                                                               
         CLI   FSTOP,0             NO TEXT REQUIRED ==> NO STOP CHAR            
         BNE   BUYERR                                                           
         B     VALRSNX                                                          
*                                                                               
VALRSN10 MVI   FSTOPS,0            READ TO END OF INPUT                         
         GOTO1 FLDVAL                                                           
         MVC   NERRCD,=Y(NORSNTXT)                                              
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         MVC   NERRCD,=Y(RCTXTERR)                                              
**NOP**  CHI   R5,L'SVRSNTXT                                                    
         CHI   R5,40                                                            
         BH    BUYERR                                                           
         LHI   RE,SVRSNTXT-BUYSAVE                                              
         AR    RE,RA                                                            
         XC    0(L'SVRSNTXT,RE),0(RE)   CLEAR PREVIOUS                          
         BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   0(0,RE),0(R4)                                                    
* UPDATE ELEMENT LENGTH                                                         
         LHI   RE,SVRSNEL-BUYSAVE                                               
         AR    RE,RA                                                            
         LA    R0,10(R5)           8+1+L'TEXT+1                                 
         STC   R0,1(RE)            SET LEN INCLUDING TEXT                       
*                                                                               
VALRSNX  MVC   BUYMSG(15),=C'REASON CODE SET'                                   
*                                                                               
         LA    R2,BUYINF                                                        
         LHI   RE,SVRSNEL-BUYSAVE                                               
         AR    RE,RA                                                            
         MVC   1(3,R2),=C'RC='                                                  
         MVC   4(6,R2),2(RE)                                                    
*                                                                               
         CLI   9(RE),C' '          TEST THERE IS TEXT                           
         BNH   EQXIT                                                            
*                                                                               
         LA    R2,10(R2)           FIND LAST CHAR OF REASON CODE                
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         MVI   1(R2),C','                                                       
         MVC   2(9,R2),9(RE)       MOVE TEXT                                    
         B     EQXIT                                                            
         EJECT                                                                  
*========================================================*                      
* BUILD TRAFFIC MASTER CLIENT CODE ELEMENT               *                      
*========================================================*                      
         SPACE 1                                                                
COMTRF   DS    0H                                                               
         MVI   ELEM,X'61'                                                       
         MVI   ELEM+1,6                                                         
         MVC   ELEM+2(2),SVMCLCOD                                               
         MVC   ELEM+4(1),SVMCLUNQ                                               
         MVC   ELEM+5(1),SVMCLPRD                                               
         MVI   ERRCD,INVERR                                                     
         OC    ELEM+2(4),ELEM+2    MAKE SURE THEY SHOULD DO THIS                
         BZ    BUYERR                                                           
         MVI   ELCDLO,X'61'                                                     
         MVI   ELCDHI,X'61'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   COMTRF2                                                          
         BAS   RE,DELEL                                                         
*                                                                               
COMTRF2  BAS   RE,ADDEL                                                         
         BAS   RE,CHGPUT                                                        
* NEED TO ADD MISSING DIRECTORY POINTER AS WELL                                 
         MVC   KEY+1(2),SVMCLCOD                                                
         MVC   KEY+10(1),SVMCLUNQ    SET UNIQUE SEQNUM                          
         MVC   ELEM(18),KEY          SAVE THE KEY                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),ELEM          TEST KEY ON FILE                           
         BE    COMTRF4               IF YES, CAN SKIP ADDING IT                 
         MVC   KEY(18),ELEM           (OBVIOUSLY)                               
         GOTO1 ADD                                                              
*                                                                               
COMTRF4  L     RE,=A(TRFMSG)                                                    
         A     RE,RELO                                                          
         MVC   BUYMSG(L'TRFMSG),0(RE)                                           
         MVI   BUYERR,X'FF'        TELL CONTROLLER WE DID MSG                   
         B     EQXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE FOR GST CHANGES                                                   
*                                                                               
COMGST   DS    0H                                                               
         BAS   RE,TESTBP                                                        
         BNE   BUYERR                                                           
         BAS   RE,SETGST                                                        
*                                                                               
* SAVE REC IN REC 3                                                             
*                                                                               
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
         CLC   BUYKEY+4(2),=X'0000'   IS THIS A NETWORK BUY                     
         BNE   CGX                                                              
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         LR    R7,R6                                                            
*                                                                               
CG10     MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         LR    R6,R7               SAVE A(EXPLODED BUY)                         
         BAS   RE,NEXTEL                                                        
         BNE   CGX                                                              
         LR    R7,R6                                                            
*                                                                               
         BAS   RE,GETEXP           READ EXPLODED KEY/REC                        
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         BAS   RE,SETGST                                                        
         B     CG10                                                             
*                                                                               
CGX      MVC   DUB(4),AREC3        FROM                                         
         MVC   DUB+4(4),AREC1      TO                                           
         GOTO1 MOVEREC                                                          
         B     EQXIT                                                            
         SPACE 2                                                                
*                                                                               
SETGST   NTR1                                                                   
         MVI   ELCDLO,X'6A'                                                     
         MVI   ELCDHI,X'6A'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   *+8                                                              
         BAS   RE,DELEL                                                         
         MVC   ELEM(2),=X'6A03'                                                 
         MVC   ELEM+2(1),ELEM+7                                                 
         GOTO1 VRECUP,DMCB,AREC,ELEM,(R6)                                       
         B     COMIX10                                                          
         EJECT                                                                  
* SUBR TO FIX X'24' ELEMENT                                                     
*                                                                               
COMFXLK  DS   0H                                                                
         MVI   ELCDLO,X'24'        DELETE EXISTING ELEMENT FIRST                
         MVI   ELCDHI,X'24'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BAS   RE,DELEL                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'24'                                                       
         MVI   ELEM+1,6                                                         
         MVC   ELEM+2(1),SVEBKTYP                                               
         CLI   SVEBKTYP,C'A'       TEST EST BOOKTYPE                            
         BNL   COMFXLK2            ALWAYS WINS                                  
         MVC   ELEM+2(1),SVBKTYPE                                               
         CLI   SVBKTYPE,C'A'                                                    
         BNL   COMFXLK2                                                         
         MVI   ELEM+2,0                                                         
*                                                                               
COMFXLK2 CLI   BUYMD,C'R'                                                       
         BNE   COMFXLK4                                                         
*                                                                               
         MVC   ELEM+3(3),SVMKTMKT                                               
         CLI   SVSTAMKT,C' '                                                    
         BNH   *+10                                                             
         MVC   ELEM+3(3),SVMKTMKT                                               
         CLI   SVSTAMKT,C' '                                                    
         BNH   *+10                                                             
         MVC   ELEM+3(3),SVSTAMKT                                               
         B     COMFXLKX                                                         
*                                                                               
COMFXLK4 CLI   BUYMD,C'T'                                                       
         BNE   EQXIT                                                            
         MVI   ELEM+1,11           CORRECT ELEMENT LEN                          
         MVC   ELEM+3(3),SVMKTMKT  SET MARKET                                   
         MVC   ELEM+6(4),SVDMLST0  AND OVERRIDE STATION (IF ANY)                
         TM    SVDMLFLG,X'01'                                                   
         BZ    *+10                                                             
         MVC   ELEM+6(4),SVDMLST1                                               
         MVC   ELEM+10(1),SVDMLFLG  AND FLAGS                                   
         CLC   ELEM+2(9),=CL9' '                                                
         BNH   EQXIT                                                            
*                                                                               
COMFXLKX BAS   RE,ADDEL                                                         
*                                                                               
         BAS   RE,CHGPUT                                                        
         MVI   RCLOPT,RCLDEM                                                    
         B     EQXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE FOR TURNING OFF DAILY BIT                                         
*                                                                               
COMDAILY DS    0H                                                               
         BAS   RE,SETDAILY                                                      
*                                                                               
* SAVE REC IN REC 3                                                             
*                                                                               
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
         CLC   BUYKEY+4(2),=X'0000'   IS THIS A NETWORK BUY                     
         BNE   CDX                                                              
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         LR    R7,R6                                                            
*                                                                               
CD10     MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         LR    R6,R7               SAVE A(EXPLODED BUY)                         
         BAS   RE,NEXTEL                                                        
         BNE   CDX                                                              
         LR    R7,R6                                                            
*                                                                               
         BAS   RE,GETEXP           READ EXPLODED KEY/REC                        
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         BAS   RE,SETDAILY                                                      
         B     CD10                                                             
*                                                                               
CDX      MVC   DUB(4),AREC3        FROM                                         
         MVC   DUB+4(4),AREC1      TO                                           
         GOTO1 MOVEREC                                                          
         B     EQXIT                                                            
         SPACE 2                                                                
*                                                                               
SETDAILY NTR1                                                                   
         NI    BDSTAT2,X'FF'-X'80'      TURN OFF DAILY BIT                      
         B     COMIX10                                                          
         EJECT                                                                  
*                                                                               
* TEST SPOTS BILLED OR PAID                                                     
*                                                                               
TESTBP   NTR1                                                                   
         MVI   ERRCD,BLLDPAID                                                   
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'0B'                                                     
*                                                                               
TBP1     BAS   RE,NEXTEL                                                        
         BNE   EQXIT                                                            
         OC    4(2,R6),4(R6)                                                    
         BNZ   NEQXIT                                                           
         B     TBP1                                                             
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE FOR ORBIT CHANGES                                                 
*                                                                               
ORB      NTR1                                                                   
         MVI   ERRCD,MATCHED                                                    
         LA    R6,BDELEM                                                        
ORB2     CLI   0(R6),X'10'                                                      
         BE    BUYERR                                                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   ORB2                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'6704'                                                 
         LA    R6,ELEM+4                                                        
         USING ORBDAY,R6                                                        
*                                                                               
         LA    R2,BUYOUTH                                                       
         BAS   RE,FNDUF                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UPORBNDX,0                                                       
*                                                                               
ORB4     ZIC   R1,UPORBNDX         AUGMENT ORBIT INDEX FOR UPLOAD               
         LA    R1,1(R1)                                                         
         STC   R1,UPORBNDX                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
*                                                                               
         CLI   5(R2),0                                                          
         BE    ORB12                                                            
         CLC   =C'DELETE',0(R4)                                                 
         BE    ORB10                                                            
         MVI   UPNDX,SORBPOSQ                                                   
         MVI   EDTVAL,DAYEDT                                                    
         GOTO1 CALLEDT                                                          
         MVI   UPNDX,SORBPSTQ                                                   
         MVI   EDTVAL,TIMEDT                                                    
         GOTO1 CALLEDT                                                          
* EDIT DESC                                                                     
         MVI   UPNDX,SORBPPRQ                                                   
         MVC   BUPROG,SPACES                                                    
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BNZ   ORB6                                                             
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         B     ORB8                                                             
ORB6     MVI   ERRCD,PGMERR                                                     
         CLI   FLEN+1,7                                                         
         BH    BUYERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BUPROG(0),0(R4) *EXECUTED*                                       
*                                                                               
ORB8     LA    R1,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R1,SVBRDEMS                                                      
         MVC   SPDEMTYP,1(R1)                                                   
         MVI   SPDEMTYP,0  <==== REMOVE TO MAKE 2-DEC RATINGS WORK              
         MVI   EDTVAL,DEMEDT                                                    
         GOTO1 CALLEDT                                                          
*                                                                               
         MVC   ORBDAY,BUDAYS                                                    
         MVC   ORBTIME,BUTIME                                                   
         MVC   ORBDESC,BUPROG                                                   
         MVC   ORBDEM,BUNEWDEM+2                                                
         TM    BUNEWDEM,X'40'      TEST 2-DEC                                   
         BZ    *+8                                                              
         OI    ORBDEM,X'40'                                                     
*                                                                               
ORB10    LA    R6,16(R6)                                                        
         DROP  R6                                                               
*                                                                               
         ZIC   RE,ELEM+1                                                        
         LA    RE,16(RE)                                                        
         STC   RE,ELEM+1                                                        
         CLI   ELEM+1,148          (9*16)+4=148                                 
         BNL   ORB12                                                            
         BAS   RE,FNDUF                                                         
         B     ORB4                                                             
*                                                                               
ORB12    MVI   ELCDLO,X'67'                                                     
         MVI   ELCDHI,X'67'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   ORB14                                                            
*                                                                               
         MVI   ERRCD,ORBDROP                                                    
         CLC   ELEM+1(1),1(R6)     MUST HAVE AT LEAST AS MANY ENTRIES           
         BL    BUYERR                                                           
* GET RID OF DELETES                                                            
ORB14    LA    RE,ELEM+4                                                        
         LA    R0,16                                                            
ORB16    CLI   0(RE),0                                                          
         BE    ORB18                                                            
         LA    RE,16(RE)                                                        
         BCT   R0,ORB16                                                         
         B     ORB25                                                            
ORB18    LR    RF,RE                                                            
         B     ORB22                                                            
*                                                                               
ORB20    CLI   0(RF),0                                                          
         BE    ORB22                                                            
         MVC   0(16,RE),0(RF)                                                   
         LA    RE,16(RE)                                                        
ORB22    LA    RF,16(RF)                                                        
         BCT   R0,ORB20                                                         
*                                                                               
         LA    R0,ELEM                                                          
         SR    RE,R0               CALCULATE ELEM LEN                           
         STC   RE,ELEM+1                                                        
*                                                                               
         CLI   0(R6),X'67'                                                      
         BNE   ORB25                                                            
         BAS   RE,DELEL                                                         
*                                                                               
ORB25    CLI   ELEM+1,4            TEST NO ORBITS LEFT                          
         BNH   ORB28                                                            
         BAS   RE,ADDEL                                                         
         GOTO1 DEMLKUP                                                          
         EJECT                                                                  
* CALC OVERRIDE TO DEMO 1 IF ESTIMATES INPUT                                    
* BUT FIRST NEED TO FIND UPGRADE ELEMENT AGAIN                                  
         MVI   ELCDLO,X'67'                                                     
         MVI   ELCDHI,X'67'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R0,1(R6)            GET ORBIT ELEM LEN                           
         SRL   R0,4                GET NUMBER OF POSITIONS                      
         ST    R0,FULL             SAVE IT                                      
         SR    R1,R1               CLEAR ACCUM                                  
         LA    R6,4(R6)                                                         
         USING ORBDAY,R6                                                        
*                                                                               
         MVI   BYTE,C'1'           ASSUME 1-DECIMAL VALUES                      
ORB26    ICM   RE,3,ORBDEM                                                      
         TM    ORBDEM,X'40'        TEST 2-DEC FLAG                              
         BZ    *+8                                                              
         MVI   BYTE,C'2'           SET 2-DECIMAL VALUES                         
         N     RE,=X'00003FFF'     DROP 2-DEC FLAG                              
         AR    R1,RE               SUM DEMO VALUES                              
         LA    R6,16(R6)                                                        
         BCT   R0,ORB26                                                         
         DROP  R6                                                               
*                                                                               
         M     R0,=F'2'            X 2                                          
         D     R0,FULL                                                          
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LTR   R1,R1                                                            
         BZ    ORB28                                                            
         ST    R1,FULL                                                          
* FIND DEMO ELEMENT                                                             
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING NDELEM,R6                                                        
ORB27    MVC   NDEMRAW,FULL                                                     
         OI    NDEMRAW,X'80'                                                    
         MVI   NDSVI,100                                                        
         CLI   BYTE,C'2'           TEST 2-DEC VALUES                            
         BNE   *+8                                                              
         OI    NDEMRAW,X'40'       SET 2-DECIMAL FLAG                           
         DROP  R6                                                               
*                                                                               
ORB28    MVI   BUWHY2,X'02'                                                     
         BAS   RE,CHGPUT                                                        
         MVI   RCLOPT,RCLORB                                                    
         B     EQXIT                                                            
         EJECT                                                                  
* THESE CHANGES ARE NON-STRUCTURAL                                              
*                                                                               
DPT      NTR1                                                                   
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  IGNORE DARE LOCKS             
*                                                                               
         MVI   BUWHY2,X'40'        SET DAYPART CHANGE                           
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   DPT4                                                             
         MVC   BDDAYPT,BUDPT       MOVE DPT FOR TESTGLS                         
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
DPT2     BAS   RE,NEXTEL                                                        
         BNE   DPT4                                                             
         GOTO1 TESTGLS,DMCB,(R6)                                                
         B     DPT2                                                             
DPT4     DS    0H                                                               
         LA    R4,BDDAYPT                                                       
         LA    R5,BUDPT                                                         
         LA    R6,L'BDDAYPT-1                                                   
         OC    SVNDEF(16),SVNDEF                                                
         BZ    CHCOM1                                                           
         CLI   BDPROGRM,C'='         CHANGE DPT FOR SIMULCAST ONLY              
         B     CHCOM1    <=========  MH 4/17/95 ALLOW FOR ALL NETWORK           
         BE    CHCOM1                                                           
         MVI   ERRCD,BADKEYWD                                                   
         B     CHCOMR                                                           
*                                                                               
ADJ      NTR1                                                                   
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
         LA    R4,BDPROGT                                                       
         LA    R5,BUADJ                                                         
         LA    R6,L'BDPROGT-1                                                   
         B     CHCOM                                                            
*                                                                               
CONF     NTR1                                                                   
         LA    R4,BDCFD                                                         
         LA    R5,BUCFD                                                         
         LA    R6,L'BDCFD-1                                                     
         B     CHCOM                                                            
*                                                                               
SPEC     NTR1                                                                   
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
         LA    R4,=C'-S'                                                        
         BAS   RE,SETSPCL                                                       
         B     CHCOM2                                                           
*                                                                               
REMSPEC  NTR1                                                                   
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
         LA    R4,=C'NS'                                                        
         BAS   RE,SETNSPCL                                                      
         B     CHCOM2                                                           
*                                                                               
CF94     NTR1                                                                   
         MVI   BUWHY,X'40'                                                      
         CLC   =C'F94=A',0(R4)                                                  
         BNE   *+12                                                             
         OI    BDSTAT2,X'04'                                                    
         B     CHCOM2                                                           
         CLC   =C'F94=N',0(R4)                                                  
         BNE   *+12                                                             
         NI    BDSTAT2,X'FF'-X'04'                                              
         B     CHCOM2                                                           
* INPUT NOT VALID                                                               
         MVI   ERRCD,INVF94                                                     
         B     BUYERR                                                           
* CHANGE ID REFERENCE NUMBER FOR MILLER                                         
*                                                                               
CIDR     NTR1                                                                   
         CLI   SVCXTRA+2,C'A'      MAKE SURE USING MGRP SCHEME                  
         BL    CIDRERR1                                                         
         CLI   SVCXTRA+2,C'Z'                                                   
         BH    CIDRERR1                                                         
         MVI   BUWHY,X'40'                                                      
* CHANGE ID ELEMENT LEAVING MKTGRP REF INTACT                                   
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   CIDRERR2                                                         
         CLI   1(R6),15                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 FLDVAL                                                           
         CLI   FLEN+1,4                                                         
         BNH   CIDRERR3                                                         
         CLI   FLEN+1,6                                                         
         BH    CIDRERR3                                                         
         MVC   BUORB,SPACES                                                     
         BCTR  R5,0                                                             
         EX    R5,MVCIDR                                                        
         MVI   8(R6),C'-'                                                       
         MVC   9(6,R6),BUORB     MOVE BEYOND MKTGRP AND EL OVHD                 
         B     CHCOM2                                                           
*                                                                               
CIDRERR1 LA    R0,NOMGRID                                                       
         B     CIDRERRX                                                         
CIDRERR2 LA    R0,BADMGRID                                                      
         B     CIDRERRX                                                         
CIDRERR3 LA    R0,BADIDR                                                        
*                                                                               
CIDRERRX STCM  R0,3,NERRCD                                                      
         MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
*                                                                               
MVCIDR   MVC   BUORB(0),0(R4)                                                   
         SPACE 2                                                                
PROG     NTR1                                                                   
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  IGNORE DARE LOCKS             
         LA    R4,BDPROGRM                                                      
         LA    R5,BUPROG                                                        
         LA    R6,L'BDPROGRM-1                                                  
*                                                                               
         OC    SVNDEF(16),SVNDEF   TEST CANAD NET                               
         BZ    CHCOM                                                            
* NEED TO CHECK DID NOT CHANGE PROGRAM TYPE                                     
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(CHGPRGE1)                                            
         CLI   0(R4),C'='          TEST LIVE TELECAST BEFORE                    
         BNE   PROG2                                                            
         CLI   0(R5),C'='          THEN HAS TO BE NOW TOO                       
         BNE   BUYERR                                                           
         B     CHCOM                                                            
*                                                                               
PROG2    MVC   NERRCD,=AL2(CHGPRGE2)                                            
         CLI   0(R5),C'='          NOT LIVE BEFORE - CAN'T BE NOW               
         BE    BUYERR                                                           
         MVC   5(12,R4),BUPROG     MOVE PROGRAM BUT PRESERVE SHOW CODE          
         MVC   17(1,R4),BUPROG+17  MOVE -S OR NOT                               
         MVI   BUWHY,X'40'         SET BUY DESC CHANGE                          
         B     CHCOM2              THEN GO DO EXPLODED RECORDS                  
         EJECT                                                                  
* COMMON EXIT FOR NON-STRUCTURAL CHANGE ROUTINES                                
*                                                                               
CHCOM    MVI   BUWHY,X'40'         SET CHG = BUY DESC CHG                       
*                                                                               
CHCOM1   EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R5) *EXECUTED*                                         
CHCOM2   DS    0H                                                               
         BAS   RE,CHGPUT                                                        
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    *+8                                                              
         BAS   RE,CNSP                                                          
         B     CHCOMX                                                           
*                                                                               
CHCOMX   B     EQXIT                                                            
*                                                                               
CHCOMR   CLC   =C'CM',BUTRCODE                                                  
         BE    CMERR                                                            
         B     BUYERR                                                           
         EJECT                                                                  
*===================================================================*           
* SUB-ROUTINE CHANGES TO PROGRAMS AND SPECIALS ON CANADIAN NETWORK  *           
*===================================================================*           
         SPACE 1                                                                
CNSP     NTR1                                                                   
*                                                                               
* SAVE NTWK REC IN REC 3                                                        
*                                                                               
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
         LR    R7,R6               SAVE EX LEN                                  
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
*                                                                               
CNSP2    BAS   RE,NEXTEL                                                        
         BNE   CNSP8                                                            
         BAS   RE,GETEXP                                                        
         CLC   =C'-S',0(R4)                                                     
         BNE   *+12                                                             
         BAS   RE,SETSPCL                                                       
         B     CNSP6                                                            
*                                                                               
         CLC   =C'NS',0(R4)                                                     
         BNE   *+12                                                             
         BAS   RE,SETNSPCL                                                      
         B     CNSP6                                                            
*                                                                               
CNSP5    EX    R7,*+8              UPDATE DATA                                  
         B     *+10                                                             
         MVC   0(0,R4),0(R5) *EXECUTED*                                         
CNSP6    DS    0H                                                               
         BAS   RE,CHGPUT                                                        
         B     CNSP2                                                            
         SPACE 1                                                                
* RESTORE REC3 TO REC1 FOR DISPLAY *                                            
         SPACE 1                                                                
CNSP8    MVC   DUB(4),AREC3        FROM                                         
         MVC   DUB+4(4),AREC1      TO                                           
         GOTO1 MOVEREC                                                          
         B     EQXIT                                                            
         EJECT                                                                  
*                                                                               
*        GET EXPLODED BUY FROM BUY REC                                          
*        R6 - X'68' ELEMENT                                                     
*                                                                               
GETEXP   NTR1                                                                   
*                                                                               
         MVC   KEY,SVKEY           READ EXPLODED KEY/REC                        
         MVC   KEY+4(5),2(R6)      MKT/STA                                      
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVC   OLDBUYLN,BUYREC+13  SAVE BUYREC LENGTH                           
         B     EQXIT                                                            
         EJECT                                                                  
SETSPCL  NTR1                                                                   
         LA    R1,BDPROGRM+16                                                   
         LA    R0,17                                                            
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         BCTR  R1,0                BACK UP TO NEXT TO LAST CHAR                 
         CLC   =C'-S',0(R1)                                                     
         BE    SETSPCLX                                                         
         LA    R1,2(R1)                                                         
         LA    R0,BDPROGRM+15        NEED AT LEAST 2 SPACES                     
         CR    R1,R0                                                            
         BNH   *+6                                                              
         LR    R1,R0                                                            
         MVC   0(2,R1),=C'-S'                                                   
SETSPCLX MVI   BDPROGRM+17,0                                                    
         MVI   BUWHY,X'40'                                                      
         B     EXIT                                                             
         SPACE 2                                                                
SETNSPCL NTR1                                                                   
         LA    R1,BDPROGRM+16                                                   
         LA    R0,17                                                            
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         BCTR  R1,0                BACK UP TO NEXT-TO-LAST CHAR                 
         CLC   =C'-S',0(R1)                                                     
         BNE   *+10                                                             
         MVC   0(2,R1),SPACES                                                   
         MVI   BDPROGRM+17,C' '                                                 
         MVI   BUWHY,X'40'                                                      
         B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO LOOK UP DEMOS AND UPDATE FILE                                  
*                                                                               
LKUP     NTR1                                                                   
         CLI   DEMLKSW,0           TEST DEMO LOOK-UP REQ'D                      
         BE    LKUPX               NO                                           
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   LKUPX                                                            
         MVI   BUWHY,X'02'         SET DEMO CHANGE                              
         GOTO1 DEMLKUP                                                          
         BAS   RE,UPDREQ                                                        
LKUPX    B     EQXIT                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO UPDATE BUY AND BUILD REQUEST PRD LIST                          
*                                                                               
UPDREQ   NTR1                                                                   
         BAS   RE,CHGPUT                                                        
*                                                                               
         GOTO1 VBLDQLST            GENERATE REQUEST PRD LIST                    
         B     EQXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE FOR CANAD NTWK                                                    
*                                                                               
CNET     NTR1                                                                   
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC                                                          
* UPDATE NTWK BUY                                                               
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   2(2,R6),BUBOOK                                                   
         MVI   BUWHY,X'42'                                                      
*                                                                               
         OC    BUBOOK,BUBOOK       TEST DEM=NSP                                 
         BZ    CNET2                                                            
*                                                                               
         LHI   RE,SVD0PROF-BUYSAVE                                              
         AR    RE,RA                                                            
         CLI   13(RE),C'Y'         TEST LOOK UP NETWORK DEMOS                   
         JE    *+12                                                             
         CLI   13(RE),C'B'                                                      
         JNE   CNET2                                                            
*                                                                               
         BAS   RE,RESETOV          RESET DEMO OVERRIDES                         
*                                                                               
         MVI   BUDEMSW,0                                                        
         MVI   BUDLUSW,C'N'        SET TO NOT REBUILD DLU ELEM                  
         MVI   SVNDINDX,X'FF'      SET FLAG FOR DEMO LOOKUPS                    
         GOTO1 VBLDDEM             REBUILD DEMEL AND LOOKUP DATA                
         GOTO1 DEMLKUP                                                          
         GOTO1 SETCHGDT                                                         
*                                                                               
         BAS   RE,PUTIT                                                         
*==========================================                                     
* READ EXPLODED BUYS                                                            
*==========================================                                     
                                                                                
CNET2    L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         LR    R7,R6                                                            
*                                                                               
CNET4    LR    R6,R7               RESTORE EL POINTER                           
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   CNET20                                                           
         LR    R7,R6               SAVE EL POINTER                              
*                                                                               
         MVC   KEY,SVKEY                                                        
         MVC   KEY+4(5),2(R6)      MKT/STA                                      
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,RESETOV                                                       
*                                                                               
CNET10   LA    R1,SVNDEF           NEED TO GET INDX FOR BLDDEM                  
         USING SVNDEFD,R1                                                       
*                                  NEED TO GET INDX FOR BLDDEM                  
CNET12   CLC   SVNDMKST,KEY+4      MATCH MKT/STA                                
         BE    CNET14                                                           
         AHI   R1,L'SVNDEF                                                      
         OC    0(2,R1),0(R1)       TEST ANOTHER ENTRY                           
         BNZ   CNET12                                                           
         B     CNET16                                                           
*                                                                               
CNET14   MVC   SVNDINDX,SVNDSEQ                                                 
         MVI   BUDEMSW,0                                                        
         MVI   BUDLUSW,C'N'        SET TO NOT REBUILD DLU ELEM                  
         GOTO1 VBLDDEM             REBUILD DEMEL AND LOOKUP DATA                
         DROP  R1                                                               
*                                                                               
CNET16   GOTO1 DEMLKUP                                                          
         MVI   BUWHY,X'42'                                                      
         BAS   RE,CHGPUT                                                        
         B     CNET4                                                            
*                                                                               
CNET20   MVC   DUB(4),AREC3        FROM                                         
         MVC   DUB+4(4),AREC1      TO                                           
         GOTO1 MOVEREC                                                          
         B     CHA80                                                            
                                                                                
*===============================================================                
* SUBROUTINE TO RESET DEMO OVERRIDES                                            
*===============================================================                
                                                                                
RESETOV  NTR1                                                                   
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,2                                                         
         OC    BUBOOK,BUBOOK                                                    
         BNZ   *+8                                                              
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
*                                                                               
RESETOV2 BAS   RE,NEXTEL                                                        
         BNE   RESETOVX                                                         
         LLC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNP   RESETOVX                                                         
*                                                                               
         LA    RE,24(R6)                                                        
         SRL   R0,3                                                             
*                                                                               
RESETOV4 XC    4(4,RE),4(RE)                                                    
         OC    BUBOOK,BUBOOK                                                    
         JNZ   *+8                                                              
         OI    4(RE),X'80'                                                      
         LA    RE,8(RE)                                                         
         BCT   R0,RESETOV4                                                      
*                                                                               
RESETOVX XIT1                                                                   
         EJECT                                                                  
* CANADIAN NETWORK BUYLINE PERCENTAGES                                          
         SPACE 1                                                                
SPCT     DS    0H                                                               
         MVI   EDTVAL,SPCTEDT                                                   
         MVI   BUCHGOV,OVLY12                                                   
         GOTO1 CALLCHA                                                          
         MVI   RCLOPT,RCLSTA                                                    
         B     EQXIT                                                            
         SPACE 2                                                                
SDT      MVI   EDTVAL,SDTEDT                                                    
         MVI   BUCHGOV,OVLY12                                                   
         GOTO1 CALLCHA                                                          
         MVI   RCLOPT,RCLDT                                                     
         B     CHA80                                                            
         EJECT                                                                  
CHKMAXEL NTR1                                                                   
         MVI   ERRCD,MAXELEMS                                                   
         MVI   ELCDLO,X'0B'                                                     
         CLI   BUYKEY+3,X'FF'                                                   
         BE    *+8                                                              
         MVI   ELCDLO,X'06'                                                     
         MVC   ELCDHI,ELCDLO                                                    
*                                                                               
         LA    R6,BDELEM                                                        
         SR    R7,R7                                                            
         IC    R7,SVMAXSPT         GET MAX SPOTS                                
         AHI   R7,1                PLUS ONE                                     
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   EQXIT                                                            
         BCT   R7,*-8                                                           
         CLC   BUYREC+13(2),OLDBUYLN  TEST CURRENT LEN TO ORIGINAL              
         BNH   EQXIT                                                            
         B     BUYERR                                                           
         SPACE 2                                                                
ADDEL    NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         J     EXIT                                                             
*                                                                               
DELEL    NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         J     EXIT                                                             
*                                                                               
CHGPUT   NTR1                                                                   
         GOTO1 SETCHGDT                                                         
         BAS   RE,PUTIT            GO UPDATE FILE                               
         J     EQXIT                                                            
*                                                                               
PUTIT    LR    R0,RE                                                            
         CLC   =C'CM',BUTRCODE     TEST CHANGE MULTIPLE                         
         BNE   *+10                NO                                           
         CLI   CMPASS,1                                                         
         BER   RE                                                               
*                                                                               
PUTIT1   DS    0H                                                               
         TM    SVAFLAG1,X'22'      TEST CTA OR TRADE ACTIVE                     
         BZ    PUTIT2                                                           
         GOTO1 GOGETCTA,DMCB,('CIBCHGQ',AREC)                                   
*                                                                               
PUTIT2   DS    0H                                                               
         TM    UPSW,UPON+UPCHA     TEST UPLOADING                               
         BO    PUTIT3              YES-DON'T WRITE THE RECORD                   
         TM    WRKRUPSW,WRKRUPSW_NOIO OR WRKR UPLOAD                            
         BO    PUTIT3                                                           
         MVI   SVMGINIT,0          RESET MG TABLE REBUILD                       
         GOTO1 PUTREC                                                           
*                                                                               
PUTIT3   TM    BUWHY,X'70'         TEST TYPE OF CHANGE                          
         BZ    *+8                                                              
         OI    SVUPDATE,X'40'      SET BUY CHANGE FLAG                          
         TM    BUWHY3,X'80'        TEST SKED CHANGE                             
         BZ    *+8                                                              
         OI    SVUPDATE,X'40'      SET BUY CHANGE FLAG                          
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
FNDUF    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    FNDUFX                                                           
         TM    1(R2),X'20'                                                      
         BO    FNDUF                                                            
         CLI   0(R2),9                                                          
         BNH   FNDUF                                                            
         CR    RB,RB               EXIT WITH CC =                               
         BR    RE                                                               
FNDUFX   LTR   RB,RB               EXIT WITH CC NOT =                           
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT LINE NUMBERS FOR CHANGE MULTIPLE                          
* AT ENTRY, HAVE JUST POSITIONED PAST 'CM,'                                     
*                                                                               
EDLIN    NTR1                                                                   
         MVI   ERRCD,LINERR                                                     
*                                                                               
EDLIN1   XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(3),=C'/,-'                                                
*                                                                               
EDLIN2   GOTO1 FLDVAL                                                           
         CLI   FSTOP,C','          TEST FOR COMMA                               
         BE    EDLIN4              YES                                          
         CLI   FSTOP,C'-'          TEST FOR DASH                                
         BE    EDLIN6              YES-EDIT FOR END LINE                        
         CLI   FSTOP,C'/'          TEST FOR SLASH                               
         BE    EDLIN4              YES                                          
         MVI   ERRCD,INVERR        NO-MISSING KEYWORD TO END LIST               
         B     BUYERR                                                           
*                                                                               
EDLIN4   BAS   RE,VALNUM                                                        
         STH   R0,STARTLIN                                                      
         STH   R0,ENDLIN                                                        
         BAS   RE,ADDNTRY                                                       
         B     EDLIN8              NO-CONTINUE EDIT                             
*                                                                               
EDLIN6   BAS   RE,VALNUM                                                        
         STH   R0,STARTLIN                                                      
         XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(2),=C'/,'                                                 
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,X'FF'                                                      
         BE    BUYERR                                                           
         BAS   RE,VALNUM                                                        
         STH   R0,ENDLIN                                                        
         CLC   STARTLIN,ENDLIN                                                  
         BNL   BUYERR                                                           
         BAS   RE,ADDNTRY                                                       
*                                                                               
EDLIN8   CLI   FSTOP,C','          TEXT IF COMMA STOPPED SCAN                   
         BE    EDLINX                                                           
         B     EDLIN1                                                           
*                                                                               
EDLINX   OC    NLINES,NLINES       TEST IF ANY LINES FOUND                      
         BNZ   EXIT                YES                                          
         MVI   ERRCD,INVERR                                                     
         B     BUYERR                                                           
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE A POTENTIAL LINE NUMBER                               
*                                                                               
* ON EXIT, R0 CONTAINS BINARY LINE NUMBER                                       
*                                                                               
VALNUM   LTR   R5,R5               TEST FOR NO DATA                             
         BZ    BUYERR                                                           
         CLI   FLEN+1,3            TEST 3 DIGITS OR LESS                        
         BH    BUYERR                                                           
         TM    FVAL,X'08'          TEST FOR NUMERIC INPUT                       
         BZ    BUYERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BUYERR                                                           
         CHI   R0,999                                                           
         BH    BUYERR                                                           
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO ADD ENTRIES TO LINE NUMBER LIST                                
*                                                                               
ADDNTRY  NTR1                                                                   
         LH    RE,ENDLIN                                                        
         LA    RE,1(RE)            GET END LINE NUMBER AND INCREMENT            
         LH    RF,STARTLIN                                                      
         SR    RE,RF               COMPUTE N'LINES TO ADD TO LIST               
         LH    R1,NLINES                                                        
         LR    R6,R1                                                            
         LA    R1,LINLIST(R1)      INDEX TO NEXT POSITION IN LIST               
         AR    R6,RE               UPDATE N'LINES IN LIST                       
         CHI   R6,L'LINLIST        TEST IF MAXIMUM BLOWN                        
         BNH   ADDNTRY1                                                         
         MVI   ERRAREA,X'FF'                                                    
         MVC   BUYMSG(30),=C'** TOO MANY LINES TO CHANGE **'                    
         B     BUYERR                                                           
*                                                                               
ADDNTRY1 STH   R6,NLINES           UPDATE N'LINES IN LIST                       
*                                                                               
ADDNTRY2 CLM   RF,3,LASTLIN        TEST IF LINES IN ORDER                       
         BL    BUYERR                                                           
         STH   RF,LASTLIN                                                       
         STH   RF,0(R1)            INSERT NEXT ENTRY IN LIST                    
         LA    R1,2(R1)            NEXT LIST POSITION                           
         LA    RF,1(RF)            NEXT LINE NUMBER                             
         BCT   RE,ADDNTRY2                                                      
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK THAT LINES HAVE SAME ORIGINAL DATA                       
* AT ENTRY, RF=A(DATA TO BE CHANGED), R1=L'DATA TO BE CHANGED                   
*                                                                               
CHKORIG  BCTR  R1,0                SET EXECUTE LENGTH                           
         OC    ORIGDATA,ORIGDATA                                                
         BNZ   CHKORIG2                                                         
         EX    R1,*+6                                                           
         BR    RE                                                               
         MVC   ORIGDATA(0),0(RF)                                                
*                                                                               
CHKORIG2 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ORIGDATA(0),0(RF)                                                
         BER   RE                                                               
         B     CMOERR                                                           
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
MULTMSG  DC    C'** MULTIPLE CHANGE MUST BE ONLY TRANSACTION **'                
         EJECT                                                                  
OVLY00   EQU   X'00'                                                            
OVLY12   EQU   X'12'                                                            
OVLY13   EQU   X'13'                                                            
*                                                                               
CHGTAB   DS    0XL(CHGTABL)                                                     
         DC    CL7'PER ',AL1(3),AL1(OVLY12),AL1(PEREDT),AL1(NBUY)               
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'NPW ',AL1(3),AL1(OVLY12),AL1(NPWEDT),AL1(NBUY)               
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'TIM ',AL1(3),AL1(OVLY12),AL1(TIMEDT),AL1(NBUY+EXP)           
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'DPT ',AL1(3),AL1(OVLY00),AL1(DPTEDT),AL1(NBUY+EXP)           
         DC    AL3(DPT),AL3(0)                                                  
*                                                                               
         DC    CL7'SLN ',AL1(3),AL1(OVLY12),AL1(SLNEDT),AL1(NBUY)               
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'LEN ',AL1(3),AL1(OVLY12),AL1(SLNEDT),AL1(NBUY)               
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'PRO ',AL1(3),AL1(OVLY00),AL1(PGMEDT),AL1(NBUY)               
         DC    AL3(PROG),AL3(0)                                                 
*                                                                               
         DC    CL7'PGM ',AL1(3),AL1(OVLY00),AL1(PGMEDT),AL1(NBUY)               
         DC    AL3(PROG),AL3(0)                                                 
*                                                                               
         DC    CL7'ADJ ',AL1(3),AL1(OVLY00),AL1(ADJEDT),AL1(NBUY)               
         DC    AL3(ADJ),AL3(0)                                                  
*                                                                               
         DC    CL7'COM ',AL1(3),AL1(OVLY00),AL1(COMEDT),AL1(NBUY+EXP)           
         DC    AL3(COMI),AL3(0)                                                 
*                                                                               
         DC    CL7'CFD ',AL1(3),AL1(OVLY00),AL1(CFMEDT),AL1(0)                  
         DC    AL3(CONF),AL3(0)                                                 
*                                                                               
         DC    CL7'TAX ',AL1(3),AL1(OVLY12),AL1(TAXEDT),AL1(NBUY)               
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'REP ',AL1(3),AL1(OVLY12),AL1(REPEDT),AL1(NBUY)               
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'COS',AL1(3),AL1(OVLY12),AL1(COSTEDT),AL1(NBUY)               
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'COST',AL1(4),AL1(OVLY12),AL1(COSTEDT),AL1(NBUY)              
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'COS2',AL1(4),AL1(OVLY12),AL1(COS2EDT),AL1(NBUY)              
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'BOOK',AL1(3),AL1(OVLY00),AL1(BOOKEDT),AL1(NBUY+EXP)          
         DC    AL3(BK),AL3(0)                                                   
*                                                                               
         DC    CL7'BK  ',AL1(2),AL1(OVLY00),AL1(BOOKEDT),AL1(NBUY+EXP)          
         DC    AL3(BK),AL3(0)                                                   
* NOTE THERE IS NO EDIT BUT VALUE GOES NEXT DOOR                                
         DC    CL7'BT  ',AL1(2),AL1(OVLY00),AL1(255),AL1(NOEDT)                 
         DC    AL3(BT),AL3(0)                                                   
*                                                                               
         DC    CL7'PKG ',AL1(3),AL1(OVLY13),AL1(REFEDT),AL1(NBUY)               
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'REV ',AL1(3),AL1(OVLY13),AL1(REFEDT),AL1(NBUY)               
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'MG  ',AL1(2),AL1(OVLY13),AL1(REFEDT),AL1(NBUY)               
         DC    AL3(0),AL3(0)                                                    
* MGX FOR USERS THAT HAVE SECRET CODE FOR -OTO OF PAID SPOTS                    
         DC    CL7'MGX ',AL1(2),AL1(OVLY13),AL1(REFEDT),AL1(NBUY)               
         DC    AL3(0),AL3(0)                                                    
* OR MGZ                                                                        
         DC    CL7'MGZ ',AL1(2),AL1(OVLY13),AL1(REFEDT),AL1(NBUY)               
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'UPT ',AL1(3),AL1(OVLY00),AL1(UPEDT),AL1(0)                   
         DC    AL3(BUP),AL3(0)                                                  
*                                                                               
         DC    CL7'UPP ',AL1(3),AL1(OVLY00),AL1(UPEDT),AL1(0)                   
         DC    AL3(BUP),AL3(0)                                                  
*                                                                               
         DC    CL7'LOOK',AL1(3),AL1(OVLY13),AL1(LKUPEDT)                        
         DC    AL1(NOEDT+NBUY),AL3(0),AL3(0)                                    
*                                                                               
         DC    CL7'LKUP',AL1(3),AL1(OVLY13),AL1(LKUPEDT)                        
         DC    AL1(NOEDT+NBUY),AL3(0),AL3(0)                                    
*                                                                               
         DC    CL7'CTYPE',AL1(3),AL1(OVLY12),AL1(CTYPEDT),AL1(CANAD)            
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'STYPE',AL1(3),AL1(OVLY12),AL1(STYPEDT),AL1(CANAD)            
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'EXCH',AL1(2),AL1(OVLY12),AL1(XRTEDT),AL1(CANAD)              
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'XCH ',AL1(2),AL1(OVLY12),AL1(XRTEDT),AL1(CANAD)              
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'C58',AL1(3),AL1(OVLY12),AL1(C58EDT),AL1(CANAD)               
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'UPID',AL1(4),AL1(OVLY12),AL1(UPIDEDT),AL1(0)                 
         DC    AL3(0),AL3(0)                                                    
*                                                                               
         DC    CL7'F94',AL1(3),AL1(OVLY00),AL1(255),AL1(NOEDT)                  
         DC    AL3(CF94),AL3(0)                                                 
* ID REFERENCE FOR MILLER                                                       
         DC    CL7'IDR',AL1(3),AL1(OVLY00),AL1(255),AL1(NOEDT)                  
         DC    AL3(CIDR),AL3(0)                                                 
*                                                                               
CHGTABX  EQU   *-1                                                              
         EJECT                                                                  
* CHANGE MULTIPLE KEYWORD TABLE                                                 
*                                                                               
CMTAB    DS    0XL(CHGTABL)                                                     
         DC    CL7'PER ',AL1(3),AL1(OVLY12),AL1(PEREDT),AL1(NBUY)               
         DC    AL3(0),AL3(CHKPER)                                               
*                                                                               
         DC    CL7'TIM ',AL1(3),AL1(OVLY12),AL1(TIMEDT),AL1(EXP)                
         DC    AL3(0),AL3(CHKTIM)                                               
*                                                                               
         DC    CL7'DPT ',AL1(3),AL1(OVLY00),AL1(DPTEDT),AL1(NBUY+EXP)           
         DC    AL3(DPT),AL3(CHKDPT)                                             
*                                                                               
         DC    CL7'SLN ',AL1(3),AL1(OVLY12),AL1(SLNEDT),AL1(NBUY)               
         DC    AL3(0),AL3(CHKSLN)                                               
*                                                                               
         DC    CL7'LEN ',AL1(3),AL1(OVLY12),AL1(SLNEDT),AL1(NBUY)               
         DC    AL3(0),AL3(CHKSLN)                                               
*                                                                               
         DC    CL7'PRO ',AL1(3),AL1(OVLY00),AL1(PGMEDT),AL1(NBUY)               
         DC    AL3(PROG),AL3(CHKPRO)                                            
*                                                                               
         DC    CL7'PGM ',AL1(3),AL1(OVLY00),AL1(PGMEDT),AL1(NBUY)               
         DC    AL3(PROG),AL3(CHKPRO)                                            
*                                                                               
         DC    CL7'ADJ ',AL1(3),AL1(OVLY00),AL1(ADJEDT),AL1(NBUY)               
         DC    AL3(ADJ),AL3(CHKADJ)                                             
*                                                                               
         DC    CL7'TAX ',AL1(3),AL1(OVLY12),AL1(TAXEDT),AL1(NBUY)               
         DC    AL3(0),AL3(CHKTAX)                                               
*                                                                               
         DC    CL7'COST',AL1(4),AL1(OVLY12),AL1(COSTEDT),AL1(NBUY)              
         DC    AL3(0),AL3(CHKCOS)                                               
*                                                                               
         DC    CL7'COS2',AL1(4),AL1(OVLY12),AL1(COS2EDT),AL1(NBUY)              
         DC    AL3(0),AL3(CHKCOS2)                                              
*                                                                               
         DC    CL7'BOOK',AL1(3),AL1(OVLY00),AL1(BOOKEDT),AL1(NBUY+EXP)          
         DC    AL3(BK),AL3(0)                                                   
*                                                                               
         DC    CL7'BK  ',AL1(2),AL1(OVLY00),AL1(BOOKEDT),AL1(NBUY+EXP)          
         DC    AL3(BK),AL3(0)                                                   
*                                                                               
         DC    CL7'BT  ',AL1(2),AL1(OVLY00),AL1(255),AL1(NOEDT)                 
         DC    AL3(BT),AL3(0)                                                   
*                                                                               
         DC    CL7'REP ',AL1(3),AL1(OVLY12),AL1(REPEDT),AL1(NBUY)               
         DC    AL3(0),AL3(CHKREP)                                               
*                                                                               
         DC    CL7'F94',AL1(3),AL1(OVLY00),AL1(255),AL1(NOEDT)                  
         DC    AL3(CF94),AL3(0)                                                 
*                                                                               
         DC   CL7'ID',AL1(2),AL1(OVLY00),AL1(255),AL1(NOEDT+CANAD+NBUY)         
         DC    AL3(ID),AL3(0)                                                   
*                                                                               
CMTABX   EQU   *-1                                                              
*                                                                               
SPECENT  DC    CL7'PROG=-S',AL1(7),AL1(OVLY00),AL1(PGMEDT)                      
         DC    AL1(NOEDT+NBUY+EXP),AL3(SPEC),AL3(CHKPRO)                        
*                                                                               
REMSPENT DC    CL7'PROG=NS',AL1(7),AL1(OVLY00),AL1(PGMEDT)                      
         DC    AL1(NOEDT+NBUY+EXP),AL3(REMSPEC),AL3(CHKPRO)                     
*                                                                               
TRFMSG   DC    C'*MISSING TRF DATA ADDED, REMEMBER A,JS'                        
         EJECT                                                                  
*==================================================================*            
* SUB-ROUTINE TO GET NEXT BUY LINE                                 *            
* AT ENTRY, R7=A(NEXT LINE NUMBER)                                 *            
* ON EXIT, CC=EQ IF BUY LINE FOUND, CC=NEQ IF NOT FOUND            *            
*==================================================================*            
         SPACE 1                                                                
GETNEXT  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(BUYKBUY-BUYKEY),SVKEY                                        
         MVC   KEY+11(2),0(R7)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   GNEQXIT                                                          
         MVC   SVKEY,KEY           MAKE SURE SVKEY PROPERLY SET                 
         MVC   AREC,AREC1                                                       
         TM    SVAFLAG1,X'22'      TEST CTA OR TRADE ACTIVE                     
         BZ    GETNEXT2                                                         
         GOTO1 GOGETCTA,DMCB,('CIBCPYQ',AREC)                                   
GETNEXT2 DS    0H                                                               
         GOTO1 GETREC                                                           
         CLI   SVCPROF+0,C'0'      TEST BRAND POOL                              
         BE    GEQXIT              NO                                           
         CLI   SVPOLPRD,0          TEST BRAND POOL BY BRAND                     
         BE    GEQXIT              NO                                           
         CLC   SVPOLPRD,BDMASPRD   TEST FOR MATCH ON BRAND                      
         BE    GEQXIT                                                           
         B     GNEQXIT                                                          
*                                                                               
GEQXIT   CR    RB,RB                                                            
         B     *+6                                                              
GNEQXIT  LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* PURPOSE CODE CHANGES                                                          
* - FOR REQUIRED PURPOSE CODES ONLY, BUYLINES THAT ARE PART OF                  
* MAKEGOOD CAN'T CHANGE PURPOSE CODES SINCE THEY HAVE TO MATCH                  
*==============================================================*                
         SPACE 1                                                                
PUR      NTR1  BASE=*,LABEL=*                                                   
         MVC   NERRCD,=Y(NOPUROPT)                                              
         CLIY  SVB0PROF+9,C'O'     TEST OPT'L PURPOSE CODES IN B0 PROF?         
         JE    *+14                                                             
         CLIY  SVB0PROF+9,C'Y'     TEST REQ'D PURPOSE CODES IN B0 PROF?         
         BNE   PURERR                                                           
*                                                                               
         XC    BUORB,BUORB         CLEAR SAVE AREA                              
         MVI   FSTOPS+1,0                                                       
         GOTO1 FLDVAL                                                           
         MVC   NERRCD,=Y(BADPURP)                                               
         LTR   R5,R5               REMOVING PURPOSE CODE?                       
         BNZ   PUR02               NO                                           
*                                                                               
         CLIY  SVB0PROF+9,C'O'     YES, ONLY FOR OPT'L PURPOSE CODES            
         BNE   PURERR                                                           
         MVI   BUORB+14,C'Y'       SET 'DELETE' FLAG                            
         B     PUR15                                                            
*                                                                               
PUR02    CHI   R5,6                MORE THAN 6 CHARACTER PURPOSE CODE?          
         BH    PURERR               SEND AN ERROR                               
         BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   BUORB(0),0(R4)      SAVE NEW PURPOSE CODE IN BUORB               
         OC    BUORB(12),SPACES    & CAPATALIZE CASE                            
*                                                                               
         CLI   FSTOP,C','                                                       
         BNE   PUR03                                                            
*                                                                               
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,BADIDERR                                                   
         CLC   =C'ALL',0(R4)                                                    
         BNE   PURERR                                                           
         MVI   BUORB+12,C'Y'       SET 'ALL' FLAG                               
         MVI   BUORB+13,C'Y'       SET FIRST TIME FLAG                          
*                                                                               
* NEED TO MAKE SURE PURPOSE CODE IS VALID                                       
*                                                                               
PUR03    MVC   NERRCD,=Y(PURCODNF)                                              
         GOTO1 RDPURP,BUORB        IS IT A VALID PURPOSE CODE?                  
         BNE   PURERR                                                           
*                                                                               
         CLI   BUORB+12,C'Y'       TEST CHANGE ALL                              
         BE    PUR15               YES - SKIP TESTS - ALL WILL CHANGE           
*                                                                               
         CLIY  SVB0PROF+9,C'O'     TEST OPT'L PURPOSE CODES IN B0 PROF?         
         BE    PUR15                YES                                         
*                                                                               
         MVC   NERRCD,=Y(MGPURPNE)                                              
         CLI   BDMGDATE,0          TEST MAKEGOOD LINE                           
         BNE   PURERR               YES, PURPOSE CODES MUST MATCH               
*                                                                               
PUR05    LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'        SCAN FOR MAKEGOOD SPOTS                      
         MVI   ELCDHI,X'0C'                                                     
         USING REGELEM,R6                                                       
PUR10    BRAS  RE,NEXTEL                                                        
         BNE   PUR15                                                            
         TM    RSTATUS,X'02'       TEST MAKEGOOD ON NEW LINE                    
         BO    PURERR               YES, PURPOSE CODES MUST MATCH               
         B     PUR10                                                            
         DROP  R6                                                               
*                                                                               
PUR15    MVI   BUPROG,X'FF'        SET FLAG FOR NO ID                           
         MVI   ELCDLO,X'70'        FIND/SAVE PURPOSE CODE IN BUPROG             
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   PUR20                                                            
*                                                                               
         GOTO1 RDPURP,3(R6)        IS IT A VALID PURPOSE CODE?                  
         BNE   PUR20                NO                                          
         MVC   BUPROG,3(R6)         YE - SAVE IT                                
*                                                                               
* ADD/CHANGE PURPOSE CODE                                                       
*                                                                               
PUR20    MVI   ELCDLO,X'70'        DELETE OLD PURPOSE CODE, IF ANY              
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
*                                                                               
         CLI   BUORB+14,C'Y'       TEST 'DELETE' PURPOSE CODE?                  
         BE    PUR21                YES, DON'T ADD PURPCODE ELEM                
*                                                                               
         XC    ELEM,ELEM           ADD NEW PURPOSE CODE ELEM                    
         MVI   ELEM,X'70'                                                       
         MVI   ELEM+1,15           MUST BE SAME LEN AS ID ELEM                  
         MVC   ELEM+3(12),BUORB                                                 
         BRAS  RE,ADDEL                                                         
*                                                                               
PUR21    CLC   BDCHG,SVTODAYB      BUY LAST CHANGED TODAY?                      
         BE    PUR22                YES                                         
         MVI   BDWHY,0                                                          
         MVI   BDWHY2,0                                                         
         MVI   BDWHY3,0                                                         
*                                                                               
PUR22    MVC   BDCHG,SVTODAYB      SET CHANGE DATE                              
         OI    BDWHY2,X'20'        AND REASON                                   
         GOTO1 PUTREC                                                           
*                                                                               
         CLI   BUORB+12,C'Y'       TEST CHANGE 'ALL'                            
         JNE   EXIT                 NO, ALL DONE                                
*                                                                               
         BAS   RE,NXTPUR                                                        
         BE    PUR20                                                            
*                                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC              RESTORE ORIGINAL BUY                         
*                                                                               
         J     EXIT                                                             
*                                                                               
PURERR   MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
         SPACE 1                                                                
*==============================================================*                
* FIND ALL BUYLINES WITH THIS PURPOSE CODE                     *                
*==============================================================*                
         SPACE 1                                                                
NXTPUR   NTR1                                                                   
         CLI   BUORB+13,C'Y'       TEST FIRST TIME                              
         BNE   NXTPUR2                                                          
         MVI   BUORB+13,C'N'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(10),BUYKEY                                                   
         GOTO1 HIGH                                                             
         B     NXTPUR4                                                          
*                                                                               
NXTPUR2  DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
NXTPUR4  CLC   KEY(10),KEYSAVE     SAME A-M/C/P/M/S/E                           
         JNE   NEQXIT                                                           
         CLC   KEY(13),SVKEY       IS IT OUR LINE                               
         BE    NXTPUR2                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   NXTPUR6             NO ID THIS LINE                              
*                                                                               
         MVC   WORK(L'KEY),KEY     SAVE BUYKEY                                  
         GOTO1 RDPURP,3(R6)        IS IT A VALID PURPOSE CODE?                  
         BNE   NXTPUR5              NO                                          
         MVC   KEY(L'KEY),WORK                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   3(12,R6),BUPROG     ELSE TEST ID'S MATCH                         
         BNE   NXTPUR2                                                          
         J     EQXIT                                                            
*                                                                               
NXTPUR5  MVC   KEY(L'KEY),WORK     RESTORE BUYKEY                               
         GOTO1 HIGH                READ SEQUENCE                                
*                                                                               
NXTPUR6  CLI   BUPROG,X'FF'        NO ID THIS LINE, TEST ID IN ORIGINAL         
         BNE   NXTPUR2             IF NONE SAVED (X'FF'), CHANGE THIS           
         J     EQXIT                                                            
         SPACE 1                                                                
*==============================================================*                
* READ PURPOSE CODE RECORD AND RETURN CC OF RECORD FOUND                        
*==============================================================*                
         SPACE 1                                                                
K        USING PRPRECD,KEY         READ PURPOSE CODE RECORD                     
RDPURP   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   K.PRPKTYP,PRPKTYPQ  X'0D19'|AGYALPH|MEDIA|PURPCODE               
         MVI   K.PRPKSUB,PRPKSUBQ                                               
         MVC   K.PRPKAGY,AGYALPHA                                               
         MVC   K.PRPKMED,BUYMD                                                  
         MVC   K.PRPCODE,0(R1)                                                  
         DROP  K                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST & RETURN CC OF RECORD FOUND             
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* ID ELEMENT CHANGES                                                            
*==============================================================*                
         SPACE 1                                                                
ID       NTR1  BASE=*,LABEL=*                                                   
         LHI   RE,SVB0PROF-BUYSAVE POINT TO B0 PROFILE                          
         AR    RE,RA                                                            
         CLI   3(RE),C'N'          TEST DON'T CHANGE ID AFTER MATCH             
         BNE   ID1X                                                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(CANTDOID)                                              
         CLI   9(RE),C'Y'          TEST PURPOSE CODES REQD                      
         BE    IDERR                                                            
* CHECK FOR PAYMENTS/AFFIDS                                                     
         MVI   ERRCD,NEWERRS                                                    
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
ID1A     BRAS  RE,NEXTEL                                                        
         BNE   ID1X                                                             
         MVC   NERRCD,=Y(CHGIDPD)                                               
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BNZ   IDERR                                                            
         MVC   NERRCD,=Y(CHGIDMAT)                                              
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'10'         TEST MATCHED                                 
         BE    IDERR                                                            
         B     ID1A                                                             
*                                                                               
ID1X     XC    BUPROG,BUPROG                                                    
         MVI   ELCDLO,X'70'        FIND AND SAVE CURRENT ID IN BUPROG           
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BE    ID2                                                              
         MVI   BUPROG,X'FF'        NO - SET FLAG FOR NO ID                      
         B     ID2X                                                             
*                                                                               
ID2      MVC   BUPROG,3(R6)        NO - SAVE IT                                 
*                                                                               
ID2X     MVI   ERRCD,INVERR                                                     
         XC    MSTRBUY,MSTRBUY                                                  
         MVI   FSTOPS+1,0                                                       
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,BADIDERR                                                   
         LTR   R5,R5                                                            
         BZ    IDERR                                                            
         CLI   FLEN+1,12                                                        
         BH    IDERR                                                            
         MVC   BUORB,SPACES                                                     
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BUORB(0),0(R4)                                                   
*                                                                               
         CLI   FSTOP,C','                                                       
         BNE   ID5                                                              
         CLC   =C'CM',BUTRCODE     TEST CHANGE MULTIPLE                         
         BE    IDERR               YES-NO ALL/OTHER INPUT ALLOWED               
*                                                                               
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,BADIDERR                                                   
         CLC   =C'ALL',0(R4)                                                    
         BNE   IDERR                                                            
         MVI   BUORB+12,C'Y'                                                    
*                                                                               
ID5      TM    SVAFLAG1,X'22'      TEST TRADE AGENCY                            
         BZ    ID7                 NO                                           
         TM    BDSTAT2,X'20'       NON-TBS TRADE BUY                            
         BO    ID80                                                             
         TM    BDCIND2,X'02'       TBS TRADE BUY                                
         BO    ID80                                                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOCTAID)                                             
         B     IDERR                                                            
*                                                                               
ID7      OC    SVNDEF(16),SVNDEF                                                
         BZ    ID10                                                             
         BAS   RE,CHGIDCN                                                       
         CLI   BUORB+12,C'Y'       TEST CHANGE ALL BUYS THIS ID                 
         JNE   EQXIT                                                            
*                                                                               
ID9      XC    KEY,KEY             RESTORE DIR FOR SEQ READ                     
         L     RE,AREC3                                                         
         MVC   KEY(10),0(RE)       A-M/CLT/PRD/MKT/STA/EST                      
         MVC   KEY+11(2),10(RE)    LINE                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,NXTID                                                         
         JNE   EQXIT               NO MORE                                      
         BAS   RE,CHGIDCN                                                       
         B     ID9                                                              
*                                                                               
         EJECT                                                                  
*==========================================================*                    
* TEST FOR ID=MKTGRP SITUATIONS                            *                    
*==========================================================*                    
         SPACE 1                                                                
ID10     MVI   BYTE,C'N'           MKT GROUP IDS NOT USED                       
         CLI   SVCXTRA+2,C'N'                                                   
         BE    ID12                                                             
         CLI   SVCXTRA+2,C'Y'      Y JUST MEANS REQD                            
         BE    ID12                                                             
         CLI   SVCXTRA+2,C'A'                                                   
         BL    ID12                                                             
         CLI   SVCXTRA+2,C'Z'                                                   
         BH    ID12                                                             
*                                                                               
         MVI   BYTE,C'Y'           MKT GROUP IDS = YES                          
         CLC   =C'CM',BUTRCODE     TEST CHANGE MULTIPLE                         
         BE    IDERR                                                            
*                                                                               
         CLC   BUORB(1),SVCXTRA+2  LETTER MUST MATCH                            
         BNE   IDERR                                                            
         CLI   FLEN+1,5            TEST INPUT LEN 5                             
         BL    IDERR                                                            
* VALIDATE MKTGRP                                                               
         MVC   WORK(13),KEY        SAVE KEY                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),SVKEY      A-M                                          
         CLI   SVCXTRA+2,C'F'                                                   
         BH    *+10                                                             
         MVC   KEY+3(2),SVKEY+1    CLT SCHEMES NEED CLT                         
         MVC   KEY+8(1),SVCXTRA+2                                               
*                                                                               
         MVI   ERRCD,BADMGRP                                                    
         LA    R0,4                                                             
         LA    RE,BUORB+1                                                       
ID11     CLI   0(RE),C'0'          MUST BE GREATER THEN CHAR 0                  
         BNL   *+12                                                             
         CLI   0(RE),C' '          OR BE A SPACE                                
         BNE   IDERR                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,ID11                                                          
*                                                                               
         PACK  DUB,BUORB+1(5)                                                   
         MVC   KEY+9(2),DUB+5                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   IDERR                                                            
         MVC   KEY(13),WORK        RESTORE KEY                                  
         EJECT                                                                  
ID12     MVI   ERRCD,BADSLVID                                                   
         MVI   ELCDLO,X'05'                                                     
         MVI   ELCDHI,X'05'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   ID20                                                             
         TM    2(R6),X'01'         TEST SLAVE LINE                              
         BZ    IDERR                                                            
         CLC   =C'CM',BUTRCODE     TEST CHANGE MULTIPLE                         
         BE    IDERR               YES-SLAVE CHANGES NOT SUPPORTED              
         ZIC   RE,1(R6)                                                         
         AHI   RE,-4               SET FOR EX                                   
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MSTRBUY(0),3(R6)    SAVE PACKAGE LINES                           
*                                                                               
ID20     BAS   RE,CHGID                                                         
*                                                                               
         CLI   MSTRBUY,0           TEST PKG                                     
         BNE   ID30                YES                                          
         CLI   BUORB+12,C'Y'       TEST 'ALL'                                   
         BNE   ID40                                                             
*                                                                               
         XC    KEY+11(2),KEY+11    SET LINE NUMBER TO READ ALL BUYS             
*                                                                               
ID22     BAS   RE,NXTID            FIND NEXT BUYLINE                            
         BNE   ID40                                                             
         BAS   RE,CHGID                                                         
         B     ID22                                                             
         EJECT                                                                  
* PROCESS ALL LINES IN THIS PKG *                                               
                                                                                
ID30     LA    R7,MSTRBUY                                                       
*                                                                               
ID32     XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         LLC   R0,0(R7)                                                         
         TM    MSTRBUY+2,X'10'     TEST 2-BYTE LINE NUMS IN PKGEL               
         BZ    *+8                                                              
         ICM   R0,3,0(R7)                                                       
         STCM  R0,3,KEY+11                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
         BAS   RE,CHGID                                                         
*                                                                               
         TM    MSTRBUY+2,X'10'                                                  
         BO    ID34                                                             
         LA    R7,1(R7)                                                         
         CLI   0(R7),0                                                          
         BNE   ID32                                                             
         B     ID40                                                             
*                                                                               
ID34     LA    R7,2(R7)                                                         
         OC    0(2,R7),0(R7)                                                    
         BNZ   ID32                                                             
*                                                                               
ID40     J     EQXIT                                                            
*                                                                               
IDEXIT   XIT1  ,                                                                
*                                                                               
IDERR    GOTO1 ERROR                                                            
         EJECT                                                                  
*=================================================================*             
* FOR CTA CLIENTS, NEED TO RESTRICT ID CHANGES TO/FROM TRADE BUYS *             
* AND INSURE THAT CONTRACTS ARE ON FILE                           *             
*=================================================================*             
         SPACE 1                                                                
*                                                                               
ID80     DS    0H                                                               
         GOTO1 GOGETCTA,DMCB,(X'81',AREC)   PROC COPY/IGNORE  ERRORS            
* NOTE THAT CHGID WILL SUPPRESS PUTREC !                                        
         BAS   RE,CHGID                     CHANGE THE RECORD                   
*                                                                               
         GOTO1 GOGETCTA,DMCB,(X'02',AREC)   PROC CHANGE                         
* ONLY RETURNS HERE IF NO ERRORS, SO NOW DO PUTREC                              
         CLC   =C'CM',BUTRCODE     TEST CHANGE MULTIPLE                         
         BNE   *+12                                                             
         CLI   CMPASS,1            YES-ONLY DO I/O ON 2ND PASS                  
         JE    EQXIT                                                            
         GOTO1 PUTREC                                                           
         J     EQXIT                                                            
         EJECT                                                                  
*==============================================================*                
* FIND ALL BUYLINES WITH THIS ID (OR PURPOSE CODE)             *                
*==============================================================*                
         SPACE 1                                                                
NXTID    NTR1                                                                   
         OC    KEY+11(2),KEY+11    TEST FIRST TIME                              
         BNE   NXTID2                                                           
         GOTO1 HIGH                                                             
         B     NXTID4                                                           
*                                                                               
NXTID2   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
NXTID4   CLC   KEY(10),KEYSAVE     SAME A-M/C/P/M/S/E                           
         JNE   NEQXIT                                                           
         CLC   KEY(13),SVKEY       IS IT OUR LINE                               
         BE    NXTID2                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   NXTID6              NO ID THIS LINE                              
*                                                                               
         CLC   3(12,R6),BUPROG     ELSE TEST ID'S MATCH                         
         BNE   NXTID2                                                           
         J     EQXIT                                                            
*                                                                               
NXTID6   CLI   BUPROG,X'FF'        NO ID THIS LINE, TEST ID IN ORIGINAL         
         BNE   NXTID2              IF NONE SAVED (X'FF'), CHANGE THIS           
         J     EQXIT                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO HANDLE ID CHANGE ON BUY RECORD                                 
* IN SEP/03 THIS CODE CHANGED TO WORK FOR ALL GROUPS A-Z, EXCEPT N              
*                                                                               
CHGID    NTR1  ,                                                                
         MVC   WORK(13),KEY        SAVE INITIAL KEY                             
*                                                                               
         TM    SVAFLAG1,X'22'      TEST TRADE AGENCY                            
         BZ    CHGID1X             NO                                           
         TM    BDSTAT2,X'20'       NON-TBS TRADE BUY                            
         BO    *+12                                                             
         TM    BDCIND2,X'02'       TEST TRADE BUY                               
         BZ    CHGID1X             NO                                           
*                                                                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
CHGID1A  BRAS  RE,NEXTEL                                                        
         BNE   CHGID1X                                                          
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BZ    CHGID1A                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOCHGID)                                             
         B     IDERR                                                            
*                                                                               
CHGID1X  MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   CHGID4                                                           
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
*                                                                               
CHGID4   CLC   =C'DELETE',BUORB                                                 
         BE    CHGID4B                                                          
         MVC   ELEM(2),=X'700F'    BUILD NEW ID ELEMENT                         
         MVI   ELEM+2,0                                                         
         MVC   ELEM+3(12),BUORB                                                 
*                                                                               
         CLI   SVCXTRA+2,C'A'      TEST USING MGRP SCHEME                       
         BL    CHGID4X                                                          
         CLI   SVCXTRA+2,C'Z'                                                   
         BH    CHGID4X                                                          
         CLI   SVCXTRA+2,C'Y'                                                   
         BE    CHGID4X                                                          
         CLI   SVCXTRA+2,C'N'                                                   
         BE    CHGID4X                                                          
*                                                                               
         CLI   BUORB+5,C'-'        DID THEY NAME AN IDR IN NEW                  
         BE    CHGID4X             YES                                          
         CLI   BUPROG+5,C'-'       IS THERE AN IDR IN OLD                       
         BNE   CHGID4X             NO                                           
         MVC   ELEM+8(7),BUPROG+5  MOVE OLD IDR (-AE1234) TO NEW                
CHGID4X  DS    0H                                                               
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
CHGID4B  TM    SVAFLAG1,X'20'      TEST CTA AGENCY                              
         BZ    CHGID4C             NO                                           
         TM    BDCIND2,X'02'       TEST TRADE BUY                               
         BZ    CHGID4C             NO                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NODELID)                                             
         B     IDERR                                                            
*                                                                               
CHGID4C  CLC   =C'CM',BUTRCODE                                                  
         BNE   *+12                                                             
         CLI   CMPASS,1            ONLY DO I/O ON 2ND 'CM' PASS                 
         BE    CHGIDX                                                           
         GOTO1 PUTREC                                                           
         B     CHGIDX                                                           
*                                                                               
CHGID5   CLI   SVCXTRA+2,C'A'      OR MKTGRP SCHEME A-Z                         
         BL    CHGID5A                                                          
         CLI   SVCXTRA+2,C'N'                                                   
         BE    CHGID5A                                                          
         CLI   SVCXTRA+2,C'Y'                                                   
         BE    CHGID5A                                                          
         CLI   SVCXTRA+2,C'Z'                                                   
         BNH   CHGID50                                                          
*                                                                               
CHGID5A  CLC   BDCHG,SVTODAYB                                                   
         BE    CHGID5X                                                          
         MVI   BDWHY,0                                                          
         MVI   BDWHY2,0                                                         
         MVI   BDWHY3,0                                                         
*                                                                               
CHGID5X  MVC   BDCHG,SVTODAYB      SET CHANGE DATE                              
         OI    BDWHY2,X'20'        AND REASON                                   
         CLC   =C'CM',BUTRCODE     TEST CHANGE MULTIPLE                         
         BNE   *+12                                                             
         CLI   CMPASS,1            YES-ONLY DO I/O ON 2ND PASS                  
         BE    CHGIDX                                                           
         GOTO1 PUTREC              WRITE RECORD WITH NEW ID                     
         B     CHGIDX                                                           
         EJECT                                                                  
CHGID50  DS    0H                                                               
         MVC   WORK(20),KEY        SAVE ORIGINAL KEY                            
         CLI   SVCXTRA+2,C'*'      TEST CCUSA                                   
         BNE   CHGID50X                                                         
         BRAS  RE,GETACN             VALIDATE ACN NUMBER                        
*                                                                               
CHGID50X XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D44'     READ FOR A STATION EQUIV REC                 
         MVC   KEY+2(3),SVKEY      A-M/CLT                                      
         MVC   KEY+5(5),QSTA       STATION                                      
         CLI   QSTA,C'0'           TEST CABLE                                   
         BL    *+8                                                              
         MVI   KEY+9,C' '                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     CONTINUE IF FOUND                            
         BE    CHGID51                                                          
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE                                      
         MVC   KEY+3(2),=X'FFFF'   TRY FOR ALL CLIENT RECORD                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    CHGID51                                                          
*                                                                               
         CLC   =C'CM',BUTRCODE                                                  
         BNE   *+12                                                             
         CLI   CMPASS,1            ONLY DO I/O ON 2ND 'CM' PASS                 
         BE    CHGIDX                                                           
         GOTO1 PUTREC              ELSE WRITE RECORD                            
         B     CHGIDX                                                           
*                                                                               
CHGID51  L     R6,AREC2                                                         
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCDLO,2            LOOK FOR AN ELEMENT FOR THIS ACN             
         MVI   ELCDHI,3                                                         
         LA    R6,24(R6)                                                        
         BRAS  RE,NEXTEL2                                                       
         BE    CHGID53                                                          
         B     CHGID70                                                          
*                                                                               
CHGID52  BRAS  RE,NEXTEL                                                        
         BNE   CHGID70                                                          
CHGID53  CLI   SVCXTRA+2,C'*'      TEST CCUSA                                   
         BNE   CHGID54                                                          
         USING STEEL02,R6                                                       
         CLC   STEACN,BUORB        MATCH ACN NUMBER                             
         BNE   CHGID52                                                          
         MVC   HALF,STEACNMK       SET NEW MARKET NUM                           
         B     CHGID60                                                          
*                                                                               
         USING STEEL03,R6                                                       
CHGID54  CLC   STEMGID,SVCXTRA+2   SAME MKTGRP ID                               
         BNE   CHGID52                                                          
         PACK  DUB(3),BUORB+1(5)                                                
         CLC   STEMGRP,DUB         SAME MKTGRP                                  
         BNE   CHGID52                                                          
         MVC   HALF,STEMGMKT       SET NEW MKT                                  
*                                                                               
CHGID60  CLC   HALF,SVKEY+4        MAKE SURE MKT NOT ALREADY CORRECT            
         BE    CHGIDX                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY       A-M/C/P/MKT/STA/EST                          
         MVC   KEY+4(2),HALF       SET NEW MARKET                               
*                                                                               
         BRAS  RE,NXTBUYLN        SET NEW LINE NUM IN KEY AND REC               
         L     R6,AREC1                                                         
         ST    R6,AREC                                                          
         CLC   =C'CM',BUTRCODE     TEST CHANGE MULTIPLE                         
         BNE   *+12                                                             
         CLI   CMPASS,1            YES-ONLY DO I/O ON 2ND PASS                  
         BE    CHGID61                                                          
         GOTO1 ADDREC                                                           
*                                                                               
CHGID61  MVC   SVKEY,WORK          NXTBUYLN CHANGES SVKEY !                     
         MVC   KEY(13),WORK        RESTORE THE ORIGINAL KEY                     
         GOTO1 HIGH                REREAD THE KEY                               
         GOTO1 GETREC              REREAD THE RECORD                            
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   CHGID62                                                          
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
CHGID62  DS    0H                                                               
         GOTO1 (RF),(R1),BUYREC,ELEM,(R6)                                       
*                                                                               
         OI    KEY+13,X'80'        SET KEY DELETED                              
         OI    BUYREC+15,X'80'      AND SET REC DELETED TOO                     
         MVC   COMMAND,=CL8'DMWRT'                                              
         MVI   GBYACT,C'W'                                                      
         GOTO1 DIR                                                              
         CLC   =C'CM',BUTRCODE                                                  
         BNE   *+12                                                             
         CLI   CMPASS,1            ONLY DO I/O ON 2ND 'CM' PASS                 
         BE    CHGID65                                                          
         GOTO1 PUTREC                                                           
CHGID65  XC    SVKEY+14(4),SVKEY+14  RECORD NO LONGER AVAILABLE                 
         B     CHGIDX                                                           
         SPACE 1                                                                
* NO OVERRIDE ELEMENT FOUND *                                                   
         SPACE 1                                                                
CHGID70  CLC   BUORB(5),SVDFLTID   TEST SETTING BACK TO DEFAULT                 
         BNE   CHGID72                                                          
         MVC   HALF,SVDFLTMK                                                    
         B     CHGID60                                                          
CHGID72  MVI   ERRCD,BADOVRD       INDICATE NO MATCH ON MGR/ACN EQ              
         B     IDERR                                                            
*                                                                               
CHGIDX   CLC   KEY(13),WORK        TEST HAVE ORIGINAL KEY BACK                  
         BE    CHGIDXX                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),WORK        RESTORE ORIGINAL KEY                         
         GOTO1 HIGH                RESTORE DIRECTORY FOR SEQ                    
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
CHGIDXX  B     IDEXIT                                                           
         EJECT                                                                  
*==========================================================*                    
* SUB-ROUTINE FOR CHANGES TO ID FOR CANADIAN NETWORK       *                    
*==========================================================*                    
         SPACE 1                                                                
CHGIDCN  NTR1  ,                                                                
         MVC   ELEM(2),=X'700F'    BUILD NEW ELEMENT                            
         MVI   ELEM+2,0                                                         
         MVC   ELEM+3(12),BUORB                                                 
         BAS   RE,CNDOID                                                        
*                                                                               
* SAVE NTWK REC IN REC 3                                                        
*                                                                               
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
*                                                                               
CNID2    MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   CNID8                                                            
* READ EXPLODED KEY/REC                                                         
         L     RE,AREC3                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(10),0(RE)                                                    
         MVC   KEY+4(5),2(R6)      MKT/STA                                      
         MVC   KEY+11(2),10(RE)    LINE NUMBER                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         BAS   RE,CNDOID                                                        
         B     CNID2                                                            
         SPACE 1                                                                
* RESTORE REC3 TO REC1 FOR DISPLAY *                                            
         SPACE 1                                                                
CNID8    MVC   DUB(4),AREC3        FROM                                         
         MVC   DUB+4(4),AREC1      TO                                           
         GOTO1 MOVEREC                                                          
* RELEASE LOCKS FOR THIS NETWORK BUYLINE                                        
         CLC   =C'CM',BUTRCODE     TEST CHANGE MULTIPLE                         
         BNE   *+12                                                             
         CLI   CMPASS,1            YES-ONLY DO UNLOCK ON 2ND PASS               
         BE    CNID9                                                            
         GOTO1 VDATAMGR,DMCB,=C'DMUNLK',=C'SPTFILE'                             
CNID9    SR    RE,RE               SET CC                                       
         B     IDEXIT                                                           
         EJECT                                                                  
CNDOID   NTR1                                                                   
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   CNDOID2                                                          
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
CNDOID2  DS    0H                                                               
         CLC   =C'DELETE',ELEM+3                                                
         BE    CNDOID4                                                          
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
CNDOID4  DS    0H                                                               
         GOTO1 SETCHGDT                                                         
         CLC   =C'CM',BUTRCODE                                                  
         BNE   *+12                                                             
         CLI   CMPASS,1            ONLY DO I/O ON 2ND 'CM' PASS                 
         BE    IDEXIT                                                           
         GOTO1 PUTREC                                                           
         B     IDEXIT                                                           
         EJECT                                                                  
***************************************                                         
* EDIT SKED= OR SKEDNN=  OR SKMMMDD=  *                                         
***************************************                                         
         SPACE 1                                                                
BUYSKED  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   EDTVAL,SKEDEDT      SET FOR RSNCODE CHECK                        
         BRAS  RE,CHKRSN                                                        
*                                                                               
         TM    SVCOPT1,X'40'       TEST INFOMERCIAL CLIENT                      
         BO    BUYSK2X                                                          
         CLI   SKEDTYP,C'N'        TEST NEW 'S' FUNCTION                        
         BE    BUYSK40             YES                                          
*                                                                               
BUYSK2   MVI   BUWHY3,X'80'                                                     
         OI    SVUPDATE,X'40'      SET ADDS ACTIVITY                            
         B     BUYSK4                                                           
*                                                                               
BUYSK2X  MVI   ERRCD,BADKEYWD                                                   
         B     BUYSKERR                                                         
*                                                                               
BUYSK4   MVI   BUCHGOV,X'12'                                                    
         CLI   FLEN+1,4            TEST WEEK NUMBER PRESENT                     
         BNE   *+12                 YES                                         
         LA    R0,1                ASSUME SKED=, START AT WEEK 1                
         B     BUYSK10                                                          
* EDIT WEEK NUMBER                                                              
         CLC   =C'SKED',0(R4)      IS IT SKEDNN=?                               
         BNE   BUYSK30              NO, GO CHECK FOR DATE INPUT                 
         LA    R4,4(R4)             YES, POINT TO WEEK                          
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    BUYSK2X                                                          
         CLI   FLEN+1,2                                                         
         BH    BUYSKERR                                                         
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BUYSK2X                                                          
         ZIC   RE,BDWKS                                                         
         CR    R0,RE                                                            
         BH    BUYSK2X                                                          
*                                                                               
BUYSK10  STC   R0,BUWKS            SAVE WEEK NUMBER                             
*                                                                               
*&&DO                                                                           
* DON'T REALLY NEED TO ENFORCE THIS (AND DESKTOP IS HAPPIER WITHOUT IT)         
         MVI   ERRCD,CANTSKED                                                   
         CLI   BDWKIND,C'O'        MUST BE EVERY WEEK                           
         BNE   BUYSKERR                                                         
*&&                                                                             
*                                                                               
BUYSK12  DS    0H                                                               
         MVI   ERRCD,SKEDERR                                                    
         XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(2),=C',/'                                                 
         GOTO1 FLDVAL                                                           
         CLI   FLEN+1,2                                                         
         BH    BUYSKERR                                                         
         LTR   R5,R5                                                            
         BNZ   BUYSK14                                                          
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         B     BUYSK20                                                          
BUYSK14  TM    FVAL,X'08'                                                       
         BZ    BUYSKERR                                                         
         CVB   R0,DUB                                                           
         STC   R0,BUNPW                                                         
         TM    SVCOPT1,X'01'       TEST GMI CLIENT                              
         BO    BUYSK15             YES - MAX IS 99                              
         CLI   SVKEY+3,X'FF'       TEST POL                                     
         BNE   *+12                NO                                           
         CLI   BUNPW,75                                                         
         BH    BUYSKERR                                                         
         SPACE 2                                                                
* CALCULATE WEEK DATE                                                           
BUYSK15  GOTO1 VDATCON,DMCB,(3,BDSTART),WORK                                    
         MVC   WORK+6(6),WORK                                                   
         ZIC   R0,BUWKS            RETRIEVE WEEK NUMBER                         
         B     BUYSK18                                                          
BUYSK16  GOTO1 VADDAY,DMCB,WORK,WORK+6,7                                        
         MVC   WORK(6),WORK+6                                                   
BUYSK18  BCT   R0,BUYSK16                                                       
         SPACE 1                                                                
* MAKE SURE WEEK IS IN BUY DESC PERIOD                                          
         SPACE 1                                                                
         GOTO1 VDATCON,DMCB,WORK+6,(3,WORK+12)                                  
         CLC   WORK+12(3),BDEND                                                 
         BH    BUYSKERR                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,WORK+6,(2,BUELDT)                                   
         OI    BUSTAT,X'40'                                                     
         MVI   EDTVAL,SKEDEDT                                                   
         GOTO1 CALLCHA                                                          
*                                                                               
BUYSK20  CLI   FSTOP,C'/'                                                       
         BNE   BUYSK22             END-OF-DATA                                  
         IC    RE,BUWKS                                                         
         LA    RE,1(RE)                                                         
         STC   RE,BUWKS                                                         
         B     BUYSK12                                                          
*                                                                               
BUYSK22  CLI   FSTOP,C','          TEST MORE SKED..=                            
         BNE   BUYSKEDX                                                         
*                                                                               
BUYSK24  XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(2),=C',='                                                 
         GOTO1 FLDVAL                                                           
         CLC   =C'SK',0(R4)                                                     
         BE    BUYSK2                                                           
         MVI   ERRCD,MULTCHG                                                    
         B     BUYSKERR                                                         
         EJECT                                                                  
* ALLOW INPUT FORMAT SKJAN14=                                                   
*                                                                               
BUYSK30  DC    0H'0'                                                            
         LA    R4,2(R4)            POINT TO MONTH                               
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ELDTEDT                                                   
         GOTO1 CALLEDT                                                          
* DETERMINE WEEK NUMBER - WORK HAS DATE                                         
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK+6                                  
         SR    R5,R5               CLEAR COUNTER                                
BUYSK32  CLC   WORK(6),WORK+6                                                   
         BE    BUYSK34                                                          
         BL    BUYSK2X                IF INPUT IS LOW, ERROR                    
         GOTO1 VADDAY,DMCB,WORK+6,WORK+6,7                                      
         BCT   R5,BUYSK32                                                       
*                                                                               
BUYSK34  BCTR  R5,0                                                             
         LPR   R0,R5               GET WEEK NUMBER IN R0                        
* NEED TO UPDATE TABLE POINTERS LEFT UNCHANGED BY ELDTEDT                       
         L     R4,FADDR                                                         
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         B     BUYSK10                                                          
         EJECT                                                                  
* SCHEDULE FROM FORMATTED SCREEN - NEW 'S' FORMAT                               
*                                                                               
BUYSK40  MVI   ERRCD,NORCLSCH                                                   
         CLI   SVRCLOPT,RCLSCH                                                  
         BNE   BUYSKERR                                                         
         MVI   BUCHGOV,X'12'                                                    
*                                                                               
         LA    R2,BUYOUTH                                                       
         XC    STDTP,STDTP                                                      
         MVC   ENDDTP,=X'FFFF'                                                  
*                                                                               
* FIND AN UNP FIELD AND PRECEDING DATE FIELD                                    
*                                                                               
BUYSK42  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    BUYSKEDX                                                         
         CLI   0(R2),9                                                          
         BE    BUYSKEDX                                                         
         TM    1(R2),X'20'         TEST PROT                                    
         BZ    BUYSK42                                                          
         CLI   0(R2),25            DATE FIELD IS MAX 17 (+ FLDHDR)              
         BH    BUYSK42             SO IF HI FORGET IT                           
         ST    R2,WORK2            SAVE PROT FIELD ADDR                         
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),11                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R2,WORK2+4          SAVE UNP FIELD ADDR                          
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    BUYSK42             NO                                           
         CLI   5(R2),0             TEST NO INPUT                                
         BE    BUYSK42             YES-IGNORE                                   
* EDIT ELEMENT DATE (PROTECTED)                                                 
         L     R2,WORK2                                                         
         LA    R4,8(R2)                                                         
         CLI   0(R4),C'+'                                                       
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ELDTEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         OC    STDTP,STDTP         SAVE EARLIEST START DATE                     
         BNZ   *+10                                                             
         MVC   STDTP,BUELDT                                                     
         CLC   STDTP,BUELDT                                                     
         BNH   *+10                                                             
         MVC   STDTP,BUELDT                                                     
* EDIT NUMBER OF SPOTS                                                          
         MVI   ERRCD,INVERR                                                     
         L     R2,WORK2+4                                                       
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    BUYSKERR                                                         
         GOTO1 PACK                                                             
         CHI   R0,99                                                            
         BH    BUYSKERR                                                         
         STC   R0,BUNPW                                                         
         CLI   SVKEY+3,X'FF'                                                    
         BNE   *+12                                                             
         CLI   BUNPW,75                                                         
         BH    BUYSKERR                                                         
*                                                                               
         OI    BUSTAT,X'40'                                                     
         MVI   EDTVAL,SKEDEDT                                                   
         GOTO1 CALLCHA                                                          
         B     BUYSK42                                                          
*                                                                               
BUYSKEDX XIT1                                                                   
*                                                                               
BUYSKERR GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*======================================================*                        
* EDIT DAILY SCHEDULING FUNCTION  SD= OR SDNN=         *                        
*======================================================*                        
         SPACE 1                                                                
         DS    0H                                                               
DSKED    NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRCD,CANTSKED                                                   
         CLI   BDWKIND,C'O'        MUST BE EVERY WEEK                           
         BNE   DSKEDERR                                                         
*                                                                               
         CLI   BUYMD,C'N'          TEST NTWK                                    
         BNE   DSKED4                                                           
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   DSKED4                                                           
*                                                                               
DSKED2X  MVI   ERRCD,BADKEYWD                                                   
         B     DSKEDERR                                                         
*                                                                               
DSKED4   XC    BUDSKED,BUDSKED                                                  
         CLI   FSTOP,C'='          '=' MEANS INPUT FOLLOWS                      
         BNE   DSKED10             IF NO '=' DATA IS IN DISPLAY AREA            
*                                                                               
         MVI   EDTVAL,DSKEDT                                                    
         BRAS  RE,CHKRSN                                                        
         GOTO1 CALLEDT                                                          
         B     DSKED20                                                          
         EJECT                                                                  
*======================================================*                        
* SCHEDULE FROM FORMATTED SCREEN                       *                        
*======================================================*                        
         SPACE 1                                                                
DSKED10  MVI   ERRCD,NORCLDSK                                                   
         CLI   SVRCLOPT,RCLDSK                                                  
         BNE   DSKEDERR                                                         
         SPACE 1                                                                
* FIND AN UNPROTECTED INPUT FIELD *                                             
         SPACE 1                                                                
         LA    R2,BUYOUTH                                                       
         SR    R0,R0                                                            
         LA    RE,1                                                             
         STC   RE,BUWKS            SET WEEK NUMBER                              
*                                                                               
DSKED12  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    DSKED20                                                          
         CLI   0(R2),9                                                          
         BE    DSKED20                                                          
         TM    1(R2),X'20'         TEST PROT                                    
         BO    DSKED12             YES - SKIP                                   
*                                                                               
         CLI   0(R2),15            FIELD LEN 7 + FLDHDR (8)                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    DSKED14             NO                                           
         CLI   5(R2),0             TEST NO INPUT                                
         BE    DSKED14             YES-IGNORE                                   
*                                                                               
         LA    RE,8(R2)            POINT TO DATA                                
         ST    RE,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,DSKEDT                                                    
         GOTO1 CALLEDT                                                          
*                                                                               
DSKED14  IC    RE,BUWKS                                                         
         LA    RE,1(RE)                                                         
         STC   RE,BUWKS            BUMP WEEK NUMBER                             
         B     DSKED12                                                          
*                                                                               
DSKED20  DS    0H                                                               
         GOTO1 VDSKED,DMCB,(RC)    CHANGE BUY RECORD                            
*                                                                               
DSKEDX   XIT1                                                                   
*                                                                               
DSKEDERR GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* SPECIAL CODE TO FIX BAD PACKAGE ELEMENTS                          *           
*                                                                   *           
* INPUT FORMAT IS PKGFIX=+4/6 TO ADD LINE 4 AFTER LINE 6            *           
*                 PKGFIX=+4/RS TO MAKE LINE 4 A REVISION SLAVE      *           
*                 PKGFIX=-8 TO DELETE LINE 8 FROM PACKAGE ELEM      *           
*********************************************************************           
         SPACE 1                                                                
PKGFIX   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ERRCD,INVERR                                                     
         LA    R4,9(R4)            POINT PAST PKGFIX=                           
         CLI   0(R4),C'+'                                                       
         BE    *+12                                                             
         CLI   0(R4),C'-'                                                       
         BNE   PKGFERR                                                          
         MVC   BUREFTYP,0(R4)                                                   
*                                                                               
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   FSTOPS+1,C'/'                                                    
         GOTO1 FLDVAL                                                           
         TM    FVAL,X'08'                                                       
         BZ    PKGFERR                                                          
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    PKGFERR                                                          
         CH    R0,=H'255'                                                       
         BH    PKGFERR                                                          
         STH   R0,BUREFMAS                                                      
         CLI   BUREFTYP,C'+'                                                    
         BNE   PKGF4                                                            
         CLC   BUREFMAS+1(1),SVKEY+11  TEST SELF REF                            
         BE    PKGFERR                                                          
* MAKE SURE NEW LINE NUM ON FILE                                                
         MVI   ERRCD,NOTFOUND                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(12),SVKEY                                                    
         MVC   KEY+11(1),BUREFMAS+1                                             
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   PKGFERR                                                          
*                                                                               
PKGF2    MVI   ERRCD,INVERR                                                     
         MVI   FSTOPS+1,0                                                       
         GOTO1 FLDVAL                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    PKGF20                                                           
* NUMERIC DATA IS LINE NUM TO ADD AFTER                                         
         CVB   R0,DUB                                                           
PKGF4    STH   R0,BUINDREF                                                      
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   PKGFERR                                                          
         IC    RE,1(R6)                                                         
         BCTR  RE,0                                                             
         BM    PKGFERR                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)  *EXECUTED*                                        
*                                                                               
         LA    R4,ELEM+3                                                        
PKGF6    CLC   0(1,R4),BUINDREF+1                                               
         BE    PKGF8                                                            
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   PKGF6                                                            
         B     PKGFERR                                                          
*                                                                               
PKGF8    CLI   BUREFTYP,C'+'                                                    
         BNE   PKGF25                                                           
* BUMP ELEM LENGTH AND ADD NEW LINE NUM                                         
         IC    RE,ELEM+1                                                        
         LA    RE,1(RE)                                                         
         STC   RE,ELEM+1                                                        
         LA    R4,1(R4)                                                         
*                                                                               
PKGF10   ZIC   R0,0(R4)            SAVE NEXT LINE NUM                           
         MVC   0(1,R4),BUREFMAS+1                                               
         LTR   R0,R0                                                            
         BZ    PKGF12                                                           
         STH   R0,BUREFMAS                                                      
         LA    R4,1(R4)                                                         
         B     PKGF10                                                           
*                                                                               
PKGF12   GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
*                                                                               
PKGF14   CLI   ELEM+1,3                                                         
         BNH   PKGF16                                                           
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
PKGF16   MVI   RCLOPT,RCLREF                                                    
         GOTO1 PUTREC                                                           
         B     PKGFX                                                            
         SPACE 2                                                                
PKGF20   LA    R1,PKGLIST                                                       
         LA    R0,8                                                             
         CLC   0(2,R4),0(R1)                                                    
         BE    PKGF22                                                           
         LA    R1,2(R1)                                                         
         BCT   R0,*-14                                                          
         B     PKGFERR                                                          
PKGLIST  DC    C'PMPS    RMRSMMMS'                                              
*                                                                               
PKGF22   XC    ELEM,ELEM                                                        
         MVI   ELEM,5                                                           
         MVI   ELEM+1,4                                                         
         LA    R1,9                                                             
         SR    R1,R0               COMPUTE REF TYPE                             
         STC   R1,ELEM+2                                                        
         MVC   ELEM+3(1),BUREFMAS+1                                             
*                                                                               
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BE    PKGF12                                                           
* IF NO ELEM, INSERT BEFORE FIRST REGEL                                         
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,14                                                        
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         B     PKGF14                                                           
         SPACE 2                                                                
* DELETE SPECIFIED LINE                                                         
*                                                                               
PKGF25   MVC   0(1,R4),1(R4)                                                    
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   PKGF25                                                           
         IC    RE,ELEM+1                                                        
         BCTR  RE,0                                                             
         STC   RE,ELEM+1                                                        
         B     PKGF12                                                           
*                                                                               
PKGFX    XIT1                                                                   
*                                                                               
PKGFERR  GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* SUBROUTINE SPLITS BUYLINES THAT HAVE GOTTEN TOO FULL                *         
*     -OR-                                                            *         
*   SEPARATE SPOT FEATURE                                             *         
* NOTE THAT NEW MAKEGOODS IGNORE ALL OLD RULES, SINCE THERE ARE NO    *         
* LINE NUMBER REFERENCES IN NEWMG BUYS                                *         
* JUN04 - CANADIAN NETWORK BUYS CATERED FOR                           *         
*=====================================================================*         
         SPACE 1                                                                
         USING BUYSPWKD,R7                                                      
BUYSP    NTR1  BASE=*,LABEL=*,WORK=(R7,BUYSPWKL)                                
         XC    BSPWORK,BSPWORK                                                  
*                                                                               
         MVI   NWKSPLTF,0          FLAG NOT A NETWORK SPLIT                     
         CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   BUYSP01                                                          
         MVI   ERRCD,TRCDERR                                                    
         CLI   SEPSPOT,C'Y'        SEPARATE SPOT?                               
         BE    BUYSPR               YES, NOT SUPPORTED FOR CANADA               
         CLI   BUYMD,C'N'          TEST NTWK                                    
         BNE   BUYSP01                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOSPLIT)                                               
         CLC   BUYKEY+4(2),=X'0000'   TEST EXPLODED BUY                         
         BNE   BUYSPR              THEN CAN'T SPLIT - MAKES A MESS              
         MVI   NWKSPLTF,1          FLAG PASS 1 FOR NETWORK SPLIT                
*                                                                               
BUYSP01  LA    R6,BDELEM           CAN'T SPLIT BUY IF PKGEL PRESENT             
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(SPLTPKG)                                               
         BRAS  RE,NEXTEL                                                        
         BE    BUYSPR                                                           
                                                                                
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C'='                                                      
         XC    FLEN,FLEN           SET TO RE-EDIT                               
         GOTO1 FLDVAL              REREAD SP=/SEPSPOT=                          
                                                                                
         MVI   FSTOPS,C','         NOW GET LEN OF RC INPUT                      
         GOTO1 FLDVAL                                                           
*                                                                               
BUYSP03  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADSPLDT)                                              
         GOTO1 VDATVAL,DMCB,(1,(R4)),WORK                                       
         OC    0(4,R1),0(R1)       TEST FOR ERROR                               
         BZ    BUYSPR                                                           
* DEDUCE YEAR                                                                   
         MVC   WORK(2),SVSTART     SET YEAR FOR EST ST                          
         CLC   SVSTART(2),SVEND    TEST EST STARTS/ENDS IN SAME YR              
         BE    BUYSP05             YES                                          
         CLC   WORK+2(4),SVSTART+2 TEST INPUT MMDD VS. EST ST MMDD              
         BNL   *+10                                                             
         MVC   WORK(2),SVEND       SET EST END YEAR                             
*                                                                               
BUYSP05  MVI   ERRCD,ESPERERR                                                   
         CLC   WORK(6),SVEND       TEST DATE W/IN ESTIMATE                      
         BH    BUYSPR                                                           
         CLC   WORK(6),SVSTART                                                  
         BL    BUYSPR                                                           
*                                                                               
         A     R4,0(R1)            POINT BEYOND DATE INPUT                      
         S     R5,0(R1)            AND ADJUST INPUT LEN                         
*                                                                               
         GOTO1 VDATCON,DMCB,WORK,(2,BUELDT)                                     
         MVI   BUELNUM,1           SET DEFAULT                                  
*                                                                               
         CLI   0(R4),C'-'          TEST SPOT NUMBER FOLLOWS                     
         BNE   BUYSP10                                                          
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
* EDIT SPOT NUMBER                                                              
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADSPLNO)                                              
*                                                                               
         LR    R0,R5                                                            
         LR    R1,R4                                                            
         CHI   R5,2                                                             
         BH    BUYSPR                                                           
*                                                                               
BUYSP07  CLI   0(R1),C'0'                                                       
         BL    BUYSPR                                                           
         CLI   0(R1),C'9'                                                       
         BH    BUYSPR                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,BUYSP07                                                       
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BUYSPR                                                           
         STC   R0,BUELNUM                                                       
*                                                                               
BUYSP10  CLI   SEPSPOT,C'Y'        SEPARATE SPOT?                               
         BNE   BUYSP30              NO                                          
*                                                                               
* BUILD SEPARATE SPOT DATE/ELEMNUM TABLE IN BSPWORK USING BUBBLE SORT           
*                                                                               
         USING BSPDND,RE                                                        
         LA    RE,BSPWORK                                                       
         OC    BSPWORK,BSPWORK     FIRST TIME?                                  
         BZ    BUYSP19              YES, MOVE FIRST ENTRY IN                    
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADSPLDT)                                              
BUYSP12  LA    RE,BSPWORK+L'BSPWORK-BSPDNL RE=A(LAST ENTRY OF TABLE)            
         OC    BSPDATE(3),BSPDATE  TEST TOO MANY ENTRIES                        
         BNZ   BUYSPR               YES, ERROR                                  
*                                                                               
         LA    RF,BSPWORK          RF=A(SOT DATE/ELEMNUM)                       
BUYSP15  CR    RE,RF               AT BEGINNING OF LIST?                        
         JE    BUYSP19              YES, INSERT ENTRY                           
         LAY   RE,-BSPDNL(0,RE)    BUMP TO PREVIOUS ENTRY                       
*                                                                               
         OC    BSPDATE(3),BSPDATE  EMPTY SLOT?                                  
         BZ    BUYSP15              YES, SKIP IT                                
*                                                                               
         CLC   BSPDATE,BUELDT       MATCH DATE?                                 
         BNE   *+10                  NO                                         
         CLC   BSPNUM,BUELNUM       MATCH NUM?                                  
         BE    BUYSPR                DUPLICATE DATE/NUM                         
         BL    *+14                                                             
         MVC   BSPDATE+3(3),BSPDATE SHIFT DATE/NUM ENTRY TO RIGHT               
         B     BUYSP15                                                          
         LA    RE,BSPDNL(RE)        BUMP TO INSERT IN NEXT SLOT                 
BUYSP19  MVC   BSPDATE,BUELDT                                                   
         MVC   BSPNUM,BUELNUM                                                   
         DROP  RE                                                               
*                                                                               
         MVI   FSTOPS,C','         NOW GET LEN OF RC INPUT                      
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BNZ   BUYSP03                                                          
         EJECT                                                                  
*=================================================================*             
* CHANGE ORIGINAL BUYLINE IN REC1 AND                                           
* - FOR SPLIT BUYLINE, REMOVE SPOTS AFTER SPLIT DATE/ELEMNUM                    
* - FOR SEPARATE SPOTS, REMOVE SPOTS MATCHING DATE/ELEMNUM                      
*=================================================================*             
         SPACE 1                                                                
BUYSP30  MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC2      TO                                           
         GOTO1 MOVEREC             SAVE REC IN REC2                             
*                                                                               
         LA    RE,BSPWORK          GET A(FIRST OF DATE/ELEMNUM TABLE)           
         ST    RE,ABSPDND          AND SET IT                                   
*                                                                               
         L     R6,AREC1                                                         
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         CLI   3(R6),X'FF'         TEST POL BUY                                 
         BE    *+12                                                             
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
*                                                                               
         LA    R6,24(R6)                                                        
         SR    R5,R5                                                            
         MVI   ELEMNO,0                                                         
         XC    ELEMDT,ELEMDT                                                    
         MVI   NWKRELSB,0                                                       
         MVI   NWKRELSA,0                                                       
*                                                                               
BUYSP32  BRAS  RE,NEXTEL                                                        
         BNE   BUYSP47                                                          
         IC    RE,NWKRELSB         COUNT RELS BEFORE SPLIT (CANADA NWK)         
         AHI   RE,1                                                             
         TM    6(R6),X'80'         TEST MINUS                                   
         BNZ   *+8                                                              
         STC   RE,NWKRELSB                                                      
*                                                                               
BUYSP35  CLC   2(2,R6),ELEMDT                                                   
         BE    *+8                                                              
         MVI   ELEMNO,0                                                         
*                                                                               
         MVC   ELEMDT,2(R6)                                                     
         IC    RE,ELEMNO                                                        
         AHI   RE,1                                                             
         TM    6(R6),X'80'         TEST MINUS                                   
         BNZ   *+8                                                              
         STC   RE,ELEMNO                                                        
*                                                                               
         CLI   SEPSPOT,C'Y'        SEPARATE SPOT?                               
         BNE   BUYSP40              NO                                          
*                                                                               
* FILTER LOGIC FOR SEPARATE SPOTS                                               
*                                                                               
         L     RE,ABSPDND          RE=A(START OF DATE/ELEMNUM ENTRY)            
         USING BSPDND,RE                                                        
         LA    RF,BSPWORK+BUYSPWKL RF=A(EOT DATE/NUM)                           
BUYSP37  CR    RE,RF                                                            
         BNL   BUYSP32             END OF TABLE, LEAVE                          
         OC    BSPDATE(3),BSPDATE  ANY MORE DATES?                              
         BZ    BUYSP32              NO, LEAVE                                   
         CLC   ELEMDT,BSPDATE      MATCH SPOT DATE?                             
         BNE   *+10                 NO                                          
         CLC   ELEMNO,BSPNUM       MATCH SPOT NUM?                              
         BL    BUYSP32              LESS THEN DATE/ELEMNUM, LEAVE               
         BE    BUYSP45              YES - DELETE                                
*                                   HIGHER THAN DATE/ELEMNUM                    
         LA    RE,BSPDNL(RE)       BUMP TO NEXT DATE/NUM ENTRY                  
         ST    RE,ABSPDND          AND SAVE NEW START ADDR                      
         B     BUYSP37                                                          
         DROP  RE                                                               
*                                                                               
* FILTER LOGIC FOR SPLIT BUYLINE                                                
*                                                                               
BUYSP40  CLC   ELEMDT,BUELDT                                                    
         BL    BUYSP32             ELEM PRIOR TO SPLIT - LEAVE                  
         BH    BUYSP42             ELEM AFTER TO SPLIT - CHECK SEPSPOT          
*                                                                               
         CLC   ELEMNO,BUELNUM                                                   
         BL    BUYSP32             ELEM PRIOR SPLIT - LEAVE                     
*                                  ELSE, ELEM AFTER SPLIT - DELETE              
BUYSP42  IC    RE,NWKRELSA         COUNT RELS AFTER SPLIT (CANADA NWK)          
         AHI   RE,1                                                             
         TM    6(R6),X'80'         TEST MINUS                                   
         BNZ   *+8                                                              
         STC   RE,NWKRELSA                                                      
BUYSP45  LR    R5,R6                   SAVE ADDRESS OF DELETED ELEM             
         GOTO1 VRECUP,DMCB,AREC1,(R6)  DELETE THIS ELEMENT                      
*                                                                               
         CLI   0(R6),X'10'         ALSO DELETE ASSOCIATED X'10'-X'1F'           
         BL    *+12                                                             
         CLI   0(R6),X'1F'                                                      
         BNH   BUYSP45                                                          
*                                                                               
         BRAS  RE,NEXTEL2                                                       
         BE    BUYSP35                                                          
*                                                                               
BUYSP47  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADSPLT1)                                              
         L     R6,AREC1                                                         
         LA    R6,24(R6)                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BUYSPR              IF NO REGELS LEFT, ERROR                     
*                                                                               
         MVC   NERRCD,=Y(BADSPLT2)                                              
         LTR   R5,R5               IF NO ELEMS DELETED, ERROR                   
         BZ    BUYSPR                                                           
*                                                                               
* SPLIT POSSIBLE, CHECK INTEGRITY OF NETWORK LOCAL BUYS BEFORE COMMIT           
         CLI   NWKSPLTF,0                                                       
         BE    BUYSP49                                                          
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC             SAVE MODIFIED NTWK REC IN REC3               
         BRAS  RE,EXPSP            (NO RETURN ON ERROR)                         
         MVC   KEY,SVKEY           RESTORE ORIGINAL BUY RECORD                  
         GOTO1 GETREC              (RESTORES DMCBW5 FOR PUTREC)                 
         MVC   DUB(4),AREC3        FROM                                         
         MVC   DUB+4(4),AREC1      TO                                           
         GOTO1 MOVEREC             REPLACE WITH MODIFIED NETWORK REC            
         L     R6,AREC1            RESET ELEMENT CRITERIA                       
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         CLI   3(R6),X'FF'         TEST POL BUY                                 
         BE    *+12                                                             
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
*                                                                               
* WRITE BACK RECORD 1                                                           
BUYSP49  GOTO1 SETCHGDT                                                         
         GOTO1 PUTREC                                                           
         EJECT                                                                  
*==============================================================*                
* NOW CREATE NEW BUYLINE IN REC2 AND                                            
* - FOR SPLIT BUYLINE, REMOVE SPOTS PRIOR TO SPLIT DATE/ELEMNUM                 
* - FOR SEPARATE SPOT, REMOVE SPOTS NOT MATCHING DATE/ELEMNUM                   
*==============================================================*                
         SPACE 1                                                                
BUYSP50  MVC   DUB(4),AREC2        FROM                                         
         MVC   DUB+4(4),AREC1      TO                                           
         GOTO1 MOVEREC             MOVE BUYREC BACK TO REC 1                    
*                                                                               
         LA    RE,BSPWORK          GET A(FIRST OF DATE/ELEMNUM TABLE)           
         ST    RE,ABSPDND          AND SET IT                                   
*                                                                               
         L     R6,AREC1                                                         
         LA    R6,24(R6)                                                        
         MVI   ELEMNO,0                                                         
         XC    ELEMDT,ELEMDT                                                    
*                                                                               
BUYSP52  BRAS  RE,NEXTEL                                                        
         BNE   BUYSP68                                                          
*                                                                               
BUYSP55  CLC   2(2,R6),ELEMDT                                                   
         BE    *+8                                                              
         MVI   ELEMNO,0                                                         
*                                                                               
         MVC   ELEMDT,2(R6)                                                     
         IC    RE,ELEMNO                                                        
         AHI   RE,1                                                             
         TM    6(R6),X'80'         TEST MINUS                                   
         BNZ   *+8                                                              
         STC   RE,ELEMNO                                                        
*                                                                               
         CLI   SEPSPOT,C'Y'        SEPARATE SPOT?                               
         BNE   BUYSP60              NO                                          
*                                                                               
* FILTER LOGIC FOR SEPARATE SPOTS                                               
*                                                                               
         L     RE,ABSPDND          RE=A(START OF DATE/ELEMNUM ENTRY)            
         USING BSPDND,RE                                                        
         LA    RF,BSPWORK+BUYSPWKL RF=A(EOT DATE/NUM)                           
BUYSP57  CR    RE,RF               END OF TABLE?                                
         BNL   BUYSP65              YES, DELETE                                 
         OC    BSPDATE(3),BSPDATE  ANY MORE DATES/NUMS?                         
         BZ    BUYSP65              NO, DELETE                                  
         CLC   ELEMDT,BSPDATE      MATCH SPOT DATE?                             
         BNE   *+10                                                             
         CLC   ELEMNO,BSPNUM       MATCH SPOT NUM?                              
         BL    BUYSP65              LESS THEN DATE/ELEMNUM, DELETE              
         BE    BUYSP52              YES - KEEP                                  
*                                   HIGHER THAN DATE/ELEMNUM                    
         LA    RE,BSPDNL(RE)       BUMP TO NEXT DATE/NUM ENTRY                  
         ST    RE,ABSPDND          AND SAVE NEW START ADDR                      
         B     BUYSP57                                                          
*                                                                               
* FILTER LOGIC FOR SPLIT BUYLINE                                                
*                                                                               
BUYSP60  CLC   ELEMDT,BUELDT                                                    
         BL    BUYSP65             ELEM PRIOR SPLIT - DELETE                    
         BH    BUYSP68             ELEM AFTER SPLIT - DONE                      
*                                                                               
         CLC   ELEMNO,BUELNUM                                                   
         BNL   BUYSP68             ELEM MATCH/AFTER SPLIT - DONE                
*                                  ELEM PRIOR SPLIT - DELETE                    
BUYSP65  GOTO1 VRECUP,DMCB,AREC1,(R6)  DELETE THIS ELEMENT                      
*                                                                               
         CLI   0(R6),X'10'         ALSO DELETE ASSOCIATED X'10'-X'1F'           
         BL    *+12                                                             
         CLI   0(R6),X'1F'                                                      
         BNH   BUYSP65                                                          
*                                                                               
         BRAS  RE,NEXTEL2                                                       
         BE    BUYSP55                                                          
*                                                                               
BUYSP68  XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY       A-M/CLT/PRD/MKT/STA/EST                      
         BRAS  RE,NXTBUYLN         GET NEW LINE NUMBER                          
         GOTO1 ADDREC                                                           
         EJECT                                                                  
*================================================================*              
* FOR TRUE POL BUYING NEED TO DO GETREC/PUTREC                   *              
* TO ADD BRAND POINTERS. NOTE DISK ADDRESS IS IN KEY+14          *              
*================================================================*              
         SPACE 1                                                                
         CLI   KEY+3,X'FF'         TEST POL                                     
         BNE   BUYSP90              NO                                          
         XC    ELEM,ELEM                                                        
         LA    R6,BDELEM                                                        
*                                                                               
BUYSP80  BRAS  RE,NEXTEL                                                        
         BNE   BUYSP85                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AHI   R0,-10                                                           
         BNP   BUYSP80                                                          
         SRL   R0,2                SET FOR BCT                                  
         LA    R1,10(R6)                                                        
*                                                                               
BUYSP82  ZIC   RE,0(R1)            GET PRODUCT NUMBER                           
         LA    RE,ELEM(RE)         POINT TO SLOT IN TABLE                       
         MVC   0(1,RE),0(R1)       SET PRD CODE IN TABLE                        
         LA    R1,4(R1)                                                         
         BCT   R0,BUYSP82                                                       
         B     BUYSP80                                                          
*                                                                               
BUYSP85  OC    ELEM,ELEM           ANY ALLOCATIONS FOUND                        
         BZ    BUYSP90                                                          
         XC    PRDLIST,PRDLIST                                                  
         LA    R1,ELEM                                                          
         LA    R0,254                                                           
         LA    RE,PRDLIST                                                       
*                                                                               
* MOVE ALL NON-ZERO ENTRIES TO PRDLIST                                          
*                                                                               
BUYSP87  MVC   0(1,RE),0(R1)       MOVE ENTRY                                   
         CLI   0(RE),0                                                          
         BE    *+8                 IF ZERO DO NOT ADVANCE LIST POINTER          
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,BUYSP87                                                       
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    RE,PRDLIST                                                       
         ST    RE,DMCB+20          SET ADDRESS OF ADDED PRD CODES               
         GOTO1 PUTREC                                                           
*                                                                               
BUYSP90  CLI   NWKSPLTF,0                                                       
         BE    BUYSPX                                                           
         MVI   NWKSPLTF,2          UPDATE LOCAL BUYS                            
         BRAS  RE,EXPSP            (NO RETURN ON ERROR)                         
*                                                                               
BUYSPX   XIT1                                                                   
*                                                                               
BUYSPR   GOTO1 ERROR                                                            
         LTORG                                                                  
         DROP  R7                                                               
*                                                                               
BUYSPWKD DSECT                                                                  
BSPWORK  DS    CL(MAXSPLT*BSPDNL)                                               
ABSPDND  DS    A                                                                
BUYSPWKL EQU   *-BUYSPWKD                                                       
*                                                                               
BSPDND   DSECT                                                                  
BSPDATE  DS    CL2                                                              
BSPNUM   DS    CL1                                                              
BSPDNL   EQU   *-BSPDND                                                         
*                                                                               
MAXSPLT  EQU   7                   MAX NUMBER OF SPLIT DATES                    
T21107   CSECT                                                                  
         EJECT                                                                  
*=====================================================================*         
* SUBROUTINE SPLITS NETWORK EXPLODED BUYLINES                         *         
* ENTRY: BUELDT/BUELNUM SET, NWKSPLTF=1 FOR VALIDATION PASS (2=UPDATE)*         
* REC AREA USAGE:                                                     *         
*  AREC1 DESTROYED - WORKING RECORD (CALLER HANDLES ANY SAVE/RESTORE) *         
*  AREC2 PRESERVED - ORIGINAL NETWORK BUY BEING SPLIT (PASSED)        *         
*  AREC3 PRESERVED - RESERVED FOR CALLER (SAVE/RESTORE)               *         
*  AREC4 DESTROYED - ORIGINAL LOCAL BUY BEING SPLIT (SET INTERNALLY)  *         
*  AREC5 PRESERVED                                                    *         
*=====================================================================*         
         SPACE 1                                                                
EXPSP    NTR1  BASE=*,LABEL=*                                                   
         L     R6,AREC2            NETWORK BUY (PASSED)                         
         LA    R6,24(R6)                                                        
         LR    R5,R6                                                            
*                                                                               
EXPSP2   MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         LR    R6,R5                                                            
         BAS   RE,NEXTEL                                                        
         BNE   EXPSPX                                                           
         LR    R5,R6               SAVE A(EXPLODED BUY ELEM)                    
*                                                                               
* READ EXPLODED KEY/REC                                                         
         L     RE,AREC2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(10),0(RE)                                                    
         MVC   KEY+4(5),2(R6)      MKT/STA                                      
         MVC   KEY+11(2),10(RE)    LINE NUMBER                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
* CANNOT SPLIT IF SHOWDEF DAY OFFSETS IN FORCE                                  
* LOCAL DATE WON'T MATCH NETWORK'S SO SPLIT WILL BE IN WRONG PLACE              
         L     RE,AREC2            LOCAL                                        
         L     RF,AREC1            NETWORK                                      
         CLC   BDSTART-BUYREC(L'BDSTART+L'BDEND,RE),BDSTART-BUYREC(RF)          
         BNE   EXPSPR                                                           
         CLC   BDDAY-BUYREC(L'BDDAY,RE),BDDAY-BUYREC(RF)                        
         BNE   EXPSPR                                                           
*                                                                               
*=================================================================*             
* REMOVE SPOTS AFTER SPLIT DATE/ELEMNUM FROM LOCAL STATION REC    *             
*=================================================================*             
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC4      TO                                           
         GOTO1 MOVEREC             SAVE LOCAL REC IN REC4                       
*                                                                               
         L     R6,AREC1                                                         
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         CLI   3(R6),X'FF'         TEST POL BUY                                 
         BE    *+12                                                             
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
*                                                                               
         LA    R6,24(R6)                                                        
         SR    R7,R7                                                            
         MVI   ELEMNO,0                                                         
         XC    ELEMDT,ELEMDT                                                    
         MVI   LCLRELSB,0                                                       
         MVI   LCLRELSA,0                                                       
*                                                                               
EXPSP12  BRAS  RE,NEXTEL                                                        
         BNE   EXPSP20                                                          
         IC    RE,LCLRELSB                                                      
         AHI   RE,1                                                             
         TM    6(R6),X'80'         TEST MINUS                                   
         BNZ   *+8                                                              
         STC   RE,LCLRELSB                                                      
*                                                                               
EXPSP14  CLC   2(2,R6),ELEMDT                                                   
         BE    *+8                                                              
         MVI   ELEMNO,0                                                         
*                                                                               
         MVC   ELEMDT,2(R6)                                                     
         IC    RE,ELEMNO                                                        
         AHI   RE,1                                                             
         TM    6(R6),X'80'         TEST MINUS                                   
         BNZ   *+8                                                              
         STC   RE,ELEMNO                                                        
*                                                                               
         CLC   ELEMDT,BUELDT                                                    
         BL    EXPSP12                                                          
         BH    EXPSP15                                                          
*                                                                               
         CLC   ELEMNO,BUELNUM                                                   
         BL    EXPSP12                                                          
*                                                                               
EXPSP15  IC    RE,LCLRELSA                                                      
         AHI   RE,1                                                             
         TM    6(R6),X'80'         TEST MINUS                                   
         BNZ   *+8                                                              
         STC   RE,LCLRELSA                                                      
EXPSP16  LR    R7,R6                   SAVE ADDRESS OF DELETED ELEM             
         GOTO1 VRECUP,DMCB,AREC1,(R6)  DELETE THIS ELEMENT                      
*                                                                               
         CLI   0(R6),X'10'                                                      
         BL    *+12                                                             
         CLI   0(R6),X'1F'                                                      
         BNH   EXPSP16                                                          
*                                                                               
         BRAS  RE,NEXTEL2                                                       
         BE    EXPSP14                                                          
*                                                                               
EXPSP20  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOSPLIT)                                               
         L     R6,AREC1                                                         
         LA    R6,24(R6)                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   EXPSPR              IF NO REGELS LEFT, ERROR                     
*                                                                               
         LTR   R7,R7               IF NO ELEMS DELETED, ERROR                   
         BZ    EXPSPR                                                           
*                                                                               
* CROSS CHECK LOCAL REGELEMS BEFORE/AFTER MATCH NETWORK'S                       
         CLC   LCLRELSB,NWKRELSB   CROSS CHECK RELS BEF/AFT MATCH NWKS          
         BE    EXPSP22                                                          
* IF SPLIT AT A SPOT# COULD MOVE SPOT IN NWK BUY THAT WE DONT IN LOCAL          
*        CLI   BUELNUM,1                                                        
*        BNE   EXPSPR              MUST SPLIT AT DATE ONLY, NOT SPOT#           
         B     EXPSPR              (DECIDED BEST TREAT AS ALWAYS ERROR)         
EXPSP22  CLC   LCLRELSA,NWKRELSA                                                
         BE    EXPSP25                                                          
* IF SPLIT AT A SPOT# COULD MOVE SPOT IN NWK BUY THAT WE DONT IN LOCAL          
*        CLI   BUELNUM,1           NWK HAS LESS AFTER...                        
*        BNE   EXPSPR              MUST SPLIT AT DATE ONLY, NOT SPOT#           
         B     EXPSPR              (DECIDED BEST TREAT AS ALWAYS ERROR)         
*                                                                               
EXPSP25  CLI   NWKSPLTF,1          THAT'S IT FOR VALIDATION PASS                
         BE    EXPSP2              CHECK NEXT 68 ELEM BUY                       
*                                                                               
* WRITE BACK RECORD 1                                                           
         GOTO1 SETCHGDT                                                         
         GOTO1 PUTREC                                                           
         EJECT                                                                  
*====================================================================*          
* NOW CREATE NEW LOCAL BUYLINE AND REMOVE SPOTS PRIOR TO SPLIT DATE  *          
*====================================================================*          
         SPACE 1                                                                
         MVC   DUB(4),AREC4        FROM                                         
         MVC   DUB+4(4),AREC1      TO                                           
         GOTO1 MOVEREC             MOVE LOCAL REC BACK TO REC 1                 
*                                                                               
         L     R6,AREC1                                                         
         LA    R6,24(R6)                                                        
         MVI   ELEMNO,0                                                         
         XC    ELEMDT,ELEMDT                                                    
*                                                                               
EXPSP32  BRAS  RE,NEXTEL                                                        
         BNE   EXPSP40                                                          
*                                                                               
EXPSP34  CLC   2(2,R6),ELEMDT                                                   
         BE    *+8                                                              
         MVI   ELEMNO,0                                                         
*                                                                               
         MVC   ELEMDT,2(R6)                                                     
         IC    RE,ELEMNO                                                        
         AHI   RE,1                                                             
         TM    6(R6),X'80'         TEST MINUS                                   
         BNZ   *+8                                                              
         STC   RE,ELEMNO                                                        
*                                                                               
         CLC   ELEMDT,BUELDT                                                    
         BL    EXPSP36             ELEM PRIOR TO SPLIT - DELETE                 
         BH    EXPSP40             ELEM AFTER SPLIT - DONE                      
*                                                                               
         CLC   ELEMNO,BUELNUM                                                   
         BNL   EXPSP40                                                          
*                                                                               
EXPSP36  GOTO1 VRECUP,DMCB,AREC1,(R6)  DELETE THIS ELEMENT                      
*                                                                               
         CLI   0(R6),X'10'                                                      
         BL    *+12                                                             
         CLI   0(R6),X'1F'                                                      
         BNH   EXPSP36                                                          
*                                                                               
         BRAS  RE,NEXTEL2                                                       
         BE    EXPSP34                                                          
*                                                                               
EXPSP40  MVC   BUYKEY+10(2),SVKEY+11  NEW LINE NUMBER INTO RECORD               
         GOTO1 ADDREC                                                           
*================================================================*              
* FOR TRUE POL BUYING NEED TO DO GETREC/PUTREC                   *              
* TO ADD BRAND POINTERS. NOTE DISK ADDRESS IS IN KEY+14          *              
*================================================================*              
         SPACE 1                                                                
         CLI   KEY+3,X'FF'         TEST POL                                     
         BNE   EXPSP60                                                          
         XC    ELEM,ELEM                                                        
         LA    R6,BDELEM                                                        
*                                                                               
EXPSP50  BRAS  RE,NEXTEL                                                        
         BNE   EXPSP54                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AHI   R0,-10                                                           
         BNP   EXPSP50                                                          
         SRL   R0,2                SET FOR BCT                                  
         LA    R1,10(R6)                                                        
*                                                                               
EXPSP52  ZIC   RE,0(R1)            GET PRODUCT NUMBER                           
         LA    RE,ELEM(RE)         POINT TO SLOT IN TABLE                       
         MVC   0(1,RE),0(R1)       SET PRD CODE IN TABLE                        
         LA    R1,4(R1)                                                         
         BCT   R0,EXPSP52                                                       
         B     EXPSP50                                                          
*                                                                               
EXPSP54  OC    ELEM,ELEM           ANY ALLOCATIONS FOUND                        
         BZ    EXPSP60                                                          
         XC    PRDLIST,PRDLIST                                                  
         LA    R1,ELEM                                                          
         LA    R0,254                                                           
         LA    RE,PRDLIST                                                       
*                                                                               
* MOVE ALL NON-ZERO ENTRIES TO PRDLIST                                          
*                                                                               
EXPSP56  MVC   0(1,RE),0(R1)       MOVE ENTRY                                   
         CLI   0(RE),0                                                          
         BE    *+8                 IF ZERO DO NOT ADVANCE LIST POINTER          
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,EXPSP56                                                       
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    RE,PRDLIST                                                       
         ST    RE,DMCB+20          SET ADDRESS OF ADDED PRD CODES               
         GOTO1 PUTREC                                                           
*                                                                               
EXPSP60  B     EXPSP2              NEXT LOCAL BUY                               
*                                                                               
EXPSPX   XIT1                                                                   
*                                                                               
EXPSPR   CLI   NWKSPLTF,1                                                       
         BE    *+6                                                              
         DC    H'0'                PROBLEM FATAL IF NOT VALIDATE PASS           
         GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*==================================================================*            
* FIND NEXT AVAILABLE LINE NUMBER.                                 *            
* KEY HAS A-M/CLT/PRD/MKT/STA/EST                                               
* RETURN NEW KEY IN KEY  AND SET KEY IN BUYREC                     *            
*==================================================================*            
         SPACE 1                                                                
         DS    0D                                                               
NXTBUYLN NTR1  BASE=*,LABEL=*                                                   
         OI    DMINBTS,X'88'                                                    
         GOTO1 HIGH                                                             
         B     NXTBUY4                                                          
*                                                                               
NXTBUY2  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
NXTBUY4  CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BE    NXTBUY2                                                          
                                                                                
         MVC   KEY,KEYSAVE                                                      
         ICM   RE,3,KEY+11                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,KEY+11                                                      
                                                                                
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(13),KEY       ONLY WANT KEY, NOT D/A                       
*                                                                               
         MVI   ERRCD,MAXLINES                                                   
         CLC   KEYSAVE+11(2),=H'255'                                            
         BL    NXTBUY10                                                         
         CLI   SV1OR2,2                                                         
         BNE   NXTBUY8                                                          
         CLC   KEYSAVE+11(2),=Y(MAXBUYS)                                        
         BL    NXTBUY10                                                         
*                                                                               
NXTBUY8  MVC   WORK(L'BUYKEY),KEY                                               
         BAS   RE,FNDPREV          FIND PREVIOUS BUY LINE NOT IN USE            
         BNE   NXTBERR                                                          
         MVC   KEY+11(2),HALF                                                   
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(13),KEY       SET IN SVKEY TOO, BUT NOT D/A!!!!            
*                                                                               
NXTBUY10 MVC   BUYKEY(10),KEY                                                   
         MVC   BUYKEY+10(2),KEY+11                                              
*                                                                               
NBX      XIT1                                                                   
*                                                                               
NXTBERR  MVI   ERRAREA,X'FE'       FORCE DCHO                                   
         GOTO1 ERROR                                                            
         EJECT                                                                  
*==================================================================*            
* FIND A PREVIOUS BUY NUMBER THAT WASN'T USED                    *              
*==================================================================*            
         SPACE 1                                                                
FNDPREV  NTR1                                                                   
         MVC   WORK+20(L'BUYKEY),KEY                                            
         MVC   HALF,=H'1'                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(11),WORK        SET A/M/CLT/PRD/MKSTA/EST                    
         OI    DMINBTS,X'08'       GET DELETED                                  
         GOTO1 HIGH                                                             
         B     FP20                                                             
                                                                                
FP10     MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
                                                                                
FP20     CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BNE   FPNO                                                             
         CLC   HALF,KEY+11         NEXT NUMERICAL NUMBER ON FILE                
         BNE   FP30                                                             
                                                                                
FP25     LH    R0,HALF                                                          
         AHI   R0,1                INCREMENT LAST LINE NUMBER                   
         STH   R0,HALF                                                          
         B     FP10                                                             
                                                                                
FP30     OC    HALF,HALF                                                        
         BZ    FP25                                                             
         LA    R0,MAXBUYS                                                       
         CLI   SV1OR2,2                                                         
         BE    *+8                                                              
         LA    R0,255                                                           
         CH    R0,HALF                                                          
         BE    FPNO                                                             
         MVC   KEY(L'BUYKEY),WORK+20                                            
         J     EQXIT                                                            
                                                                                
FPNO     MVC   KEY(L'BUYKEY),WORK+20                                            
         LTR   RB,RB                                                            
         J     NEQXIT                                                           
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* SUBROUTINE VALIDATES CCUSA ACN NUMBERS                                        
*===============================================================                
         SPACE 1                                                                
         DS    0D                                                               
GETACN   NTR1  BASE=*,LABEL=*                                                   
         SPACE 1                                                                
* MAKE SURE ACN IS 5 DIGITS *                                                   
         SPACE 1                                                                
         LA    R0,5                                                             
         LA    R1,BUORB                                                         
GETACN32 CLI   0(R1),C'0'                                                       
         BL    ACNERR2                                                          
         CLI   0(R1),C'9'                                                       
         BH    ACNERR2                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,GETACN32                                                      
         CLI   0(R1),C' '                                                       
         BNE   ACNERR2                                                          
*                                                                               
GETACN40 DS    0H                  SWITCH TO ACC SYSTEM                         
         L     RF,VCOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVXFRSYS    SET SYSTEM NUMBER                            
****************                                                                
         CLC   T211FFD+10(2),=X'0011' TEST ID=SJR                               
         BNE   GETACN41                                                         
         MVI   DMCB,6                 FORCE ACC SYSTEM NUMBER                   
         MVI   SVXFRCOM,C'9'                                                    
****************                                                                
GETACN41 GOTO1 (RF),DMCB                                                        
         CLI   4(R1),2             TEST ACC NOT OP                              
         BE    ACNERR3                                                          
*                                                                               
         CLI   4(R1),0             ALL OTHER ERRORS ARE FATAL                   
         BE    GETACN44                                                         
         DC    H'0'                                                             
         SPACE 1                                                                
* VALIDATE ACN AND AGENCY ON SE LEDGER *                                        
         SPACE 1                                                                
GETACN44 DS    0H                                                               
         MVC   WORK2,SPACES                                                     
         MVC   WORK2(1),SVXFRCOM   SET COMPANY                                  
         MVC   WORK2+1(2),=C'SE'                                                
         MVC   WORK2+3(5),BUORB    SET ACN NUMBER                               
         GOTO1 ACCHIGH                                                          
         L     RE,AREC2                                                         
         CLC   WORK2(42),0(RE)                                                  
         BNE   ACNERR2             SET INVALID ACN ERR                          
*                                                                               
         MVC   WORK2+8(3),SVXFRAGY                                              
         GOTO1 ACCHIGH                                                          
         L     RE,AREC2                                                         
         CLC   WORK2(42),0(RE)                                                  
         BNE   ACNERR6                                                          
*                                                                               
GETACN52 DS    0H                  SWITCH BACK TO SPOT                          
         L     RF,VCOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'SPOT',0                                             
* RESTORE DIRECTORY FOR SEQUENTIAL (NOTE KEY NOT TOUCHED) *                     
         GOTO1 HIGH                                                             
*                                                                               
GETACNX  DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
GETACNEL L     R1,AREC2            POINT TO IO AREA                             
         LA    R1,49(R1)           POINT TO FIRST ELEM                          
         SR    R0,R0                                                            
ACNEL2   CLC   ELCODE,0(R1)                                                     
         BER   RE                  RETURN WITH CC EQ                            
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   ACNEL2                                                           
         LTR   RE,RE               RETURN WITH CC NOT EQ                        
         BR    RE                                                               
         SPACE 1                                                                
* NOTE ACCHIGH USES IOAREA 2 *                                                  
         SPACE 1                                                                
ACCHIGH  LR    R0,RE                                                            
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',WORK2,AREC2                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ACNERR1  LA    R1,LACNMSG1         BAD INTERFACE NUMBER                         
         B     ACNERRX                                                          
ACNERR2  LA    R1,LACNMSG2         BAD ACN NUMBER                               
         B     ACNERRX                                                          
ACNERR3  LA    R1,LACNMSG3         ACC SYSTEM NOT OP                            
         B     ACNERRX                                                          
ACNERR4  LA    R1,LACNMSG4         VEHICLE NOT VALID                            
         B     ACNERRX                                                          
ACNERR5  LA    R1,LACNMSG5         NO ACN ON VEHICLE                            
         B     ACNERRX                                                          
ACNERR6  LA    R1,LACNMSG6         INVALID AGENCY/ACN                           
         B     ACNERRX                                                          
*                                                                               
ACNERRX  MVC   BUYMSG(13),=C'* ACN ERROR *'                                     
         ZIC   RE,0(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BUYMSG+14(0),1(R1) *EXECUTED*                                    
*                                                                               
         MVI   ERRAREA,X'FF'       INDICATE MESSAGE PRESENT                     
         GOTO1 ERROR                                                            
*                                                                               
LACNMSG1 DC    AL1(L'ACNMSG1)                                                   
ACNMSG1  DC    C'INVALID INTERFACE NUMBER'                                      
LACNMSG2 DC    AL1(L'ACNMSG2)                                                   
ACNMSG2  DC    C'INVALID ACN NUMBER'                                            
LACNMSG3 DC    AL1(L'ACNMSG3)                                                   
ACNMSG3  DC    C'ACC SYSTEM NOT OPERATIONAL'                                    
LACNMSG4 DC    AL1(L'ACNMSG4)                                                   
ACNMSG4  DC    C'VEHICLE NOT VALID ON 3M LEDGER'                                
LACNMSG5 DC    AL1(L'ACNMSG5)                                                   
ACNMSG5  DC    C'NO ACN ON VEHICLE (SE)'                                        
LACNMSG6 DC    AL1(L'ACNMSG6)                                                   
ACNMSG6  DC    C'INVALID ACN/AGENCY PAIR'                                       
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        NOW FIX/BLD ELEMENT - PSTOUT HAS 10 CHAR                               
*                                                                               
SETPST   NTR1  BASE=*,LABEL=*                                                   
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
SPST2    IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    SPST30                                                           
         CLI   0(R6),X'6B'                                                      
         BNE   SPST2                                                            
*                                                                               
         CLC   BDSTART,=X'6E061C'  TEST JUL/10 +                                
         BL    SPST4               NO                                           
         GOTO1 VRECUP,DMCB,BUYREC,(R6)    DELETE OLD ELEMENT                    
         B     SPST30                     AND INSERT NEW                        
*                                                                               
SPST4    LA    R6,2(R6)            POINT TO START OF PST CODES                  
         LA    R1,PSTOUT                                                        
         LA    R4,10                                                            
*                                                                               
SPST10   CLC   0(1,R6),0(R1)       IS THIS PROVINCE EQUAL                       
         BE    SPST20                                                           
         CLI   0(R1),C' '          WAS THERE AN OVERRIDE FOR THIS PROV          
         BNH   SPST20                                                           
         MVC   0(1,R6),0(R1)       THEN SET CODE TO NEW CODE                    
*                                                                               
SPST20   LA    R6,1(R6)            BUMP                                         
         LA    R1,1(R1)                                                         
         BCT   R4,SPST10                                                        
         B     SPST40                                                           
*                                                                               
SPST30   XC    ELEM,ELEM           ELSE GO ADD THE ELEMENT                      
         MVC   ELEM(2),=X'6B0C'                                                 
         MVC   ELEM+2(10),PSTOUT                                                
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
SPST40   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*==============================================================                 
* SUBROUTINE CHECKS THAT REASON CODE IS PRESENT IF REQUIRED                     
* EDTVAL VALUES ARE LISTED BY REASON CODE SCHEME                                
*==============================================================                 
         SPACE 1                                                                
CHKRSN   NTR1  BASE=*,LABEL=*                                                   
         LHI   R4,SVB0PROF-BUYSAVE                                              
         AR    R4,RA                                                            
         CLI   7(R4),C'1'          CHECK RSNCODES ACTIVE                        
         BL    CHKRSNX                                                          
*                                                                               
         LA    R1,RSNTAB1                                                       
         LHI   RE,SVRSNFLD-BUYSAVE                                              
         AR    RE,RA                                                            
         MVI   0(RE),0             ASSUME RSNCODE NOT REQD                      
*                                                                               
CHKRSN2  CLC   7(1,R4),0(R1)       MATCH RSNCODE SCHEME                         
         BE    CHKRSN4                                                          
         SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),X'FF'                                                      
         BNE   CHKRSN2                                                          
*                                                                               
CHKRSN4  CLI   2(R1),C'Y'          TEST REQD ONLY IF LOCKED                     
         BNE   CHKRSN6                                                          
         CLI   SVANYLOK,C'Y'       TEST ORDER LOCKED                            
         BNE   CHKRSNX             NO - NO REASON CODE NEEDED                   
*                                                                               
CHKRSN6  LA    R1,3(R1)            POINT TO FIRST EDTVAL                        
*                                                                               
CHKRSN8  CLC   EDTVAL,0(R1)                                                     
         BE    CHKRSN10                                                         
         LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   CHKRSN8                                                          
         B     CHKRSNX                                                          
*                                                                               
CHKRSN10 LHI   R4,SVRSNEL-BUYSAVE                                               
         AR    R4,RA                                                            
         CLI   0(R4),0             TEST ELEMENT PRESENT                         
         BE    CHKRSNER                                                         
         LHI   R4,SVRSNFLD-BUYSAVE POINT TO FIELD ID                            
         AR    R4,RA                                                            
         MVC   0(1,R4),1(R1)       SET FIELD ID IN ELEMENT                      
* NOW CLEAR REASON CODE SO THEY MUST RE-ENTER !                                 
         LHI   R4,SVRSNEL-BUYSAVE                                               
         AR    R4,RA                                                            
         XC    0(69,R4),0(R4)                                                   
*                                                                               
CHKRSNX  XIT1                                                                   
*                                                                               
CHKRSNER MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NORSNCOD)                                              
         GOTO1 ERROR                                                            
*                                                                               
RSNTAB1  DC    C'1'                  SCHEME ID                                  
         DC    AL1(RSNTAB2-RSNTAB1)  LEN OF THIS SCHEME 1 TABLE                 
         DC    C'Y'                  RSNCODE REQD ONLY IF LOCKED                
         DC    AL1(PEREDT,RCID_PER)  EDTVAL,VALUE IN RSNCODE ELEM               
         DC    AL1(TIMEDT,RCID_TIM)                                             
         DC    AL1(SLNEDT,RCID_SLN)                                             
         DC    AL1(COSTEDT,RCID_COS)                                            
         DC    AL1(NPWEDT,RCID_NPW)                                             
         DC    AL1(SKEDEDT,RCID_SKD)                                            
         DC    AL1(DSKEDT,RCID_SKD)                                             
         DC    X'FF'                 END OF THIS TABLE                          
*                                                                               
RSNTAB2  DC    X'FF'                 END OF ALL TABLES                          
         LTORG                                                                  
         EJECT                                                                  
* DSECT TO COVER OVERLAY WORKING STORAGE                                        
*                                                                               
LOCALD   DSECT                                                                  
*                                                                               
STARTLIN DS    XL2                                                              
ENDLIN   DS    XL2                                                              
LASTLIN  DS    XL2                                                              
THISLIN  DS    XL2                 LINE NUMBER BEING PROCESSED                  
NLINES   DS    XL2                                                              
LINLIST  DS    XL60                                                             
*                                                                               
ACHGNTRY DS    A                                                                
SAVER4   DS    A                                                                
SAVER5   DS    A                                                                
SVFLDVAL DS    XL(SLNTAB-FADDR)    SAVE FLDVAL VALUES                           
SKEDTYP  DS    C                   O=OLD SKED, N=NEW 'S' FUNCTION               
*                                                                               
ORIGDATA DS    CL(L'BDPROGRM)      LENGTH OF LONGEST FIELD                      
*                                                                               
PSTOUT   DS    CL10                                                             
NWKSPLTF DS    XL1                 NETWORK SPLIT FLAG 1=VAL 2=PROCESS           
NWKRELSB DS    XL1                 # REGELS BEFORE SPLIT MARK, NETWORK          
NWKRELSA DS    XL1                 # REGELS AFTER SPLIT MARK, NETWORK           
LCLRELSB DS    XL1                 # REGELS BEFORE SPLIT MARK, LOCAL            
LCLRELSA DS    XL1                 # REGELS AFTER SPLIT MARK, LOCAL             
*                                                                               
SEPSPOT  DS    C                   SEPARATE SPOT FLAG                           
         DS    XL(L'OVWORK-(*-LOCALD))  SPARE                                   
         SPACE 2                                                                
* DSECT TO COVER CHANGE KEYWORD TABLE                                           
*                                                                               
CHGTABD  DSECT                                                                  
CHGNAME  DS    CL7                 KEYWORD NAME                                 
CHGMINL  DS    X                   MINIMUM LENGTH FOR KEYWORD                   
CHGOV    DS    X                   CHANGE OVERLAY                               
CHGEDT   DS    X                   EDIT VALUE                                   
CHGCTL   DS    X                   CONTROL BITS                                 
CHGROUT  DS    AL3                 A(CHANGE ROUTINE)                            
CHGORIG  DS    AL3                 A(ORIGINAL DATA CHECK ROUTINE)               
CHGTABL  EQU   *-CHGTABD                                                        
         SPACE 2                                                                
NOEDT    EQU   X'80'               CHANGE KEYWORD DOES NOT NEED EDIT            
NBUY     EQU   X'40'               CANADIAN NETWORK BUY                         
EXP      EQU   X'20'               BUY EXPLODE                                  
CANAD    EQU   X'10'               CANADIAN AGENCIES                            
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTEQ                                                      
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPBUYWORK                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENRSN                                                       
       ++INCLUDE SPGENPURP                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061SPBUY07   11/03/20'                                      
         END                                                                    
