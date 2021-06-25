*          DATA SET ACSCR17    AT LEVEL 030 AS OF 08/27/15                      
*PHASE T60C17A,+0                                                               
*INCLUDE LOADER                                                                 
         TITLE 'APG FUNCTIONALITY'                                              
T60C17   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C17,RA,R9,RR=RE,CLEAR=YES                                    
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
*                                                                               
         L     RC,APALOCAL                                                      
*                                                                               
         L     R3,=V(LOADER)                                                    
         AR    R3,RE                                                            
         ST    R3,ALOADER                                                       
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
SCR30    CLI   APMODE,APMVALR      VALIDATE RECORD MODE?                        
         BNE   SCR50               NO                                           
         CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APPFKEY,PFKEXIT     RETURN?                                      
         BNE   SCR50               NO                                           
         TM    TWASWPST,TWASWAP    SHOULD WE SWAP?                              
         BZ    EXIT                                                             
         MVI   APPFKEY,0                                                        
         MVI   APMODE,APMSWP       SWAP                                         
         MVC   APPARM(1),TWASWPRE                                               
         MVC   APPARM+1(1),TWASWPAC                                             
         B     EXIT                                                             
*                                                                               
SCR50    LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE           GET MODE                                     
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY              VALIDATE KEY                                 
         B     VALREC              VALIDATE RECORD                              
         B     DISKEY              DISPLAY KEY                                  
         B     DISREC              DISPLAY RECORD                               
         B     DELREC              DELETE RECORD                                
         B     RESREC              RESTORE RECORD                               
         B     VALSEL              VALSEL                                       
         B     GETSEL              GETSEL                                       
         B     DISSEL              DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     PROCLST             PROCESS LIST/SELECT                          
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ  (NOT USED)                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                COPY RECORD                                  
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT ,                                                                
EXIT     DS    0H                                                               
         CLI   APPFKEY,PFKNEXT     PF11 ?                                       
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK      VALKEY ?                                     
         BE    EXIT95                                                           
*                                                                               
         TM    TWASWPST,TWASWAP    SWAP  TO   NEW  RECORD ACTION ?              
         BZ    EXIT95              NO                                           
         XC    APCURSOR,APCURSOR   DON'T SET  CURSOR   ON WRONG SCREEN          
         MVI   APMODE,APMSWP       SWAP                                         
         MVC   APPARM(1),TWASWPRE  SWAP  RECORD                                 
*                                  SWAP  ACTION                                 
         MVC   APPARM+1(1),TWASWPAC                                             
*                                                                               
EXIT95   DS    0H                                                               
         OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   DS    0H                                                               
         OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK) ANY   ERRORS    FOUND ?                      
*                                                                               
XIT      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         SPACE 1                                                                
         USING APGRECD,R2                                                       
         SPACE 1                                                                
VALKEY   GOTO1 VCOLY,APPARM,('OVLYBBLK',0),0,0                                  
         CLI   4(R1),X'FF'                                                      
         BNE   VALKEY10                                                         
         MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         B     VALKEY99                                                         
*                                                                               
VALKEY10 DS    0H                                                               
         L     R1,0(,R1)           GET   LOAD MODULE    ADDRESS                 
         LA    R1,0(,R1)           CLEAR HIGH ORDER     BYTE                    
         ST    R1,AAPGIO           SAVE  LOAD MODULE    ADDRESS                 
         MVI   NEWKEY,NO                                                        
         MVC   APGKEY,SPACES                                                    
         MVI   APGKTYP,APGKTYPQ    X'2D'                                        
         MVI   APGKSUB,APGKSUBQ    X'07'                                        
         MVC   APGKCPY,CUABIN      COMPANY CODE                                 
         MVC   APGKRTY,APREPCDE    FI or M2 or IV                               
         MVC   SAVFORM,SPACES                                                   
         GOTO1 AFVAL,COMCODEH                                                   
         MVC   APGKFMT,FVIFLD      FORMAT                                       
         MVC   SAVFORM,FVIFLD                                                   
         OI    COMCODEH+6,FVOXMT                                                
         MVC   APRECKEY(L'APGKEY),APGKEY                                        
         LA    R1,IORDD+IOACCFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99                                                         
         TM    IOERR,IOERNF        RECORD NOT ON FILE                           
         BO    VALKEY20                                                         
         LA    R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT                                                 
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
         MVI   APINDS,APIOKDIS                                                  
         CLI   APACTN,ACTDIS                                                    
         BE    VALKEY98                                                         
         OI    APINDS,APIOKCHA                                                  
         CLI   APACTN,ACTCHA                                                    
         BE    VALKEY98                                                         
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         CLI   APACTN,ACTRES       RESTORE                                      
         BE    VALKEY98                                                         
         MVI   APINDS,APIOKDIS+APIOKDEL                                         
         CLI   APACTN,ACTDEL       DELETE                                       
         BE    VALKEY98                                                         
         CLI   APACTN,ACTADD       ADD                                          
         BNE   VALKEY99                                                         
         MVC   FVMSGNO,=AL2(ACERECEX)                                           
         B     VALKEY99                                                         
*                                                                               
VALKEY20 MVI   APINDS,APIOKADD                                                  
         MVI   NEWKEY,YES                                                       
         MVC   APDUB,=CL8'AC'                                                   
         MVC   APDUB+2(2),APGKRTY                                               
         MVC   APDUB+4(2),CUAALF                                                
         MVC   APDUB+6(1),APGKFMT                                               
         GOTO1 ALOADER,APPARM,APDUB,AAPGIO,(C'M',AAPGIO)                        
         OC    APPARM+4(4),APPARM+4                                             
         BNZ   VALKEY30                                                         
         MVC   FVMSGNO,=AL2(6)                                                  
         B     VALKEY99                                                         
*                                                                               
VALKEY30 L     R2,AIOAREA1                                                      
         XC    APGKEY(256),APGKEY                                               
         TWAXC COMNMEH,COMTABH                                                  
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY KEY                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         MVC   APGCODE,APGKFMT                                                  
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         MVI   APPFKEY,0                                                        
*                                                                               
DISKEY99 B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY SINGLE REQUEST                            *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                REMOVE HEADLINE ELEMENTS              
         TWAXC COMNMEH,COMTABH                                                  
         OI    COMLDTEH+6,FVOXMT                                                
         MVC   COMLDTE,SPACES                                                   
         OI    COMRDTEH+6,FVOXMT                                                
         MVC   COMRDTE,SPACES                                                   
         OI    COMLDGRH+6,FVOXMT                                                
         MVC   COMLDGR,SPACES                                                   
         OI    COMSLDGH+6,FVOXMT                                                
         MVC   COMSLDG,SPACES                                                   
         OI    COMOPT1H+6,FVOXMT                                                
         MVC   COMOPT1,SPACES                                                   
         OI    COMOPT3H+6,FVOXMT                                                
         MVC   COMOPT3,SPACES                                                   
         OI    COMOPT4H+6,FVOXMT                                                
         MVC   COMOPT4,SPACES                                                   
         OI    COMBUD1H+6,FVOXMT                                                
         MVC   COMBUD1,SPACES                                                   
         MVI   REPNAME,YES                                                      
         GOTO1 GETNAME,APPARM,(R2),COMNMEH                                      
         GOTO1 GETPER,APPARM,(R2),COMOWNH                                       
         CLC   COMNME,SPACES                                                    
         BH    *+8                                                              
         MVI   REPNAME,NO                                                       
*                                                                               
         MVC   APDUB,=CL8'AC'                                                   
         MVC   APDUB+2(2),APGKRTY                                               
         MVC   APDUB+4(2),CUAALF                                                
         MVC   APDUB+6(1),APGKFMT                                               
         GOTO1 ALOADER,APPARM,APDUB,AAPGIO,(C'M',AAPGIO)                        
         OC    APPARM+4(4),APPARM+4                                             
         BNZ   DISREC15                                                         
         MVC   FVMSGNO,=AL2(6)                                                  
         B     DISREC99                                                         
*                                                                               
         USING SPECD,R2                                                         
*                                                                               
DISREC15 L     R3,AAPGIO                                                        
         MVI   LDGR#,0                                                          
         MVC   LDGRLIST,SPACES                                                  
         XC    BUDGLIST(32),BUDGLIST                                            
         XC    OPT1LIST,OPT1LIST                                                
         MVI   OPT1#,0                                                          
         XC    OPT3LIST,OPT3LIST                                                
         MVI   OPT3#,0                                                          
         XC    OPT4LIST,OPT4LIST                                                
         MVI   OPT4#,0                                                          
*                                                                               
DISREC20 SR    R1,R1                                                            
         CLI   0(R3),X'FF'                                                      
         BE    DISREC45                                                         
         LA    R2,SPECTAB                                                       
*                                                                               
DISREC24 CLI   0(R2),EOT                                                        
         BNE   DISREC26                                                         
         IC    R1,1(,R3)           LENGTH OF ELEMENT                            
         AR    R3,R1                                                            
         B     DISREC20                                                         
*                                                                               
DISREC26 CLC   SPECNUM,0(R3)       MATCH ON COMMAND                             
         BE    DISREC30                                                         
         LA    R2,SPECLNQ(,R2)                                                  
         B     DISREC24                                                         
*                                                                               
DISREC30 LH    R4,SPECFLDH         FIELD TO DISPLAY                             
         A     R4,ATWA                                                          
         GOTO1 AFVAL,(R4)                                                       
         LA    RE,DISREC40         RETURN ADDRESS                               
         SR    RF,RF                                                            
         IC    RF,SPECROUT                                                      
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     DREPNAME            DISPLAY REPORT NAME                          
         B     DREPLDGR            DISPLAY REPORT LEDGERS                       
         B     DSUPLDGR            DISPLAY SUPERLEDGER                          
         B     DBUDGETS            DISPLAY BUDGETS                              
         B     DOPTIONS            DISPLAY OPTIONS                              
*                                                                               
         DROP  R2                                                               
*                                                                               
DISREC40 SR    R1,R1               LENGTH OF ELEMENT                            
         IC    R1,1(,R3)           LENGTH OF ELEMENT                            
         AR    R3,R1                                                            
         B     DISREC20                                                         
*                                                                               
         USING TFDELD,R3                                                        
*                                                                               
DISREC45 L     R3,AAPGIO                  DISPLAY "DISPLAY LINES"               
         BAS   RE,DISLDGRS                                                      
         BAS   RE,DISBDGTS                                                      
         BAS   RE,DISOPTS                                                       
         L     R2,AIOAREA1                DISPLAY "DISPLAY LINES"               
         MVI   APELCODE,TFDELQ     X'B0'  TEXT FIELD                            
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC60                                                         
*                                                                               
DISREC50 LR    R3,R1                                                            
         LA    R4,COMQ1RDH         OPTION ONE DISCRIPTION                       
         CLI   TFDSEQ,1            FIRST ONE                                    
         BE    DISREC55                                                         
         LA    R4,COMQ3RDH         OPTION THREE DISCRIPTION                     
         CLI   TFDSEQ,3            THIRD ONE                                    
         BE    DISREC55                                                         
         LA    R4,COMQ4RDH         OPTION FOUR DISCRIPTION                      
         CLI   TFDSEQ,4            FOURTH ONE                                   
         BE    DISREC55                                                         
         LA    R4,COMSELDH         SELECT FIELD DISCRIPTION                     
         CLI   TFDSEQ,10                                                        
         BNE   DISREC60                                                         
*                                                                               
DISREC55 SR    RF,RF                                                            
         IC    RF,TFDLN                                                         
         SH    RF,=Y(TFDTEXT-TFDELD)                                            
         SR    R1,R1                                                            
         IC    R1,0(,R4)           LENGTH OF FIELD                              
         SH    R1,=H'08'                                                        
         CR    RF,R1                                                            
         BNH   *+6                                                              
         LR    RF,R1                                                            
         BCTR  RF,0                SUBTRACT ONE                                 
         EXMVC RF,8(R4),TFDTEXT                                                 
         GOTO1 NEXTEL,(R3)                                                      
         BE    DISREC50                                                         
*                                                                               
         USING PACELD,R3                                                        
*                                                                               
DISREC60 L     R2,AIOAREA1         DISPLAY DATES                                
         MVI   APELCODE,PACELQ     X'A1' PERSON ACTIVITY                        
         GOTO1 GETEL,(R2)                                                       
         LR    R3,R1                                                            
         GOTO1 VDATCON,APPARM,(1,PACDATE),(17,COMLDTE)                          
*                                                                               
         USING DTSELD,R3                                                        
*                                                                               
         MVI   APELCODE,DTSELQ     X'FB' DATE/TIME STAMP                        
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC65                                                         
         LR    R3,R1                                                            
         GOTO1 VDATCON,APPARM,(2,DTSDATE),(17,COMRDTE)                          
*                                                                               
         USING COMMD,R4                                                         
         USING SCMELD,R3                                                        
*                                                                               
DISREC65 L     R2,AIOAREA1                DISPLAY COMMENTS                      
         MVI   APELCODE,SCMELQ     X'3E'  COMMENT                               
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC95                                                         
*                                                                               
DISREC70 LA    R4,COMLINEH                                                      
         LR    R3,R1                                                            
         SR    R1,R1                                                            
         IC    R1,SCMSEQ                                                        
         SH    R1,=H'01'                                                        
         BZ    *+8                                                              
         MH    R1,=Y(COMMLNQ)                                                   
         AR    R4,R1                                                            
         SR    R1,R1                                                            
         IC    R1,SCMLN            LENGTH OF ELEMENT                            
         SH    R1,=Y(SCMLN1Q+1)                                                 
         EXMVC R1,COMMENT,SCMNARR                                               
         GOTO1 NEXTEL,(R3)                                                      
         BE    DISREC70                                                         
*                                                                               
DISREC95 DS    0H                                                               
*                                                                               
DISREC99 B     EXIT                                                             
*                                                                               
         DROP  R3,R4                                                            
         EJECT ,                                                                
         SPACE 1                                                                
DREPNAME CLI   REPNAME,YES                                                      
         BER   RE                  DON'T BOTHER                                 
         MVI   REPNAME,YES                                                      
         OI    6(R4),FVOXMT                                                     
         SR    R1,R1                                                            
         IC    R1,1(,R3)           GET LENGTH OF DATA                           
         SH    R1,=H'03'                                                        
         SR    RF,RF                                                            
         IC    RF,0(,R4)           FIELD HEADER + FIELD LENGTH                  
         SH    RF,=H'09'                                                        
         CR    RF,R1                                                            
         BH    *+6                                                              
         LR    R1,RF                                                            
         EXMVC R1,8(R4),2(R3)                                                   
         LA    R1,1(,R1)           GET  NAME LENGTH                             
         STC   R1,5(,R4)           SAVE IN   HEADER                             
         BR    RE                                                               
         EJECT ,                                                                
         SPACE 1                                                                
DREPLDGR DS    0H                                                               
         ST    RE,SAVERE           SAVE RETURN REGISTER                         
         SR    R1,R1                                                            
         IC    R1,1(,R3)           LENGTH OF DATA                               
         SH    R1,=H'02'                                                        
         LA    RE,2(,R3)                                                        
         CLI   0(R3),CMREAD                                                     
         BE    DREPLD20                                                         
         BCTR  R1,0                SUBTRACT ONE FOR COMPANY HEX CODE            
         LA    RE,3(,R3)                                                        
*                                                                               
DREPLD20 LA    R6,LDGRLIST                                                      
         SR    RF,RF                                                            
         ICM   RF,1,LDGR#          NUMBER OF LEDGERS SO FAR                     
         BZ    DREPLD30                                                         
*                                                                               
DREPLD25 CLC   0(2,RE),0(R6)                                                    
         BE    DREPLD35            ALREADY HAVE IT                              
         LA    R6,3(,R6)           NEXT LEDGER                                  
         BCT   RF,DREPLD25                                                      
*                                                                               
DREPLD30 IC    RF,LDGR#                                                         
         LA    RF,1(,RF)                                                        
         STC   RF,LDGR#            INCREMENT LEDGER COUNT                       
         MVC   0(3,R6),0(RE)       SAVE OFF LEDGER                              
*                                                                               
DREPLD35 LA    RE,3(,RE)                                                        
         SH    R1,=H'03'                                                        
         BNZ   DREPLD20                                                         
         L     RE,SAVERE           RESTORE RETURN REGISTER                      
         BR    RE                  FINISHED FOR NOW                             
         EJECT ,                                                                
         SPACE 1                                                                
DSUPLDGR MVC   COMSLDG,SPACES                                                   
         MVC   COMSLDG,2(R3)                                                    
         BR    RE                  FINISHED FOR NOW                             
         EJECT ,                                                                
         SPACE 1                                                                
DBUDGETS DS    0H                                                               
         ST    RE,SAVERE           SAVE RETURN REGISTER                         
         SR    R6,R6                                                            
         IC    R6,2(,R3)           BUDGET TO PROCESS                            
         BCTR  R6,0                                                             
         MH    R6,=H'02'                                                        
         LA    R6,BUDGLIST(R6)                                                  
         OC    3(2,R3),3(R3)       ANY BUDGET NUMBER?                           
         BZ    DBUDG20             GET BY BUDGET NAME                           
         MVC   0(2,R6),3(R3)       SAVE OFF BUDGET NUMBER                       
         B     DBUDG80                                                          
*                                                                               
DBUDG20  SR    RF,RF                                                            
         IC    RF,1(,R3)           LENGTH OF ELEMENT                            
         SH    RF,=H'06'                                                        
         BM    DBUDG80                                                          
         MVC   BUDCDE,SPACES       CLEAR                                        
         EXMVC RF,BUDCDE,5(R3)     MOVE IN BUDGET NAME                          
         GOTO1 BUDCODE,(R6)        GET BUDGET CODE                              
*                                                                               
DBUDG80  DS    0H                                                               
         L     RE,SAVERE           RESTORE RETURN REGISTER                      
         BR    RE                  FINISHED FOR NOW                             
         EJECT ,                                                                
         SPACE 1                                                                
DOPTIONS DS    0H                                                               
         ST    RE,SAVERE           SAVE RETURN REGISTER                         
         SR    R1,R1                                                            
         LA    R1,OPT1#                                                         
         LA    R2,OPT1LIST         OPTION 1                                     
         CLI   3(R3),FRQ1                                                       
         BE    DOPT10                                                           
         LA    R1,OPT3#                                                         
         LA    R2,OPT3LIST         OPTION 3                                     
         CLI   3(R3),FRQ3                                                       
         BE    DOPT10                                                           
         LA    R1,OPT4#                                                         
         LA    R2,OPT4LIST         OPTION 3                                     
         CLI   3(R3),FRQ4                                                       
         BNE   DOPT90                                                           
*                                                                               
DOPT10   MVC   APBYTE,7(R3)        GET OPTION VALUE                             
         CLI   1(R3),8                                                          
         BNH   *+8                                                              
         MVI   APBYTE,C' '                                                      
         SR    RF,RF                                                            
         ICM   RF,1,0(R1)          NUMBER OF OPTIONS SO FAR                     
         BZ    DOPT80                                                           
         LR    RE,R2                                                            
         AR    R2,RF               POINT TO FINAL PLACE                         
*                                                                               
DOPT20   DS    0H                                                               
         CLC   APBYTE,0(RE)                                                     
         BE    DOPT90              ALREADY HAVE IT                              
         LA    RE,1(,RE)                                                        
         BCT   RF,DOPT20                                                        
*                                                                               
DOPT80   MVC   0(1,R2),APBYTE                                                   
         IC    RF,0(,R1)                                                        
         LA    RF,1(,RF)                                                        
         STC   RF,0(,R1)           NEW NUMBER OF OPTIONS                        
*                                                                               
DOPT90   DS    0H                                                               
         L     RE,SAVERE           RESTORE RETURN REGISTER                      
         BR    RE                  FINISHED FOR NOW                             
         EJECT                                                                  
         SPACE 1                                                                
DISOPTS  NTR1                                                                   
         LA    R4,APELEM                                                        
         XC    APELEM,APELEM                                                    
         SR    RF,RF                                                            
         ICM   RF,1,OPT1#                                                       
         BZ    DISOPT30                                                         
         LA    R3,OPT1LIST                                                      
         BAS   RE,BLDOPT                                                        
         GOTO1 DISPIT,APPARM,COMOPT1H                                           
*                                                                               
DISOPT30 LA    R4,APELEM                                                        
         XC    APELEM,APELEM                                                    
         SR    RF,RF                                                            
         ICM   RF,1,OPT3#                                                       
         BZ    DISOPT40                                                         
         LA    R3,OPT3LIST                                                      
         BAS   RE,BLDOPT                                                        
         GOTO1 DISPIT,APPARM,COMOPT3H                                           
*                                                                               
DISOPT40 LA    R4,APELEM                                                        
         XC    APELEM,APELEM                                                    
         SR    RF,RF                                                            
         ICM   RF,1,OPT4#                                                       
         BZ    DISOPTX                                                          
         LA    R3,OPT4LIST                                                      
         BAS   RE,BLDOPT                                                        
         GOTO1 DISPIT,APPARM,COMOPT4H                                           
*                                                                               
DISOPTX  B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
BLDOPT   MVC   0(1,R4),0(R3)                                                    
         CLI   0(R3),C' '                                                       
         BNE   BLDOPT10                                                         
         MVC   0(6,R4),=C'SPACE'                                                
         LA    R4,4(,R4)                                                        
*                                                                               
BLDOPT10 DS    0H                                                               
         LA    R4,1(,R4)                                                        
         MVC   0(1,R4),SCCOMMA                                                  
         LA    R4,1(,R4)                                                        
         LA    R3,1(,R3)                                                        
         BCT   RF,BLDOPT                                                        
         BR    RE                                                               
         EJECT                                                                  
         SPACE 1                                                                
DISBDGTS NTR1                                                                   
         LA    R4,APELEM                                                        
         XC    APELEM,APELEM                                                    
         OC    BUDGLIST(32),BUDGLIST                                            
         BZ    DISBDGX                                                          
         LA    R6,1                                                             
         LA    R3,BUDGLIST                                                      
*                                                                               
DISBDG10 OC    0(2,R3),0(R3)       ANY BUDGET?                                  
         BNZ   DISBDG20                                                         
*                                                                               
DISBDG15 LA    R6,1(,R6)           NEXT BUDGET                                  
         LA    R3,2(,R3)           NEXT ONE                                     
         CH    R6,=H'16'                                                        
         BNH   DISBDG10                                                         
         B     DISBDGX                                                          
*                                                                               
DISBDG20 MVC   0(3,R4),=C'BUD'                                                  
         LA    R4,3(,R4)           BUMP TO NEXT LOCATION                        
         CVD   R6,APDUB                                                         
         OI    APDUB+7,X'0F'       CHANGE SIGN TO X'0F'                         
         UNPK  0(2,R4),APDUB                                                    
         CLI   0(R4),C'0'                                                       
         BNE   DISBDG22                                                         
         MVC   0(1,R4),1(R4)       MOVE OVER, PLEASE                            
         BCTR  R4,0                BACK OUT ONE                                 
*                                                                               
DISBDG22 DS    0H                                                               
         LA    R4,2(,R4)                                                        
         MVC   0(2,R4),=C'='                                                    
         LA    R4,1(,R4)                                                        
         GOTO1 BUDNAME,(R3)                                                     
         LA    R1,L'BUDCDE                                                      
         LA    RE,BUDCDE+L'BUDCDE-1                                             
*                                                                               
DISBDG24 DS    0H                                                               
         CLI   0(RE),C' '                                                       
         BH    DISBDG30                                                         
         BCTR  RE,0                                                             
         BCT   R1,DISBDG24                                                      
         B     DISBDG40                                                         
*                                                                               
DISBDG30 BCTR  R1,0                                                             
         EXMVC R1,0(R4),BUDCDE                                                  
         LA    R4,1(R1,R4)                                                      
*                                                                               
DISBDG40 SR    RE,RE                                                            
         ICM   RE,3,0(R3)                                                       
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         MVC   0(4,R4),=C'(000'                                                 
         UNPK  1(3,R4),APDUB                                                    
         CLI   1(R4),C'0'                                                       
         BNE   DISBDG42                                                         
         MVC   1(2,R4),2(R4)       ELIMINATE A CHAR C'0'                        
         BCTR  R4,0                                                             
*                                                                               
DISBDG42 DS    0H                                                               
         LA    R4,4(,R4)                                                        
         MVC   0(2,R4),=C'),'                                                   
         LA    R4,2(,R4)                                                        
         B     DISBDG15                                                         
*                                                                               
DISBDGX  GOTO1 DISPIT,APPARM,COMBUD1H                                           
         B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
DISLDGRS NTR1                                                                   
         SR    RF,RF                                                            
         CLI   LDGR#,5             CHECK FOR MAX OF 5 LEDGERS                   
         BNH   *+8                                                              
         MVI   LDGR#,5                                                          
         ICM   RF,1,LDGR#          NUMBER OF FIELDS                             
         BZ    DISLDGRX                                                         
         OI    COMLDGRH+6,FVOXMT   TRANSMIT                                     
         MVC   COMLDGR,SPACES      CLEAR                                        
         LA    R3,LDGRLIST                                                      
         LA    R4,COMLDGR          FIELD                                        
*                                                                               
DISLDG10 MVC   0(3,R4),0(R3)                                                    
         LA    R3,3(,R3)                                                        
         LA    R4,3(,R4)                                                        
         SH    RF,=H'01'                                                        
         BZ    DISLDGRX                                                         
         MVC   0(1,R4),SCCOMMA     FILL IN COMMAS                               
         LA    R4,1(,R4)                                                        
         B     DISLDG10                                                         
*                                                                               
DISLDGRX B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
DISPIT   SR    RF,RF                                                            
         L     R6,0(,R1)           FIELD HEADER ADDRESS                         
         IC    RF,0(,R6)                                                        
         SH    RF,=H'09'                                                        
         LA    R2,APELEM                                                        
         CR    R4,R2               R4 IS AT THE LAST CHAR IN APELEM             
         BE    XIT                                                              
         BCTR  R4,0                SUBTRACT ONE                                 
         MVI   0(R4),C' '          WIPE OUT COMMA                               
         SR    R4,R2                                                            
         BCTR  R4,0                SUBTRACT ONE FOR EXECUTE MOVE                
         CR    R4,RF                                                            
         BNH   *+6                                                              
         LR    R4,RF                                                            
         EXMVC R4,8(R6),APELEM                                                  
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE SINGLE REQUEST                           *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1         REMOVE HEADLINE ELEMENTS                     
         GOTO1 AFVAL,COMNMEH                                                    
         BE    VALREC01            NAME HAS BEEN INPUT, SKIP                    
         BAS   RE,VREPNAME         FIND NAME FROM RECORD                        
         CLC   COMNME,SPACES       NAME FOUND ?                                 
         BE    VALREC03            NO,  SKIP                                    
*                                                                               
VALREC01 DS    0H                                                               
         GOTO1 ADDNAME,APPARM,(R2),COMNMEH                                      
         BNE   VALREC99            ON ERROR, EXIT                               
         BAS   RE,DISLST           UPDATE LIST NAME                             
*                                                                               
VALREC03 MVC   APGKEY,APRECKEY                                                  
         BNE   VALREC09            NOTHING TO DELETE                            
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
*                                                                               
         USING SCMELD,R3                                                        
*                                                                               
VALREC04 CLI   0(R3),0             EOR                                          
         BE    VALREC06                                                         
         CLI   SCMEL,SCMELQ                                                     
         BNE   *+8                 NO, SO DON'T DELETE                          
         MVI   SCMEL,X'FF'         MANUALLY MARK FOR DELETION                   
*                                                                               
         USING TFDELD,R3                                                        
*                                                                               
         CLI   TFDEL,TFDELQ                                                     
         BNE   *+8                 NO, SO DON'T DELETE                          
         MVI   TFDEL,X'FF'         MANUALLY MARK FOR DELETION                   
         SR    R1,R1                                                            
         IC    R1,1(,R3)          NEXT ELEMENT                                  
         AR    R3,R1                                                            
         B     VALREC04                                                         
*                                                                               
VALREC06 MVI   APELCODE,X'FF'      DELETE MARKED HEADLINE ELEMENTS              
         GOTO1 DELEL,(R2)                                                       
         BNE   VALREC99                                                         
*                                                                               
         DROP  R3                                                               
*                                                                               
VALREC09 CLI   NEWKEY,YES          ADD ONLY                                     
         BNE   VALREC10                                                         
         MVC   APREPNUM,APREPUL+1  RESET DEFAULT LEDGER                         
         GOTO1 ADDREPTY,APPARM,AIOAREA1                                         
         BNE   VALREC99            ON ERROR, EXIT                               
*                                                                               
         USING TFDELD,R3                                                        
*                                                                               
VALREC10 SR    R6,R6                                                            
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   TFDEL,TFDELQ        X'B0' TEXT FIELD                             
         MVI   TFDTYPE,TFDTREG     REGULAR TYPE                                 
         GOTO1 AFVAL,COMQ1RDH                                                   
         BNE   VALREC12                                                         
         IC    R6,FVXLEN           LENGTH OF EXMVC                              
         EXMVC R6,TFDTEXT,FVIFLD                                                
         AH    R6,=Y(TFDTEXT-TFDELD+1)                                          
         STC   R6,TFDLN            SAVE LENGTH                                  
         MVI   TFDSEQ,1                                                         
         GOTO1 ADDEL,(R2)                                                       
         BNE   VALREC99            ON ERROR, EXIT                               
*                                                                               
VALREC12 GOTO1 AFVAL,COMQ3RDH                                                   
         BNE   VALREC14                                                         
         IC    R6,FVXLEN           LENGTH OF EXMVC                              
         EXMVC R6,TFDTEXT,FVIFLD                                                
         AH    R6,=Y(TFDTEXT-TFDELD+1)                                          
         STC   R6,TFDLN            SAVE LENGTH                                  
         MVI   TFDSEQ,3                                                         
         GOTO1 ADDEL,(R2)                                                       
         BNE   VALREC99            ON ERROR, EXIT                               
*                                                                               
VALREC14 GOTO1 AFVAL,COMQ4RDH                                                   
         BNE   VALREC16                                                         
         IC    R6,FVXLEN           LENGTH OF EXMVC                              
         EXMVC R6,TFDTEXT,FVIFLD                                                
         AH    R6,=Y(TFDTEXT-TFDELD+1)                                          
         STC   R6,TFDLN            SAVE LENGTH                                  
         MVI   TFDSEQ,4                                                         
         GOTO1 ADDEL,(R2)                                                       
         BNE   VALREC99            ON ERROR, EXIT                               
*                                                                               
VALREC16 GOTO1 AFVAL,COMSELDH                                                   
         BNE   VALREC20                                                         
         IC    R6,FVXLEN           LENGTH OF EXMVC                              
         EXMVC R6,TFDTEXT,FVIFLD                                                
         AH    R6,=Y(TFDTEXT-TFDELD+1)                                          
         STC   R6,TFDLN            SAVE LENGTH                                  
         MVI   TFDSEQ,10                                                        
         GOTO1 ADDEL,(R2)                                                       
         BNE   VALREC99            ON ERROR, EXIT                               
*                                                                               
VALREC20 DS    0H                                                               
*                                                                               
         USING SCMELD,R3                                                        
         USING COMMD,R4                                                         
*                                                                               
VALREC60 LA    R6,1                UP TO 6 COMMENT FOR NOW                      
         LA    R3,APELEM                                                        
         LA    R4,COMLINEH         FIRST COMMENT LINE                           
*                                                                               
VALREC62 GOTO1 AFVAL,COMMENTH                                                   
         BNE   VALREC68            NO INPUT                                     
         XC    APELEM,APELEM                                                    
         MVI   SCMEL,SCMELQ        X'3D' COMMENT                                
         STC   R6,SCMSEQ           COMMENT LINE #                               
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EXMVC R1,SCMNARR,FVIFLD                                                
         LA    R1,SCMLN1Q+1(,R1)                                                
         STC   R1,SCMLN            SAVE LENGTH                                  
         L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VALREC99            ON ERROR, EXIT                               
*                                                                               
VALREC68 AH    R4,=Y(COMMLNQ)      BUMP TO NEXT LINE                            
         LA    R6,1(,R6)                                                        
         CH    R6,=H'06'                                                        
         BNH   VALREC62                                                         
*                                                                               
VALREC95 DS    0H                                                               
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VALREC99            ON ERROR, EXIT                               
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD     ADDING A RECORD?                             
         BO    VALREC97                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGING A RECORD?                           
         BO    VALREC97                                                         
         DC    H'0'                                                             
*                                                                               
VALREC97 GOTO1 AIO                                                              
         BE    VALREC98                                                         
         TM    IOERR,IOEDUP        DELETED RECORD (DUPLICATE) ON FILE           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFCCDR)                                            
*                                                                               
VALREC98 DS    0H                                                               
         CLI   APACTN,ACTDEL       DELETE ONLY                                  
         BE    DISREC                                                           
*                                                                               
VALREC99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                                                             
         TM    TWAMODE,TWAMDFR     2ND PASS IN ACTION COPY?                     
         BZ    DISREC                                                           
         CLI   APMODE,APMNEWK      ACTION COPY?                                 
         BNE   DISREC              NO                                           
         CLI   NEWKEY,YES          IS IT REALY A NEW KEY?                       
         BNE   IVALIPUT            NO UPDATE RECORD WITH NEW HEADINGS           
         B     EXIT                ADD NEW RECORD WITH HEADING ELEMENTS         
*                                                                               
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
*  FIND NAME FROM RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
         SPACE 1                                                                
VREPNAME NTR1                                                                   
         MVI   REPNAME,NO          NAME NOT  YET FOUND                          
*                                                                               
         LA    R2,IOKEY            IO   KEY                                     
         MVC   APDUB,=CL8'AC'                                                   
         MVC   APDUB+2(2),APGKRTY  FI   or   M2   or   IV                       
         MVC   APDUB+4(2),CUAALF                                                
         MVC   APDUB+6(1),APGKFMT  FORMAT                                       
         GOTO1 ALOADER,APPARM,APDUB,AAPGIO,(C'M',AAPGIO)                        
         OC    APPARM+4(4),APPARM+4                                             
         BZ    VREPNAMX            NOT  FOUND,    EXIT                          
*                                                                               
         L     R3,AAPGIO                                                        
*                                                                               
VREP20   DS    0H                                                               
         SR    R1,R1                                                            
         CLI   0(R3),X'FF'         END  OF   RECORD ?                           
         BE    VREPNAMX            YES, EXIT                                    
         LA    R2,SPECTAB                                                       
*                                                                               
         USING SPECD,R2                                                         
*                                                                               
VREP25   DS    0H                                                               
         CLI   0(R2),EOT           END  OF   TABLE ?                            
         BE    VREP40              YES, GET  NEXT RECORD    ELEMENT             
*                                                                               
         CLC   SPECNUM,0(R3)       MATCH     ON   COMMAND ?                     
         BE    VREP30              YES, CONTINUE                                
         LA    R2,SPECLNQ(,R2)     NO,  NEXT      TABLE     ENTRY               
         B     VREP25                                                           
*                                                                               
VREP30   DS    0H                                                               
         CLI   SPECROUT,RTREPNM    NAME ROUTINE ?                               
         BNE   VREP40              NO,  CONTINUE  SEARCH                        
         LA    R4,COMNMEH          FIELD     HEADER                             
         BAS   RE,DREPNAME         CALL DISPLAY   ROUTINE                       
         B     VREPNAMX            FINISHED                                     
*                                                                               
         DROP  R2                                                               
*                                                                               
VREP40   DS    0H                                                               
         SR    R1,R1               GET  NEXT RECORD    ELEMENT                  
         IC    R1,1(,R3)                                                        
         AR    R3,R1                                                            
         B     VREP20                                                           
*                                                                               
VREPNAMX DS    0H                  EXIT                                         
         B     XIT                 RETURN                                       
         EJECT                                                                  
         SPACE 1                                                                
         USING BUDRECD,R2                                                       
         SPACE 1                                                                
BUDNAME  NTR1                                                                   
         MVC   BUDCDE,SPACES                                                    
         SR    R3,R3                                                            
         ICM   R3,3,0(R1)          GET BUDGET NUMBER                            
         BZ    BUDNAMEX                                                         
         LA    R2,IOKEY                                                         
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN      COMPANY CODE                                 
         STCM  R3,3,BUDKNO1                                                     
         GOTO1 AIO,IO2+IOACCDIR+IOHI                                            
*        BNE   BUDNAMEX                                                         
         CLM   R3,3,BUDKNO1                                                     
         BNE   BUDNAMEX                                                         
         MVC   BUDCDE,BUDKCOD                                                   
*                                                                               
BUDNAMEX B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
         SPACE 1                                                                
         USING BUDRECD,R2                                                       
         SPACE 1                                                                
BUDCODE  NTR1                                                                   
         LA    R2,IOKEY                                                         
         LR    R3,R1                                                            
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN      COMPANY CODE                                 
         MVC   BUDKCOD,BUDCDE                                                   
         GOTO1 AIO,IO2+IOACCDIR+IOHI                                            
*        BNE   BUDCODEX                                                         
         CLC   BUDKCOD,BUDCDE                                                   
         BNE   BUDCODEX                                                         
         MVC   0(2,R3),BUDKNO2                                                  
*                                                                               
BUDCODEX B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY SELECT RECORD                             *         
*        ENTER FROM --> REPORT LIST                                   *         
*              VALIDATE ORIENTATION LANDSCAPRE/PORTRIAT               *         
*              VALIDATE OUTPUT                                        *         
*              VALIDATE DESTINATION                                   *         
*              VALIDATE MOA RANGE                                     *         
*              VALIDATE START DATE                                    *         
*              VALIDATE END DATE                                      *         
*              REQUESTOR                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
         SPACE 1                                                                
VALSEL   MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   APRECKEY,SPACES                                                  
         CLC   FLTFORM,SPACES                                                   
         BE    VALSEL10                                                         
         MVC   APGCODE,FLTFORM                                                  
         OI    APGCODEH+6,FVOXMT                                                
         MVC   FLTFORM,SPACES                                                   
*                                                                               
VALSEL10 LA    R0,APGSEL1H                                                      
         ST    R0,APPARM                                                        
         LA    R1,APGSEL2H                                                      
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,15         ONLY 15 LINES OF SELECT                      
*                                                                               
*        LA    R2,APRECKEY                                                      
*        MVC   APGKEY,SPACES                                                    
*        MVI   APGKTYP,APGKTYPQ    X'2D'                                        
*        MVI   APGKSUB,APGKSUBQ    X'07'                                        
*        MVC   APGKCPY,CUABIN      COMPANY CODE                                 
*                                                                               
VALSEL90 B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO GET SELECT RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
GETSEL   LA    R2,IOKEY                                                         
         MVC   APGKEY,APRECKEY                                                  
         CLI   APINDS,APILFLST                                                  
         BE    GETSEL05                                                         
         CLI   APINDS,APILNLST                                                  
         BNE   *+10                                                             
*                                                                               
GETSEL05 MVC   SAVLSTKY,APRECKEY   SAVE CURRENT KEY                             
         MVC   APGKEY,APRECKEY                                                  
         TM    SCRTYPH+4,FVITHIS   SAME AS BEFORE?                              
         BNZ   GETSEL08            NO, SO RESET                                 
         TM    APGCODEH+4,FVITHIS                                               
         BNZ   GETSEL08            NO, SO RESET                                 
         TM    APGOWNH+4,FVITHIS                                                
         BZ    *+8                                                              
*                                                                               
GETSEL08 MVI   APGKEY,0            NO, RESET TO START OVER                      
         NI    SCRTYPH+4,TURNOFF-FVITHIS                                        
         NI    APGCODEH+4,TURNOFF-FVITHIS                                       
         NI    APGOWNH+4,TURNOFF-FVITHIS                                        
*                                                                               
         CLI   APGKEY,APGKTYPQ     X'2D'                                        
         BNE   GETSEL10                                                         
         CLI   APGKSUB,APGKSUBQ    X'07'                                        
         BNE   GETSEL10                                                         
         CLC   APGKCPY,CUABIN      COMPANY CODE                                 
         BE    GETSEL20                                                         
*                                                                               
GETSEL10 MVC   APGKEY,SPACES                                                    
         MVI   APGKTYP,APGKTYPQ    X'2D'                                        
         MVI   APGKSUB,APGKSUBQ    X'07'                                        
         MVC   APGKCPY,CUABIN      COMPANY CODE                                 
         B     GETSEL60                                                         
*                                                                               
GETSEL20 TM    APINDS,APILRERD     RE-READ RECORD, IO OCCURED                   
         BZ    GETSEL40                                                         
         GOTO1 AIO,IORDD+IOACCFIL+IO1                                           
         BE    GETSEL80                                                         
         TM    IOERR,IOEDEL                                                     
         BNZ   GETSEL80                                                         
         B     GETSEL95                                                         
*                                                                               
GETSEL40 TM    APINDS,APILNSEQ     CONTINUE WITH SEQUENCIAL READ?               
         BNZ   GETSEL80                                                         
*                                                                               
GETSEL60 LA    R1,IOHID+IOACCFIL+IO1                                            
         B     *+8                                                              
*                                                                               
GETSEL80 LA    R1,IOSQD+IOACCFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    GETSEL81                                                         
         TM    IOERR,IOEDEL                                                     
         BZ    GETSEL95                                                         
*                                                                               
GETSEL81 L     R2,AIOAREA1                                                      
         CLI   APGKEY,APGKTYPQ      X'2D'                                       
         BNE   GETSEL95                                                         
         CLI   APGKSUB,APGKSUBQ     X'07'                                       
         BNE   GETSEL95                                                         
         CLC   APGKCPY,CUABIN       COMPANY                                     
         BNE   GETSEL95                                                         
***********************************************************************         
*  REPORT TYPE FILTER                                                 *         
***********************************************************************         
         SPACE 1                                                                
         L     R6,AIOAREA1                                                      
         MVI   APELCODE,FFNELQ                                                  
         GOTO1 GETEL,(R6)                                                       
         BE    GETSEL82                                                         
         CLC   APREPCDE,AC@RCV                                                  
         BNE   GETSEL80                                                         
         B     GETSEL84                                                         
*                                                                               
GETSEL82 CLC   APREPCDE,FFNUMBER-FFNELD+1(R1)                                   
         BNE   GETSEL80                                                         
***********************************************************************         
*  FILTER FORMAT                                                      *         
***********************************************************************         
         SPACE 1                                                                
GETSEL84 GOTO1 AFVAL,APGCODEH                                                   
         BNE   GETSEL85            NO INPUT IN OPTION                           
         SR    R6,R6                                                            
         IC    R6,FVXLEN                                                        
         EXCLC R6,APGKFMT,FVIFLD                                                
         BNE   GETSEL80                                                         
***********************************************************************         
*  FILTER PERSON                                                      *         
***********************************************************************         
         SPACE 1                                                                
GETSEL85 GOTO1 AFVAL,APGOWNH                                                    
         BNE   GETSEL90            NO INPUT IN OPTION                           
         SR    R6,R6                                                            
         IC    R6,FVXLEN                                                        
         GOTO1 GETPER,APPARM,(R2),(L'TEMPOWN,TEMPOWN)                           
         BNE   GETSEL80                                                         
         EXCLC R6,TEMPOWN,FVIFLD                                                
         BNE   GETSEL80                                                         
*                                                                               
GETSEL90 MVC   APRECKEY(L'APGKEY),APGKEY                                        
         B     GETSEL99                                                         
*                                                                               
GETSEL95 MVI   APMODE,APMEOFS      END OF SELECT RECORDS                        
*                                                                               
GETSEL99 MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY SELECT RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
         USING LISTD,R4                                                         
         SPACE 1                                                                
REDIS    MVI   ACPFCUR,PFKNEXT                                                  
*                                                                               
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM           R4=A(LIST/SELECT LINE)                       
         TWAXC LISTDATH,LISTDATH                                                
         MVC   LISTFMT,APGKFMT                                                  
         MVC   LISTTYP,SCRTYP                                                   
         GOTO1 GETPER,APPARM,(R2),(L'LISTOWN,LISTOWN)                           
         XC    APBYTE,APBYTE                                                    
         TM    INOPT1,INORDTE                                                   
         BZ    *+8                                                              
         MVI   APBYTE,C'R'                                                      
         GOTO1 GETACT,APPARM,(APBYTE,(R2)),(L'LISTADTE,LISTADTE)                
         TM    APGRSTA,X'80'                                                    
         BZ    DISSEL10                                                         
         OI    LISTDATH+1,FVAHIGH                                               
         GOTO1 TEXTGET,APPARM,1600,(L'LISTNME,LISTNME),0                        
         B     EXIT                                                             
*                                                                               
DISSEL10 GOTO1 GETNAME,APPARM,(R2),(L'LISTNME,LISTNME)                          
         B     EXIT                                                             
*                                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO RESTORE RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
RESREC   CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT                                                             
         L     R2,AIOAREA1                                                      
         TM    APGRSTA,X'80'       IS IT DELETED                                
         BNZ   RESREC10                                                         
         MVC   FVMSGNO,=AL2(59)    ALREADY RESTORED                             
         B     RESREC90                                                         
*                                                                               
RESREC10 NI    APGRSTA,TURNOFF-X'80'                                            
         GOTO1 ADDID,APPARM,(R2)                                                
         GOTO1 AIO,IOWRITE+IOACCFIL+IO1                                         
         BE    RESREC20                                                         
         DC    H'00'                                                            
*                                                                               
RESREC20 DS    0H                                                               
         BAS   RE,DISLST           UPDATE LIST LINE                             
*                                                                               
RESREC90 B     EXIT97                                                           
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO DELETE A RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
DELREC   CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT                                                             
         L     R2,AIOAREA1                                                      
         TM    APGRSTA,X'80'       IS IT DELETED                                
         BZ    DELREC10                                                         
         MVC   FVMSGNO,=AL2(77)    ALREADY DELETED                              
         B     DELREC90                                                         
*                                                                               
DELREC10 OI    APGRSTA,X'80'                                                    
         GOTO1 ADDID,APPARM,(R2)                                                
         GOTO1 AIO,IOWRITE+IOACCFIL+IO1                                         
         BE    DELREC20                                                         
         DC    H'00'                                                            
*                                                                               
DELREC20 DS    0H                                                               
         BAS   RE,DISLST           UPDATE LIST LINE                             
*                                                                               
DELREC90 B     EXIT97                                                           
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*  UPDATE LIST DATA LINE                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING LISTD,R4                                                         
         USING LSMD,R6                                                          
DISLST   NTR1                                                                   
         L     R2,AIOAREA1                                                      
         TM    TWAMODE,TWAMLSM                                                  
         BZ    DISLST90                                                         
         L     R6,ALSM                                                          
         LA    R4,LSMTWSV                                                       
         AH    R4,LSMTWDSP                                                      
         OI    LISTDATH+1,FVAHIGH                                               
         OI    LISTDATH+6,FVOXMT                                                
         MVC   LISTNME,SPACES                                                   
         CLI   APMODE,APMDELR      MODE DELREC ?                                
         BE    DISLST10            YES, SKIP                                    
         GOTO1 GETNAME,APPARM,(R2),(L'LISTNME,LISTNME)                          
         B     DISLST20                                                         
*                                                                               
DISLST10 DS    0H                                                               
         GOTO1 TEXTGET,APPARM,1600,(L'LISTNME,LISTNME),0                        
*                                                                               
DISLST20 DS    0H                                                               
         GOTO1 GETPER,APPARM,(R2),(L'LISTOWN,LISTOWN)                           
         XC    APBYTE,APBYTE                                                    
         TM    INOPT1,INORDTE                                                   
         BZ    *+8                                                              
         MVI   APBYTE,C'R'                                                      
         GOTO1 GETACT,APPARM,(APBYTE,(R2)),(L'LISTADTE,LISTADTE)                
*                                                                               
DISLST90 B     XIT                 RETURN                                       
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PROCESS SELECTION FROM LIST                       *         
***********************************************************************         
         SPACE 1                                                                
         USING APGRECD,R2                                                       
         USING LISTD,R4                                                         
PROCLST  L     R4,APLSTADD         CURRENT LIST LINE                            
         MVC   APGKEY,SPACES                                                    
         MVI   APGKTYP,APGKTYPQ    X'2D'                                        
         MVI   APGKSUB,APGKSUBQ    X'07'                                        
         MVC   APGKCPY,CUABIN                                                   
         MVC   APGKRTY,APREPCDE                                                 
         MVC   APGKFMT,LISTFMT                                                  
         L     R2,AIOAREA2                                                      
         GOTO1 AIO,IORDD+IOACCFIL+IO2                                           
         BL    PRCLST99                                                         
         BE    PRCLST20                                                         
         CLI   APACTN,ACTDEL       DELETE?                                      
         BNE   PRCLST20                                                         
         TM    APGRSTA,X'80'                                                    
         BZ    PRCLST80                                                         
         MVC   FVMSGNO,=AL2(77)                                                 
         B     PRCLST99                                                         
*                                                                               
PRCLST20 DS    0H                                                               
         CLI   APACTN,ACTRES       RESTORE?                                     
         BNE   PRCLST99                                                         
         TM    APGRSTA,X'80'                                                    
         BNZ   PRCLST90                                                         
         MVC   FVMSGNO,=AL2(59)                                                 
         B     PRCLST99                                                         
*                                                                               
PRCLST80 DS    0H                                                               
*                                                                               
PRCLST90 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
PRCLST99 B     XIT                 EXIT                                         
*                                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
IVALEREQ MVC   FVMSGNO,=AL2(ACIENREQ)                                           
         MVI   FVOMTYP,GTMINF                                                   
         ST    R2,FVADDR                                                        
         B     EXIT                                                             
         SPACE 1                                                                
IVALOFF  MVC   FVMSGNO,=AL2(ACEIVOF)                                            
         B     EXIT                                                             
         SPACE 1                                                                
IVALIPUT MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         SPACE 1                                                                
IVALMISS MVC   FVMSGNO,=AL2(FVFMISS)                                            
         B     EXIT                                                             
         SPACE 1                                                                
IVALMRSP MVC   FVMSGNO,=AL2(136)                                                
         B     EXIT                                                             
         SPACE 1                                                                
IVALNUM  MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXIT                                                             
         SPACE 1                                                                
IVALFMT  MVC   FVMSGNO,=AL2(ACEIVFT)                                            
         B     EXIT                                                             
         SPACE 1                                                                
IVALACCT MVC   FVMSGNO,=AL2(ACEACCT)                                            
         B     EXIT                                                             
         SPACE 1                                                                
IVALLEDG MVC   FVMSGNO,=AL2(ACELEDG)                                            
         B     EXIT                                                             
         SPACE 1                                                                
IVALULFA MVC   FVMSGNO,=AL2(108)                                                
         B     EXIT                                                             
         SPACE 1                                                                
IVALSTUP MVC   FVMSGNO,=AL2(ACESETUP)                                           
         B     EXIT                                                             
         SPACE 1                                                                
IVALRQUL MVC   FVMSGNO,=AL2(ACEULCFL)                                           
         B     EXIT                                                             
         SPACE 1                                                                
IVALSIGN MVC   FVMSGNO,=AL2(ACELTSG)                                            
         B     EXIT                                                             
         SPACE 1                                                                
IVALLIST MVC   FVMSGNO,=AL2(ACEIVLT)                                            
         B     EXIT                                                             
         SPACE 1                                                                
IVALREQ  MVC   FVMSGNO,=AL2(ACEIVRQ)                                            
         B     EXIT                                                             
         SPACE 1                                                                
IVALSEL  MVC   FVMSGNO,=AL2(ACEIVRQN)                                           
         B     EXIT                                                             
         SPACE 1                                                                
IVALDATE MVC   FVMSGNO,=AL2(ACEIVDT)                                            
         B     EXIT                                                             
         SPACE 1                                                                
IVALLST  MVC   FVMSGNO,=XL2'FF09'                                               
         MVI   FVOMTYP,GTMINF                                                   
         ST    RE,FVADDR                                                        
         B     EXIT                                                             
         SPACE 1                                                                
IVALEXIT MVI   FVOMTYP,GTMINF                                                   
         MVC   FVADDR,AACTHDR                                                   
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 1                                                                
DMADD    DC    CL8'DMADD'                                                       
DMGETR   DC    CL8'GETREC  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMRDIR   DC    CL8'DMRDIR  '                                                    
DMWRT    DC    CL8'DMWRT   '                                                    
TEMPSTR  DC    CL8'TEMPSTR '                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
SPECTAB  DS    0F                                                               
         DC    AL1(CMREPN),AL1(RTREPNM),AL2(COMNMEH-TWAD)                       
         DC    AL1(CMCNLG),AL1(RTSUPLG),AL2(COMSLDGH-TWAD)                      
         DC    AL1(CMREAD),AL1(RTRDLDG),AL2(COMLDGRH-TWAD)                      
         DC    AL1(CMRLST),AL1(RTRDLDG),AL2(COMLDGRH-TWAD)                      
         DC    AL1(CMBGDT),AL1(RTBUDGT),AL2(COMBUD1H-TWAD)                      
         DC    AL1(CMIF),AL1(RTCONIF),AL2(COMOPT1H-TWAD)                        
         DC    AL1(CMOR),AL1(RTCONIF),AL2(COMOPT1H-TWAD)                        
         DC    AL1(CMAND),AL1(RTCONIF),AL2(COMOPT1H-TWAD)                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
LWSD     DSECT                                                                  
ALOADER  DS    A                                                                
AAPGIO   DS    A                                                                
DUB      DS    D                                                                
DWORK    DS    12D                                                              
TMPPARM  DS    8F                                                               
SAVERE   DS    F                                                                
BUDGLIST DS    16H                 UP TO 16 BUDGETS                             
ERRORMSG DS    CL60                                                             
SELKEY   DS    CL(L'RESKEY)                                                     
SELOWN   DS    CL8                 OWNER                                        
TEMPOWN  DS    CL8                 TEMPORAY STORAGE FOR OWNER                   
REPNAME  DS    CL1                                                              
LDGRLIST DS    8CL3                UP TO 8 LEDGERS                              
LDGR#    DS    AL1                 NUMBER OF LEDGERS, MAX IS 8                  
OPT1LIST DS    XL10                UP TO TEN OPTIONS                            
OPT1#    DS    XL1                                                              
OPT3LIST DS    XL10                                                             
OPT3#    DS    XL1                                                              
OPT4LIST DS    XL10                                                             
OPT4#    DS    XL1                                                              
BUDCDE   DS    CL10                                                             
MULTLDGR DS    XL1                 YES/NO (MULTILEDGER REQUEST)                 
RFPFLAG  DS    XL1                 RFP INDICATOR                                
RFPFON   EQU   X'80'               RFP IS ON                                    
SOONIND  DS    XL1                                                              
SOONIUL  EQU   X'80'               VALID UNIT/LEDGER                            
EXTFLD   DS    XL1                                                              
MOASTR   DS    CL4                 YYMM      MOA START                          
MOAEND   DS    CL4                 YYMM      MOA END                            
STRDTE   DS    CL6                 YYMMDD    START DATE                         
ENDDTE   DS    CL6                 YYMMDD    END DATE                           
ACTSDTE  DS    CL6                 YYMMDD    ACTIVITY START                     
ACTEDTE  DS    CL6                 YYMMDD    ACTIVITY END                       
TEMPDATE DS    CL6                                                              
*                                                                               
XTRA_MSG DS    CL15                                                             
OPTNME   DS    CL15                OPTION NAME TO DISPLAY                       
OPTIDNUM DS    AL1                                                              
OPTEQU   DS    AL1                 TYPE EQUATE                                  
TEMPACC  DS    CL12                TEMPORARY STORAGE FOR ACCOUNT                
TEMPLIST DS    CL6                 TEMPORARY STORAGE FOR BILLING LIST           
OFFFLDH  DS    CL8                 FAKE OFFICE FIELD HEADER                     
OFFFLD   DS    CL2                 FAKE OFFICE FIELD                            
FAKEFLDH DS    CL8                 FAKE FIELD HEADER                            
FAKEFLD  DS    CL15                FAKE FIELD                                   
PRTSTYLE DS    CL1                 PORTRAIT/LANDSCAPE                           
CUROFF   DS    CL2                                                              
SVFLTS   DS    CL5                                                              
         ORG   SVFLTS                                                           
SVFLT1   DS    CL1                 FILTER 1                                     
SVFLT2   DS    CL1                 FILTER 2                                     
SVFLT3   DS    CL1                 FILTER 3                                     
SVFLT4   DS    CL1                 FILTER 4                                     
SVFLT5   DS    CL1                 FILTER 5                                     
TYPEC1   DS    XL1                 X'C1' ELEMENT TYPE                           
TYPEC5   DS    XL1                 X'C5' ELEMENT TYPE                           
WORK     DS    CL20                                                             
PERBLOCK DS    CL58                                                             
LWSX     DS    0C                                                               
         EJECT                                                                  
SPECD    DSECT                                                                  
SPECNUM  DS    AL1                 COMMAND NUMBER FROM EQUATES                  
SPECROUT DS    AL1                 ROUTINE NUMBER                               
RTREPNM  EQU   1                                                                
RTRDLDG  EQU   2                                                                
RTSUPLG  EQU   3                                                                
RTBUDGT  EQU   4                                                                
RTCONIF  EQU   5                                                                
SPECFLDH DS    AL2                 FIELD HEADER DISPLAY                         
SPECLNQ  EQU   *-SPECD                                                          
         SPACE 2                                                                
COMMD    DSECT                                                                  
COMCHR1H DS    CL8                                                              
COMCHR1  DS    CL1                                                              
COMMENTH DS    CL8                                                              
COMMENT  DS    CL75                                                             
COMCHR2H DS    CL8                                                              
COMCHR2  DS    CL1                                                              
COMMLNQ  EQU   *-COMMD                                                          
         SPACE 2                                                                
LISTD    DSECT                                                                  
LISTACTH DS    CL8                 HEADER FOR NUMBER                            
LISTACT  DS    CL3                 SELECTION FIELD                              
LISTDATH DS    CL8                 HEADER FOR DATA                              
LISTDAT  DS    CL73                DATA                                         
         ORG   LISTDAT                                                          
         DS    CL1                                                              
LISTTYP  DS    CL4                 TYPE                                         
         DS    CL2                                                              
LISTFMT  DS    CL1                 FORMAT                                       
         DS    CL9                                                              
LISTNME  DS    CL36                NAME                                         
         DS    CL2                                                              
LISTOWN  DS    CL8                 OWNER                                        
         DS    CL2                                                              
LISTADTE DS    CL8                 ACTIVITY DATE                                
         ORG                                                                    
LISTLNQ  EQU   *-LISTD                                                          
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT OFF                                                              
       ++INCLUDE ACAPGEQU                                                       
         PRINT ON                                                               
         EJECT                                                                  
*              DSECT FOR HEADLINE DEFINITIONS                                   
*                                                                               
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRE5D                                                       
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRE6D                                                       
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACSCR17   08/27/15'                                      
         END                                                                    
