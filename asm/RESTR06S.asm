*          DATA SET RESTR06S   AT LEVEL 024 AS OF 05/01/02                      
*PHASE T80E06A                                                                  
         TITLE 'T80E06 - RESTR06 - STRATEGY REPORT'                             
***********************************************************************         
*                                                                     *         
*  RESTR06 (T80E06) --- REPORT OF STRATEGY RECORDS                    *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 08FEB94 (SKU) DATE OF BIRTH                                         *         
*                                                                     *         
* 07MAR94 (SKU) ADD GROUP/SUBGROUP TO KEY                             *         
*                                                                     *         
* 28MAR95 (BU ) REARRANGE HEADINGS, RIGHT-JUSTIFY DOLLAR VALUES       *         
*                                                                     *         
* 09OCT96 (SEP) FORMAT LOW POWER   TV STATION CALL LETTERS            *         
*                                                                     *         
*               ***  END TOMBSTONE  ***                               *         
***********************************************************************         
T80E06   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80E06*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         XC    STATFILT,STATFILT   CLEAR FILTERS                                
         XC    STDTFILT,STDTFILT                                                
         XC    EDDTFILT,EDDTFILT                                                
                                                                                
         CLC   =C'..',SRPGRUP      FLAG TO TELL US THAT WE CAME BY A            
         BNE   VK03                PFKEY.  CLEAR GROUP AND PROMPT USER          
         XC    SRPGRUP,SRPGRUP     FOR INPUT.                                   
         MVI   SRPGRUPH+5,0                                                     
         MVI   SRPGRUPH+4,0        CLEAR INPUT FLAG                             
         OI    SRPGRUPH+6,X'80'+X'01' XMIT/MODIFIED IN CASE THE USER            
         LA    R2,SRPGRUPH         IS HAPPY WITH THE FILTERS ALREADY            
         B     GETFILT             PLEASE ENTER FILTER                          
*                                                                               
* EITHER GROUP/SUBGROUP OR STATION CAN BE ENTERED                               
*                                                                               
VK03     DS    0H                                                               
         CLI   SRPGRUPH+5,0        GROUP/SUB-GROUP?                             
         BE    VK08                                                             
*                                                                               
         LA    R2,SRPSTATH                                                      
         CLI   SRPSTATH+5,0                                                     
         BNE   INVLFLD                                                          
                                                                                
         LA    R2,SRPGRUPH                                                      
         CLI   TWAACCS,C'$'        STA SIGN-ON MUST FILTER ON STA               
         BE    INVLSIGN                                                         
                                                                                
         OC    SRPGRUP,SPACES      BLANK PAD                                    
         MVC   SVKEY,KEY           SAVE OFF KEY                                 
         GOTO1 VALIGRP                                                          
         BNZ   INVLGRP                                                          
         MVC   GRPFILT,SRPGRUP                                                  
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     VK20                                                             
*                                                                               
VK08     DS    0H                                                               
         LA    R2,SRPSTATH                                                      
         CLI   SRPSTATH+5,0        STATION?                                     
         BE    MISSFLD                                                          
*                                                                               
* VALIDATE STATION CALL LETTERS                                                 
*                                                                               
VK10     DS    0H                                                               
         CLI   SRPSTATH+5,0        STATION?                                     
         BE    VK20                                                             
         MVC   SVKEY,KEY                                                        
         LA    R2,SRPSTATH         VALIDATE STATION CALL LETTER FORMAT          
         GOTO1 VALISTA                                                          
         MVC   STATFILT,WORK       SAVE CALL LETTERS                            
         MVC   GRPFILT,WORK+41                                                  
         MVC   KEY,SVKEY                                                        
                                                                                
         GOTO1 CKACCESS                                                         
         BNZ   SLOCKOUT                                                         
*                                                                               
* VALIDATE PERIOD                                                               
                                                                                
VK20     DS    0H                                                               
         LA    R2,SRPPERIH                                                      
         CLI   SRPPERIH+5,0        PERIOD?                                      
         BE    VK30                                                             
                                                                                
         GOTO1 VALIPERI                                                         
         BNZ   INVLPER                                                          
         MVC   STDTFILT,WORK                                                    
         MVC   EDDTFILT,WORK+3                                                  
                                                                                
         CLC   EDDTFILT,STDTFILT   END V START DATE                             
         BH    INVLDAT             ERR - END DATE BEFORE START DATE             
                                                                                
VK30     DS    0H                                                               
         CLI   SRPDATEH+5,0        ACTIVITY DATE                                
         BE    VKX                                                              
                                                                                
         GOTO1 DATVAL,DMCB,SRPDATE,WORK                                         
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLFLD                                                          
         GOTO1 DATCON,DMCB,WORK,(2,ACTVDT)      ACTIVITY DATE                   
                                                                                
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS REPORT                                                                
***********************************************************************         
PR       DS    0H                                                               
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVI   RSTRKTYP,RSTRTYPQ                                                
         MVI   RSTRKSUB,RSTRSITQ                                                
         MVC   RSTRKREP,AGENCY                                                  
*                                                                               
         CLI   SRPGRUPH+5,0        FILTER ON GRP/SUBGRP?                        
         BE    PR05                                                             
         MVC   RSTRKGRP,GRPFILT    YES, START WITH THIS GRP/SUBGRP              
         B     PR10                                                             
*                                                                               
PR05     DS    0H                                                               
         CLI   SRPSTATH+5,0        FILTER ON STATION?                           
         BE    PR10                                                             
         MVC   RSTRKGRP,GRPFILT    YES, START WITH THIS GRP/SUBGRP              
         MVC   RSTRKSTA,STATFILT   YES, START WITH THIS STATION                 
         DROP  R6                                                               
*                                                                               
PR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
PR20     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         CLI   RSTRKTYP,RSTRTYPQ                                                
         BNE   PRX                                                              
         CLI   RSTRKSUB,RSTRSITQ                                                
         BNE   PRX                                                              
         CLC   RSTRKREP,AGENCY                                                  
         BNE   PRX                                                              
*                                                                               
         MVC   PRINTKEY,KEY                                                     
*                                                                               
         CLI   SRPGRUPH+5,0        FILTER ON GRP/SUB                            
         BE    PR25                                                             
         CLI   SRPGRUPH+5,1                                                     
         BH    PR23                                                             
         CLC   RSTRKGRP(1),GRPFILT                                              
         BNE   PRX                                                              
         B     PR25                                                             
*                                                                               
PR23     DS    0H                                                               
         CLC   RSTRKGRP,GRPFILT                                                 
         BNE   PRX                 WANT ONLY RECORDS WITH THIS G/SG             
*                                                                               
PR25     DS    0H                                                               
         CLI   SRPSTATH+5,0        FILTER ON STATION                            
         BE    PR30                                                             
         CLC   RSTRKSTA,STATFILT                                                
         BNE   PRX                 WANT ONLY RECORDS WITH THIS STATION          
*                                                                               
PR30     DS    0H                                                               
         CLI   SRPPERIH+5,0        FILTER ON PERIOD?                            
         BE    PR60                                                             
*                                                                               
* PERIOD FILTER MUST AT LEAST OVERLAP                                           
*                                                                               
         CLC   STDTFILT,RSTRKEND   THESE DATES ARE IN 9'S COMP!                 
         BL    PRSEQ                                                            
         CLC   EDDTFILT,RSTRKSTD                                                
         BH    PRSEQ                                                            
         DROP  R6                                                               
                                                                                
PR60     DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SRPDATEH+5,0        FILTER ON ACTIVITY DATE?                     
         BE    PR75                                                             
*                                                                               
         L     R6,AIO                                                           
         USING RSTRDESD,R6                                                      
         MVI   ELCODE,RSTRDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   ACTVDT,RSTRDUPT     STARTS FROM THIS ACTIVITY DATE               
         BH    PRSEQ                                                            
         DROP  R6                                                               
*                                                                               
PR75     DS    0H                                                               
         L     R6,AIO                                                           
         USING RSTRREC,R6                                                       
         MVC   STATION,RSTRKSTA    SAVE OFF FOR HEADHOOK                        
         MVC   STARTDT,RSTRKSTD                                                 
         MVC   ENDDT,RSTRKEND                                                   
         MVC   LASTUPD,RSTRDUPT                                                 
         MVC   SHRGOAL,RSTRDSHR                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         USING RSTRTXTD,R6                                                      
         MVI   ELCODE,RSTRTCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   GKC10                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT SITUATION ANALYSIS                                                      
***********************************************************************         
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRINT                                                         
                                                                                
         MVC   P(26),=C'*** SITUATION ANALYSIS ***'                             
         BAS   RE,PRINT                                                         
                                                                                
SIT10    DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         CLC   SEQNUM,RSTRTXSQ                                                  
         BL    SIT10                                                            
                                                                                
         CLC   =C'C=',RSTRTEXT     CHECK FOR STORED COMMENTS                    
         BE    SIT20                                                            
         ZIC   R1,RSTRTELN         PRINT THE NOTES                              
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),RSTRTEXT                                                    
         BAS   RE,PRINT                                                         
         B     SIT30                                                            
*                                                                               
SIT20    DS    0H                  EXPAND THE STORED COMMENTS                   
         GOTO1 PRTSTCMT,DMCB,RSTRTEXT                                           
*                                                                               
SIT30    BAS   RE,NEXTEL                                                        
         BE    SIT10                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT GOALS, KEY INVENTORY, COMMENTS                                          
***********************************************************************         
GKC10    DS    0H                                                               
         MVC   KEY,PRINTKEY                                                     
         MVC   AIO,AIO2                                                         
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVI   RSTRKSUB,RSTRGKCQ   GOALS FIRST                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE                                           
         BNE   SAT10                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTRGOLD,R6                                                      
         MVI   ELCODE,RSTRGCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   GKC50                                                            
*                                                                               
         MVI   SEQNUM,0                                                         
         BAS   RE,PRINT                                                         
         MVC   P(13),=C'*** GOALS ***'                                          
         BAS   RE,PRINT                                                         
                                                                                
GKC20    DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         CLC   SEQNUM,RSTRGOSQ                                                  
         BL    GKC20                                                            
*                                                                               
         CLC   =C'C=',RSTRGOLE     CHECK FOR STORED COMMENTS                    
         BE    GKC30                                                            
         ZIC   R1,RSTRGELN         PRINT THE NOTES                              
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),RSTRGOLE                                                    
         BAS   RE,PRINT                                                         
         B     GKC40                                                            
*                                                                               
GKC30    DS    0H                  EXPAND THE STORED COMMENTS                   
         GOTO1 PRTSTCMT,DMCB,RSTRGOLE                                           
*                                                                               
GKC40    BAS   RE,NEXTEL                                                        
         BE    GKC20                                                            
*                                                                               
GKC50    DS    0H                  PRINT KEY INVENTORY                          
         L     R6,AIO                                                           
         USING RSTRKEYD,R6                                                      
         MVI   ELCODE,RSTRKCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   GKC90                                                            
*                                                                               
         MVI   SEQNUM,0                                                         
         BAS   RE,PRINT                                                         
         MVC   P(21),=C'*** KEY INVENTORY ***'                                  
         BAS   RE,PRINT                                                         
                                                                                
GKC60    DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         CLC   SEQNUM,RSTRKESQ                                                  
         BL    GKC60                                                            
*                                                                               
         CLC   =C'C=',RSTRKEYE     CHECK FOR STORED COMMENTS                    
         BE    GKC70                                                            
         ZIC   R1,RSTRKELN         PRINT THE NOTES                              
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),RSTRKEYE                                                    
         BAS   RE,PRINT                                                         
         B     GKC80                                                            
*                                                                               
GKC70    DS    0H                  EXPAND THE STORED COMMENTS                   
         GOTO1 PRTSTCMT,DMCB,RSTRKEYE                                           
*                                                                               
GKC80    BAS   RE,NEXTEL                                                        
         BE    GKC60                                                            
*                                                                               
GKC90    DS    0H                  PRINT COMMENTS                               
         L     R6,AIO                                                           
         USING RSTRCMTD,R6                                                      
         MVI   ELCODE,RSTRCCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   SAT10                                                            
*                                                                               
         MVI   SEQNUM,0                                                         
         BAS   RE,PRINT                                                         
         MVC   P(16),=C'*** COMMENTS ***'                                       
         BAS   RE,PRINT                                                         
                                                                                
GKC100   DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         CLC   SEQNUM,RSTRCMSQ                                                  
         BL    GKC100                                                           
*                                                                               
         CLC   =C'C=',RSTRCMTE     CHECK FOR STORED COMMENTS                    
         BE    GKC110                                                           
         ZIC   R1,RSTRCELN         PRINT THE NOTES                              
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),RSTRCMTE                                                    
         BAS   RE,PRINT                                                         
         B     GKC120                                                           
*                                                                               
GKC110   DS    0H                  EXPAND THE STORED COMMENTS                   
         GOTO1 PRTSTCMT,DMCB,RSTRCMTE                                           
*                                                                               
GKC120   BAS   RE,NEXTEL                                                        
         BE    GKC100                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT STRATEGY AND TACTICS                                                    
***********************************************************************         
SAT10    DS    0H                                                               
         MVC   KEY,PRINTKEY                                                     
         MVC   AIO,AIO2                                                         
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVI   RSTRKSUB,RSTRSATQ                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE                                           
         BNE   SAC10                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTRTXTD,R6                                                      
         MVI   ELCODE,RSTRTCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   SAC10                                                            
*                                                                               
         MVI   SEQNUM,0                                                         
         BAS   RE,PRINT                                                         
         MVC   P(28),=C'*** STRATEGY AND TACTICS ***'                           
         BAS   RE,PRINT                                                         
                                                                                
SAT20    DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         CLC   SEQNUM,RSTRTXSQ                                                  
         BL    SAT20                                                            
*                                                                               
         CLC   =C'C=',RSTRTEXT     CHECK FOR STORED COMMENTS                    
         BE    SAT30                                                            
         ZIC   R1,RSTRTELN         PRINT THE NOTES                              
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),RSTRTEXT                                                    
         BAS   RE,PRINT                                                         
         B     SAT40                                                            
*                                                                               
SAT30    DS    0H                  EXPAND THE STORED COMMENTS                   
         GOTO1 PRTSTCMT,DMCB,RSTRTEXT                                           
*                                                                               
SAT40    BAS   RE,NEXTEL                                                        
         BE    SAT20                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT SYSTEMS AND CONTROLS                                                    
***********************************************************************         
SAC10    DS    0H                                                               
         MVC   KEY,PRINTKEY                                                     
         MVC   AIO,AIO2                                                         
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVI   RSTRKSUB,RSTRSACQ                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE                                           
         BNE   ACC10                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTRTXTD,R6                                                      
         MVI   ELCODE,RSTRTCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   ACC10                                                            
*                                                                               
         MVI   SEQNUM,0                                                         
         BAS   RE,PRINT                                                         
         MVC   P(28),=C'*** SYSTEMS AND CONTROLS ***'                           
         BAS   RE,PRINT                                                         
                                                                                
SAC20    DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         CLC   SEQNUM,RSTRTXSQ                                                  
         BL    SAC20                                                            
*                                                                               
         CLC   =C'C=',RSTRTEXT     CHECK FOR STORED COMMENTS                    
         BE    SAC30                                                            
         ZIC   R1,RSTRTELN         PRINT THE NOTES                              
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),RSTRTEXT                                                    
         BAS   RE,PRINT                                                         
         B     SAC40                                                            
*                                                                               
SAC30    DS    0H                  EXPAND THE STORED COMMENTS                   
         GOTO1 PRTSTCMT,DMCB,RSTRTEXT                                           
*                                                                               
SAC40    BAS   RE,NEXTEL                                                        
         BE    SAC20                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT ACCOUNTS                                                                
***********************************************************************         
ACC10    DS    0H                                                               
         MVC   KEY,PRINTKEY                                                     
         MVC   AIO,AIO2                                                         
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVI   RSTRKSUB,RSTRACCQ                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(RSTRKPG-RSTRKEY),KEYSAVE                                     
         BNE   PR100                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTRACCD,R6                                                      
         MVI   ELCODE,RSTRACDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR100                                                            
*                                                                               
         BAS   RE,PRINT                                                         
         MVC   PPRISHR(5),=C'PRIOR'                                             
         MVC   PPRIDOL+7(5),=C'PRIOR'                                           
         MVC   PSHRGOL(5),=C'SHARE'                                             
         MVC   PSHRDOL+5(7),=C'DOLLARS'                                         
         BAS   RE,PRINT                                                         
         MVC   PACCT(13),=C'CODE  ACCOUNT'                                      
         MVC   POFF(3),=C'OFF'                                                  
         MVC   PPRISHR(5),=C'SHARE'                                             
         MVC   PPRIDOL+5(7),=C'DOLLARS'                                         
         MVC   PSHRGOL(4),=C'GOAL'                                              
         MVC   PSHRDOL+8(4),=C'GOAL'                                            
         BAS   RE,PRINT                                                         
         MVC   PACCT(4),=7C'-'                                                  
         MVC   PACCEXP(7),=7C'-'                                                
         MVC   POFF(3),=7C'-'                                                   
         MVC   PPRISHR(5),=7C'-'                                                
         MVC   PPRIDOL+5(7),=7C'-'                                              
         MVC   PSHRGOL(4),=7C'-'                                                
         MVC   PSHRDOL+5(7),=7C'-'                                              
         BAS   RE,PRINT                                                         
                                                                                
ACC20    DS    0H                                                               
         MVI   SEQNUM,0                                                         
                                                                                
ACC30    DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         CLC   SEQNUM,RSTRACSQ                                                  
         BL    ACC30                                                            
                                                                                
         MVC   PACCT,RSTRACCT                                                   
                                                                                
         MVC   SVKEY2,KEY                                                       
         XC    ELEM,ELEM                                                        
         MVC   ELEM+8(4),RSTRACCT  FAKE A SCREEN HEADER                         
         LA    R2,ELEM                                                          
         GOTO1 VALIADV                                                          
         BNZ   ACC40                                                            
         MVC   PACCEXP,WORK                                                     
                                                                                
ACC40    DS    0H                                                               
         MVC   KEY,SVKEY2                                                       
         GOTO1 HIGH                RE-ESTABLISH SEQ ORDER                       
                                                                                
         MVC   POFF,RSTRAOFF                                                    
         LA    R4,RSTRAPSH                                                      
         GOTO1 ALINDATA,DMCB,(3,(R4))                                           
         MVC   PPRISHR,RSTRAPSH                                                 
         LA    R4,RSTRAPDO                                                      
         GOTO1 ALINDATA,DMCB,(12,(R4))                                          
         MVC   PPRIDOL,RSTRAPDO                                                 
         LA    R4,RSTRASGL                                                      
         GOTO1 ALINDATA,DMCB,(3,(R4))                                           
         MVC   PSHRGOL,RSTRASGL                                                 
         LA    R4,RSTRASDL                                                      
         GOTO1 ALINDATA,DMCB,(12,(R4))                                          
         MVC   PSHRDOL,RSTRASDL                                                 
         BAS   RE,PRINT                                                         
                                                                                
         BAS   RE,NEXTEL                                                        
         BE    ACC30                                                            
                                                                                
         GOTO1 SEQ                 GET ALL PAGES                                
         CLC   KEY(RSTRKPG-RSTRKEY),KEYSAVE                                     
         BNE   PR100                                                            
                                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,RSTRACDQ                                                  
         BAS   RE,GETEL                                                         
         BE    ACC20                                                            
                                                                                
PR100    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,PRINTKEY                                                     
         GOTO1 HIGH                RE-ESTABLISH SEQ ORDER                       
*                                                                               
PRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     PR20                                                             
*                                                                               
PRX      B     EXIT                                                             
         EJECT                                                                  
         DROP  R6                                                               
*                                                                               
*   ALINDATA:  CHANGE IN FORMAT NECESSITATES CONVERTING, ON THE FLY,            
*        ANY DATA ENTERED EARLIER, WHICH WAS LEFT-ALIGNED, TO A RIGHT-          
*        ALIGNED FORMAT FOR REDISPLAY.                                          
*                                                                               
ALINDATA NTR1                                                                   
         ZICM  R4,1(R1),3          RESET A(ENTRY TO BE REALIGNED)               
         ZIC   R5,0(R1)            LENGTH OF ENTRY                              
         LR    R0,R5               SET LOOP CONTROL                             
         AR    R4,R5               FIND LAST CHARACTER + 1                      
         BCTR  R4,0                BACK UP TO LAST CHARACTER                    
         LR    RF,R4               SAVE A(LAST POSITION OF ENTRY)               
ALIN0020 EQU   *                   FIND FIRST SPACE FIELD                       
         CLI   0(R4),C' '          NON-SPACE FIELD FOUND?                       
         BE    ALIN0040            NO                                           
         CLI   0(R4),X'00'         NON-SPACE FIELD FOUND?                       
         BNE   ALIN0080            YES - MOVE IT OVER                           
ALIN0040 EQU   *                   FIND FIRST SPACE FIELD                       
         BCTR  R4,0                NO  - BACK UP 1 POSITION                     
         BCT   R0,ALIN0020         GO BACK AND LOOK A PREVIOUS POSN             
         B     ALIN0200            ALL POSITIONS SPACED -                       
ALIN0080 EQU   *                                                                
         CR    R0,R5               WAS LAST POSITION NON-SPACE/ZERO?            
         BE    ALIN0200            NO  - STILL AT LAST POSITION                 
         MVC   0(1,RF),0(R4)       MOVE NON-SPACE TO END                        
         MVI   0(R4),C' '          CLEAR LEADING CHAR TO SPACE                  
         BCTR  R4,0                                                             
         BCTR  RF,0                                                             
         BCT   R0,ALIN0080         GO BACK AND MOVE ANOTHER                     
ALIN0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT STORED COMMENTS                                                         
* USES IO3                                                                      
***********************************************************************         
PRTSTCMT NTR1                                                                   
         MVC   MYELCODE,ELCODE     SAVE OFF ELCODE                              
         MVC   ANOTE,0(R1)          A(RSTRTEXT)                                 
         MVC   SVKEY2,KEY                                                       
         MVC   MYSVAIO,AIO                                                      
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCMTKEY,R6                                                       
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,AGENCY     REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         L     R1,ANOTE                                                         
         MVC   RCMTKCDE,2(R1)      COMMENT CODE                                 
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BE    PRTST10                                                          
*                                                                               
         L     R1,ANOTE                                                         
         MVC   P(10),0(R1)         DIDN'T FIND THE CODE                         
         BAS   RE,PRINT            PRINT THE CODE AND EXIT                      
         B     PRTSTX                                                           
*                                                                               
PRTST10  DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RCMTELM2,R6                                                      
         MVI   ELCODE,2            COMMENT TEXT ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   PRTSTX              NO TEXT FOUND                                
*                                                                               
PRTST20  DS    0H                                                               
         CLI   RCMT2LEN,3          GET NON-BLANK COMMT LINE                     
         BH    PRTST40                                                          
         CLI   RCMT2TXT,C' '                                                    
         BNE   PRTST40                                                          
*                                                                               
PRTST30  BAS   RE,NEXTEL                                                        
         BE    PRTST20                                                          
         B     PRTSTX                                                           
*                                                                               
PRTST40  DS    0H                                                               
         ZIC   R1,RCMT2LEN         MOVE IN LENGTH OF COMMENT                    
         SH    R1,=H'2'            SUBTRACT CODE AND LENGTH                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),RCMT2TXT       MOVE IN COMMENT                              
         BAS   RE,PRINT                                                         
         B     PRTST30                                                          
*                                                                               
PRTSTX   DS    0H                                                               
         MVC   KEY,SVKEY2                                                       
         MVC   AIO,MYSVAIO                                                      
         MVC   ELCODE,MYELCODE                                                  
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
GETFILT  MVC   RERROR,=AL2(ASKFILT)                                             
         B     ERREND                                                           
*                                                                               
INVLGRP  MVC   RERROR,=AL2(INVGRP)                                              
         B     ERREND                                                           
*                                                                               
INVLPER  MVC   RERROR,=AL2(417)    FORMAT IS MMM/YY - MMM/YY                    
         B     ERREND                                                           
*                                                                               
INVLDAT  MVC   RERROR,=AL2(362)    START DATE MUST BE BEFORE END DATE           
         B     ERREND                                                           
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
SLOCKOUT MVC   RERROR,=AL2(55)                                                  
         B     ERREND                                                           
*                                                                               
INVLSIGN MVC   RERROR,=AL2(418)                                                 
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR                                                          
         EJECT                                                                  
***********************************************************************         
* REPORT HEADLINE SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REQUESTOR                                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1                                                                   
         MVC   H2(10),=C'UPDATED ON'                                            
         GOTO1 DATCON,DMCB,(2,LASTUPD),(5,H2+11)                                
         MVC   H5(4),STATION                                                    
         MVC   H5+4(5),=C'- M /'                                                
         MVC   H5+5(1),STATION+4                                                
         CLI   H5+5,C'L'                                                        
         BE    HOOK09                                                           
         CLI   H5+5,C' '                                                        
         BNE   HOOK10                                                           
         MVC   H5+5(2),=C'TV'     TV                                            
         B     HOOK10                                                           
HOOK09   EQU   *                                                                
         MVC   H5+4(5),=C'-   /'                                                
         MVC   H5+5(1),STATION+4                                                
*                                  PERIOD                                       
HOOK10   DS    0H                                                               
         ZAP   WORK+6(3),=P'0'                                                  
         MVO   WORK+6(3),STARTDT(2)  CHANGE TO PACK WITH SIGN                   
         ZAP   WORK+3(3),=P'9999'                                               
         SP    WORK+3(3),WORK+6(3) GET 9'S COMPLEMENT                           
         MVO   WORK(3),WORK+3(3)   CHANGE TO PWOS                               
         XC    WORK+2(1),WORK+2    NO DAY                                       
         GOTO1 DATCON,DMCB,(1,WORK),(6,H7+15)                                   
                                                                                
         CLC   STARTDT,ENDDT                                                    
         BE    HOOK13                                                           
         MVI   H7+21,C'-'                                                       
                                                                                
         ZAP   WORK+6(3),=P'0'                                                  
         MVO   WORK+6(3),ENDDT(2)  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+3(3),=P'9999'                                               
         SP    WORK+3(3),WORK+6(3) GET 9'S COMPLEMENT                           
         MVO   WORK(3),WORK+3(3)   CHANGE TO PWOS                               
         XC    WORK+2(1),WORK+2    NO DAY                                       
         GOTO1 DATCON,DMCB,(1,WORK),(6,H7+22)                                   
                                                                                
HOOK13   DS    0H                                                               
         EDIT  SHRGOAL,(5,H8+11),ALIGN=LEFT                                     
                                                                                
         MVC   SVKEY,KEY                                                        
         MVC   MYSVAIO2,AIO                                                     
         LA    RF,MYIOAREA                                                      
         ST    RF,AIO                                                           
                                                                                
         LA    R6,KEY                                                           
         USING RSTAKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,STATION                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   HOOK20                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RSTAREC,R6                                                       
         CLC   RSTAAFFL,SPACES                                                  
         BNE   HOOK15                                                           
         MVC   H5+10(L'RSTAMKT),RSTAMKT                                         
         B     HOOK20                                                           
*                                                                               
HOOK15   MVC   H5+10(5),=C'(   )'                                               
         MVC   H5+11(L'RSTAAFFL),RSTAAFFL                                       
         MVC   H5+16(L'RSTAMKT),RSTAMKT                                         
         OC    H5+16(L'RSTAMKT),SPACES    BLANK PAD FOR CENTERING               
         DROP  R6                                                               
*                                                                               
HOOK20   DS    0H                                                               
         GOTO1 CENTER,DMCB,H5,88                                                
         MVC   H6(15),=C'S T R A T E G Y'                                       
         GOTO1 CENTER,DMCB,H6,88                                                
         MVC   H7(15),=C'FOR THE PERIOD '                                       
         GOTO1 CENTER,DMCB,H7,88                                                
         MVC   H8(10),=C'SHARE GOAL'                                            
         GOTO1 CENTER,DMCB,H8,88                                                
*                                                                               
         MVC   AIO,MYSVAIO2                                                     
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                RE-ESTABLISH SEQ ORDER                       
*                                                                               
HOOKX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOCAL STORAGE AREA                                                            
***********************************************************************         
STATION  DS    CL5                                                              
GRPFILT  DS    CL2                                                              
STATFILT DS    CL5                                                              
SHRGOAL  DS    XL2                                                              
STARTDT  DS    XL2                                                              
ENDDT    DS    XL2                                                              
STDTFILT DS    XL2                                                              
EDDTFILT DS    XL2                                                              
ACTVDT   DS    XL2                                                              
MYSVAIO  DS    A                   FOR STORED COMMENT ROUTI                     
MYSVAIO2 DS    A                   FOR HEAD HOOK                                
SVKEY    DS    CL(L'KEY)                                                        
SVKEY2   DS    CL(L'KEY)                                                        
PRINTKEY DS    CL(L'KEY)                                                        
LASTUPD  DS    CL(L'RSTRDUPT)                                                   
ANOTE    DS    A                   ADDRESS OF STORED COMMENT                    
MYELCODE DS    X                   SAVES ELCODE                                 
SEQNUM   DS    X                   SEQUENCE NUMBER                              
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MYIOAREA DS    CL1000                                                           
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE RESTRFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESTRFBD          (OUR REPORT SCREEN OVERLAY)                  
       ++INCLUDE RESTRWORKD                                                     
       ++INCLUDE RESTRDSECT                                                     
         PRINT ON                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PACCT    DS    CL(L'RSTRACCT)                                                   
         DS    CL2                                                              
PACCEXP  DS    CL20                                                             
         DS    CL2                                                              
POFF     DS    CL(L'RSTRAOFF)                                                   
         DS    CL3                                                              
PPRISHR  DS    CL(L'RSTRAPSH)                                                   
         DS    CL4                                                              
PPRIDOL  DS    CL(L'RSTRAPDO)                                                   
         DS    CL2                                                              
PSHRGOL  DS    CL(L'RSTRASGL)                                                   
         DS    CL4                                                              
PSHRDOL  DS    CL(L'RSTRASDL)                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024RESTR06S  05/01/02'                                      
         END                                                                    
