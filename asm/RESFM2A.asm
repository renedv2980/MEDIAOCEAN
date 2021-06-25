*          DATA SET RESFM2A    AT LEVEL 028 AS OF 11/09/93                      
*PHASE T8182AA                                                                  
         TITLE 'T8182A - RESFM2A - GOALN REPORT'                                
***********************************************************************         
*                                                                     *         
*  RESFM2A (T8182A) --- REPORT OF GOALN CARDS                         *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 26MAY93 (SKU) DATE OF BIRTH                                         *         
*                                                                     *         
* 08NOV93 (SKU) ADD PAGES TO GOAL RECORD                              *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* NOTE: AIO1 = GOALN RECORD                                           *         
*       AIO2 = GOAL PAGES                                             *         
*       AIO3 = USED BY HEADHOOK                                       *         
*                                                                     *         
***********************************************************************         
T8182A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T8182A*,R7,RR=R3                                              
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
         CLC   =C'..',GNRGRUP      FLAG TO TELL US THAT WE CAME BY A            
         BNE   VK03                PFKEY.  CLEAR GROUP AND PROMPT USER          
         XC    GNRGRUP,GNRGRUP     FOR INPUT.                                   
         MVI   GNRGRUPH+5,0                                                     
         MVI   GNRGRUPH+4,0        CLEAR INPUT FLAG                             
         OI    GNRGRUPH+6,X'80'+X'01' XMIT/MODIFIED IN CASE THE USER            
         LA    R2,GNRGRUPH         IS HAPPY WITH THE FILTERS ALREADY            
         B     GETFILT             PLEASE ENTER FILTER                          
*                                                                               
* EITHER GROUP/SUBGROUP OR STATION CAN BE ENTERED                               
*                                                                               
VK03     DS    0H                                                               
         CLI   GNRGRUPH+5,0        GROUP/SUB-GROUP?                             
         BE    VK08                                                             
*                                                                               
         LA    R2,GNRSTATH                                                      
         CLI   GNRSTATH+5,0                                                     
         BNE   INVLFLD                                                          
*                                                                               
         LA    R2,GNRGRUPH                                                      
         OC    GNRGRUP,SPACES      BLANK PAD                                    
*                                                                               
         XC    KEY,KEY             GET GROUP/SUBGROUP RECORD                    
         LA    R6,KEY                                                           
         USING RGRPKEY,R6                                                       
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,AGENCY                                                  
         MVC   RGRPKGRP,GNRGRUP                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RGRPKEY),KEYSAVE                                           
         BNE   INVLGRP                                                          
         B     VK20                                                             
*                                                                               
VK08     DS    0H                                                               
         LA    R2,GNRSTATH                                                      
         CLI   GNRSTATH+5,0        STATION?                                     
         BE    MISSFLD                                                          
*                                                                               
* VALIDATE STATION CALL LETTERS                                                 
*                                                                               
VK10     DS    0H                                                               
         CLI   GNRSTATH+5,0        STATION?                                     
         BE    VK20                                                             
         LA    R2,GNRSTATH         VALIDATE STATION CALL LETTER FORMAT          
         GOTO1 VALISTA                                                          
         MVC   STATION,WORK        SAVE CALL LETTERS                            
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
VK20     DS    0H                                                               
         CLI   GNRPERIH+5,0        PERIOD?                                      
         BE    VK30                                                             
*                                                                               
         LA    R2,GNRPERIH         VALIDATE PERIOD                              
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),BLOCK,C',=,-'                                  
         CLI   DMCB+4,0                                                         
         BE    INVLFLD             ERROR ENCOUNTERED                            
*                                                                               
* VALIDATE START DATE                                                           
*                                                                               
         LA    R5,BLOCK                                                         
         GOTO1 DATVAL,DMCB,12(R5),WORK                                          
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLFLD                                                          
         GOTO1 DATCON,DMCB,WORK,(1,STARTDT)      START DATE                     
*                                                                               
* VALIDATE END DATE                                                             
* END DATE IS SECOND IN SCANNER BLOCK                                           
*                                                                               
         GOTO1 DATVAL,DMCB,22(R5),WORK+6                                        
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLFLD                                                          
         GOTO1 DATCON,DMCB,WORK+6,(1,ENDDT)      END DATE                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),ENDDT(3)  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         MVC   ENDDT,WORK                                                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),STARTDT(3) CHANGE TO PACK WITH SIGN                    
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         MVC   STARTDT,WORK                                                     
*                                                                               
VK30     DS    0H                                                               
         CLI   GNRDATEH+5,0        ACTIVITY DATE                                
         BE    VKX                                                              
*                                                                               
         GOTO1 DATVAL,DMCB,GNRDATE,WORK                                         
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLFLD                                                          
         GOTO1 DATCON,DMCB,WORK,(3,ACTVDT)      ACTIVITY DATE                   
*                                                                               
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
         USING RGOLKEY,R6                                                       
         MVI   RGOLTYP,RGOLTYPQ                                                 
         MVC   RGOLREP,AGENCY                                                   
*                                                                               
         CLI   GNRSTATH+5,0        FILTER ON STATION?                           
         BE    PR10                                                             
         MVC   RGOLSTA,STATION     YES, START WITH THIS STATION                 
         DROP  R6                                                               
*                                                                               
PR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
PR20     DS    0H                                                               
         CLC   KEY(15),KEYSAVE                                                  
         BNE   PRX                                                              
*                                                                               
         CLI   GNRSTATH+5,0        FILTER ON STATION                            
         BE    PR30                                                             
         CLC   KEY+15(5),KEYSAVE+15                                             
         BNE   PRX                 WANT ONLY RECORDS WITH THIS STATION          
*                                                                               
PR30     CLI   KEY+26,0            MUST BE MASTER RECORD                        
         BNE   PRSEQ                                                            
*                                                                               
         CLI   GNRGRUPH+5,0        FILTER ON GROUP?                             
         BE    PR60                                                             
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),SVKEY+15                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     STATION RECORD MUST BE THERE                 
         BE    PR40                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                RESTORE SEQ ORDER                            
         B     PRSEQ                                                            
*                                                                               
PR40     MVC   AIO,AIO2            USE IO2                                      
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         ZIC   R1,GNRGRUPH+5       MATCH ON GROUP/SUBGROUP                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   RSTAGRUP(0),GNRGRUP                                              
         BE    PR50                                                             
         DROP  R6                                                               
         MVC   AIO,AIO1            RESTORE IOAREA                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         GOTO1 HIGH                RESTORE SEQ ORDER                            
         B     PRSEQ                                                            
*                                                                               
PR50     DS    0H                                                               
         MVC   AIO,AIO1            RESTORE IOAREA                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         GOTO1 HIGH                RESTORE SEQ ORDER                            
*                                                                               
PR60     DS    0H                                                               
         CLI   GNRPERIH+5,0        FILTER ON PERIOD?                            
         BE    PR70                                                             
*                                                                               
* PERIOD FILTER MUST AT LEAST OVERLAP                                           
*                                                                               
         LA    R6,KEY                                                           
         USING RGOLKEY,R6                                                       
         CLC   STARTDT,RGOLENDT    THESE DATES ARE IN 9'S COMP!                 
         BL    PRSEQ                                                            
         CLC   ENDDT,RGOLSTDT                                                   
         BH    PRSEQ                                                            
         DROP  R6                                                               
*                                                                               
PR70     DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLI   GNRDATEH+5,0        FILTER ON ACTIVITY DATE?                     
         BE    PR75                                                             
*                                                                               
         L     R6,AIO                                                           
         USING RGOLDESD,R6                                                      
         MVI   ELCODE,RGOLDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   ACTVDT,RGOLDLUP     STARTS FROM THIS ACTIVITY DATE               
         BH    PRSEQ                                                            
         DROP  R6                                                               
*                                                                               
PR75     DS    0H                                                               
         L     R6,AIO              SAVE OFF CONTROL ID                          
         USING RGOLREC,R6                                                       
         MVC   CTRLID,RGOLDCID                                                  
         MVC   LASTUPD,RGOLDLUP                                                 
         MVC   SHRGOAL,RGOLDSHR                                                 
         MVC   STATION,RGOLSTA     SAVE OFF FOR HEADHOOK                        
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTRUCT ONE REPORT RECORD                                                   
* PROCESS GOALN RECORD FOR STRATEGY & TACTICS, KEY INVENTORY, AND               
* COMMENTS                                                                      
***********************************************************************         
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   P(19),=C'STRATEGY && TACTICS:'                                   
         BAS   RE,PRINT                                                         
         L     R6,AIO                                                           
         USING RGOLSTED,R6                                                      
         MVI   ELCODE,RGOLSCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    PR78                                                             
         LA    R3,6                NO STT ELEMENT, SKIP 6 LINES                 
PR76     BAS   RE,PRINT                                                         
         BCT   R3,PR76                                                          
         B     PR100                                                            
*                                                                               
PR78     DS    0H                                                               
         MVI   SEQNUM,0                                                         
         B     PR90                                                             
*                                                                               
PR80     DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
PR90     DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         CLC   SEQNUM,RGOLSTSQ                                                  
         BL    PR80                                                             
*                                                                               
         CLC   =C'C=',RGOLSOTE                                                  
         BE    PR93                                                             
         ZIC   R1,RGOLSELN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),RGOLSOTE                                                    
         BAS   RE,PRINT                                                         
         B     PR94                                                             
*                                                                               
PR93     DS    0H                                                               
         GOTO1 PRTSTCMT,DMCB,RGOLSOTE                                           
*                                                                               
PR94     DS    0H                  THIS SECTION ALL DONE                        
         BAS   RE,NEXTEL                                                        
         BE    PR90                                                             
*                                                                               
PR95     DS    0H                  THIS SECTION ALL DONE                        
         CLI   SEQNUM,6              FILL REST WITH BLANKS                      
         BNL   PR100                                                            
         BAS   RE,PRINT                                                         
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         B     PR95                                                             
         DROP  R6                                                               
         EJECT                                                                  
PR100    DS    0H                                                               
         MVC   P(14),=C'KEY INVENTORY:'                                         
         BAS   RE,PRINT                                                         
         L     R6,AIO                                                           
         USING RGOLKTED,R6                                                      
         MVI   ELCODE,RGOLKCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    PR105                                                            
         LA    R3,3                NO KEY ELEMENT, SKIP 3 LINES                 
PR103    BAS   RE,PRINT                                                         
         BCT   R3,PR103                                                         
         B     PR130                                                            
*                                                                               
PR105    DS    0H                                                               
         MVI   SEQNUM,0                                                         
         B     PR120                                                            
*                                                                               
PR110    DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
PR120    DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         CLC   SEQNUM,RGOLKTSQ                                                  
         BL    PR110                                                            
*                                                                               
         CLC   =C'C=',RGOLSOTE                                                  
         BE    PR123                                                            
         ZIC   R1,RGOLKELN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),RGOLKOTE                                                    
         BAS   RE,PRINT                                                         
         B     PR124                                                            
*                                                                               
PR123    DS    0H                                                               
         GOTO1 PRTSTCMT,DMCB,RGOLKOTE                                           
*                                                                               
PR124    DS    0H                  THIS SECTION ALL DONE,                       
         BAS   RE,NEXTEL                                                        
         BE    PR120                                                            
*                                                                               
PR125    DS    0H                  THIS SECTION ALL DONE,                       
         CLI   SEQNUM,3              FILL REST WITH BLANKS                      
         BNL   PR130                                                            
         BAS   RE,PRINT                                                         
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         B     PR125                                                            
         DROP  R6                                                               
         EJECT                                                                  
PR130    DS    0H                                                               
         MVC   P(9),=C'COMMENTS:'                                               
         BAS   RE,PRINT                                                         
         L     R6,AIO                                                           
         USING RGOLCTED,R6                                                      
         MVI   ELCODE,RGOLCCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    PR135                                                            
         LA    R3,3                NO CMT ELEMENT, SKIP 3 LINES                 
PR133    BAS   RE,PRINT                                                         
         BCT   R3,PR133                                                         
         B     PR155                                                            
*                                                                               
PR135    DS    0H                                                               
         MVI   SEQNUM,0                                                         
         B     PR150                                                            
*                                                                               
PR140    DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
PR150    DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         CLC   SEQNUM,RGOLCTSQ                                                  
         BL    PR140                                                            
*                                                                               
         CLC   =C'C=',RGOLCOTE                                                  
         BE    PR151                                                            
         ZIC   R1,RGOLCELN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),RGOLCOTE                                                    
         BAS   RE,PRINT                                                         
         B     PR152                                                            
*                                                                               
PR151    DS    0H                                                               
         GOTO1 PRTSTCMT,DMCB,RGOLCOTE                                           
*                                                                               
PR152    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    PR150                                                            
*                                                                               
PR153    DS    0H                  THIS SECTION ALL DONE,                       
         CLI   SEQNUM,3              FILL REST WITH BLANKS                      
         BNL   PR155                                                            
         BAS   RE,PRINT                                                         
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         B     PR153                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS GOAL RECORD FOR ACCOUNT/OFFICE INFORMATION                            
***********************************************************************         
PR155    DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   P(7),=C'ACCOUNT'                                                 
         MVC   P+32(6),=C'OFFICE'                                               
         MVC   P+40(11),=C'PRIOR SHARE'                                         
         MVC   P+53(13),=C'PRIOR DOLLARS'                                       
         MVC   P+68(10),=C'SHARE GOAL'                                          
         BAS   RE,PRINT                                                         
         MVC   P(7),=8C'-'                                                      
         MVC   P+32(6),=13C'-'                                                  
         MVC   P+40(11),=13C'-'                                                 
         MVC   P+53(13),=13C'-'                                                 
         MVC   P+68(10),=13C'-'                                                 
         BAS   RE,PRINT                                                         
         EJECT                                                                  
         MVC   SVKEY,KEY           SAVE OFF KEY TO READ GOAL REC                
         MVC   AIO,AIO2                                                         
         LA    R6,KEY                                                           
         USING RGOLKEY,R6                                                       
         MVI   RGOLMAST,1                                                       
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RGOLKEY),KEYSAVE                                           
         BNE   PR200                                                            
*                                                                               
PR158    DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RGOLGOLD,R6                                                      
         MVI   ELCODE,RGOLGCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR200                                                            
         MVI   SEQNUM,0                                                         
         B     PR170                                                            
*                                                                               
PR160    DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
PR170    DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         CLC   SEQNUM,RGOLGSQ                                                   
         BL    PR160                                                            
*                                                                               
         MVC   PACC,RGOLGACC                                                    
         MVC   POFF,RGOLGOFF                                                    
         MVC   PPRISHR,RGOLGPSH                                                 
         MVC   PPRIDOL,RGOLGPDO                                                 
         MVC   PSHRGOL,RGOLGSHR                                                 
         BAS   RE,PRINT                                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    PR170                                                            
         DROP  R6                                                               
         EJECT                                                                  
PR200    DS    0H                                                               
         GOTO1 SEQ                 GET NEXT PAGE                                
         CLC   KEY(L'RGOLKEY-1),KEYSAVE                                         
         BE    PR158                                                            
                                                                                
         MVC   AIO,AIO1                                                         
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                RE-ESTABLISH SEQ ORDER                       
*                                                                               
PRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     PR20                                                             
*                                                                               
PRX      B     EXIT                                                             
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
* P1 = ADDRESS CONTAINING STORE COMMENT CODE                                    
* USES IO3                                                                      
***********************************************************************         
PRTSTCMT NTR1                                                                   
         MVC   SVELCODE,ELCODE     SAVE OFF ELCODE                              
         MVC   ANOTE,0(R1)          A(RGOLCOTE)                                 
         MVC   SVKEY2,KEY                                                       
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
         MVC   P(0),RCMT2TXT    MOVE IN COMMENT                                 
         BAS   RE,PRINT                                                         
         B     PRTST30                                                          
*                                                                               
PRTSTX   DS    0H                                                               
         MVC   KEY,SVKEY2                                                       
         MVC   AIO,AIO1                                                         
         MVC   ELCODE,SVELCODE                                                  
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
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
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
         PSPEC H2,1,C'CONTROL ID:'                                              
         PSPEC H2,76,REQUESTOR                                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1                                                                   
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING RGOLREC,R6                                                       
         MVC   H2+13(L'CTRLID),CTRLID                                           
         MVI   H2+21,C'/'                                                       
         GOTO1 DATCON,DMCB,(3,LASTUPD),(0,H2+22)                                
         MVC   H5(4),STATION                                                    
         MVC   H5+4(5),=C'- M /'                                                
         MVC   H5+5(1),STATION+4                                                
         CLI   H5+5,C' '                                                        
         BNE   HOOK10                                                           
         MVC   H5+5(2),=C'TV'     TV                                            
*                                  PERIOD                                       
HOOK10   DS    0H                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RGOLSTDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
*                                                                               
         GOTO1 DATCON,DMCB,(1,WORK),(5,H7+15)                                   
         MVI   H7+23,C'-'                                                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RGOLENDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
*                                                                               
         GOTO1 DATCON,DMCB,(1,WORK),(5,H7+24)                                   
*                                                                               
         EDIT  RGOLDSHR,(5,H8+11),ALIGN=LEFT                                    
         DROP  R6                                                               
*                                                                               
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO3                                                         
         LA    R6,KEY                                                           
         USING RSTAKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,SVKEY+15                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   HOOK20                                                           
*                                                                               
         GOTO1 GETREC                                                           
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
         GOTO1 CENTER,DMCB,H5,88                                                
         MVC   H6(19),=C'STATION GOAL REPORT'                                   
         GOTO1 CENTER,DMCB,H6,88                                                
         MVC   H7(15),=C'FOR THE PERIOD '                                       
         GOTO1 CENTER,DMCB,H7,88                                                
         MVC   H8(10),=C'SHARE GOAL'                                            
         GOTO1 CENTER,DMCB,H8,88                                                
*                                                                               
HOOK20   DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                RE-ESTABLISH SEQ ORDER                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOCAL STORAGE AREA                                                            
***********************************************************************         
STATION  DS    CL5                                                              
STARTDT  DS    XL3                                                              
ENDDT    DS    XL3                                                              
ACTVDT   DS    XL3                                                              
SVKEY    DS    CL(L'KEY)                                                        
SVKEY2   DS    CL(L'KEY)                                                        
CTRLID   DS    CL(L'RGOLDCID)                                                   
LASTUPD  DS    CL(L'RGOLDLUP)                                                   
SHRGOAL  DS    CL(L'RGOLDSHR)                                                   
ANOTE    DS    A                   ADDRESS OF STORED COMMENT                    
SVELCODE DS    X                   SAVES ELCODE                                 
SEQNUM   DS    X                   SEQUENCE NUMBER                              
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE RESFMFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMCFD          (OUR REPORT SCREEN OVERLAY)                  
       ++INCLUDE RESFMWORKD                                                     
       ++INCLUDE REGENGOL                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENGRP                                                       
       ++INCLUDE REGENCMT                                                       
         PRINT ON                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PACC     DS    CL(L'RGOLGACC)                                                   
         DS    CL2                                                              
POFF     DS    CL(L'RGOLGOFF)                                                   
         DS    CL6                                                              
PPRISHR  DS    CL(L'RGOLGPSH)                                                   
         DS    CL10                                                             
PPRIDOL  DS    CL(L'RGOLGPDO)                                                   
         DS    CL3                                                              
PSHRGOL  DS    CL(L'RGOLGSHR)                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028RESFM2A   11/09/93'                                      
         END                                                                    
