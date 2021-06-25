*          DATA SET REWRI01    AT LEVEL 009 AS OF 01/14/13                      
*PHASE T82101A,*                                                                
         TITLE 'T82101 - REP WRITER APPLICATION'                                
***********************************************************************         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG, KEY DSECT POINTER, GETEL REG                *         
*          R5 - WORK REG                                              *         
*          R6 - WORK REG & ELEM POINTER                               *         
*          R7 -                                                       *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (REWRI00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 -                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*  HISTORY OF CHANGES                                                 *         
*  08NOV95 (BU ) --- CHANGE REGENALL TO REGENALL1 (2K BFR/CONTRACT)   *         
*                                                                     *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
         TITLE 'T82101 - REP WRITER APPLICATION'                                
T82101   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T82101                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE                                                                  
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE                                                                  
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 2                                                                
VREC     NTR1                                                                   
         SPACE                                                                  
         GOTO1 USERVAL             GET REP NAME & ADDRESS                       
         SPACE                                                                  
         LA    R2,REWOPTH          VALIDATE OPTIONS BEFORE OTHERS               
         GOTO1 VALOPTS                                                          
         SPACE                                                                  
**       CLI   DOWNOPT,C'Y'        IF WE ARE DOWNLOADING                        
**       BNE   VREC10                                                           
**       CLI   CONOUT,C' '            AND OUTPUT TYPE NOT REQUESTED             
**       BH    VREC10                                                           
**       MVC   CONOUT(8),=CL8'DOWN'   DEFAULT TO OUTPUT OF 'DOWN'               
**       OI    CONOUTH+6,X'80'                                                  
**       MVC   TWAOUT,CONOUT                                                    
         SPACE                                                                  
VREC10   GOTO1 INITDRON            INITIALIZE DRONE                             
         SPACE                                                                  
*                                  OPTIONAL FIELDS                              
         SPACE                                                                  
         LA    R2,REWREGH          REGION                                       
         SPACE                                                                  
         GOTO1 VALIREG                                                          
*        GOTO1 RECVAL,DMCB,RREGKTYQ,(R2),0                                      
         SPACE                                                                  
         LA    R2,REWOFFH          OFFICE                                       
         SPACE                                                                  
         GOTO1 VALIOFF                                                          
         SPACE                                                                  
         LA    R2,REWPERH          PERIOD DATES                                 
         SPACE                                                                  
         GOTO1 VALIPDT                                                          
         SPACE                                                                  
         LA    R2,REWGRPH          GROUP/SUBGROUP                               
         SPACE                                                                  
         GOTO1 VALIGS                                                           
*        GOTO1 RECVAL,DMCB,RREGKTYQ,(R2),0                                      
         SPACE                                                                  
         LA    R2,REWSTAH          STATION                                      
         SPACE                                                                  
         GOTO1 VALISTA                                                          
*        GOTO1 RECVAL,DMCB,RSTAKTYQ,(R2),0                                      
         SPACE                                                                  
         LA    R2,REWADTEH        ACTIVITY DATES                                
         SPACE                                                                  
         GOTO1 VALIACT                                                          
         SPACE                                                                  
         LA    R2,REWSALH          SALESPERSON                                  
         SPACE                                                                  
         GOTO1 VALISAL                                                          
         SPACE                                                                  
         LA    R2,REWTEMH          DIV/TEAM                                     
         SPACE                                                                  
         GOTO1 VALIDT                                                           
*        GOTO1 RECVAL,DMCB,RREGKTYQ,(R2),0                                      
         SPACE                                                                  
         LA    R2,REWADVH          ADVERTISER                                   
         SPACE                                                                  
         GOTO1 VALIADV                                                          
         SPACE                                                                  
         LA    R2,REWAGYH          AGENCY                                       
         SPACE                                                                  
         GOTO1 VALIAGY                                                          
         SPACE                                                                  
         LA    R2,REWCLSH          CONTRACT CLASS                               
         SPACE                                                                  
         GOTO1 VALICLS                                                          
*        GOTO1 RECVAL,DMCB,RCLSKTYQ,(R2),0                                      
         SPACE                                                                  
         LA    R2,REWCATH          CATEGORY                                     
         SPACE                                                                  
         GOTO1 VALICAT                                                          
         SPACE                                                                  
         LA    R2,REWPRDH          PRODUCT                                      
         SPACE                                                                  
         GOTO1 VALIPRD                                                          
         SPACE                                                                  
         LA    R2,REWBOKH          BOOK                                         
         SPACE                                                                  
         GOTO1 VALIBOK                                                          
         SPACE                                                                  
         LA    R2,REWFILTH         OPTIONAL FILTERS                             
         GOTO1 VALFILT                                                          
         SPACE                                                                  
         LA    R2,REWLEFTH         LEFT HEADERS                                 
         MVI   MAX,4                                                            
         GOTO1 VALLEFT                                                          
         LA    R2,REWRGHTH         RIGHT HEADERS                                
         MVI   MAX,3                                                            
         GOTO1 VALRIGHT                                                         
         LA    R2,REWMIDH          MID LINE                                     
         MVI   MAX,1                                                            
         GOTO1 VALMID                                                           
         LA    R2,REWROWSH         ROWS                                         
         CLI   5(R2),0                                                          
         BE    NEED1R                                                           
         MVI   MAX,8                                                            
         GOTO1 VALROWS                                                          
         LA    R2,REWCOLSH         COLUMNS                                      
         CLI   5(R2),0                                                          
         BE    NEED1C                                                           
         MVI   MAX,16                                                           
         GOTO1 VALCOLS                                                          
         LA    R2,REWTITLH         USER TITLES                                  
         GOTO1 VALTITS                                                          
         GOTO1 WRAPDRON                                                         
         SPACE                                                                  
         B     XIT                                                              
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 2                                                                
PREP     NTR1                                                                   
         GOTO1 INITDRIV            INITIALIZE DRIVER                            
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         LA    R1,HOOK             APPLICATION HOOK                             
         ST    R1,GLAHOOK                                                       
         MVI   GLMODE,GLINIT                                                    
         GOTO1 DRIVER,DMCB,(R4)                                                 
         SPACE                                                                  
         MVC   REACOMFC,ACOMFACS   SET UP FOR REWRIIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,REHOOK                                                        
         MVC   REACCS,TWAACCS      PASS THROUGH LIMIT ACCESS                    
         MVC   REAUTH,TWAAUTH                   AND AUTHORIZATION               
         MVC   REUSERID,TWAORIG                 AND REQUESTING ID#              
         MVC   REREP,AGENCY                                                     
         MVC   RADDAY,ADDAY                                                     
         SPACE                                                                  
* READ CATEGORY RECS TO BUILD CLASS TABLE *                                     
         SPACE                                                                  
         TM    REQTABLE,REQCTCL    IS CLASS PART OF REQUEST                     
         BZ    PREP10                                                           
         SPACE                                                                  
         MVI   REREAD,RCTGCLST     READ CATEGORY RECS, SAVING CAT/CLS           
         SPACE                                                                  
         GOTO1 REWRIIO,DMCB,REWRIIOD                                            
         SPACE                                                                  
* READ OFFICE RECS TO BUILD REGION TABLE *                                      
         SPACE                                                                  
PREP10   TM    REQTABLE,REQOFRG    IS REGION PART OF REQUEST                    
         BZ    PREP14                                                           
         SPACE                                                                  
         MVI   REREAD,ROFFREGT     READ OFFICE RECS, SAVING OFF & REG           
         SPACE                                                                  
         GOTO1 REWRIIO,DMCB,REWRIIOD                                            
         SPACE                                                                  
* READ STATION RECS TO BUILD OWNER TABLE *                                      
         SPACE                                                                  
PREP14   TM    REQTABLE,REQSTOW    IS STATION OWNER PART OF REQUEST             
         BZ    PREP20                                                           
         SPACE                                                                  
         MVI   REREAD,RSTAOWNT     READ STATION RECS, SAVING STA & OWN          
         SPACE                                                                  
         GOTO1 REWRIIO,DMCB,REWRIIOD                                            
         SPACE                                                                  
PREP20   MVI   READRECS,RCONKTYQ                                                
         MVC   REREAD,READRECS     RECORDS TO BE READ                           
         GOTO1 REWRIIO,DMCB,REWRIIOD                                            
         SPACE                                                                  
         MVI   GLMODE,GLOUTPUT     THEN PRINT THE REPORTS                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     XIT                                                              
         EJECT                                                                  
* IOHOOK FOR CONTRACT AND OPTIONAL PWC RECS *                                   
         SPACE                                                                  
IOHOOK   NTR1                                                                   
         CLI   REMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BE    IOHOOK2                                                          
         B     XIT                                                              
         SPACE                                                                  
IOHOOK2  CLI   TESTOPT,0           TEST OPTION                                  
         BE    IOHOOK4                                                          
         BAS   RE,TEST                                                          
         CLI   TESTOPT,C'B'                                                     
         BNE   XIT                                                              
         SPACE                                                                  
IOHOOK4  BAS   RE,SUBCON           MAY CONTROL AT SUB RECORD LEVEL              
         B     XIT                                                              
         SPACE 3                                                                
*              SUB RECORD CONTROL                                               
         SPACE                                                                  
SUBCON   NTR1                                                                   
         L     R6,REAREC           MUST MATCH ON RECORD                         
         CLC   READRECS,0(R6)                                                   
         BE    SUBCON2                                                          
         CLC   READRECS,REKEY      OR KEY                                       
         BNE   XIT                                                              
         SPACE                                                                  
SUBCON2  DS    0H                                                               
         MVI   GLMODE,GLINPUT                                                   
         XC    ATHISEL,ATHISEL     (NO SUB CONTROL)                             
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     XIT                                                              
         EJECT                                                                  
* TEST OPTION - PRINT REC CODES FROM CONTRACT RECS *                            
         SPACE 3                                                                
TEST     NTR1                                                                   
         MVC   P(21),REKEY+2                                                    
         MVI   BLOCK,C' '                                                       
         MVC   BLOCK+1(254),BLOCK                                               
         LA    R2,TESTTAB                                                       
         LA    R3,BLOCK                                                         
         SPACE                                                                  
TEST2    CLI   0(R2),X'FF'                                                      
         BNE   TEST4                                                            
         GOTO1 SQUASHER,DMCB,BLOCK,250                                          
         GOTO1 CHOPPER,DMCB,(250,BLOCK),(90,P+30),4                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE                                                                  
TEST4    ZIC   R1,10(R2)           L'DATA                                       
         BCTR  R1,0                                                             
         ZIC   RE,11(R2)           DISPLACEMENT INTO RECODES                    
         LA    RE,RECODES(RE)                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)       CHECK IF ANY DATA                            
         BZ    TEST6                                                            
         ZIC   R1,0(R2)            L'LITERAL                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),1(R2)                                                    
         AR    R3,R1                                                            
         MVI   0(R3),C'='                                                       
         LA    R3,1(R3)                                                         
         ZIC   R1,10(R2)           L'DATA                                       
         BCTR  R1,0                                                             
         ZIC   RE,11(R2)           DISPLACEMENT INTO RECODES                    
         LA    RE,RECODES(RE)                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RE)                                                    
         LA    R3,2(R1,R3)                                                      
         SPACE                                                                  
TEST6    LA    R2,L'TESTTAB(R2)                                                 
         B     TEST2                                                            
         EJECT                                                                  
TESTTAB  DS    0CL12                                                            
         DC    AL1(6),C'GRPSUB   ',AL1(L'REGRS),AL1(REGRS-RECODES)              
         DC    AL1(3),C'STA      ',AL1(L'RESTA),AL1(RESTA-RECODES)              
         DC    AL1(3),C'OFF      ',AL1(L'REOFF),AL1(REOFF-RECODES)              
         DC    AL1(3),C'AGY      ',AL1(L'REAGY),AL1(REAGY-RECODES)              
         DC    AL1(3),C'AOF      ',AL1(L'REAOF),AL1(REAOF-RECODES)              
         DC    AL1(3),C'ADV      ',AL1(L'READV),AL1(READV-RECODES)              
         DC    AL1(3),C'CON      ',AL1(L'RECONZD),AL1(RECONZD-RECODES)          
         DC    X'FF'                                                            
         DC    AL1(8),C'SALESMAN ',AL1(L'RESAL),AL1(RESAL-RECODES)              
         DC    AL1(7),C'CONTYPE  ',AL1(L'RECTY),AL1(RECTY-RECODES)              
         DC    AL1(4),C'RANK     ',AL1(L'RERNK),AL1(RERNK-RECODES)              
         DC    AL1(6),C'REGION   ',AL1(L'REREG),AL1(REREG-RECODES)              
         DC    AL1(7),C'DIVTEAM  ',AL1(L'REDVT),AL1(REDVT-RECODES)              
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
HOOK     NTR1                                                                   
         CLI   GLHOOK,GLHEAD                                                    
         BNE   XIT                                                              
         GOTO1 GENHEAD                                                          
         B     XIT                                                              
         SPACE                                                                  
XIT      XIT1                                                                   
                                                                                
NEED1R   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(20),=C'ERROR - NEED ONE ROW'                             
         GOTO1 ERREX2                                                           
NEED1C   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(23),=C'ERROR - NEED ONE COLUMN'                          
         GOTO1 ERREX2                                                           
                                                                                
*              LTORG AND TABLES                                                 
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE REWRIWORKD                                                     
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*REGENALL1                                                                      
*REGENPWC                                                                       
*DRGLOBAL                                                                       
*FAFACTS                                                                        
*FATIOB                                                                         
*REWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REGENPWC                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE REWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE REWRIF1D                                                       
         SPACE                                                                  
READRECS DS    XL1                                                              
       ++INCLUDE DDGENTWA                                                       
         SPACE                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009REWRI01   01/14/13'                                      
         END                                                                    
