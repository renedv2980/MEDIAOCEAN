*          DATA SET SPBLDMGE   AT LEVEL 138 AS OF 05/01/02                      
*PHASE T00A7FA                                                                  
         SPACE 2                                                                
*                                                                               
* THIS ROUTINE DEALS WITH MAKEGOOD OFFERS                                       
*                                                                               
* P1 - PARAMETER BLOCK                                                          
*                                                                               
T00A7F   TITLE 'SPFLDMGE - BUILD TABLE OF MAKEGOOD OFFERS'                      
         PRINT NOGEN                                                            
BLDMGE   CSECT                                                                  
         NMOD1 WORKX-WORKD,**BMGE**,R7                                          
         USING WORKD,RC                                                         
*                                                                               
         LR    R6,R1                                                            
         USING MGEBLKD,R6                                                       
         L     R9,MGEACOM                                                       
         USING COMFACSD,R9                                                      
         MVC   USERRD,4(RD)        SAVE LINK BACK TO USER                       
         USING TSARD,R8                                                         
         LA    R8,TSARBLK                                                       
         XC    TSARBLK,TSARBLK                                                  
*                                                                               
         MVI   MGEERR,0                                                         
         MVC   DATADISP,=H'24'                                                  
         MVC   AIO,MGEMGIO         SET DEFAULT I/O AREA                         
*                                                                               
         CLI   MGEACT,0            BUILD THE TABLE                              
         BNE   M10                                                              
         BAS   RE,MGEBLDT                                                       
         B     MGEX                                                             
*                                                                               
M10      CLI   MGEACT,MGEQADD      ADD AN ENTRY                                 
         BNE   M20                                                              
         BAS   RE,ADDENTRY                                                      
         B     MGEX                                                             
*                                                                               
M20      CLI   MGEACT,MGEQDEL      DELETE AN ENTRY                              
         BNE   M30                                                              
         BAS   RE,DELENTRY                                                      
*                                                                               
M30      CLI   MGEACT,MGEQCOD      GET NEXT AVAILABLE CODE                      
         BNE   M40                                                              
         BAS   RE,GETCODE                                                       
*                                                                               
M40      CLI   MGEACT,MGEQFND      FIND THIS ENTRY                              
         BNE   M50                                                              
         BAS   RE,FNDENTRY                                                      
*                                                                               
M50      CLI   MGEACT,MGEQNXT      FIND NEXT ENTRY                              
         BNE   M60                                                              
         BAS   RE,NXTENTRY                                                      
*                                                                               
M60      CLI   MGEACT,MGEQRDH      READ HIGH                                    
         BNE   M70                                                              
         BAS   RE,RDHI                                                          
*                                                                               
M70      CLI   MGEACT,MGEQBLN      BUILD A TABLE FOR A SINGLE BUY LINE          
         BNE   M80                                                              
         BAS   RE,BLINE                                                         
*                                                                               
M80      CLI   MGEACT,MGEQGET      GET AN ENTRY - MGETSNUM SET                  
         BNE   M90                                                              
         BAS   RE,GETENTRY                                                      
*                                                                               
M90      CLI   MGEACT,MGEQTOT      DO TOTALS FOR TABLE ENTRY                    
         BNE   M100                                                             
         BAS   RE,TOTAL                                                         
*                                                                               
M100     CLI   MGEACT,MGEQPRNT     SET UP A PRINT LINE                          
         BNE   M120                                                             
*****    BAS   RE,SETPRNT                                                       
*                                                                               
M120     DS    0H                                                               
*                                                                               
MGEX     B     MGEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD THE MAKEGOOD EVALUATION TABLE                                           
***********************************************************************         
MGEBLDT  NTR1                                                                   
         OC    MGETAB,MGETAB       IS THERE A TABLE                             
         BZ    MGE05                                                            
         L     R1,MGETAB           A(BEGINING OF TABLE)                         
         LH    R2,MGETABLN                                                      
         TM    MGEOPT,MGOFULN      USE FULL WORD FOR LENGTH                     
         BNO   *+8                                                              
         L     R2,MGETABLF                                                      
         AR    R1,R2                                                            
         ST    R1,ATABLEX                                                       
         B     MGE10                                                            
*                                                                               
MGE05    XC    TSARBLK,TSARBLK                                                  
         CLI   MGTSINIT,C'Y'       WAS TSAR INITIALIZED                         
         BE    MGE10                                                            
         MVI   MGTSINIT,C'Y'                                                    
*                                                                               
         XC    GMISSTOT,GMISSTOT                                                
         XC    GMGTOT,GMGTOT                                                    
         XC    GMISSRTG,GMISSRTG                                                
         XC    GMGRTG,GMGRTG                                                    
*                                                                               
         BAS   RE,CLRACCUM                                                      
*                                                                               
         MVI   BYTE,TSAINI         INITIALIZE TSAR                              
         BAS   RE,CALLTSAR                                                      
         TM    TSINDS,TSIINIOK                                                  
         BNZ   *+6                                                              
         DC    H'0'                NO MORE SPACE ON TEMPEST FILE                
***************                                                                 
* MAKEGOOD NOTICE RECORD                                                        
***************                                                                 
MGE10    OC    MGEKEY,MGEKEY       ALREADY READ A REC?                          
         BZ    MGE15                                                            
         CLI   MGEKEY+MNKTYPE-MNKEY,MNKTYPQ     MAKEGOOD RECORD?                
         BNE   MGE15               NO, SHOULD BE READING MAKEGOOD RECS          
         CLI   MGEKEY+MNKSUBTY-MNKEY,MNKSTYPQ                                   
         BNE   MGE50                                                            
         B     MGE20                                                            
*                                                                               
MGE15    LA    R3,MGEKEY           NO, READ IT NOW                              
         USING MNKEY,R3                                                         
         MVI   MNKTYPE,MNKTYPQ                                                  
         MVI   MNKSUBTY,MNKSTYPQ                                                
         MVC   MNKAGMD,MGEAGMD     AGY/MEDIA                                    
         MVC   MNKBYR,MGEBUYER     BUYER                                        
         MVC   MNKORDER,MGEORDER   ORDER NUMBER                                 
         CLC   MGEMGGRP,SPACES                                                  
         BNH   *+10                                                             
         MVC   MNKGROUP,MGEMGGRP   MAKEGOOD GROUP CODE                          
         DROP  R3                                                               
*                                                                               
MGE20    MVC   KEY,MGEKEY                                                       
         BAS   RE,HIGH                                                          
         B     MGE40                                                            
*                                                                               
MGE30    BAS   RE,SEQ                                                           
*                                                                               
MGE40    MVC   MGEKEY,KEY          SET LAST KEY READ                            
         CLC   KEY(MNKGROUP-MNKEY),KEYSAVE                                      
         BNE   MGE45                                                            
         CLC   MGEMGGRP,SPACES                                                  
         BNH   *+14                                                             
         CLC   KEY+MNKGROUP-MNKEY(L'MNKGROUP),KEYSAVE+MNKGROUP-MNKEY            
         BNE   MGE45                                                            
*                                                                               
         BAS   RE,GETREC           GET THE RECORD                               
*                                                                               
         BAS   RE,PROCREC          PROCESS THE RECORD                           
         B     MGE30                                                            
*                                                                               
MGE45    XC    MGEKEY,MGEKEY                                                    
***************                                                                 
* MAKEGOOD OFFER RECORD                                                         
***************                                                                 
MGE50    OC    MGEKEY,MGEKEY       ALREADY READ A REC?                          
         BNZ   MGE60                                                            
*                                                                               
         LA    R3,MGEKEY           NO, READ IT NOW                              
         USING MOKEY,R3                                                         
         MVI   MOKTYPE,MOKTYPQ                                                  
         MVI   MOKSUBTY,MOKSTYPQ                                                
         MVC   MOKAGMD,MGEAGMD     AGY/MEDIA                                    
         MVC   MOKORDER,MGEORDER   ORDER NUMBER                                 
         CLC   MGEMGGRP,SPACES                                                  
         BNH   *+10                                                             
         MVC   MOKMGCD,MGEMGGRP   MAKEGOOD GROUP CODE                           
         DROP  R3                                                               
*                                                                               
MGE60    MVC   KEY,MGEKEY                                                       
         BAS   RE,HIGH                                                          
         B     MGE90                                                            
*                                                                               
MGE80    DS    0H                                                               
         MVC   KEY,MGEKEY                                                       
         BAS   RE,HIGH                                                          
         BAS   RE,SEQ                                                           
*                                                                               
MGE90    MVC   MGEKEY,KEY          SET LAST KEY READ                            
         CLC   KEY(MOKMGCD-MOKEY),KEYSAVE                                       
         BNE   MGE100                                                           
         CLC   MGEMGGRP,SPACES                                                  
         BNH   *+14                                                             
         CLC   KEY+MOKMGCD-MOKEY(L'MOKMGCD),KEYSAVE+MOKMGCD-MOKEY               
         BNE   MGE100                                                           
*                                                                               
         BAS   RE,GETREC           GET THE RECORD                               
*                                                                               
         BAS   RE,PROCREC          PROCESS THE RECORD                           
         B     MGE80                                                            
*                                                                               
MGE100   BAS   RE,WRAPUP                                                        
*                                                                               
MGEBX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD A TABLE FOR A SINGLE ENTRY                                              
***********************************************************************         
BLINE    NTR1                                                                   
         MVI   MGEERR,MGEQRMIS                                                  
         OC    MGETAB,MGETAB       AND A(TABLE)                                 
         BZ    BLNX                                                             
*                                                                               
         L     R1,MGETAB           A(TABLE)                                     
         LH    R2,MGETABLN                                                      
         TM    MGEOPT,MGOFULN      USE FULL WORD FOR LENGTH                     
         BNO   *+8                                                              
         L     R2,MGETABLF                                                      
         AR    R1,R2                                                            
         ST    R1,ATABLEX          SET A(END OF TABLE)                          
*                                                                               
         L     R4,MGETAB           A(TABLE)                                     
         TM    MGEOPT,MGOPENTB     PUT AT END OF TABLE                          
         BNO   BL20                                                             
*                                                                               
BL10     OC    0(MGERECL,R4),0(R4) YES - FIND END OF TABLE                      
         BZ    BL30                                                             
         LA    R4,MGERECL(R4)      BUMP TO NEXT ENTRY                           
         CR    R4,R1               R1 POINTS TO END OF TABLE                    
         BNL   TABFULER                                                         
         B     BL10                                                             
*                                                                               
BL20     XC    0(MGERECL,R4),0(R4)                                              
*                                                                               
BL30     ST    R4,ANENTRY          A(NEXT ENTRY)                                
*                                                                               
         MVI   MGEERR,0                                                         
         BAS   RE,PROCREC                                                       
         TM    MGEOPT,MGOPENTB     DON'T DO TOTALS YET                          
         BO    BLNX                                                             
         BAS   RE,WRAPUP                                                        
*                                                                               
BLNX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* JUST DO TOTALS                                                                
***********************************************************************         
TOTAL    NTR1                                                                   
         L     R1,MGETAB           A(TABLE)                                     
         LH    R2,MGETABLN                                                      
         TM    MGEOPT,MGOFULN      USE FULL WORD FOR LENGTH                     
         BNO   *+8                                                              
         L     R2,MGETABLF                                                      
         AR    R1,R2                                                            
         ST    R1,ATABLEX          SET A(END OF TABLE)                          
*                                                                               
         L     R4,MGETAB           A(TABLE)                                     
         TM    MGEOPT,MGOPENTB     PUT AT END OF TABLE                          
         BNO   TO20                                                             
*                                                                               
TO10     OC    0(MGERECL,R4),0(R4) YES - FIND END OF TABLE                      
         BZ    TO30                                                             
         LA    R4,MGERECL(R4)      BUMP TO NEXT ENTRY                           
         CR    R4,R1               R1 POINTS TO END OF TABLE                    
         BNL   TABFULER                                                         
         B     TO10                                                             
*                                                                               
TO20     XC    0(MGERECL,R4),0(R4)                                              
*                                                                               
TO30     ST    R4,ANENTRY          A(NEXT ENTRY)                                
         BAS   RE,WRAPUP                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* WRAP UP TOTALS & SORT FOR OFFLINE                                             
***********************************************************************         
WRAPUP   NTR1                                                                   
         BAS   RE,TOTENTRY                                                      
*                                                                               
         OC    MGETAB,MGETAB       IS THERE A TABLE                             
         BZ    WRX                 NO, MUST BE OFFLINE                          
*                                                                               
         LH    R4,MGECNT                                                        
         LTR   R4,R4                                                            
         BZ    WRX                                                              
         GOTO1 CXSORT,DMCB,MGETAB,(R4),MGERECL,MGEKEYL,0                        
         BAS   RE,TABTOTAL         DO TOTALS FOR TABLE                          
         LH    R4,MGECNT                                                        
         LTR   R4,R4                                                            
         BNZ   *+6                 SORT AGAIN                                   
         DC    H'0'                                                             
         GOTO1 CXSORT,DMCB,MGETAB,(R4),MGERECL,MGEKEYL,0                        
*                                                                               
WRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS A RECORD                                                              
*                                                                               
* ON ENTRY:    MGEMGIO             A(MAKEGOOD NOTICE/OFFER RECORD)              
***********************************************************************         
PROCREC  NTR1                                                                   
         XC    COUNTER,COUNTER                                                  
         XC    COUNTER2,COUNTER2                                                
         L     R3,MGEMGIO                                                       
         CLI   0(R3),MNKTYPQ       DARE MAKEGOOD RECORD?                        
         BNE   PRECNO                                                           
         CLI   1(R3),MNKSTYPQ      MAKEGOOD NOTICE?                             
         BE    PRECMISS                                                         
         CLI   1(R3),MOKSTYPQ      MAKEGOOD OFFER?                              
         BE    PRECMKGD                                                         
         B     PRECNO                                                           
***********************************                                             
* DARE MAKEGOOD NOTICE RECORD                                                   
***********************************                                             
         USING MNKEY,R3                                                         
PRECMISS LA    R3,MNRFRST                                                       
PCMS10   CLI   0(R3),0             END OF RECORD?                               
         BE    PCMSX                                                            
*                                                                               
         XC    ENTRY,ENTRY         CLEAR ENTRY                                  
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
*                                                                               
         CLI   0(R3),MNMSELQ       MISSED BUYLINE ELEM?                         
*        BNE   *+12                                                             
*        BAS   RE,FIXELEM                                                       
         BE    PCMSMS00                                                         
         CLI   0(R3),MNMCELQ                                                    
         BE    PCMSCM00                                                         
         CLI   0(R3),MNMOELQ                                                    
         BE    PCMSOR00                                                         
*                                                                               
PCMSNXEL ZIC   R0,1(R3)            CHECK NEXT ELEMENT                           
         AR    R3,R0                                                            
         B     PCMS10                                                           
***************                                                                 
* MISSED BUYLINE ELEMENT                                                        
***************                                                                 
         USING MNMSELD,R3                                                       
PCMSMS00 DS    0H                                                               
         MVC   SVELCDS,ELCDLO      SAVE PREVIOUS ELCODES                        
*                                                                               
         L     R1,MGEMGIO                                                       
         MVC   MGECODE,MNKGROUP-MNKEY(R1)                                       
         MVI   MGETYPE,MGETYPNR    NOTICE RECORD                                
         MVC   MGELINNO,MNMSBLIN   AGENCY BUYLINE NUMBER                        
         MVI   MGESTYPE,MGESTYDT   DETAIL ELEMENT                               
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,MGERECNO                                                      
         STC   R1,COUNTER          NO DUPS HERE                                 
*                                                                               
         MVC   MGEDTDAT,MNMSBDAT   DATE (PWOS JULIAN)                           
         MVC   MGEDTSTM,MNMSSTIM   START AND END TIMES                          
         MVC   MGEDTETM,MNMSETIM                                                
         MVC   MGEDTROT,MNMSOROT   OUT OF WEEK ROTATOR                          
         MVC   MGEDTDYS,MNMSDAYS   DAY BITS                                     
         MVC   MGEDTSLN,MNMSTSLN   TOTAL SPOT LENGTH                            
         ZICM  R1,MNMSCOST,3       COST                                         
         STCM  R1,15,MGEDTCST                                                   
         MVC   MGEDMSFD,MNMSFND                                                 
         ZICM  R1,MNMSLEN          PROGRAM NAME                                 
         SH    R1,=Y(MNMSOVRH+1)                                                
         BM    PCMSMS10                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MGEDTPGM(0),MNMSTEXT                                             
*                                                                               
PCMSMS10 BAS   RE,GETRTG                                                        
         MVC   MGEDRTG,RATING      RATING                                       
         MVC   MGEDRTGB,RATINGB    BINARY RATING                                
         MVC   MGEDMSFD,MNMSFND                                                 
*                                                                               
         CLI   MNMSNSPT,0          ANY SPOTS?                                   
         BE    PCMSMSX             NONE                                         
         ZIC   R0,MNMSNSPT                                                      
*                                                                               
PCMSMS15 STC   R0,MGESPOTN                                                      
         MVI   MGESEQN,0                                                        
         CLI   MGESPOTN,1                                                       
         BNH   *+8                                                              
         MVI   MGESEQN,1           IF SPOTN > 1 -> PUT A SEQN                   
         LH    R1,MGECNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         AH    R1,=H'1'                                                         
         STH   R1,MGECNT                                                        
         BAS   RE,BLDTABLE         PUT ENTRY INTO OUR TABLE OF ENTRIES          
         BCT   R0,PCMSMS15         ADD FOR THE NUMBER OF SPOTS                  
*                                                                               
PCMSMSX  B     PCMSNXEL                                                         
         DROP  R3                                                               
***************                                                                 
* MISSED BUYLINE COMMENT ELEMENT                                                
***************                                                                 
         USING MNMCELD,R3                                                       
PCMSCM00 DS    0H                                                               
         L     R1,MGEMGIO                                                       
         MVC   MGECODE,MNKGROUP-MNKEY(R1)                                       
         MVI   MGETYPE,MGETYPNR    NOTICE RECORD                                
         MVC   MGELINNO,MNMCREC    MISSED SPOT LINE NUMBER                      
         MVI   MGESTYPE,MGESTYCM   COMMENT ELEMENT                              
         MVC   MGERECNO,MNMCLINE   COMMENT LINE #                               
*                                                                               
         ZIC   R1,MNMCLEN                                                       
         SH    R1,=Y(MNMCOVRH+1)                                                
         BM    PCMSCMX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MGECMTXT(0),MNMCTEXT                                             
*                                                                               
         LH    R1,MGECNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         AH    R1,=H'1'                                                         
         STH   R1,MGECNT                                                        
*                                                                               
         BAS   RE,BLDTABLE                                                      
*                                                                               
PCMSCMX  B     PCMSNXEL                                                         
         DROP  R3                                                               
***************                                                                 
* MISSED BUYLINE ORBIT ELEMENT                                                  
***************                                                                 
         USING MNMOELD,R3                                                       
PCMSOR00 DS    0H                                                               
         L     R1,MGEMGIO                                                       
         MVC   MGECODE,MNKGROUP-MNKEY(R1)                                       
         MVI   MGETYPE,MGETYPNR    NOTICE RECORD                                
         MVC   MGELINNO,MNMOREC    AGENCY BUYLINE NUMBER                        
         MVI   MGESTYPE,MGESTYOR   ORBIT ELEMENT                                
         MVC   MGERECNO,MNMODAYS <--- NEED A NUMBER SO NO DUPLICATES            
*                                                                               
         MVC   MGEORROT,MNMOSDAY   OUT OF WEEK ROTATOR                          
         MVC   MGEORDYS,MNMODAYS   DAY BITS                                     
         MVC   MGEORSTM,MNMOSTIM   START AND END TIMES                          
         MVC   MGEORETM,MNMOETIM                                                
         ZICM  R1,MNMOLEN          PROGRAM TEXT                                 
         SH    R1,=Y(MNMOOVRH+1)                                                
         BM    PCMSOR10                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MGEORTXT(0),MNMOPROG                                             
*                                                                               
PCMSOR10 LH    R1,MGECNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         AH    R1,=H'1'                                                         
         STH   R1,MGECNT                                                        
*                                                                               
         BAS   RE,BLDTABLE                                                      
*                                                                               
PCMSORX  B     PCMSNXEL                                                         
         DROP  R3                                                               
*                                                                               
PCMSX    B     PRECYES                                                          
         EJECT                                                                  
***********************************                                             
* DARE MAKEGOOD OFFER RECORD                                                    
***********************************                                             
         USING MOKEY,R3                                                         
PRECMKGD LA    R3,MORFRST                                                       
PCMG10   CLI   0(R3),0                                                          
         BE    PCMGX                                                            
*                                                                               
         XC    ENTRY,ENTRY                                                      
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
*                                                                               
         CLI   0(R3),MOCMELQ       MAKEGOOD GROUP COMMENT ELEMENT?              
         BE    PCMGGC00                                                         
         CLI   0(R3),MOMBELQ       MAKEGOOD BUY ELEMENT?                        
         BE    PCMGBY00                                                         
         CLI   0(R3),MOBCELQ       MAKEGOOD BUY COMMENT ELEMENT?                
         BE    PCMGBC00                                                         
         CLI   0(R3),MORBELQ       MAKEGOOD ORBIT DESCRIPTION ELEMENT?          
         BE    PCMGBO00                                                         
         CLI   0(R3),MOBDELQ       MAKEGOOD BUY DETAIL?                         
         BE    PCMGBD00                                                         
         CLI   0(R3),MODMELQ       MAKEGOOD BUY DEMO ELEMENT?                   
         BE    PCMGDM00                                                         
         CLI   0(R3),MOBBCELQ      MAKEGOOD BUYER'S BUY COMMENT ELEM?           
         BE    PCMBBC00                                                         
*                                                                               
PCMGNXEL ZIC   R0,1(R3)            SKIP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     PCMG10                                                           
***************                                                                 
* MAKEGOOD GROUP COMMENT ELEMENT                                                
***************                                                                 
         USING MOCMELD,R3                                                       
PCMGGC00 DS    0H                                                               
         L     R1,MGEMGIO                                                       
         MVC   MGECODE,MOKMGCD-MOKEY(R1)                                        
         MVI   MGETYPE,MGETYPGC    GROUP COMMENT                                
         MVI   MGELINNO,1         NO DUPS                                       
         MVI   MGESTYPE,MGESTYCM   COMMENT ELEMENT                              
         MVC   MGERECNO,MOCMLINE   COMMENT LINE #                               
*                                                                               
         ZIC   R1,MOCMLEN                                                       
         SH    R1,=Y(MOCMOVRH+1)                                                
         BM    PCMGGCX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MGECMTXT(0),MOCMTEXT                                             
*                                                                               
         LH    R1,MGECNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         AH    R1,=H'1'                                                         
         STH   R1,MGECNT                                                        
*                                                                               
         BAS   RE,BLDTABLE                                                      
*                                                                               
PCMGGCX  B     PCMGNXEL                                                         
         DROP  R3                                                               
***************                                                                 
* MAKEGOOD BUY ELEMENT                                                          
***************                                                                 
         USING MOMBELD,R3                                                       
PCMGBY00 DS    0H                                                               
         MVC   SVDTIMES,MOMBSTIM   SAVE TIMES FOR THE DETAILS                   
         MVC   SVDOWROT,MOMBOROT   OUT OF WEEK ROTATOR                          
         MVC   SVDDYBTS,MOMBDAYS   DAY BITS                                     
         MVC   SVDTSLN,MOMBTSLN    SAVE SPOT LENGTH FOR THE DETAILS             
         MVC   SVDDYPRT,MOMBDYPT   DAYPART                                      
         MVI   OFRRCNTH,1          START FROM 1 FOR THIS OFFER/RECORD           
*                                                                               
         CLI   MOMBLEN,L'SVDPRGM+MOMBOVRH                                       
         BNH   *+14                                                             
         MVC   SVDPRGM,MOMBPROG                                                 
         B     PCMGBYX                                                          
*                                                                               
         MVC   SVDPRGM,SPACES                                                   
         ZIC   R1,MOMBLEN                                                       
         SH    R1,=Y(MOMBOVRH+1)                                                
         BM    PCMGBYX             NO PROGRAM YET                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVDPRGM(0),MOMBPROG                                              
*                                                                               
PCMGBYX  B     PCMGNXEL                                                         
         DROP  R3                                                               
***************                                                                 
* MAKEGOOD BUY COMMENT ELEMENT                                                  
***************                                                                 
         USING MOBCELD,R3                                                       
PCMGBC00 DS    0H                                                               
         L     R1,MGEMGIO                                                       
         MVC   MGECODE,MOKMGCD-MOKEY(R1)                                        
         MVI   MGETYPE,MGETYPOR    GROUP COMMENT                                
         MVC   MGELINNO,MOBCOFFR   OFFER NUMBER                                 
         MVI   MGESTYPE,MGESTYCM   COMMENT ELEMENT                              
         MVC   MGERECNO,MOBCREC    RECORD NUMBER                                
         MVC   MGESPOTN,MOBCLINE   COMMENT LINE #                               
*                                                                               
         ZIC   R1,MOBCLEN                                                       
         SH    R1,=Y(MOBCOVRH+1)                                                
         BM    PCMGBCX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MGECMTXT(0),MOBCTEXT                                             
*                                                                               
         LH    R1,MGECNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         AH    R1,=H'1'                                                         
         STH   R1,MGECNT                                                        
*                                                                               
         BAS   RE,BLDTABLE                                                      
*                                                                               
PCMGBCX  B     PCMGNXEL                                                         
         DROP  R3                                                               
***************                                                                 
* MAKEGOOD ORBIT DESCRIPTION ELEMENT                                            
***************                                                                 
         USING MORBELD,R3                                                       
PCMGBO00 DS    0H                                                               
         L     R1,MGEMGIO                                                       
         MVC   MGECODE,MOKMGCD-MOKEY(R1)                                        
         MVI   MGETYPE,MGETYPOR    MAKEGOOD OFFER RECORD                        
         MVC   MGELINNO,MORBOFFR   OFFER NUMBER                                 
         MVI   MGESTYPE,MGESTYOR   ORBIT ELEMENT                                
         MVC   MGERECNO,MORBREC    RECORD NUMBER                                
         MVC   MGESPOTN,MORBDAYS <--- NEED A NUMBER SO NO DUPLICATES            
*                                                                               
         MVC   MGEORROT,MORBSDAY   OUT OF WEEK ROTATOR                          
         MVC   MGEORDYS,MORBDAYS   DAY BITS                                     
         MVC   MGEORSTM,MORBSTIM   START AND END TIMES                          
         MVC   MGEORETM,MORBETIM                                                
         ZICM  R1,MORBLEN          PROGRAM TEXT                                 
         SH    R1,=Y(MORBOVRH+1)                                                
         BM    PCMGOR10                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MGEORTXT(0),MORBPROG                                             
*                                                                               
PCMGOR10 LH    R1,MGECNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         AH    R1,=H'1'                                                         
         STH   R1,MGECNT                                                        
*                                                                               
         BAS   RE,BLDTABLE                                                      
*                                                                               
PCMGORX  B     PCMGNXEL                                                         
         DROP  R3                                                               
***************                                                                 
* MAKEGOOD BUY DETAIL ELEMENT                                                   
***************                                                                 
         USING MOBDELD,R3                                                       
PCMGBD00 DS    0H                                                               
         L     R1,MGEMGIO                                                       
         MVC   MGECODE,MOKMGCD-MOKEY(R1)                                        
         MVI   MGETYPE,MGETYPOR    MAKEGOOD OFFER RECORD                        
         MVC   MGELINNO,MOBDOFFR   OFFER NUMBER                                 
         MVI   MGESTYPE,MGESTYDT   DETAIL ELEMENT                               
         MVC   MGERECNO,MOBDREC    RECORD NUMBER                                
         MVC   MGESEQN,MOBDSEQ     SEQUENCE NUMBER                              
*                                                                               
         MVC   MGEDTDAT,MOBDBDAT   DATE (PWOS JULIAN)                           
         MVC   MGEDTSTM(L'SVDTIMES),SVDTIMES   START AND END TIMES              
         MVC   MGEDTROT,SVDOWROT   OUT OF WEEK ROTATOR                          
         MVC   MGEDTDYS,SVDDYBTS   DAY BITS                                     
         MVC   MGEDTSLN,SVDTSLN    TOTAL SPOT LENGTH                            
         ZICM  R1,MOBDCOST,3       COST                                         
         STCM  R1,15,MGEDTCST                                                   
         MVC   MGEDTDPT,SVDDYPRT   DAYPART                                      
         MVC   MGEDTPGM,SVDPRGM    PROGRAM                                      
         MVC   MGEDTNWK,MOBDNWKS                                                
         MVC   MGEDTNSP,MOBDNSPW                                                
*                                                                               
         BAS   RE,GTMKORTG                                                      
         MVC   MGEDRTG,RATING      RATING                                       
         MVC   MGEDRTGB,RATINGB     BINARY RATING                               
*                                                                               
         ZIC   R0,MOBDNSPW         FIND TOTAL NUMBER OF SPOTS                   
         ZIC   R1,MOBDNWKS           SO TOTALS WILL BE CORRECT                  
         STH   R1,HALF                                                          
         MH    R0,HALF                                                          
*                                                                               
         LTR   R0,R0               ANY SPOTS                                    
         BZ    PCMGBDX             NONE                                         
*                                                                               
PCMGBD15 MVC   MGESPOTN,OFRRCNTH                                                
         LH    R1,MGECNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         AH    R1,=H'1'                                                         
         STH   R1,MGECNT                                                        
         BAS   RE,BLDTABLE         PUT ENTRY INTO OUR TABLE OF ENTRIES          
*                                                                               
         MVI   MGEDTNWK,0          SO THAT WE KNOW THESE ARE JUST               
         MVI   MGEDTNSP,0              FOR THE TOTALS                           
*                                                                               
         ZIC   R1,OFRRCNTH         ANOTHER RECORD FOR THIS OFFER/RECORD         
         AH    R1,=H'1'                                                         
         CH    R1,=H'255'                                                       
         BNH   *+6                                                              
         DC    H'0'                OVERFLOW ON THIS OFFER/RECORD                
         STC   R1,OFRRCNTH                                                      
         BCT   R0,PCMGBD15         ADD FOR THE NUMBER OF SPOTS                  
*                                                                               
PCMGBDX  B     PCMGNXEL                                                         
         DROP  R3                                                               
***************                                                                 
* MAKEGOOD BUY DEMO ELEMENT                                                     
***************                                                                 
         USING MODMELD,R3                                                       
PCMGDM00 DS    0H                                                               
*                                                                               
         L     R1,MGEMGIO                                                       
*                                                                               
         MVC   MGECODE,MOKMGCD-MOKEY(R1)                                        
         MVI   MGETYPE,MGETYPOR    MAKEGOOD OFFER RECORD                        
         MVC   MGELINNO,MODMOFFR   OFFER NUMBER                                 
         MVI   MGESTYPE,MGESTYDM   DEMO ELEMENT                                 
         MVC   MGERECNO,MODMREC    RECORD NUMBER                                
*                                                                               
         XC    MGEDMOVR,MGEDMOVR                                                
         LA    R5,MGEDMOVR         TSAR LIST OF 14 DEMOVALUES                   
         LA    RE,MODMDEMO         REC LIST OF UP TO 14 DEMOS & VALUES          
         ZIC   R1,MODMLEN                                                       
         SH    R1,=H'4'                                                         
*                                                                               
PCMGDM05 BZ    PCMGDM10                                                         
         MVC   0(5,R5),3(RE)       MOVE DEMO VALUE TO TSAR                      
         LA    RE,8(RE)            BUMP LIST IN MKOFFER REC                     
         LA    R5,5(R5)            BUMP LIST IN TSAR                            
         SH    R1,=H'8'                                                         
         BNZ   PCMGDM05            END OF ELEMENT?                              
*                                                                               
PCMGDM10 LH    R1,MGECNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         AH    R1,=H'1'                                                         
         STH   R1,MGECNT                                                        
*                                                                               
         BAS   RE,BLDTABLE                                                      
*                                                                               
PCMGDMX  B     PCMGNXEL                                                         
         DROP  R3                                                               
***************                                                                 
* MAKEGOOD BUY COMMENT ELEMENT                                                  
***************                                                                 
         USING MOBBCELD,R3                                                      
PCMBBC00 DS    0H                                                               
         L     R1,MGEMGIO                                                       
         MVC   MGECODE,MOKMGCD-MOKEY(R1)                                        
         MVI   MGETYPE,MGETYPOR    GROUP COMMENT                                
         MVC   MGELINNO,MOBBCOFR   OFFER NUMBER                                 
         MVI   MGESTYPE,MGESTYBC   COMMENT ELEMENT                              
         MVC   MGERECNO,MOBBCREC   RECORD NUMBER                                
         MVC   MGESPOTN,MOBBCLNE   COMMENT LINE #                               
         MVC   MGESEQN,MOBBCSEQ    SEQUENCE #                                   
*                                                                               
         ZIC   R1,MOBBCLEN                                                      
         SH    R1,=Y(MOBBCOVH+1)                                                
         BM    PCMBBCX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MGECMTXT(0),MOBBCTXT                                             
*                                                                               
         LH    R1,MGECNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         AH    R1,=H'1'                                                         
         STH   R1,MGECNT                                                        
*                                                                               
         BAS   RE,BLDTABLE                                                      
*                                                                               
PCMBBCX  B     PCMGNXEL                                                         
         DROP  R3                                                               
*                                                                               
PCMGX    B     PRECYES                                                          
***********************************                                             
* RETURN POINTS                                                                 
***********************************                                             
PRECYES  B     YES                                                              
*                                                                               
PRECNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* MAKE ALL X'10' ELEMENTS FOR MKN RECS 3 BYTES LONGER                           
***********************************************************************         
FIXELEM  NTR1                                                                   
         USING MNMSELD,R3                                                       
         LA    R1,MNMSFND                                                       
         TM    0(R1),X'80'         IF X'80' IS ON, IT IS TEXT AND NEEDS         
         BNZ   FX10                                                             
*                                                                               
         ZIC   R1,MNMSLEN                                                       
         LH    R2,=Y(MNMSFOVR)                                                  
         CR    R1,R2               OLD LENGTH                                   
         BH    FIXELX              BIGGER - ALREADY BEEN CHANGED                
*                                                                               
FX10     XC    WORK,WORK                                                        
         ZIC   R1,MNMSLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R3)       STORE OLD ELEM                               
*                                                                               
         L     R5,MGEMGIO                                                       
         GOTO1 MGERECUP,DMCB,(C'S',(R5)),(R3),(R3)    DEL ELEM                  
         DROP  R3                                                               
         MVC   EMEL,WORK                                                        
         LA    R1,EMEL                                                          
         AH    R1,=Y(MNMSFOVR)                                                  
         LR    R5,R1                                                            
         LA    R2,WORK                                                          
         USING MNMSELD,R2                                                       
         ZIC   R1,MNMSLEN                                                       
         LA    R1,3(R1)                                                         
         STC   R1,MNMSLEN                                                       
         XC    MNMSFND,MNMSFND                                                  
         XC    MNMDTHLD,MNMDTHLD                                                
         MVC   MNMSTEXT,0(R5)                                                   
         L     R5,MGEMGIO                                                       
         GOTO1 MGERECUP,DMCB,(C'S',(R5)),(R2),(R3)    AND ADD NEW               
         MVC   COMMAND,=C'PUTREC'                                               
         GOTO1 CDATAMGR,DMCB,COMMAND,=C'SPTFILE',KEY+14,MGEMGIO,DMWORK          
         DROP  R2                                                               
FIXELX   B     XIT                                                              
***********************************************************************         
* PUT OUT TOTAL ENTRY                                                           
***********************************************************************         
TOTENTRY NTR1                                                                   
         OC    MGETAB,MGETAB       IF AN AREA IS PASSED TO US                   
         BNZ   TEX                    SKIP                                      
*                                                                               
         BAS   RE,CLRACCUM         CLEAR ACCUMULATORS                           
*                                                                               
         XC    THISCODE,THISCODE                                                
*                                                                               
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
*                                                                               
         XC    TSRNUM,TSRNUM                                                    
         XC    MGECODE(MGERECL),MGECODE            CLR REC                      
*                                                                               
         MVI   BYTE,TSARDH         READ HIGH                                    
         B     *+8                                                              
TE10     MVI   BYTE,TSANXT         READ NEXT                                    
         BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BO    TE60                                                             
*                                                                               
         CLC   MGECODE,THISCODE    TEST SAME CODE                               
         BE    TE30                                                             
         OC    THISCODE,THISCODE   SKIP FIRST TIME                              
         BZ    TE30                                                             
         CLC   MGECODE,=X'FEFEFE'  IS THIS THE GRAND TOTAL RECORD               
         BE    TE70                                                             
         BAS   RE,ADDTOT           ADD TOTAL RECORD                             
         BAS   RE,CLRACCUM         CLEAR ACCUMULATORS                           
         XC    THISCODE,THISCODE   DONE WITH THIS CODE                          
         B     TE40                                                             
*                                                                               
TE30     CLI   MGETYPE,MGETYPTR    IS THIS THE TOTAL RECORD                     
         BNE   TE40                                                             
         MVI   BYTE3,C'Y'          THIS CODE'S TOTAL DOES EXIST                 
         BAS   RE,PUTTOT           PUT TOTAL RECORD                             
         XC    THISCODE,THISCODE   DONE WITH THIS CODE                          
         BAS   RE,CLRACCUM         CLEAR ACCUMULATORS                           
         B     TE50                                                             
*                                                                               
TE40     MVI   BYTE3,C'N'          THIS CODE'S TOTAL DOESN'T EXIST              
         BAS   RE,ACCUM            ACCUMULATE THIS ENTRY                        
*                                                                               
TE50     B     TE10                                                             
*                                                                               
TE60     OC    THISCODE,THISCODE   IF THERE WAS NOTHING - EXIT                  
         BZ    TEX                                                              
         BAS   RE,ADDTOT           ADD LAST TOTAL RECORD                        
         MVI   BYTE3,C'N'                                                       
         MVC   THISCODE,=X'FEFEFE' SET FOR GRAND TOTAL                          
         MVC   MISSTOT,GMISSTOT    SET GRAND TOTAL                              
         MVC   MGTOT,GMGTOT                                                     
         MVC   MISSRTG,GMISSRTG                                                 
         MVC   MGRTG,GMGRTG                                                     
         BAS   RE,ADDTOT           ADD TOTAL RECORD                             
         B     TEX                                                              
*                                                                               
TE70     MVC   MISSTOT,GMISSTOT    SET GRAND TOTAL                              
         MVC   MGTOT,GMGTOT                                                     
         MVC   MISSRTG,GMISSRTG                                                 
         MVC   MGRTG,GMGRTG                                                     
         BAS   RE,PUTTOT           PUT GRAND TOTAL RECORD                       
*                                                                               
TEX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PUT A TOTAL RECORD                                                            
***********************************************************************         
TABTOTAL NTR1                                                                   
         XC    GMISSTOT,GMISSTOT                                                
         XC    GMGTOT,GMGTOT                                                    
         XC    GMISSRTG,GMISSRTG                                                
         XC    GMGRTG,GMGRTG                                                    
*                                                                               
         BAS   RE,CLRACCUM                                                      
         XC    THISCODE,THISCODE                                                
         L     R4,MGETAB           A(TABLE)                                     
         USING MGENTRYD,R4                                                      
         OC    0(MGERECL,R4),0(R4)   IF THE TABLE IS EMPTY                      
         BZ    TTX                      EXIT                                    
*                                                                               
TT10     OC    0(MGERECL,R4),0(R4) END OF TABLE                                 
         BZ    TT30                                                             
         CLI   MGETYPE,MGETYPTR    DID WE REACH THE TOTALS                      
         BE    TT30                                                             
         CLC   MGECODE,THISCODE    SAME CODE                                    
         BE    TT20                                                             
         OC    THISCODE,THISCODE   SKIP FIRST TIME                              
         BZ    TT20                                                             
         BAS   RE,TABTOT                                                        
*                                                                               
TT20     BAS   RE,ACCUM            ACCUMULATE THIS ENTRY                        
         LA    R4,MGERECL(R4)      BUMP TO NEXT POSITION                        
         B     TT10                                                             
*                                                                               
TT30     BAS   RE,TABTOT           ADD LAST TOTAL                               
         MVC   THISCODE,=X'FEFEFE' SET GRAND TOTAL                              
         MVC   MISSTOT,GMISSTOT                                                 
         MVC   MGTOT,GMGTOT                                                     
         MVC   MISSRTG,GMISSRTG                                                 
         MVC   MGRTG,GMGRTG                                                     
         BAS   RE,TABTOT           ADD GRAND TOTAL                              
*                                                                               
TTX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ACCUMULATE AMOUNTS IN THIS ENTRY                                              
***********************************************************************         
ACCUM    NTR1                                                                   
         CLI   MGESTYPE,MGESTYDT   DETAIL?                                      
         BNE   ACX                 NO, THEN NO AMOUNTS                          
*                                                                               
         MVC   THISCODE,MGECODE                                                 
*                                                                               
         ICM   R1,15,MGEDTCST      COST                                         
         ICM   R2,15,MGEDRTGB       BINARY RATING                               
*                                                                               
         CLI   MGETYPE,MGETYPNR    MISSED SPOT?                                 
         BNE   AC10                                                             
         L     R5,MISSTOT          MISSED TOTALS                                
         AR    R5,R1                                                            
         ST    R5,MISSTOT                                                       
         L     R5,GMISSTOT                                                      
         AR    R5,R1                                                            
         ST    R5,GMISSTOT                                                      
*                                                                               
         L     R5,MISSRTG                                                       
         AR    R5,R2                                                            
         ST    R5,MISSRTG                                                       
         L     R5,GMISSRTG                                                      
         AR    R5,R2                                                            
         ST    R5,GMISSRTG                                                      
         B     ACX                                                              
*                                                                               
AC10     CLI   MGETYPE,MGETYPOR    MAKEGOOD SPOT?                               
         BNE   ACX                                                              
*                                                                               
         L     R5,MGTOT            MAKEGOOD TOTALS                              
         AR    R5,R1                                                            
         ST    R5,MGTOT                                                         
         L     R5,GMGTOT                                                        
         AR    R5,R1                                                            
         ST    R5,GMGTOT                                                        
*                                                                               
         L     R5,MGRTG                                                         
         AR    R5,R2                                                            
         ST    R5,MGRTG                                                         
         L     R5,GMGRTG                                                        
         AR    R5,R2                                                            
         ST    R5,GMGRTG                                                        
*                                                                               
ACX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD A TOTAL RECORD                                                            
***********************************************************************         
ADDTOT   NTR1                                                                   
         CLI   BYTE3,C'Y'          WAS THE TOTAL RECORD WRITTEN BACK            
         BE    ADX                                                              
         MVC   ENTRY2,ENTRY                                                     
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
         XC    ENTRY,ENTRY                                                      
         MVC   MGECODE,THISCODE                                                 
         MVI   MGETYPE,MGETYPTR                                                 
*                                                                               
         MVC   MGETMISS,MISSTOT                                                 
         MVC   MGETMG,MGTOT                                                     
         MVC   MGEMSRTG,MISSRTG                                                 
         MVC   MGEMGRTG,MGRTG                                                   
         EDIT  MISSRTG,(4,MGEMSRTE),1                                           
         EDIT  MGRTG,(4,MGEMGRTE),1                                             
*                                                                               
         MVI   BYTE,TSAADD         ADD RECORD TO TSAR                           
         BAS   RE,CALLTSAR                                                      
*                                                                               
         TM    TSERRS,TSEDUP       DUPLICATE KEY                                
         BNO   *+6                                                              
         DC    H'0'                NO MORE SPACE ON TEMPEST FILE                
*                                                                               
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BNO   *+6                                                              
         DC    H'0'                NO MORE SPACE ON TEMPEST FILE                
*                                                                               
         MVC   ENTRY,ENTRY2                                                     
*                                                                               
         MVI   BYTE,TSARDH         RE-READ LAST RECORD READ                     
         BAS   RE,CALLTSAR                                                      
*                                                                               
ADX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GRAND TOTAL ENTRY                                                             
***********************************************************************         
TABTOT   NTR1                                                                   
         L     R1,ANENTRY          SET A(NEXT ENTRY)                            
*                                                                               
         XC    ENTRY,ENTRY                                                      
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
         MVC   MGECODE,THISCODE                                                 
         MVI   MGETYPE,MGETYPTR                                                 
*                                                                               
         MVC   MGETMISS,MISSTOT                                                 
         MVC   MGETMG,MGTOT                                                     
         MVC   MGEMSRTG,MISSRTG                                                 
         MVC   MGEMGRTG,MGRTG                                                   
         EDIT  MISSRTG,(4,MGEMSRTE),1                                           
         EDIT  MGRTG,(4,MGEMGRTE),1                                             
*                                                                               
         MVC   0(MGERECL,R1),ENTRY                                              
*                                                                               
         LH    R4,MGECNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         LA    R4,1(R4)                                                         
         STH   R4,MGECNT                                                        
*                                                                               
         LA    R1,MGERECL(R1)      BUMP TO NEXT ENTRY                           
         L     R4,ATABLEX          CHECK IF TABLE IS FULL                       
         CR    R1,R4                                                            
         BNL   TABFULER                                                         
         XC    0(MGERECL,R1),0(R1) CLEAR NEXT POSITION IN TABLE                 
         ST    R1,ANENTRY          SET A(NEXT ENTRY)                            
*                                                                               
         BAS   RE,CLRACCUM         CLEAR ACCUMULATORS                           
*                                                                               
TBX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PUT OUT TOTAL RECORD                                                          
***********************************************************************         
PUTTOT   NTR1                                                                   
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
         MVC   MGETMISS,MISSTOT                                                 
         MVC   MGETMG,MGTOT                                                     
         MVC   MGEMSRTG,MISSRTG                                                 
         MVC   MGEMGRTG,MGRTG                                                   
*                                                                               
         MVI   BYTE,TSAPUT         PUT RECORD TO TSAR                           
         BAS   RE,CALLTSAR                                                      
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* CLEAR THE ACCUMULATORS                                                        
***********************************************************************         
CLRACCUM DS    0H                                                               
         XC    MISSTOT,MISSTOT     CLEAR ACCUMULATORS                           
         XC    MGTOT,MGTOT                                                      
         XC    MISSRTG,MISSRTG                                                  
         XC    MGRTG,MGRTG                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD THE TABLE OF ENTRIES                                                    
***********************************************************************         
BLDTABLE NTR1                                                                   
         OC    MGETAB,MGETAB       IS AN AREA PASSED TO US                      
         BZ    BT10                                                             
         L     R4,MGETAB           A(TABLE)                                     
         OC    0(MGERECL,R4),0(R4) FIRST TIME IN - TABLE EMPTY                  
         BNZ   *+8                                                              
         ST    R4,ANENTRY          A(NEXT ENTRY)                                
         L     R4,ANENTRY                                                       
         L     R1,ATABLEX                                                       
         CR    R4,R1                                                            
         BNL   TABFULER                                                         
*                                                                               
         MVC   0(MGERECL,R4),ENTRY PUT ENTRY IN TABLE                           
         LA    R4,MGERECL(R4)      BUMP TO NEXT POSITION                        
         XC    0(MGERECL,R4),0(R4)                                              
         ST    R4,ANENTRY          SET A(NEXT ENTRY)                            
         B     BT20                                                             
*                                                                               
BT10     XC    TSARBLK,TSARBLK                                                  
         MVI   BYTE,TSAADD         ADD RECORD TO TSAR                           
         BAS   RE,CALLTSAR                                                      
*                                                                               
BT20     OC    MGEHOOK,MGEHOOK                                                  
         BZ    BTX                                                              
         L     RF,MGEHOOK          HOOK TO USER                                 
         L     RE,USERRD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF               MAY RETURN CC                                
*                                                                               
BTX      B     XIT                                                              
*                                                                               
TABFULER MVI   MGEERR,MGEQTFUL                                                  
         B     BTX                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* GET RATING FROM OFFER RECORD                                                  
*--------------------------------------------------------------------*          
         SPACE 1                                                                
GTMKORTG NTR1                                                                   
         XC    RATING,RATING                                                    
         XC    RATINGB,RATINGB                                                  
         MVI   MGEERR,MGEQDMIS     A(DEMOS) MISSING                             
         OC    MGEBRDEM,MGEBRDEM   NEED A(SVBRDEM)                              
         BZ    GORX                                                             
         OC    MGEDEM,MGEDEM       AND A(SVDEMS)                                
         BZ    GORX                                                             
         MVI   MGEERR,0                                                         
*                                                                               
         L     R3,MGEMGIO                                                       
         LA    R3,24(R3)                                                        
         MVI   ELCDLO,X'60'                                                     
         MVI   ELCDHI,X'60'                                                     
GOR05    BAS   RE,NEXTEL                                                        
*        BE    *+12                                                             
*        BAS   RE,GETRTG           IF NO DEMO ELEM JUST USE BUY INFO            
*        B     GORX                                                             
*                                                                               
         BNE   GORX                REMOVE DEFAULT DEMO LOOKUP                   
*                                                                               
         CLC   MGELINNO,2(R3)      OFFER NO.                                    
         BNE   GOR05                                                            
         CLC   MGERECNO,3(R3)      RECORD NO.                                   
         BNE   GOR05                                                            
*                                                                               
         L     R4,MGEBRDEM                                                      
         OC    0(118,R4),0(R4)                                                  
         BNZ   *+8                                                              
         L     R4,MGEDEM                                                        
*                                                                               
GOR10    CLI   1(R4),0             TEST E-O-L                                   
         BE    GORX                                                             
         XC    RATING,RATING                                                    
         XC    RATINGB,RATINGB                                                  
*                                                                               
         ZIC   R0,1(R3)            LENGTH                                       
         SH    R0,=H'4'                                                         
         BNP   GORX                                                             
         SRL   R0,3                R0 CONTAINS # DEMOS                          
         LA    R5,4(R3)                                                         
*                                                                               
GOR20    CLC   0(3,R4),0(R5)                                                    
         BE    GOR30                                                            
         LA    R5,8(R5)                                                         
         BCT   R0,GOR20                                                         
         LA    R4,3(R4)                                                         
         B     GOR10                                                            
*                                                                               
GOR30    L     R0,4(R5)                                                         
         N     R0,=X'7FFFFFFF'                                                  
         BAS   RE,EDITDEM                                                       
         TM    4(R5),X'80'                                                      
         BZ    *+8                                                              
         MVI   RATING+7,C'*'                                                    
*                                                                               
GORX     B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* GET RATING FROM NOTICE RECORD                                                 
*--------------------------------------------------------------------*          
         SPACE 1                                                                
GETRTG   NTR1                                                                   
         BAS   RE,READBUY                                                       
         XC    RATING,RATING                                                    
         XC    RATINGB,RATINGB                                                  
         MVI   MGEERR,MGEQDMIS     A(DEMOS) MISSING                             
         OC    MGEBRDEM,MGEBRDEM   NEED A(SVBRDEM)                              
         BZ    GRX                                                              
         OC    MGEDEM,MGEDEM       AND A(SVDEMS)                                
         BZ    GRX                                                              
         MVI   MGEERR,0                                                         
*                                                                               
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         L     R3,MGEBIO                                                        
         LA    R3,24(R3)                                                        
         CLI   0(R3),X'02'                                                      
         BE    *+12                                                             
         BAS   RE,NEXTEL                                                        
         BNE   GRX                                                              
*                                                                               
         L     R4,MGEBRDEM                                                      
         OC    0(118,R4),0(R4)                                                  
         BNZ   *+8                                                              
         L     R4,MGEDEM                                                        
*                                                                               
GR10     CLI   1(R4),0             TEST E-O-L                                   
         BE    GRX                                                              
         XC    RATING,RATING                                                    
         XC    RATINGB,RATINGB                                                  
*                                                                               
         ZIC   R0,1(R3)             FIND DEMO IN BUYREC                         
         SH    R0,=H'24'                                                        
         BNP   GRX                                                              
         SRL   R0,3                                                             
         LA    R5,24(R3)                                                        
*                                                                               
GR20     CLC   0(3,R4),0(R5)                                                    
         BE    GR30                                                             
         LA    R5,8(R5)                                                         
         BCT   R0,GR20                                                          
         LA    R4,3(R4)                                                         
         B     GR10                                                             
*                                                                               
GR30     L     R0,4(R5)                                                         
         N     R0,=X'7FFFFFFF'                                                  
         BAS   RE,EDITDEM                                                       
         TM    4(R5),X'80'                                                      
         BZ    *+8                                                              
         MVI   RATING+7,C'*'                                                    
*                                                                               
GRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET ADDRESS OF BUYREC INTO MGEBIO                                             
***********************************************************************         
READBUY  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING BUYREC,R2                                                        
         L     R1,MGEBKEY                                                       
         MVC   BUYKAM,0(R1)                                                     
         MVC   BUYKCLT,1(R1)                                                    
         MVC   BUYKPRD,3(R1)                                                    
         MVC   BUYMSTA,4(R1)                                                    
         MVC   BUYKEST,9(R1)                                                    
         MVC   BUYKBUY+1(1),MGELINNO                                            
         CLI   MGETYPE,MGETYPOR    OFFER?                                       
         BNE   *+8                                                              
         MVI   BUYKBUY+1,1                                                      
         GOTO1 HIGH                                                             
         MVC   COMMAND,=C'GETREC'                                               
         GOTO1 CDATAMGR,DMCB,COMMAND,=C'SPTFILE',KEY+14,MGEBIO,DMWORK           
         B     XIT                                                              
***********************************************************************         
* SUBROUTINE TO FORMAT DEMO AND HUT VALUES                                      
***********************************************************************         
EDITDEM  DS    0H                                                               
         ZIC   R1,3(R5)             GET HUT VALUE                               
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         IC    R1,X'64'                                                         
         MR    R0,R0                                                            
         AH    R1,=H'50'                                                        
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         STCM  R0,15,RATINGB                                                    
         TM    MGEOPT,MGONEDIT                                                  
         BNO   ED10                                                             
         STCM  R0,15,RATING                                                     
         BR    RE                                                               
*                                                                               
ED10     EDIT  (R0),(4,RATING),1                                                
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET NEXT AVAILABLE CODE                                                       
***********************************************************************         
GETCODE  NTR1                                                                   
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
         XC    ENTRY,ENTRY                                                      
         MVI   TSERRS,0                                                         
         SR    R2,R2               SET COUNTER                                  
*                                                                               
         MVI   BYTE,TSARDH         READ HI                                      
         B     GC12                                                             
*                                                                               
GC10     MVI   BYTE,TSANXT                                                      
*                                                                               
GC12     BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BZ    GC14                                                             
         LA    R2,1(R2)            USE NEXT NUMBER                              
         B     GC20                                                             
*                                                                               
GC14     CLI   MGETYPE,2           TEST MAKEGOOD                                
         BNE   GC10                NO - SKIP                                    
         CLM   R2,1,MGECODE        TEST THIS NUMBER MATCHES                     
         BE    GC10                YES - CONTINUE                               
         LA    R2,1(R2)            TRY NEXT NUMBER                              
         CLM   R2,1,MGECODE        IF DIFFERENT, IT'S AVAILABLE                 
         BE    GC10                ELSE CONTINUE                                
*                                                                               
GC20     CH    R2,=X'003C'         NEXT AVAILABLE CODE                          
         BNL   GCERR                                                            
         STC   R2,BCODE            SET UP TO TRANSLATE                          
         MVI   BYTE,0                                                           
         MVC   MGECODE,CODE                                                     
         MVC   MGEENTRY,ENTRY                                                   
         B     XIT                                                              
*                                                                               
GCERR    MVI   MGEERR,MGEQFULL     NO MORE CODES AVAILABLE                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD AN ENTRY TO TSAR                                                          
***********************************************************************         
ADDENTRY NTR1                                                                   
         MVC   ENTRY,MGEENTRY      SET UP ENTRY PASSED BY USER                  
         MVI   BYTE,TSAADD         ADD RECORD TO TSAR                           
         BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSEDUP       DUPLICATE KEY                                
         BNO   *+8                                                              
         MVI   MGEERR,MGEQDUP                                                   
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BNO   *+8                                                              
         MVI   MGEERR,MGEQEOF                                                   
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* FIND AN ENTRY - KEY IN MGEENTRY                                               
***********************************************************************         
FNDENTRY NTR1                                                                   
         MVC   ENTRY,MGEENTRY                                                   
         XC    TSRNUM,TSRNUM                                                    
         MVI   BYTE,TSARDH         READ HI                                      
         BAS   RE,CALLTSAR                                                      
         CLI   TSERRS,0            IS THERE AN ERROR                            
         BE    *+8                                                              
         MVI   MGEERR,MGEQNF                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DELETE AN ENTRY FROM TSAR                                                     
***********************************************************************         
DELENTRY NTR1                                                                   
         MVC   ENTRY,MGEENTRY      SET UP ENTRY PASSED BY USER                  
         MVI   BYTE,TSADEL         DELETE A RECORD FROM TSAR                    
         BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSERNF       RECORD NOT FOUND                             
         BO    DE10                                                             
         LH    R1,MGECNT           DECREMENT NUMBER OF ENTRIES IN TABLE         
         BCTR  R1,0                                                             
         STH   R1,MGECNT                                                        
         B     *+8                                                              
*                                                                               
DE10     MVI   MGEERR,MGEQNF                                                    
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* FIND NEXT ENTRY - TSRNUM IN MGETSNUM                                          
***********************************************************************         
NXTENTRY NTR1                                                                   
         MVC   TSRNUM,MGETSNUM                                                  
         MVI   BYTE,TSANXT         READ NEXT                                    
         BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BE    *+8                                                              
         MVI   MGEERR,MGEQEOF                                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET AN ENTRY - TSRNUM IN MGETSNUM                                             
***********************************************************************         
GETENTRY NTR1                                                                   
         MVC   TSRNUM,MGETSNUM                                                  
         MVI   BYTE,TSAGET         READ NEXT                                    
         XC    MGEENTRY,MGEENTRY                                                
         BAS   RE,CALLTSAR                                                      
         CLI   TSERRS,0                                                         
         BE    GEX                                                              
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BNE   GE10                                                             
         MVI   MGEERR,MGEQEOF                                                   
         B     GEX                                                              
*                                                                               
GE10     TM    TSERRS,TSERNF       RECORD NOT FOUND                             
         BNO   GEX                                                              
         MVI   MGEERR,MGEQNF                                                    
*                                                                               
GEX      B     XIT                                                              
         EJECT                                                                  
*        EDIT OUT TO TOTAL LINE                                                 
*                                                                               
PRTAMT   NTR1                                                                   
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(8,0(R5)),FLOAT=$                                           
         B     XIT                                                              
*                                                                               
*        EDIT OUT RATINGS TO TOTAL LINE                                         
*                                                                               
PRTRTG   NTR1                                                                   
**********((T  (R0),(5,0(R5)),1                                                 
         EDIT  (R0),(8,0(R5)),1                                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* READ HI - MGEENTRY SET                                                        
***********************************************************************         
RDHI     NTR1                                                                   
         XC    TSRNUM,TSRNUM                                                    
         MVC   ENTRY,MGEENTRY                                                   
         MVI   BYTE,TSARDH         READ HIGH                                    
         BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BE    *+8                                                              
         MVI   MGEERR,MGEQEOF                                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CALL TSAR                                                                     
***********************************************************************         
CALLTSAR NTR1                                                                   
         LA    R8,TSARBLK                                                       
         USING TSARD,R8                                                         
         MVC   TSACOM,MGEACOM      A(COMFACS)                                   
         MVI   TSPAGL,3            USE TEMPSTR PAGE 3                           
         MVI   TSPAGN,2            USE 2 PAGES                                  
         MVI   TSKEYL,MGEKEYL      KEY LENGTH                                   
         LA    R1,MGERECL          RECORD LENGTH                                
         STH   R1,TSRECL                                                        
         OI    TSINDS,TSIXTTWA     14K RECORDS                                  
         MVC   TSACTN,BYTE                                                      
         LA    R1,ENTRY            A(RECORD)                                    
         ST    R1,TSAREC                                                        
         GOTO1 MGETSAR,TSARBLK                                                  
         MVC   MGETSNUM,TSRNUM     RETURN TSAR RECORD NUMBER                    
         MVC   MGEENTRY,ENTRY                                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATAMGR ROUTINES                                                              
***********************************************************************         
HIGH     NTR1                                                                   
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIR                                                              
*                                                                               
SEQ      NTR1                                                                   
         MVC   COMMAND,=C'DMRSEQ'                                               
*                                                                               
DIR      GOTO1 CDATAMGR,DMCB,COMMAND,=C'SPTDIR',KEY,KEY                         
         B     DIRX                                                             
*                                                                               
GETREC   NTR1                                                                   
         MVC   COMMAND,=C'GETREC'                                               
         GOTO1 CDATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,AIO,DMWORK           
*                                                                               
DIRX     CLI   DMCB+8,0            TEST DATAMGR ERROR                           
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
SPACES   DC    80C' '                                                           
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
MGEXIT   XMOD1 1                                                                
         EJECT                                                                  
NEXTEL   CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
NEXTEL2  CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R3)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R3)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
USERRD   DS    A                                                                
ANENTRY  DS    A                                                                
AIO      DS    A                                                                
DUB      DS    D                                                                
DMWORK   DS    12D                                                              
TSARBLK  DS    CL48                                                             
DMCB     DS    6F                                                               
SAVER3   DS    F                                                                
MISSTOT  DS    F                   TOTAL ACCUMULATORS                           
MGTOT    DS    F                                                                
GMISSTOT DS    F                                                                
GMGTOT   DS    F                                                                
MISSRTG  DS    F                                                                
MGRTG    DS    F                                                                
GMISSRTG DS    F                                                                
GMGRTG   DS    F                                                                
*                                                                               
BUYCOST  DS    F                                                                
LASTCOST DS    F                                                                
ATABLEX  DS    F                                                                
DATADISP DS    H                                                                
HALF     DS    H                                                                
COUNTER  DS    XL1                                                              
COUNTER2 DS    XL1                                                              
BYTE     DS    XL1                                                              
BYTE2    DS    XL1                                                              
BYTE3    DS    XL1                                                              
BYTE4    DS    XL1                                                              
SVELCDS  DS    XL2                                                              
KEY      DS    CL20                                                             
KEYSAVE  DS    CL20                                                             
WORK     DS    CL48                                                             
EMEL     DS    CL48                                                             
RATING   DS    CL10                                                             
RATINGB  DS    XL4                                                              
ELCODE   DS    XL1                                                              
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
BUYCIND  DS    XL1                                                              
COMMAND  DS    CL8                                                              
ENTRY    DS    CL(MGERECL)                                                      
ENTRY2   DS    CL(MGERECL)                                                      
OFRRCNTH DS    XL1                 NTH BUY DETAIL NTRY FOR OFFER/RECORD         
THISCODE DS    CL3                                                              
CODE     DS    CL2                                                              
BCODE    DS    XL1                                                              
PRD1     DS    XL1                                                              
SLN1     DS    XL1                                                              
PRD2     DS    XL1                                                              
SLN2     DS    XL1                                                              
SVDTIMES DS    XL4                 SAVED TIMES FROM MG BUY ELEMENT              
SVDOWROT DS    XL1                 SAVED OUT OF WEEK ROTATOR BYTE               
SVDDYBTS DS    XL1                 SAVED DAY BITS                               
SVDTSLN  DS    XL1                 SAVED SPOT LENGTH FROM MG BUY ELEM           
SVDDYPRT DS    CL1                 SAVED DAYPART                                
SVDPRGM  DS    CL17                SAVED PROGRAM TEXT                           
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPMGED2                                                        
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
*SPGENBUY                                                                       
*SPGENDRORD                                                                     
*SPGENDRMKN                                                                     
*SPGENDRMKO                                                                     
*DDCOMFACSD                                                                     
*DDCOREQUS                                                                      
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPGENDRMKN                                                     
       ++INCLUDE SPGENDRMKO                                                     
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'138SPBLDMGE  05/01/02'                                      
         END                                                                    
