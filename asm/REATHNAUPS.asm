*          DATA SET REATHNAUPS AT LEVEL 118 AS OF 05/01/02                      
*PHASE REATNUPA,*                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE BUFFAHI                                                                
*INCLUDE COVAIL                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE REPVALMN                                                               
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'REATHNAUP - REPPAK ATHENA FILE UPDATE'                          
********************************************************************            
*   HISTORY OF CHANGES                                             *            
********************************************************************            
*   MAY19/92 (BU ) --- TEMPORARY CHANGE TO BYPASS BAD RECORD ON    *            
*                      RECOVERY TAPE.                              *            
*                                                                  *            
*   MAY27/92 (BU ) --- DEACTIVATE ABOVE CHANGE, EXPAND IO AREAS    *            
*                                                                  *            
*   FEB18/93 (BU ) --- UPGRADE UTL ACCESS FOR > 1 REP SYSTEM #     *            
*                                                                  *            
*   FEB01/95 (BU ) --- BYPASS RECORDS WITH NO X'02' DAY/TIME ELTS  *            
*                                                                  *            
*   SEP20/00 (BU ) --- EXPAND MTHTBL + NEW SSB                     *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                      ***  END TOMBSTONE  ***                     *            
********************************************************************            
REATNUP  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NBASE 0,REATNUP,VREGSAVE,R9,RA                                         
***      LA    RC,2048(RB)                                                      
***      LA    RC,2048(RC)                                                      
***      LA    RA,2048(RC)                                                      
***      LA    RA,2048(RA)                                                      
***      USING REATNUP+4096,RC,RA                                               
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         L     RE,=A(IO1)                                                       
         ST    RE,AIO1                                                          
         ST    RE,AIO                                                           
         L     RE,=A(IO2)                                                       
         ST    RE,AIO2                                                          
         L     RE,=A(IO3)                                                       
         ST    RE,AIO3                                                          
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         L     R4,=V(STXITER)                                                   
         ST    R4,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),DMCB,WORK                                            
         GOTO1 =V(DATCON),(R1),(5,WORK),(2,TODAY2)                              
         BAS   R5,GETMONDT         GET MONDAY DATE                              
*                                                                               
IN100    GOTO1 =V(CARDS),DMCB,WORK,=C'RE00'                                     
         CLC   =C'/*',WORK                                                      
         BE    IN200                                                            
         CLC   =C'WRITE=Y',WORK                                                 
         BNE   *+16                                                             
         MVI   WRITEFLG,C'Y'                                                    
         MVI   FLIST,C'U'                                                       
         B     IN100                                                            
*                                                                               
         CLC   =C'DATE=',WORK                                                   
         BNE   IN120                                                            
         GOTO1 =V(DATVAL),DMCB,(0,WORK+5),DUB                                   
         OC    DMCB(4),DMCB                                                     
         BZ    INCANCEL                                                         
         GOTO1 =V(DATCON),(R1),(0,DUB),(2,TODAY2)                               
         BAS   R5,GETMONDT         GET MONDAY DATE                              
         B     IN100                                                            
*                                                                               
IN120    CLC   =C'MED=',WORK                                                    
         BNE   *+14                                                             
         MVC   MED,WORK+4                                                       
         B     IN100                                                            
*                                                                               
         CLC   =C'REP=',WORK                                                    
         BNE   *+14                                                             
         MVC   REP,WORK+4                                                       
         B     IN100                                                            
*                                                                               
****     CLC   =C'STE=',WORK                                                    
****     BNE   IN140                                                            
****     MVC   STAGEFIL,WORK+4                                                  
****     CLI   STAGEFIL+6,C'T'                                                  
****     BNE   IN100                                                            
****     MVI   STAGEFIL+6,C' '                                                  
****     B     IN100                                                            
*                                                                               
IN140    CLC   =C'STA=',WORK                                                    
         BNE   IN150                                                            
         MVC   STAGFIL,WORK+4                                                   
         CLI   STAGFIL+6,C'T'                                                   
         BNE   IN100                                                            
         MVI   STAGFIL+6,C' '                                                   
         B     IN100                                                            
*                                                                               
IN150    CLC   =C'ID=',WORK                                                     
         BNE   IN160                                                            
         MVC   SAVNAME,WORK+3                                                   
         B     IN100                                                            
IN160    CLC   =C'LOCAL=',WORK                                                  
         BNE   IN170                                                            
         MVC   SAVLOCAL,WORK+6                                                  
         B     IN100                                                            
*                                                                               
IN170    EQU   *                                                                
*                                                                               
         CLC   =C'YEAR=',WORK      YEAR FILTER ENTERED?                         
         BNE   IN175               NO - SO CONTINUE                             
*                                                                               
         MVC   WORK+7(4),=C'0101'  FORCE TO JAN1/YY                             
         GOTO1 =V(DATCON),DMCB,(0,WORK+5),(3,WORK+11)                           
         MVC   YEAR,WORK+11        MOVE IN THE BINARY YEAR                      
         B     IN100                                                            
*                                                                               
IN175    EQU   *                                                                
*                                                                               
         CLC   =C'OFFICE=',WORK    OFFICE FILTER ENTERED?                       
         BNE   IN180               NO - SO CONTINUE                             
*                                                                               
         MVC   OFFICE,WORK+7       ELSE - MOVE IN OFFICE FILTER                 
         B     IN100               AND LOOP BACK                                
*                                                                               
IN180    EQU   *                                                                
*                                                                               
INCANCEL MVC   P(30),=C'*** UNKNOWN PARAMETER CARD ***'                         
INCAN2   GOTO1 =V(LOGIO),DMCB,1,(30,P)                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),WORK                                                       
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
*                                                                               
IN200    OC    REP,REP                                                          
         BNZ   IN220                                                            
         MVC   P(30),=CL30'** REP CARD MISSING (REP=) **'                       
         B     INCAN2                                                           
*                                                                               
IN220    L     R1,=A(BUFFALOC)                                                  
         ST    R1,BUFFC                                                         
         GOTO1 =V(COVAIL),DMCB,C'SETB',3000,600000,BUFFC                        
         ICM   R1,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFC,DMCB+12                                                    
         GOTO1 =V(BUFFALO),DMCB,=C'SET',BUFFC                                   
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE IDENTIFICATION                   
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',AIO,0                                              
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
         MVC   WORK+15(10),SAVNAME LOAD AGENCY NAME                             
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO                  
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,AIO                                                           
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0020            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0030            YES                                          
CTRL0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   CTRL0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0030 EQU   *                                                                
         MVC   UTL+4(1),3(R1)      OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'REP',FLIST,AIO                    
*                                                                               
         MVI   DATADISP+1,34                                                    
         XC    KEY,KEY                                                          
         MVI   KEY+RSDDKTYP-RSDDKEY,X'26'                                       
         MVC   KEY+RSDDKREP-RSDDKEY(2),REP                                      
         MVC   KEY+RSDDKSTA-RSDDKEY(4),=4XL1'FF'                                
         GOTO1 HIGH                                                             
         CLC   KEY(26),KEYSAVE     FOUND DEFAULT                                
         BNE   IN300                                                            
         GOTO1 GETREC                                                           
         CLI   KEY+26,C' '         TV ?                                         
         BNE   IN240                                                            
         L     R6,AIO                                                           
         LA    R2,DEFSDDT                                                       
         BAS   RE,SVSDD                                                         
*                                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(26),KEYSAVE     FOUND RADIO DEFAULT                          
         BNE   IN300                                                            
         GOTO1 GETREC                                                           
IN240    LA    R2,DEFSDDR                                                       
         L     R6,AIO                                                           
         BAS   RE,SVSDD                                                         
*                                                                               
IN300    MVI   FRST,C'Y'                                                        
         EJECT                                                                  
* READ DAILY RECOVERY TAPE                                                      
* TEST FOR A CONTRACT RECORD                                                    
RR100    DS    0H                                                               
         LA    R1,RECVIN                                                        
         L     R0,AIO                                                           
         LR    R2,R0                                                            
         GET   (1),(0)                                                          
         SPACE                                                                  
* SET 2X'00' AT EOR                                                             
         LR    RE,R2                                                            
         AH    RE,0(R2)            POINT TO END OF REC                          
         XC    0(2,RE),0(RE)       CLEAR ELCODE AND LEN                         
*                                                                               
         LA    R2,4(R2)                                                         
         USING RECVHDR,R2                                                       
*                                                                               
         CLI   RFILTY,X'81'        DIRECTORY POINTER?                           
         BE    RR100               YES - BYPASS THIS RECORD                     
*                                                                               
         LA    R6,24(R2)                                                        
         L     R7,AIO2                                                          
         USING SORTRECD,R7                                                      
         XC    0(43,R7),0(R7)      CLEAR KEY AREA                               
         CLC   0(2,R6),=X'0C00'    CONTRACT RECORD                              
         BNE   RR400                                                            
         USING RCONKEY,R6                                                       
         CLC   RCONKREP,REP        TEST CORRECT REP                             
         BNE   RR100               NO - SAVE IT                                 
*                                                                               
         OC    OFFICE,OFFICE       OFFICE FILTER ENTERED?                       
         BZ    RR120               NO - SO CONTINUE                             
*                                                                               
         CLC   RCONKOFF,OFFICE     FOR REQUESTED OFFICE?                        
         BNE   RR100               NO - SO CONTINUE                             
*                                                                               
RR120    EQU   *                                                                
*                                                                               
         OC    MED,MED                                                          
         BZ    RR200                                                            
         CLI   MED,C'T'                                                         
         BNE   *+16                                                             
         CLI   RCONKSTA+4,C' '                                                  
         BNE   RR100                                                            
         B     *+12                                                             
*                                                                               
         CLI   RCONKSTA+4,C' '                                                  
         BE    RR100                                                            
*                                                                               
RR200    EQU   *                                                                
****     OC    STAGEFIL,STAGEFIL                                                
****     BZ    RR220                                                            
****     CLC   RCONKGRP(7),STAGEFIL                                             
****     BH    RR100                                                            
*                                                                               
RR220    OC    STAGFIL,STAGFIL                                                  
         BZ    *+14                                                             
         CLC   RCONKGRP(7),STAGFIL                                              
         BL    RR100                                                            
*                                                                               
RR240    CLI   RRECTY,COPY                                                      
         BNE   RR260                                                            
         MVC   SVOCATG,RCONCTGY                                                 
         MVC   SVOSRV,RCONRTGS                                                  
         MVC   SVOADV,RCONKADV                                                  
         MVC   SVOPRD,SPACES                                                    
         MVC   SVOGSTA,RCONKGRP                                                 
         B     RR100                                                            
*                                                                               
RR260    ZIC   RE,RRECTY                                                        
         LA    RE,1(RE)            ACTION FOR SORT ADD=1 COPY=2 CHG=3           
         STC   RE,SRACT                                                         
         CLI   RRECTY,3            ADD ?                                        
         BNE   *+8                                                              
         MVI   SRACT,SRTADDE       MAKE ADD COME BEFORE CHANGE IN SORT          
*                                                                               
         LR    R4,R6                                                            
         BAS   RE,SETCONSR                                                      
         B     RR100                                                            
*                                                                               
*                                                                               
         EJECT                                                                  
* TEST FOR A BUY                                                                
RR400    CLI   0(R6),X'0B'         TEST FOR BUY RECORD                          
         BNE   RR100               NO READ NEXT RECORD                          
         USING RBUYKEY,R6                                                       
         CLC   RBUYKREP,REP        TEST CORRECT REP                             
         BNE   RR100               NO - SKIP IT                                 
         CLC   =XL2'FFFF',RBUYKMLN SKIP PLAN                                    
         BE    RR100                                                            
*                                                                               
         LA    R1,CNLSTTAB         SEE IF NEW CONTRACT                          
         LA    R0,CNLSTTBX         END OF LIST                                  
RR420    OC    0(4,R1),0(R1)       NO MORE CONTRACTS                            
         BZ    RR440                                                            
         CLC   0(4,R1),RBUYKCON    IS CONTRACT # IN LIST                        
         BNE   *+14                                                             
         MVC   SRSTAG(7),4(R1)                                                  
         B     RR500                                                            
*                                                                               
         LA    R1,11(R1)                                                        
         CR    R0,R1                                                            
         BH    RR420                                                            
*                                                                               
RR440    XC    KEY,KEY             READ CONTRACT                                
         MVI   KEY,X'8C'                                                        
         MVC   KEY+(RCONPREP-RCONKEY)(2),REP                                    
         PACK  KEY+23(1),RBUYKCON+3(1)                                          
         PACK  KEY+24(1),RBUYKCON+2(1)                                          
         PACK  KEY+25(1),RBUYKCON+1(1)                                          
         PACK  KEY+26(1),RBUYKCON(1)                                            
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         BAS   RE,GETREC                                                        
         MVC   AIO,AIO1                                                         
         L     R4,AIO3                                                          
         MVC   SRSTAG,RCONKGRP-RCONKEY(R4) SET ADDR OF GRP/STA                  
         MVI   SRACT,MYCON                                                      
         BAS   RE,SETCONSR                                                      
*                                                                               
RR500    OC    MED,MED                                                          
         BZ    RR520                                                            
         CLI   MED,C'T'                                                         
         BNE   *+16                                                             
         CLI   SRSTAG+6,C' '                                                    
         BNE   RR100                                                            
         B     *+12                                                             
*                                                                               
         CLI   SRSTAG+6,C' '                                                    
         BE    RR100                                                            
*                                                                               
RR520    EQU   *                                                                
****     OC    STAGEFIL,STAGEFIL                                                
****     BZ    RR540                                                            
****     CLC   SRSTAG,STAGEFIL                                                  
****     BH    RR100                                                            
*                                                                               
RR540    OC    STAGFIL,STAGFIL                                                  
         BZ    *+14                                                             
         CLC   SRSTAG,STAGFIL                                                   
         BL    RR100                                                            
*                                                                               
         MVC   SRCON#,RBUYKCON                                                  
         MVI   SRREC,X'02'         BUY RECORD                                   
         MVC   SRACT,RRECTY                                                     
         MVC   SRBTYP,RBUYTYP                                                   
         MVC   SRBDUR,RBUYDUR                                                   
         MVC   SRBNW,RBUYNW                                                     
         MVC   SRBCOS,RBUYCOS                                                   
         MVC   SRBCNTL,RBUYCNTL                                                 
         MVI   SRBPLN,C'N'                                                      
         CLC   RBUYKPLN,=3X'FF'                                                 
         BE    *+8                                                              
         MVI   SRBPLN,C'Y'                                                      
         MVC   SRLEN(2),=H'31'                                                  
         L     RE,RECCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,RECCNT                                                        
         STCM  RE,15,SRNUM         KEEP COPY/CHANGE CONSECUTIVE                 
*                                                                               
         LA    R5,SRBDTM                                                        
         LR    R4,R6                                                            
         MVI   ELCODE,X'02'                                                     
RR600    BAS   RE,GETEL                                                         
         B     *+8                                                              
RR620    BAS   RE,NEXTEL                                                        
         BNE   RR700                                                            
         SR    RE,RE                                                            
         ICM   RE,3,SRLEN                                                       
         ZIC   RF,1(R6)                                                         
         AR    RE,RF                                                            
         STCM  RE,3,SRLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,MVCELEM                                                       
         AR    R5,RF                                                            
         LA    R5,1(R5)                                                         
         B     RR620                                                            
*                                                                               
RR700    CLI   ELCODE,X'07'                                                     
         BE    RR720                                                            
         LR    R6,R4                                                            
         CLI   ELCODE,X'02'                                                     
         BNE   *+12                                                             
         MVI   ELCODE,X'03'                                                     
         B     RR600                                                            
         CLI   ELCODE,X'03'                                                     
         BNE   *+12                                                             
         MVI   ELCODE,X'06'                                                     
         B     RR600                                                            
         MVI   ELCODE,X'07'                                                     
         B     RR600                                                            
*                                                                               
RR720    AP    INCNTB,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRLEN                                    
         B     RR100               AND CONTINUE                                 
RREND    DS    0H                                                               
MVCELEM  MVC   0(0,R5),0(R6)                                                    
         DROP  R2,R6,R7                                                         
         EJECT                                                                  
*******************************************************************             
*                                                                 *             
*            -----     MAIN LINE CODE     -----                   *             
*                                                                 *             
*            -----       END = ENDIT      -----                   *             
*                                                                 *             
* READ SORT FILE 1ST ANALYSE CONTRACT CHANGES - SET CHANGE        *             
* AREAS TO TRIGGER TOTAL CONTRACT # CHANGE (READ ALL BUYS FOR     *             
* CONTRACT PUT BUFFALO RECORDS WITH NEG/POS SPOTS DOLLARS)        *             
* IF NO AREAS CHANGE TO TRIGGER TOTAL CHANGE & AFTER TOTAL CHANGE *             
* READ BUYS FROM SORT AND CREATE BUFFALO RECORDS WITH CHANGED     *             
* AMOUNTS. WHEN A STATION BREAK OCCURS READ BUFFALO RECORDS BACK  *             
* READ ATHNEA RECORD AND MAKE CHANGES. READ NEXT STATION FROM     *             
* SORTER AND START AGAIN.                                         *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
ML100    CLOSE RECVIN                                                           
         MVC   P(9),=C'CONTRACTS'                                               
         EDIT  (P4,INCNTC),(7,P+11)                                             
         MVC   P+20(4),=C'BUYS'                                                 
         EDIT  (P4,INCNTB),(7,P+26)                                             
         GOTO1 =V(PRINTER)                                                      
         SPACE                                                                  
ML120    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         BZ    ENDIT                                                            
*                                                                               
* MOVE RECORD *                                                                 
         L     R7,AIO1                                                          
         USING SORTRECD,R7                                                      
         LH    RE,0(R2)            GET LENGTH                                   
ML140    CH    RE,=H'256'                                                       
         BNH   ML160                                                            
         MVC   0(256,R7),0(R2)                                                  
         LA    R7,256(R7)                                                       
         LA    R2,256(R2)                                                       
         SH    RE,=H'256'                                                       
         B     ML140                                                            
ML160    BCTR  RE,0                                                             
         EX    RE,MVCREC                                                        
*                                                                               
         LA    R7,1(RE,R7)         POINT TO END OF REC                          
         XC    0(2,R7),0(R7)       CLEAR NEXT ELEM CODE/LENGTH                  
         L     R7,AIO1                                                          
*        GOTO1 =V(HEXOUT),DMCB,0(R7),P,60,=C'T0G'                               
*        GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   REJSTA,SRSTAG                                                    
         BE    ML120                                                            
         XC    REJSTA,REJSTA                                                    
*                                                                               
         CLI   SRREC,1             CONTRACT ?                                   
         BNE   ML300               - NO DO BUY CHANGES                          
         CLI   FRST,C'Y'                                                        
         BE    ML220                                                            
*                                                                               
* STA BREAK & NEW CONTRACT # - ADD/CHG ATHENA RECS - PRINT & CLEAR STA          
         CLC   SRSTAG,SVSRKEY+SVGSTAE                                           
         BE    ML200                                                            
         CLI   ABUYSW,C'Y'         IF LAST CON HAD BUYS SKIP CON CHGS           
         BE    *+8                                                              
         BAS   RE,READBUY          TYPE CODE CHG READ ALL BUYS FOR CON#         
         XC    BUYCHGN,BUYCHGN                                                  
         BAS   RE,CHGATHNA                                                      
         BZ    *+12                NOTHING FOR THIS STATION                     
         BAS   RE,PRNTSTA                                                       
         BAS   RE,RSETBUFF         RESET STATION BUFFALO                        
         XC    LSTSTA,LSTSTA                                                    
         B     ML220                                                            
*                                                                               
* NO STATION BREAK BUT NEW CONTRACT #                                           
ML200    CLC   SRCON#,SVSRKEY+SVCONE NO STA BREAK BUT NEW CONTRACT #            
         BE    ML240                                                            
         CLI   ABUYSW,C'Y'         IF THERE WHERE BUY CHG SKIP CON CHGS         
         BE    *+8                                                              
         BAS   RE,READBUY          TYPE CODE CHG READ ALL BUYS FOR CON#         
         XC    BUYCHGN,BUYCHGN                                                  
*                                                                               
ML220    MVI   ABUYSW,C'N'                                                      
         XC    NCDTES,NCDTES                                                    
         MVI   FSTCHG,C'Y'                                                      
         MVI   ADDCNT,C'N'                                                      
ML240    MVC   SVSRKEY,0(R7)                                                    
         BAS   RE,SETCNTRT                                                      
*        MVC   P(34),FSTCHG                                                     
*        GOTO1 =V(HEXOUT),DMCB,NCNTL,P+36,12,=C'T0G'                            
*        GOTO1 =V(PRINTER)                                                      
         MVI   FRST,C'N'                                                        
         B     ML120                                                            
*                                                                               
ML300    CLI   ABUYSW,C'Y'         IF THERE WHERE BUY CHG SKIP CON CHGS         
         BE    *+8                                                              
         BAS   RE,READBUY          TYPE CODE CHG READ ALL BUYS FOR CON#         
         ICM   RE,15,BUYCHGN                                                    
         BZ    ML320                                                            
         CLI   SRACT,3             SKIP ADDS                                    
         BE    ML360                                                            
         CLC   SRNUM,BUYCHGN                                                    
         BNL   ML360                                                            
         MVC   SVSTA,OGRSTA                                                     
         BAS   RE,GETSDD                                                        
ML320    CLI   SRACT,CHANGE                                                     
         BNE   *+12                                                             
         TM    SRBCNTL,X'80'                                                    
         BO    ML360                                                            
         MVC   NCNTL,SRBCNTL                                                    
         MVC   NDUR,SRBDUR                                                      
         MVC   NTYP,SRBTYP                                                      
         MVC   NNW,SRBNW                                                        
         MVC   DUB,SRBCOS          WORD ALIGN                                   
         MVI   PLANSW,C'N'                                                      
         CLI   SRBPLN,C'Y'                                                      
         BNE   *+8                                                              
         MVI   PLANSW,C'Y'                                                      
         LA    R6,SRBDTM           ADDR START OF DAY/TIME ELEMENTS              
         ST    R6,ADYTMEL                                                       
*                                                                               
*   FOLLOWING TEST FOR RECORDS WITH NO X'02' DAY/TIME ELEMENTS                  
*                                                                               
         CLI   0(R6),X'02'         DAY/TIME ELEMENT?                            
         BNE   ML120               NO  - SKIP THIS RECORD                       
         BAS   RE,CALCBUY                                                       
ML360    MVI   ABUYSW,C'Y'         HAD A BUY ALSO NOT 1ST BUY THIS CON          
         B     ML120                                                            
*                                                                               
ENDIT    CLI   FRST,C'Y'                                                        
         BNE   EN100                                                            
         MVC   P(25),=C'NO ATHENA RECORDS UPDATED'                              
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
         SPACE                                                                  
EN100    CLI   ABUYSW,C'Y'         IF THERE WHERE BUY CHG SKIP CON CHGS         
         BE    *+8                                                              
         BAS   RE,READBUY          TYPE CODE CHG READ ALL BUYS FOR CON#         
         BAS   RE,CHGATHNA                                                      
         BZ    *+8                 NOTHING FOR THIS STATION                     
         BAS   RE,PRNTSTA                                                       
         BAS   RE,FINALPRT                                                      
EXIT     XIT1                                                                   
MVCREC   MVC   0(0,R7),0(R2)                                                    
         EJECT                                                                  
***********************************************************************         
*  2 TYPES OF CONTRACT RECORDS CHANGE/ADD - 1ST CHG TAKES OLD TYPES   *         
*    & NEW TYPES - ALL OTHER CHANGES JUST NEW TYPES                   *         
*  IF AN ADD NO OLD TYPES (NOT ADDED TO ATHENA FILE UNTIL TODAY)      *         
*  ROUTINE BUILDS MONTH TABLES FROM LAST CON (CON DATES ONLY GROW)    *         
*  READS FOR STATION SDD RECORD / IF STA HAS NO SDD REC SETS REJSTA   *         
***********************************************************************         
         SPACE 2                                                                
SETCNTRT NTR1                                                                   
         OC    NCDTES(6),NCDTES                                                 
         BNZ   *+14                                                             
         MVC   NCDTES(6),SRCDTES   SET UP DATES                                 
         B     CT100                                                            
         CLC   NCDTES(3),SRCDTES                                                
         BL    *+10                                                             
         MVC   NCDTES(3),SRCDTES   SET UP LOWEST DATE                           
         CLC   SRCDTES+3(3),NCDTES+3                                            
         BL    *+10                                                             
         MVC   NCDTES+3(3),SRCDTES+3 SET UP HIGHEST DATE                        
CT100    CLI   SRACT,MYCON         IF JUST READ FROM FILE NEWEST                
         BNE   CT120                                                            
         CLI   FSTCHG,C'Y'                                                      
         BE    CT160                                                            
         B     CT300               IF CONTRACT NOT CHG DON'T USE TYPES          
CT120    CLI   ADDCNT,C'Y'         IF ADDED TODAY NO READ OF BUYS               
         BE    CT200                                                            
         CLI   SRACT,SRTADDE                                                    
         BNE   CT140                                                            
         MVI   ADDCNT,C'Y'         CONTRACT ADDED TODAY                         
         B     CT200                                                            
*                                                                               
* CHANGE NO ADD (ADD ALWAYS 1ST) 1ST ONLY SAVE OLD TYPES                        
CT140    CLI   FSTCHG,C'Y'                                                      
         BNE   CT200                                                            
         MVI   FSTCHG,C'N'                                                      
CT160    MVC   OSERV,SRCOSER                                                    
         MVC   OCATG,SRCOCATG                                                   
         MVC   OADV,SRCOADV                                                     
         MVC   OPRD,SPACES                                                      
*                                                                               
CT200    MVC   NSERV,SRCSER                                                     
         MVC   NCATG,SRCCATG                                                    
         MVC   NADV,SRCADV                                                      
         MVC   NPRD,SPACES                                                      
         MVC   NGRSTA,SRSTAG                                                    
         CLI   SRACT,4             IF I READ NO CHANGE                          
         BE    CT300                                                            
         CLI   SRACT,1             ADD                                          
         BE    CT300                                                            
         OC    BUYCHGN,BUYCHGN                                                  
         BNZ   CT300                                                            
         CLC   SRCOGSTA,SRSTAG                                                  
         BE    CT300                                                            
         MVC   OGRSTA,SRCOGSTA                                                  
         MVC   BUYCHGN,SRCBUYN                                                  
*CT300   GOTO1 =V(HEXOUT),DMCB,OGRSTA,P+22,18,=C'T0G'                           
*        GOTO1 =V(PRINTER)                                                      
*                                                                               
CT300    CLC   SVSTA,SVSRKEY+SVGSTAE                                            
         BE    EXIT                                                             
         MVC   SVSTA,SVSRKEY+SVGSTAE                                            
         BAS   RE,GETSDD                                                        
         B     EXIT                                                             
*                                                                               
* GET SDD RECORD                                                                
GETSDD   ST    RE,FULL                                                          
         XC    KEY,KEY                                                          
         MVI   KEY+RSDDKTYP-RSDDKEY,X'26'                                       
         MVC   KEY+RSDDKREP-RSDDKEY(2),REP                                      
         MVC   KEY+RSDDKSTA-RSDDKEY(5),SVSTA+2                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND STATION SDD                            
         BNE   DD100                                                            
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO3                                                          
         LA    R2,STASDDT                                                       
         BAS   RE,SVSDD                                                         
         B     DD200                                                            
         MVC   AIO,AIO1                                                         
*                                                                               
DD100    LA    R2,DEFSDDT                                                       
         CLI   SVSRKEY+SVSTAME,C' '                                             
         BE    *+8                                                              
         LA    R2,DEFSDDR                                                       
         CLI   0(R2),0             DEFAULT SDD                                  
         BNE   DD200                                                            
         MVC   REJSTA,SVSRKEY+SVGSTAE                                           
         MVC   P(7),SVSRKEY+SVGSTAE                                             
         MVC   P+9(18),=C'HAS NO SDD RECORD'                                    
         GOTO1 =V(PRINTER)                                                      
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
DD200    ST    R2,ACURSDDT                                                      
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*****************************************************************               
*                                                               *               
*  ROUTINE READS BUYS FOR 1 CONTRACT - STORES 2-8 BUFFALO RECS  *               
*  - 1-4 BUFF RECORDS WITH OLD CONTRACT INFO NEGATED            *               
*  - 1-4 BUFF RECORDS WITH NEW CONTRACT INFO - READS NEXT BUY   *               
*  UNTIL ALL BUYS FOR CONTRACT NUMBER ARE READ                  *               
*                                                               *               
*****************************************************************               
         SPACE                                                                  
READBUY  NTR1                                                                   
         BAS   RE,SETCBUCS         SET UP MONTH BUCKETS                         
         ICM   RE,15,BUYCHGN                                                    
         BNZ   RB140                                                            
         MVI   TCDCHGSW,C'N'                                                    
         CLI   ADDCNT,C'Y'         CON ADDED TODAY NO CHANGE                    
         BE    RB300                                                            
         CLC   OSERV(3),NSERV      SERVICE/CATAGORY CHANGE                      
         BNE   RB120                                                            
RB100    CLC   OADV(7),NADV        ADVERTISER CHANGE                            
         BE    RB300                                                            
RB120    MVI   TCDCHGSW,C'Y'       CHANGE TO TYPE CODES                         
RB140    MVI   RDBUYSW,C'Y'        READING BUYS SWITCH FOR CALCBUY              
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'                                                        
         MVC   KEY+(RBUYKREP-RBUYKEY)(2),REP                                    
         MVC   KEY+18(4),SVSRKEY+SVCONE CONTRACT NUMBER                         
         GOTO1 HIGH                                                             
         B     RB220                                                            
*                                                                               
RB200    GOTO1 SEQ                                                              
RB220    CLC   KEY(22),KEYSAVE                                                  
         BNE   RB300                                                            
         CLC   =XL2'FFFF',KEY+25   SKIP PLAN                                    
         BE    RB200                                                            
         ICM   RE,15,BUYCHGN                                                    
         BZ    *+12                                                             
         TM    KEY+27,X'80'                                                     
         BO    RB200                                                            
         LA    R3,KEY                                                           
*        GOTO1 =V(HEXOUT),DMCB,0(R3),P+10,34,=C'T0G'                            
*        GOTO1 =V(PRINTER)                                                      
         MVC   AIO,AIO2                                                         
         BAS   RE,GETREC                                                        
         MVC   AIO,AIO1                                                         
         L     R6,AIO2                                                          
         USING RBUYKEY,R6                                                       
         MVC   NCNTL,RBUYCNTL                                                   
         MVC   NDUR,RBUYDUR                                                     
         MVC   DUB,RBUYCOS                                                      
         MVC   NNW,RBUYNW                                                       
         MVC   NTYP,RBUYTYP                                                     
         MVI   PLANSW,C'N'                                                      
         CLC   RBUYKPLN,=3X'FF'                                                 
         BE    *+8                                                              
         MVI   PLANSW,C'Y'                                                      
         MVI   ELCODE,X'02'        GET DAY/TIME ELEM                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R6,ADYTMEL                                                       
*                                                                               
*   FOLLOWING TEST FOR RECORDS WITH NO X'02' DAY/TIME ELEMENTS                  
*                                                                               
         CLI   0(R6),X'02'         DAY/TIME ELEMENT?                            
         BNE   RB200               NO  - SKIP THIS RECORD                       
         BAS   RE,CALCBUY                                                       
         B     RB200                                                            
*                                                                               
RB300    MVI   RDBUYSW,C'N'        SET OFF READ BUY SWITCH                      
RBEXT    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*                                                               *               
*  ROUTINE ADDS BUFF RECORDS FOR BUY - MATCH DAY/TIMES TO SDD   *               
*  RECORDS. DOES THIS BY 1ST FINDING A MATCH ON DAY/TIME START  *               
*  (CAN ONLY MATCH TO 1) THAN CHECKS END TIME FOR MATCH         *               
*  YES - READS NEXT ELEMENT, NO ADDS 1 TO SDD END TIME, STORES  *               
*  IN START TIME USE DPT SDD TO MATCH ON DAY/TIME - NO MATCH    *               
*  CRERATE PSUEDO(ZZZ) DPT. AFTER THIS MESS IT READS NEXT ELEM  *               
*  AND STARTS OVER USING MATCHED DPT.                           *               
*  CREATES BUFFALO RECORDS (UP TO 4)                            *               
*                                                               *               
*****************************************************************               
         SPACE                                                                  
CALCBUY  NTR1                                                                   
         XC    BFREC,BFREC                                                      
         MVC   BFSTA,SVSTA                                                      
         MVC   BFSPL,NDUR                                                       
         CLI   PLANSW,C'Y'                                                      
         BNE   BC020                                                            
         MVC   BFDPT(2),=X'FEFE'                                                
         MVC   BFSPL,=X'FFFF'                                                   
         MVI   BFPGT,X'FC'         PLAN INDICATOR                               
         B     BC700                                                            
BC020    CLI   NTYP,C' '                                                        
         BE    BC100                                                            
         SR    RE,RE                                                            
         ICM   RE,1,NTYP           IF TYPE GIVEN THAN NO DAYPARTS               
         BZ    BC100                                                            
         MVC   BFDPT(2),=X'FEFE'                                                
         STC   RE,BFPGT                                                         
         B     BC700                                                            
*                                                                               
BC100    MVI   ELCODE,X'02'        GET DAY/TIME ELEM                            
         L     R6,ADYTMEL                                                       
         MVI   WORK+5,C'Y'         1ST TIME SWITCH                              
         CLI   0(R6),X'02'         DAY/TIME ELEM ADDR                           
         BE    *+6                                                              
BCDUMP   DC    H'0'                                                             
         BAS   R5,CHKTIM                                                        
         L     R2,ACURSDDT                                                      
BC200    LR    R3,R2                                                            
         MVC   BFDPT(2),0(R2)                                                   
BC220    ZIC   R5,2(R2)            # OF TIMES THIS DPT                          
         LA    R2,3(R2)                                                         
BC240    ZIC   RE,0(R2)                                                         
         EX    RE,DAYTEST          DAY OVERLAP                                  
         BZ    BC260                                                            
         CLC   WORK(2),3(R2)                                                    
         BH    BC260               ELEM ST GT DPT END                           
         CLC   WORK(2),1(R2)                                                    
         BNL   BC300               ELEM ST BEFORE DPT START                     
BC260    LA    R2,5(R2)            DAY OR TIME NOT MATCHED                      
         BCT   R5,BC240                                                         
*                                                                               
         CLI   WORK+5,C'Y'         AFTER 1ST TIME NO MATCH TO ANY DPT           
         BNE   BC500                                                            
         CLI   0(R2),0             1ST TIME MUST FIND MATCH                     
         BNE   BC200                                                            
         B     BC500               START TIME HAS TO FIT                        
*                                                                               
BC300    MVI   WORK+5,C'N'                                                      
         OC    WORK+2(2),WORK+2    END TIME - NO A MATCH                        
         BZ    BC400                                                            
         CLC   WORK+2(2),3(R2)                                                  
         BNH   BC400               END BEFORE END - MATCH                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,3(R2)          ADD 1 TO END DATE (NEW START)                
         LA    RF,1(RF)                                                         
         LR    R1,RF                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'          CHECK FOR END OF AN HOUR                     
         CH    RE,=H'60'                                                        
         BNE   *+8                                                              
         AH    R1,=H'40'                                                        
         LR    RF,R1                                                            
         STCM  RF,3,WORK                                                        
         LR    R2,R3                                                            
         B     BC220                                                            
*                                                                               
BC400    BAS   RE,NEXTEL                                                        
         BNE   BC700                                                            
         BAS   R5,CHKTIM                                                        
         LR    R2,R3                                                            
         B     BC220                                                            
*                                                                               
* CHKTIM SUB ROUTINE MAY RETURN HERE                                            
BC500    MVC   BFDPT(2),=X'FFFF'                                                
*                                                                               
BC700    L     R6,ADYTMEL          SET BACK TO DAY TIME ELEMENT                 
         LA    R2,MTHTBL                                                        
         MVI   ELCODE,X'03'        GET WEEKS ELEM                               
BC720    BAS   RE,NEXTEL                                                        
         BNE   BC900                                                            
         MVC   WORK(3),2(R6)                                                    
BC740    CLC   WORK(3),5(R2)                                                    
         BNH   BC800               END BEFORE START                             
         BAS   RE,SETADDBF                                                      
*                                                                               
         LA    R2,8(R2)                                                         
         OC    0(2,R2),0(R2)                                                    
         BNZ   BC740                                                            
         MVC   P(24),=C'* DATES OUTSIDE CONTRACT'                               
         L     R6,AIO2                                                          
         GOTO1 =V(HEXOUT),DMCB,0(R6),P+38,20,=C'T0G'                            
         GOTO1 =V(PRINTER)                                                      
         B     BCEXT                                                            
*                                                                               
BC800    MVC   BFYM,0(R2)                                                       
         ZIC   RF,NNW                                                           
         CLI   1(R6),5                                                          
         BE    BC820                                                            
         ZIC   RF,RBUYDTNW-RBUYDTEL(R6)                                         
BC820    LR    R1,RF                                                            
         A     RF,BFSPOT                                                        
         ST    RF,BFSPOT                                                        
         ICM   RF,15,DUB                                                        
         BZ    BC832                                                            
         CLI   BFPGT,X'FC'         IF PLAN ITS TOTAL COST                       
         BNE   BC830                                                            
         LA    R1,1                                                             
BC830    LR    RF,R1                                                            
         M     RE,DUB                                                           
         LTR   RF,RF                                                            
         BM    *+12                                                             
         AH    RF,=H'50'                                                        
         B     *+8                                                              
         SH    RF,=H'50'                                                        
         D     RE,=F'100'                                                       
         A     RF,BFDOLS                                                        
         ST    RF,BFDOLS                                                        
*                                                                               
BC832    CLI   1(R6),5             NO END DATE - NEXT ELEM                      
         BE    BC720                                                            
         LA    R5,7                1 WEEK                                       
         TM    8(R6),X'40'                                                      
         BNO   *+8                                                              
         LA    R5,14               2 WEEKS                                      
         ZIC   RE,WORK+2                                                        
         AR    RE,R5                                                            
         CH    RE,=H'28'                                                        
         BH    *+12                                                             
         STC   RE,WORK+2           LT 28 ADD 7/14 TO DAYS                       
         B     BC840                                                            
*                                                                               
         GOTO1 =V(DATCON),DMCB,(3,WORK),(0,WORK+6)                              
         GOTO1 =V(ADDAY),(R1),WORK+6,WORK+12,(R5)                               
         GOTO1 =V(DATCON),(R1),(0,WORK+12),(3,WORK)                             
BC840    CLC   WORK(3),5(R6)                                                    
         BH    BC720                                                            
         B     BC740                                                            
*                                                                               
BC900    BAS   RE,SETADDBF                                                      
*                                                                               
* LOOK FOR MAKEGOOD/MISSED ELEMENTS                                             
BC1000   MVI   ELCODE,X'06'        GET WEEKS ELEM                               
BC1020   L     R6,ADYTMEL                                                       
BC1040   BAS   RE,NEXTEL                                                        
         BNE   BC1200                                                           
*                                                                               
         ZIC   RF,RBUYMSSP-RBUYMSEL(R6)                                         
         TM    NCNTL,X'80'                                                      
         BO    *+6                                                              
         LCR   RF,RF                                                            
         ST    RF,BFSPOT                                                        
         CLI   ELCODE,X'07'        MISSED ALREADY ADDED IN                      
         BE    BC1100                                                           
         CLI   BFPGT,X'FC'         IF PLAN TOTAL COST                           
         BE    BC1100                                                           
         M     RE,DUB                                                           
         LTR   RF,RF                                                            
         BM    *+12                                                             
         AH    RF,=H'50'                                                        
         B     *+8                                                              
         SH    RF,=H'50'                                                        
         D     RE,=F'100'                                                       
         ST    RF,BFDOLS                                                        
*                                                                               
BC1100   LA    R2,MTHTBL                                                        
BC1120   CLC   2(3,R6),5(R2)                                                    
         BNH   BC1140              END BEFORE START                             
         LA    R2,8(R2)                                                         
         OC    0(2,R2),0(R2)                                                    
         BNZ   BC1120                                                           
         DC    H'0'                                                             
BC1140   MVC   BFYM,0(R2)                                                       
         BAS   RE,SETADDBF                                                      
         B     BC1040                                                           
*                                                                               
BC1200   CLI   ELCODE,X'07'                                                     
         BE    BCEXT                                                            
         MVI   ELCODE,X'07'                                                     
         B     BC1020                                                           
*                                                                               
BCEXT    B     EXIT                                                             
DAYTEST  TM    3(R6),0                                                          
         SPACE                                                                  
* CHECK TIME FOR NONE/VARYIOUS/CC                                               
*                                                                               
*        **** ROUTINE RETURNS TO BC500 IF NONE OR VARYIOUS TIMES ****           
*                                                                               
CHKTIM   MVC   WORK(4),4(R6)       START/END TIME                               
*        GOTO1 =V(HEXOUT),DMCB,WORK,P+40,4,=C'T0G'                              
         CLC   =C'NONE',WORK                                                    
         BE    BC500                                                            
         CLC   =C'VARY',WORK                                                    
         BE    BC500                                                            
         CLC   =C'CC',WORK+2                                                    
         BNE   *+10                                                             
         MVC   WORK+2(2),=H'2600'                                               
         CLC   WORK(2),=H'600'                                                  
         BNL   KM120                                                            
         CLC   WORK(2),=H'500'                                                  
         BL    KM100                                                            
         MVC   WORK(2),=H'600'     5A - 6A                                      
         CLC   WORK+2(2),=H'559'                                                
         BH    KM200                                                            
         CLC   WORK+2(2),=H'501'                                                
         BL    KM120                                                            
         MVC   WORK+2(2),=H'600'   5A = 6A                                      
         B     KM200                                                            
KM100    CLC   WORK(2),=H'201'                                                  
         BL    KM120                                                            
         MVC   WORK(2),=H'200'     2A - 459                                     
KM120    CLC   WORK+2(2),=H'600'                                                
         BNL   KM200                                                            
         CLC   WORK+2(2),=H'201'                                                
         BL    KM200                                                            
         CLC   WORK+2(2),=H'500'                                                
         BH    KM140                                                            
         MVC   WORK+2(2),=H'200'   201A - 459 = 2A                              
         B     KM200                                                            
KM140    MVC   WORK+2(2),=H'600'   5A = 6A                                      
KM200    CLC   WORK(2),WORK+2                                                   
         BNE   *+10                                                             
         XC    WORK+2(2),WORK+2                                                 
         LA    R1,WORK                                                          
         BAS   RE,ADJTIME                                                       
*        GOTO1 =V(HEXOUT),DMCB,WORK,P+20,4,=C'T0G'                              
*        GOTO1 =V(PRINTER)                                                      
         CLC   =H'2600',WORK+2     LEAVE 2A ALONE                               
         BE    KM300                                                            
         SR    RE,RE                                                            
         ICM   RE,3,WORK+2                                                      
         BZ    KMEXT                                                            
         SH    RE,=H'1'            ADJUST END TIME                              
         STCM  RE,3,WORK+2                                                      
KM300    CLC   WORK(2),WORK+2      START GT END NO MATCH                        
         BH    BC500                                                            
KMEXT    BR    R5                                                               
         SPACE 2                                                                
* FIGURE OUT HOW TO ADD TO BUFFALO                                              
SETADDBF NTR1                                                                   
         CLI   RDBUYSW,C'Y'                                                     
         BNE   AU200                                                            
         MVC   SERV(10),NSERV                                                   
         BAS   R5,ADTOBUFF         ADD TO NEW RECORDS                           
         ICM   RE,15,BUYCHGN                                                    
         BNZ   AU100                                                            
*                                                                               
         MVC   SERV(10),OSERV                                                   
         L     RE,BFSPOT                                                        
         LCR   RE,RE                                                            
         ST    RE,BFSPOT                                                        
         L     RE,BFDOLS                                                        
         LCR   RE,RE                                                            
         ST    RE,BFDOLS                                                        
         BAS   R5,ADTOBUFF         SUBTRACT FROM OLD RECORDS                    
AU100    XC    BFYM,BFYM                                                        
         XC    BFSPOT(8),BFSPOT                                                 
         B     AUEXT                                                            
*                                                                               
AU200    CLI   TCDCHGSW,C'Y'       CHANGE TO TYPE CODES                         
         BNE   AU300                                                            
         CLI   SRACT,COPY          IGONORE CHANGE/ADD RECORDS                   
         BE    AU320                                                            
         B     AU340               ADD TO OLD RECORDS                           
*                                                                               
* NO TYPE CODE CHANGES                                                          
AU300    CLI   SRACT,COPY          COPY RECORDS SUBTRACT FROM OLD               
         BE    AU320                                                            
         MVC   SERV(10),NSERV                                                   
         B     AU400               ADD TO NEW RECORDS                           
*                                                                               
AU320    L     RE,BFDOLS                                                        
         LCR   RE,RE                                                            
         ST    RE,BFDOLS                                                        
         L     RE,BFSPOT                                                        
         LCR   RE,RE                                                            
         ST    RE,BFSPOT                                                        
AU340    MVC   SERV(10),OSERV                                                   
AU400    BAS   R5,ADTOBUFF         SUBTRACT FROM OLD RECORDS                    
AUEXT    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PUT UP TO 4 BUFFALO RECORDS                                                   
ADTOBUFF OC    BFYM,BFYM                                                        
         BZ    AB500                                                            
         MVI   BFTYP,1                                                          
         XC    BFTYPC,BFTYPC                                                    
         BAS   RE,PUTBUFF                                                       
*        GOTO1 =V(HEXOUT),DMCB,BFREC,P+15,32,=C'T0G'                            
*        GOTO1 =V(PRINTER)                                                      
*                                                                               
         OC    SERV,SERV                                                        
         BZ    AB100                                                            
         MVI   BFTYP,2                                                          
         MVC   BFTYPC(1),SERV                                                   
         BAS   RE,PUTBUFF                                                       
*        GOTO1 =V(HEXOUT),DMCB,BFREC,P+46,32,=C'T0G'                            
*        GOTO1 =V(PRINTER)                                                      
*                                                                               
AB100    OC    CATG,CATG                                                        
         BZ    AB200                                                            
         MVI   BFTYP,3                                                          
         MVC   BFTYPC(2),CATG                                                   
         BAS   RE,PUTBUFF                                                       
*                                                                               
AB200    MVI   BFTYP,4                                                          
         MVC   BFTYPC(7),ADV                                                    
         BAS   RE,PUTBUFF                                                       
*                                                                               
AB500    CLI   RDBUYSW,C'Y'                                                     
         BE    ABEXT                                                            
         XC    BFYM,BFYM                                                        
         XC    BFSPOT(8),BFSPOT                                                 
ABEXT    BR    R5                                                               
         EJECT                                                                  
*  ROUTINE ADDS ATHENA RECORDS - BUILDS STATION TOTALS                          
CHGATHNA NTR1                                                                   
         SR    R3,R3               ANTHING FOUND                                
*                                                                               
         BAS   RE,HIGHBUFF                                                      
         B     *+8                                                              
*                                                                               
AA100    BAS   RE,SEQBUFF                                                       
         BO    AA700                                                            
*                                                                               
         OC    BFSPOT(8),BFSPOT                                                 
         BZ    AA100               NO SPOTS OR DOLLARS DON'T UPDATE             
*                                                                               
         OC    LSTSTA,LSTSTA                                                    
         BZ    *+18                                                             
         CLC   BFSTA,LSTSTA                                                     
         BZ    *+8                                                              
         BAS   RE,PRNTSTA                                                       
         MVC   LSTSTA,BFSTA                                                     
*                                                                               
* SEE IF RECORD EXISTS                                                          
         MVI   DMINBTS,X'80'       READ FOR UPDATE                              
         XC    KEY,KEY                                                          
         MVI   KEY,X'27'                                                        
         MVC   KEY+RATNKREP-RATNKEY(2),REP                                      
         MVC   KEY+RATNKGRP-RATNKEY(7),BFSTA                                    
         MVC   KEY+RATNKTPE-RATNKEY(8),BFTYP                                    
         MVC   KEY+RATNKDPT-RATNKEY(7),BFDPT                                    
         GOTO1 HIGH                                                             
         MVC   AIO,AIO2                                                         
         L     R6,AIO2                                                          
         CLC   KEY(27),KEYSAVE     SEE IF ANY ATNEA RECORD PER STA              
         BNE   AA400                                                            
*                                                                               
* THIS IS A RECORD THAT EXISTS                                                  
         GOTO1 GETREC                                                           
         USING RATNRECD,R6                                                      
         SR    R3,R3                                                            
         ICM   R3,3,RATNLEN                                                     
         AH    R3,=H'10'           SIZE OF NEW ELEMENT                          
         CH    R3,=H'998'                                                       
         BNH   AA120                                                            
         MVC   P(25),=C'* ERROR - RECORD TO LARGE'                              
         MVC   P+26(7),RATNKGRP                                                 
         GOTO1 =V(HEXOUT),DMCB,BFREC,P+38,20,=C'T0G'                            
         GOTO1 =V(PRINTER)                                                      
         B     AA100                                                            
*                                                                               
AA120    MVC   RATNLCHG,TODAY2                                                  
         SR    R5,R5                                                            
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
AA200    BAS   RE,NEXTEL                                                        
         BNE   AA220                                                            
         CLC   TODAY2,RATNSCAS-RATNSCDT(R6)                                     
         BNE   AA200                                                            
         LR    R5,R6               IF ELEMENT EXISTS SAVE ADDR                  
         B     AA200                                                            
*                                                                               
AA220    XC    0(11,R6),0(R6)      IF NEW ELEMENT CLEAR R6                      
         LTR   R5,R5                                                            
         BZ    AA240                                                            
         LR    R6,R5                                                            
         B     AA260                                                            
AA240    MVC   0(2,R6),=X'020A'    BUILD ELEMENT                                
         MVC   2(2,R6),TODAY2                                                   
         LR    RF,R6                                                            
         L     R6,AIO2             RESET R6 TO BASE REG                         
         SR    RE,RE                                                            
         ICM   RE,3,RATNLEN                                                     
         AH    RE,=H'10'           SIZE OF NEW ELEMENT                          
         STCM  RE,3,RATNLEN                                                     
         LR    R6,RF                                                            
AA260    L     RE,BFSPOT                                                        
         SR    RF,RF                                                            
         ICM   RF,3,4(R6)          IF ELEM CURRENT SPOTS                        
         AR    RE,RF                                                            
         STCM  RE,3,4(R6)                                                       
         STCM  RE,15,BFSPOT        FOR SPOT TOTALS                              
         L     RE,BFDOLS                                                        
         ICM   RF,15,6(R6)         IF ELEM CURRENT DOLLARS                      
         AR    RE,RF                                                            
         STCM  RE,15,6(R6)                                                      
         STCM  RE,15,BFDOLS        FOR DOLLAR TOTALS                            
*                                                                               
         OC    YEAR,YEAR           FILTER ON YEAR?                              
         BZ    AA300               NO - SO CONTINUE                             
*                                                                               
         CLC   YEAR,RATNKYM        THIS YEAR?                                   
         BNE   AA100               NO - SO SKIP THIS RECORD                     
*                                                                               
AA300    EQU   *                                                                
*                                                                               
         AP    TCCOUNT,=P'1'                                                    
         CLI   WRITEFLG,C'Y'                                                    
         BNE   AA500                                                            
         BAS   RE,PUTREC           CHANGE FILE RECORD                           
         B     AA600                                                            
*                                                                               
* THIS IS A NEW RECORD - BUILD & ADD                                            
AA400    XC    0(256,R6),0(R6)     CLEAR RECORD AREA                            
         MVI   RATNKTYP,X'27'                                                   
         MVC   RATNKREP,REP        REP                                          
         MVC   RATNKGRP(7),BFSTA                                                
         MVC   RATNCODE(2),=X'010C'                                             
         MVC   RATNCREA,TODAY2                                                  
         MVC   RATNLCHG,TODAY2                                                  
         MVC   RATNKTPE(8),BFTYP                                                
         MVC   RATNKDPT(7),BFDPT                                                
         MVC   RATNLEN,=H'57'                                                   
         LA    R4,RATNCODE                                                      
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               INCASE ELEM 1 LENGTH CHG                     
         MVC   0(2,R4),=X'020A'                                                 
         MVC   2(2,R4),TODAY2                                                   
         MVC   4(2,R4),BFSPOT+2                                                 
         MVC   6(4,R4),BFDOLS                                                   
         LA    R3,56                                                            
*                                                                               
         OC    YEAR,YEAR           FILTER ON YEAR?                              
         BZ    AA450               NO - SO CONTINUE                             
*                                                                               
         CLC   YEAR,RATNKYM        THIS YEAR?                                   
         BNE   AA100               NO - SO SKIP THIS RECORD                     
*                                                                               
AA450    EQU   *                                                                
*                                                                               
         DROP  R6                                                               
         AP    TACOUNT,=P'1'                                                    
         CLI   WRITEFLG,C'Y'                                                    
         BNE   AA500                                                            
         BAS   RE,ADDREC           ADD DIRECTLY TO FILE                         
         B     AA600                                                            
*                                                                               
AA500    L     R4,AIO                                                           
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R3),=C'2D'                
*                                                                               
AA600    CLI   BFTYP,1             ONLY ADD 1 BUYLINES SPOTS/DOLLARS            
         BNE   AA620                                                            
         L     RE,TSTASP                                                        
         A     RE,BFSPOT                                                        
         ST    RE,TSTASP                                                        
         L     RE,TSTACS                                                        
         A     RE,BFDOLS                                                        
         ST    RE,TSTACS                                                        
AA620    AP    COUNT,=P'1'                                                      
         B     AA100                                                            
*                                                                               
AA700    MVC   AIO,AIO1                                                         
         LTR   R3,R3                                                            
AAEXT    B     EXIT                                                             
         EJECT                                                                  
*  ROUTINE PRINTS TOTAL PER STATION                                             
PRNTSTA  NTR1                                                                   
         CP    LINE,=P'48'                                                      
         BL    PS100                                                            
         ZAP   LINE,=P'100'        FORCE PAGE BREAK                             
         MVC   TITLE+11(20),=C'ATHENA UPDATE REPORT'                            
PS100    MVC   P(7),LSTSTA                                                      
         CLI   P+6,C' '                                                         
         BNE   *+8                                                              
         MVI   P+6,C'T'                                                         
         MVC   P+12(29),=CL29'** STATION RECORDS UPDATED **'                    
         EDIT  (P3,COUNT),(5,P+45)                                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P(19),=CL19'** STATION SPOTS **'                                 
         EDIT  (4,TSTASP),(6,P+22)                                              
         MVC   P+30(21),=CL21'** STATION DOLLARS **'                            
         EDIT  (4,TSTACS),(12,P+54)                                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         L     RE,TTLSP                                                         
         A     RE,TSTASP                                                        
         ST    RE,TTLSP                                                         
         L     RE,TTLCS                                                         
         A     RE,TSTACS                                                        
         ST    RE,TTLCS                                                         
*                                                                               
         ZAP   COUNT,=P'0'         CLEAR STATION ACCUMS                         
         XC    TSTASP,TSTASP                                                    
         XC    TSTACS,TSTACS                                                    
*                                                                               
PSEXT    B     EXIT                                                             
         EJECT                                                                  
*  ROUTINE PRINTS TOTAL PER STATION                                             
FINALPRT NTR1                                                                   
         ZAP   LINE,=P'100'        FORCE PAGE BREAK                             
         MVC   TITLE+11(20),=C'ATHENA UPDATE REPORT'                            
         MVC   MID1+50(11),=C'FINAL TOTAL'                                      
         MVC   MID2+50(12),=15CL1'-'                                            
         MVC   P(17),=CL17'** TOTAL SPOTS **'                                   
         EDIT  (4,TTLSP),(6,P+22)                                               
         MVC   P+30(19),=CL19'** TOTAL DOLLARS **'                              
         EDIT  (4,TTLCS),(12,P+54)                                              
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(31),=CL31'** NUMBER OF RECORDS ADDED **'                       
         EDIT  (P5,TACOUNT),(8,P+32)                                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P(31),=CL31'** NUMBER OF RECORDS CHANGED **'                     
         EDIT  (P5,TCCOUNT),(8,P+32)                                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
FNEXT    B     EXIT                                                             
         SPACE 2                                                                
* ROUTINE TO MOVE SDD RECORDS TO GIVEN AREA                                     
* - R2 = ADDRESS OF AREA TO PLACE RECORD                                        
SVSDD    NTR1                                                                   
         LR    RE,R2                                                            
         LA    RF,LSDDTBL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SD100    BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         MVC   0(3,R2),3(R6)       DAYPART - # OF TIMES                         
         LA    R2,3(R2)                                                         
         LR    R3,R6                                                            
         ZIC   R5,5(R6)            # DAY TIMES IN ELEMENT                       
SD200    MVC   0(5,R2),6(R3)                                                    
         LA    R1,1(R2)                                                         
         BAS   RE,ADJTIME                                                       
         CLC   =H'2600',3(R2)      LEAVE 2A ALONE                               
         BE    SD220                                                            
         SR    RE,RE                                                            
         ICM   RE,3,3(R2)                                                       
         BNZ   *+14                                                             
         MVC   3(2,R2),1(R2)       END 0 SAME AS START                          
         B     SD220                                                            
*                                                                               
         SH    RE,=H'1'            ADJUST END TIME                              
         STCM  RE,3,3(R2)                                                       
SD220    LA    R3,5(R3)            NEXT ELEM DAY TIME                           
         LA    R2,5(R2)            NEXT OF TALBE                                
         BCT   R5,SD200                                                         
         B     SD100                                                            
         SPACE                                                                  
* ADJUST TIME IF LESS THAN 600 (001-2A)                                         
ADJTIME  NTR1                                                                   
         LR    R0,2                                                             
AJ100    CLC   =H'600',0(R1)       ADJUST AFTER 1200                            
         BNH   AJ200                                                            
         SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         AH    RE,=H'2400'                                                      
         STCM  RE,3,0(R1)                                                       
AJ200    LA    R1,2(R1)                                                         
         OC    0(2,R1),0(R1)       IF END TIME 0 DON'T CONVERT                  
         BE    EXIT                                                             
         BCT   R0,AJ100                                                         
         B     EXIT                                                             
         EJECT                                                                  
* SET UP CONTRACT BUCKETS                                                       
SETCBUCS NTR1                                                                   
         XC    MTHTBL(200),MTHTBL                                               
         XC    MTHTBL+200(202),MTHTBL+200                                       
         LA    R3,MTHTBL           2(BYTES)-YM/3 BCM-ST YMD/3-EN YMD            
         LA    R5,50                                                            
         GOTO1 =V(DATCON),DMCB,(3,NCDTES),(0,WORK+20)                           
SB100    GOTO1 =V(GETBROAD),(R1),WORK+20,WORK,V(GETDAY),V(ADDAY)                
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(DATCON),(R1),(0,WORK),(3,2(R3))                               
         GOTO1 =V(DATCON),(R1),(0,WORK+6),(3,5(R3))                             
         LA    R0,7                GET DATE WITH CORRECT MONTH                  
         GOTO1 =V(ADDAY),(R1),WORK,WORK+30,(R0)                                 
         GOTO1 =V(DATCON),(R1),(0,WORK+30),(3,WORK+38)                          
         MVC   0(2,R3),WORK+38     MOVE IN MONTH YEAR                           
         CLC   NCDTES+3(3),5(R3)                                                
         BNH   SBEXT                                                            
         LA    R0,1                                                             
         GOTO1 =V(ADDAY),(R1),WORK+6,WORK+20,(R0)                               
         LA    R3,8(R3)                                                         
         BCT   R5,SB100                                                         
         DC    H'0'                                                             
SBEXT    B     EXIT                                                             
         EJECT                                                                  
* SET UP CONTRACT SORT FIELDS                                                   
         USING RCONKEY,R4                                                       
         USING SORTRECD,R7                                                      
SETCONSR ST    RE,FULL                                                          
         MVI   SRREC,X'01'         CONTRACT RECORD                              
         MVC   SRSTAG,RCONKGRP                                                  
         MVC   SRCOCATG(17),SVOCATG                                             
         UNPK  WORK+10(9),RCONKCON(5) CONTRACT NUMBER                           
         PACK  WORK(5),WORK+10(8)                                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),WORK(5)                                               
         MVO   WORK(5),WORK+10(5)                                               
         MVC   SRCON#,WORK                                                      
         PACK  SRCON#(1),WORK+3(1) REVERSE THE COMPLIMENT                       
         PACK  SRCON#+1(1),WORK+2(1)                                            
         PACK  SRCON#+2(1),WORK+1(1)                                            
         PACK  SRCON#+3(1),WORK(1)                                              
         MVC   SRCCATG,RCONCTGY                                                 
         MVC   SRCSER,RCONRTGS                                                  
         MVC   SRCADV,RCONKADV                                                  
         MVC   SRCPRD,SPACES                                                    
         MVC   SRCDTES,RCONDATE                                                 
         MVC   SRCBUYN,RECCNT                                                   
         CLI   SRACT,MYCON         MY CONTRAC SAVE OLD & NEW TYPES              
         BNE   *+10                                                             
         MVC   SRCOCATG(17),SRCCATG                                             
         MVC   SRLEN(2),=H'58'                                                  
         AP    INCNTC,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRLEN                                    
         XC    SVOTCD,SVOTCD                                                    
         CLI   SRACT,MYCON         STORE CONTRACT # THAT I READ                 
         BNE   TCEXT                                                            
*                                                                               
* STORE GROUP/STATION CONTRACT # IN TABLE                                       
         L     RF,ANEXTCNT                                                      
         MVC   0(4,RF),SRCON#                                                   
         MVC   4(7,RF),SRSTAG                                                   
         LA    RF,11(RF)                                                        
         LA    R0,CNLSTTBX         END OF LIST                                  
         CR    R0,RF                                                            
         BNL   *+8                                                              
         LA    RF,CNLSTTAB         SEE IF NEW CONTRACT                          
         ST    RF,ANEXTCNT         SAVE NEXT ADDRESS                            
TCEXT    L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R4,R7                                                            
         SPACE                                                                  
* GET A MONDAY DATE      -- USES DUB/FULL(6)/HALF --                            
GETMONDT GOTO1 =V(DATCON),DMCB,(2,TODAY2),DUB                                   
         GOTO1 =V(GETDAY),(R1),DUB,FULL                                         
         ZIC   RE,0(R1)            DAY NUMBER OF AIR DATE                       
         LA    R0,1                USE THE START DAY'S NUMBER                   
         SR    R0,RE               DIFFERENCE BETWEEN THE TWO DAYS              
         BZ    MOEXT               THERE IS NONE                                
         GOTO1 =V(ADDAY),(R1),DUB,FULL,(R0)                                     
         GOTO1 =V(DATCON),(R1),(0,FULL),(2,TODAY2)                              
MOEXT    BR    R5                                                               
         EJECT                                                                  
*  BUFFALO ROUTINES                                                             
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
         GOTO1 =V(BUFFALO),DMCB,,BUFFC,BFREC,1                                  
         TM    DMCB+8,X'80'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*              I/O HANDLING ROUTINES - DIRECTORY                                
         SPACE                                                                  
HIGH     MVC   COMMAND(6),=C'DMRDHI'                                            
         B     DR100                                                            
         SPACE                                                                  
SEQ      MVC   COMMAND(6),=C'DMRSEQ'                                            
         B     DR100                                                            
         SPACE                                                                  
READ     MVC   COMMAND(6),=C'DMREAD'                                            
DR100    MVC   KEYSAVE,KEY                                                      
         B     DIR                                                              
         SPACE                                                                  
WRITE    MVC   COMMAND(6),=C'DMWRT '                                            
         B     DIR                                                              
         SPACE                                                                  
ADD      MVC   COMMAND(6),=C'DMADD '                                            
         SPACE                                                                  
DIR      NTR1                                                                   
         ZIC   R4,DMINBTS                                                       
         GOTO1 =V(DATAMGR),DMCB,((R4),COMMAND),=C'REPDIR',KEY,KEY,0             
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE                                                                  
FILE     NTR1                                                                   
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         ZIC   R4,DMINBTS                                                       
         GOTO1 =V(DATAMGR),DMCB,((R4),COMMAND),=C'REPFILE',            X        
               (R2),AIO,DMWORK                                                  
         SPACE 2                                                                
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVI   DMINBTS,X'08'                                                    
         TM    8(R1),X'02'         DELETED OK                                   
         BO    EXIT                                                             
         CLI   8(R1),0                                                          
         BZ    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
         SPACE 3                                                                
GETEL    GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=4048,                                             X        
               MACRF=GM,                                               X        
               EODAD=ML100                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0F                                                               
VREGSAVE DC    V(REGSAVE)                                                       
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
         DC    C'*DMCB*'                                                        
DMCB     DS    6F                                                               
BUFFC    DS    A                                                                
         DC    C'*KEY'                                                          
KEY      DS    XL34                                                             
KEYSAVE  DS    XL34                                                             
SVSRKEY  DS    XL21                                                             
DMWORK   DS    XL96                                                             
COMMAND  DS    CL8                                                              
DMINBTS  DC    X'08'               (PASS BACK DELETED RECORDS)                  
*                                                                               
         DS    0F                                                               
WORK     DS    CL80                                                             
UTL      DC    F'0',X'0A'                                                       
*SB      DC    F'2'                INHIBIT RECOVERY                             
SSB      DS    0CL256              NEW SSB                                      
         DC    XL2'00'                                                          
         DC    X'FF'                                                            
         DC    X'02'                                                            
         DC    252X'00'                                                         
SAVNAME  DS    CL10                                                             
SAVLOCAL DS    CL1                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(4,18,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=1004'                                  
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8' REPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X'                                                           
BUFEND   DS    C                                                                
         DS    0F                                                               
ANEXTCNT DC    A(CNLSTTAB)         ADDRESS OF NEXT STATION IN TABLE             
ACURSDDT DS    A                   ADDRESS OF STATION SDD REC                   
ADYTMEL  DS    A                   ADDRESS OF BUY DAYTIME ELEM                  
TSTASP   DS    F                   TOTAL SPOTS/STATION                          
TSTACS   DS    F                   TOTAL DOLLARS/STATION                        
TTLSP    DS    F                   TOTAL SPOTS                                  
TTLCS    DS    F                   TOTAL DOLLARS                                
RECCNT   DS    F                   ORDER OF RECORDS ADDED INTO SORT             
DATADISP DS    H                                                                
TODAY2   DS    H                   TODAY"S COMRESSED DATE                       
         DS    0F                                                               
COLVALS  DS    24XL16                                                           
AIO      DS    A                                                                
AIO1     DS    A                   IOAREA                                       
AIO2     DS    A                   2ND IOAREA                                   
AIO3     DS    A                   3RD IOAREA                                   
CNLSTTAB DS    1100C               STA TBL IN RECOVERY  (RR100-RREND)           
CNLSTTBX EQU   *-1                                                              
REP      DS    CL2                 REP CODE                                     
MED      DS    CL1                 MEDIA                                        
STAGFIL  DS    CL7                 STATION GROUP FILTER                         
STAGEFIL DS    CL7                 END STATION GROUP FILTER                     
WRITEFLG DS    CL1                 DON'T WRITE                                  
YEAR     DS    CL1                 TEST RUN YEAR                                
OFFICE   DS    CL2                 TEST RUN OFFICE                              
FRST     DS    CL1                                                              
FSTCHG   DS    CL1                 KEEP 1ST CHANGES OLD PRODUCT                 
ADDCNT   DS    CL1                 CONTRACT ADDED TODAY (NO ATHNEAS)            
ABUYSW   DS    CL1                 HAD A BUY FOR THIS CONTRACT #                
TCDCHGSW DS    CL1                 CONTRACT TYPE CODE CHANGE                    
RDBUYSW  DS    CL1                 ADD TO BUFFALO FROM BUY READS                
CHGSTASW DS    CL1                 A STATION CHANGE                             
PLANSW   DS    CL1                 BUY A PLAN                                   
SERV     DS    CL1                 RATING SERVICE                               
CATG     DS    CL2                 CATEGORY                                     
ADV      DS    CL4                 ADVERTISER                                   
PRD      DS    CL3                 PRODUCT-ALWAYS SPACES                        
NSERV    DS    CL1                 NEW RATING SERVICE                           
NCATG    DS    CL2                 NEW CATEGORY                                 
NADV     DS    CL4                 NEW ADVERTISER                               
NPRD     DS    CL3                 NEW PRODUCT - ALWAYS SPACES                  
OSERV    DS    CL1                 OLD RATING SERVICE                           
OCATG    DS    CL2                 OLD CATEGORY                                 
OADV     DS    CL4                 OLD ADVERTISER                               
OPRD     DS    CL3                 OLD PRODUCT - ALWAYS SPACES                  
SVSTA    DS    CL7                 LAST CONTRACT STATION FOR SDD READ           
LSTSTA   DS    CL7                 LAST CONTRACT STATION FOR ADD RTN            
OGRSTA   DS    CL7                 OLD CONTRACT STATION                         
NGRSTA   DS    CL7                 NEW CONTRACT STATION                         
BUYCHGN  DS    CL4                 STATION CHG - NEXT BUY #                     
REJSTA   DS    CL7                 REJECT THIS STATION                          
NCNTL    DS    XL1                 DEFALUT SPOTS PER WEEK                       
NNW      DS    XL1                 DEFALUT SPOTS PER WEEK                       
NTYP     DS    CL1                 BUY TYPE                                     
NDUR     DS    CL2                 BUY LENGTH                                   
NCDTES   DS    CL6                 NEW CONTRACT DATES                           
SVOTCD   DS    0CL17               3 TYPE CODES                                 
SVOCATG  DS    CL2                 CATEGORY CODE                                
SVOSRV   DS    CL1                 SERVICE CODE                                 
SVOADV   DS    CL4                 ADVERTISER CODE                              
SVOPRD   DS    CL3                 PRODUCT CODE - ALWAYS SPACES                 
SVOGSTA  DS    CL7                 GROUP/STATION                                
ELCODE   DS    XL1                                                              
COUNT    DC    PL3'0'              COUNT OF ATHENA RECS/STATION                 
TACOUNT  DC    PL5'0'              COUNT OF ATHENA RECS/ADDED TOTAL             
TCCOUNT  DC    PL5'0'              COUNT OF ATHENA RECS/CHG TOTAL               
INCNTC   DC    PL4'0'              COUNT OF CONTRACT INPUT RECORDS              
INCNTB   DC    PL4'0'              COUNT OF BUY INPUT RECORDS                   
MTHTBL   DS    CL402               50 BROADCAST MONTH DATES+ENDING 0000         
* MAX NUMBER OF DAY/TIMES (48) * MAX LENGTH SIZE OF ENTRY (8),                  
* + ENDING ZERO (1) = TOTAL SIZE (385)                                          
LSDDTBL  EQU   48*8+1                                                           
DEFSDDT  DS    (LSDDTBL)CL1        DEFAULT TV SDD TABLE                         
         ORG   DEFSDDT                                                          
DEFDPT   DS    0CL2                - DAYPART                                    
DEFCNT   DS    0CL1                - COUNT OF DAY TIMES                         
DEFDAY   DS    0CL1                - DAY                                        
DEFTIME  DS    0CL4                - TIME                                       
         ORG   DEFSDDT+LSDDTBL                                                  
DEFSDDR  DS    (LSDDTBL)CL1        DEFAULT RADIO SDD TABLE                      
         ORG   DEFSDDR                                                          
DEFDPTR  DS    0CL2                - DAYPART                                    
DEFCNTR  DS    0CL1                - COUNT OF DAY TIMES                         
DEFDAYR  DS    0CL1                - DAY                                        
DEFTIMER DS    0CL4                - TIME                                       
         ORG   DEFSDDR+LSDDTBL                                                  
STASDDT  DS    (LSDDTBL)CL1        STATION SDD TABLE                            
         ORG   STASDDT                                                          
STADPT   DS    0CL2                - DAYPART                                    
STACNT   DS    0CL1                - COUNT OF DAY TIMES                         
STADAY   DS    0CL1                - DAY                                        
STATIME  DS    0CL4                - TIME                                       
         ORG   STASDDT+LSDDTBL                                                  
         DS    0F                                                               
BFREC    DS    0CL32               BUFFALO RECORD LAYOUT                        
BFKEY    DS    0CL24                                                            
BFSTA    DS    CL7                 GROUP/STATION                                
BFTYP    DS    CL1                 TYPE (1-4)                                   
BFTYPC   DS    CL7                 SERVICE/CATAGORY/ADV PRODUCT                 
BFDPT    DS    CL1                 DAYPART                                      
BFSDPT   DS    CL1                 SUB DAYPART                                  
BFPGT    DS    CL1                 PROGRAM TYPE                                 
BFSPL    DS    CL2                 SPOT LENGTH                                  
BFYM     DS    CL2                 YEAR MONTH                                   
         DS    CL2                 SPARE                                        
BFSPOT   DS    F                   TOTAL SPOTS                                  
BFDOLS   DS    F                   TOTAL DOLLARS                                
*                                                                               
         BUFF  LINES=1,ROWS=1,COLUMNS=2,FLAVOR=BINARY,                 X        
               KEYLIST=(24,A)                                                   
*                                                                               
         DC    C'REC1'                                                          
IO1      DS    6100C                                                            
         DC    C'REC2'                                                          
IO2      DS    6100C                                                            
         DC    C'REC3'                                                          
IO3      DS    6100C                                                            
PRGEND   EQU   *                                                                
         SPACE 2                                                                
SVLEN    EQU   0                                                                
SVGSTAE  EQU   4                                                                
SVSTAE   EQU   6                                                                
SVSTAME  EQU   10                                                               
SVCONE   EQU   11                                                               
SVRECE   EQU   15                                                               
SVNUME   EQU   16                                                               
SVACTE   EQU   20                                                               
*          DATA SET NET133     AT LEVEL 010 AS OF 10/22/86                      
COPY     EQU   1                                                                
CHANGE   EQU   2                                                                
*  ADD      EQU   3                SOME WHERE A DUPLICATE                       
SRTCPYE  EQU   2                                                                
SRTCHGE  EQU   3                                                                
SRTADDE  EQU   1                                                                
MYCON    EQU   4                                                                
         SPACE 2                                                                
*                                                                               
SORTRECD DSECT                                                                  
SRLEN    DS    CL4                 LENGTH                                       
SRSTAG   DS    CL7                 GROUP/STATION                                
SRCON#   DS    CL4                 CONTRACT NUMBER (P'S COMP REVERSED)          
SRREC    DS    CL1                 REC (01-CONTRACT/02-BUY)                     
SRNUM    DS    CL4                 ORDER OF RECS (IGNORE IF CONTRACT)           
SRACT    DS    CL1                 ACTION (ADD/COPY/CHANGE)                     
SRCOMMON DS    0CL1                START OF RECORD INFO                         
* CONTRACT INFO EXTRACTED                                                       
SRCCATG  DS    CL2                 CATEGORY                                     
SRCSER   DS    CL1                 SERVICE                                      
SRCADV   DS    CL4                 ADVERTISER                                   
SRCPRD   DS    CL3                 PRODUCT - ALWAYS SPACES                      
SRCDTES  DS    CL6                 START END DATES OF CONTRACT                  
SRCBUYN  DS    CL4                 MOST RECENT BUY NUMBER                       
SRCOCATG DS    CL2                 OLD CATEGORY                                 
SRCOSER  DS    CL1                 OLD SERVICE                                  
SRCOADV  DS    CL4                 OLD ADVERTISER                               
SRCOPRD  DS    CL3                 OLD PRODUCT - ALWAYS SPACES                  
SRCOGSTA DS    CL7                 GROUP/STATION                                
* BUY INFO EXTRACTED                                                            
         ORG   SRCOMMON                                                         
SRBCNTL  DS    CL1                 CONTROL WORK (X'80'DEL/CAN)                  
SRBTYP   DS    CL1                 TYPE                                         
SRBDUR   DS    CL2                 LENGTH                                       
SRBNW    DS    CL1                 NUMBER SPOT PER WEEK                         
SRBCOS   DS    CL4                 RATE                                         
SRBPLN   DS    CL1                 PLAN = Y                                     
SRBDTM   DS    0CL1                DAY TIME ELEMENTS                            
SRBWKS   DS    0CL1                WEEKS RUN ELEMENT                            
         DSECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
* REGENCON                                                                      
* REGENBUY                                                                      
* REGENATNA                                                                     
* REGENSDD                                                                      
* DDBUFFALOD                                                                    
* DDDPRINT                                                                      
         PRINT OFF                                                              
         DSECT                                                                  
         DS    CL4                                                              
       ++INCLUDE REGENCON                                                       
         DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
       ++INCLUDE REGENATNA                                                      
       ++INCLUDE REGENSDD                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'118REATHNAUPS05/01/02'                                      
         END                                                                    
