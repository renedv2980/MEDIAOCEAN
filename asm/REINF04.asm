*          DATA SET REINF04    AT LEVEL 149 AS OF 05/01/02                      
*PHASE T80B04A,*                                                                
         TITLE 'T80B04 - REINF04 - INFO READER AND LISTER IV'                   
*                                                                               
*********************************************************************           
*                                                                   *           
*        REINF04 --- REP INFO READER/LISTER PART 4                  *           
*                    STATION MASTER MAINTENANCE ONLY                *           
*                                                                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* OCT24/00 (BU ) --- EXPAND SIGNON CODE CHECK TO SIX CHARACTERS     *           
* MAR18/99 (SKU) --- REMOVE DEMOCON INCLUDE                         *           
* JAN28/99 (BU ) --- ORIGINAL ENTRY FROM REINF02                    *           
* JUN28/00 (BU ) --- REMOVE REFERENCES TO GLV1GOTO PER MEL HERTZIG  *           
*                                                                   *           
*                                                                   *           
*                ***  END TOMBSTONE  ***                            *           
*********************************************************************           
*                                                                               
T80B04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**8B04**,R9,R7,RR=R5                                           
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T80BFFD,RA                                                       
*                                                                               
         ST    R5,RELO1            SAVE RELO FACTOR                             
*                                                                               
*  CHECK IF PF KEY PRESSED                                                      
*                                                                               
         L     RF,ATIOB            SET A(FATIOB)                                
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
         STC   R0,PFKEY            PFKEY ADJUSTED TO 1..12 (0 = ENTER)          
         DROP  RF                                                               
*                                                                               
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         MVI   DISPCNT,0           CLEAR DISPLAY LINE COUNT                     
         XC    LASTSTAT,LASTSTAT                                                
         LA    R2,INFOUTH                                                       
**       XC    INFOUT,INFOUT                                                    
**       FOUT  (R2)                                                             
         SR    RF,RF                                                            
         L     R6,4(R1)            GET LINK                                     
         CLI   0(R6),X'5C'         LINK = STATION MASTER?                       
         BE    STAMASTR            YES -                                        
         DC    H'0'                NO  - UNIDENTIFIED                           
         EJECT                                                                  
STAMASTR EQU   *                                                                
         CLI   PFKEY,2             PF TO 'DISPLAY'?                             
         BE    STAM0012            YES                                          
         CLI   PFKEY,3             PF TO 'CHANGE'?                              
         BE    STAM0012            YES                                          
         CLI   PFKEY,4             PF TO 'ADD'?                                 
         BNE   STAM0018            NO                                           
*                                                                               
*   NEED CURSOR POSITION TO VALIDATE SIGNON REP:  ADDRESS OF CURSOR             
*        IS PASSED BACK IN FULL, IF VALID                                       
*                                                                               
         BAS   RE,CHKCURSR         VALID CURSOR POSITION:                       
         BE    STAM0004            YES -                                        
         MVC   INFMESS(L'STATLINE),STATLINE                                     
         FOUT  INFMESSH                                                         
         B     PREEXIT                                                          
STAM0004 EQU   *                                                                
         BAS   RE,VALIREP          YES - CHECK FOR VALID SIGNON ID              
         BZ    STAM0010            SIGNON IS VALID                              
         MVC   INFMESS(L'SIGNONNG),SIGNONNG                                     
         FOUT  INFMESSH                                                         
         L     R2,FULL             SET CURSOR POSITION                          
         B     PREEXIT                                                          
STAM0010 DS    0H                  CLEAR THE SCREEN                             
         L     RF,ACOMFACS         SET A(COMFACS)                               
         L     RF,CGETFACT-COMFACSD(RF)                                         
*                                  GET A(GETFACT)                               
         GOTO1 (RF),DMCB,0         GO TO GETFACT                                
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         CLC   FASYS,SRCEUTL       SIGNON ON SAME FILE?                         
         BE    STAM0012            YES                                          
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   INFMESS(L'DIFFFILE),DIFFFILE                                     
         FOUT  INFMESSH                                                         
         B     PREEXIT                                                          
STAM0012 EQU   *                                                                
         BAS   RE,CHKCURSR         VALID CURSOR POSITION:                       
         BE    STAM0014            YES -                                        
         MVC   INFMESS(L'STATLINE),STATLINE                                     
         FOUT  INFMESSH                                                         
         B     PREEXIT                                                          
STAM0014 EQU   *                                                                
         BAS   RE,SWAP             YES - SET GLOBBER AND SWAP OUT               
         B     PREEXIT                                                          
STAM0018 EQU   *                                                                
         LA    RE,INFOUTH                                                       
         LA    R1,14               CLEAR 1ST 14 LINES                           
*                                  LINE 15 CONTAINS PF KEY LEGENDS              
STAM0020 DS    0H                  CLEAR THE SCREEN                             
         OI    6(RE),X'80'                                                      
         MVC   8(79,RE),SPACES                                                  
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         BCT   R1,STAM0020         GO BACK FOR NEXT                             
         OI    1(RE),X'20'         PROTECT THE LEGEND                           
         MVC   12(25,RE),=C'PF2 DIS  PF3 CHA  PF4 ADD'                          
         FOUT  (RE)                                                             
         MVI   BYTE2,0                                                          
*                                                                               
STAM0040 CLI   NEXTBYTE,1                                                       
         BE    STAM0060                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),INFSTRT   'START FROM' FLD ON SCREEN                   
         OC    KEY+22(5),SPACES                                                 
*                                                                               
STAM0060 BAS   RE,HIGH             FIRST READ                                   
         CLI   KEY,2               SAME REC TYPE                                
         BNE   NOREC                                                            
         CLC   REPALPHA,KEY+20     SAME REP CODE                                
         BNE   NOREC                                                            
         B     STAM0120                                                         
         SPACE 1                                                                
STAM0080 EQU   *                                                                
         MVC   KEY,SAVEKEY         RESTART MAIN KEY SEQ                         
         BAS   RE,HIGH             REREAD LAST KEY                              
STAM0100 EQU   *                                                                
*                                                                               
*   TEST INDICATOR SET                                                          
*        MVI   DUMPFLAG,0                                                       
*   TEST INDICATOR SET END                                                      
*                                                                               
         BAS   RE,SEQ              READ NEXT                                    
         CLI   KEY,2               STILL STATION KEY?                           
         BNE   SETDONE             NO  - FINISHED                               
         CLC   REPALPHA,KEY+20     SAME MASTER REP?                             
         BNE   SETDONE             NO  - FINISHED                               
*                                                                               
STAM0120 EQU   *                                                                
         CLC   BYTE2,ALL           ANY MEDIA FILTER?                            
         BE    STAM0140            NO                                           
         CLC   BYTE2,KEY+26        YES - STATION AGREES?                        
         BNE   STAM0100            READ NEXT MASTER KEY                         
*                                                                               
STAM0140 MVC   SAVEKEY,KEY         SAVE MASTER KEY FOR RESTART                  
         BAS   RE,GETREC                                                        
         XC    IOAREA1(250),IOAREA1 CLEAR IOAREA                                
         XC    IOAREA1+250(250),IOAREA1+250                                     
         XC    IOAREA1+500(250),IOAREA1+500                                     
         XC    IOAREA1+750(250),IOAREA1+1000                                    
         ZICM  R1,RSTALEN,2        LOAD LENGTH OF RECORD                        
         MOVE  (IOAREA1,(R1)),RSTAREC                                           
*                                  SAVE MASTER STATION RECORD                   
         OC    AFFLTR,SPACES                                                    
         OC    GRPFLTR(2),SPACES                                                
         OC    TVBFLTR,SPACES                                                   
         OC    OWNFLTR,SPACES                                                   
         OC    ACFLTR,SPACES                                                    
         OC    OFTMFLTR,SPACES                                                  
         LA    R4,IOAREA1+RSTAELEM-RSTAREC                                      
*                                  SET LOOP START                               
**       CLC   KEY+20(6),=C'MSKLSK'                                             
**       BNE   *+6                                                              
**       DC    H'0'                                                             
STAM0160 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD REACHED?                       
         BE    STAM0080            YES - GO BACK FOR NEXT RECORD                
*                                                                               
*   TEST DUMP                                                                   
***>>    MVC   STAM0160(2),=X'0000' SET DUMP                                    
*   TEST DUMP END                                                               
*                                                                               
         CLI   0(R4),X'51'         MASTER SUBSIDIARY CURRENT ELT?               
         BE    STAM0200            YES - PROCESS IT                             
         CLI   0(R4),X'52'         MASTER SUBSIDIARY PREVIOUS ELT?              
         BE    STAM0200            YES - PROCESS IT                             
         CLI   0(R4),X'53'         MASTER SUBSIDIARY HISTORY ELT?               
         BE    STAM0200            YES - PROCESS IT                             
STAM0180 EQU   *                                                                
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     STAM0160            GO BACK FOR NEXT                             
STAM0200 EQU   *                                                                
         CLI   DISPCNT,14          ALL LINES FILLED?                            
         BNL   SETNEXT             YES - NO ROOM FOR MORE                       
STAM0220 EQU   *                                                                
         MVC   KEY+20(2),2(R4)     INSERT NEW REP CODE                          
         BAS   RE,HIGH             READ SUBSIDIARY STN RECORD                   
         CLC   KEY(27),KEYSAVE     RECORD KEY FOUND?                            
         BNE   STAM0180            NO  - SKIP THIS ENTRY                        
         ZIC   RF,DISPCNT          INCREMENT DISPLAYED COUNT                    
         LA    RF,1(RF)                                                         
         STC   RF,DISPCNT                                                       
         BAS   RE,GETREC           YES - RETRIEVE RECORD FOR DISPLAY            
*                                                                               
*                                                                               
*   TEST CONDITION                                                              
**       CLC   =C'U1',2(R4)                                                     
**       BNE   TEST0020                                                         
**       CLC   =C'WIIIF',KEY+22                                                 
**       BNE   TEST0020                                                         
**       ZIC   RF,DUMPFLAG         BUMP DUMPFLAG COUNTER                        
**       LA    RF,1(RF)                                                         
**       STC   RF,DUMPFLAG                                                      
**       B     TEST0020                                                         
*DUMPFLAG DS    F                                                               
TEST0020 EQU   *                                                                
*   TEST CONDITION END                                                          
*                                                                               
         CLC   AFFLTR,SPACES                                                    
         BE    STAM0240                                                         
         CLC   AFFLTR,RSTAAFFL                                                  
         BNE   STAM0080                                                         
*                                                                               
STAM0240 CLC   GRPFLTR(2),SPACES                                                
         BE    STAM0260                                                         
         CLC   GRPFLTR,RSTAGRUP                                                 
         BNE   STAM0080                                                         
         CLC   SGRFLTR,SPACES                                                   
         BE    STAM0260                                                         
         CLC   SGRFLTR,RSTAGRUP+1                                               
         BNE   STAM0080                                                         
*                                                                               
STAM0260 CLC   TVBFLTR,SPACES                                                   
         BE    STAM0280                                                         
         CLC   TVBFLTR,RSTATVB                                                  
         BNE   STAM0080                                                         
*                                                                               
STAM0280 CLC   OWNFLTR,SPACES                                                   
         BE    STAM0300                                                         
         CLC   OWNFLTR,RSTAOWN                                                  
         BNE   STAM0080                                                         
*                                                                               
STAM0300 EQU   *                                                                
         CLC   RNKFLTR,ALL                                                      
         BE    STAM0320                                                         
         CLC   RNKFLTR,RSTARANK                                                 
         BNE   STAM0080                                                         
*                                                                               
STAM0320 CLC   TRAFLTR,ALL                                                      
         BE    STAM0340                                                         
         CLC   TRAFLTR,RSTATRAF                                                 
         BNE   STAM0080                                                         
*                                                                               
STAM0340 EQU   *                                                                
***      CLC   ACFLTR,SPACES       ACCEPT ACT+INACT STAS?                       
***      BNE   STAM0360            YES                                          
***      CLC   RSTAEND,ALL         NO END DATE = ACTIVE                         
***      BNE   STAM0080            TAKE ACTIVES ONLY                            
*                                                                               
STAM0360 EQU   *                                                                
**       CLI   DUMPFLAG,1                                                       
**       BNE   *+6                                                              
**       DC    H'0'                                                             
         CLC   OFTMFLTR,SPACES     ANY TEAM/OFFTEAM FILTER?                     
         BE    STAM0380            NO                                           
         BAS   RE,OFTMFILT         YES                                          
         BNZ   STAM0080            NOT ACCEPTED - SKIP IT                       
*                                                                               
STAM0380 EQU   *                                                                
         ZIC   R5,0(R2)                                                         
         LA    R5,0(R2,R5)                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT                                                          
         CLI   0(R5),0                                                          
         BE    SETNEXT                                                          
         USING LMASTER,R2                                                       
*                                                                               
STAM0400 EQU   *                                                                
         MVC   LMASTER+8(L'INFOUT),SPACES      CLEAR THE LINE                   
         MVC   LMASSIGN,=C'     '  SIGNON POSITIONER                            
         CLC   LASTSTAT,RSTAKSTA   REPEATED STATION?                            
         BNE   STAM0410            NO                                           
         MVI   LMASMED+2,C'*'      YES - INSERT INDICATOR                       
STAM0410 EQU   *                                                                
         MVC   LASTSTAT,RSTAKSTA   SAVE LAST STATION DISPLAYED                  
         MVC   LMASSTA,RSTAKSTA                                                 
         MVC   LMASMED,=C'TV'                                                   
         CLI   RSTAKSTA+4,C' '                                                  
         BE    STAM0420                                                         
         MVC   LMASMED,=C'L '      MOVE IN L FOR LOW POWER STATIONS             
         CLI   RSTAKSTA+4,C'L'     IS IT A LOW POWER STAION                     
         BE    STAM0420            THEN TAKE BRANCH                             
         MVC   LMASMED,=C'FM'                                                   
         CLI   RSTAKSTA+4,C'F'                                                  
         BE    STAM0420                                                         
         MVC   LMASMED,=C'CM'      COMBINED STATION                             
         CLI   RSTAKSTA+4,C'C'                                                  
         BE    STAM0420                                                         
         MVC   LMASMED,=C'AM'                                                   
*                                                                               
STAM0420 EQU   *                                                                
         MVC   LMASMKT,RSTAMKT                                                  
         MVI   LMASSTA+4,C'-'                                                   
***>>>   EDIT  RSTACHAN,(4,LMASCNL),ALIGN=LEFT                                  
***      CLI   LMASMED,C'F'                                                     
***      BNE   STAM0440                                                         
***      LA    R1,LMASCNL+3                                                     
***      CLI   0(R1),C' '                                                       
***      BNE   *+6                                                              
***      BCTR  R1,0                                                             
***      MVC   1(1,R1),0(R1)                                                    
***      MVI   0(R1),C'.'                                                       
STAM0440 EQU   *                                                                
         MVC   LMASAFL,RSTAAFFL                                                 
         MVC   LMASGRP,RSTAGRUP                                                 
         MVI   LMASGRP+1,C'/'                                                   
         MVC   LMASSG,RSTAGRUP+1                                                
         MVC   LMASOWN,RSTAOWN                                                  
*                                                                               
STAM0500 EQU   *                                                                
         OC    RSTASTRT,RSTASTRT   ANY START DATE?                              
         BZ    STAM0520            NO                                           
         GOTO1 VDATCON,DMCB,(3,RSTASTRT),(5,LMASJDTE)                           
*                                  YES - SHOW JOIN DATE                         
STAM0520 EQU   *                                                                
         OC    RSTAEND,RSTAEND     ANY END   DATE?                              
         BZ    STAM0540            NO                                           
         GOTO1 VDATCON,DMCB,(3,RSTAEND),(5,LMASLDTE)                            
*                                  YES - SHOW LEAVE DATE                        
STAM0540 EQU   *                                                                
         MVC   LMASPOWR,RSTAKREP   INSERT REP CODE                              
         CLC   LASTREP,RSTAKREP    SAME REP?                                    
         BE    STAM0560            YES - REUSE NAME                             
         MVC   LASTREP,RSTAKREP    SAVE REP CODE                                
         MVC   SAVSTAKY,KEY        SAVE STATION KEY                             
         XC    KEY,KEY                                                          
         MVI   KEY,1               INSERT REC TYPE                              
         MVC   KEY+25(2),RSTAKREP  INSERT REP CODE                              
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                REP RECORD MUST BE ON FILE                   
         BAS   RE,GETREC           READ RECORD INTO IOAREA                      
         MVC   LASTREPN,RREPSHRT   SAVE SHORT NAME                              
         MVC   KEY(27),SAVSTAKY    RESTORE STATION KEY                          
STAM0560 EQU   *                                                                
         MVC   LMASREP,LASTREPN    LOAD 13 CHARS OF NAME                        
         OC    LMASTER+8(L'INFOUT),SPACES                                       
*                                  CLEAR BINARY ZEROS TO SPACES                 
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         BAS   RE,CLRLINE                                                       
         B     STAM0180            GO BACK FOR NEXT STATION                     
*                                     IN MASTER RECORD                          
SAVSTAKY DS    CL27                STATION KEY STORAGE: TEMP                    
         DS    0F                                                               
*                                                                               
STAM0580 ST    RE,FULL             SAVE LINK REGISTER                           
         OC    LMASTER+8(L'INFOUT),SPACES                                       
*                                  CLEAR BINARY ZEROS TO SPACES                 
         FOUT  (R2)                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         LA    RE,INFLAST                                                       
         CR    R2,RE               MAY BE AT BOTTOM OF SCREEN                   
         BNL   STAM0080                                                         
         BAS   RE,CLRLINE                                                       
         L     RE,FULL             BACK TO CALLER                               
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
*   OFTMFILT:  CHECK STATION RECORD FOR COMPLIANCE WITH OFFICE/TEAM             
*        OR TEAM FILTER.  IF NOT SELECTED, RETURN CC NOT = ZERO.                
*                                                                               
OFTMFILT NTR1                                                                   
         LA    R3,RSTAELEM         A(DESCRIPT ELEMENT)                          
OTMF0020 EQU   *                                                                
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         CLI   0(R3),0             END OF RECORD?                               
         BE    OTMF0200            YES - NOT FOUND                              
         CLI   0(R3),X'04'         OFFICE/TEAM ELEMENT?                         
         BNE   OTMF0020            NO  - GO BACK FOR NEXT                       
         USING RSTAOTEL,R3         YES - CHECK IT OUT                           
         TM    FLTRBYT3,X'01'      OFFICE/TEAM REQUEST?                         
         BNO   OTMF0080            NO  - CHECK FOR TEAM ONLY                    
         CLC   OFTMFLTR,RSTAOTOF   FOUND? FILTER VS OFF/TEAM IN ELT             
         BNE   OTMF0020            NO  - GO BACK FOR NEXT                       
         B     OTMF0160            YES - RETURN CC = ZERO                       
OTMF0080 EQU   *                                                                
         TM    FLTRBYT3,X'02'      TEAM REQUEST?  (REDUNDANT)                   
         BO    *+6                 YES                                          
         DC    H'0'                SHOULDN'T HAPPEN                             
         CLC   OFTMFLTR(2),RSTAOTTM FOUND? FILTER VS TEAM IN ELT                
         BNE   OTMF0020            NO  - GO BACK FOR NEXT                       
OTMF0160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     OTMF0240                                                         
OTMF0200 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
OTMF0240 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
CLRLINE  EQU   *                                                                
         CLC   12(25,R2),=C'PF2 DIS  PF3 CHA  PF4 ADD'                          
*                                  PF-KEY LINE?                                 
         BER   RE                  YES - DON'T CLEAR IT                         
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LTR   R5,R5                                                            
         BZ    *+8                                                              
         SH    R5,=H'9'                                                         
         EX    R5,OCLINE                                                        
         BZR   RE                                                               
         EX    R5,XCLINE                                                        
         FOUT  (R2)                                                             
         BR    RE                                                               
*                                                                               
XCLINE   XC    8(0,R2),8(R2)                                                    
*                                                                               
OCLINE   OC    8(0,R2),8(R2)                                                    
*                                                                               
NOREC    LA    R2,INFTITLH                                                      
NOREC2   BAS   RE,CLRLINE                                                       
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         CLI   0(R2),0                                                          
         BNE   NOREC2                                                           
*                                                                               
         LA    R2,INFRCRDH                                                      
         BAS   RE,INVAL                                                         
         MVC   INFMESS(L'MSG3),MSG3                                             
         FOUT  INFMESSH                                                         
         B     PREEXIT                                                          
         SPACE 2                                                                
FLOAT    OI    0(R8),C' '                                                       
         CLI   0(R8),C' '                                                       
         BNE   FLOAT2                                                           
         BCT   R8,FLOAT                                                         
*                                                                               
FLOAT2   LA    R8,2(R8)                                                         
         BR    RE                                                               
         EJECT                                                                  
INVAL    NI    INFRCRDH+4,X'DF'                                                 
         NI    INFSTRTH+4,X'DF'                                                 
         NI    INFFLTRH+4,X'DF'                                                 
         NI    INFOPTNH+4,X'DF'                                                 
         MVI   NEXTBYTE,0                                                       
         BR    RE                                                               
         SPACE 2                                                                
SETNEXT  MVI   NEXTBYTE,1                                                       
         MVC   INFMESS(L'MSG2),MSG2                                             
         FOUT  INFMESSH                                                         
***      LA    R2,INFNEXTH                                                      
         LA    R2,INFSTRTH         SET CURSOR TO 'START'                        
***      ST    R2,DUB              SET CURSOR ADDRESS                           
         B     PREEXIT                                                          
         SPACE 2                                                                
SETDONE  FOUT  (R2)                                                             
         OC    INFOUT,INFOUT                                                    
         BZ    NOREC                                                            
         BAS   RE,INVAL                                                         
         MVC   INFMESS(L'MSG1),MSG1                                             
         FOUT  INFMESSH                                                         
*                                  CLEAR REST OF PAGE                           
DONE10   SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         CLI   0(R2),0                                                          
         BE    DONE20                                                           
         BAS   RE,CLRLINE                                                       
         B     DONE10                                                           
*                                                                               
DONE20   EQU   *                                                                
         LA    R2,INFSTRTH         SET CURSOR TO 'START'                        
         ST    R2,DUB              SET CURSOR ADDRESS                           
***      LA    R2,INFRCRDH                                                      
         B     PREEXIT                                                          
         EJECT                                                                  
***>>>                                                                          
VALIREP  NTR1                                                                   
         MVC   TEMPAIO,AIOAREA     SAVE A(IOAREA IN USE)                        
         LA    RF,IOAREA1                                                       
         ST    RF,AIOAREA          SET A(IOAREA1) FOR CONTROL READ              
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
         L     RF,FULL             A(CURSOR ON LINE)                            
         CLC   5(8,RF),SPACES      ANY VALUE IN FIELD?                          
         BE    VREP0080            NO  - REQUIRED                               
***      CLI   5(RF),10            MAX KEY FIELD?                               
***      BNH   VREP0005            WITHIN REASON: S/B 6                         
***      LA    RF,10               SET TO MAX                                   
***      B     VREP0007                                                         
*REP0005 EQU   *                                                                
*        ZIC   RE,5(RF)            L(UPDATE REP INPUT)                          
*REP0007 EQU   *                                                                
***      BCTR  RE,0                -1 FOR EX                                    
***      EX    RE,VREP0800         MOVE BY LENGTH                               
***      EX    RE,VREP0810         MOVE BY LENGTH: SAVE CODE                    
         MVC   WORK+15(6),8(RF)    LOAD UPDATE REP                              
         MVC   SRCESIGN(0),8(RF)   SAVE UPDATE REP                              
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
         OC    SRCESIGN(6),SPACES  SET REMAINDER TO SPACES                      
         PRINT GEN                                                              
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIOAREA                 
         PRINT NOGEN                                                            
         CLI   8(R1),0             FOUND?                                       
         BNE   VREP0080            NOT FOUND - SHOW MESSAGE                     
         L     R1,AIOAREA                                                       
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BNE   VREP0080            NOT FOUND - SHOW MESSAGE                     
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
VREP0020 EQU   *                                                                
         CLI   0(R1),X'06'         AGENCY ID ELEMENT?                           
         BNE   VREP0030            NO                                           
         MVC   SRCEREP,2(R1)       YES - SAVE 2-CHAR REP ID                     
         B     VREP0040            BUMP TO NEXT ELEMENT                         
VREP0030 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   VREP0040            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    VREP0060            YES                                          
VREP0040 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   VREP0020            NO                                           
         B     VREP0080            NO X'21' - ERROR MESSAGE                     
VREP0060 EQU   *                                                                
         MVC   SRCEUTL,3(R1)       SAVE SOURCE UTL NUMBER                       
****>>>> GOTO1 =A(GETREPNM),DMCB,(RC),RR=Y                                      
****>>>>                           RETRIEVE REP'S SHORT NAME                    
         MVC   AIOAREA,TEMPAIO     RESET A(IOAREA IN USE)                       
         SR    R0,R0               SET CC = ZERO                                
         B     VREP0100                                                         
VREP0080 EQU   *                                                                
         MVC   AIOAREA,TEMPAIO     RESET A(IOAREA IN USE)                       
         LTR   RB,RB               SET CC NOT ZERO:  ERROR                      
VREP0100 EQU   *                                                                
         XIT1                                                                   
VREP0800 MVC   WORK+15(0),INFSTRT  LOAD UPDATE REP BY LENGTH                    
VREP0810 MVC   SRCESIGN(0),INFSTRT SAVE UPDATE REP BY LENGTH                    
         EJECT                                                                  
***>>>                                                                          
**>>**>>                                                                        
* BUMP DOWN SCREEN OUT LINES TO DETERMINE IF CURSOR ON VALID LINE               
* RETURN SCREEN LINE ADDR IN FULL                                               
* R3 USED FOR CURSOR ADDR  R4 SCREEN OUT LINE ADDR                              
                                                                                
CHKCURSR NTR1                                                                   
         L     R2,ATIOB                                                         
         USING TIOBD,R2                                                         
         LH    R3,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         AR    R3,RA               RA=TWA                                       
         LA    R4,INFOUTH          FIRST OUT LINE ON SCRREN                     
         CR    R3,R4                                                            
         BL    CCNO                CURSOR CAN'T BE LESS THAN 1ST LINE           
         BE    CC20                FOUND                                        
         BH    CC15                GET NEXT LINE                                
*                                                                               
CC10     CR    R3,R4                                                            
         BE    CC20                                                             
         BL    CCYES                                                            
         BH    CC15                                                             
                                                                                
CC15     ST    R4,FULL             STORE ADDR OF OUT LINE                       
         ZIC   R1,0(R4)                                                         
         C     R1,=F'9'            END OF SCREEN                                
         BNH   CCNO                                                             
         AR    R4,R1               NO/BUMP TO NEXT SCREEN OUTLINE               
         B     CC10                                                             
                                                                                
* - IF CURSOR POINTS TO START OF LINE                                           
CC20     ST    R4,FULL             PASS SCREEN LINE ADDR IN FULL                
*                                                                               
CCYES    SR    R3,R3                                                            
CCNO     LTR   R3,R3                                                            
CCX      XIT1                                                                   
         DROP  R2                                                               
**>>**>>                                                                        
***<<<<                                                                         
*                                                                               
*    GLOBBER PASSES DATA TO CONTRACT PROGRAM                                    
*    INPUT ADDRESS OF SCREEN LINE IN FULL                                       
*                                                                               
SWAP     NTR1                                                                   
         L     R5,ACOMFACS                                                      
         USING COMFACSD,R5                                                      
         XC    BLOCK(256),BLOCK                                                 
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'INF'    INFO PROGRAM                                 
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'FIL'    FILE PROGRAM                                 
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R1                                                               
**                                 SET UP THE TRANSFER CONTROL BLOCK            
                                                                                
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*   SET UP GLOBBER ELEMENT FOR FILE DISPLAY/ADD/CHANGE                          
*                                                                               
         XC    TEMPWORK,TEMPWORK                                                
         LA    R3,TEMPWORK                                                      
         USING GLFILEL,R3                                                       
*                                                                               
         CLI   PFKEY,2             DISPLAY REQUEST?                             
         BNE   SWAP0020            NO                                           
         MVC   GLFILFA(3),=C'DIS'  YES                                          
         B     SWAP0100                                                         
SWAP0020 EQU   *                                                                
         CLI   PFKEY,3             CHANGE  REQUEST?                             
         BNE   SWAP0040            NO                                           
         MVC   GLFILFA(3),=C'CHA'  YES                                          
         B     SWAP0100                                                         
SWAP0040 EQU   *                                                                
         CLI   PFKEY,4             ADD     REQUEST?                             
         BNE   SWAP0060            NO                                           
         MVC   GLFILFA(3),=C'CHA'  YES                                          
         B     SWAP0100                                                         
SWAP0060 EQU   *                                                                
         DC    H'0'                UNRECOGNIZED PFKEY ALLOWED                   
SWAP0100 EQU   *                                                                
         L     R2,FULL             ADDR OF OUT LINE                             
         USING LMASTER,R2                                                       
*                                                                               
         MVC   GLFILREC,=CL6'STAM' INSERT RECTYPE                               
         MVC   GLFILSTA(4),LMASSTA INSERT STATION CALL LETTERS                  
         MVC   GLFILSTA+4(1),LMASMED                                            
*                                  INSERT STATION MEDIA                         
         MVC   GLFILPOW,LMASPOWR   INSERT POWER CODE                            
*                                                                               
*   RETRIEVE REP CODE FOR REP NAME                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,1               INSERT REC TYPE                              
         MVC   KEY+25(2),LMASPOWR  INSERT REP CODE FROM LINE                    
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                REP RECORD MUST BE ON FILE                   
         L     RF,AIOAREA                                                       
         BAS   RE,GETREC           READ RECORD INTO IOAREA                      
         MVC   GLFILNAM,RREPSHRT   INSERT SHORT NAME                            
*                                                                               
         CLI   PFKEY,4             ADD     REQUEST?                             
         BNE   SWAP0120            NO                                           
*                                  YES - GET REP INFO FOR ADD                   
         XC    KEY,KEY                                                          
         MVI   KEY,1               INSERT REC TYPE                              
         MVC   KEY+25(2),SRCEREP   INSERT REP CODE FROM ADD REP                 
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                REP RECORD MUST BE ON FILE                   
         L     RF,AIOAREA                                                       
         BAS   RE,GETREC           READ RECORD INTO IOAREA                      
         MVC   GLFILARP,SRCEREP    INSERT REP CODE FROM ADD                     
         MVC   GLFILANM,RREPSHRT   INSERT SHORT NAME                            
*                                                                               
SWAP0120 EQU   *                                                                
         GOTO1 CGLOBBER,DMCB,=C'PUTD',TEMPWORK,GLFILLNQ,GLRINFIL                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
STGX     XIT1                                                                   
         B     PREEXIT                                                          
PREEXIT  EQU   *                                                                
         LA    R1,INFBLEGH              SET A(LEGEND)                           
         OI    1(R1),X'20'              TURN ON PROTECT BIT                     
         FOUT  INFBLEGH                                                         
         B     EXIT                                                             
*                                                                               
         DROP  R2,R3,R5                                                         
***<<<<                                                                         
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MSG1     DC    C'ACTION COMPLETED - ENTER NEXT REQUEST'                         
MSG2     DC    C'MORE RECORDS AVAILABLE - HIT ''ENTER'' TO CONTINUE'            
MSG3     DC    C'NO RECORDS ON FILE - ENTER NEXT REQUEST'                       
SIGNONNG DC    C'PF4 KEY REQUIRES VALID SIGNON ON SELECTED LINE'                
STATLINE DC    C'CURSOR NOT POSITIONED TO A VALID STATION LINE'                 
DIFFFILE DC    C'SIGNON YOU SPECIFIED NOT ON YOUR FILE - REJECTED '             
SPACES   DC    CL80' '                                                          
ALL      DC    5X'00'                                                           
ELCODE   DC    1X'00'                                                           
DISPCNT  DS    XL1                 DISPLAY LINE COUNT                           
LASTSTAT DS    CL5                                                              
SRCESIGN DS    CL8                                                              
SRCEREP  DS    CL2                                                              
SRCEUTL  DS    CL1                                                              
LASTREP  DS    CL2                                                              
LASTREPN DS    CL20                                                             
BLOCK    DS    256C                                                             
GLOBKEY  DS    CL48                                                             
TEMPAIO  DS    CL4                                                              
*   TEMPWORK DS    CL96      ---NOW IN REINFWRK                                 
       ++INCLUDE REINFWRK                                                       
DEMOD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REGLFILE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'149REINF04   05/01/02'                                      
         END                                                                    
