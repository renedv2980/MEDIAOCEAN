*          DATA SET REREPSW02  AT LEVEL 177 AS OF 02/01/12                      
*PHASE RESW02A                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREPSW02 (RESW02) --- REP FILE DATA SWITCH'                    
*                                                                               
********************************************************************            
*        REREPSW02 --- REP SFM STATION/SALESPERSON/ADVERTISER/     *            
*                       PRODUCT/AGENCY CONTRACT, ET AL, RECORD     *            
*                       SWITCHER                                   *            
* ---------------------------------------------------------------- *            
*     QUESTOR  =  BIGWORK  - ASK FOR LARGE COVAIL SPACE            *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
*                                                                  *            
* APR--/89 (MRR) --- MAKE TABLE BIGGER                             *            
*                                                                  *            
* APR20/89 (MRR) --- CHANGE SWITCH PURGER FROM 91 DAYS TO 14       *            
*                                                                  *            
* 08/31/89  PJS  --- ADD COMMISSION RECORD SWITCHING -             *            
*                     STATION AND ADVERTISER KEY FIELDS            *            
*                                                                  *            
*                --- INTEREP MASTER/SUBSIDIARY PROCESSING          *            
*                                                                  *            
* 02JAN90   EFJ  --- HAPPY NEW YEAR - CHANGE STATION RECS ON SW    *            
*                                                                  *            
* 17MAR91  (EFJ) --- ADD ++KILL ROSENBERG CODE                     *            
*                                                                  *            
* 26APR91  (EFJ) --- ADD FOX TO AFFILIATE LIST                     *            
*                                                                  *            
* SEP24/91 (MRR) --- REMOVE BUILDAREA BUFFER (1.8MEG) AND MAKE A   *            
*                     COVAIL CALL                                  *            
*                                                                  *            
* JUN26/92 (BU ) --- ADD BUY RECORDS FOR REP-TO-SPOT STATION       *            
*                    CHANGE                                        *            
*                                                                  *            
* SEP30/92 (BU ) --- STATION CHANGE:  ADD COMBO STATION CHECK      *            
*                    CONTRACT RECORD: IF STATION CHANGE, CHECK     *            
*                                     COMBO CONTROL ELEMENT        *            
*                    AGENCY CHANGE:   DON'T DELETE OLD CODE IF     *            
*                                     CHANGE IS CORPORATE TO       *            
*                                     AGENCY/OFFICE.               *            
*                                                                  *            
* JUN29/93 (BU ) --- MAKE CHANGES FOR AVERAGE UNIT RATE.           *            
*                                                                  *            
* JUL16/93 (BU ) --- FIX STATION/GROUP MISMATCH BUG...             *            
*                                                                  *            
* JAN24/95 (BU ) --- ADD TIME STAMP/READ COUNT DISPLAY             *            
*                                                                  *            
* AUG14/95 (SKU) --- FIX AGENCY SWITCH BUG OF SWITCHING FROM       *            
*                    OFFICE TO CORPORATE                           *            
*                                                                  *            
* NOV16/95 (BU ) --- NEW VERSION FOR KATZ MASTER RECORD SETUP      *            
*                                                                  *            
* DEC06/95 (BU ) --- 'DDSSTATION' ACTION USE                       *            
*                                                                  *            
* DEC08/95 (BU ) --- SET KATZ AGY/ADV CONVERTED ORDER SWITCH FLAG  *            
*                                                                  *            
* JAN18/96 (SKU) --- INCREASE REC FROM 1,008 TO 2,008 BYTES        *            
*                                                                  *            
* FEB29/96 (WSB) --- SWITCH DARE, MKG, STRATEGY, AND STA SET RECS  *            
*                    ON A STATION SWITCH. SWITCH AGY II RECS. ADD  *            
*                    DEVSAL SWITCH AND USE TO SWITCH CONTRACTS.    *            
*                    ADD START DATE FILTER FOR CONTRACTS. ADD PP   *            
*                    SWITCH AND USE TO SWITCH PRODUCT RECS. SWITCH *            
*                    ADV, PP, AGY, AND SALES SET RECS WHEN ORIG    *            
*                    RECS ARE DELETED (NO FILTERS). ONLY CHANGE    *            
*                    COMM RECS ON ADV SWITCH IF NO FILTS. SWITCH   *            
*                    EOP RECS IF AGY, ADV, OR SAL RECS ARE DELETED *            
*                    (NO FILTERS).  SWITCH STA ON EOP RECS ON STA  *            
*                    SWITCH. SWITCH DIRECT RESP RECS ON STA SWITCH *            
*                                                                  *            
* APR03/96 (WSB) --- SWITCH PROPOSAL RECS ON A STATION, OFF, SAL,  *            
*                    TEAM, ADV, AGY, GRO, OR DEVSAL SWITCH         *            
*                                                                  *            
* JUN26/96 (WSB) --- FIX BUG: AUR IS '2C' NOT '27' (LABEL BLDT0260) *           
*                                                                  *            
* SEP23/96 (WSB) --- ADD 'UPN' AND 'WBT' TO AFFTAB                 *            
*                                                                  *            
* OCT04/96 (WSB) --- AS PER BILL, REMOVE 'TEAM' TEST FROM CONTRACT *            
*                    AND PROPOSAL SWITCH OF SALESPERSON CODE       *            
*                                                                  *            
* NOV17/97 (JRD) --- ADD X'20' ACL RECORD TO STATION SWITCH        *            
*                                                                  *            
* FEB19/98 (JRD) --- MAKE REC 6144 BYTES                           *            
*                                                                  *            
* MAR24/98 (SKU) --- APPLY OFFICE SWITCH                           *            
*                                                                  *            
* JUN15/98 (JRD) --- ADD SELLERS WORKSHEET FOR WINDOWS REC (4302)  *            
*                                                                  *            
* AUG03/98 (AST) --- CHANGE 'ST' STATION SET RECORDS IF THERE IS   *            
*                      IS A STATION SWITCH                         *            
*                                                                  *            
* AUG03/98 (AST) --- CHANGE '04' ELEM OF PRODUCT RECORDS IF THERE  *            
*                      IS AN AGENCY SWITCH                         *            
*                                                                  *            
* AUG19/98 (SKU) --- FIX INV SWITCH TO ALLOW LOW POWER STATION     *            
*                                                                  *            
* OCT16/98 (RHV) --- REMOVE TEAM FROM SAL SWITCH                   *            
*                                                                  *            
* OCT22/98 (JRD) --- ADD SONNET BOXID RECORDS TO STATION SWITCH    *            
*                                                                  *            
* NOV16/98 (SKU) --- INVENTORY SWITCH BUG FIX                      *            
*                                                                  *            
* NOV30/98 (BU ) --- TAKEOVER CHANGES                              *            
*                    MAKEGOOD S/P-TEAM SWITCH                      *            
*                                                                  *            
* 03JUN99  (BU ) --- ADD PAX TO AFFILIATE LIST                     *            
*                                                                  *            
* 25JUN99  (SKU) --- ADD OTH TO AFFILIATE LIST                     *            
*                                                                  *            
* 26OCT00  (BU ) --- KATZ RADIO:  CLEAR CHANNEL SPLITOFF           *            
*                                                                  *            
* 17DEC01  (BU ) --- KATZ RADIO:  COMPENSATION S/P MODS            *            
*                                                                  *            
* 13JUL02  (BU ) --- ADD 'TF ' TO AFFILIATE LIST                   *            
*                                                                  *            
* 22NOV02  (BU ) --- DARE PROCESSING:  S/P-P/P                     *            
*                                                                  *            
* 08AUG03  (BU ) --- ADD DARE/ROM EOP ADV RECORD                   *            
*                                                                  *            
* 27JAN05  (BU ) --- INVENTORY / SATELLITE DATA STAYS AS '-1'      *            
*                                                                  *            
* 05SEP06  (BU ) --- UPDATE AFFIL LIST: PBS, CBL, GEM, MUN, CW,    *            
*                    MNT                                           *            
*                                                                  *            
* 12OCT06  (BU ) --- ADD 'ION' TO AFFILS                           *            
*                                                                  *            
* 02JAN07  (BU ) --- ADD 'NBW' TO AFFILS                           *            
*                                                                  *            
* 07MAY07  (HQ ) --- ADD 'LAT' TO AFFILS                           *            
*                                                                  *            
* 21JUN07  (SKU) --- MOVE AFFILIATE LIST TO COMMON PAN BOOK        *            
*                                                                  *            
* 22APR09  (SKU) --- SUPPORT NEW INVENTORY KEY                     *            
*                                                                  *            
* 15NOV11  (RCR) --- NEW BLKSIZE FOR RECOVERY TAPE                 *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
RESW02   CSECT                                                                  
         NMOD1 MYWORKX-MYWORKD,**RESW**,R9,R7,RR=R5                             
         USING MYWORKD,RC                                                       
         LR    RE,RC                                                            
         AH    RE,=Y(RCVREC-MYWORKD)                                            
         ST    RE,ARCVREC                                                       
         AH    RE,=Y(REC-RCVREC)                                                
         ST    RE,AREC                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
SWXIT    XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         OPEN  (FILEIN,(INPUT))                                                 
**       LTR   RF,RF                                                            
**       BNZ   0001                                                             
         OPEN  (FILOUTA,(OUTPUT))                                               
**       LTR   RF,RF                                                            
**       BNZ   0003                                                             
         OPEN  (FILOUTB,(OUTPUT))                                               
**       LTR   RF,RF                                                            
**       BNZ   0005                                                             
         OPEN  (RCVROUT,(OUTPUT))                                               
**       LTR   RF,RF                                                            
**       BNZ   0007                                                             
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
***      GOTO1 (RF),DMCB,C'GET',0200000,4000000   TEST SIZE                     
         CLC   =C'BIGWORK',QUESTOR NEED LARGE SPACE?                            
         BE    SW12                YES                                          
*                                  NO                                           
         GOTO1 (RF),DMCB,C'GET',1800000,4000000   PROD SIZE                     
         B     SW14                                                             
SW12     DS    0H                                                               
         GOTO1 (RF),DMCB,C'GET',4500000,7000000   XTRA SIZE                     
SW14     DS    0H                                                               
         OC    P2(4),P2                                                         
         BNZ   SW15                                                             
         DC    H'0'                                                             
SW15     DS    0H                                                               
         MVC   ABLDAREA,P2                                                      
         MVC   LBLDAREA,P3                                                      
         SR    R2,R2                                                            
         L     R3,P3                                                            
         LA    R4,LENFIXT                                                       
         DR    R2,R4                                                            
         ST    R3,NUMBLD                                                        
         SPACE 1                                                                
         CLC   QASAT,SPACES                                                     
         BE    SW50                                                             
         MVC   DUB(6),QASAT        CAN OVERRIDE TODAY                           
         B     SW100                                                            
SW50     GOTO1 DATCON,DMCB,(5,0),DUB      TODAY'S DATE                          
SW100    GOTO1 GETDAY,DMCB,DUB,FULL       DAY OF WEEK                           
         SR    R3,R3                                                            
         IC    R3,DMCB                                                          
         BCTR  R3,0                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,DMCB,DUB,DMCB+12,(R3)                                      
         GOTO1 DATCON,DMCB,DMCB+12,(3,MDATE)                                    
         SPACE 1                                                                
*  GET DATE 2 WEEKS FROM MDATE TO SELF-PURGE SWITCH RECORDS                     
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(3,MDATE),(0,DUB)   PUT MONDATE IN EBCDIC            
         LA    R3,14               14 DAYS = 2 WEEKS                            
         LNR   R3,R3                                                            
         GOTO1 ADDAY,DMCB,DUB,DMCB+12,(R3)     SUBTRACT 2 WEEKS                 
         GOTO1 DATCON,DMCB,DMCB+12,(3,PRGYMD)  PUT PURGE DATE IN YMD            
         SPACE 1                                                                
         ZAP   SWICNT,=P'0'        START COUNT AT ZERO AND                      
         ZAP   SWINTH,=P'1'        PRINT EVERY SWITCH RECORD                    
         XC    TBLCNT,TBLCNT       CLEAR OUT TABLE ENTRIES COUNTER              
         SPACE 1                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(11),=C'**BLDMAST**'                                          
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         BAS   RE,BLDMAST          BUILD MASTER RECORD LIST                     
         SPACE                                                                  
*                                                                               
*- READ SWITCH RECORDS AND TABLE THEM UP.                                       
*                                                                               
*   TEST                                                                        
*        MVC   P+1(11),=C'**BLDTBL **'                                          
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         BAS   RE,BLDTBL           BUILD TABLE FROM X'28' RECORDS               
*                                                                               
*   TEST                                                                        
*        MVC   P+1(11),=C'**BLDRCVH**'                                          
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         BAS   RE,BLDRCVH          BUILD RECOVERY HEADER                        
*                                                                               
*   TEST                                                                        
*        MVC   P+1(11),=C'**RCV BACK*'                                          
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVI   FIRSTCON,C'Y'       NEED TO WRITE 1ST CONTRACT TO RCVOUT         
*                                  TO ENSURE AT LEAST ONE RECORD                
         EJECT                                                                  
*    READ IN RECORDS FROM REP FILE TAPE AND PROCESS THEM                        
SW200    DS    0H'0'                                                            
******   AP    RECCNT,=P'1'        ***TEST-READ ONLY 'N' RECORDS***             
******   CP    RECCNT,=P'1000000'  ***TEST***                                   
******   BH    SWEOF               ***TEST***                                   
         L     R0,AREC                                                          
         SH    R0,=H'4'            POINT TO REC-4                               
         GET   FILEIN,(R0)         (ALLOW FOR 4-BYTE HEADER)                    
         SPACE 1                                                                
*  SET 2X'00' AT EOR (FOR GETEL)                                                
         SPACE 1                                                                
         L     RE,AREC                                                          
         SH    RE,=H'4'            POINT TO REC-4                               
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)       PUT ZERO AT END OF RECORD                    
*                                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(11),=C'**TIMSTAMP*'                                          
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         GOTO1 =A(TIMSTAMP),RR=Y                                                
*                                                                               
*                                                                               
*   TEST END OF RUN  -  IF USED, MUST BE REMOVED PRIOR TO                       
*        PROMOTING VERSION FOR PRODUCTION  !!                                   
*        FAILURE TO DO SO WILL RESULT IN THE FINAL FILE BEING                   
*        CHOPPED OFF AFTER THE RECORD INDICATED !!                              
*        THIS WOULD BE A DISASTER   !!!!!                                       
*                                                                               
         L     R3,AREC                                                          
****     CLI   0(R3),RINVKTYQ      **TEST** RECTYPE REACHED?                    
****     BH    SWEOF               **TEST** YES - END JOB                       
*                                                                               
*   TEST                                                                        
**       MVC   P+1(07),=C'**AREC='                                              
**       MVC   P+10(48),0(R3)                                                   
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
SW202    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW210               NO                                           
         CLC   =C'STAT',QUESTOR+6  YES - STATION?                               
         BNE   SW220               NO                                           
SW210    EQU   *                                                                
         CLI   0(R3),X'02'         STATION                                      
         BE    STATION                                                          
         SPACE 1                                                                
SW220    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW230               NO                                           
         CLC   =C'SPER',QUESTOR+6  YES - SALESPERSON?                           
         BNE   SW240               NO                                           
SW230    EQU   *                                                                
         CLI   0(R3),X'06'         SALESPERSON                                  
         BE    SALPRSON                                                         
         SPACE 1                                                                
SW240    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW250               NO                                           
         CLC   =C'ADV',QUESTOR+6   YES - ADVERTISER?                            
         BNE   SW260               NO                                           
SW250    EQU   *                                                                
         CLI   0(R3),X'08'         ADVERTISER                                   
         BE    ADVERTSR                                                         
         SPACE 1                                                                
SW260    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW270               NO                                           
         CLC   =C'PROD',QUESTOR+6  YES - PRODUCT?                               
         BNE   SW280               NO                                           
SW270    EQU   *                                                                
         CLI   0(R3),X'09'         PRODUCT                                      
         BE    PRODUCT                                                          
         SPACE 1                                                                
SW280    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW290               NO                                           
         CLC   =C'AGY',QUESTOR+6   YES - AGENCY?                                
         BNE   SW300               NO                                           
SW290    EQU   *                                                                
         CLI   0(R3),X'0A'         AGENCY                                       
         BE    AGENCY                                                           
         CLI   0(R3),X'1A'         AGENCY II                                    
         BE    AGENCY                                                           
         SPACE 1                                                                
SW300    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW310               NO                                           
         CLC   =C'BUY',QUESTOR+6   YES - BUY?                                   
         BNE   SW320               NO                                           
SW310    EQU   *                                                                
         CLI   0(R3),X'0B'         BUY                                          
         BE    BUY                                                              
         SPACE 1                                                                
SW320    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW330               NO                                           
         CLC   =C'CON',QUESTOR+6   YES - CONTRACT?                              
         BNE   SW340               NO                                           
SW330    EQU   *                                                                
         CLI   0(R3),X'0C'         CONTRACT                                     
         BE    CONTRACT                                                         
         SPACE 1                                                                
SW340    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW350               NO                                           
         CLC   =C'INV',QUESTOR+6   YES - INVENTORY?                             
         BNE   SW360               NO                                           
SW350    EQU   *                                                                
         CLI   0(R3),RINVKTYQ      INVENTORY                                    
         BE    INVNTRY                                                          
         SPACE 1                                                                
SW360    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW370               NO                                           
         CLC   =C'BUD',QUESTOR+6   YES - BUDGET?                                
         BNE   SW380               NO                                           
SW370    EQU   *                                                                
         CLI   0(R3),X'13'         BUDGET                                       
         BE    BUDGET                                                           
         SPACE 1                                                                
SW380    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW390               NO                                           
         CLC   =C'SDD',QUESTOR+6   YES - SDD?                                   
         BNE   SW400               NO                                           
SW390    EQU   *                                                                
         CLI   0(R3),X'26'         SDD                                          
         BE    SDD                                                              
         SPACE 1                                                                
SW400    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW410               NO                                           
         CLC   =C'ATH',QUESTOR+6   YES - ATHENA?                                
         BNE   SW420               NO                                           
SW410    EQU   *                                                                
         CLI   0(R3),X'27'         ATHENA                                       
         BE    ATHENA                                                           
         SPACE 1                                                                
SW420    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW430               NO                                           
         CLC   =C'SWI',QUESTOR+6   YES - SWITCH?                                
         BNE   SW440               NO                                           
SW430    EQU   *                                                                
         CLI   0(R3),X'28'         SWITCH RECORDS - FOR SELF PURGING            
         BE    SWITCH              AFTER 2 WEEKS                                
         SPACE                                                                  
SW440    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW450               NO                                           
         CLC   =C'COMM',QUESTOR+6  YES - COMMISSION?                            
         BNE   SW460               NO                                           
SW450    EQU   *                                                                
         CLI   0(R3),X'29'         COMMISSION RECORD                            
         BE    COMMREC                                                          
         SPACE                                                                  
SW460    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW470               NO                                           
         CLC   =C'DAR',QUESTOR+6   YES - DARE?                                  
         BNE   SW480               NO                                           
SW470    EQU   *                                                                
         CLI   0(R3),X'41'         DARE                                         
         BE    DARE                                                             
         CLI   0(R3),X'51'         DARE HEADER                                  
         BE    DARE                                                             
         SPACE                                                                  
SW480    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW490               NO                                           
         CLC   =C'MKG',QUESTOR+6   YES - MAKEGOOD?                              
         BNE   SW492               NO                                           
SW490    EQU   *                                                                
         CLI   0(R3),X'11'         MAKEGOOD                                     
         BE    MAKEGOOD                                                         
         SPACE                                                                  
***>>>                                                                          
SW492    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW494               NO                                           
         CLC   =C'TKO',QUESTOR+6   YES - TAKEOVER?                              
         BNE   SW500               NO                                           
SW494    EQU   *                                                                
         CLI   0(R3),X'1F'         TAKEOVER                                     
         BE    TAKEOVER                                                         
         SPACE                                                                  
***>>>                                                                          
SW500    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW510               NO                                           
         CLC   =C'SET',QUESTOR+6   YES - SET?                                   
         BNE   SW520               NO                                           
SW510    EQU   *                                                                
         CLI   0(R3),X'38'         SET RECORD?                                  
         BE    SET                                                              
         SPACE                                                                  
SW520    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW530               NO                                           
         CLC   =C'DSP',QUESTOR+6   YES - DEVELOPMENTAL SALESPERSON?             
         BNE   SW540               NO                                           
SW530    EQU   *                                                                
         CLI   0(R3),X'3A'         DEVELOPMENTAL SALESPERSON                    
         BE    DEVSALPR                                                         
         SPACE 1                                                                
SW540    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW550               NO                                           
         CLC   =C'PTP',QUESTOR+6   YES - POINT PERSON?                          
         BNE   SW560               NO                                           
SW550    EQU   *                                                                
         CLI   0(R3),X'31'         POINT PERSON                                 
         BE    POINTPER                                                         
         SPACE 1                                                                
SW560    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW570               NO                                           
         CLC   =C'EOP',QUESTOR+6   YES - EOP RECORD?                            
         BNE   SW580               NO                                           
SW570    EQU   *                                                                
         CLI   0(R3),X'1B'         EOP REC -- ADV                               
         BE    EOPADV                                                           
         CLI   0(R3),X'1C'         EOP REC -- AGY                               
         BE    EOPAGY                                                           
         CLI   0(R3),X'1D'         EOP REC -- OFF                               
         BE    EOPOFF                                                           
         CLI   0(R3),X'1E'         EOP REC -- SAL                               
         BE    EOPSAL                                                           
         SPACE 1                                                                
SW580    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW590               NO                                           
         CLC   =C'STR',QUESTOR+6   YES - STRATEGY REC?                          
         BNE   SW600               NO                                           
SW590    EQU   *                                                                
         CLI   0(R3),X'39'         STRATEGY REC                                 
         BE    STRATEGY                                                         
         SPACE 1                                                                
SW600    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW610               NO                                           
         CLC   =C'DIR',QUESTOR+6   YES - DIRECT RESPONSE REC?                   
         BNE   SW620               NO                                           
SW610    EQU   *                                                                
         CLI   0(R3),X'35'         DIRECT RESPONSE REC                          
         BE    DIRRESP                                                          
         SPACE 1                                                                
SW620    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW630               NO                                           
         CLC   =C'PRP',QUESTOR+6   YES - PROPOSAL?                              
         BNE   SW632               NO                                           
SW630    EQU   *                                                                
         CLC   0(2,R3),=X'4301'    PROPOSAL RECORD                              
         BE    PROPOSAL                                                         
         SPACE                                                                  
SW632    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW634               NO                                           
         CLC   =C'SLW',QUESTOR+6   YES - SELWIN?                                
         BNE   SW636               NO                                           
SW634    EQU   *                                                                
         CLC   0(2,R3),=X'4302'    SELLERS WORKSHEET FOR WINDOWS                
         BE    SELWIN                                                           
         SPACE                                                                  
SW636    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW638               NO                                           
         CLC   =C'BOX',QUESTOR+6   YES - SONNET BOXID RECORD?                   
         BNE   SW640               NO                                           
SW638    EQU   *                                                                
         CLI   0(R3),X'45'         SONNET BOXID RECORD                          
         BE    BOXID                                                            
         SPACE                                                                  
SW640    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW650               NO                                           
         CLC   =C'ACL',QUESTOR+6   YES - ACL?                                   
         BNE   SW660               NO                                           
SW650    EQU   *                                                                
         CLI   0(R3),X'20'         ACL RECORD                                   
         BE    ACL                                                              
         SPACE                                                                  
SW660    EQU   *                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW680               NO                                           
         CLC   =C'AUR',QUESTOR+6   YES - A.U.R.?                                
         BE    SW680               YES                                          
         CLC   =C'AUR',QUESTOR+9   YES - SOMETHING + A.U.R.?                    
         BNE   SW700               NO                                           
SW680    EQU   *                                                                
         CLI   0(R3),X'2C'         AUR                                          
         BE    AUR                                                              
SW700    EQU   *                                                                
         SPACE 1                                                                
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SW720               NO                                           
         CLC   =C'DRO',QUESTOR+6   YES - ROM/DARE EOP REC?                      
         BNE   SW740               NO                                           
SW720    EQU   *                                                                
         CLI   0(R3),X'4C'         ROM/DARE EOP REC?                            
         BE    DROADV              YES - PROCESS IT                             
         SPACE 1                                                                
SW740    EQU   *                                                                
SW1000   EQU   *                                                                
         B     KEEP                                                             
         EJECT                                                                  
*                                                                               
KEEP     L     R0,AREC                                                          
         SH    R0,=H'4'                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         L     R0,AREC                                                          
         SH    R0,=H'4'                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         B     SW200               CONTINUE                                     
         SPACE 2                                                                
PURGE    AP    PURGCNT,=P'1'       INCREMENT RECORD COUNT                       
         B     SW200               CONTINUE                                     
PURGCNT  DC    PL5'0'                                                           
RECCNT   DC    PL5'0'                                                           
         SPACE 2                                                                
SWEOF    DS    0H'0'                                                            
         BAS   RE,COUNT                                                         
         CLOSE FILEIN                                                           
         CLOSE FILOUTA                                                          
         CLOSE FILOUTB                                                          
         CLOSE RCVROUT                                                          
         B     SWXIT                                                            
         EJECT                                                                  
******************************************************************              
*              BUY RECORDS - GOES THROUGH ENTIRE TABLE           *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
BUY      DS    0H'0'                                                            
BUY00010 EQU   *                                                                
         L     R4,ABLDAREA                                                      
         LA    R6,RBUYELEM                                                      
         MVI   ELCODE,8            FIND RTS ELEMENT                             
         BAS   RE,NEXTEL                                                        
         BNE   KEEP                NO RTS ELEMENT - KEEP RECORD                 
*                                    NOTHING TO BE CHANGED                      
BUY00020 EQU   *                                                                
         LA    R5,BUYCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RBUYKREP,FREP       FILTER ON REP                                
         BNE   BUY00030                                                         
         CLI   BUYCHA,X'0B'                                                     
         BE    BUY00040                                                         
         SPACE 1                                                                
BUY00030 LA    R4,LENFIXT(R4)                                                   
         B     BUY00020                                                         
         SPACE 1                                                                
BUY00040 EQU   *                                                                
         CLC   FSTA,SPACES         STATION CHANGE IN REQUEST?                   
         BE    BUY00030            NO - CHECK NEXT REQUEST                      
*                                                                               
         USING RBUYSPEL,R6                                                      
         CLC   RBUYSPST,FSTA       STATION TO BE CHANGED?                       
         BNE   BUY00030            NO - CHECK NEXT REQUEST                      
         CLC   NSTA,SPACES         ANY NEW STATION?                             
         BE    BUY00030            NO - CHECK NEXT REQUEST                      
         BAS   RE,DMPGET           DUMP                                         
         MVC   RBUYSPST,NSTA       YES - INSERT NEW STATION                     
*                                                                               
BUY00050 LA    R5,BUYCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     BUY00030                                                         
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*              CONTRACT RECORDS - GOES THROUGH ENTIRE TABLE      *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
CONTRACT DS    0H'0'                                                            
CONT0020 EQU   *                                                                
         L     R4,ABLDAREA                                                      
CONT0040 EQU   *                                                                
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         MVI   STAMATCH,C'N'       SET STATION MATCH TO 'NO'                    
         CLC   RCONKREP,FREP       FILTER ON REP                                
         BNE   CONT0220                                                         
         CLC   FSTA,SPACES         ANY OLD STATION?                             
         BE    CONT0060            NO  -                                        
         GOTO1 =A(CONCOMBO),DMCB,(R3),RR=Y                                      
***                                YES - CHECK FOR COMBO CONTROL                
         CLC   RCONKSTA,FSTA       STATION                                      
         BNE   CONT0220                                                         
         MVI   STAMATCH,C'Y'       SET STATION MATCH TO 'YES'                   
CONT0060 EQU   *                                                                
         OC    STADATE,STADATE     START DATE FILTER?                           
         BZ    CONT0080            NO                                           
         CLC   RCONDATE(3),STADATE IS CON DATE BEFORE START DATE?               
         BNL   CONT0080            NO - ACCEPT IT                               
         TM    SWFLAGS,X'40'       START W/IN FLIGHT WANTED?                    
         BNO   CONT0220            NO  - SKIP ORDER                             
         CLC   RCONDATE+3(3),STADATE                                            
*                                  YES - START DATE W/IN FLIGHT?                
         BL    CONT0220            NO  - SKIP IT                                
CONT0080 EQU   *                                                                
         CLC   FOFF,SPACES                                                      
         BE    *+14                NO OFFICE                                    
         CLC   RCONKOFF,FOFF       OFFICE?                                      
         BNE   CONT0220            NO  -                                        
         CLC   FGRP,SPACES                                                      
         BE    CONT0100            NO GROUP                                     
         CLC   RCONKGRP,FGRP       GROUP?                                       
         BE    CONT0100            YES -                                        
         CLI   STAMATCH,C'Y'       NO  - STATION MATCH FOR CONTRACT?            
         BNE   CONT0220            NO  - GROUP DOESN'T MATCH                    
*                                  YES - FORCE-CORRECT GROUP MISMATCH           
CONT0100 EQU   *                                                                
         CLC   FSAL,SPACES         ANY SPERSON FILTER?                          
         BE    CONT0120            NO                                           
         CLC   RCONSAL,FSAL        YES - SAME SALESMAN?                         
         BNE   CONT0220            NO  - SKIP IT                                
         B     CONT0140            YES - DON'T CHECK THE TEAM                   
CONT0120 EQU   *                                                                
         CLC   FTEAM,SPACES                                                     
         BE    *+14                                                             
         CLC   RCONTEM,FTEAM       TEAM                                         
         BNE   CONT0220                                                         
CONT0140 EQU   *                                                                
*                                                                               
         CLC   FDSP,SPACES                                                      
         BE    CONT0160                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        DEVSAL ELEM                                  
         BAS   RE,GETEL                                                         
         BNE   CONT0220            ELEM NOT THERE                               
         USING RCONDVEL,R6                                                      
         CLC   RCONDVSP,FDSP       DEVELOPMENTAL SALESPERSON                    
         BNE   CONT0220                                                         
         DROP  R6                                                               
*                                                                               
CONT0160 CLC   FADV,SPACES                                                      
         BE    *+14                                                             
         CLC   RCONKADV,FADV       ADVERTISER                                   
         BNE   CONT0220                                                         
         CLC   FAGY,SPACES                                                      
         BE    CONT0200                                                         
         CLI   EXCLC,0             IF OLD CODE IS AGENCY                        
         BE    CONT0180                                                         
         ZIC   RE,EXCLC            MAY BE COMPARING FOR 4 (AGENCY)              
         EX    RE,*+8                    OR 6 (AGENCY/OFFICE)                   
         B     *+10                                                             
         CLC   RCONKAGY(0),FAGY    AGENCY                                       
         BNE   CONT0220                                                         
         B     CONT0200                                                         
CONT0180 EQU   *                                                                
         CLC   RCONKAGY,FAGY       IF FILTERING ON AGENCY                       
         BNE   CONT0220                                                         
         CLC   FAGYOF,SPACES       CHECK AGENCY AND OFFICE                      
         BE    CONT0200                                                         
         CLC   RCONKAOF,FAGYOF                                                  
         BNE   CONT0220                                                         
         SPACE 1                                                                
CONT0200 EQU   *                                                                
         LA    R5,CONCNT                                                        
         CLI   CONCHA,X'0C'                                                     
         BE    CONT0240                                                         
         SPACE 1                                                                
CONT0220 EQU   *                                                                
         LA    R4,LENFIXT(R4)                                                   
         B     CONT0040                                                         
         SPACE 1                                                                
CONT0240 EQU   *                                                                
         BAS   RE,DMPGET           DUMP                                         
         CLI   ACTION,C'P'                                                      
         BE    PURGE                                                            
         CLC   NADV,SPACES         IF ADVERTISER CHANGES,                       
         BNE   CONT0260                                                         
         CLC   NOFF,SPACES         OR OFFICE CHANGES                            
         BNE   CONT0260                                                         
         CLC   NAGY,SPACES         OR AGENCY CHANGES                            
         BNE   CONT0260                                                         
         CLI   FIRSTCON,C'Y'       OR IF 1ST CONTRACT, THEN                     
         BNE   CONT0280                                                         
CONT0260 EQU   *                                                                
*                                                                               
*   TEST                                                                        
***      MVC   P+1(07),=C'BLDRCVR'                                              
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         BAS   RE,BLDRCVR          BUILD RECOVERY REC FOR A.U.R. UPDATE         
CONT0280 EQU   *                                                                
         MVI   FIRSTCON,C'N'                                                    
*                                                                               
*   SAVE ORIGINAL S/P AND S/P OFFICE CODES FOR COMP S/P ROUTINE                 
*                                                                               
         MVC   DUB(3),RCONSAL      SAVE ORIGINAL S/P                            
         MVC   DUB+3(2),RCONKOFF   SAVE ORIGINAL OFFICE                         
         CLC   NSTA,SPACES                                                      
         BE    *+10                                                             
         MVC   RCONKSTA,NSTA                                                    
         CLC   NOFF,SPACES                                                      
         BE    *+10                                                             
         MVC   RCONKOFF,NOFF                                                    
         CLC   NGRP,SPACES                                                      
         BE    *+10                                                             
         MVC   RCONKGRP,NGRP                                                    
         CLC   NSAL,SPACES                                                      
         BE    CONT0290                                                         
         MVC   RCONSAL,NSAL                                                     
*                                                                               
*   TEST                                                                        
**       MVC   P+1(07),=C'CSP IN '                                              
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         GOTO1 =A(COMPSP),DMCB,(R4),RR=Y                                        
*                                  COMPENSATION CHANGES, IF ANY                 
*                                                                               
*   TEST                                                                        
**       MVC   P+1(07),=C'CSP OUT'                                              
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
CONT0290 EQU   *                                                                
         CLC   NTEAM,SPACES                                                     
         BE    CONT0300                                                         
         MVC   RCONTEM,NTEAM                                                    
         MVI   ELCODE,X'1E'        FLAG ELEMENT                                 
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   CONT0300                                                         
         USING RCONRFEL,R6                                                      
         NI    RCONRF1,X'FF'-X'80' TEAM OVERRIDE OFF                            
         DROP  R6                                                               
*                                                                               
CONT0300 EQU   *                                                                
         CLC   NDSP,SPACES                                                      
         BE    CONT0320                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        DEVSAL ELEM                                  
         BAS   RE,GETEL                                                         
         BNE   CONT0320            ELEM NOT THERE                               
         USING RCONDVEL,R6                                                      
         MVC   RCONDVSP,NDSP       DEVELOPMENTAL SALESPERSON                    
         DROP  R6                                                               
*                                                                               
CONT0320 CLC   NADV,SPACES                                                      
         BE    CONT0340                                                         
         MVC   RCONKADV,NADV                                                    
         TM    RCONMODR+1,X'10'    KATZ CONVERTED ORDER?                        
         BNO   CONT0340            NO                                           
         OI    RCONMODR+1,X'04'    YES - SET 'ADVERT SWITCHED' FLAG             
CONT0340 EQU   *                                                                
         SPACE 1                                                                
         CLC   NAGY,SPACES                                                      
         BE    CONT0380                                                         
         TM    RCONMODR+1,X'10'    KATZ CONVERTED ORDER?                        
         BNO   CONT0360            NO                                           
         OI    RCONMODR+1,X'08'    YES - SET 'AGENCY SWITCHED' FLAG             
CONT0360 EQU   *                                                                
*                                                                               
         MVC   RCONKAGY(6),SPACES  RE-INITIALIZE AGENCY+OFFICE FIELD            
*                                                                               
         CLI   EXMVC,0             MUST BE THERE IF THERE'S NAGY                
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,EXMVC            MAY BE MOVING FOR 4 (AGENCY)                 
         EX    RE,*+8                    OR 6 (AGENCY/OFFICE)                   
         B     *+10                                                             
         MVC   RCONKAGY(0),NAGY    AGENCY                                       
         SPACE 1                                                                
CONT0380 EQU   *                                                                
         LA    R6,RCONELEM                                                      
         CLC   FCSTA,SPACES                                                     
         BE    CONT0440                                                         
         MVI   ELCODE,6                                                         
         BAS   RE,NEXTEL                                                        
         BNE   CONT0440                                                         
         SPACE 1                                                                
         USING RCONSPEL,R6                                                      
         SR    R1,R1                                                            
         IC    R1,RCONSPNU         NUMBER OF MINI ELEMENTS                      
         LA    R5,RCONSPST                                                      
         SPACE 1                                                                
CONT0400 EQU   *                                                                
         CLC   0(5,R5),FCSTA       COMPETING STATION                            
         BNE   CONT0420                                                         
         CLC   NCSTA,SPACES                                                     
         BE    *+10                                                             
         MVC   0(5,R5),NCSTA    NEW COMPETING                                   
         B     CONT0440                                                         
         SPACE 1                                                                
CONT0420 EQU   *                                                                
         LA    R5,9(R5)                                                         
         BCT   R1,CONT0400                                                      
         DROP  R6                                                               
         SPACE 1                                                                
CONT0440 EQU   *                                                                
         LA    R5,CONCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     CONT0220            LOOP THROUGH ENTIRE TABLE                    
         EJECT                                                                  
******************************************************************              
*              STATION RECORDS - GOES THROUGH ENTIRE TABLE       *              
******************************************************************              
         SPACE 1                                                                
STATION  DS    0H'0'                                                            
STAT0010 L     R4,ABLDAREA                                                      
STAT0020 CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RSTAKREP,FREP       FILTER REP                                   
         BNE   STAT0040                                                         
         CLC   RSTAKSTA,FSTA       IS STATION FOUND?                            
         BE    STAT0030            YES -                                        
         CLC   NSTA,SPACES         NO  - IS THERE A NEW STATION?                
         BE    STAT0040            NO  - NO FURTHER CHECKING                    
         B     STAT0090            YES - CHECK FOR COMBO STATIONS               
         SPACE 1                                                                
*                                                                               
*  NOTE:  IF STATION IS TO BE CHANGED, NO CHECK FOR COMBO STATIONS              
*         IS MADE.  IT IS NOT LOGICAL THAT THE STATION WOULD CARRY              
*         ITSELF AS ITS OWN COMBO.  CHECKING WOULD BE A WASTE OF                
*         PROCESSING TIME.                                                      
*                                                                               
STAT0030 EQU   *                                                                
         LA    R5,STACNT                                                        
         CLI   STACHA,X'02'                                                     
         BE    STAT0050                                                         
         SPACE 1                                                                
STAT0040 LA    R4,LENFIXT(R4)                                                   
         B     STAT0020                                                         
         SPACE 1                                                                
STAT0050 BAS   RE,DMPGET                                                        
         CLI   DDSTAT,C'N'         DDSSTATION SWITCH RECORD?                    
         BNE   STAT0052            NO  - CONTINUE TO PROCESS                    
         MVC   P+1(33),=C'DDSSTATION SWITCH: RECORD DROPPED'                    
*                                  YES - ORIGINAL STN TO BE DROPPED.            
         GOTO1 REPORT                                                           
         GOTO1 REPORT              INSERT SPACE LINE                            
         B     PURGE                                                            
STAT0052 DS    0H                                                               
         CLI   ACTION,C'P'                                                      
         BE    PURGE                                                            
STAT0060 DS    0H                                                               
         CLC   NSTA,SPACES                                                      
         BE    STAT0062                                                         
         MVC   RSTAKSTA,NSTA                                                    
STAT0062 DS    0H                                                               
         CLC   NGRP,SPACES                                                      
         BE    *+10                                                             
         MVC   RSTAGRUP,NGRP                                                    
         SPACE 1                                                                
STAT0070 LA    R6,RSTAELEM         PROCESS COMPETITIVE STA ELTS                 
         CLC   FCSTA,SPACES                                                     
         BE    STAT0080                                                         
         MVI   ELCODE,2            SET COMPETITIVE ELEMENT                      
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   STAT0080            NOT FOUND                                    
         SPACE 1                                                                
         USING RSTAMKEL,R6                                                      
         CLC   RSTAMKST,FCSTA      COMPETITIVE = OLD STATION?                   
         BNE   *-14                NO  - GO BACK FOR NEXT ELEMENT               
         CLC   NCSTA,SPACES        YES - ANY NEW STATION?                       
         BE    *+10                NO                                           
         MVC   RSTAMKST,NCSTA      YES - MOVE IN NEW STATION                    
         CLC   NCAFF,SPACES        ANY AFFIL?                                   
         BE    *+10                NO  -                                        
         MVC   RSTAMKAF,NCAFF      YES - MOVE IN NEW AFFIL                      
         DROP  R6                                                               
         SPACE 1                                                                
STAT0080 LA    R5,STACNT                                                        
         BAS   RE,DMPPUT                                                        
         B     STAT0040            GO THROUGH ENTIRE TABLE                      
STAT0090 EQU   *                                                                
         LA    R5,STACNT                                                        
         CLI   STACHA,X'02'        IS IT A STATION CHANGE?                      
         BNE   STAT0040            NO  - DON'T DO ANYTHING                      
*                                  YES - NOT CHANGING MAIN STATION              
*                                         SCAN FOR X'0A' ELEMENTS               
         LA    R6,RSTAELEM                                                      
         MVI   ELCODE,X'0A'        RETRIEVE NEXT X'0A'                          
STAT0100 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BNE   STAT0110            NONE, OR FINISHED                            
*                                                                               
         USING RSTACSEL,R6                                                      
         CLC   RSTACS,FSTA         COMBO STATION = OLD STATION?                 
         BNE   STAT0100            NO  - LOOK FOR NEXT X'0A'                    
         BAS   RE,DMPGET                                                        
         MVC   RSTACS,NSTA         INSERT NEW STATION IN COMBO ELEMENT          
         BAS   RE,DMPPUT                                                        
STAT0110 EQU   *                                                                
         B     STAT0040            GO THROUGH ENTIRE TABLE                      
         EJECT                                                                  
******************************************************************              
*              INVENTORY RECORDS                                 *              
******************************************************************              
         SPACE 1                                                                
INVNTRY  DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
         SPACE 1                                                                
INV10    CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RINVKREP,FREP                                                    
         BNE   INV30                                                            
*                                                                               
         CLC   RINVKSTA(4),FSTA                                                 
         BNE   INV30                                                            
         CLI   RINVKSTA+4,C'1'     SATELLITE STATION?                           
         BE    INV12               YES - TREAT AS TV                            
         CLI   RINVKSTA+4,C'T'     TV STATION?                                  
         BNE   INV15               NO  - CHECK FOR MEDIA IN COMPARISON          
INV12    DS    0H                                                               
         CLI   FSTA+4,C' '         TV:  NO MEDIA IN FROM STATION?               
         BE    INV20               YES - LEAVE MEDIA AS IS                      
*                                     (MAY BE SATELLITE)                        
         B     INV30                                                            
*                                                                               
INV15    DS    0H                                                               
         CLC   RINVKSTA+4(1),FSTA+4                                             
         BNE   INV30                                                            
*                                                                               
INV20    DS    0H                                                               
         LA    R5,INVCNT                                                        
         CLI   INVCHA,RINVKTYQ                                                  
         BE    INV40                                                            
         SPACE 1                                                                
INV30    LA    R4,LENFIXT(R4)                                                   
         B     INV10                                                            
         SPACE                                                                  
INV40    BAS   RE,DMPGET                                                        
         CLI   ACTION,C'P'                                                      
         BE    PURGE                                                            
         CLC   NSTA,SPACES                                                      
         BE    INV90                                                            
         CLI   RINVKSTA+4,C'1'     SATELLITE STATION DATA?                      
         BNE   INV42               NO                                           
         MVC   RINVKSTA(4),NSTA    YES - DON'T OVERLAY THE MEDIA = 1 !!         
         B     INV44                                                            
INV42    EQU   *                                                                
         MVC   RINVKSTA,NSTA                                                    
INV44    EQU   *                                                                
         CLI   RINVKSTA+4,C' '                                                  
         BNE   INV90                                                            
         MVI   RINVKSTA+4,C'T'                                                  
         SPACE 1                                                                
INV90    LA    R5,INVCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     KEEP                                                             
         EJECT                                                                  
******************************************************************              
*              BUDGET RECORDS                                    *              
******************************************************************              
         SPACE 1                                                                
* NOTE - BUDGET RECORD ALSO CAN HAVE TEAM OR OFFICE IN THE KEY                  
* THIS PROGRAM DOES NOT CHANGE THE TEAM OR OFFICE IN THE BUDGET                 
*  RECORD      -------                                                          
         SPACE 2                                                                
BUDGET   DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
BUD1     CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RBUDKREP,FREP                                                    
         BNE   BUD2                                                             
         CLC   RBUDKSTA,FSTA                                                    
         BNE   BUD2                                                             
         SPACE 1                                                                
         LA    R5,BUDCNT                                                        
         CLI   BUDCHA,X'13'                                                     
         BE    BUD4                                                             
         SPACE 1                                                                
BUD2     LA    R4,LENFIXT(R4)                                                   
         B     BUD1                                                             
         SPACE 1                                                                
BUD4     BAS   RE,DMPGET                                                        
         CLI   ACTION,C'P'                                                      
         BE    PURGE                                                            
         CLC   NSTA,SPACES                                                      
         BE    *+10                                                             
         MVC   RBUDKSTA,NSTA                                                    
         SPACE 1                                                                
BUD9     LA    R5,BUDCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     KEEP                                                             
         EJECT                                                                  
******************************************************************              
*      DARE & DARE HEADER RECORDS - GOES THROUGH ENTIRE TABLE    *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
DARE     DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
*                                                                               
DARE0020 EQU   *                                                                
         CLI   0(R3),X'41'         DARE RECORD?                                 
         BNE   *+12                NO                                           
         LA    R5,DARCNT           YES                                          
         B     *+8                                                              
         LA    R5,DUMCNT           DARE HEADER, USE DUMMY COUNTER               
*                                                                               
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RDARKREP,FREP       FILTER ON REP                                
         BNE   DARE0030                                                         
         CLI   DARCHA,X'41'                                                     
         BE    DARE0040                                                         
         SPACE 1                                                                
DARE0030 LA    R4,LENFIXT(R4)                                                   
         B     DARE0020                                                         
         SPACE 1                                                                
DARE0040 EQU   *                                                                
         CLC   FSTA,SPACES         STATION CHANGE IN REQUEST?                   
         BE    DARE0050            NO - CHECK S/P-P/P CHANGE REQUEST            
*                                                                               
*                                  YES - PROCESS STATION CHANGE REQUEST         
*                                                                               
         CLI   RDARKSTA+4,C'T'     IS THIS STATION TV?                          
         BNE   *+14                NO, COMPARE FOR 5 PLACES                     
         CLC   FSTA(4),RDARKSTA    YES, COMPARE FOR 4 PLACES (NO BAND)          
         B     *+10                GO TO CONDITIONAL BRANCH                     
*                                                                               
         CLC   FSTA,RDARKSTA       STATION TO BE CHANGED (5 PLACES)?            
*                                                                               
         BNE   DARE0030            NO - CHECK NEXT REQUEST                      
         CLC   NSTA,SPACES         ANY NEW STATION?                             
         BE    DARE0030            NO - CHECK NEXT REQUEST                      
         BAS   RE,DMPGET           DUMP                                         
*                                                                               
         MVI   RDARKSTA+5,C' '     PUT BLANK IN 6TH CHAR                        
         MVC   RDARKSTA(5),NSTA    YES - INSERT NEW STATION                     
         CLI   RDARKSTA+4,C' '     IS THIS A TV STATION?                        
         BNE   *+8                 NO                                           
         MVI   RDARKSTA+4,C'T'     YES, MOVE IN A 'T'                           
*                                                                               
         CLI   0(R3),X'41'         DARE RECORD?                                 
         BNE   *+12                NO                                           
         LA    R5,DARCNT           YES                                          
         B     *+8                                                              
         LA    R5,DUMCNT           DARE HEADER, USE DUMMY COUNTER               
*                                                                               
         BAS   RE,DMPPUT                                                        
         B     DARE0030            CHECK NEXT REQUEST                           
DARE0050 EQU   *                                                                
                                                                                
         CLC   FSAL,SPACES         ANY SPERSON FILTER?                          
         BE    DARE0060            NO                                           
*                                                                               
*   TEST                                                                        
***      MVC   P+1(10),=C'DARE1: S/P'                                           
***      MVC   P+20(27),RDARREC                                                 
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         LA    R6,RDARREC                                                       
         MVI   ELCODE,X'0F'        MISC. FLAGS ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DARE0100            ELEM NOT THERE: CAN'T BE SWITCHED            
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'01'      UNWIRED ORDER?                               
         BO    DARE0060            YES - DON'T CHANGE: NOT S/P CODE             
*                                  NO  - SALESPERSON: CHECK FOR MATCH           
         DROP  R6                                                               
*                                                                               
         LA    R6,RDARREC                                                       
         MVI   ELCODE,X'0A'        S/P-P/P ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DARE0100            ELEM NOT THERE: CAN'T BE SWITCHED            
         USING RDARPPEL,R6                                                      
         CLC   RDARPPSP,FSAL       YES - SAME SALESPERSON?                      
         BNE   DARE0060            NO  - NO CHANGE                              
         CLC   NSAL,SPACES         NEW SALESPERSON?                             
         BE    DARE0060            NO                                           
         BAS   RE,DMPGET           DUMP                                         
         MVC   RDARPPSP,NSAL       YES - INSERT INTO ELEMENT                    
         BAS   RE,DMPPUT           DUMP                                         
         DROP  R6                                                               
DARE0060 EQU   *                                                                
*                                                                               
         CLC   FPTP,SPACES         ANY POINTPERSON FILTER?                      
         BE    DARE0070            NO                                           
*                                                                               
*   TEST                                                                        
***      MVC   P+1(10),=C'DARE2: P/P'                                           
***      MVC   P+20(27),RDARREC                                                 
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         LA    R6,RDARREC                                                       
         MVI   ELCODE,X'0F'        MISC. FLAGS ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DARE0100            ELEM NOT THERE: CAN'T BE SWITCHED            
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'01'      UNWIRED ORDER?                               
         BNO   DARE0070            NO  - DON'T CHANGE: NOT P/P CODE             
*                                  YES - POINTPERSON: CHECK FOR MATCH           
         DROP  R6                                                               
*                                                                               
         LA    R6,RDARREC                                                       
         MVI   ELCODE,X'0A'        S/P-P/P ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DARE0100            ELEM NOT THERE: CAN'T BE SWITCHED            
         USING RDARPPEL,R6                                                      
         CLC   RDARPPSP,FPTP       YES - SAME POINTPERSON?                      
         BNE   DARE0070            NO  - NO CHANGE                              
         CLC   NPTP,SPACES         NEW POINTPERSON?                             
         BE    DARE0070            NO                                           
         BAS   RE,DMPGET           DUMP                                         
         MVC   RDARPPSP,NPTP       YES - INSERT INTO ELEMENT                    
         BAS   RE,DMPPUT           DUMP                                         
         DROP  R6                                                               
DARE0070 EQU   *                                                                
*                                                                               
DARE0100 EQU   *                                                                
         B     DARE0030            GO BACK FOR NEXT TABLE ENTRY                 
*                                                                               
         EJECT                                                                  
******************************************************************              
*          MAKEGOOD RECORDS - GOES THROUGH ENTIRE TABLE          *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
MAKEGOOD DS    0H'0'                                                            
         GOTO1 =A(MKGNMOD1),(RC),RR=Y                                           
         B     KEEP                                                             
         EJECT                                                                  
******************************************************************              
*     TKO EQUIVALENT RECORDS - GOES THROUGH ENTIRE TABLE         *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
TAKEOVER DS    0H'0'                                                            
         GOTO1 =A(TKONMOD1),(RC),RR=Y                                           
         B     KEEP                                                             
         EJECT                                                                  
******************************************************************              
*          STRATEGY RECORDS - GOES THROUGH ENTIRE TABLE          *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
STRATEGY DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
*                                                                               
STRA0020 EQU   *                                                                
         LA    R5,STRCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         MVI   STAMATCH,C'N'       SET STATION MATCH TO 'NO'                    
         CLC   RSTRKREP,FREP       FILTER ON REP                                
         BNE   STRA0030                                                         
         CLI   STRCHA,X'39'        STRATEGY CHANGE?                             
         BE    STRA0040                                                         
         SPACE 1                                                                
STRA0030 LA    R4,LENFIXT(R4)                                                   
         B     STRA0020                                                         
         SPACE 1                                                                
STRA0040 EQU   *                                                                
         CLC   FSTA,SPACES         STATION CHANGE IN REQUEST?                   
         BE    STRA0050            NO                                           
         CLC   RSTRKSTA,FSTA       STATION TO BE CHANGED?                       
         BNE   STRA0030            NO - CHECK NEXT REQUEST                      
         MVI   STAMATCH,C'Y'       SET STATION MATCH TO 'YES'                   
*                                                                               
STRA0050 EQU   *                                                                
         CLC   FGRP,SPACES                                                      
         BE    STRA0060            NO GROUP                                     
         CLC   RSTRKGRP,FGRP       GROUP?                                       
         BE    STRA0060            YES -                                        
         CLI   STAMATCH,C'Y'       NO  - STATION MATCH FOR STRATEGY?            
         BNE   STRA0030            NO  - GROUP DOESN'T MATCH                    
*                                  YES - FORCE-CORRECT GROUP MISMATCH           
STRA0060 EQU   *                                                                
         BAS   RE,DMPGET           DUMP                                         
         CLC   NSTA,SPACES                                                      
         BE    *+10                                                             
         MVC   RSTRKSTA,NSTA                                                    
         CLC   NGRP,SPACES                                                      
         BE    *+10                                                             
         MVC   RSTRKGRP,NGRP                                                    
*                                                                               
STRA0100 LA    R5,STRCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     STRA0030                                                         
*                                                                               
         EJECT                                                                  
******************************************************************              
*        DIRECT RESPONSE RECORDS - GOES THROUGH ENTIRE TABLE     *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
DIRRESP  DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
*                                                                               
DIRR0020 EQU   *                                                                
         LA    R5,DIRCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RDIRREP,FREP        FILTER ON REP                                
         BNE   DIRR0030                                                         
         CLI   DIRCHA,X'35'        DIRECT RESPONSE CHANGE?                      
         BE    DIRR0040                                                         
         SPACE 1                                                                
DIRR0030 LA    R4,LENFIXT(R4)                                                   
         B     DIRR0020                                                         
         SPACE 1                                                                
DIRR0040 EQU   *                                                                
         CLC   FSTA,SPACES         STATION CHANGE IN REQUEST?                   
         BE    DIRR0030            NO - CHECK NEXT REQUEST                      
*                                                                               
         CLC   RDIRSTA,FSTA        STATION TO BE CHANGED?                       
         BNE   DIRR0030            NO - CHECK NEXT REQUEST                      
         CLC   NSTA,SPACES         ANY NEW STATION?                             
         BE    DIRR0030            NO - CHECK NEXT REQUEST                      
         BAS   RE,DMPGET           DUMP                                         
         MVC   RDIRSTA,NSTA        YES - INSERT NEW STATION                     
*                                                                               
DIRR0050 LA    R5,DIRCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     DIRR0030                                                         
*                                                                               
         EJECT                                                                  
******************************************************************              
*              EOP ADV RECORDS - GOES THROUGH ENTIRE TABLE       *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
EOPADV   DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
*                                                                               
EOAD0020 EQU   *                                                                
         LA    R5,EOPCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   REOPKREP,FREP       FILTER ON REP                                
         BNE   EOAD0030                                                         
         CLI   EOPCHA,X'1B'        EOP CHANGE?                                  
         BE    EOAD0040                                                         
         SPACE 1                                                                
EOAD0030 LA    R4,LENFIXT(R4)                                                   
         B     EOAD0020                                                         
         SPACE 1                                                                
EOAD0040 EQU   *                                                                
         CLC   FSTA,SPACES         STATION CHANGE?                              
         BE    EOAD0045            NO, CHECK FOR ADV CHANGE                     
*                                                                               
         CLC   REOPKSTA,FSTA       YES, CORRECT STATION?                        
         BNE   EOAD0030            NO, GO TO NEXT                               
         CLC   NSTA,SPACES         NEW STATION?                                 
         BE    EOAD0030            NO, GO TO NEXT                               
         BAS   RE,DMPGET                                                        
         MVC   REOPKSTA,NSTA       INSERT NEW STATION                           
         B     EOAD0050                                                         
*                                                                               
EOAD0045 CLI   ADVCHA,X'08'        EOP ADV CHANGE? (ONLY IF NO FILTERS)         
         BNE   EOAD0030                                                         
         CLC   FADV,SPACES         ADV CHANGE?                                  
         BE    EOAD0030            NO - CHECK NEXT REQUEST                      
*                                                                               
         CLC   REOPKCOD,FADV       ADV TO BE CHANGED?                           
         BNE   EOAD0030            NO - CHECK NEXT REQUEST                      
         CLC   NEWCODE(4),SPACES   ANY NEW ADV?                                 
         BE    EOAD0030            NO - CHECK NEXT REQUEST                      
         BAS   RE,DMPGET           DUMP                                         
         CLI   ACTION,C'P'         PURGE ON ADV CHANGE                          
         BE    PURGE                                                            
         B     EOAD0030            SHOULDN'T HAPPEN                             
*                                                                               
EOAD0050 LA    R5,EOPCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     EOAD0030                                                         
*                                                                               
         EJECT                                                                  
******************************************************************              
*              EOP AGY RECORDS - GOES THROUGH ENTIRE TABLE       *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
EOPAGY   DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
*                                                                               
EOAG0020 EQU   *                                                                
         LA    R5,EOPCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   REO2KREP,FREP       FILTER ON REP                                
         BNE   EOAG0030                                                         
         CLI   EOPCHA,X'1B'        EOP CHANGE?                                  
         BE    EOAG0040                                                         
         SPACE 1                                                                
EOAG0030 LA    R4,LENFIXT(R4)                                                   
         B     EOAG0020                                                         
         SPACE 1                                                                
EOAG0040 EQU   *                                                                
         CLC   FSTA,SPACES         STATION CHANGE?                              
         BE    EOAG0045            NO, CHECK FOR AGY CHANGE                     
*                                                                               
         CLC   REO2KSTA,FSTA       YES, CORRECT STATION?                        
         BNE   EOAG0030            NO, GO TO NEXT                               
         CLC   NSTA,SPACES         NEW STATION?                                 
         BE    EOAG0030            NO, GO TO NEXT                               
         BAS   RE,DMPGET                                                        
         MVC   REO2KSTA,NSTA       INSERT NEW STATION                           
         B     EOAG0050                                                         
*                                                                               
EOAG0045 CLI   AGYCHA,X'0A'        EOP AGY CHANGE? (ONLY IF NO FILTERS)         
         BNE   EOAG0030                                                         
*                                                                               
         CLC   FAGY(4),NEWCODE     TRANSFER WITHIN SAME AGENCY?                 
         BNE   *+14                NO  - PROCEED                                
         CLC   FAGY+4(2),SPACES    OLD AGENCY WITH OFFICE?                      
         BE    EOAG0030            NO  - TRANSFERRING CORPORATE                 
*                                     TO LOCAL - KEEP CORPORATE                 
         CLC   FAGY(6),SPACES      AGY CHANGE?                                  
         BE    EOAG0030            NO - CHECK NEXT REQUEST                      
*                                                                               
         CLC   REO2KCOD,FAGY       AGY TO BE CHANGED?                           
         BNE   EOAG0030            NO - CHECK NEXT REQUEST                      
         CLC   NEWCODE(6),SPACES   ANY NEW AGY?                                 
         BE    EOAG0030            NO - CHECK NEXT REQUEST                      
         BAS   RE,DMPGET           DUMP                                         
         CLI   ACTION,C'P'         PURGE ON AGY CHANGE                          
         BE    PURGE                                                            
         B     EOAG0030            SHOULDN'T HAPPEN                             
*                                                                               
EOAG0050 LA    R5,EOPCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     EOAG0030                                                         
*                                                                               
         EJECT                                                                  
******************************************************************              
*              EOP SAL RECORDS - GOES THROUGH ENTIRE TABLE       *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
EOPSAL   DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
*                                                                               
EOSP0020 EQU   *                                                                
         LA    R5,EOPCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   REO4KREP,FREP       FILTER ON REP                                
         BNE   EOSP0030                                                         
         CLI   EOPCHA,X'1B'        EOP CHANGE?                                  
         BE    EOSP0040                                                         
         SPACE 1                                                                
EOSP0030 LA    R4,LENFIXT(R4)                                                   
         B     EOSP0020                                                         
         SPACE 1                                                                
EOSP0040 EQU   *                                                                
         CLC   FSTA,SPACES         STATION CHANGE?                              
         BE    EOSP0045            NO, CHECK FOR SAL CHANGE                     
*                                                                               
         CLC   REO4KSTA,FSTA       YES, CORRECT STATION?                        
         BNE   EOSP0030            NO, GO TO NEXT                               
         CLC   NSTA,SPACES         NEW STATION?                                 
         BE    EOSP0030            NO, GO TO NEXT                               
         BAS   RE,DMPGET                                                        
         MVC   REO4KSTA,NSTA       INSERT NEW STATION                           
         B     EOSP0050                                                         
*                                                                               
EOSP0045 CLI   SALCHA,X'06'        EOP SAL CHANGE? (ONLY IF NO FILTERS)         
         BNE   EOSP0030                                                         
         CLC   FSAL,SPACES         SAL CHANGE?                                  
         BE    EOSP0030            NO - CHECK NEXT REQUEST                      
*                                                                               
         CLC   REO4KCOD,FSAL       SAL TO BE CHANGED?                           
         BNE   EOSP0030            NO - CHECK NEXT REQUEST                      
         CLC   NEWCODE(3),SPACES   ANY NEW SAL?                                 
         BE    EOSP0030            NO - CHECK NEXT REQUEST                      
         BAS   RE,DMPGET           DUMP                                         
         CLI   ACTION,C'P'         PURGE ON SAL CHANGE                          
         BE    PURGE                                                            
         B     EOSP0030            SHOULDN'T HAPPEN                             
*                                                                               
EOSP0050 LA    R5,EOPCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     EOSP0030                                                         
*                                                                               
         EJECT                                                                  
******************************************************************              
*              EOP OFF RECORDS - GOES THROUGH ENTIRE TABLE       *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
EOPOFF   DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
*                                                                               
EOOF0020 EQU   *                                                                
         LA    R5,EOPCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   REO3KREP,FREP       FILTER ON REP                                
         BNE   EOOF0030                                                         
         CLI   EOPCHA,X'1B'        EOP CHANGE?                                  
         BE    EOOF0040                                                         
         SPACE 1                                                                
EOOF0030 LA    R4,LENFIXT(R4)                                                   
         B     EOOF0020                                                         
         SPACE 1                                                                
EOOF0040 EQU   *                                                                
         CLC   FSTA,SPACES         STATION CHANGE?                              
         BE    EOOF0030            NO, GO TO NEXT                               
*                                                                               
         CLC   REO3KSTA,FSTA       YES, CORRECT STATION?                        
         BNE   EOOF0030            NO, GO TO NEXT                               
         CLC   NSTA,SPACES         NEW STATION?                                 
         BE    EOOF0030            NO, GO TO NEXT                               
         BAS   RE,DMPGET                                                        
         MVC   REO3KSTA,NSTA       INSERT NEW STATION                           
*                                                                               
         LA    R5,EOPCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     EOOF0030                                                         
*                                                                               
         EJECT                                                                  
******************************************************************              
*             SET RECORDS - GOES THROUGH ENTIRE TABLE            *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
SET      DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
*                                                                               
SET00020 EQU   *                                                                
         LA    R5,SETCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RSETKREP,FREP       FILTER ON REP                                
         BNE   SET00030                                                         
         CLI   SETCHA,X'38'                                                     
         BE    SET00040                                                         
         SPACE 1                                                                
SET00030 LA    R4,LENFIXT(R4)                                                   
         B     SET00020                                                         
         SPACE 1                                                                
SET00040 EQU   *                                                                
*                                                                               
         CLC   RSETKSET,=C'MS'     MARKET STATIONS RECORD                       
         BE    *+14                CHECK 'STACHA'                               
*                                                                               
         CLC   RSETKSET,=C'ST'     STATION SET RECORD?                          
         BNE   SET00050                                                         
         CLI   STACHA,X'02'        STATION SET CHANGE?                          
         BNE   SET00030            NO, GO TO NEXT TABLE ENTRY                   
         MVC   FILTCODE(L'FSTA),FSTA                                            
         MVC   NEWCODE(L'NSTA),NSTA   SINCE DIDN'T DO IN BLDTBL ROUTINE         
         B     SET00170                                                         
*                                                                               
SET00050 CLC   RSETKSET,=C'AD'     ADVERTISER SET RECORD?                       
         BNE   SET00060                                                         
         CLI   ADVCHA,X'08'        ADVERTISER SET CHANGE?                       
         BNE   SET00030            NO, GO TO NEXT TABLE ENTRY                   
         MVC   FILTCODE(L'FADV),FADV                                            
         B     SET00170                                                         
*                                                                               
SET00060 CLC   RSETKSET,=C'SP'     SALESPERSON SET RECORD?                      
         BNE   SET00070                                                         
         CLI   SALCHA,X'06'        SALESPERSON SET CHANGE?                      
         BNE   SET00030            NO, GO TO NEXT TABLE ENTRY                   
         MVC   FILTCODE(L'FSAL),FSAL                                            
         B     SET00170                                                         
*                                                                               
SET00070 CLC   RSETKSET,=C'AG'     AGENCY SET RECORD?                           
         BNE   SET00160                                                         
         CLI   AGYCHA,X'0A'        AGENCY SET CHANGE?                           
         BNE   SET00030            NO, GO TO NEXT TABLE ENTRY                   
         CLC   FAGY(4),NEWCODE     TRANSFER WITHIN SAME AGENCY?                 
         BNE   *+14                NO  - PROCEED                                
         CLC   FAGY+4(2),SPACES    OLD AGENCY WITH OFFICE?                      
         BE    SET00030            NO  - TRANSFERRING CORPORATE                 
*                                     TO LOCAL - KEEP CORPORATE                 
         MVC   FILTCODE(6),FAGY                                                 
         B     SET00170                                                         
*                                                                               
SET00160 CLC   RSETKSET,=C'PP'     POINT PERSON SET RECORD?                     
         BNE   KEEP                                                             
         CLI   PTPCHA,X'31'        POINT PERSON SET CHANGE?                     
         BNE   SET00030            NO, GO TO NEXT TABLE ENTRY                   
         MVC   FILTCODE(L'FPTP),FPTP                                            
SET00170 DS    0H                                                               
*                                                                               
         CLC   FILTCODE,SPACES     CODE CHANGE IN REQUEST?                      
         BE    SET00030            NO - CHECK NEXT REQUEST                      
         CLC   NEWCODE,SPACES      ANY NEW CODE?                                
         BE    SET00030            NO - CHECK NEXT REQUEST                      
*                                                                               
         LA    R6,RSETREC                                                       
         MVI   ELCODE,X'20'        SET MEMBERS ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   KEEP                DON'T BOTHER CHECKING ANY MORE               
*                                                                               
         USING RSETMEMD,R6                                                      
         ZIC   R0,RSETMLEN         LENGTH OF MEMBERS                            
         ZIC   R1,RSETMELN         ELEMENT LENGTH                               
         DROP  R6                                                               
*                                                                               
         LA    R8,RSETMTOV         ELEMENT OVERHEAD                             
         AR    R6,R8                                                            
         SR    R1,R8                                                            
*                                                                               
         LTR   R0,R0               LENGTH OF MEMBERS                            
         BZ    KEEP                IF 0, SOMETHING WRONG                        
         MVI   CHIND,C'N'          NOTHING HAS BEEN CHANGED YET                 
         MVI   DUPIND,C'N'         MEMBER HASN'T OCCURRED YET                   
SET00180 LTR   R1,R1               END OF ELEMENT?                              
         BZ    SET00230            YES                                          
*                                                                               
         LR    R8,R0                                                            
         BCTR  R8,0                FOR EXECUTED COMMANDS                        
*                                                                               
         EX    R8,*+8              COMPARE FOR LENGTH OF MEMBER                 
         B     *+10                                                             
         CLC   0(0,R6),NEWCODE     IS THIS THE CODE BEING CHANGED TO?           
         BNE   *+20                NO, GO ON                                    
         CLI   CHIND,C'Y'          YES, DOES THIS CODE ALREADY EXIST?           
         BE    SET00200            YES, DELETE THIS CODE FROM SET               
         MVI   DUPIND,C'Y'         NO, SET A FLAG THAT IT'S OCCURRED            
         B     SET00190            AND GO TO NEXT SET MEMBER                    
*                                                                               
         EX    R8,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),FILTCODE    IS THIS THE CODE BEING CHANGED FROM?         
         BNE   SET00190            NO, GO TO NEXT SET MEMBER                    
         CLI   CHIND,C'N'          YES, ANYTHING BEEN CHANGED YET?              
         BNE   *+12                YES                                          
         MVI   CHIND,C'Y'          SOMETHING WILL NOW BE CHANGED                
         BAS   RE,DMPGET                                                        
         CLI   DUPIND,C'Y'         HAS THE CODE OCCURRED ALREADY?               
         BE    SET00200            YES, DELETE THIS ONE                         
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),NEWCODE     NO, INSERT NEW CODE                          
*                                                                               
SET00190 AR    R6,R0               GO ON IN ELEMENT                             
         SR    R1,R0                                                            
         B     SET00180                                                         
*                                                                               
*** DELETE THE DUPLICATE SET MEMBER (BY COMPRESSING THE ELEMENT)                
*                                                                               
SET00200 LR    R2,R6               SAVE THE POSITION IN ELEMENT                 
         LA    R6,RSETREC          GO BACK TO BEGINNING OF ELEMENT              
         MVI   ELCODE,X'20'        SET MEMBERS ELEMENT                          
         BAS   RE,GETEL                                                         
         SR    R2,R6               DISPLACEMENT FROM BEGINNING                  
*                                                                               
         USING RSETMEMD,R6                                                      
         ZIC   R8,RSETMELN         ELEMENT LENGTH                               
         DROP  R6                                                               
         LTR   R8,R8               IS IT 0?                                     
         BZ    KEEP                YES (SHOULD NEVER HAPPEN)                    
*                                                                               
         XC    ELEM,ELEM                                                        
         BCTR  R8,0                FOR EXECUTED COMMAND                         
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)       MOVE ENTIRE ELEMENT TO 'ELEM'                
*                                                                               
         LA    R6,ELEM(R2)         POINT R6 IN ELEM WHERE IT WAS BEFORE         
         LR    R8,R1               AMOUNT LEFT IN ELEMENT                       
         SR    R8,R0               SUBTRACT OFF CURRENT MEMBER                  
         LTR   R8,R8               ARE WE AT END OF ELEMENT?                    
         BZ    SET00220            YES, NO NEED TO COMPRESS                     
*                                                                               
         LR    R2,R6               CURRENT POSITION IN ELEM                     
         AR    R2,R0               MOVE PAST CURRENT MEMBER                     
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R2)       COMPRESS OVER DUPLICATE MEMBER               
*                                                                               
SET00220 LA    R6,ELEM                                                          
         USING RSETMEMD,R6                                                      
         ZIC   R1,RSETMELN         ELEMENT LENGTH                               
         SR    R1,R0               CORRECT TO THE NEW ELEMENT LENGTH            
         STC   R1,RSETMELN         PUT IT BACK                                  
         DROP  R6                                                               
*                                                                               
** DELETE OLD ELEM AND ADD NEW ONE                                              
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'20',RSETREC),0,0            
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSETREC,ELEM,         +        
               =C'ADD=CODE'                                                     
*                                                                               
SET00230 CLI   CHIND,C'Y'          HAS ANYTHING BEEN CHANGED?                   
         BNE   SET00030            GO ON IN TABLE                               
         LA    R5,SETCNT           YES, DUMP IT OUT                             
         BAS   RE,DMPPUT                                                        
         B     SET00030            GO ON IN TABLE                               
*                                                                               
         EJECT                                                                  
******************************************************************              
*          PROPOSAL RECORDS - GOES THROUGH ENTIRE TABLE          *              
******************************************************************              
         SPACE 1                                                                
         USING RPROHDRD,R3                                                      
         USING FIXD,R4                                                          
         SPACE 1                                                                
PROPOSAL DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
*                                                                               
PROP0020 EQU   *                                                                
         LA    R5,PRPCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         MVI   STAMATCH,C'N'       SET STATION MATCH TO 'NO'                    
         CLC   RPROKRCD,FREP       FILTER ON REP                                
         BNE   PROP0030                                                         
         CLI   PRPCHA,X'43'                                                     
         BE    PROP0040                                                         
         SPACE 1                                                                
PROP0030 LA    R4,LENFIXT(R4)                                                   
         B     PROP0020                                                         
         SPACE 1                                                                
PROP0040 EQU   *                                                                
         NI    FLAGS,X'FF'-X'80'   NO X'02' ELEM ENCOUNTERED YET                
         LA    R6,RPROHDRD                                                      
         MVI   ELCODE,RPRSWELQ     X'02' SWITCH ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   PROP0260            NO X'02' ELEMS, CHECK FOR X'30'S             
         USING RPRSWELD,R6                                                      
         OI    FLAGS,X'80'         X'02' ELEM ENCOUNTERED                       
*                                                                               
         CLC   FSTA,SPACES         ANY OLD STATION?                             
         BE    PROP0070            NO  -                                        
         CLC   RPRSWSTA,FSTA       STATION                                      
         BNE   *+12                                                             
         MVI   STAMATCH,C'Y'       SET PRIMARY STATION MATCH TO 'YES'           
         B     PROP0070                                                         
*                                                                               
         CLC   NSTA,SPACES                                                      
         BNE   PROP0260                                                         
         B     PROP0030                                                         
*                                                                               
PROP0070 EQU   *                                                                
*                                                                               
         OC    STADATE,STADATE     START DATE FILTER?                           
         BZ    PROP0080            NO                                           
         CLC   RPRSWFLT(3),STADATE IS CON DATE BEFORE START DATE?               
***>>>DATE WITHIN CHECK                                                         
         BNL   PROP0080            NO - ACCEPT IT                               
         TM    SWFLAGS,X'40'       START W/IN FLIGHT WANTED?                    
         BNO   PROP0030            NO  - SKIP ORDER                             
         CLC   RPRSWFLT+3(3),STADATE                                            
*                                  YES - START DATE W/IN FLIGHT?                
         BL    PROP0030            NO  - SKIP IT                                
PROP0080 EQU   *                                                                
***>>>DATE WITHIN CHECK                                                         
*                                                                               
         CLC   FOFF,SPACES                                                      
         BE    *+14                NO OFFICE                                    
         CLC   RPRSWOFF,FOFF       OFFICE?                                      
         BNE   PROP0030            NO  -                                        
*                                                                               
         CLC   FGRP,SPACES         GROUP?                                       
         BE    PROP0090            NO GROUP                                     
         CLC   RPRSWGRP,FGRP       GROUP MATCH?                                 
         BE    PROP0090            YES                                          
         CLI   STAMATCH,C'Y'       NO, STATION MATCH?                           
         BNE   PROP0030            NO, GROUP DOESN'T MATCH                      
*                                  YES, FORCE GROUP TO MATCH ANYWAY             
PROP0090 EQU   *                                                                
         CLC   FSAL,SPACES         ANY SPERSON FILTER?                          
         BE    PROP0093            NO                                           
         CLC   RPRSWSAL,FSAL       YES - SAME SALESPERSON?                      
         BNE   PROP0030            NO  - SKIP IT                                
         B     PROP0096            YES - DON'T CHECK THE TEAM                   
PROP0093 EQU   *                                                                
         CLC   FTEAM,SPACES                                                     
         BE    *+14                                                             
         CLC   RPRSWTEM,FTEAM      TEAM                                         
         BNE   PROP0030                                                         
PROP0096 EQU   *                                                                
*                                                                               
         CLC   FDSP,SPACES                                                      
         BE    *+14                                                             
         CLC   RPRSWDSP,FDSP       DEVELOPMENTAL SALESPERSON                    
         BNE   PROP0030                                                         
*                                                                               
         CLC   FADV,SPACES                                                      
         BE    *+14                                                             
         CLC   RPRSWADV,FADV       ADVERTISER                                   
         BNE   PROP0030                                                         
         CLC   FAGY,SPACES                                                      
         BE    PROP0130                                                         
         CLI   EXCLC,0             IF OLD CODE IS AGENCY                        
         BE    PROP0120                                                         
         ZIC   RE,EXCLC            MAY BE COMPARING FOR 4 (AGENCY)              
         EX    RE,*+8                    OR 6 (AGENCY/OFFICE)                   
         B     *+10                                                             
         CLC   RPRSWAGY(0),FAGY    AGENCY                                       
         BNE   PROP0030                                                         
         B     PROP0130                                                         
PROP0120 EQU   *                                                                
         CLC   RPRSWAGY,FAGY       IF FILTERING ON AGENCY                       
         BNE   PROP0030                                                         
         CLC   FAGYOF,SPACES       CHECK AGENCY AND OFFICE                      
         BE    PROP0130                                                         
         CLC   RPRSWAOF,FAGYOF                                                  
         BNE   PROP0030                                                         
         SPACE 1                                                                
*                                                                               
PROP0130 EQU   *                                                                
         BAS   RE,DMPGET           DUMP                                         
*                                                                               
         CLC   NSTA,SPACES         STATION?                                     
         BE    PROP0180                                                         
         MVC   RPRSWSTA,NSTA                                                    
*                                                                               
         LA    R6,RPROHDRD                                                      
         MVI   ELCODE,RPRSTELQ     X'30' STATION ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   PROP0170            DON'T BOTHER CHECKING ANY MORE               
         USING RPRSTELD,R6                                                      
         CLC   RPRSTSTA,FSTA       CORRECT STATION?                             
         BNE   PROP0170            NO, GET NEXT ELEMENT ????                    
         MVC   RPRSTSTA,NSTA       INSERT NEW CODE                              
         DROP  R6                                                               
PROP0170 LA    R6,RPROHDRD                                                      
         MVI   ELCODE,RPRSWELQ     X'02' SWITCH ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   KEEP                DON'T BOTHER CHECKING ANY MORE               
         USING RPRSWELD,R6                                                      
PROP0180 EQU   *                                                                
*                                                                               
         CLC   NOFF,SPACES                                                      
         BE    *+10                                                             
         MVC   RPRSWOFF,NOFF                                                    
*                                                                               
         CLC   NGRP,SPACES                                                      
         BE    *+10                                                             
         MVC   RPRSWGRP,NGRP                                                    
*                                                                               
         CLC   NTEAM,SPACES                                                     
         BE    *+10                                                             
         MVC   RPRSWTEM,NTEAM                                                   
*                                                                               
         CLC   NDSP,SPACES                                                      
         BE    *+10                                                             
         MVC   RPRSWDSP,NDSP       DEVELOPMENTAL SALESPERSON                    
*                                                                               
         CLC   NADV,SPACES                                                      
         BE    *+10                                                             
         MVC   RPRSWADV,NADV                                                    
*                                                                               
         SPACE 1                                                                
         CLC   NAGY,SPACES                                                      
         BE    PROP0220                                                         
*                                                                               
         MVC   RPRSWAGY(6),SPACES  RE-INITIALIZE AGENCY+OFFICE FIELD            
*                                                                               
         CLI   EXMVC,0             MUST BE THERE IF THERE'S NAGY                
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,EXMVC            MAY BE MOVING FOR 4 (AGENCY)                 
         EX    RE,*+8                    OR 6 (AGENCY/OFFICE)                   
         B     *+10                                                             
         MVC   RPRSWAGY(0),NAGY    AGENCY                                       
         SPACE 1                                                                
PROP0220 EQU   *                                                                
*                                                                               
         CLC   NSAL,SPACES         SALESPERSON?                                 
         BE    PROP0240                                                         
         MVC   RPRSWSAL,NSAL                                                    
         DROP  R6                                                               
PROP0240 EQU   *                                                                
*                                                                               
         LA    R5,PRPCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     PROP0030                                                         
*                                                                               
* ROUTINE TO PROCESS STATION CHANGE FOR SECONDARY X'30' ELEMENTS                
PROP0260 EQU   *                                                                
         CLC   FSTA,SPACES         STATION CHANGE IN REQUEST?                   
         BE    PROP0030            NO - CHECK NEXT REQUEST                      
         CLC   NSTA,SPACES         ANY NEW STATION?                             
         BE    PROP0030            NO - CHECK NEXT REQUEST                      
*                                                                               
         LA    R6,RPROHDRD                                                      
         MVI   ELCODE,RPRSTELQ     X'30' STATION ELEMENT?                       
         BAS   RE,GETEL                                                         
         BE    *+16                YES                                          
*                                                                               
         TM    FLAGS,X'80'         NO, WAS THERE A X'02' ELEM?                  
         BNO   KEEP                NO, FINISHED WITH THIS RECORD                
         B     PROP0030            YES, CHECK NEXT REQUEST                      
*                                                                               
         USING RPRSTELD,R6                                                      
         MVI   CHIND,C'N'          NOTHING CHANGED YET                          
PROP0280 CLC   RPRSTSTA,FSTA       CORRECT STATION?                             
         BNE   PROP0290            NO, GET NEXT ELEMENT                         
         CLI   CHIND,C'N'          ANYTHING BEEN CHANGED YET?                   
         BNE   *+12                YES                                          
         MVI   CHIND,C'Y'          SOMETHING BEING CHANGED NOW                  
         BAS   RE,DMPGET                                                        
         MVC   RPRSTSTA,NSTA       INSERT NEW CODE                              
PROP0290 BAS   RE,NEXTEL           NEXT ELEM                                    
         BE    PROP0280                                                         
         DROP  R6                                                               
*                                                                               
         CLI   CHIND,C'Y'          WAS ANYTHING CHANGED?                        
         BNE   PROP0030            NO                                           
         LA    R5,PRPCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     PROP0030                                                         
*                                                                               
         EJECT                                                                  
******************************************************************              
*          SELLERS WORKSHET FOR WINDOWS, STATION SWITCH ONLY     *              
******************************************************************              
         SPACE 1                                                                
         USING RSLWREC,R3                                                       
         USING FIXD,R4                                                          
         SPACE 1                                                                
SELWIN   DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
*                                                                               
SELWIN20 EQU   *                                                                
         LA    R5,SLWCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         MVI   STAMATCH,C'N'       SET STATION MATCH TO 'NO'                    
         CLC   RSLWKREP,FREP       FILTER ON REP                                
         BNE   SELWIN30                                                         
         CLI   SLWCHA,X'43'                                                     
         BE    SELWIN40                                                         
         SPACE 1                                                                
SELWIN30 LA    R4,LENFIXT(R4)                                                   
         B     SELWIN20                                                         
         SPACE 1                                                                
SELWIN40 EQU   *                                                                
*                                                                               
         CLC   FSTA,SPACES         STATION CHANGE IN REQUEST?                   
         BE    SELWIN30            NO - CHECK NEXT REQUEST                      
         CLC   NSTA,SPACES         ANY NEW STATION?                             
         BE    SELWIN30            NO - CHECK NEXT REQUEST                      
*                                                                               
         LA    R6,RSLWREC                                                       
         MVI   ELCODE,RSWCSELQ     X'30' STATION ELEMENT?                       
         BAS   RE,GETEL                                                         
         BNE   KEEP                NO, FINISHED WITH THIS RECORD                
*                                                                               
         USING RSWCSELD,R6                                                      
         MVI   CHIND,C'N'          NOTHING CHANGED YET                          
SELWIN80 CLC   RSWCSSTA,FSTA       CORRECT STATION?                             
         BNE   SELWIN90            NO, GET NEXT ELEMENT                         
         CLI   CHIND,C'N'          ANYTHING BEEN CHANGED YET?                   
         BNE   *+12                YES                                          
         MVI   CHIND,C'Y'          SOMETHING BEING CHANGED NOW                  
         BAS   RE,DMPGET                                                        
         MVC   RSWCSSTA,NSTA       INSERT NEW CODE                              
SELWIN90 BAS   RE,NEXTEL           NEXT ELEM                                    
         BE    SELWIN80                                                         
         DROP  R6                                                               
*                                                                               
         CLI   CHIND,C'Y'          WAS ANYTHING CHANGED?                        
         BNE   SELWIN30            YES                                          
         LA    R5,SLWCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     SELWIN30                                                         
*                                                                               
         EJECT                                                                  
**********************************************************************          
*              ATHENA RECORDS - GOES THROUGH ENTIRE TABLE            *          
*                                                                    *          
*   THIS PROGRAM SWITCHES STATION AND GROUP IN ATHENA RECORDS        *          
*                                                                    *          
*   ***IF CONTRACTS ALREADY EXIST FOR THE NEW STATION, THERE IS A*** *          
*   ***GOOD POSSIBILITY OF CREATING DUPLICATE RECORD DURING THE  *** *          
*   ***SWITCH.  THEN THE ATHENA RECORDS MUST BE PURGED AND       *** *          
*   ***RECREATED.                                                *** *          
*                                                                    *          
*  IF THE ADVERTISER CHANGES, THAT IS HANDLED THROUGH A 2 STEP       *          
*   PROCESS.  FIRST, A RECOVERY TAPE IS BUILT WHILE READING THE      *          
*   CONTRACT RECORDS.  THEN, THAT TAPE IS USED WITH THE ATHENA       *          
*   UPDATE PROGRAM TO SUBTRACT OUT THE OLD AND CREATE THE NEW.       *          
**********************************************************************          
         SPACE 2                                                                
         USING RATNRECD,R3                                                      
ATHENA   DS    0H'0'                                                            
ATN0     L     R4,ABLDAREA                                                      
ATN1     CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RATNKREP,FREP                                                    
         BNE   ATN2                                                             
         SPACE 1                                                                
         CLC   FGRP,SPACES                                                      
         BE    *+14                                                             
         CLC   RATNKGRP,FGRP                                                    
         BNE   ATN2                                                             
         CLC   FSTA,SPACES                                                      
         BE    *+14                                                             
         CLC   RATNKSTA,FSTA                                                    
         BNE   ATN2                                                             
         SPACE 1                                                                
         LA    R5,ATNCNT                                                        
         CLI   ATNCHA,X'27'                                                     
         BE    ATN4                                                             
         SPACE 1                                                                
ATN2     LA    R4,LENFIXT(R4)                                                   
         B     ATN1                                                             
         SPACE 1                                                                
ATN4     BAS   RE,DMPGET                                                        
         CLI   ACTION,C'P'                                                      
         BE    PURGE                                                            
         CLC   NGRP,SPACES                                                      
         BE    *+10                                                             
         MVC   RATNKGRP,NGRP                                                    
         CLC   NSTA,SPACES                                                      
         BE    *+10                                                             
         MVC   RATNKSTA,NSTA                                                    
         LA    R5,ATNCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     ATN2                LOOP THROUGH ENTIRE TABLE                    
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
*       AVERAGE UNIT RATE RECS - GOES THROUGH ENTIRE TABLE           *          
*                                                                    *          
*   THIS PROGRAM SWITCHES STATION AND GROUP IN A.U.R. RECORDS        *          
*                                                                    *          
*   ***IF CONTRACTS ALREADY EXIST FOR THE NEW STATION, THERE IS A*** *          
*   ***GOOD POSSIBILITY OF CREATING DUPLICATE RECORD DURING THE  *** *          
*   ***SWITCH.  THEN THE A.U.R. RECORDS MUST BE PURGED AND       *** *          
*   ***RECREATED.                                                *** *          
*                                                                    *          
*  IF AGENCY OR OFFICE CHANGE THAT IS HANDLED THROUGH A 2 STEP       *          
*   PROCESS.  FIRST, A RECOVERY TAPE IS BUILT WHILE READING THE      *          
*   CONTRACT RECORDS.  THEN, THAT TAPE IS USED WITH THE A.U.R.       *          
*   UPDATE PROGRAM TO SUBTRACT OUT THE OLD AND CREATE THE NEW.       *          
**********************************************************************          
         SPACE 2                                                                
         USING RAURRECD,R3                                                      
AUR      DS    0H'0'                                                            
AUR0     L     R4,ABLDAREA                                                      
AUR1     CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RAURKREP,FREP                                                    
         BNE   AUR2                                                             
         SPACE 1                                                                
         CLC   FGRP,SPACES                                                      
         BE    *+14                                                             
         CLC   RAURKGRP,FGRP                                                    
         BNE   AUR2                                                             
         CLC   FSTA,SPACES                                                      
         BE    *+14                                                             
         CLC   RAURKSTA,FSTA                                                    
         BNE   AUR2                                                             
         SPACE 1                                                                
         LA    R5,AURCNT                                                        
         CLI   AURCHA,X'2C'                                                     
         BE    AUR4                                                             
         SPACE 1                                                                
AUR2     LA    R4,LENFIXT(R4)                                                   
         B     AUR1                                                             
         SPACE 1                                                                
AUR4     BAS   RE,DMPGET                                                        
         CLI   ACTION,C'P'                                                      
         BE    PURGE                                                            
         CLC   NGRP,SPACES                                                      
         BE    *+10                                                             
         MVC   RAURKGRP,NGRP                                                    
         CLC   NSTA,SPACES                                                      
         BE    *+10                                                             
         MVC   RAURKSTA,NSTA                                                    
         LA    R5,AURCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     AUR2                LOOP THROUGH ENTIRE TABLE                    
         DROP  R3                                                               
         EJECT                                                                  
******************************************************************              
*              SDD RECORDS                                       *              
******************************************************************              
         SPACE 1                                                                
         USING RSDDRECD,R3                                                      
SDD      DS    0H'0'                                                            
SDD0     L     R4,ABLDAREA                                                      
SDD1     CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RSDDKREP,FREP                                                    
         BNE   SDD2                                                             
         CLC   FSTA,SPACES                                                      
         BE    SDD2                                                             
         CLC   RSDDKSTA,FSTA                                                    
         BNE   SDD2                                                             
         SPACE 1                                                                
         LA    R5,SDDCNT                                                        
         CLI   SDDCHA,X'26'                                                     
         BE    SDD4                                                             
         SPACE 1                                                                
SDD2     LA    R4,LENFIXT(R4)                                                   
         B     SDD1                                                             
         SPACE 1                                                                
SDD4     BAS   RE,DMPGET                                                        
         CLI   ACTION,C'P'                                                      
         BE    PURGE                                                            
         CLC   NSTA,SPACES                                                      
         BE    SDD9                                                             
         MVC   RSDDKSTA,NSTA                                                    
         SPACE 1                                                                
SDD9     LA    R5,SDDCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     KEEP                                                             
         DROP  R3                                                               
         EJECT                                                                  
******************************************************************              
*              SALESPERSON RECORDS                               *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
SALPRSON DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
SAL1     CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    KEEP                YES - KEEP THIS RECORD                       
         CLC   RSALKREP,FREP       NO  - RECD REP = TABLE REP?                  
         BNE   SAL2                NO                                           
         CLC   FSAL,SPACES         YES - ANY OLD S/P CODE?                      
         BE    SAL2                NO                                           
         CLC   RSALKSAL,FSAL       YES - RECD S/P = OLD S/P?                    
         BNE   SAL2                NO                                           
         SPACE 1                                                                
         LA    R5,SALCNT           YES - SET A(S/P CHANGE CTR)                  
         CLI   SALCHA,X'06'        CHANGE THIS S/P?                             
         BE    SAL4                YES                                          
         SPACE 1                                                                
SAL2     LA    R4,LENFIXT(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     SAL1                GO BACK FOR NEXT                             
         SPACE 1                                                                
SAL4     BAS   RE,DMPGET           DISPLAY 'GET' SIDE OF PROCESS                
         CLI   ACTION,C'P'         PURGE THIS RECORD?                           
****>>>  BE    PURGE               YES                                          
         BNE   SAL6                NO  -                                        
         CLC   =C'K3',RSALKREP     KATZ RADIO?                                  
         BE    SAL5                YES                                          
         CLC   =C'B3',RSALKREP     EJOR (REP TEST)?                             
         BNE   PURGE               NO  - REGULAR PROCESSING                     
SAL5     EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,0),(3,RSALLEAV)                                   
*                                  INSERT TODAY'S DATE AS LEAVE DATE            
         B     SAL9                KEEP S/P AS INACTIVE                         
SAL6     EQU   *                                                                
         CLC   NTEAM,SPACES        NO  - TEAM CHANGE ON S/P?                    
         BE    *+14                NO                                           
         MVC   RSALTEAM,NTEAM      YES - MOVE NEW TEAM TO RECORD                
         SPACE 1                                                                
SAL9     LA    R5,SALCNT           SET A(S/P CHANGE CTR)                        
         BAS   RE,DMPPUT           DISPLAY 'PUT' SIDE OF PROCESS                
         B     KEEP                KEEP RECORD                                  
         EJECT                                                                  
******************************************************************              
*              DEVELOPMENTAL SALESPERSON RECORDS - PURGE ONLY    *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
DEVSALPR DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
DSP1     CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RDSPKREP,FREP                                                    
         BNE   DSP2                                                             
         CLC   FDSP,SPACES                                                      
         BE    DSP2                                                             
         CLC   RDSPKSAL,FDSP                                                    
         BNE   DSP2                                                             
         SPACE 1                                                                
         LA    R5,DSPCNT                                                        
         CLI   DSPCHA,X'3A'                                                     
         BE    DSP4                                                             
         SPACE 1                                                                
DSP2     LA    R4,LENFIXT(R4)                                                   
         B     DSP1                                                             
         SPACE 1                                                                
DSP4     BAS   RE,DMPGET                                                        
         CLI   ACTION,C'P'                                                      
         BE    PURGE                                                            
         DC    H'0'                                                             
         EJECT                                                                  
******************************************************************              
*              POINT PERSON RECORDS - PURGE ONLY                 *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
POINTPER DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
PTP1     CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RPTPKREP,FREP                                                    
         BNE   PTP2                                                             
         CLC   FPTP,SPACES                                                      
         BE    PTP2                                                             
         CLC   RPTPKREC,FPTP                                                    
         BNE   PTP2                                                             
         SPACE 1                                                                
         LA    R5,PTPCNT                                                        
         CLI   PTPCHA,X'31'        POINT PERSON RECORD?                         
         BE    PTP4                YES                                          
*                                  NO  - BUMP TO NEXT SLOT                      
         SPACE 1                                                                
PTP2     LA    R4,LENFIXT(R4)                                                   
         B     PTP1                                                             
         SPACE 1                                                                
PTP4     BAS   RE,DMPGET                                                        
         CLI   ACTION,C'P'                                                      
         BE    PURGE                                                            
         DC    H'0'                                                             
         EJECT                                                                  
******************************************************************              
*              ADVERTISER RECORDS - PURGE ONLY                   *              
******************************************************************              
         SPACE 1                                                                
ADVERTSR DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
ADV1     CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RADVKREP,FREP                                                    
         BNE   ADV2                                                             
         CLC   FADV,SPACES                                                      
         BE    ADV2                                                             
         CLC   RADVKADV,FADV                                                    
         BNE   ADV2                                                             
         SPACE 1                                                                
         LA    R5,ADVCNT                                                        
         CLI   ADVCHA,X'08'                                                     
         BE    ADV4                                                             
         SPACE 1                                                                
ADV2     LA    R4,LENFIXT(R4)                                                   
         B     ADV1                                                             
         SPACE 1                                                                
ADV4     BAS   RE,DMPGET                                                        
         CLI   ACTION,C'P'                                                      
         BE    PURGE                                                            
         DC    H'0'                                                             
         EJECT                                                                  
******************************************************************              
*              PRODUCT RECORDS                                   *              
******************************************************************              
         SPACE 1                                                                
PRODUCT  DS    0H'0'                                                            
*                                                                               
*   TEST                                                                        
***      MVC   P+1(05),=C'PREC='                                                
***      MVC   P+6(64),RPRDREC                                                  
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         L     R4,ABLDAREA                                                      
*                                                                               
*   TEST                                                                        
***      MVC   P+1(09),=C'PROD100-:'                                            
***      BAS   RE,PTEST                                                         
*   TEST END                                                                    
*                                                                               
PRD100   EQU   *                                                                
*                                                                               
*   TEST                                                                        
**       MVC   P+1(09),=C'LOOP100-:'                                            
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
*                                                                               
         CLC   RPRDKREP,FREP                                                    
         BNE   PRD200                                                           
*                                                                               
         CLC   FPTP,SPACES         IS THIS A PP SWITCH?                         
         BE    PRD150              NO, COULD BE PURGING DUE TO ADV SW           
*                                  YES, PP SWITCH                               
         CLC   FADV,SPACES         IS THERE AN ADV FILTER?                      
         BE    *+14                NO                                           
         CLC   RPRDKADV,FADV                                                    
         BNE   PRD200                                                           
*                                                                               
         LA    R6,RPRDREC                                                       
         MVI   ELCODE,X'02'        NETWORK CONTRACT ELEM                        
         BAS   RE,GETEL                                                         
         BNE   PRD200              ELEM NOT THERE                               
         USING RPRDNELM,R6                                                      
         CLC   RPRDNPNT,FPTP       POINT PERSON                                 
         BNE   PRD200                                                           
         B     PRD170                                                           
         DROP  R6                                                               
*                                                                               
PRD150   DS    0H                                                               
         CLC   FADV,SPACES         ADVERTISER FILTER?                           
         BE    PRD160              NO  - CHECK AGENCY SWITCH                    
         CLC   RPRDKADV,FADV       YES - SAME ADVERTISER?                       
****     BE    PRD170                                                           
         BNE   PRD200              NO  - SKIP THIS SWITCH REQUEST               
         SPACE 1                                                                
*                                                                               
PRD160   DS    0H                                                               
         MVC   WORK,FIXCOD                                                      
         CLC   FAGY,SPACES         AGENCY FILTER PRESENT?                       
         BE    PRD200              NO, CHECK NEXT                               
*                                                                               
         LA    R6,RPRDREC                                                       
         MVI   ELCODE,X'04'        AGENCY ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   PRD200              ELEM NOT THERE                               
         USING RPRDAGFL,R6                                                      
*                                                                               
PRD165   DS    0H                                                               
         CLC   RPRDAGAG,FAGY       DOES REC HAVE ORIGINAL AGY?                  
         BNE   PRD200              NO, SKIP                                     
         CLC   FAGYOF,SPACES       IS AGYOF BLANK?                              
         BE    PRD170              YES, CONTINUE                                
         CLC   RPRDAGAO,FAGYOF     NO, DOES REC HAVE ORIGINAL AGYOF?            
         BNE   PRD200              NO, SKIP                                     
         DROP  R6                                                               
         SPACE 1                                                                
PRD170   DS    0H                                                               
         LA    R5,PRDCNT                                                        
         CLI   PRDCHA,X'09'                                                     
         BE    PRD400                                                           
         SPACE 1                                                                
PRD200   LA    R4,LENFIXT(R4)                                                   
*                                                                               
*   TEST                                                                        
***      MVC   P+1(09),=C'PROD200-:'                                            
***      BAS   RE,PTEST                                                         
*   TEST END                                                                    
*                                                                               
         B     PRD100                                                           
         SPACE 1                                                                
PTEST    NTR1                                                                   
         MVC   P+10(50),0(R4)                                                   
         MVC   P+61(10),=C'AG  AD  PR'                                          
         MVC   P+63(1),AGYCHA                                                   
         MVC   P+67(1),ADVCHA                                                   
         MVC   P+71(1),PRDCHA                                                   
         GOTO1 REPORT                                                           
         XIT1                                                                   
PRD400   EQU   *                                                                
*                                                                               
*   TEST                                                                        
***      MVC   P+1(09),=C'PROD400-:'                                            
***      BAS   RE,PTEST                                                         
*   TEST END                                                                    
*                                                                               
         BAS   RE,DMPGET                                                        
         CLI   ACTION,C'P'                                                      
         BE    PURGE                                                            
*                                                                               
         CLC   NPTP,SPACES         NEW POINT PERSON?                            
         BE    PRD450              NO, CHECK FOR AGENCY SWITCH                  
         LA    R6,RPRDREC                                                       
         MVI   ELCODE,X'02'        NETWORK CONTRACT ELEM                        
         BAS   RE,GETEL                                                         
         BNE   PRD500              ELEM NOT THERE                               
         USING RPRDNELM,R6                                                      
         MVC   RPRDNPNT,NPTP       POINT PERSON                                 
         DROP  R6                                                               
*                                                                               
PRD450   DS    0H                                                               
         CLC   NAGY,SPACES         AGENCY SWITCH?                               
         BE    PRD500              NO, GET OUT                                  
         LA    R6,RPRDREC                                                       
         MVI   ELCODE,X'04'        AGENCY ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   PRD500              ELEM NOT THERE                               
         USING RPRDAGFL,R6                                                      
*                                                                               
         MVC   RPRDAGAG(6),SPACES  RE-INITIALIZE AGENCY+OFFICE FIELD            
*                                                                               
         CLI   EXMVC,0             MUST BE THERE IF THERE'S NAGY                
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,EXMVC            MAY BE MOVING FOR 4 (AGENCY)                 
         EX    RE,*+8                    OR 6 (AGENCY/OFFICE)                   
         B     *+10                                                             
         MVC   RPRDAGAG(0),NAGY    AGENCY                                       
         DROP  R6                                                               
*                                                                               
PRD500   LA    R5,PRDCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     PRD200              LOOP THROUGH ENTIRE TABLE                    
         EJECT                                                                  
******************************************************************              
*              AGENCY & AGY II RECORDS - PURGE ONLY              *              
******************************************************************              
         SPACE 1                                                                
AGENCY   DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
AGY1     CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RAGYKREP,FREP                                                    
         BNE   AGY2                                                             
         CLC   FAGY(6),SPACES                                                   
         BE    AGY2                                                             
         CLC   RAGYKAGY(6),FAGY                                                 
         BNE   AGY2                                                             
         SPACE 1                                                                
         CLI   0(R3),X'0A'         AGENCY RECORD?                               
         BNE   *+12                NO                                           
         LA    R5,AGYCNT           YES                                          
         B     *+8                                                              
*                                                                               
         LA    R5,DUMCNT           AGENCY II, USE DUMMY COUNTER                 
*                                                                               
         CLI   AGYCHA,X'0A'                                                     
         BE    AGY4                                                             
         SPACE 1                                                                
AGY2     LA    R4,LENFIXT(R4)                                                   
         B     AGY1                                                             
         SPACE 1                                                                
AGY4     BAS   RE,DMPGET                                                        
         CLC   FAGY(4),NAGY        TRANSFER WITHIN SAME AGENCY?                 
         BNE   AGY5                NO  - PROCEED                                
         CLC   FAGY+4(2),SPACES    OLD AGENCY WITH OFFICE?                      
         BE    AGY6                NO  - TRANSFERRING CORPORATE                 
*                                     TO LOCAL - KEEP CORPORATE                 
AGY5     EQU   *                                                                
         CLI   ACTION,C'P'         ACTION PURGE?                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'0A'         AGENCY                                       
         BE    PURGE                                                            
         CLI   0(R3),X'1A'         IF AGENCY II,                                
         BE    SW200               PURGE BUT DON'T KEEP A COUNT OF IT           
         DC    H'0'                SHOULDN'T HAPPEN                             
*                                                                               
AGY6     EQU   *                                                                
         CLI   0(R3),X'0A'         AGENCY RECORD?                               
         BNE   *+12                NO                                           
         LA    R5,AGYCNT           YES                                          
         B     *+8                                                              
*                                                                               
         LA    R5,DUMCNT           AGENCY II, USE DUMMY COUNTER                 
*                                                                               
         BAS   RE,DMPPUT           DISPLAY THE RECORD                           
         B     KEEP                KEEP THE RECORD                              
         EJECT                                                                  
******************************************************************              
*              ACL RECORDS - ALTERNATE CALENDAR RECORDS                         
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
ACL      DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
*                                                                               
ACL0020  EQU   *                                                                
         LA    R5,ACLCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RACLKREP,FREP       FILTER ON REP                                
         BNE   ACL0030                                                          
         CLI   ACLCHA,X'20'        ACL CHANGE?                                  
         BE    ACL0040                                                          
         SPACE 1                                                                
ACL0030  LA    R4,LENFIXT(R4)                                                   
         B     ACL0020                                                          
         SPACE 1                                                                
ACL0040  EQU   *                                                                
         CLC   FSTA,SPACES         STATION CHANGE?                              
         BE    ACL0030             NO                                           
         CLC   RACLKNAM,FSTA       YES, CORRECT STATION?                        
         BNE   ACL0030             NO, GO TO NEXT                               
         CLC   NSTA,SPACES         NEW STATION?                                 
         BE    ACL0030             NO, GO TO NEXT                               
         BAS   RE,DMPGET                                                        
         MVC   RACLKNAM,NSTA       INSERT NEW STATION                           
         B     ACL0050                                                          
*                                                                               
ACL0050  LA    R5,ACLCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     ACL0030                                                          
*                                                                               
         EJECT                                                                  
******************************************************************              
*              SONNET BOXID RECORD                                              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
BOXID    DS    0H'0'                                                            
         L     R4,ABLDAREA                                                      
*                                                                               
BOXID20  EQU   *                                                                
         LA    R5,BOXCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    KEEP                                                             
         CLC   RBOXKREP,FREP       FILTER ON REP                                
         BNE   BOXID30                                                          
         CLI   BOXCHA,X'45'        BOXID CHANGE?                                
         BE    BOXID40                                                          
         SPACE 1                                                                
BOXID30  LA    R4,LENFIXT(R4)                                                   
         B     BOXID20                                                          
         SPACE 1                                                                
BOXID40  EQU   *                                                                
         CLC   FSTA,SPACES         STATION CHANGE?                              
         BE    BOXID30             NO                                           
         CLC   RBOXKSTA,FSTA       YES, CORRECT STATION?                        
         BNE   BOXID30             NO, GO TO NEXT                               
         CLC   NSTA,SPACES         NEW STATION?                                 
         BE    BOXID30             NO, GO TO NEXT                               
         BAS   RE,DMPGET                                                        
         MVC   RBOXKSTA,NSTA       INSERT NEW STATION                           
         B     BOXID50                                                          
*                                                                               
BOXID50  LA    R5,BOXCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     BOXID30                                                          
*                                                                               
         EJECT                                                                  
******************************************************************              
*             SWITCH RECORDS - SELF PURGE AFTER 2 WEEKS          *              
******************************************************************              
         SPACE 1                                                                
SWITCH   DS    0H'0'                                                            
         USING RSWIKEY,R3                                                       
         CLC   RSWIKYMD,PRGYMD                                                  
         BH    KEEP                                                             
         LA    R5,SWICNT                                                        
         BAS   RE,DMPGET                                                        
         B     PURGE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* COMMISSION RECORDS - STATION AND ADVERTISER CHANGE.                 *         
***********************************************************************         
         USING RECD,R3                                                          
COMMREC  DS    0H                                                               
         L     R4,ABLDAREA          SWITCH RECORD TABLE                         
*                                                                               
COM10    CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    KEEP                                                             
*                                                                               
*- SEE IF THIS SWITCH RECORD APPLIES TO THIS FILE RECORD.                       
         CLI   COMCHA,X'29'        NEED TO CHANGE COM REC?                      
         BNE   COM99               NO.                                          
*                                                                               
*DEBUG    MVC   P(6),=C'COMREC'                                                 
*         MVC   P+10(2),RCOMKREP                                                
*         MVC   P+14(2),FREP                                                    
*         MVC   P+20(5),RCOMKSTA                                                
*         MVC   P+26(5),FSTA                                                    
*         MVC   P+35(4),RCOMKADV                                                
*         MVC   P+40(4),FADV                                                    
*         GOTO1 REPORT                                                          
*                                                                               
         CLC   RCOMKREP,FREP       REP CODE MATCH                               
         BNE   COM99               NEXT TABLE ENTRY                             
*                                                                               
         CLC   FSTA,SPACES         ANY STATION CHANGE?                          
         BE    COM20                                                            
         CLC   RCOMKSTA,FSTA                                                    
         BE    COM50               CHANGE THIS RECORD                           
*                                                                               
COM20    CLC   FADV,SPACES         ADV CHANGE?                                  
         BE    COM30                                                            
         CLC   RCOMKADV,FADV       ONLY CHANGE IF ADV HAS NO FILTS              
         BE    COM50               CHANGE THIS RECORD                           
*                                                                               
COM30    EQU   *                   FUTURE FILTERS GO HERE                       
         B     COM99               TRY NEXT TABLE ENTRY                         
*                                                                               
*- RECORD ACCEPTED FOR CHANGE.                                                  
COM50    LA    R5,COMCNT                                                        
         BAS   RE,DMPGET           DUMP RECORD TO PRINTER                       
*                                                                               
*        CLI   ACTION,C'P'         NEVER PURGE COMMISSION RECS                  
*        BE    PURGE                                                            
*                                                                               
*- REPLACE STATION AND/OR ADVERTISER WITH NEW VALUES.                           
         CLC   NSTA,SPACES         NEW STATION?                                 
         BE    COM60                                                            
         MVC   RCOMKSTA,NSTA                                                    
*                                                                               
COM60    CLC   NEWCODE(4),SPACES   NEW ADV?                                     
         BE    COM70                                                            
         MVC   RCOMKADV,NEWCODE                                                 
*                                                                               
COM70    LA    R5,COMCNT                                                        
         BAS   RE,DMPPUT           LOG TO PRINTER                               
         B     KEEP                                                             
*                                                                               
COM99    LA    R4,LENFIXT(R4)      NEXT SWITCH REC IN TABLE                     
         B     COM10                                                            
         EJECT                                                                  
***>>>ROM EOP ADV RECORDS                                                       
******************************************************************              
*          ROM EOP ADV RECORDS - GOES THROUGH ENTIRE TABLE       *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
         SPACE 1                                                                
DROADV   DS    0H'0'                                                            
         GOTO1 =A(DROAMOD1),(RC),RR=Y                                           
         B     KEEP                                                             
         EJECT                                                                  
***>>    COUNT                                                                  
******************************************************************              
*              END OF FILE - COUNT WHAT'S BEEN DONE              *              
******************************************************************              
         SPACE 1                                                                
         USING RECD,R3                                                          
COUNT    NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         SPACE 1                                                                
         L     R4,ABLDAREA                                                      
CNT10    CLI   0(R4),X'FF'                                                      
         BE    CNT20                                                            
         MVC   P+1(1),ACTION                                                    
         MVC   P+4(2),FREP                                                      
         MVC   P+8(5),FSTA                                                      
         MVC   P+14(5),FCSTA                                                    
         MVC   P+20(2),FOFF                                                     
         MVC   P+23(3),FSAL                                                     
         MVC   P+27(2),FTEAM                                                    
         MVC   P+30(2),FGRP                                                     
         MVC   P+33(6),FAGY                                                     
         MVC   P+40(4),FADV                                                     
         MVC   P+45(3),FDSP                                                     
         MVC   P+49(3),FPTP                                                     
         SPACE 1                                                                
         MVC   PSECOND+8(5),NSTA                                                
         MVC   PSECOND+14(5),NCSTA                                              
         MVC   PSECOND+20(2),NOFF                                               
         MVC   PSECOND+23(3),NSAL                                               
         MVC   PSECOND+27(2),NTEAM                                              
         MVC   PSECOND+30(2),NGRP                                               
         MVC   PSECOND+33(6),NAGY                                               
         MVC   PSECOND+40(4),NADV                                               
         MVC   PSECOND+45(3),NDSP                                               
         MVC   PSECOND+49(3),NPTP                                               
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(3,STADATE),(8,P+53)                                 
         SPACE 1                                                                
         PRINT GEN                                                              
         EDIT  (P4,STACNT),(7,P+62)                                             
         EDIT  (P4,CONCNT),(7,P+71)                                             
         EDIT  (P4,INVCNT),(7,P+80)                                             
         EDIT  (P4,BUDCNT),(7,P+89)                                             
         EDIT  (P4,ATNCNT),(7,P+98)                                             
         EDIT  (P4,COMCNT),(7,P+107)                                            
         EDIT  (P4,BUYCNT),(7,P+116)                                            
         EDIT  (P4,AURCNT),(7,P+125)                                            
         PRINT NOGEN                                                            
         SPACE 1                                                                
         LA    R5,PSECOND+62                                                    
         EDIT  (P4,SDDCNT),(7,(R5))                                             
         LA    R5,9(R5)                                                         
         EDIT  (P4,SALCNT),(7,(R5))                                             
         LA    R5,9(R5)                                                         
         EDIT  (P4,AGYCNT),(7,(R5))                                             
         LA    R5,9(R5)                                                         
         EDIT  (P4,ADVCNT),(7,(R5))                                             
         LA    R5,9(R5)                                                         
         EDIT  (P4,PRDCNT),(7,(R5))                                             
         LA    R5,9(R5)                                                         
         EDIT  (P4,PRPCNT),(7,(R5))                                             
         LA    R5,9(R5)                                                         
         EDIT  (P4,SETCNT),(7,(R5))                                             
         LA    R5,9(R5)                                                         
         EDIT  (P4,DSPCNT),(7,(R5))                                             
         LA    R5,9(R5)                                                         
         SPACE 1                                                                
         EDIT  (P4,DARCNT),(7,PTHIRD+62)                                        
         EDIT  (P4,PTPCNT),(7,PTHIRD+71)                                        
         EDIT  (P4,EOPCNT),(7,PTHIRD+80)                                        
         EDIT  (P4,STRCNT),(7,PTHIRD+89)                                        
         EDIT  (P4,DIRCNT),(7,PTHIRD+98)                                        
***      EDIT  (P4,MKGCNT),(7,PTHIRD+107)                                       
         EDIT  (P4,SLWCNT),(7,PTHIRD+116)                                       
         CLC   PTHIRD,SPACES       IS ANYTHING PRINTED ON 3RD LINE?             
         BE    *+8                 NO                                           
         MVI   PSECOND,X'00'       YES, MAKE SURE SECOND LINE PRINTS            
         SPACE 1                                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT              SPACING LINE                                 
         SPACE 1                                                                
         LA    R4,LENFIXT(R4)                                                   
         B     CNT10                                                            
         SPACE 1                                                                
CNT20    GOTO1 REPORT                                                           
         SPACE 1                                                                
         MVC   P+3(25),=C'**SWITCH RECORDS PURGED**'                            
         EDIT  (P4,SWICNT),(7,P+31)                                             
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         MVC   P+3(24),=C'**TOTAL RECORDS PURGED**'                             
         EDIT  (P5,PURGCNT),(7,P+31)                                            
         GOTO1 REPORT                                                           
         B     SWXIT                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
******************************************************************              
*  BUILD TABLE OF CHANGES FROM SWITCH (X'28') RECORDS            *              
******************************************************************              
*                                                                               
*- INTEREP MASTER/SUBSIDIARY CHANGES:                                           
*        FOR EACH MASTER RECORD SWITCH (TEAM/GRP/ADV/AGY)                       
*        ADD 1 'FIXED REC' FOR EACH SUBSIDIARY REP.                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
******************************************************************              
         SPACE 2                                                                
BLDTBL   NTR1                                                                   
         L     R4,ABLDAREA          POINT TO BEGINNING OF TABLE                 
         USING FIXD,R4                                                          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RSWIKEY,R3                                                       
         MVI   RSWIKTYP,X'28'                                                   
         SPACE 1                                                                
BLDT0020 BAS   RE,HIGHDIR                                                       
         B     BLDT0060                                                         
BLDT0040 BAS   RE,SEQDIR                                                        
         SPACE 1                                                                
BLDT0060 CLI   KEY,X'28'                                                        
         BNE   BLDT0900                                                         
         XC    MASTREP,MASTREP     1ST TIME FOR RECORD                          
*                                                                               
         CLC   KEY+15(3),MDATE                                                  
         BNL   BLDT0080                                                         
         ZIC   RE,KEY+17           MOVE TO NEXT DATE                            
         LA    RE,1(RE)                                                         
         STC   RE,KEY+17                                                        
         XC    KEY+18(9),KEY+18    AND CLEAR OUT REST OF KEY                    
         B     BLDT0020                                                         
         SPACE 1                                                                
BLDT0080 BAS   RE,GETSWI           GET THE RECORD                               
*                                                                               
*   TEST                                                                        
         MVC   P,SPACES            CLEAR PRINT AREA                             
         MVC   P+1(06),=C'SW REQ'                                               
         MVC   P+10(72),RSWIREC                                                 
         GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*                                                                               
*   TEST DUMP                                                                   
***      DC    H'0'                                                             
*   TEST DUMP END                                                               
*                                                                               
BLDT0100 EQU   *                                                                
         L     R3,AREC                                                          
         LA    R6,RSWIELEM                                                      
         USING RSWIELEM,R6                                                      
         MVC   FIXCOD,SPACES       PREPARE FRESH TABLE ENTRY                    
         XC    FIXCNT,FIXCNT                                                    
         MVC   FREP,RSWIKREP                                                    
*                                                                               
         MVC   STADATE,RSWISTA     START DATE FILTER                            
         MVC   SWFLAGS,RSWIDDS     SAVE/SET COMP S/P FLAGS                      
*                                                                               
         SPACE 1                                                                
         CLI   RSWIKCHA,X'01'      TEAM                                         
         BNE   BLDT0120                                                         
         MVC   FOFF,RSWIKOLD           OFFICE                                   
         MVC   FSAL,RSWIKOLD+2         SALESPERSON                              
         MVC   FTEAM,RSWIKOLD+5        TEAM                                     
         MVC   NOFF,RSWINEW                                                     
         MVC   NSAL,RSWINEW+2                                                   
         MVC   NTEAM,RSWINEW+5                                                  
         MVI   CONCHA,X'0C'        CHANGE CONTRACT                              
         MVI   MKGCHA,X'11'        CHANGE MAKEGOOD                              
         MVI   PRPCHA,X'43'        CHANGE PROPOSAL                              
         B     BLDT0380                                                         
         SPACE 1                                                                
BLDT0120 CLI   RSWIKCHA,X'02'      STATION/GROUP                                
         BNE   BLDT0160                                                         
         MVC   FSTA,RSWIKOLD                                                    
         MVC   FGRP,RSWIKOLD+5        GROUP                                     
         MVC   NSTA,RSWINEW                                                     
         MVC   NGRP,RSWINEW+5                                                   
         MVI   DDSTAT,C'Y'         SET 'OVERWRITE STATION CALLS'                
         TM    RSWIDDS,X'80'       SPECIAL DDSSTATION REQ?                      
         BNO   BLDT0140            NO                                           
         MVI   DDSTAT,C'N'         YES - SET 'DON'T OVERWRITE'                  
BLDT0140 EQU   *                                                                
         MVC   FCSTA,RSWIKOLD      CHANGE COMP IN SPL                           
         MVC   NCSTA,RSWINEW                                                    
         MVI   STACHA,X'02'        CHANGE STATION REC                           
         MVI   BUYCHA,X'0B'        CHANGE BUY                                   
         MVI   CONCHA,X'0C'        CHANGE CONTRACT                              
         MVI   INVCHA,RINVKTYQ     CHANGE INVENTORY                             
         MVI   BUDCHA,X'13'        CHANGE BUDGET                                
         MVI   ACLCHA,X'20'        CHANGE ACL                                   
         MVI   SDDCHA,X'26'        CHANGE SDD                                   
         MVI   ATNCHA,X'27'        CHANGE ATHENA                                
         MVI   COMCHA,X'29'        CHANGE COMMISSION REC                        
         MVI   AURCHA,X'2C'        CHANGE A.U.R.                                
         MVI   DARCHA,X'41'        CHANGE DARE                                  
         MVI   MKGCHA,X'11'        CHANGE MAKEGOOD                              
         MVI   SETCHA,X'38'        CHANGE SET                                   
         MVI   EOPCHA,X'1B'        CHANGE EOP REC                               
         MVI   STRCHA,X'39'        CHANGE STRATEGY REC                          
         MVI   DIRCHA,X'35'        CHANGE DIRECT RESPONSE REC                   
         MVI   PRPCHA,X'43'        CHANGE PROPOSAL                              
         MVI   SLWCHA,X'43'        CHANGE PROPOSAL                              
         MVI   BOXCHA,X'45'        CHANGE ACL                                   
         B     BLDT0380                                                         
         SPACE 1                                                                
BLDT0160 CLI   RSWIKCHA,X'03'      DEVELOPMENTAL SALESPERSON                    
         BNE   BLDT0180                                                         
         MVC   FDSP,RSWIKOLD                                                    
         MVC   NDSP,RSWINEW                                                     
         MVI   CONCHA,X'0C'        CHANGE CONTRACT                              
         MVI   MKGCHA,X'11'        CHANGE MAKEGOOD                              
         MVI   PRPCHA,X'43'        CHANGE PROPOSAL                              
         B     BLDT0380                                                         
         SPACE 1                                                                
BLDT0180 CLI   RSWIKCHA,X'04'      OFFICE                                       
         BNE   BLDT0200                                                         
         MVC   FOFF,RSWIKOLD           OFFICE                                   
         MVC   FSAL,RSWIKOLD+2         SALESPERSON                              
         MVC   FTEAM,RSWIKOLD+5        TEAM                                     
         MVC   NOFF,RSWINEW                                                     
         MVC   NSAL,RSWINEW+2                                                   
         MVC   NTEAM,RSWINEW+5                                                  
         MVI   CONCHA,X'0C'        CHANGE CONTRACT                              
         MVI   MKGCHA,X'11'        CHANGE MAKEGOOD                              
         MVI   PRPCHA,X'43'        CHANGE PROPOSAL                              
         B     BLDT0380                                                         
         SPACE 1                                                                
BLDT0200 CLI   RSWIKCHA,X'05'      POINT PERSON                                 
         BNE   BLDT0220                                                         
         MVC   FPTP,RSWIKOLD                                                    
         MVC   NPTP,RSWINEW                                                     
         MVI   PRDCHA,X'09'        CHANGE PRODUCT REC                           
         MVI   DARCHA,X'41'        CHANGE X'41'/X'51' RECORDS                   
         B     BLDT0380                                                         
         SPACE 1                                                                
BLDT0220 CLI   RSWIKCHA,X'08'      ADVERTISER                                   
         BNE   BLDT0240                                                         
         MVC   FADV,RSWIKOLD                                                    
         MVC   NADV,RSWINEW                                                     
         MVI   CONCHA,X'0C'        CHANGE CONTRACT                              
         MVI   MKGCHA,X'11'        CHANGE MAKEGOOD                              
         MVI   TKOCHA,X'1F'        CHANGE TKO EQUIV                             
         MVI   PRPCHA,X'43'        CHANGE PROPOSAL                              
* NOTE- ATHENA RECORDS GET CHANGED VIA RECOVERY TAPE AND ATHENA UPDATE          
         B     BLDT0380                                                         
         SPACE 1                                                                
BLDT0240 CLI   RSWIKCHA,X'10'      SALESPERSON                                  
         BNE   BLDT0260                                                         
         MVC   FSAL,RSWIKOLD+2                                                  
         MVC   FTEAM,RSWIKOLD+5         TEAM                                    
         MVC   FOFF,RSWIKOLD            OFFICE                                  
         MVC   NSAL,RSWINEW+2                                                   
         MVC   NTEAM,RSWINEW+5                                                  
         MVC   NOFF,RSWINEW                                                     
         MVI   CONCHA,X'0C'        CHANGE CONTRACT                              
         MVI   TKOCHA,X'1F'        CHANGE TKO EQUIV                             
         MVI   MKGCHA,X'11'        CHANGE MAKEGOOD                              
         MVI   PRPCHA,X'43'        CHANGE PROPOSAL                              
         MVI   DARCHA,X'41'        CHANGE X'41'/X'51' RECORDS                   
         B     BLDT0380                                                         
         SPACE 1                                                                
BLDT0260 CLI   RSWIKCHA,X'20'      GROUP                                        
         BNE   BLDT0280                                                         
         MVC   FGRP,RSWIKOLD                                                    
         MVC   NGRP,RSWINEW                                                     
         MVI   STACHA,X'02'        CHANGE STATION                               
         MVI   CONCHA,X'0C'        CHANGE CONTRACT                              
         MVI   MKGCHA,X'11'        CHANGE MAKEGOOD                              
         MVI   ATNCHA,X'27'        CHANGE ATHENA                                
         MVI   AURCHA,X'2C'        CHANGE A.U.R.                                
         MVI   STRCHA,X'39'        CHANGE STRATEGY REC                          
         MVI   PRPCHA,X'43'        CHANGE PROPOSAL                              
         B     BLDT0380                                                         
         SPACE 1                                                                
BLDT0280 CLI   RSWIKCHA,X'40'      AGENCY                                       
         BNE   BLDT0300                                                         
         MVC   FAGY(6),RSWIKOLD                                                 
         MVC   NAGY(6),RSWINEW                                                  
         MVC   EXCLC,RSWIOPT+1                                                  
         MVC   EXMVC,RSWIOPT+2                                                  
         MVI   CONCHA,X'0C'        CHANGE CONTRACT                              
         MVI   MKGCHA,X'11'        CHANGE MAKEGOOD                              
         MVI   TKOCHA,X'1F'        CHANGE TKO EQUIV                             
         MVI   PRPCHA,X'43'        CHANGE PROPOSAL                              
         MVI   PRDCHA,X'09'        IF AGY SWITCH, PRODUCT CHANGE                
         B     BLDT0380                                                         
         SPACE 1                                                                
BLDT0300 CLI   RSWIKCHA,X'80'      COMPETITIVE                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   STACHA,X'02'        CHANGE STATION                               
         MVI   CONCHA,X'0C'        CHANGE CONTRACT                              
         MVC   FCSTA,RSWIKOLD                                                   
         MVC   NCSTA,RSWINEW                                                    
         MVC   NCAFF,SPACES            COMPETING AFFILIATE                      
         MVC   NCAFF(2),RSWINEW+5                                               
         CLC   NCAFF(2),SPACES                                                  
         BE    BLDT0380                                                         
         SPACE 1                                                                
*                                                                               
*   SPECIAL CONSIDERATION FOR AFFIL = 'CBL'.  REQUEST CARD                      
*        CONTAINS 'C*', WHICH IS FORCIBLY TRANSLATED AS 'CBL'                   
*        BY THIS TEST.                                                          
*                                                                               
         CLC   =C'C*',NCAFF        SPECIAL 'CBL' INDICATOR?                     
         BNE   BLDT0320            NO                                           
         MVC   NCAFF,=C'CBL'       YES - FORCE TO 'CBL'                         
         B     BLDT0380                                                         
BLDT0320 CLI   0(R1),X'FF'                                                      
         LA    R1,AFFTAB                                                        
BLDT0340 CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   NCAFF(2),0(R1)                                                   
         BE    BLDT0360                                                         
         LA    R1,L'AFFTAB(R1)                                                  
         B     BLDT0340                                                         
BLDT0360 MVC   NCAFF,0(R1)                                                      
         SPACE 2                                                                
BLDT0380 MVI   ACTION,C'C'                                                      
***      BAS   RE,NTH              FILL IN REST OF TABLE                        
         GOTO1 =A(NTHRELO),RR=Y    FILL IN REST OF TABLE                        
         EJECT                                                                  
******************************************************************              
*                        DEAL WITH FILTER ELEMENTS               *              
******************************************************************              
         SPACE 2                                                                
         L     R6,AREC                                                          
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL            ARE THERE FILTER ELEMENTS?                   
         BE    BLDT0420            YES                                          
         OC    STADATE,STADATE     IF NO FILTERS, MAY WANT TO                   
         BZ    BLDT0560            DELETE OLD RECORDS                           
         B     BLDT0780            START DATE FILTER PRESENT                    
*                                                                               
BLDT0400 BAS   RE,NEXTEL                                                        
         BNE   BLDT0780            NEXT RECORD                                  
         USING RSWIFIEL,R6                                                      
BLDT0420 CLI   RSWIFTYP,X'01'      TEAM                                         
         BNE   BLDT0440                                                         
         MVC   FTEAM,RSWIFFTR                                                   
         B     BLDT0400                                                         
         SPACE 1                                                                
BLDT0440 CLI   RSWIFTYP,X'02'      STATION                                      
         BNE   BLDT0460                                                         
         MVC   FSTA,RSWIFFTR                                                    
         MVC   FGRP,RSWIFFTR+5         GROUP                                    
         B     BLDT0400                                                         
         SPACE 1                                                                
BLDT0460 CLI   RSWIFTYP,X'04'      OFFICE                                       
         BNE   BLDT0480                                                         
         MVC   FOFF,RSWIFFTR                                                    
         B     BLDT0400                                                         
         SPACE 1                                                                
BLDT0480 CLI   RSWIFTYP,X'08'      ADVERTISER                                   
         BNE   BLDT0500                                                         
         MVC   FADV,RSWIFFTR                                                    
         B     BLDT0400                                                         
         SPACE 1                                                                
BLDT0500 CLI   RSWIFTYP,X'10'      SALESPERSON                                  
         BNE   BLDT0520                                                         
         MVC   FSAL,RSWIFFTR                                                    
         B     BLDT0400                                                         
         SPACE 1                                                                
BLDT0520 CLI   RSWIFTYP,X'20'      GROUP                                        
         BNE   BLDT0540                                                         
         MVC   FGRP,RSWIFFTR                                                    
         B     BLDT0400                                                         
         SPACE 1                                                                
BLDT0540 CLI   RSWIFTYP,X'40'      AGENCY                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FAGY(6),RSWIFFTR                                                 
         B     BLDT0400                                                         
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*  WHEN THERE ARE NO FILTERS, WE MAY WANT TO DELETE THE OLD CODE *              
******************************************************************              
         SPACE 1                                                                
BLDT0560 LA    R4,LENFIXT(R4)      POINT TO NEXT TABLE ENTRY                    
         SPACE 1                                                                
         L     RE,TBLCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,TBLCNT                                                        
         CLC   TBLCNT(4),NUMBLD                                                 
         BNE   *+6                                                              
         DC    H'0'                TOO MANY ENTRIES IN TABLE                    
         SPACE 1                                                                
         MVC   FIXCOD,SPACES       PREPARE FRESH TABLE ENTRY                    
         XC    FIXCNT,FIXCNT                                                    
         MVC   FREP,RSWIKREP                                                    
         XC    STADATE,STADATE     CLEAR START DATE FILTER                      
         SPACE 1                                                                
         CLI   RSWIKCHA,X'01'      TEAM                                         
         BNE   BLDT0580                                                         
         MVC   FSAL,RSWIKOLD+2                                                  
         MVC   NEWCODE(3),RSWINEW+2   FOR SET CHANGE                            
         MVI   ACTION,C'P'                                                      
         MVI   SETCHA,X'38'        CHANGE SET                                   
         MVI   SALCHA,X'06'        DELETE SALES REC                             
         MVI   EOPCHA,X'1B'        DELETE SALES EOP REC                         
         B     BLDT0760            DELETE OLD SALESPERSON RECORD                
         SPACE 1                                                                
BLDT0580 CLI   RSWIKCHA,X'02'      STATION                                      
         BE    BLDT0800                                                         
*         BNE   BLDT0600                                                        
*         MVC   FSTA,RSWIKOLD                                                   
*         MVI   ACTION,C'P'                                                     
*         MVI   STACHA,X'02'                                                    
*         B     BLDT0760                                                        
         SPACE 1                                                                
BLDT0600 CLI   RSWIKCHA,X'03'      DEVELOPMENTAL SALESPERSON                    
         BNE   BLDT0620                                                         
         MVC   FDSP,RSWIKOLD                                                    
         MVI   ACTION,C'P'                                                      
         MVI   DSPCHA,X'3A'        DELETE DSP REC                               
         B     BLDT0760            DELETE OLD SALESPERSON RECORD                
         SPACE 1                                                                
BLDT0620 CLI   RSWIKCHA,X'04'      OFFICE                                       
         BNE   BLDT0640                                                         
         MVC   FSAL,RSWIKOLD+2                                                  
         MVC   NEWCODE(3),RSWINEW+2   FOR SET CHANGE                            
         MVI   ACTION,C'P'                                                      
         MVI   SETCHA,X'38'        CHANGE SET                                   
         MVI   SALCHA,X'06'        DELETE SALES REC                             
         MVI   EOPCHA,X'1B'        DELETE SALES EOP REC                         
         B     BLDT0760            DELETE OLD SALESPERSON RECORD                
         SPACE 1                                                                
BLDT0640 CLI   RSWIKCHA,X'05'      POINT PERSON                                 
         BNE   BLDT0660                                                         
         MVC   FPTP,RSWIKOLD                                                    
         MVC   NEWCODE,RSWINEW     FOR SET CHANGE                               
         MVI   ACTION,C'P'                                                      
         MVI   SETCHA,X'38'        CHANGE SET                                   
         MVI   PTPCHA,X'31'        DELETE PP REC                                
         B     BLDT0760            DELETE OLD POINT PERSON RECORD               
         SPACE 1                                                                
BLDT0660 CLI   RSWIKCHA,X'08'      ADVERTISER                                   
         BNE   BLDT0680                                                         
         MVC   FADV,RSWIKOLD                                                    
         MVC   NEWCODE,RSWINEW     FOR SET CHANGE                               
         MVI   ACTION,C'P'                                                      
         MVI   SETCHA,X'38'        CHANGE SET                                   
         MVI   ADVCHA,X'08'        DELETE ADV REC                               
         MVI   PRDCHA,X'09'        DELETE PROD RECS                             
         MVI   COMCHA,X'29'        CHANGE COMMISSION REC                        
         MVI   EOPCHA,X'1B'        DELETE ADV EOP REC                           
         MVI   DROCHA,X'4C'        CHANGE ROM ADV EOP REC                       
         B     BLDT0760                                                         
         SPACE 1                                                                
BLDT0680 CLI   RSWIKCHA,X'10'      SALESPERSON                                  
         BNE   BLDT0700                                                         
         MVC   FSAL,RSWIKOLD+2                                                  
         MVC   NEWCODE(3),RSWINEW+2   FOR SET CHANGE                            
         MVI   ACTION,C'P'                                                      
         MVI   SETCHA,X'38'        CHANGE SET                                   
         MVI   SALCHA,X'06'        DELETE SALES REC                             
         MVI   EOPCHA,X'1B'        DELETE SALES EOP REC                         
         B     BLDT0760                                                         
         SPACE 1                                                                
BLDT0700 CLI   RSWIKCHA,X'20'      GROUP                                        
         BE    BLDT0800            NEVER DELETE OLD RECORD                      
         SPACE 1                                                                
BLDT0720 CLI   RSWIKCHA,X'40'      AGENCY                                       
         BNE   BLDT0800                                                         
         CLI   RSWIOPT,C'Y'                                                     
         BNE   BLDT0800              N=DON'T DELETE FROM AGENCY                 
         SPACE 1                                                                
BLDT0740 MVC   FAGY(6),RSWIKOLD       ELSE DELETE FROM AGENCY                   
         MVC   NEWCODE,RSWINEW     FOR SET CHANGE                               
         MVI   ACTION,C'P'                                                      
* IF ADD ANY MORE RECORD CHANGES FOR WHEN AN AGENCY REC IS PURGED,              
* BE SURE TO INCLUDE THE CODE FROM THE SET CHANGE FOR AGY                       
         MVI   SETCHA,X'38'        CHANGE SET                                   
         MVI   AGYCHA,X'0A'        DELETE AGY REC                               
         MVI   EOPCHA,X'1B'        DELETE AGY EOP REC                           
         B     BLDT0760                                                         
         SPACE                                                                  
BLDT0760 EQU   *                                                                
***      BAS   RE,NTH              FILL IN REST OF TABLE                        
         GOTO1 =A(NTHRELO),RR=Y        FILL IN REST OF TABLE                    
         SPACE                                                                  
BLDT0780 LA    R4,LENFIXT(R4)      POINT TO NEXT TABLE ENTRY                    
         SPACE 1                                                                
         L     RE,TBLCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,TBLCNT                                                        
         CLC   TBLCNT(4),NUMBLD                                                 
         BNE   *+6                                                              
         DC    H'0'                TOO MANY ENTRIES IN TABLE                    
         SPACE 1                                                                
BLDT0800 EQU   *                                                                
         GOTO1 =A(NEXTREP),(RC),RR=Y                                            
         CLC   RSWIKREP,=XL2'00'   ANOTHER REP CODE?                            
         BNE   BLDT0100            YES                                          
*                                                                               
         B     BLDT0040            READ NEXT SWITCH RECORD.                     
         SPACE                                                                  
BLDT0900 MVI   0(R4),X'FF'         END OF TABLE                                 
         B     SWXIT                                                            
         EJECT                                                                  
NTH      EQU   *                                                                
****     GOTO1 =A(NTHRELO),RR=Y                                                 
****     BR    RE                                                               
         EJECT                                                                  
* BUILD RECOVERY 24 BYTE HEADER                                                 
         SPACE 2                                                                
BLDRCVH  NTR1                                                                   
         LA    R5,RECVHDR                                                       
         XC    RECVHDR,RECVHDR                                                  
         MVI   RFILTY,X'82'                                                     
         MVC   RSIN,=X'00000001'                                                
         MVI   RSYS,X'08'                                                       
         MVI   RPRG,C'S'                                                        
         GOTO1 DATCON,DMCB,(5,0),(3,RDATE)                                      
         B     SWXIT                                                            
         EJECT                                                                  
* BUILD RECOVERY RECORD                                                         
         SPACE 1                                                                
* IF STATION, GROUP AND OR ADVERTISER CHANGES, IT AFFECTS ATHENA.               
* IF IT'S STATION OR GROUP, WE SWITCH THROUGH THIS PROGRAM. BUT IN              
* THE CASE OF ADVERTISER SWITCHES, IN MANY CASES THE ATHENA RECORD              
* CREATED BY THE CHANGE ALREADY EXISTS. SO INSTEAD OF CHANGING THE              
* ATHENA RECORD HERE, BUILD 2 FAKE RECOVERY RECORDS - A COPY AND A              
* CHANGE - AND LET THE ATHENA UPDATE PROGRAM DEAL WITH THE ATHENA               
* RECORDS FOR ADVERTISER SWITCHES.                                              
         SPACE 1                                                                
* R3 IS POINTING AT REC                                                         
* R4 IS POINTING AT BLDAREA                                                     
         USING FIXD,R4                                                          
         SPACE 2                                                                
BLDRCVR  NTR1                                                                   
         LA    RF,RCVREC           RECOVERY RECORD                              
         MVC   HALF,27(R3)         CONTRACT RECORD LENGTH                       
         LH    R1,HALF                                                          
         LR    RE,R3               R3 POINTS TO CONTRACT                        
         MOVE  ((RF),(R1)),(RE)    MOVE CONTRACT TO RECOVERY RECORD             
*                                                                               
         LA    R5,RECVHDR-4        RECOVERY RECORD LENGTH =                     
         MVC   HALF,27(R3)         CONTRACT LENGTH PLUS                         
         LH    R1,HALF                                                          
         AH    R1,=H'28'           4 BYTES LENGTH PLUS 24 BYTE HEADER           
         STCM  R1,3,0(R5)                                                       
*                                                                               
         AR    R1,R5                                                            
         XC    0(2,R1),0(R1)       AND MARK RECORD END WITH ZEROS               
*                                                                               
         MVI   RRECTY,X'01'        COPY                                         
         THMS                                                                   
         ST    R1,RTIME            SET TIME (0HHMMSS+)                          
         LA    R0,RECVHDR-4                                                     
         PUT   RCVROUT,(R0)        PUT COPY RECORD TO OUTPUT                    
         BAS   RE,DMPRCVR1         DISPLAY RECOVERY                             
         SPACE 1                                                                
         MVI   RRECTY,X'02'        CHANGE - FILL IN NEW CODES                   
         LA    RF,RCVREC           ON RECOVERY RECORD                           
         USING RECD,RF                                                          
         CLC   NADV,SPACES         ADVERTISER CHANGE?                           
         BE    *+10                NO                                           
         MVC   RCONKADV,NADV                                                    
         CLC   NOFF,SPACES         OFFICE     CHANGE?                           
         BE    *+10                NO                                           
         MVC   RCONKOFF,NOFF                                                    
         CLC   NAGY,SPACES         AGENCY     CHANGE?                           
         BE    BLDR0040            NO                                           
         CLI   EXMVC,0             MUST BE THERE IF THERE'S NAGY                
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,EXMVC            MAY BE MOVING FOR 4 (AGENCY)                 
         EX    RE,BLDR0010               OR 6 (AGENCY/OFFICE)                   
         B     BLDR0040                                                         
BLDR0010 MVC   RCONKAGY(0),NAGY    AGENCY                                       
BLDR0040 EQU   *                                                                
         LA    R0,RECVHDR-4                                                     
         PUT   RCVROUT,(R0)        PUT CHANGE RECORD TO OUTPUT                  
         BAS   RE,DMPRCVR2         DISPLAY RECOVERY                             
         B     SWXIT                                                            
         DROP  RF,R4                                                            
         EJECT                                                                  
DMPRCVR1 NTR1                                                                   
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SWXIT               NO  - NO DISPLAY                             
         MVC   P(3),=C'CPY'                                                     
         LA    R6,P+5                                                           
         B     DRCV0010                                                         
DMPRCVR2 NTR1                                                                   
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BNE   SWXIT               NO  - NO DISPLAY                             
         MVC   P(3),=C'CHG'                                                     
         LA    R6,P+5                                                           
DRCV0010 EQU   *                                                                
         LA    R3,RCVREC           SET A(RECOVERY RECORD)                       
         B     DUMP                GO PRINT IT OUT                              
         EJECT                                                                  
DMPGET   NTR1                                                                   
         AP    0(4,R5),=P'1'                                                    
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BE    DMPGET01            YES - DISPLAY ALL                            
         ZAP   DUB,0(4,R5)                                                      
         DP    DUB,4(4,R5)                                                      
         CP    DUB+4(4),=P'0'                                                   
         BNE   SWXIT                                                            
DMPGET01 EQU   *                                                                
         ZAP   DUB,0(4,R5)                                                      
         CP    DUB+4(4),=P'100'    DISPLAY FIRST N RECS                         
         BH    SWXIT                                                            
         MVC   P(3),=C'GET'                                                     
         LA    R6,P+5                                                           
         B     DUMP                                                             
         SPACE 1                                                                
DMPPUT   NTR1                                                                   
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BE    DMPPUT01            YES - DISPLAY ALL                            
         ZAP   DUB,0(4,R5)                                                      
         DP    DUB,4(4,R5)                                                      
         CP    DUB+4(4),=P'0'                                                   
         BNE   SWXIT                                                            
DMPPUT01 EQU   *                                                                
         ZAP   DUB,0(4,R5)                                                      
         CP    DUB+4(4),=P'100'    DISPLAY FIRST N RECS                         
         BH    SWXIT                                                            
         MVC   P(3),=C'PUT'                                                     
         SPACE 1                                                                
DUMP     EQU   *                                                                
         GOTO1 =A(DUMPRELO),RR=Y   COMMON ROUTINE FOR DUMP                      
         XIT1                                                                   
         EJECT                                                                  
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 2                                                                
SEQDIR   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETSWI   LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               AREC,(0,DMWORK)                                                  
         B     DMCHECK                                                          
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         B     SWXIT                                                            
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         B     SWXIT                                                            
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
         SPACE 1                                                                
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     SWXIT                                                            
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
*                                                                               
*- BLDMAST -- READ THRU ALL REP CODES ON FILE AND BUILD                         
*             MASTER/SUBSIDIARY REP LIST.                                       
*                                                                               
*  LIST FORMAT:                                                                 
* +0     CL2'MASTER REP CODE'                                                   
* +2     H'LENGTH OF LIST ENTRY'                                                
* +4     H'NUMBER OF SUBSIDIARY REPS THAT FOLLOW'                               
* +6     NCL2'VARIABLE NUMBER OF SUBSIDIARY REP CODES'                          
*                                                                               
* LIST ENDS WITH H'0'                                                           
*                                                                               
BLDMAST  NTR1                                                                   
*                                                                               
         L     RF,=A(COMPREPS)                                                  
         A     RF,RELO             RF = A(REPS USING COMP S/P)                  
         XC    0(L'COMPREPS,RF),0(RF)                                           
*                                  CLEAR COMPREPS TABLE                         
         L     R2,=A(MASTLIST)                                                  
         A     R2,RELO             R2 = A(CURRENT LIST ENTRY)                   
*                                                                               
         L     R8,AREC                                                          
         USING RECD,R8                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           REP REC KEY ID                               
         GOTO1 HIGHDIR                                                          
         B     BMST030                                                          
BMST020  GOTO1 SEQDIR                                                           
BMST030  CLI   KEY,X'01'                                                        
         BE    BMST040                                                          
*                                                                               
*- END OF REP KEYS.  INCLUDE ENDING H'0' AND EXIT.                              
         MVC   0(2,R2),=H'00'                                                   
*                                                                               
*   TEST DISPLAY                                                                
***      L     RF,=A(COMPREPS)                                                  
***      A     RF,RELO             RF = A(REPS USING COMP S/P)                  
***      MVC   P(09),=C'COMP S/P:'                                              
***      MVC   P+20(62),0(RF)                                                   
***      GOTO1 REPORT                                                           
*   END TEST DISPLAY                                                            
*                                                                               
         B     SWXIT                                                            
*                                                                               
*- ONLY INTERESTED IN MASTER REPS                                               
BMST040  GOTO1 GETSWI                                                           
*                                                                               
*        MVC   P(10),=CL10'*REP REC*'                                           
*        MVC   P+10(27),RREPREC                                                 
*        MVC   P+40(2),RREPMAST                                                 
**       CLC   =H'-1',RREPMAST                                                  
*        BNE   BMST045                                                          
*        MVC   P+44(2),=C'-1'                                                   
BMST045  GOTO1 REPORT                                                           
*                                                                               
         CLC   =H'-1',RREPMAST     MASTER REP?                                  
         BE    BMST048             YES                                          
         GOTO1 =A(CHKCMPSP),RR=Y   CHECK FOR COMPENSATION S/P USE               
         B     BMST020             GO BACK FOR NEXT REP                         
BMST048  EQU   *                                                                
         LA    R3,RREPELEM         X'01' ELEMENT                                
BMST050  CLI   0(R3),0                                                          
         BE    BMST020             NO SUBSIDIARY ELEMENT                        
*                                                                               
         CLI   0(R3),X'02'         SUBSIDIARY ELEMENT?                          
         BE    BMST060                                                          
*                                                                               
         ZIC   RE,1(R3)            NEXT ELEMENT                                 
         AR    R3,RE                                                            
         B     BMST050                                                          
*                                                                               
*- R3 = A(SUBSIDIARY REP ELEMENT).  BUILD MASTER LIST ENTRY.                    
BMST060  MVC   0(2,R2),RREPKREP      REP CODE                                   
*                                                                               
         ZIC   RE,2(R3)            NUMBER OF SUB REPS (FROM ELEMENT)            
         STH   RE,4(R2)                                                         
*                                                                               
         SLA   RE,1                X2                                           
*                                                                               
         LR    RF,RE               SAVE LENGTH FOR 'MVC'                        
         LA    RE,6(RE)            +6 (REP CODE/ENTRY LEN/$ SUBREPS)            
*                                                                               
*   SPECIAL PROCESSING FOR KRG:  CLEAR CHANNEL (NU) IS NO LONGER IN             
*        THE SUBSIDIARY LIST, BUT MUST BE REINSERTED FOR SWITCH                 
*        PURPOSES.                                                              
*                                                                               
         CLC   =C'K3',RREPKREP     KRG?                                         
         BNE   BMST080             NO                                           
         LA    RE,2(RE)            YES - INCREASE TABLE LENGTH BY TWO           
         STH   RE,2(R2)            LIST ENTRY LENGTH                            
         MVC   6(2,R2),=C'NU'      INSERT 'NU' AS FIRST ENTRY IN TABLE          
         BCTR  RF,0                LESS 1 FOR EX                                
         EX    RF,BMSTMOV2                                                      
         ZICM  RE,4(R2),2          INCREASE COUNT BY 1                          
         LA    RE,1(RE)                                                         
         STH   RE,4(R2)            PUT COUNT BACK                               
         B     BMST0100                                                         
BMST080  EQU   *                                                                
         STH   RE,2(R2)            LIST ENTRY LENGTH                            
*                                                                               
         BCTR  RF,0                LESS 1 FOR EX                                
         EX    RF,BMSTMOVE                                                      
BMST0100 EQU   *                                                                
*                                                                               
*        MVC   P(6),=C'MASTER'                                                  
*        MVC   P+8(2),RREPKREP                                                  
*        MVC   P+12(4),=C'SUBS'                                                 
*        EX    RF,BMSTPRNT                                                      
*        GOTO1 REPORT                                                           
*        MVC   P(6),=C'MASTER'                                                  
*        MVC   P+8(2),RREPKREP                                                  
*        MVC   P+12(5),=C'TABLE'                                                
*        MVC   P+20(36),0(R2)                                                   
*        GOTO1 REPORT                                                           
*                                                                               
         AH    R2,2(R2)            POINT TO NEXT ENTRY                          
*                                                                               
         L     RF,=A(MASTLSTX)     DON'T EXCEED TABLE MAX                       
         A     RF,RELO                                                          
         CR    R2,RF                                                            
         BL    BMST020                                                          
         DC    H'0'                REP MASTER LIST EXCEEDED.                    
*                                  MAKE IT BIGGER.                              
*                                                                               
BMSTMOVE MVC   6(0,R2),10(R3)      SUB REPS TO MASTER LIST                      
BMSTMOV2 MVC   8(0,R2),10(R3)      SUB REPS TO MASTER LIST: + NU                
BMSTPRNT MVC   P+20(0),10(R3)                                                   
         DROP  R8                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
AFFTAB   DS    0CL3                                                             
       ++INCLUDE REAFFLIST                                                      
         LTORG                                                                  
         EJECT                                                                  
         SPACE 3                                                                
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,MACRF=GM,               X        
               EODAD=SWEOF                                                      
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
RCVROUT  DCB   DDNAME=RCVROUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=8200,BLKSIZE=27648,BUFNO=2                                 
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 3                                                                
         DS    0H                  HALFWORD ALIGNMENT NEEDED.                   
COMPREPS DS    CL60                REPS USING COMP S/P CODES                    
*                                     ROOM FOR 30 CODES                         
COMPDEL  DC    X'FFFF'             TABLE DELIMITER                              
MASTLIST DS    3000C               MASTER/SUBSID REC LIST                       
MASTLSTX EQU   *                                                                
         DS    H'0'                MASTER LIST EOT.                             
*                                                                               
MYWORKD  DSECT                                                                  
RELO     DS    F                   RELOCATION FACTOR                            
MASTREP  DS    A                                                                
SUBREP   DS    A                                                                
ABLDAREA DS    A                                                                
LBLDAREA DS    F                                                                
NUMBLD   DS    F                                                                
*********NUMBLD   EQU   (BLDLEN/LENFIXT)-1                                      
*                                                                               
ELCODE   DS    CL1                                                              
MDATE    DS    CL3   (BINARY YMD MONDAY OF THIS WEEK (OR MON OF ASAT))          
FILTCODE DS    CL6                 FILTER CODE (FOR SET RECS)                   
SETTYPE  DS    X                   RECORD TYPE OF THE SET TYPE                  
FIRSTCON DS    CL1                                                              
SWICNT   DS    PL4                 SWITCH COUNT                                 
SWINTH   DS    PL4                 SWITCH DUMP (1)                              
PRGYMD   DS    CL3                 BINARY YMD - 91 DAYS PRIOR TO MDATE          
STAMATCH DS    CL1                 STATION MATCH FLAG                           
TBLCNT   DS    F                   COUNTER FOR ENTRIES IN TABLE                 
CHIND    DS    CL1                 Y=RECORD CHANGED IN THIS PASS                
DUPIND   DS    CL1                 Y=MEMBER HAS ALREADY OCCURRED IN SET         
FLAGS    DS    XL1                 X'80' = X'02' ELEMENT ENCOUNTERED            
SWFLAG   DS    CL1                                                              
ELEM     DS    CL256               ELEMENT WORKSPACE                            
*                                                                               
AREC     DS    A                                                                
ARCVREC  DS    A                                                                
*                                                                               
         DS    F                   LENGTH OF RECOVERY RECORD                    
       ++INCLUDE DMRCVRHDR                                                      
RCVREC   DS    CL6144              AREA FOR RECOVERY RECORD                     
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL6144              AREA FOR RECORD                              
         SPACE 2                                                                
MYWORKX  EQU   *                                                                
*                                                                               
         EJECT                                                                  
FIXD     DSECT         DSECT FOR CHANGE LIST                                    
FIXCOD   DS    0CL84                                                            
FILTER   DS    0CL35               FILTER FIELDS                                
FREP     DS    CL2                 REP                                          
FSTA     DS    CL5                 STATION                                      
FOFF     DS    CL2                 OFFICE                                       
FGRP     DS    CL2                 GROUP/SUB-GROUP                              
FTEAM    DS    CL2                 DIVISION/TEAM                                
FSAL     DS    CL3                 SALESPERSON                                  
FADV     DS    CL4                 ADVERTISER                                   
FAGY     DS    CL4                 AGENCY                                       
FAGYOF   DS    CL2                 AGENCY-OFFICE                                
FDSP     DS    CL3                 DEVELOPMENTAL SALESPERSON                    
FPTP     DS    CL3                 POINT PERSON                                 
STADATE  DS    XL3                 START DATE FILTER FOR CONTRACTS              
         SPACE 1                                                                
NEW      DS    0CL49                                                            
NSTA     DS    CL5                 NEW STATION                                  
NOFF     DS    CL2                 NEW OFFICE                                   
NGRP     DS    CL2                 NEW GROUP/SUB-GROUP                          
NTEAM    DS    CL2                 NEW DIVISION/TEAM                            
NSAL     DS    CL3                 NEW SALESPERSON                              
NADV     DS    CL4                 ADVERTISER                                   
NAGY     DS    CL4                 AGENCY                                       
NAGYOF   DS    CL2                 AGENCY-OFFICE                                
NDSP     DS    CL3                 NEW DEVELOPMENTAL SALESPERSON                
NPTP     DS    CL3                 NEW POINT PERSON                             
NEWCODE  DS    CL6                 USED FOR NEW CODE IN SWITCHING SETS          
         SPACE 1                                                                
FCSTA    DS    CL5                 COMPETING STATION                            
NCSTA    DS    CL5                 NEW COMPETING STATION                        
NCAFF    DS    CL3                 NEW COMPETING AFFILIATE                      
         SPACE 1                                                                
CNT1ST   DS    0C                                                               
STACHA   DS    CL1                 X'02' = CHANGE STATION RECORD                
STACNT   DS    PL4                 COUNT STATION RECORDS CHANGED                
STANTH   DS    PL4                 DUMP EVERY NTH RECORD                        
BUYCHA   DS    CL1                 X'0B' = CHANGE BUY      RECORD               
BUYCNT   DS    PL4                 COUNT                                        
BUYNTH   DS    PL4                 DUMP                                         
CONCHA   DS    CL1                 X'0C' = CHANGE CONTRACT RECORD               
CONCNT   DS    PL4                 COUNT                                        
CONNTH   DS    PL4                 DUMP                                         
INVCHA   DS    CL1                 X'62' = CHANGE INVENTORY RECORD              
INVCNT   DS    PL4                 COUNT                                        
INVNTH   DS    PL4                 DUMP                                         
BUDCHA   DS    CL1                 X'13' = CHANGE BUDGET RECORD                 
BUDCNT   DS    PL4                 COUNT                                        
BUDNTH   DS    PL4                 DUMP                                         
ATNCHA   DS    CL1                 X'27' = CHANGE ATHENA RECORD                 
ATNCNT   DS    PL4                 COUNT                                        
ATNNTH   DS    PL4                 DUMP                                         
AURCHA   DS    CL1                 X'2C' = CHANGE A.U.R. RECORD                 
AURCNT   DS    PL4                 COUNT                                        
AURNTH   DS    PL4                 DUMP                                         
ADVCHA   DS    CL1                 X'08' = CHANGE ADVERTISER RECORD             
ADVCNT   DS    PL4                 COUNT                                        
ADVNTH   DS    PL4                 DUMP                                         
PRDCHA   DS    CL1                 X'09' = CHANGE PRODUCT RECORD                
PRDCNT   DS    PL4                 COUNT                                        
PRDNTH   DS    PL4                 DUMP                                         
SDDCHA   DS    CL1                 X'26' = CHANGE SDD RECORD                    
SDDCNT   DS    PL4                 COUNT                                        
SDDNTH   DS    PL4                 DUMP                                         
AGYCHA   DS    CL1                 X'0A' = CHANGE AGENCY RECORD                 
AGYCNT   DS    PL4                 COUNT                                        
AGYNTH   DS    PL4                 DUMP                                         
SALCHA   DS    CL1                 X'06' = CHANGE SALESPERSON RECORD            
SALCNT   DS    PL4                 COUNT                                        
SALNTH   DS    PL4                 DUMP                                         
COMCHA   DS    CL1                 X'29' = COMMISSION RECORD.                   
COMCNT   DS    PL4                 COUNT                                        
COMNTH   DS    PL4                 DUMP                                         
DARCHA   DS    CL1                 X'41' = DARE RECORD.                         
DARCNT   DS    PL4                 COUNT                                        
DARNTH   DS    PL4                 DUMP                                         
MKGCHA   DS    CL1                 X'11' = MAKEGOOD RECORD                      
MKGCNT   DS    PL4                 COUNT                                        
MKGNTH   DS    PL4                 DUMP                                         
SETCHA   DS    CL1                 X'38' = SET RECORD                           
SETCNT   DS    PL4                 COUNT                                        
SETNTH   DS    PL4                 DUMP                                         
DSPCHA   DS    CL1                 X'3A' = CHANGE DEVSAL RECORD                 
DSPCNT   DS    PL4                 COUNT                                        
DSPNTH   DS    PL4                 DUMP                                         
PTPCHA   DS    CL1                 X'31' = CHANGE POINT PERSON RECORD           
PTPCNT   DS    PL4                 COUNT                                        
PTPNTH   DS    PL4                 DUMP                                         
EOPCHA   DS    CL1                 X'1B' = CHANGE EOP REC (ALL TYPES)           
EOPCNT   DS    PL4                 COUNT                                        
EOPNTH   DS    PL4                 DUMP                                         
STRCHA   DS    CL1                 X'39' = CHANGE STRATEGY REC                  
STRCNT   DS    PL4                 COUNT                                        
STRNTH   DS    PL4                 DUMP                                         
DIRCHA   DS    CL1                 X'35' = CHANGE DIRECT RESPONSE REC           
DIRCNT   DS    PL4                 COUNT                                        
DIRNTH   DS    PL4                 DUMP                                         
PRPCHA   DS    CL1                 X'43' = PROPOSAL RECORD                      
PRPCNT   DS    PL4                 COUNT                                        
PRPNTH   DS    PL4                 DUMP                                         
SLWCHA   DS    CL1                 X'43' = SELWIN RECORD                        
SLWCNT   DS    PL4                 COUNT                                        
SLWNTH   DS    PL4                 DUMP                                         
DUMCHA   DS    CL1                 DUMMY VARIABLES-SO DON'T UPDATE REAL         
DUMCNT   DS    PL4                 DUMMY COUNTER                                
DUMNTH   DS    PL4                 DUMMY                                        
ACLCHA   DS    CL1                 X'20' = ACL RECORD                           
ACLCNT   DS    PL4                 COUNT                                        
ACLNTH   DS    PL4                 DUMP                                         
BOXCHA   DS    CL1                 X'45' = SONNET BOXID RECORD                  
BOXCNT   DS    PL4                 COUNT                                        
BOXNTH   DS    PL4                 DUMP                                         
TKOCHA   DS    CL1                 X'1F' = TAKEOVER EQUIV RECORD                
TKOCNT   DS    PL4                 COUNT                                        
TKONTH   DS    PL4                 DUMP                                         
DROCHA   DS    CL1                 X'4C' = ROM/DARE EOP RECORD                  
DROCNT   DS    PL4                 COUNT                                        
DRONTH   DS    PL4                 DUMP                                         
FIXCNTX  EQU   *                                                                
         ORG   CNT1ST                                                           
FIXCNT   DS    CL(FIXCNTX-*)       CODED FOR SOFT LENGTH                        
         ORG                                                                    
*                                                                               
                                                                                
ACTION   DS    C                   P=PURGE,  C=CHANGE                           
EXCLC    DS    X                   EXECUTED COMPARE FOR AGY OR AGY/OFF          
EXMVC    DS    X                   EXECUTED MOVE FOR AGY OR AGY/OFF             
DDSTAT   DS    CL1                 FLAG: DON'T OVERWRITE STATION CALLS          
SWFLAGS  DS    CL1                 X'80' = DON'T OVERWRITE OLD STATION          
*                                  X'40' = 'DATE START' WITHIN FLIGHT           
*                                  X'20' = COMP S/P -> NEW S/P                  
*                                  X'10' = COMP S/P -> HOUSE ACCOUNT            
         DS    CL2                 SPARE                                        
FIXDX    EQU   *                                                                
LENFIXT  EQU   FIXDX-FIXD                                                       
         EJECT                                                                  
SWID     DSECT                                                                  
       ++INCLUDE REGENSWI                                                       
         EJECT                                                                  
*                                                                               
*  INCLUDE REGENCOM                COMMISSION RECORD                            
*  INCLUDE REGENREPA               NEW REP RECORD                               
*  INCLUDE REGENADV                ADVERTISER RECORD                            
*  INCLUDE REGENAGY                AGENCY RECORD                                
*  INCLUDE REGENBUD                BUDGET RECORD                                
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENINV                INVENTORY RECORD                             
*  INCLUDE REGENPRD                PRODUCT RECORD                               
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENSTA                STATION RECORD                               
*  INCLUDE REGENATNA                                                            
*  INCLUDE REGENSDD                                                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL1008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENCOM          COMMISSION RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENREPA         NEW REP RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          ADVERTISER RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUD          BUDGET RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENINV          INVENTORY RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDAR          DARE RECORD                                  
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENMKG          MAKEGOOD RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENTKO          TAKEOVER EQUIVALENTS                         
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSET          SET RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDSP          DEVSAL RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPTP          POINT PERSON RECORD                          
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENEOP          EOP RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDRO          ROM/DARE EOP RECORD                          
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTR          STRATEGY RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENACL          ACL RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBOX          SONNET BOXID RECORDS                         
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDIR          DIRECT RESPONSE RECORD                       
         EJECT                                                                  
       ++INCLUDE REGENPRO          PROPOSAL RECORD                              
         EJECT                                                                  
       ++INCLUDE REGENATNA         ATHENA                                       
         EJECT                                                                  
       ++INCLUDE REGENAUR          AVERAGE UNIT RATE RECORD                     
         EJECT                                                                  
       ++INCLUDE REGENSLW          SELLERS WORKSHEET FOR WINDOWS                
         EJECT                                                                  
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
RESW02   CSECT                                                                  
DUMPRELO NTR1  LABEL=*,BASE=*                                                   
         MVC   HALF,27(R3)                                                      
         LH    R8,HALF                                                          
         LA    R6,P+5                                                           
         LH    R4,=H'60'           PRINT IN GROUPS OF 60                        
DUMP5    CR    R8,R4                                                            
         BNL   DUMP10                                                           
         LR    R4,R8                                                            
DUMP10   GOTO1 HEXOUT,DMCB,(R3),(R6),(R4)                                       
         GOTO1 REPORT                                                           
         LA    R3,60(R3)                                                        
         SR    R8,R4                                                            
         BNZ   DUMP5                                                            
         GOTO1 REPORT              SPACING LINE                                 
*                                                                               
*   NOW DISPLAY NON-HEX RECORD                                                  
*                                                                               
         L     R3,AREC             RESET A(INPUT RECORD)                        
         MVC   HALF,27(R3)                                                      
         LH    R8,HALF                                                          
         LA    R6,P+5                                                           
         LH    R4,=H'60'           PRINT IN GROUPS OF 60                        
DUMP50   CR    R8,R4                                                            
         BNL   DUMP100                                                          
         LR    R4,R8                                                            
DUMP100  EQU   *                                                                
         EX    R4,DUMP1000         MOVE BY LENGTH                               
         GOTO1 REPORT                                                           
         LA    R3,60(R3)                                                        
         SR    R8,R4                                                            
         BNZ   DUMP50                                                           
         GOTO1 REPORT              SPACING LINE                                 
*                                                                               
         XIT1                                                                   
*                                                                               
DUMP1000 MVC   P+5(0),0(R3)        MOVE BY LENGTH                               
         LTORG                                                                  
         EJECT                                                                  
***>>    NTHRELO                                                                
*                                                                               
*  NTHRELO: INITIALIZATION AND COUNTER CODE MOVED OUT TO REGAIN                 
*    ADDRESSABILITY                                                             
*                                                                               
NTHRELO  NTR1  LABEL=*,BASE=*                                                   
         USING FIXD,R4                                                          
*                                                                               
         CLI   STACHA,X'02'        PRINT OUT EVERY STATION,                     
         BNE   *+10                                                             
         ZAP   STANTH,=P'1'                                                     
         CLI   SALCHA,X'06'        AND SALESPERSON                              
         BNE   *+10                                                             
         ZAP   SALNTH,=P'1'                                                     
         CLI   ADVCHA,X'08'        AND ADVERTISER                               
         BNE   *+10                                                             
         ZAP   ADVNTH,=P'1'                                                     
         CLI   PRDCHA,X'09'        AND PRODUCT                                  
         BNE   *+10                                                             
         ZAP   PRDNTH,=P'1'                                                     
         CLI   AGYCHA,X'0A'        AND AGENCY                                   
         BNE   *+10                                                             
         ZAP   AGYNTH,=P'1'                                                     
         CLI   BUDCHA,X'13'        AND BUDGET                                   
         BNE   *+10                                                             
         ZAP   BUDNTH,=P'1'                                                     
         CLI   SDDCHA,X'26'        AND SDD RECORD CHANGED                       
         BNE   *+10                                                             
         ZAP   SDDNTH,=P'1'                                                     
         CLI   COMCHA,X'29'        AND COMMISSION REC                           
         BNE   *+10                                                             
         ZAP   COMNTH,=P'1'                                                     
         CLI   DARCHA,X'41'        AND DARE REC                                 
         BNE   *+10                                                             
         ZAP   DARNTH,=P'500'                                                   
         CLI   MKGCHA,X'11'        AND MAKEGOOD REC                             
         BNE   *+10                                                             
         ZAP   MKGNTH,=P'500'                                                   
         CLI   SETCHA,X'38'        AND SET REC                                  
         BNE   *+10                                                             
         ZAP   SETNTH,=P'1'                                                     
         CLI   DSPCHA,X'3A'        AND DEVSAL REC                               
         BNE   *+10                                                             
         ZAP   DSPNTH,=P'1'                                                     
         CLI   PTPCHA,X'31'        AND POINT PERSON REC                         
         BNE   *+10                                                             
         ZAP   PTPNTH,=P'1'                                                     
         CLI   EOPCHA,X'1B'        AND EOP REC (ALL TYPES INCLUDED)             
         BNE   *+10                                                             
         ZAP   EOPNTH,=P'1'                                                     
         CLI   STRCHA,X'39'        AND STRATEGY REC                             
         BNE   *+10                                                             
         ZAP   STRNTH,=P'1'                                                     
         CLI   DIRCHA,X'35'        AND DIRECT RESPONSE REC                      
         BNE   *+10                                                             
         ZAP   DIRNTH,=P'1'                                                     
         CLI   PRPCHA,X'43'        AND PROPOSAL REC                             
         BNE   *+10                                                             
         ZAP   PRPNTH,=P'1'                                                     
         CLI   SLWCHA,X'43'        AND SELLERS WORKSHEET FOR WINDOWS            
         BNE   *+10                                                             
         ZAP   SLWNTH,=P'1'                                                     
         SPACE 1                                                                
         CLI   BUYCHA,X'0B'        PRINT EVERY 500TH BUY                        
         BNE   *+10                                                             
         ZAP   BUYNTH,=P'500'                                                   
         CLI   CONCHA,X'0C'        PRINT EVERY 100TH CONTRACT                   
         BNE   *+10                                                             
         ZAP   CONNTH,=P'100'                                                   
         CLI   ATNCHA,X'27'        AND ATHENA RECORD CHANGED                    
         BNE   *+10                                                             
         ZAP   ATNNTH,=P'500'                                                   
         SPACE 1                                                                
         CLI   AURCHA,X'2C'        AND A.U.R. RECORD CHANGED                    
         BNE   *+10                                                             
         ZAP   AURNTH,=P'500'                                                   
         SPACE 1                                                                
         CLI   INVCHA,RINVKTYQ     PRINT EVERY 200TH INVENTORY                  
         BNE   *+10                                                             
         ZAP   INVNTH,=P'200'                                                   
         SPACE 1                                                                
         CLI   ACLCHA,X'20'        PRINT EVERY ACL RECORD                       
         BNE   *+10                                                             
         ZAP   ACLNTH,=P'1'                                                     
         CLI   BOXCHA,X'45'        PRINT EVERY BOXID RECORD                     
         BNE   *+10                                                             
         ZAP   BOXNTH,=P'1'                                                     
         SPACE 1                                                                
         CLI   TKOCHA,X'1F'        PRINT EVERY TKO RECORD                       
         BNE   *+10                                                             
         ZAP   TKONTH,=P'1'                                                     
         CLI   DROCHA,X'4C'        PRINT EVERY DRO RECORD                       
         BNE   *+10                                                             
         ZAP   DRONTH,=P'1'                                                     
         SPACE 1                                                                
         ZAP   DUMNTH,=P'1'        INITIALIZE DUMMY NTH TO SOME VALUE           
         SPACE 1                                                                
         ZAP   STACNT,=P'0'        INITIALIZE CNT FIELDS TO PACKED ZERO         
         ZAP   CONCNT,=P'0'                                                     
         ZAP   BUYCNT,=P'0'                                                     
         ZAP   INVCNT,=P'0'                                                     
         ZAP   BUDCNT,=P'0'                                                     
         ZAP   ATNCNT,=P'0'                                                     
         ZAP   AURCNT,=P'0'                                                     
         ZAP   ADVCNT,=P'0'                                                     
         ZAP   PRDCNT,=P'0'                                                     
         ZAP   SDDCNT,=P'0'                                                     
         ZAP   AGYCNT,=P'0'                                                     
         ZAP   SALCNT,=P'0'                                                     
         ZAP   COMCNT,=P'0'                                                     
         ZAP   DARCNT,=P'0'                                                     
         ZAP   MKGCNT,=P'0'                                                     
         ZAP   SETCNT,=P'0'                                                     
         ZAP   DSPCNT,=P'0'                                                     
         ZAP   PTPCNT,=P'0'                                                     
         ZAP   EOPCNT,=P'0'                                                     
         ZAP   STRCNT,=P'0'                                                     
         ZAP   DIRCNT,=P'0'                                                     
         ZAP   PRPCNT,=P'0'                                                     
         ZAP   SLWCNT,=P'0'                                                     
         ZAP   DUMCNT,=P'0'                                                     
         ZAP   ACLCNT,=P'0'                                                     
         ZAP   BOXCNT,=P'0'                                                     
         ZAP   TKOCNT,=P'0'                                                     
         ZAP   DROCNT,=P'0'                                                     
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  R4                                                               
***>>    NTHRELO                                                                
         EJECT                                                                  
*                                                                               
*  CONCOMBO:  IF STATION IS BEING CHANGED, THE CONTRACT MAY BE                  
*    PARTICIPATING IN A COMBO ORDER, AND THE STATION BEING CHANGED              
*    MAY BE REFERENCED WITHIN THE BODY OF THE ORDER ITSELF, AND NOT             
*    IN THE KEY.                                                                
*    R4  ->  TABLE ENTRY                                                        
*                                                                               
CONCOMBO NTR1  LABEL=*,BASE=*                                                   
         USING FIXD,R4                                                          
         L     R3,0(R1)                                                         
         USING RECD,R3                                                          
*                                                                               
         CLC   NSTA,SPACES         ANY NEW STATION?                             
         BE    CONC0030            NO  - NOTHING TO DO                          
         LA    R6,RCONELEM         LOOK FOR COMBO CONTROL ELT                   
         MVI   ELCODE,X'17'        COMBO CONTROL ELEMENT CODE                   
         BAS   RE,NEXTEL                                                        
         BNE   CONC0030            NOT FOUND - EXIT                             
*******  LA    R5,CONCNT           **TEST**                                     
******** BAS   RE,DMPGET           **TEST**                                     
         SPACE 1                                                                
*                                                                               
*  CALCULATE # OF BUCKETS IN COMBO BY DIVIDING THE LENGTH OF THE                
*    COMBO CONTROL ELEMENT (MINUS ELEMENT CODE AND LENGTH BYTES)                
*    BY LENGTH OF ONE COMBO CONTRACT ENTRY.                                     
*                                                                               
         ZIC   RF,1(R6)            LENGTH OF ELEMENT                            
         LA    RE,2                MINUS L(CODE AND LENGTH BYTES)               
         SR    RF,RE                                                            
         SR    RE,RE                                                            
         LA    R0,9                L(CONTROL ELEMENT)                           
         DR    RE,R0               LENGTH / SIZE = # ENTRIES                    
*                                    RF = # ENTRIES                             
         LA    R6,2(R6)            SKIP ELEMENT CONTROL BYTES                   
CONC0010 EQU   *                                                                
         CLC   0(5,R6),FSTA        SAME AS OLD STATION?                         
         BE    CONC0020            YES - UPDATE IT                              
         LA    R6,9(R6)            NO  - BUMP TO NEXT COMBO ELEMENT             
         BCT   RF,CONC0010         GO BACK FOR NEXT                             
         B     CONC0030            FINISHED                                     
CONC0020 EQU   *                                                                
         MVC   0(5,R6),NSTA        INSERT NEW STATION CALL LETTERS              
*                                    AND CONSIDER IT FINISHED.                  
*****    LA    R5,CONCNT           **TEST**                                     
*****    BAS   RE,DMPPUT           **TEST**                                     
CONC0030 EQU   *                                                                
         XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*    COMPSP:                                                                    
*        IF REP OF ORDER IS USING COMP S/P                                      
*           IF OPTION IS COMP S/P -> NEW, SET TO NEW                            
*           IF OPTION IS COMP S/P -> HSE, SET TO HOUSE ACCOUNT                  
*    R4 -> A(SWITCH REQUEST ENTRY)                                              
*                                                                               
COMPSP   NTR1  BASE=*,LABEL=*                                                   
         L     R8,AREC                                                          
         USING RECD,R8                                                          
         USING FIXD,R4                                                          
         L     RF,=A(COMPREPS)                                                  
         A     RF,RELO             RF = A(REPS USING COMP S/P)                  
COSP0020 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    COSP0800            YES - NOT IN TABLE                           
         CLC   0(2,RF),=C'  '      EMPTY SLOT?                                  
         BNH   COSP0800            YES - NOT IN TABLE                           
         CLC   0(2,RF),RCONKREP    CONTRACT REP IN TABLE?                       
         BE    COSP0040            YES -                                        
         LA    RF,2(RF)            NO  - BUMP TO NEXT SLOT                      
         B     COSP0020            GO BACK FOR NEXT                             
COSP0040 EQU   *                                                                
         LA    R2,RCONELEM         SET A(DESCRIPTOR ELEMENT)                    
COSP0060 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BNE   COSP0070            NO                                           
*                                  YES - INSERT A BLANK 1E ELEMENT              
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,NEW1EELT,0             
         B     COSP0040            GO BACK AND PROCESS REC AGAIN                
*                                                                               
COSP0070 EQU   *                                                                
         CLI   0(R2),X'1E'         RANDOM FLAG ELEMENT?                         
         BE    COSP0080            YES - PROCESS                                
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     COSP0060            GO BACK FOR NEXT                             
         USING RCONRFEL,R2                                                      
COSP0080 EQU   *                                                                
*                                                                               
*   TEST                                                                        
**       MVC   P+1(08),=C'SWFLAGS:'                                             
**       MVC   P+10(1),SWFLAGS                                                  
**       MVC   P+12(5),DUB                                                      
**       MVC   P+18(3),RCONSAL                                                  
**       MVC   P+22(3),RCONKOFF                                                 
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   RCONRPSP,SPACES     ANY VALUE IN FIELD?                          
         BH    COSP0090            YES - USE IT                                 
         MVC   RCONRPSP(5),DUB     NO  - SET S/P OF RECORD                      
*                                     ORIGINAL VALUES SAVED IN DUB              
COSP0090 EQU   *                                                                
         TM    SWFLAGS,X'20'       COMP S/P -> NEW S/P?                         
         BNO   COSP0100                                                         
         MVC   RCONRPSP,RCONSAL    INSERT NEW S/P                               
         MVC   RCONRSPO,RCONKOFF   INSERT NEW S/P OFFICE                        
         B     COSP0800                                                         
COSP0100 EQU   *                                                                
         TM    SWFLAGS,X'10'       COMP S/P -> HOUSE ACCOUNT?                   
         BNO   COSP0120            NO  - DEFAULT SET: NO CHANGE                 
         MVI   RCONRPSP,C'$'       YES - BUILD HOUSE ACCOUNT                    
         MVC   RCONRPSP+1(2),FOFF  INSERT ORIGINAL S/P OFFICE                   
*                                                                               
         B     COSP0800                                                         
COSP0120 EQU   *                                                                
         CLC   RCONRPSP,SPACES     VALUE SET?                                   
         BH    COSP0800            YES                                          
         MVC   RCONRPSP(3),FSAL    NO  - INSERT ORIGINAL S/P                    
         MVC   RCONRSPO(2),FOFF    INSERT ORIGINAL OFFICE                       
*                                                                               
*                                                                               
COSP0800 EQU   *                                                                
         XIT1                                                                   
NEW1EELT DC    X'1E16'                                                          
         DC    20X'00'                                                          
         DS    0F                                                               
         DROP  R2,R4,R8                                                         
         EJECT                                                                  
*                                                                               
*    CHKCMPSP:  CHECK PROFILE TO SEE IF COMPENSATION S/P IS BEING               
*        USED BY EACH REP.                                                      
*                                                                               
CHKCMPSP NTR1  BASE=*,LABEL=*                                                   
         L     R8,AREC                                                          
         USING RECD,R8                                                          
         LA    R3,RREPELEM         X'01' ELEMENT                                
CCMP0020 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    CCMP0800            YES - NO PROFILE ELEMENT                     
         CLI   0(R3),4             PROGRAM PROFILE ELEMENT?                     
         BE    CCMP0040            YES                                          
         ZIC   RE,1(R3)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R3,RE                                                            
         B     CCMP0020                                                         
CCMP0040 EQU   *                                                                
         ZIC   RF,2(R3)            NUMBER OF PROFILE ENTRIES                    
         LA    R3,4(R3)            SET A(1ST PROFILE)                           
CCMP0060 EQU   *                                                                
         CLI   0(R3),RREPQCNT      CONTRACT PROFILE?                            
         BE    CCMP0080            YES - TEST PROPER SETTING                    
         LA    R3,RREPPGML(R3)     NO  - BUMP TO NEXT PROFILE                   
         BCT   RF,CCMP0060         GO BACK FOR NEXT                             
         B     CCMP0800            NO CONTRACT PROFILE                          
CCMP0080 EQU   *                                                                
         TM    8(R3),X'80'         PAY S/P PROFILE SET?                         
         BNO   CCMP0800            NO  - EXIT                                   
         L     RF,=A(COMPREPS)                                                  
         A     RF,RELO             RF = A(REPS USING COMP S/P)                  
CCMP0100 EQU   *                                                                
         CLI   0(RF),X'FF'         TABLE FULL?                                  
         BNE   *+6                                                              
         DC    H'0'                YES - MUST KILL JOB                          
         OC    0(2,RF),0(RF)       ANY ENTRY IN SLOT?                           
         BZ    CCMP0120            NO                                           
         LA    RF,2(RF)            YES - BUMP TO NEXT SLOT                      
         B     CCMP0100            GO BACK FOR NEXT SLOT                        
CCMP0120 EQU   *                                                                
         MVC   0(2,RF),RREPKREP    INSERT CODE INTO TABLE                       
CCMP0800 EQU   *                                                                
         XIT1                                                                   
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
*   TIMSTAMP - RETRIEVE TIME.  DISPLAY AN INDICATOR EVERY X RECORDS.            
*        TIME + NUMBER TAPE RECORDS READ SINCE LAST STAMP, RESET                
*        COUNTER.                                                               
*                                                                               
TIMSTAMP NTR1  BASE=*,LABEL=*                                                   
         L     RF,TIMECTR2         INCREMENT TOTAL COUNTER                      
         LA    RF,1(RF)                                                         
         ST    RF,TIMECTR2                                                      
         L     RF,TIMECNTR         INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,TIMECNTR                                                      
         C     RF,=F'25000'        25000 TAPE RECORDS READ?                     
***      C     RF,=F'01000'        01000 TAPE RECORDS READ?                     
         BL    TSTA0100            NO  - EXIT                                   
         XC    TIMECNTR,TIMECNTR   YES - DISPLAY STAMP                          
         TIME  DEC                                                              
         ST    R0,TIMEWORK                                                      
         GOTO1 HEXOUT,DMCB,TIMEWORK,TIMEWORK+4,4,=C'TOG'                        
         MVC   P+1(30),=C'25000 TAPE RECORDS READ: TIME '                       
***      MVC   P+1(30),=C'01000 TAPE RECORDS READ: TIME '                       
         MVC   P+32(2),TIMEWORK+4  INSERT TIME                                  
         MVI   P+34,C':'                                                        
         MVC   P+35(2),TIMEWORK+6  INSERT TIME                                  
         MVI   P+37,C':'                                                        
         MVC   P+38(2),TIMEWORK+8  INSERT TIME                                  
         MVI   P+40,C':'                                                        
         MVC   P+41(2),TIMEWORK+10 INSERT TIME                                  
         MVC   P+50(16),=C'TOTAL RECS READ:'                                    
         EDIT  TIMECTR2,(10,P+70)                                               
         GOTO1 REPORT                                                           
         MVC   P+1(10),=C'      KEY='                                           
         L     RF,AREC                                                          
         MVC   P+11(27),0(RF)                                                   
         GOTO1 REPORT                                                           
TSTA0100 EQU   *                                                                
         XIT1                                                                   
                                                                                
TIMECNTR DS    F                                                                
TIMECTR2 DS    F                                                                
TIMEWORK DS    6F                                                               
         LTORG                                                                  
*                                                                               
*   TKONMOD1:  RELOCATED ROUTINE FOR ADDRESSABILITY                             
*        THREE OF THE SIX TYPES OF TKO SUBRECORDS ARE CONSIDERED:               
*        TYPE 1  = AGENCY/AGYOFF                                                
*        TYPE 2  = ADVERTISER                                                   
*        TYPE 3  = SALESPERSON                                                  
*        TYPES 4-6 ARE PASSED THROUGH UNCHECKED.                                
*                                                                               
TKONMOD1 NMOD1 0,*TKOV*                                                         
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
*                                                                               
         LR    RC,R1                                                            
         L     R3,AREC                                                          
*                                                                               
         L     R4,ABLDAREA                                                      
         LA    R5,TKOCNT                                                        
***>>>   BAS   RE,DMPIPTKO                                                      
***<<<                                                                          
TKOV0020 EQU   *                                                                
         LA    R5,TKOCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    TKOV0300                                                         
         CLC   RTKOKREP,FREP                                                    
         BNE   TKOV0040                                                         
         CLI   RTKOKRTP,1          TKO AGY RECORD?                              
         BNE   TKOV0100            NO  - CHECK NEXT TYPE                        
         CLC   FAGY,SPACES         YES - AGY CODE BEING CHANGED?                
         BE    TKOV0040            NO                                           
         CLC   RTKOEQIV(6),FAGY    YES - THIS CODE IN RECORD?                   
         BNE   TKOV0040            NO                                           
         SPACE 1                                                                
         CLI   TKOCHA,X'1F'        THIS CODE TO BE CHANGED?                     
         BE    TKOV0060            YES                                          
         SPACE 1                                                                
TKOV0040 LA    R4,LENFIXT(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     TKOV0020                                                         
         SPACE 1                                                                
TKOV0060 EQU   *                                                                
         CLC   NAGY,SPACES         ANY NEW AGENCY CODE?                         
         BE    TKOV0300            NO                                           
*                                                                               
*   CHECK FOR FILTERS:                                                          
*                                                                               
         CLC   FSTA,SPACES         STATION FILTER?                              
         BNE   TKOV0300            YES                                          
         CLC   FOFF,SPACES         OFFICE  FILTER?                              
         BNE   TKOV0300            YES                                          
         CLC   FGRP,SPACES         GROUP   FILTER?                              
         BNE   TKOV0300            YES                                          
         CLC   FSAL,SPACES         S/P     FILTER?                              
         BNE   TKOV0300            YES                                          
         CLC   FADV,SPACES         ADVERT  FILTER?                              
         BNE   TKOV0300            YES                                          
         MVC   RTKOEQIV,NAGY       NO  - INSERT NEW AGENCY/AOFFICE              
TKOV0080 LA    R5,TKOCNT                                                        
***>>>   BAS   RE,DMOT0040                                                      
         B     TKOV0300                                                         
TKOV0100 EQU   *                                                                
         CLI   RTKOKRTP,2          TKO ADV RECORD?                              
         BNE   TKOV0200            NO  - CHECK NEXT TYPE                        
         CLC   FADV,SPACES         YES - ADV CODE BEING CHANGED?                
         BE    TKOV0140            NO                                           
         CLC   RTKOEQIV(4),FADV    YES - THIS CODE IN RECORD?                   
         BNE   TKOV0140            NO                                           
         SPACE 1                                                                
         LA    R5,TKOCNT           YES                                          
         CLI   TKOCHA,X'1F'        THIS CODE TO BE CHANGED?                     
         BE    TKOV0160            YES                                          
         SPACE 1                                                                
TKOV0140 LA    R4,LENFIXT(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     TKOV0020                                                         
         SPACE 1                                                                
TKOV0160 EQU   *                                                                
         CLC   NADV,SPACES         ANY NEW ADV CODE?                            
         BE    TKOV0300            NO                                           
*                                                                               
*   CHECK FOR FILTERS:                                                          
*                                                                               
         CLC   FSTA,SPACES         STATION FILTER?                              
         BNE   TKOV0300            YES                                          
         CLC   FOFF,SPACES         OFFICE  FILTER?                              
         BNE   TKOV0300            YES                                          
         CLC   FGRP,SPACES         GROUP   FILTER?                              
         BNE   TKOV0300            YES                                          
         CLC   FSAL,SPACES         S/P     FILTER?                              
         BNE   TKOV0300            YES                                          
         CLC   FTEAM,SPACES        TEAM    FILTER?                              
         BNE   TKOV0300            YES                                          
         MVC   RTKOEQIV(4),NADV    NO  - INSERT NEW ADV                         
TKOV0180 LA    R5,TKOCNT                                                        
***>>>   BAS   RE,DMOT0040                                                      
         B     TKOV0300                                                         
TKOV0200 EQU   *                                                                
         CLI   RTKOKRTP,3          TKO S/P RECORD?                              
         BNE   TKOV0300            NO  - KEEP ALL OTHER RECTYPES                
         CLC   FSAL,SPACES         YES - S/P CODE BEING CHANGED?                
         BE    TKOV0240            NO                                           
         CLC   RTKOEQIV(3),FSAL    YES - THIS CODE IN RECORD?                   
         BNE   TKOV0240            NO                                           
         SPACE 1                                                                
         CLI   TKOCHA,X'1F'        THIS CODE TO BE CHANGED?                     
         BE    TKOV0260            YES                                          
         SPACE 1                                                                
TKOV0240 LA    R4,LENFIXT(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     TKOV0020                                                         
         SPACE 1                                                                
TKOV0260 EQU   *                                                                
         CLC   NSAL,SPACES         ANY NEW S/P CODE?                            
         BE    TKOV0300            NO                                           
*                                                                               
*   CHECK FOR FILTERS:                                                          
*                                                                               
         CLC   FSTA,SPACES         STATION FILTER?                              
         BNE   TKOV0300            YES                                          
         CLC   FGRP,SPACES         GROUP   FILTER?                              
         BNE   TKOV0300            YES                                          
         CLC   FADV,SPACES         ADVERT  FILTER?                              
         BNE   TKOV0300            YES                                          
         CLC   FAGY,SPACES         AGENCY  FILTER?                              
         BNE   TKOV0300            YES                                          
         MVC   RTKOEQIV(3),NSAL    NO  - INSERT NEW S/P                         
         MVC   RTKOEQIV+3(2),NOFF  INSERT NEW S/P OFFICE                        
TKOV0280 LA    R5,TKOCNT                                                        
***>>>   BAS   RE,DMOT0040                                                      
         B     TKOV0300                                                         
TKOV0300 EQU   *                                                                
         XIT1                      ALL OTHER X'1F' RECS ARE KEPT                
         EJECT                                                                  
**-->                                                                           
DMPIPTKO NTR1                                                                   
         AP    0(4,R5),=P'1'                                                    
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BE    DMIT0020            YES - DISPLAY ALL                            
         ZAP   DUB,0(4,R5)                                                      
         DP    DUB,4(4,R5)                                                      
         CP    DUB+4(4),=P'0'                                                   
         BNE   DMIT0200                                                         
DMIT0020 EQU    *                                                               
         MVC   P(3),=C'GET'                                                     
         LA    R6,P+5                                                           
         B     DMIT0080                                                         
         SPACE 1                                                                
DMOT0040 NTR1                                                                   
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BE    DMOT0060            YES - DISPLAY ALL                            
         ZAP   DUB,0(4,R5)                                                      
         DP    DUB,4(4,R5)                                                      
         CP    DUB+4(4),=P'0'                                                   
         BNE   DMIT0200                                                         
DMOT0060 EQU   *                                                                
         MVC   P(3),=C'PUT'                                                     
         SPACE 1                                                                
DMIT0080 MVC   HALF,27(R3)                                                      
         LH    R8,HALF                                                          
         LA    R6,P+5                                                           
         LH    R4,=H'60'           PRINT IN GROUPS OF 60                        
DMIT0100 CR    R8,R4                                                            
         BNL   DMIT0120                                                         
         LR    R4,R8                                                            
DMIT0120 GOTO1 HEXOUT,DMCB,(R3),(R6),(R4)                                       
         GOTO1 REPORT                                                           
         LA    R3,60(R3)                                                        
         SR    R8,R4                                                            
         BNZ   DMIT0100                                                         
         GOTO1 REPORT              SPACING LINE                                 
*                                                                               
*   NOW DISPLAY NON-HEX RECORD                                                  
*                                                                               
         L     R3,AREC             RESET A(INPUT RECORD)                        
         MVC   HALF,27(R3)                                                      
         LH    R8,HALF                                                          
         LA    R6,P+5                                                           
         LH    R4,=H'60'           PRINT IN GROUPS OF 60                        
DMIT0140 CR    R8,R4                                                            
         BNL   DMIT0160                                                         
         LR    R4,R8                                                            
DMIT0160 EQU   *                                                                
         EX    R4,DMIT0180         MOVE BY LENGTH                               
         GOTO1 REPORT                                                           
         LA    R3,60(R3)                                                        
         SR    R8,R4                                                            
         BNZ   DMIT0140                                                         
         GOTO1 REPORT              SPACING LINE                                 
         B     DMIT0200                                                         
DMIT0180 MVC   P+5(0),0(R3)        MOVE BY LENGTH                               
DMIT0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***????                                                                         
*                                                                               
*                                                                               
*   DROAMOD1:  RELOCATED ROUTINE FOR ADDRESSABILITY                             
*                                                                               
DROAMOD1 NMOD1 0,*DROADV*                                                       
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
*                                                                               
         LR    RC,R1                                                            
         L     R3,AREC                                                          
*                                                                               
*   TEST DISPLAY                                                                
         MVC   P+1(10),=C'4C RECORD:'                                           
         MVC   P+12(27),0(R3)                                                   
         GOTO1 REPORT                                                           
*                                                                               
         L     R4,ABLDAREA                                                      
*                                                                               
DROD0020 EQU   *                                                                
         LA    R5,DROCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    DROD0100                                                         
*                                                                               
         CLC   RDROKREP,FREP       FILTER ON REP                                
         BNE   DROD0030                                                         
         CLC   FSTA,SPACES         STATION CHANGE?                              
         BNE   DROD0030            YES - DON'T CHANGE DRO ADVERT                
*                                                                               
         CLI   DROCHA,X'4C'        ROM EOP CHANGE?                              
         BE    DROD0040                                                         
         SPACE 1                                                                
DROD0030 LA    R4,LENFIXT(R4)                                                   
         B     DROD0020                                                         
         SPACE 1                                                                
DROD0040 EQU   *                                                                
         CLI   DROCHA,X'4C'        ROM EOP ADV CHG? (ONLY IF NO FILTS)          
         BNE   DROD0030                                                         
         CLC   FADV,SPACES         ADV CHANGE?                                  
         BE    DROD0030            NO - CHECK NEXT REQUEST                      
*                                                                               
         OC    RDROEQIV(4),SPACES  CLEAR TO SPACES                              
         CLC   RDROEQIV,FADV       ADV TO BE CHANGED? BODY OF RECORD            
         BNE   DROD0030            NO - CHECK NEXT REQUEST                      
         CLC   NEWCODE(4),SPACES   ANY NEW ADV?                                 
         BE    DROD0030            NO  - CHECK NEXT REQUEST                     
         BAS   RE,DMPINMOD         DISPLAY THE ORIGINAL RECORD                  
         MVC   DROORIG,RDROEQIV    MOVE ORIG CODE TO AUDIT ELT                  
         GOTO1 DATCON,DMCB,(5,WORK),(3,DRODATE)                                 
*                                  INSERT TODAY'S DATE INTO AUDIT ELT           
         MVC   RDROEQIV,NEWCODE    YES - CHANGE CODE TO NEW                     
*                                                                               
***                                                                             
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RDROREC,DROELEM                
*                                                                               
***                                                                             
         LA    R5,DROCNT           ADD TO COUNT                                 
         BAS   RE,DMOTN040         DISPLAY THE REVISED RECORD                   
DROD0100 EQU   *                                                                
         XIT1                      KEEP THE MODIFIED RECORD                     
*                                                                               
DROELEM  DC    X'1009'             ELEMENT CODE/LENGTH                          
DRODATE  DC    X'000000'           DATE SWITCHED                                
DROORIG  DC    X'00000000'         ORIGINAL ADV CODE                            
*                                                                               
         EJECT                                                                  
**-->                                                                           
DMPINMOD NTR1                                                                   
         AP    0(4,R5),=P'1'                                                    
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BE    DMITN020            YES - DISPLAY ALL                            
         ZAP   DUB,0(4,R5)                                                      
         DP    DUB,4(4,R5)                                                      
         CP    DUB+4(4),=P'0'                                                   
         BNE   DMITN200                                                         
DMITN020 EQU    *                                                               
         MVC   P(3),=C'GET'                                                     
         LA    R6,P+5                                                           
         B     DMITN080                                                         
         SPACE 1                                                                
DMOTN040 NTR1                                                                   
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
         BE    DMOTN060            YES - DISPLAY ALL                            
         ZAP   DUB,0(4,R5)                                                      
         DP    DUB,4(4,R5)                                                      
         CP    DUB+4(4),=P'0'                                                   
         BNE   DMITN200                                                         
DMOTN060 EQU   *                                                                
         MVC   P(3),=C'PUT'                                                     
         SPACE 1                                                                
DMITN080 MVC   HALF,27(R3)                                                      
         LH    R8,HALF                                                          
         LA    R6,P+5                                                           
         LH    R4,=H'60'           PRINT IN GROUPS OF 60                        
DMITN100 CR    R8,R4                                                            
         BNL   DMITN120                                                         
         LR    R4,R8                                                            
DMITN120 GOTO1 HEXOUT,DMCB,(R3),(R6),(R4)                                       
         GOTO1 REPORT                                                           
         LA    R3,60(R3)                                                        
         SR    R8,R4                                                            
         BNZ   DMITN100                                                         
         GOTO1 REPORT              SPACING LINE                                 
*                                                                               
*   NOW DISPLAY NON-HEX RECORD                                                  
*                                                                               
         L     R3,AREC             RESET A(INPUT RECORD)                        
         MVC   HALF,27(R3)                                                      
         LH    R8,HALF                                                          
         LA    R6,P+5                                                           
         LH    R4,=H'60'           PRINT IN GROUPS OF 60                        
DMITN140 CR    R8,R4                                                            
         BNL   DMITN160                                                         
         LR    R4,R8                                                            
DMITN160 EQU   *                                                                
         EX    R4,DMITN180         MOVE BY LENGTH                               
         GOTO1 REPORT                                                           
         LA    R3,60(R3)                                                        
         SR    R8,R4                                                            
         BNZ   DMITN140                                                         
         GOTO1 REPORT              SPACING LINE                                 
         B     DMITN200                                                         
DMITN180 MVC   P+5(0),0(R3)        MOVE BY LENGTH                               
DMITN200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***>>>ROM EOP ADV RECORDS                                                       
***????                                                                         
***<<<                                                                          
*                                                                               
*                                                                               
*   MKGNMOD1:  RELOCATED ROUTINE FOR ADDRESSABILITY                             
*                                                                               
MKGNMOD1 NMOD1 0,*MKGD*                                                         
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
*                                                                               
         LR    RC,R1                                                            
         L     R3,AREC                                                          
*                                                                               
         L     R4,ABLDAREA                                                      
*                                                                               
*   TEST : ONLY DISPLAY ORDERS 3863999 AND 3864000                              
*                                                                               
         CLC   =X'00063169',RMKGKCON                                            
         BE    TEST0010                                                         
         CLC   =X'99953169',RMKGKCON                                            
         BNE   TEST0020                                                         
TEST0010 EQU   *                                                                
         MVC   P+1(08),=C'PRE-FIX:'                                             
         GOTO1 REPORT                                                           
         L     RF,=F'160'          LOAD DATA AREA LENGTH TO DUMP                
         GOTO1 =V(PRNTBL),DMCB,(0,(R3)),(R3),C'DUMP',(RF),=C'2D'                
TEST0020 EQU   *                                                                
*                                                                               
*   TEST END                                                                    
*                                                                               
         OC    RMKGKPLN(6),RMKGKPLN                                             
*                                       GROUP COMMENT RECORD?                   
         BNZ   MKGD0005                 NO  - DON'T KEEP                        
         MVC   OLDMGKEY,RMKGKEY         YES - SAVE KEY                          
         MVC   NEWMGKEY,RMKGKEY         SET NEW KEY = OLD KEY                   
MKGD0005 EQU   *                                                                
         LA    R5,MKGCNT                                                        
         OC    MKGCNT,MKGCNT       CHECK FOR INITIALIZATION                     
         BNZ   MKGD0010            ALREADY SET                                  
         ZAP   MKGCNT,=P'0'        NOT DONE - SET TO ZERO                       
MKGD0010 EQU   *                                                                
         BAS   RE,DMPINPUT         DUMP INPUT                                   
         MVI   SWFLAG,C'N'         SET 'SWITCH?' TO NO                          
*                                                                               
MKGD0020 EQU   *                                                                
         LA    R5,MKGCNT                                                        
         CLI   0(R4),X'FF'                                                      
         BE    MKGD0400                                                         
         CLC   RMKGKREP,FREP       FILTER ON REP                                
         BNE   MKGD0040                                                         
         CLI   MKGCHA,X'11'                                                     
         BE    MKGD0060                                                         
         SPACE 1                                                                
MKGD0040 LA    R4,LENFIXT(R4)                                                   
         B     MKGD0020                                                         
         SPACE 1                                                                
MKGD0060 EQU   *                                                                
         CLC   FSTA,SPACES         STATION CHANGE IN REQUEST?                   
         BE    MKGD0080            NO - CHECK NEXT FIELD                        
*                                                                               
         CLC   RMKGKSTA,FSTA       STATION TO BE CHANGED?                       
         BNE   MKGD0080            NO - CHECK NEXT FIELD                        
         CLC   NSTA,SPACES         ANY NEW STATION?                             
         BE    MKGD0080            NO - CHECK NEXT FIELD                        
         MVC   RMKGKSTA,NSTA       YES - INSERT NEW STATION                     
         MVI   SWFLAG,C'Y'         SET 'SWITCH?' TO YES                         
*                                                                               
MKGD0080 DS    0H                                                               
**       CLC   FOFF,SPACES         OFFICE CHANGE IN REQUEST?                    
**       BE    MKGD0100            NO - CHECK NEXT FIELD                        
**       CLC   RMKGKOFF,FOFF       OFFICE TO BE CHANGED?                        
**       BNE   MKGD0100            NO - CHECK NEXT FIELD                        
**       CLC   NOFF,SPACES         ANY NEW OFFICE?                              
**       BE    MKGD0100            NO - CHECK NEXT FIELD                        
**       MVC   RMKGKOFF,NOFF       YES - INSERT NEW OFFICE                      
**       MVI   SWFLAG,C'Y'         SET 'SWITCH?' TO YES                         
***>>>>                                                                         
MKGD0100 EQU   *                                                                
         NI    FLAGS,X'FF'-X'80'   NO X'0A' ELEM ENCOUNTERED YET                
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,RMKGXELQ     X'0A' SWITCH ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   MKGD0380            NO X'0A' ELEMS                               
         USING RMKGXEL,R6                                                       
         OI    FLAGS,X'80'         X'0A' ELEM ENCOUNTERED                       
*                                                                               
         CLC   FSTA,SPACES         ANY OLD STATION?                             
         BE    MKGD0120            NO  -                                        
         CLC   RMKGXSTA,FSTA       YES - RECORD STATION CHANGING?               
         BNE   *+12                NO                                           
         MVI   STAMATCH,C'Y'       SET PRIMARY STATION MATCH TO 'YES'           
         B     MKGD0120            PROCESS REST OF STATION INFO                 
*                                                                               
         CLC   NSTA,SPACES         ANY NEW STATION?                             
         BNE   MKGD0380            YES -                                        
         B     MKGD0040            NO  - GO BACK FOR NEXT TABLE                 
*                                                                               
MKGD0120 EQU   *                                                                
*                                                                               
         OC    STADATE,STADATE     START DATE FILTER?                           
         BZ    MKGD0130            NO  - NO CHECK                               
         CLC   RMKGXFLT(3),STADATE IS CON DATE BEFORE START DATE?               
***>>>DATE WITHIN CHECK                                                         
         BNL   MKGD0130            NO - ACCEPT IT                               
         TM    SWFLAGS,X'40'       START W/IN FLIGHT WANTED?                    
         BNO   MKGD0040            NO  - SKIP ORDER                             
         CLC   RMKGXFLT+3(3),STADATE                                            
*                                  YES - START DATE W/IN FLIGHT?                
         BL    MKGD0040            NO  - SKIP IT                                
MKGD0130 EQU   *                                                                
***>>>DATE WITHIN CHECK                                                         
*                                                                               
         CLC   FOFF,SPACES         OFFICE CHANGE?                               
         BE    *+14                NO OFFICE                                    
         CLC   RMKGXOFF,FOFF       SAME OFFICE?                                 
         BNE   MKGD0040            NO  -                                        
*                                                                               
         CLC   FGRP,SPACES         GROUP?                                       
         BE    MKGD0140            NO GROUP                                     
         CLC   RMKGXGRP,FGRP       GROUP MATCH?                                 
         BE    MKGD0140            YES                                          
         CLI   STAMATCH,C'Y'       NO, STATION MATCH?                           
         BNE   MKGD0040            NO, GROUP DOESN'T MATCH                      
*                                  YES, FORCE GROUP TO MATCH ANYWAY             
MKGD0140 EQU   *                                                                
         CLC   FSAL,SPACES         ANY SPERSON FILTER?                          
         BE    MKGD0160            NO                                           
         CLC   RMKGXSAL,FSAL       YES - SAME SALESPERSON?                      
         BNE   MKGD0040            NO  - SKIP IT                                
         B     MKGD0180            YES - DON'T CHECK THE TEAM                   
MKGD0160 EQU   *                                                                
         CLC   FTEAM,SPACES                                                     
         BE    *+14                                                             
         CLC   RMKGXTEM,FTEAM      TEAM                                         
         BNE   MKGD0040                                                         
MKGD0180 EQU   *                                                                
*                                                                               
         CLC   FDSP,SPACES                                                      
         BE    *+14                                                             
         CLC   RMKGXDSP,FDSP       DEVELOPMENTAL SALESPERSON                    
         BNE   MKGD0040                                                         
*                                                                               
         CLC   FADV,SPACES                                                      
         BE    *+14                                                             
         CLC   RMKGXADV,FADV       ADVERTISER                                   
         BNE   MKGD0040                                                         
         CLC   FAGY,SPACES                                                      
         BE    MKGD0220                                                         
         CLI   EXCLC,0             IF OLD CODE IS AGENCY                        
         BE    MKGD0200                                                         
         ZIC   RE,EXCLC            MAY BE COMPARING FOR 4 (AGENCY)              
         EX    RE,*+8                    OR 6 (AGENCY/OFFICE)                   
         B     *+10                                                             
         CLC   RMKGXAGY(0),FAGY    AGENCY                                       
         BNE   MKGD0040                                                         
         B     MKGD0220                                                         
MKGD0200 EQU   *                                                                
         CLC   RMKGXAGY,FAGY       IF FILTERING ON AGENCY                       
         BNE   MKGD0040                                                         
         CLC   FAGYOF,SPACES       CHECK AGENCY AND OFFICE                      
         BE    MKGD0220                                                         
         CLC   RMKGXAOF,FAGYOF                                                  
         BNE   MKGD0040                                                         
         SPACE 1                                                                
*                                                                               
MKGD0220 EQU   *                                                                
         CLC   NSTA,SPACES         STATION?                                     
         BE    MKGD0240                                                         
         MVC   RMKGXSTA,NSTA                                                    
         MVI   SWFLAG,C'Y'         SET 'SWITCH?' TO YES                         
*                                                                               
MKGD0240 EQU   *                                                                
*                                                                               
         CLC   NOFF,SPACES                                                      
         BE    MKGD0260                                                         
         MVC   RMKGXOFF,NOFF                                                    
         MVC   RMKGKOFF,NOFF       INSERT INTO KEY ALSO                         
         MVI   SWFLAG,C'Y'         SET 'SWITCH?' TO YES                         
MKGD0260 EQU   *                                                                
*                                                                               
         CLC   NGRP,SPACES                                                      
         BE    MKGD0280                                                         
         MVC   RMKGXGRP,NGRP                                                    
         MVI   SWFLAG,C'Y'         SET 'SWITCH?' TO YES                         
MKGD0280 EQU   *                                                                
*                                                                               
         CLC   NTEAM,SPACES                                                     
         BE    MKGD0300                                                         
         MVC   RMKGXTEM,NTEAM                                                   
         MVI   SWFLAG,C'Y'         SET 'SWITCH?' TO YES                         
MKGD0300 EQU   *                                                                
*                                                                               
         CLC   NDSP,SPACES                                                      
         BE    MKGD0320                                                         
         MVC   RMKGXDSP,NDSP       DEVELOPMENTAL SALESPERSON                    
         MVI   SWFLAG,C'Y'         SET 'SWITCH?' TO YES                         
MKGD0320 EQU   *                                                                
*                                                                               
         CLC   NADV,SPACES                                                      
         BE    MKGD0340                                                         
         MVC   RMKGXADV,NADV                                                    
         MVI   SWFLAG,C'Y'         SET 'SWITCH?' TO YES                         
MKGD0340 EQU   *                                                                
*                                                                               
         SPACE 1                                                                
         CLC   NAGY,SPACES                                                      
         BE    MKGD0360                                                         
*                                                                               
         MVI   SWFLAG,C'Y'         SET 'SWITCH?' TO YES                         
         MVC   RMKGXAGY(6),SPACES  RE-INITIALIZE AGENCY+OFFICE FIELD            
*                                                                               
         CLI   EXMVC,0             MUST BE THERE IF THERE'S NAGY                
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,EXMVC            MAY BE MOVING FOR 4 (AGENCY)                 
         EX    RE,*+8                    OR 6 (AGENCY/OFFICE)                   
         B     *+10                                                             
         MVC   RMKGXAGY(0),NAGY    AGENCY                                       
         SPACE 1                                                                
MKGD0360 EQU   *                                                                
*                                                                               
         CLC   NSAL,SPACES         SALESPERSON?                                 
         BE    MKGD0380                                                         
         MVC   RMKGXSAL,NSAL                                                    
         MVI   SWFLAG,C'Y'         SET 'SWITCH?' TO YES                         
         DROP  R6                                                               
MKGD0380 EQU   *                                                                
*                                                                               
         LA    R5,MKGCNT                                                        
         B     MKGD0040            GO BACK AND CHECK NEXT TABLE                 
*                                                                               
MKGD0400 EQU   *                                                                
         CLI   SWFLAG,C'Y'         RECORD CHANGED?                              
         BNE   MKGD0420            NO  - DON'T DISPLAY OUTPUT                   
         BAS   RE,DMOP0040                                                      
MKGD0420 EQU   *                                                                
         OC    RMKGKPLN(6),RMKGKPLN     GROUP COMMENT RECORD?                   
         BNZ   MKGD0440                 NO                                      
         MVC   NEWMGKEY,RMKGKEY         YES - SAVE KEY, WHICH MAY HAVE          
*                                          BEEN CHANGED (POSSIBLY NOT)          
         B     MKGD0500                 EXIT                                    
MKGD0440 EQU   *                                                                
         MVC   RMKGKEY(21),NEWMGKEY     DETAIL RECORD:                          
*                                          REPLACE HIGH-ORDER 21 BYTES          
*                                          TO AGREE WITH  (POSSIBLE)            
*                                          NEW KEY IN GROUP CMT REC             
MKGD0500 EQU   *                                                                
*                                                                               
*   TEST : ONLY DISPLAY ORDERS 3863999 AND 3864000                              
*                                                                               
         CLC   =X'00063169',RMKGKCON                                            
         BE    TEST0030                                                         
         CLC   =X'99953169',RMKGKCON                                            
         BNE   TEST0040                                                         
TEST0030 EQU   *                                                                
         MVC   P+1(09),=C'POST-FIX:'                                            
         GOTO1 REPORT                                                           
         L     RF,=F'160'          LOAD DATA AREA LENGTH TO DUMP                
         GOTO1 =V(PRNTBL),DMCB,(0,(R3)),(R3),C'DUMP',(RF),=C'2D'                
TEST0040 EQU   *                                                                
*                                                                               
*   TEST END                                                                    
*                                                                               
         XIT1                                                                   
*                                                                               
*   DON'T REALLY NEW OLDMGKEY.  MG RECS COME IN AS A GROUP: A                   
*        GROUP COMMENT RECORD, FOLLOWED BY THE DETAILS OF THAT                  
*        GROUP.  IF THE GROUP COMMENT RECORD, WHICH CONTAINS AN                 
*        X'0A' ELEMENT PERMITTING CHANGES BASED ON ITEMS THAT ARE               
*        NOT IN THE KEY, IS CHANGED, THE DETAIL RECORDS ALSO HAVE               
*        TO CHANGE IN SYNCH.  THE DETAILS DON'T HAVE THE X'0A'                  
*        ELEMENT, SO THEY AREN'T CHANGED BY THE LOGIC ABOVE.                    
*        NEW CODE FORCES THE DETAILS TO AGREE WITH THE GROUP                    
*        COMMENT KEY.   BILL UHR.   MARCH1/02.                                  
*                                                                               
OLDMGKEY DS    CL27                     STORE OLD KEY:  MG GRP COMMENT          
NEWMGKEY DS    CL27                     STORE NEW KEY:  MG GRP COMMENT          
         DC    0H                                                               
         EJECT                                                                  
**-->                                                                           
DMPINPUT NTR1                                                                   
***>>>   AP    0(4,R5),=P'1'                                                    
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
***>>>   BE    DMIP0020            YES - DISPLAY ALL                            
         B     DMIP0200            DON'T DISPLAY ANY                            
*                                                                               
         ZAP   DUB,0(4,R5)                                                      
         DP    DUB,4(4,R5)                                                      
         CP    DUB+4(4),=P'0'                                                   
         BNE   DMIP0200                                                         
DMIP0020 EQU    *                                                               
         MVC   P(3),=C'GET'                                                     
         LA    R6,P+5                                                           
         B     DMIP0080                                                         
         SPACE 1                                                                
DMOP0040 NTR1                                                                   
         CLC   =C'$$TEST',QUESTOR  TEST RUN?                                    
*****    BE    DMOP0060            YES - DISPLAY ALL                            
         XIT1                                                                   
         ZAP   DUB,0(4,R5)                                                      
         DP    DUB,4(4,R5)                                                      
         CP    DUB+4(4),=P'0'                                                   
         BNE   DMIP0200                                                         
DMOP0060 EQU   *                                                                
         MVC   P(3),=C'PUT'                                                     
         SPACE 1                                                                
DMIP0080 MVC   HALF,27(R3)                                                      
         LH    R8,HALF                                                          
         LA    R6,P+5                                                           
         LH    R4,=H'60'           PRINT IN GROUPS OF 60                        
DMIP0100 CR    R8,R4                                                            
         BNL   DMIP0120                                                         
         LR    R4,R8                                                            
DMIP0120 GOTO1 HEXOUT,DMCB,(R3),(R6),(R4)                                       
         GOTO1 REPORT                                                           
         LA    R3,60(R3)                                                        
         SR    R8,R4                                                            
         BNZ   DMIP0100                                                         
         GOTO1 REPORT              SPACING LINE                                 
*                                                                               
*   NOW DISPLAY NON-HEX RECORD                                                  
*                                                                               
         L     R3,AREC             RESET A(INPUT RECORD)                        
         MVC   HALF,27(R3)                                                      
         LH    R8,HALF                                                          
         LA    R6,P+5                                                           
         LH    R4,=H'60'           PRINT IN GROUPS OF 60                        
DMIP0140 CR    R8,R4                                                            
         BNL   DMIP0160                                                         
         LR    R4,R8                                                            
DMIP0160 EQU   *                                                                
         EX    R4,DMIP0180         MOVE BY LENGTH                               
         GOTO1 REPORT                                                           
         LA    R3,60(R3)                                                        
         SR    R8,R4                                                            
         BNZ   DMIP0140                                                         
         GOTO1 REPORT              SPACING LINE                                 
         B     DMIP0200                                                         
DMIP0180 MVC   P+5(0),0(R3)        MOVE BY LENGTH                               
DMIP0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
**-->                                                                           
         LTORG                                                                  
         EJECT                                                                  
***>>>>                                                                         
*                                                                               
*                                                                               
*- NEXTREP -- FIND 1ST/NEXT SUBSIDIARY REP CODE                                 
*         IF REQUEST COMES IN AS A MASTER REP CODE, THE REQUEST                 
*         ITSELF IS PROPAGATED FOR EACH OF THE SUBREPS.  THIS ENSURES           
*         THAT THE CODE IS CHANGED FOR EACH OF THE SUBREPS SERVICED             
*         BY THE MASTER REP.                                                    
*                                                                               
*  INPUT: MASTREP - 0 ON 1ST PASS.  MAINTAINED INTERNALLY                       
*         RSWIKREP- MASTER REP CODE ON 1ST PASS.                                
*                                                                               
*  RETURN: RSWIKREP - NEXT REP CODE OR X'00' = NO MORE REPS                     
*                                                                               
NEXTREP  NMOD1 0,*NEXTRP*                                                       
         LR    RC,R1                                                            
*         L     R3,=A(REC)                                                      
         L     R3,AREC                                                          
*         A     R3,RELO                                                         
         USING RSWIKEY,R3                                                       
*                                                                               
         MVC   P(12),=C'NEXTREP INPT'                                           
         MVC   P+14(2),RSWIKREP                                                 
*                                                                               
         ICM   R2,15,MASTREP       A(MASTER REP ENTRY)                          
         BNZ   NREP50              2ND+ PASS.                                   
*                                                                               
         L     R2,=A(MASTLIST)     1ST PASS: SET A(MASTER TABLE)                
         A     R2,RELO             SET ADDRESSABILITY                           
*                                                                               
NREP20   CLI   0(R2),0             ANY ENTRY?                                   
         BE    NREP99              NO   - NO MORE MASTER REPS                   
         CLC   RSWIKREP,0(R2)      IS THIS A MASTER REP?                        
         BE    NREP30              YES - USE THIS MASTER/SUB SET                
         AH    R2,2(R2)            NO  - BUMP BY LENGTH OF SET                  
         B     NREP20              GO BACK FOR NEXT SET                         
*                                                                               
NREP30   ST    R2,MASTREP          SAVE A(MASTER REP)                           
         LA    R4,6(R2)            SET A(1ST SUB REP)                           
         B     NREP80              PROCESS SET                                  
*                                                                               
NREP50   EQU   *                                                                
         L     R4,SUBREP           RESET A(CURRENT SUB REP)                     
         LA    R4,2(R4)            BUMP TO NEXT REP                             
         A     R2,2(R2)            SET A(NEXT MASTER/SUB SET)                   
         CR    R4,R2               END OF CURRENT SET REACHED?                  
         BL    NREP80              NO  - PROCESS THIS REP                       
         B     NREP99              YES - NO MORE REPS                           
*                                                                               
NREP80   ST    R4,SUBREP           SAVE A(SUBREP)                               
         MVC   RSWIKREP(2),0(R4)   PASS BACK TO CALLER                          
         B     NREPEX                                                           
NREP99   XC    RSWIKREP,RSWIKREP   NO MORE SUB REPS                             
NREPEX   EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'177REREPSW02 02/01/12'                                      
         END                                                                    
