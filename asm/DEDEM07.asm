*          DATA SET DEDEM07    AT LEVEL 013 AS OF 05/01/02                      
*PHASE T21B07A,*                                                                
*INCLUDE DAYUNPK                                                                
         TITLE 'DEDEM07 - $DEM ESTIMATE BOOK DEMO LOOK-UPS'                     
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* Nov16/95 011 GLEE - Prepatory stage for STEREO support              *         
*                                                                     *         
*  ??????   ?   ??  - HISTORY UNKNOWN                                 *         
***********************************************************************         
         EJECT                                                                  
DEM07    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DEM7**,RR=RE                                                 
         USING DEMWRKD,R9          R9=A(GLOBAL W/S)                             
         USING DEMTWAD,R8          R8=A(TWA)                                    
         L     R7,AAPWORK                                                       
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
         ST    RE,RELO07                                                        
*                                  HANDLE CONTROLLER MODE SETTINGS              
         CLI   APMODE,FORMHEAD     FORMAT HEADLINES & INITIALISE                
         BE    DEMHEAD                                                          
         CLI   APMODE,PROCESS      READ & POST RECORDS TO BUFFER                
         BE    DEMPROC                                                          
         CLI   APMODE,FORMLINE     FORMAT & PRINT A BUFFER RECORD               
         BE    DEMLINE                                                          
         CLI   APMODE,PROCREC      CONTINUATION RECORD                          
         BE    DEMLINE                                                          
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
* FORMAT HEADLINES & INITIALIZE FOR DEMO LOOK-UPS                               
*                                                                               
DEMHEAD  LA    R1,STAWIDTH                                                      
         STH   R1,CHUNKLEN         CHUNKLEN=L'STATION CHUNK                     
         IC    R1,NDEMS                                                         
         LA    RE,DEMWIDTH                                                      
         MR    R0,RE                                                            
         BCTR  R1,0                                                             
         STH   R1,HEADLEN          HEADLEN=L'STATION HEADING                    
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         STH   R1,EXECLEN          EXECLEN=L'STATION HEADING-2                  
         LH    R1,HEADLEN                                                       
         LA    R0,L'STASTA                                                      
         SR    R1,R0                                                            
         SRL   R1,1                                                             
         STH   R1,STADISP          STADISP=DISP. TO STATION IN HEADING          
*                                                                               
         LA    R2,DEMHD1+15        R2=A(START OF STATION HEADINGS)              
*                                                                               
DEMHEAD2 LR    R0,R2               CALCULATE DATA DISPLACEMENT                  
         LA    R1,DEMHD1                                                        
         SR    R0,R1                                                            
         STH   R0,DATADISP         SET DATA DISPLACEMENT                        
         MVI   0(R2),C'-'                                                       
         LH    R1,EXECLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),0(R2)       FILL HEADLINE WITH DASHES                    
         LR    R1,R2                                                            
         AH    R1,STADISP          CENTRALIZE STATION IN HEADING                
         MVC   0(L'STASTA,R1),STAS                                              
         CLI   4(R1),C'1'          TEST PARENT STATION                          
         BNL   *+8                                                              
         MVI   4(R1),C'-'          YES DON'T SHOW THE 'T'                       
         CLI   3(R1),C' '          TEST 3 BYTE STATION NAME                     
         BNE   *+8                                                              
         MVI   3(R1),C'-'                                                       
*                                  MOVE DEMO NAMES UNDER HEADING                
         LA    RE,L'DEMHD1H+L'DEMHD1(R2)                                        
         LA    RF,L'DEMHD1H+L'DEMHD1(RE)                                        
         LA    R1,DEMOUT75                                                      
         ZIC   R0,NDEMS                                                         
DEMHEAD4 MVC   0(7,RE),0(R1)                                                    
         MVC   1(5,RF),7(R1)                                                    
         LA    RE,DEMWIDTH(RE)                                                  
         LA    RF,DEMWIDTH(RF)                                                  
         LA    R1,L'DEMOUT75(R1)                                                
         BCT   R0,DEMHEAD4                                                      
*                                                                               
         LA    RE,BINKEYL                                                       
         ST    RE,BINLKEY          SET L'BINSRCH TABLE KEY                      
         ZIC   R1,NDEMS                                                         
         LA    R0,L'BINDEMS                                                     
         MR    R0,R0               R1=L'DEMO ACCUMULATORS                       
         LA    R1,L'BINPROG(R1,RE)                                              
         ST    R1,BINLREC          SET TOTAL BINSRCH RECORD LENGTH              
*                                  SET ACTUAL DEMOS TO LOOK-UP                  
         MVC   DEMONDEM,NDEMS                                                   
         ZIC   R1,DEMONDEM                                                      
         LA    R0,L'DEMS                                                        
         MR    R0,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DEMODEMS(0),DEMS                                                 
*                                  BUILD EXTENDED AVAIL LOOK-UP LIST            
         ZIC   R0,DEMONDEM                                                      
         LR    RE,R0                                                            
         MH    RE,=H'3'                                                         
         STC   RE,DEMONDEM                                                      
         LA    RE,DEMODEMS(R1)                                                  
         LA    RF,0(R1,RE)                                                      
         LA    R1,DEMS                                                          
DEMHEAD6 MVC   0(3,RE),0(R1)                                                    
         MVC   0(3,RF),0(R1)                                                    
         MVI   1(RE),C'S'          SHARE MODIFIER                               
         MVI   1(RF),C'P'          PUT MODIFIER                                 
         CLI   1(R1),C'R'          TEST RATINGS DEMO                            
         BE    *+12                                                             
         MVI   1(RE),C'X'          TSA SHARE MODIFIER                           
         MVI   1(RF),C'Q'          TSA TOTAL MODIFIER                           
         LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,DEMHEAD6                                                      
         MVI   0(RF),X'FF'                                                      
*                                                                               
DEMHEADX B     EXIT                                                             
         EJECT                                                                  
* READ DEMO FILES & POST TO BINSRCH BUFFER                                      
*                                                                               
DEMPROC  CLI   PROGPROF,C'N'       CHECK FOR NEW STYLE RECORDS                  
         BE    SIDPROC             AND DO NEW PROCESS FOR THEM                  
         XC    SYSVALS,SYSVALS                                                  
         XC    STAVALS,STAVALS                                                  
         L     R5,AEBREC                                                        
         MVI   WORK,EBPCODEQ                                                    
         BAS   RE,GETEL                                                         
         BNE   DEMPRC2                                                          
         USING EBPELEM,R1          R1=A(FIRST BUYING PERIOD ELEMENT)            
         SR    R0,R0               LOCATE BUYING PERIOD ELEMENT                 
DEMPRC1  CLI   EBPCODE,0           TEST E-O-R                                   
         BE    DEMPRC2                                                          
         CLI   EBPCODE,EBPCODEQ                                                 
         BNE   *+14                                                             
         CLC   EBPNUM,STAS+5       MATCH BUYING PERIOD NUMBER                   
         BE    *+14                                                             
         IC    R0,EBPLEN                                                        
         AR    R1,R0                                                            
         B     DEMPRC1                                                          
         MVC   SYSUPFIL,EBPUPFIL                                                
         MVC   SYSUPGRD,EBPUPGRD                                                
         MVC   SYSUPFBK,EBPUPFBK                                                
*                                                                               
DEMPRC2  LA    R5,ESREC                                                         
         ST    R5,AIOAREA          ESREC USED TO READ DAY/TIME RECORDS          
         MVI   WORK,SOUCODEQ                                                    
         BAS   RE,GETEL                                                         
         BNE   DEMPRC4                                                          
         USING SOUELEM,R1                                                       
         MVC   STAUPFIL,SOUUPFIL                                                
         MVC   STAUPGRD,SOUUPGRD                                                
         MVC   STAUPSTA,SOUUPSTA                                                
         MVC   STAUPFBK,SOUUPFBK                                                
*                                                                               
DEMPRC4  LA    R5,KEY                                                           
         USING ESDD,R5             R5=A(ESTIMATE BOOK STATION KEY)              
         MVC   ESDKEY,ESREC        ROOT SUPPLIES FIRST KEY                      
         MVC   ESDKNUM,STAS+5      BUYING PERIOD NUMBER                         
         XC    ESDKSEQ(3),ESDKSEQ                                               
         MVI   ESDKLINE,1                                                       
         MVI   IOFLAG,DIR+HIGH+SPT                                              
         B     DEMPRC8                                                          
DEMPRC6  MVI   IOFLAG,DIR+SEQ+SPT                                               
DEMPRC8  GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  FILTER ON STATION KEY                        
         L     R5,AIOAREA                                                       
         CLC   ESDKEY(ESDKSEQ-ESDKEY),KEYSAVE                                   
         BNE   DEMPRCX                                                          
         CLI   OPTDPRT,0           TEST DAYPART FILTER REQUIRED                 
         BE    *+14                                                             
         CLC   ESDKDPT,OPTDPRT     YES - TEST THIS DAYPART = FILTER             
         BNE   DEMPRC6                                                          
         CLI   OPTPTYP,0           TEST PROGTYP FILTER REQUIRED                 
         BE    *+14                                                             
         CLC   ESDKPRG,OPTPTYP                                                  
         BNE   DEMPRC6                                                          
*                                  READ STATION RECORD                          
         MVI   IOFLAG,FIL+READ+SPT                                              
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  EXTRACT KEY VALUES                           
         L     R5,AIOAREA                                                       
         MVC   THISSEQ,ESDKSEQ                                                  
         MVC   THISDPT,ESDKDPT                                                  
         MVC   THISPRG,ESDKPRG                                                  
         MVC   THISLIN,ESDKLINE                                                 
*                                                                               
         XC    DATVALS,DATVALS                                                  
         MVI   WORK,EDTCODEQ                                                    
         BAS   RE,GETEL                                                         
         BNE   DEMPRC6                                                          
         USING EDTELEM,R1                                                       
         MVC   DATDAY,EDTDAY                                                    
         MVC   DATTIM,EDTTIME                                                   
*                                                                               
         MVI   WORK,SOUCODEQ                                                    
         BAS   RE,GETEL                                                         
         BNE   DEMPRC9                                                          
         USING SOUELEM,R1                                                       
         MVC   DATUPFIL,SOUUPFIL                                                
         MVC   DATUPGRD,SOUUPGRD                                                
         MVC   DATUPSTA,SOUUPSTA                                                
         MVC   DATUPDAY,SOUUPDAY                                                
         MVC   DATUPTIM,SOUUPTIM                                                
         MVC   DATUPFBK,SOUUPFBK                                                
*                                  MERGE UPGRADE INTO DATVALS                   
DEMPRC9  OC    DATUPFIL,DATUPFIL   UPGRADE FILE                                 
         BNZ   DEMPRC10                                                         
         OC    DATUPFIL,STAUPFIL                                                
         BNZ   DEMPRC10                                                         
         OC    DATUPFIL,SYSUPFIL                                                
         BNZ   DEMPRC10                                                         
         MVI   DATUPFIL,C'T'                                                    
DEMPRC10 OC    DATUPGRD,DATUPGRD   UPGRADE EXPRESSION                           
         BNZ   DEMPRC12                                                         
         OC    DATUPGRD,STAUPGRD                                                
         BNZ   DEMPRC12                                                         
         OC    DATUPGRD,SYSUPGRD                                                
         BZ    DEMPRC6                                                          
DEMPRC12 OC    DATUPSTA,DATUPSTA   STATION                                      
         BNZ   DEMPRC14                                                         
         OC    DATUPSTA,STAUPSTA                                                
         BNZ   DEMPRC14                                                         
         OC    DATUPSTA,SYSUPSTA                                                
         BNZ   DEMPRC14                                                         
         MVC   DATUPSTA,STAS                                                    
DEMPRC14 OC    DATUPDAY,DATUPDAY   DAY                                          
         BNZ   DEMPRC16                                                         
         OC    DATUPDAY,STAUPDAY                                                
         BNZ   DEMPRC16                                                         
         OC    DATUPDAY,SYSUPDAY                                                
DEMPRC16 OC    DATUPTIM,DATUPTIM   TIME                                         
         BNZ   DEMPRC18                                                         
         OC    DATUPTIM,STAUPTIM                                                
         BNZ   DEMPRC18                                                         
         OC    DATUPTIM,SYSUPTIM                                                
DEMPRC18 OC    DATUPFBK,DATUPFBK   FROM BOOK                                    
         BNZ   DEMPRC20                                                         
         OC    DATUPFBK,STAUPFBK                                                
         BNZ   DEMPRC20                                                         
         OC    DATUPFBK,SYSUPFBK                                                
*                                                                               
DEMPRC20 LA    R5,SPDEMUP1                                                      
         USING SPDEMUPD,R5         R5=A(UPGRADE BLOCK)                          
         XC    SPDEMUPD(SPDEMUPL),SPDEMUPD                                      
         LA    R0,IOAREA1                                                       
         ST    R0,SPUPAREC                                                      
         MVC   SPUPAFAC,AFAC                                                    
         MVC   SPUPAGY,AGYALPH                                                  
         MVC   SPUPMED,DBMED                                                    
         MVC   SPUPSTA,DATUPSTA                                                 
         MVC   SPUPDAY,DATDAY                                                   
         MVC   SPUPTIM,DATTIM                                                   
         MVC   SPUPFIL,DATUPFIL                                                 
         MVC   SPUPSRC,DBSRC                                                    
         MVC   SPUPFBK,DATUPFBK                                                 
         MVC   SPUP2YRP,OPT2YRP                                                 
         MVC   SPUP2YRR,OPT2YRR                                                 
         L     R1,AIOAREA                                                       
         LA    R1,EDTELEM-ESDREC(R1)                                            
         ST    R1,SPUPAOVR         SET A(DEMO OVERRIDE LIST)                    
         MVC   SPUPUDAY,DATUPDAY                                                
         MVC   SPUPUTIM,DATUPTIM                                                
         MVC   SPUPTYPE(L'DATUPGRD),DATUPGRD                                    
*                                  CALL DEMUP TO CALCULATE DEMOS                
         GOTO1 VSPDEMUP,DMCB,SPDEMUPD,DEMODEMS,THISDEMS                         
         CLI   0(R1),0             TEST FOR ERRORS                              
         BNE   DEMPRC6                                                          
*                                  SET POSTING KEY VALUES & PROG NAME           
         MVC   THISDAY,DATDAY                                                   
         MVC   THISTIM,DATTIM                                                   
*                                                                               
         LA    R1,POSTLINE                                                      
         USING BINRECD,R1                                                       
*                                  BUILD BINSRCH KEY                            
         MVC   BINKEY(BINKEYL),THISKEY                                          
*                                  BUILD RECORD DATA                            
         MVC   BINPROG,SPUPPRG                                                  
         SR    R2,R2               R2=LINE TYPE                                 
         LA    R3,3                THREE RECORDS FOR AVAIL FORMAT               
         LA    R4,THISDEMS         R4=A(DEMO VALUES)                            
DEMPRC22 STC   R2,BINTYPE                                                       
         LA    RE,BINDEMS                                                       
         ZIC   R0,NDEMS                                                         
DEMPRC24 MVC   0(3,RE),1(R4)                                                    
         LA    RE,3(RE)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,DEMPRC24                                                      
         GOTO1 APOST               GO ADD RECORD TO BINSRCH BUFFER              
         LA    R2,1(R2)                                                         
         BCT   R3,DEMPRC22                                                      
         B     DEMPRC6                                                          
         DROP  R1                                                               
*                                                                               
DEMPRCX  XC    THISKEY(THISKEYX-THISKEY),THISKEY                                
         B     EXIT                                                             
         EJECT                                                                  
SIDPROC  XC    SYSVALS,SYSVALS                                                  
         XC    STAVALS,STAVALS                                                  
         LA    R1,SRBLK                                                         
         LA    R0,SRBLKLN/256                                                   
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A47'                                       
         MVC   VRANCID,0(R1)                                                    
*                                                                               
         MVC   SRASIR,AEBREC       SET RANCID IO AREA                           
         L     R1,AFAC             AND ADDRESS CONSTANTS                        
         ST    R1,SRACOM                                                        
         MVC   SRACLPAC,VCLPACK                                                 
         MVC   SRAMSUNP,VMSUNPK                                                 
         L     RF,RELO07                                                        
         L     RE,VDAYUNPK                                                      
         AR    RE,RF                                                            
         ST    RE,SRADYUNP                                                      
         MVC   SRAUNTIM,VUNTIME                                                 
*                                  SET UP TO READ RECORDS                       
         MVC   SRSELSCH,NSIDSCHM                                                
         MVC   SRSELAM,AGYMED                                                   
         MVC   SRSELAGY,AGYALPH                                                 
         MVC   SRSELMED,DBMED                                                   
         MVC   SRSELPER,NSIDPER                                                 
         MVC   SRSELSTA,NSIDSTAB                                                
         MVC   SRSELDPT(1),OPTDPRT                                              
         MVC   SRSELPRG(1),OPTPTYP                                              
         MVC   SRSELSLN,OPTSECS                                                 
*                                  NOW GET THE BLOODY BUGGERS                   
SIDPRC10 GOTO1 VRANCID,DMCB,SRBLK                                               
         CLI   SRMODE,SRONEREC                                                  
         BNE   SIDPRCX                                                          
         MVC   THISSEQ,SRACTSEQ                                                 
         MVC   THISPRG,SRACTPRG                                                 
         MVC   THISDPT,SRACTDPT                                                 
         MVC   THISLIN,=X'1'                                                    
SIDPRC20 LA    R5,SPDEMUP1                                                      
         USING SPDEMUPD,R5         R5=A(UPGRADE BLOCK)                          
         XC    SPDEMUPD(SPDEMUPL),SPDEMUPD                                      
         LA    R0,IOAREA1                                                       
         ST    R0,SPUPAREC                                                      
         MVC   SPUPAFAC,AFAC                                                    
         MVC   SPUPAGY,AGYALPH                                                  
         MVC   SPUPMED,DBMED                                                    
         OC    SRUPSTA,SRUPSTA                                                  
         BNZ   *+10                                                             
         MVC   SPUPSTA,SRERSTA                                                  
         MVC   SPUPDAY,SRACTDAY                                                 
         MVC   SPUPTIM,SRACTTIM                                                 
         MVC   SPUPFIL,SRUPFILE                                                 
         MVC   SPUPSRC,SRBKSRC                                                  
         MVC   SPUPFBK,SRUPBOOK                                                 
         MVC   SPUP2YRP,OPT2YRP                                                 
         MVC   SPUP2YRR,OPT2YRR                                                 
         MVC   SPUPAOVR,SROVELEM                                                
         MVC   SPUPUDAY,SRUPDAY                                                 
         MVC   SPUPUTIM,SRUPTIM                                                 
         MVC   SPUPTYPE(L'SRUPEXP),SRUPEXP                                      
*                                  CALL DEMUP TO CALCULATE DEMOS                
         GOTO1 VSPDEMUP,DMCB,SPDEMUPD,DEMODEMS,THISDEMS                         
         CLI   0(R1),0             TEST FOR ERRORS                              
         BNE   SIDPRC10                                                         
*                                  SET POSTING KEY VALUES & PROG NAME           
         MVC   THISDAY,SRACTDAY                                                 
         MVC   THISTIM,SRACTTIM                                                 
*                                                                               
         LA    R1,POSTLINE                                                      
         USING BINRECD,R1                                                       
*                                  BUILD BINSRCH KEY                            
         MVC   BINKEY(BINKEYL),THISKEY                                          
*                                  BUILD RECORD DATA                            
         MVC   BINPROG,SPUPPRG                                                  
         SR    R2,R2               R2=LINE TYPE                                 
         LA    R3,3                THREE RECORDS FOR AVAIL FORMAT               
         LA    R4,THISDEMS         R4=A(DEMO VALUES)                            
SIDPRC22 STC   R2,BINTYPE                                                       
         LA    RE,BINDEMS                                                       
         ZIC   R0,NDEMS                                                         
SIDPRC24 MVC   0(3,RE),1(R4)                                                    
         LA    RE,3(RE)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,SIDPRC24                                                      
         GOTO1 APOST               GO ADD RECORD TO BINSRCH BUFFER              
         LA    R2,1(R2)                                                         
         BCT   R3,SIDPRC22                                                      
         B     SIDPRC10                                                         
         DROP  R1                                                               
*                                                                               
SIDPRCX  XC    THISKEY(THISKEYX-THISKEY),THISKEY                                
         B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO FIND AN ELEMENT ON A RECORD                                        
* ON ENTRY WORK(1)=ELEMENT CODE TO BE FOUND, R5=A(RECORD)                       
* ON EXIT R1=A(ELEMENT) WITH CC=EQ IF ELEMENT FOUND                             
*                                                                               
GETEL    LA    R1,DCNELEM-EBDREC(R5)                                            
         SR    R0,R0                                                            
GETEL2   CLI   0(R1),0                                                          
         BE    GETELX                                                           
         CLC   0(1,R1),WORK                                                     
         BE    GETELX                                                           
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETEL2                                                           
GETELX   CLC   0(1,R1),WORK                                                     
         BR    RE                                                               
         EJECT                                                                  
* FORMAT PRINT LINES                                                            
*                                                                               
DEMLINE  L     R5,BINAREC                                                       
         USING BINRECD,R5          R5=A(RECORD)                                 
         CLI   BINSEQ,X'FF'                                                     
         BE    DEMLINX                                                          
         CLC   BINKEY(BINTYPE-BINKEY),THISKEY                                   
         BE    DEMLIN2                                                          
*                                                                               
         MVI   LINE1,C'*'                                                       
         MVC   LINE1+1(6),LINE1                                                 
         BAS   RE,GETDAY                                                        
         MVC   LINE1+8(L'DAYTIME),DAYTIME                                       
         LA    R2,LINE1+8+L'DAYTIME-1                                           
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         BAS   RE,GETDPT                                                        
         OC    WORK(L'DCNDNAME),WORK                                            
         BZ    DEMLINA                                                          
         MVC   2(4,R2),=C'DPT='                                                 
         MVC   6(L'DCNDNAME,R2),WORK                                            
         LA    R2,6+L'DCNDNAME-1(R2)                                            
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
DEMLINA  BAS   RE,GETTYP                                                        
         OC    WORK(L'PCNDNAME),WORK                                            
         BZ    DEMLINB                                                          
         MVC   2(4,R2),=C'TYP='                                                 
         MVC   6(L'DCNDNAME,R2),WORK                                            
         LA    R2,6+L'DCNDNAME-1(R2)                                            
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
DEMLINB  MVC   2(4,R2),=C'PGM='                                                 
         MVC   6(L'BINPROG,R2),BINPROG                                          
         MVC   THISKEY(BINKEYL),BINKEY                                          
         MVI   APMODE,SAMEREC      RETURN RECORD AGAIN                          
         B     DEMLINX                                                          
*                                                                               
DEMLIN2  LA    R2,LINE1                                                         
         LA    RE,=C'DEMOS'                                                     
         CLI   BINTYPE,BINTDEM                                                  
         BE    DEMLIN4                                                          
         LA    R2,LINE2                                                         
         LA    RE,=C'SHARE'                                                     
         CLI   BINTYPE,BINTSHR                                                  
         BE    DEMLIN4                                                          
         LA    R2,LINE3                                                         
         LA    RE,=C'H/P/T'                                                     
         CLI   BINTYPE,BINTHPT                                                  
         BE    DEMLIN4                                                          
         DC    H'0'                                                             
DEMLIN4  MVI   0(R2),C'*'                                                       
         MVC   8(5,R2),0(RE)                                                    
         AH    R2,DATADISP                                                      
         LA    R3,DEMS             R3=A(DEMO EXPRESSIONS)                       
         LA    R4,BINDEMS          R4=A(DEMO VALUES)                            
         ZIC   R0,NDEMS            R0=N'DEMOS                                   
*                                                                               
DEMLIN6  BAS   RE,GETDEM           EDIT DEMO VALUE                              
         LA    R2,DEMWIDTH(R2)     BUMP TO NEXT DEMO                            
         LA    R3,L'DEMS(R3)                                                    
         LA    R4,L'BINDEMS(R4)                                                 
         BCT   R0,DEMLIN6          DO FOR NUMBER OF DEMOS                       
*                                  TEST IF NEXT RECORD IS WANTED NOW            
         A     R5,BINLREC          GO FORWARD ONE RECORD                        
         CLC   BINKEY(BINTYPE-BINKEY),THISKEY                                   
         BNE   *+8                                                              
         MVI   APMODE,NEXTREC      TELL ROOT I WANT ANOTHER RECORD              
DEMLINX  B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO CONVERT DAY/TIME VALUES INTO OUTPUT FORMAT                         
*                                                                               
GETDAY   NTR1                                                                   
*                                  CONVERT INPUT DAY VALUES                     
         MVC   DAYTIME,SPACES                                                   
         MVC   DUB(1),BINDAY                                                    
         MVC   WORK(1),BINDAY                                                   
         IC    RF,DUB              RF=DAY EXPRESSION                            
         LA    R1,IDAYTAB                                                       
         MVI   DUB+1,0                                                          
*                                                                               
GETDAY10 CLI   0(R1),X'FF'         TEST E-O-T                                   
         BE    GETDAY14                                                         
         CLI   DAYTIME,C' '        TEST START DAY SET                           
         BE    *+10                                                             
         OC    DUB+1(1),0(R1)      YES - SET THIS DAY ON                        
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    0(R1),0             TEST IF DAY BIT ON                           
         BZ    GETDAY12                                                         
         MVC   DAYTIME+3(3),1(R1)  SET START & END DAY                          
         CLC   DAYTIME(3),SPACES                                                
         BNE   *+10                                                             
         MVC   DAYTIME(3),DAYTIME+3                                             
         OC    DUB+1(1),0(R1)                                                   
         XC    DUB(1),0(R1)        TURN OFF THIS DAY IN MASK                    
         CLI   DUB,0               TEST ALL DAYS PROCESSED                      
         BE    GETDAY14            YES - GO FORMAT OUTPUT                       
GETDAY12 LA    R1,L'IDAYTAB(R1)                                                 
         B     GETDAY10                                                         
GETDAY14 CLC   DUB+1(1),WORK       TEST REGULAR ROTATOR (M-W,M-F ETC.)          
         BE    *+8                                                              
         MVI   DAYTIME+3,C'+'      NO - INDICATE IRREGULAR                      
         CLC   DAYTIME(3),DAYTIME+3 TEST START EQ END DAY                       
         BE    *+14                                                             
         MVI   DAYTIME+1,C'-'      NO - SHOW FIRST CHAR OF EACH DAY             
         MVC   DAYTIME+2(1),DAYTIME+3                                           
         MVC   DAYTIME+3(3),SPACES                                              
*                                  EDIT TIME VALUES                             
*                                  MILITARY TIME FORMAT                         
GETDAY16 MVI   DAYTIME+3,C'/'                                                   
         CLI   OPTTIME,C'M'                                                     
         BNE   GETDAY18                                                         
         ICM   R0,15,BINTIM                                                     
         SRDL  R0,16               R0=START TIME                                
         SRL   R1,16               R1=END TIME                                  
         CVD   R0,DUB              EDIT START TIME                              
         OI    DUB+7,X'0F'                                                      
         UNPK  DAYTIME+4(4),DUB                                                 
         CR    R0,R1               TEST START TIME EQ END TIME                  
         BE    GETDAYX                                                          
         MVI   DAYTIME+8,C'-'                                                   
         CVD   R1,DUB              EDIT END TIME                                
         OI    DUB+7,X'0F'                                                      
         UNPK  DAYTIME+9(4),DUB                                                 
         B     GETDAYX                                                          
*                                  STANDARD TIME FORMAT                         
GETDAY18 CLC   BINTIM(2),BINTIM+2  TEST START TIME EQ END TIME                  
         BNE   *+10                                                             
         XC    BINTIM+2(2),BINTIM+2                                             
         GOTO1 VUNTIME,DMCB,BINTIM,DAYTIME+4                                    
*                                                                               
GETDAYX  B     EXIT                                                             
         EJECT                                                                  
* LOOK-UP PROGRAM TYPE NAME                                                     
*                                                                               
GETTYP   L     R1,AEBREC                                                        
         LA    R1,DCNELEM-EBDD(R1)                                              
         XC    WORK(L'PCNDNAME),WORK                                            
         CLI   BINPRG,0                                                         
         BE    GETTYPX                                                          
         MVC   WORK(7),=C'UNKNOWN'                                              
         SR    R0,R0                                                            
         USING PCNELEM,R1          LOCATE PROGTYP ELEMENT ON RECORD             
GETTYP2  CLI   PCNCODE,0                                                        
         BE    GETTYPX                                                          
         CLI   PCNCODE,PCNCODEQ                                                 
         BE    *+14                                                             
         IC    R0,PCNLEN                                                        
         AR    R1,R0                                                            
         B     GETTYP2                                                          
         IC    R0,PCNLEN                                                        
         SH    R0,=H'2'                                                         
         SRL   R0,3                R0=NUMBER OF ENTRIES IN LIST                 
         LA    R1,PCNDATA          R1=A(FIRST LIST ENTRY)                       
         USING PCNDATA,R1                                                       
GETTYP4  CLC   PCNDCODE,BINPRG                                                  
         BNE   *+14                                                             
         MVC   WORK(L'PCNDNAME),PCNDNAME                                        
         B     GETTYPX                                                          
         LA    R1,8(R1)                                                         
         BCT   R0,GETTYP4                                                       
GETTYPX  BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
* LOOK-UP DAYPART NAME                                                          
*                                                                               
GETDPT   L     R1,AEBREC                                                        
         LA    R1,DCNELEM-EBDD(R1)                                              
         XC    WORK(L'DCNDNAME),WORK                                            
         CLI   BINDPT,0                                                         
         BE    GETDPTX                                                          
         MVC   WORK(7),=C'UNKNOWN'                                              
         SR    R0,R0                                                            
         USING DCNELEM,R1          LOCATE DAYPART ELEMENT ON RECORD             
GETDPT2  CLI   DCNCODE,0                                                        
         BE    GETDPTX                                                          
         CLI   DCNCODE,DCNCODEQ                                                 
         BE    *+14                                                             
         IC    R0,DCNLEN                                                        
         AR    R1,R0                                                            
         B     GETDPT2                                                          
         IC    R0,DCNLEN                                                        
         SH    R0,=H'2'                                                         
         SRL   R0,3                R0=NUMBER OF ENTRIES IN LIST                 
         LA    R1,DCNDATA          R1=A(FIRST LIST ENTRY)                       
         USING DCNDATA,R1                                                       
GETDPT4  CLC   DCNDCODE,BINDPT                                                  
         BNE   *+14                                                             
         MVC   WORK(L'DCNDNAME),DCNDNAME                                        
         B     GETDPTX                                                          
         LA    R1,8(R1)                                                         
         BCT   R0,GETDPT4                                                       
GETDPTX  BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
* ROUTINE TO EDIT A DEMO VALUE                                                  
* ON ENTRY R3=A(3 BYTE DEMO EXPRESSION)                                         
*          R4=A(3 BYTE DEMO VALUE)                                              
*          R2=A(7 BYTE OUTPUT DEMO VALUE)                                       
*                                                                               
GETDEM   NTR1                                                                   
         SR    R0,R0                                                            
         ICM   R0,7,0(R4)          R0=DEMO VALUE                                
         MVC   DUB(1),DBFIL                                                     
         MVC   DUB+1(1),1(R3)                                                   
         MVI   DUB+2,0                                                          
         LA    R1,EDITTAB          R1=A(EDIT TABLE)                             
*                                  SEARCH TABLE FOR DEMO                        
GETDEM2  CLI   0(R1),EOT           TEST E-O-T                                   
         BE    GETDEM4                                                          
         CLC   0(2,R1),DUB         MATCH FILE/DEMO MODIFIER                     
         BE    *+12                                                             
         LA    R1,L'EDITTAB(R1)                                                 
         B     GETDEM2                                                          
         MVC   DUB+2(1),2(R1)      EXTRACT EDIT VALUES                          
*                                  EDIT DEMO VALUE                              
GETDEM4  TM    DUB+2,X'80'         TEST DEMO NEEDS SCALING                      
         BZ    *+8                                                              
         MH    R0,=H'10'                                                        
         TM    DUB+2,X'02'         TEST EDIT TO 2 DECIMALS                      
         BO    GETDEM6                                                          
         TM    DUB+2,X'01'         TEST EDIT TO 1 DECIMAL                       
         BO    GETDEM8                                                          
         EDIT  (R0),(7,0(R2))                                                   
         B     GETDEMX                                                          
GETDEM6  EDIT  (R0),(7,0(R2)),2,ZERO=BLANK                                      
         B     GETDEMX                                                          
GETDEM8  EDIT  (R0),(7,0(R2)),1,ZERO=BLANK                                      
         B     GETDEMX                                                          
GETDEMX  B     EXIT                                                             
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
STAWIDTH EQU   64                  TOTAL WIDTH OF STATION HEADINGS              
DEMWIDTH EQU   8                   WIDTH FOR ONE DEMO ENTRY                     
         SPACE 1                                                                
*                                  TABLE FOR DEMO EDITTING ROUTINE              
EDITTAB  DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         DC    C'TI',X'01'                                                      
         DC    C'TR',X'01'                                                      
         DC    C'TP',X'01'                                                      
         DC    C'TS',X'01'                                                      
         DC    C'TQ',X'01'                                                      
         DC    C'TX',X'01'                                                      
         DC    C'TE',X'01'                                                      
         DC    C'PI',X'01'                                                      
         DC    C'PR',X'01'                                                      
         DC    C'PP',X'81'                                                      
         DC    C'PS',X'01'                                                      
         DC    C'PQ',X'01'                                                      
         DC    C'PX',X'01'                                                      
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
*                                  TABLE TO CONVERT INPUT DAY VALUES            
IDAYTAB  DS    0XL4                                                             
         DC    X'40',C'MON'                                                     
         DC    X'20',C'TUE'                                                     
         DC    X'10',C'WED'                                                     
         DC    X'08',C'THU'                                                     
         DC    X'04',C'FRI'                                                     
         DC    X'02',C'SAT'                                                     
         DC    X'01',C'SUN'                                                     
         DC    X'FF'                                                            
*                                                                               
VDAYUNPK DC    V(DAYUNPK)                                                       
         EJECT                                                                  
* DEDEMWRK                                                                      
       ++INCLUDE DEDEMWRK                                                       
         ORG   APSAVE                                                           
         SPACE 1                                                                
* SAVE STORAGE FOR OVERLAY                                                      
*                                                                               
CHUNKLEN DS    H                   TOTAL L'STATION CHUNK                        
HEADLEN  DS    H                   L'STATION HEADINGS                           
STADISP  DS    H                   DISP. INTO HEADING OF STATION                
EXECLEN  DS    H                   L'STATION HEADINGS-2 (FOR EX)                
DATADISP DS    H                   DISP. INTO LINE OF FIRST DEMO                
         ORG                                                                    
         EJECT                                                                  
* DSECT TO COVER TEMPORARY WORKING STORAGE                                      
*                                                                               
DEMTMPD  DSECT                                                                  
ESREC    DS    300C                ESTIMATE BOOK STATION RECORD                 
*                                  SYSTEM UPGRADE VALUES                        
SYSVALS  DS    0XL26                                                            
SYSUPFIL DS    XL1                 UPGRADE FILE (T OR P)                        
SYSUPGRD DS    XL8                 UPGRADE EXPRESSION                           
SYSUPSTA DS    CL5                 OVERRIDE STATION                             
SYSUPDAY DS    XL1                 OVERRIDE DAY                                 
SYSUPTIM DS    XL4                 OVERRIDE TIME                                
SYSUPFBK DS    XL2                 OVERRIDE BOOK                                
         DS    XL5                 SPARE                                        
*                                  STATION UPGRADE VALUES                       
STAVALS  DS    0XL26                                                            
STAUPFIL DS    XL1                 UPGRADE FILE (T OR P)                        
STAUPGRD DS    XL8                 UPGRADE EXPRESSION                           
STAUPSTA DS    CL5                 OVERRIDE STATION                             
STAUPDAY DS    XL1                 OVERRIDE DAY                                 
STAUPTIM DS    XL4                 OVERRIDE TIME                                
STAUPFBK DS    XL2                 OVERRIDE BOOK                                
         DS    XL5                 SPARE                                        
*                                  DAY/TIME UPGRADE VALUES                      
DATVALS  DS    0XL26                                                            
DATUPFIL DS    XL1                 UPGRADE FILE (T OR P)                        
DATUPGRD DS    XL8                 UPGRADE EXPRESSION                           
DATUPSTA DS    CL5                 OVERRIDE STATION                             
DATUPDAY DS    XL1                 OVERRIDE DAY                                 
DATUPTIM DS    XL4                 OVERRIDE TIME                                
DATUPFBK DS    XL2                 OVERRIDE BOOK                                
DATDAY   DS    XL1                 DAY                                          
DATTIM   DS    XL4                 TIME                                         
*                                                                               
THISKEY  DS    0X                  CURRENT KEY VALUES                           
THISSEQ  DS    XL1                 CURRENT SEQUENCE NUMBER                      
THISDPT  DS    XL1                 CURRENT DAYPART CODE                         
THISPRG  DS    XL1                 CURRENT PROGRAM TYPE CODE                    
THISLIN  DS    XL1                 CURRENT LINE NUMBER                          
THISDAY  DS    XL1                 CURRENT KEY DAY                              
THISTIM  DS    XL4                 CURRENT START & END TIMES                    
THISTYPE DS    XL1                 CURRENT LINE TYPE                            
THISKEYX EQU   *                                                                
*                                                                               
THISDEMS DS    (MAXDEMS*3)XL4      CURRENT DEMOUT DEMO VALUES                   
DEMONDEM DS    XL1                 ACTUAL N'DEMOS TO LOOK-UP                    
DEMODEMS DS    (MAXDEMS*3)XL3,X    DEMO VALUES (+LIST TERMINATOR)               
DAYTIME  DS    CL20                OUTPUT DAY/TIME VALUE                        
         EJECT                                                                  
RELO07   DS    F                                                                
VRANCID  DS    F                                                                
       ++INCLUDE SPRANSIDD                                                      
         EJECT                                                                  
* DSECT TO COVER BINSRCH RECORD                                                 
*                                                                               
BINRECD  DSECT                                                                  
BINKEY   DS    0X                  BINSRCH KEY                                  
BINSEQ   DS    XL1                 SEQUENCE NUMBER                              
BINDPT   DS    XL1                 DAYPART CODE                                 
BINPRG   DS    XL1                 PROGRAM TYPE CODE                            
BINLIN   DS    XL1                 LINE NUMBER                                  
BINDAY   DS    XL1                 DAY CODE                                     
BINTIM   DS    XL4                 START & END TIME (MILITARY)                  
BINTYPE  DS    XL1                 FORMAT TYPE (SEE BELOW)                      
BINTDEM  EQU   0                   DEMOS OR RTG/IMPS                            
BINTSHR  EQU   1                   DEMO SHARES       (AVAIL FORMAT)             
BINTHPT  EQU   2                   DEMO HUT/PUT/TOTS (AVAIL FORMAT)             
BINKEYL  EQU   *-BINKEY                                                         
BINDATA  DS    0X                  BINSRCH DATA                                 
BINPROG  DS    CL16                PROGRAM NAME(S)                              
BINDEMS  DS    0XL3                VARIABLE NUMBER OF DEMO VALUES               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013DEDEM07   05/01/02'                                      
         END                                                                    
