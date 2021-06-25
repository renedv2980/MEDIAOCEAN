*          DATA SET NEPOD01    AT LEVEL 244 AS OF 09/17/20                      
*PHASE T32501B                                                                  
*INCLUDE COMSTACN                                                               
         TITLE 'T32501 - RESEARCH WRITER APPLICATION'                           
*                                                                               
T32501   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T32501,RA,R8,RR=R2,CLEAR=YES                               
         LR    R6,RC                                                            
         USING WORKD,R6                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         ST    R2,RELO                                                          
         USING PODBD,PODBOOK                                                    
*                                                                               
         BRAS  RE,INITIAL                                                       
*                                                                               
         LA    R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
         BRAS  RE,INITIAL2                                                      
*                                                                               
         ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NENACWKS  TABLE OF MONTH<->WEEK                        
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ANACWKS                                                       
*                                                                               
PODMAIN  DS    0H                                                               
         CLI   MODE,VALREC                                                      
         BNE   PODM06                                                           
         CLI   OFFLINE,C'Y'                                                     
         BE    PODM02                                                           
         L     R2,AFRSTKEY                                                      
         CLI   5(R2),0             IF FIRST KEY FIELD IS BLANK                  
         BNE   PODM02                                                           
         CLC   SPLLAST+1(2),=X'0101' AND SCREEN JUST LOADED                     
         BNE   PODM02                                                           
         MVI   PERROR+1,2          GIVE USER A SHOT BEFORE VALIDATING           
         MVI   PMSGTYPE,C'I'                                                    
         GOTO1 PDSERR                                                           
*                                                                               
         MVI   PDPRMOPT,0                                                       
PODM02   GOTOR SUBR01,DMCB,('VRECE',(RC))                                       
         L     RE,APODINPL                                                      
         ZIC   RF,0(RE)                                                         
         CLC   1(4,RE),=C'PREM'                                                 
         BE    PODM04                                                           
         LA    RE,10(RE)                                                        
         BCT   RF,*-14                                                          
         MVI   PDPRMPRT,C'N'                                                    
         B     PODMX                                                            
*                                                                               
PODM04   MVI   PDPRMPRT,C'Y'                                                    
         CLI   PDPRMOPT,0                                                       
         BNE   *+8                                                              
         MVI   PDPRMOPT,C'Y'                                                    
*                                                                               
PODM06   CLI   MODE,PRINTREP                                                    
         BNE   PODMX                                                            
*        BAS   RE,PREP                                                          
         GOTOR SUBR03,DMCB,('PREPE',(RC))                                       
         BAS   RE,PROCESS                                                       
*                                                                               
*ODMX    B     XIT                                                              
PODMX    DS    0H                                                               
*                                                                               
XIT      XIT1                                                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         J     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
*        PRINT THE REPORT                                             *         
***********************************************************************         
*                                                                               
PROCESS  NTR1                                                                   
         BRAS  RE,INITPROC                                                      
*                                                                               
* THIS WAS MOVED TO INITPROC DUE TO ADDRESSABILITY                              
* BEGINNING OF MOVE                                                             
*&&DO                                                                           
         OC    PDMGRP,PDMGRP       DO WE HAVE A MARKET GROUP?                   
         BZ    PROC01              NO, CONTINUE                                 
         MVC   PDQMGR(5),PDMGRP                                                 
         XC    PDSVMGKY,PDSVMGKY                                                
         XC    PDSIDMKT,PDSIDMKT                                                
         LA    R2,PDQMGR                                                        
*                                                                               
PROC00   GOTOR SUBR03,DMCB,('NEXTME',(RC)),(R2)                                 
         XC    DBSELSTA,DBSELSTA                                                
         MVC   SVDAYTME,DBSELDAY                                                
         OC    PDSIDMKT,PDSIDMKT                                                
         BZ    PROC01                                                           
         GOTOR SUBR02,DMCB,('RDRMKTE',(RC))                                     
         MVC   DBSELDAY(5),SVDAYTME                                             
         B     PROC00                                                           
*&&                                                                             
* END OF MOVE                                                                   
*                                                                               
PROC01   GOTOR SUBR03,DMCB,('GETNTISE',(RC))                                    
*                                                                               
PROC02   L     R2,APODBKL                                                       
         MVC   PODBD(PODBLNQ),0(R2)                                             
         MVC   PDSBOOK,PODBBKS                                                  
         MVC   PDEBOOK,PODBBKS                                                  
         CLI   0(R2),0             IS BOOK A RANGE                              
         BE    *+10                NO, THEN START AND END BOOK EQUAL            
         MVC   PDEBOOK,PODBLNQ+PODBBKS-PODBD(R2)                                
*                                                                               
         L     RE,APODMKTL         SET POINTER AT BEGINNING OF LIST             
         CLI   0(RE),X'FF'                                                      
         BE    PROC04                                                           
*                                                                               
PROC03   CLC   =C'CCV',PODBEXT                                                  
         BE    PROC04A                                                          
         MVC   PODMPAMK,0(RE)                                                   
         LA    RE,2(RE)                                                         
         STCM  RE,15,PODMPTR       SAVE ADDRESS OF NEXT                         
         GOTOR SUBR02,DMCB,('RDDMKTE',(RC))                                     
*                                                                               
PROC04   DS    0H                                                               
         CLC   =C'CCV',PODBEXT                                                  
         BNE   PROC05                                                           
         OC    PDSIDMKT,PDSIDMKT                                                
         BZ    PROC05                                                           
         LA    RE,PDSIDMKT         SET POINTER AT BEGINNING OF LIST             
PROC04A  CLI   0(RE),X'FF'                                                      
         BE    PROC05                                                           
         OC    0(2,RE),0(RE)                                                    
         BZ    PROC05                                                           
         MVC   PODMPAMK,0(RE)                                                   
         LA    RE,2(RE)                                                         
         STCM  RE,15,PODMPTR       SAVE ADDRESS OF NEXT                         
         GOTOR SUBR02,DMCB,('RDCNTYE',(RC))   GET ALL STA'S FOR COUNTY          
*                                                                               
PROC05   MVI   EBOPT,C'N'          DEFAULT, NO EST BOOK                         
         MVI   PDIBKSEQ,1          INIT INPUT BOOK SEQ                          
         LA    R3,1                                                             
         TM    13(R2),X'80'        DID WE REQUEST EST BOOK?                     
         BZ    PROC06                                                           
         MVI   EBOPT,C'Y'          SET FLAG ON                                  
         NI    13(R2),X'7F'        TURN HOB OFF                                 
*                                                                               
PROC06   L     R3,APODNET                                                       
         CLC   0(3,R3),=CL3'ZZZ'                                                
         BNE   *+8                                                              
         BAS   RE,GETALLST                                                      
         LA    R5,PODDAYTM                                                      
*                                                                               
PROC08   MVC   CURSRC,1(R2)        SAVE CURRENT SOURCE                          
         L     R1,PDADEMTB                                                      
         LA    RE,REALDEMS                                                      
*                                                                               
PROC10   CLI   0(R1),X'FF'                                                      
         BE    PROC14                                                           
         MVC   0(1,RE),1(R1)                                                    
         LA    RE,1(RE)                                                         
         CLC   =C'NMI',1(R2)                                                    
         BE    PROC12                                                           
         CLC   =C'EMI',1(R2)                                                    
         BNE   *+8                                                              
*                                                                               
PROC12   MVI   1(R1),C'T'          USE THIS TO TEST                             
         AH    R1,LNDEMCOD                                                      
         B     PROC10                                                           
*                                                                               
PROC14   MVI   0(RE),X'FF'                                                      
*                                                                               
PROC15   CLI   0(R3),X'FF'         NO STATIONS FOUND                            
         BNE   PROC16                                                           
         CLI   10(R3),C'X'         COMSCORE NETWORK?                            
         BE    PROC24                                                           
         ICM   R0,15,PODNETAD                                                   
         BZ    PROC16                                                           
         LR    R3,R0               RELOAD REGULAR STATION TABLE                 
*        XC    PODNETAD,PODNETAD                                                
*        XC    PODNETMB,PODNETMB                                                
         XC    PODNETAD(L'PODNETAD+L'PODNETMB),PODNETAD                         
         B     PROC24                                                           
*                                                                               
PROC16   MVI   ODYTSET,C'N'        DEFAULT. NO CABLE OPTIMIZATION               
*                                  FOR STATION (ODYT EXTEND)                    
         MVC   DAYHOLD,0(R5)       SAVE DAY FIELD                               
         MVC   TIMEHOLD,1(R5)      SAVE TIME FIELD                              
         ST    R2,SVPODBOK                                                      
         XC    MPAHOOK,MPAHOOK                                                  
         MVI   ENMIFLAG,0                                                       
         CLC   =C'NMI',1(R2)                                                    
         BE    PROC18                                                           
         CLC   =C'EMI',1(R2)                                                    
         BNE   PROC20              NOT SPECIAL SOURCE                           
*                                                                               
PROC18   MVC   ENMIFLAG,1(R2)      GET FIRST LETTER                             
         LAY   RE,GETMPAHK                                                      
         ST    RE,MPAHOOK                                                       
*                                                                               
PROC20   CLC   8(3,R2),=CL3'EVN'   PROGRAM RECORD RETRIEVAL                     
*        BNE   *+12                                                             
*        BAS   RE,PROGS                                                         
         BNE   PROC21                                                           
         GOTOR SUBR03,DMCB,('PROGSE',(RC)),(R2)                                 
         B     PROC22                                                           
                                                                                
PROC21   XC    AVIEWTYP,AVIEWTYP                                                
         OC    PDVIEWS,PDVIEWS                                                  
         BZ    PROC21D                                                          
         LA    RE,PDVIEWS          LIST OF REQUESTED VIEWING TYPES              
         CLI   0(RE),X'FF'                                                      
         BE    PROC21D                                                          
PROC21A  ST    RE,AVIEWTYP                                                      
*                                                                               
         CLC   =C'OOH',1(R2)                                                    
         BE    PROC21AA                                                         
         CLI   0(RE),SRCLOLQ       SKIP ALL OOH/OOHC VTYPES FOR                 
         BE    PROC21E             NON OOH/OOHC REQUESTS                        
         CLI   0(RE),SRCLOSQ                                                    
         BE    PROC21E                                                          
         CLI   0(RE),SRCLO3Q                                                    
         BE    PROC21E                                                          
         CLI   0(RE),SRCLO7Q                                                    
         BE    PROC21E                                                          
         CLI   0(RE),SRCLOCLQ                                                   
         BE    PROC21E                                                          
         CLI   0(RE),SRCLOCSQ                                                   
         BE    PROC21E                                                          
         CLI   0(RE),SRCLOC3Q                                                   
         BE    PROC21E                                                          
         CLI   0(RE),SRCLOC7Q                                                   
         BE    PROC21E                                                          
         B     PROC21C                                                          
*                                                                               
PROC21AA CLC   =C'OOH ',1(R2)      TEMP FOR OOH MIT TAPE (SPEC-30665)           
         BNE   PROC21B                                                          
         CLI   0(RE),SRCLOLQ                                                    
         BE    PROC21C                                                          
         CLI   0(RE),SRCLOSQ                                                    
         BE    PROC21C                                                          
         CLI   0(RE),SRCLO3Q                                                    
         BE    PROC21C                                                          
         CLI   0(RE),SRCLO7Q                                                    
         BE    PROC21C                                                          
         B     PROC21E                                                          
*                                                                               
PROC21B  CLC   =C'OOHC',1(R2)      TEMP FOR OOH MIT TAPE (SPEC-30665)           
         BNE   PROC21C                                                          
         CLI   0(RE),SRCLOCLQ                                                   
         BE    PROC21C                                                          
         CLI   0(RE),SRCLOCSQ                                                   
         BE    PROC21C                                                          
         CLI   0(RE),SRCLOC3Q                                                   
         BE    PROC21C                                                          
         CLI   0(RE),SRCLOC7Q                                                   
         BNE   PROC21E                                                          
*                                                                               
PROC21C  BRAS  RE,SVTFILT          FILTER ON REQUESTED SOURCE+VTYPE             
         BNE   PROC21E                                                          
*                                                                               
PROC21D  BAS   RE,MULSBK           DEMO RECORD RETRIEVAL                        
*                                                                               
PROC21E  ICM   RE,15,AVIEWTYP                                                   
         BZ    PROC21F                                                          
         LA    RE,1(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   PROC21A                                                          
*                                                                               
PROC21F  CLI   PODBMED,C'N'        NETWORK CABLE                                
         BNE   *+8                                                              
         CLI   DBSELSTA+4,C'C'     FOR CABLE WE PASS THE DAY/TIME LIST          
         BNE   *+8                                                              
         CLI   ODYTSET,C'Y'                                                     
         BE    PROC22              STOP LOOP THRU DAYS/TIMES                    
*                                                                               
         LA    R5,5(R5)            BUMP DAY TIME TABLE                          
         CLI   0(R5),X'99'         END OF TABLE?                                
         BNE   PROC16                                                           
*                                                                               
PROC22   LA    R5,PODDAYTM                                                      
         OC    PODNETAD,PODNETAD   ARE WE IN ALL MODE                           
         BNZ   PROC26              YES PROCESS ALL STATION TABLE                
*                                                                               
PROC24   LA    R3,PODNETL(R3)      BUMP NETWORK TABLE                           
         CLC   0(3,R3),=CL3'ZZZ'                                                
         BNE   PROC24A                                                          
         BAS   RE,GETALLST         CREATE ALL STATION TABLE                     
         B     PROC15                                                           
PROC24A  OC    0(3,R3),0(R3)       FORCE EOT TO AVOID LOOP                      
         BZ    PROC24B                                                          
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    PROC24B                                                          
*                                                                               
         BRAS  RE,TSTNET           TEST IF CERTAIN NETWORKS ARE ZZZ-C           
         BE    PROC24              WILL BE PROCESSED WITH ZZZ-C                 
         B     PROC16                                                           
*                                                                               
PROC24B  L     R3,APODNET                                                       
         CLC   0(3,R3),=CL3'ZZZ'                                                
         BNE   PROC28                                                           
         BAS   RE,GETALLST         CREATE ALL STATION TABLE                     
         B     PROC30                                                           
*                                                                               
PROC26   BRAS  RE,RESTCNA          REPORT CERTAIN STATIONS IF CNAD/CNAW         
         LA    R3,PODNETL(R3)      BUMP ALL NETWORK TABLE                       
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   PROC16                                                           
         ICM   R3,15,PODNETAD      RELOAD REGULAR STATION TABLE                 
         XC    PODNETAD,PODNETAD                                                
         XC    PODNETMB,PODNETMB                                                
         B     PROC24                                                           
*                                                                               
PROC28   ICM   RE,15,PODMPTR       GET MARKET POINTER                           
         CLI   0(RE),X'FF'         ANY MORE MARKETS?                            
         BE    PROC30              NO, DONE                                     
         CLC   =C'CCV',1(R2)                                                    
         BNE   *+14                                                             
         OC    0(2,RE),0(RE)                                                    
         BZ    PROC30                                                           
         CLC   =C'NMI',1(R2)                                                    
         BE    PROC03                                                           
         CLC   =C'EMI',1(R2)                                                    
         BE    PROC03                                                           
         L     RE,APODNET          & GO PROCESS IT                              
         LA    RF,LPODNET                                                       
         XCEF                                                                   
         ICM   RE,15,PODMPTR       RESTORE RE                                   
         B     PROC03                                                           
*                                                                               
PROC30   MVI   HUTSW,0                                                          
         BRAS  RE,WKLYSPT          INCREMENT WEEKLY SPOT BOOK                   
         BE    PROC40              AND SKIP OTHER BOOK INCREMENTS               
*                                                                               
         ZIC   RF,PDSBOOK+1                                                     
         LA    RF,1(RF)                                                         
         STC   RF,PDSBOOK+1                                                     
*                                                                               
         CLI   PDSBOOK+1,53        HAVE WE EXCEEDED A YEAR?                     
         BNH   PROC32              NO                                           
         MVI   PDSBOOK+1,1         YES, RESET WEEK                              
*                                                                               
         ZIC   RF,PDSBOOK          YES, INCREMENT YEAR                          
         LA    RF,1(RF)                                                         
         STC   RF,PDSBOOK                                                       
*                                                                               
PROC32   BRAS  RE,TSTCNA           TEST CNAD/CNAW                               
         BE    PROC38                                                           
         CLI   PODBMED,C'V'        PIV WEEKLY                                   
         BE    PROC38                                                           
         CLI   PODBMED,C'W'        NHT WEEKLY                                   
         BE    PROC38                                                           
         CLC   =C'REN',PDSOURCE    RENTRAK IS WEEKLY                            
         BE    PROC38                                                           
         CLI   PODBMED,C'N'        OTHER MEDIA ONLY HAVE MONTHLIES              
         BNE   PROC34                                                           
         CLC   8(3,R2),=CL3'NAD'   CHECK FILE FOR NAD/NHT                       
         BNE   PROC38                                                           
         CLC   =C'CNAW',PDSOURCE   CNAW IS WEEKLY                               
         BE    PROC38                                                           
*                                                                               
PROC34   CLC   =C'CSI',PDSOURCE    CANDIAN NSI IS WEEKLY                        
         BNE   PROC35                                                           
         CLI   PDSBOOK,96          STARTING WITH 1996                           
         BL    PROC36                                                           
         CHI   RF,53                                                            
         B     PROC37                                                           
*                                                                               
PROC35   CLC   =C'BBM',PDSOURCE    BBM CAN BE WEEKLY                            
         BNE   PROC36                                                           
         CLI   SVBBMWK,C'Y'                                                     
         BNE   PROC36                                                           
         CHI   RF,53                                                            
         B     *+8                                                              
*                                                                               
PROC36   CHI   RF,12               CHECK FOR YEAR BREAK                         
PROC37   BNH   PROC40                                                           
         ZIC   RF,PDSBOOK                                                       
         LA    RF,1(RF)                                                         
         STC   RF,PDSBOOK                                                       
         MVI   PDSBOOK+1,X'01'                                                  
*                                                                               
PROC38   CLC   8(3,R2),=CL3'NTI'   CHECK FILE FOR NTI                           
         BE    *+10                                                             
         CLC   8(4,R2),=CL4'OOH '  CHECK FILE FOR OOH                           
         BE    *+10                                                             
         CLC   8(3,R2),=CL3'REN'   CHECK FILE FOR REN                           
         BE    *+10                                                             
         CLC   8(5,R2),=CL5'ACM  '   CHECK FILE FOR ACM                         
         BE    *+10                                                             
         CLC   8(5,R2),=CL5'OOHC '   CHECK FILE FOR OOHC                        
         BE    *+10                                                             
         CLC   8(5,R2),=CL5'RLD  ' CHECK FILE FOR RLD                           
         BE    *+10                                                             
         CLC   8(3,R2),=CL3'NHW'   CHECK FILE FOR NHW                           
         BE    *+10                                                             
         CLC   8(3,R2),=CL3'HPW'   CHECK FILE FOR NHW NATIONAL SAMPLE           
         BE    *+10                                                             
         CLC   8(3,R2),=CL3'NAW'   CHECK FILE FOR NAW                           
         BE    *+10                                                             
         CLC   8(3,R2),=CL3'EVN'   CHECK FILE FOR PIV                           
         BNE   PROC40                                                           
         GOTO1 NETUNBK,DMCB,(C'W',PDSBOOK),DUB,GETDAY,ADDAY,GETBROAD            
         CLI   DUB,X'FF'           CHECK FOR YEAR BREAK                         
         BNE   PROC40                                                           
         ZIC   RF,PDSBOOK                                                       
         LA    RF,1(RF)                                                         
         STC   RF,PDSBOOK                                                       
         MVI   PDSBOOK+1,X'01'                                                  
*                                                                               
PROC40   CLI   0(R2),0             NO RANGE FOR BOOKS                           
         BE    PROC42              DON'T READ DATA AGAIN, AVOID DOUBLES         
         MVC   RETADD,=A(PROC15)                                                
         CLC   PDSBOOK,PDEBOOK                                                  
         BNH   PROC60                                                           
*                                                                               
PROC42   CLI   ENMIFLAG,0                                                       
         BZ    PROC44                                                           
         LA    R2,PODBLNQ*3(R2)           SKIP RANGE AND MPA                    
         B     PROC46                                                           
*                                                                               
PROC44   BRAS  RE,RESTCNA          REPORT CERTAIN STATIONS IF CNAD/CNAW         
         CLI   0(R2),0                                                          
         BE    *+8                                                              
         LA    R2,PODBLNQ(R2)      BUMP BOOK TABLE                              
         LA    R2,PODBLNQ(R2)      BUMP BOOK TABLE                              
*                                                                               
PROC46   CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    PROC52                                                           
         MVC   PODBD(PODBLNQ),0(R2)                                             
         ZIC   RE,PDIBKSEQ         INCR INPUT BOOK SEQ                          
         LA    RE,1(RE)            FOR INPUT SEQ ROUTINES                       
         STC   RE,PDIBKSEQ                                                      
*                                                                               
         MVI   EBOPT,C'N'                                                       
         TM    13(R2),X'80'        DID WE REQUEST EST BOOK?                     
         BZ    PROC48                                                           
         MVI   EBOPT,C'Y'          SET FLAG ON                                  
         NI    13(R2),X'7F'        TURN HOB OFF                                 
*                                                                               
PROC48   MVC   PDSBOOK,12(R2)                                                   
         MVC   PDEBOOK,12(R2)                                                   
*                                                                               
         L     RE,PDADEMTB                                                      
         LA    RF,REALDEMS                                                      
         LA    R4,PDNDEMS                                                       
*                                                                               
PROC50   MVC   1(1,RE),0(RF)       COPY ORIGINAL DEMO AGAIN                     
         AH    RE,LNDEMCOD                                                      
         LA    RF,1(RF)                                                         
         BCT   R4,PROC50                                                        
*                                                                               
         CLI   0(R2),0             IS BOOK A RANGE                              
         BE    *+10                NO, THEN START AND END BOOK EQUAL            
         MVC   PDEBOOK,PODBLNQ+12(R2)                                           
         MVC   RETADD,=A(PROC08)   SAVE RETURN ADDRESS                          
         B     PROC60                                                           
*                                                                               
PROC52   L     RE,APDPDFLT                                                      
         OC    0(L'PDPDFLT,RE),0(RE)                                            
         BZ    PROC54              SKIP IF NOT USING PDF FILTER                 
         LA    RF,L'PDPDFLT        ELSE, INITIALIZE FOR 2ND PASS                
         XCEFL                                                                  
*                                                                               
         ICM   RE,15,PRGTPTR                                                    
         MVI   0(RE),X'FF'         MARK END OF TABLE                            
         B     PROC02                                                           
*                                                                               
PROC54   ICM   RE,15,PRGLPTR       GET PROGRAM GROUP POINTER                    
         CLI   0(RE),X'FF'         ANY MORE LEFT?                               
         BNE   PROC01              YES                                          
*                                                                               
PROC55   L     RE,PDADEMTB         PRINT THE REPORT                             
         LA    RF,REALDEMS                                                      
         LA    R4,PDNDEMS                                                       
*                                                                               
PROC56   MVC   1(1,RE),0(RF)       COPY ORIGINAL DEMO AGAIN                     
         AH    RE,LNDEMCOD                                                      
         LA    RF,1(RF)                                                         
         BCT   R4,PROC56                                                        
*                                                                               
PROC561  BRAS  RE,GTCSONLY         PROCESS COMSCORE NETWORKS ONLY               
         JNE   PROC562                                                          
*                                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R4)                                                 
         J     PROC561                                                          
*                                                                               
PROC562  TM    REQIND,REQITRN      TRANSMITTING?                                
         BZ    PROC57              NO                                           
         GOTO1 GENEDICT            YES, GENERATE CONTROL CARDS                  
*                                                                               
PROC57   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
*                                                                               
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                  CLOSE THE BOX                                
         ICM   R3,15,TWADCONS                                                   
         BZ    PROC58                                                           
         ICM   R3,15,TISPRACT-TWADCOND(R3) IF A(PR ACT SW) RESOLVED             
         BZ    PROC58                                                           
         CLI   0(R3),C'N'          NOTHING PRINTED                              
         BE    PROC58                                                           
*                                                                               
         L     R3,ASPOOLD                                                       
         L     R1,GENHEAD          RESET HOOK ADDRESS IN SPOOL BLOCK            
         ST    R1,HEADHOOK-SPOOLD(R3)                                           
         L     R3,ABOX-SPOOLD(R3)                                               
         USING BOXD,R3                                                          
         CLI   BOXYORN,C'Y'                                                     
         BNE   PROC58                                                           
         MVI   BOXREQ,C'C'                                                      
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
PROC58   L     R3,PDAOFFBF         FREE UP OFFLINE BUFFER                       
         L     R5,0(R3)            R5=A(LENGTH OF BUFFER)                       
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R5)                                    
         L     R3,ANAMPOOL         FREE UP NAME POOL BUFFER                     
         L     R5,LNAMPOOL                                                      
         GOTO1 (RF),(R1),C'FREE',(R3),(R5)                                      
         L     RE,AIO3                                                          
         XCEF  (RE),2000                                                        
         B     PROCX                                                            
*                                                                               
PROC60   L     RE,APODMKTL         START MARKET LIST AT BEGINNING               
         STCM  RE,15,PODMPTR       SAVE THE ADDRESS                             
*                                                                               
         L     RE,APODMKTL         SET POINTER AT BEGINNING OF LIST             
         CLI   0(RE),X'FF'                                                      
         BE    PROC62                                                           
*                                                                               
         MVC   PODMPAMK,0(RE)                                                   
         GOTOR SUBR02,DMCB,('RDDMKTE',(RC))                                     
*                                                                               
PROC62   ICM   RE,15,RETADD                                                     
         BR    RE                                                               
*                                                                               
PROCX    BRAS  RE,PUTCOM           PUT COMSCORE REQUEST                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL PROCESS ROUTINES                                  *         
***********************************************************************         
GETALLST LR    R0,RE                                                            
         STCM  R3,15,PODNETAD                                                   
         OC    PODNETMB,PODNETMB                                                
         BNZ   GETA02                                                           
         MVC   PODNETMB,4(R3)      MEDIA AND BOOK TYPE                          
*                                                                               
*                                  GET ALL PIV STATIONS                         
         GOTO1 =A(GTALLSTA),DMCB,0,(R9),(RC),RR=RELO                            
*                                                                               
*                                  GET ALL NTI STATIONS                         
         GOTO1 (RF),(R1),(X'01',X'000000'),(R9),(RC)                            
*                                                                               
GETA02   MVC   AIO,AIO1                                                         
         LR    RE,R0                                                            
         L     R3,AZZZLIST                                                      
         LA    R3,10(R3)                                                        
         CLC   8(3,R2),=CL3'EVN'   DO WE WANT PROGRAM STATIONS                  
         BNER  RE                                                               
*                                                                               
*        L     R3,AIO2                                                          
*        LA    R3,1330(R3)                                                      
         L     R3,AZZZPIV                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        GET DEMO INFORMATION, IF NEEDED                              *         
***********************************************************************         
*                                                                               
MULSBK   NTR1                                                                   
         BRAS  RE,SETCNA           REPORT CERTAIN STATIONS IF CNAD/CNAW         
*                                                                               
         CLC   =C'NAD',8(R2)       IF INTERNAL FILE IS NAD                      
         BNE   MULS0010                                                         
         CLI   4(R3),C'C'          AND CABLE STATION REQUESTED                  
         BNE   MULS0010                                                         
         CLC   =C'CNA',1(R2)       CABLE NAD HAS TO BE SOURCE CNAD,CNAW         
         BNE   MULSX               EXIT IF OTHER SOURCES                        
*                                                                               
MULS0010 CLC   =C'NHC',1(R2)       SOURCE NHC IS FOR CBLE STATIONS ONLY         
         BE    *+14                                                             
         CLC   =C'HPC',1(R2)       SOURCE NHC FROM NATIONAL SAMPLE              
         BNE   MULS0020                                                         
         BRAS  RE,CKHCAB           CK HISPANIC CABLE STATION                    
         BNE   MULSX                                                            
*                                                                               
MULS0020 BRAS  RE,MULSBK2          MASSAGE VARIOUS FIELDS                       
*                                                                               
MULS31   BAS   RE,ONEDT                                                         
*                                                                               
MULSX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        READ FOR SPECIFIC NETWORK/BOOK/DAY/TIME                      *         
***********************************************************************         
*                                                                               
ONEDT    NTR1                                                                   
         OC    PODNETAD,PODNETAD     ARE WE IN ALL MODE?                        
         BZ    ONED01                                                           
         CLI   PDZZZTYP,C'C'         ZZZ-C (ALL CABLE NETWORKS)                 
         BNE   ONED01                                                           
         CLC   =C'FUS',PDSOURCE                                                 
         BE    ONED01                                                           
         CLC   =C'5830',DBSELSTA     TBS INCLUDING ATLANTA                      
         BE    *+14                                                             
         CLC   =C'TBS ',DBSELSTA                                                
         BNE   ONED01                                                           
         CLC   DBSELBK,=X'6528'        EXCLUDE IT AFTER OCT1/01                 
         BNL   ONEDX                 (INCLUDE TBSN=7237 INSTEAD)                
*                                                                               
ONED01   CLI   PDSIDOPT,C'Y'       IF WE ARE DOING SIDS                         
         BNE   ONED02                                                           
         OC    PODMPAMK,PODMPAMK   AND HAVE A SPILL MARKET                      
         BZ    ONED02                                                           
         SR    RE,RE                                                            
         ICM   RE,3,PODMPAMK                                                    
         SHI   RE,400                                                           
         OC    PDSIDMKT,PDSIDMKT   AND NO AGENCY MARKET                         
         BNZ   *+8                                                              
         STCM  RE,3,PDSIDMKT      DEFAULT TO SPILL MARKET                       
         XC    PODMPAMK,PODMPAMK   KILL SPILL FOR SIDS                          
*                                                                               
ONED02   SR    R5,R5                                                            
         GOTOR SUBR01,DMCB,('CABFORME',(RC))                                    
         MVC   DBAUTH,TWAAUTH                                                   
         CLC   =C'TP',PDSOURCE     REGULAR TP                                   
         BNE   *+8                                                              
         MVI   DBTPTT,C'T'                                                      
*                                                                               
         CLC   =C'T3',PDSOURCE     SPECIAL TP FOR CANADA                        
         BE    *+10                                                             
         CLC   =C'T4',PDSOURCE     SPECIAL TP                                   
         BNE   ONED04                                                           
         MVI   DBTPTT,C'P'                                                      
* THIS LINE WAS REMOVED ON 2/25/10. THIS OPTION BEING SET TRANSLATED            
* INTO NO DATA BEING RETURNED BETWEEN 2 AM AND 6 AM. MARIA BELIEVES             
* THIS NO LONGER APPLIES TO TODAY'S DATA.                                       
***      MVI   DBDAYOPT,C'Y'                                                    
*                                                                               
ONED04   CLC   =C'OPA',PDSOURCE                                                 
         BNE   *+8                                                              
         MVI   DBBEST,C'A'         ALWAYS ALL RECDS FOR OVERNIGHT PAV           
         CLC   =C'NSI-W',PDSOURCE                                               
         BNE   *+8                                                              
         MVI   DBBEST,C'R'         RESRCH APPLICATION, DONT ADJUST BOOK         
         CLC   =C'PAV',PDSOURCE                                                 
         BNE   ONED06                                                           
         MVC   DBBEST,PDBSTOPT                                                  
         CLI   DBSELDAY,X'7F'                                                   
         BNE   ONED06                                                           
         MVI   DBSELDAY,X'FF'                                                   
*                                                                               
ONED06   CLC   =C'MPA',PDSOURCE    ONLY FOR MPA FILE, NSI AND ARB               
         BNE   ONED12                                                           
         CLI   PRECOPT,C'N'        DEFAULT IS X'00'                             
         BE    *+8                 IF 'N', DON'T SET TO YES                     
         MVI   PRECOPT,C'Y'                                                     
         MVI   PDBKTYP,X'00'       NOT NEEDED FOR MPA                           
         MVI   DBBTYPE,X'00'                                                    
         XC    DBSELPRG,DBSELPRG                                                
         L     R4,APDNTIFT                                                      
         CLI   0(R4),0                                                          
         BE    ONED14                                                           
         LA    RF,5                POSSIBLE 5 CHAR PROGRAM                      
         LA    RE,4(R4)            START FROM LAST CHAR                         
*                                                                               
ONED08   CLI   0(RE),C' '          CHECK FOR SPACE                              
         BNE   ONED10                                                           
         BCTR  RE,0                                                             
         BCT   RF,ONED08                                                        
*                                                                               
ONED10   BCTR  RF,0                MINUS ONE FOR EX                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),0(0,R4)      USED TO TEST ONE PROGRAM                     
         CVB   R1,DUB              DON'T NEED IT ANYMORE                        
         STCM  R1,3,DBSELPRG                                                    
         MVC   PDGPROG,5(R4)       GET NAME, IF ANY                             
         LR    RF,R1                                                            
         CLI   4(R4),C' '                                                       
         BNE   ONED14                                                           
         CVD   R1,DUB                                                           
         UNPK  0(5,R4),DUB                                                      
         OI    4(R4),X'F0'                                                      
*                                                                               
ONED12   CLC   =C'MPA',PDSOURCE                                                 
         BE    ONED14                                                           
         CLC   =C'MI',PDSOURCE+1                                                
         BNE   ONED16                                                           
*                                                                               
ONED14   DS    0H                                                               
         XC    DBSELRMK,DBSELRMK                                                
         OC    PODMPAMK,PODMPAMK                                                
         BZ    ONED16                                                           
         MVC   DBSELRMK,PODMPAMK                                                
*                                                                               
ONED16   DS    0H                                                               
         BRAS  RE,MKEXDEMS         MAKE EXTRA DEMOS                             
         CLI   PDSIDOPT,C'Y'                                                    
         BNE   ONED22                                                           
         XC    DBSELSTA,DBSELSTA                                                
         CLI   PDMGROPT,C'Y'                                                    
         BE    ONED18                                                           
         BAS   RE,NSID                                                          
         B     ONEDX                                                            
*                                                                               
ONED18   L     R2,APODNET               POINT TO STATION LIST                   
         MVC   PDQMGR(5),0(R2)                                                  
         XC    PDSVMGKY,PDSVMGKY                                                
         XC    PDSIDMKT,PDSIDMKT        CLEAR MARKETS                           
*                                                                               
ONED20   GOTOR SUBR03,DMCB,('NEXTME',(RC)),(R2)                                 
         XC    DBSELSTA,DBSELSTA                                                
         MVC   SVDAYTME,DBSELDAY                                                
         OC    PDSIDMKT,PDSIDMKT        ARE WE DONE?                            
         BZ    ONEDX                                                            
         BAS   RE,NSID                  PROCESS IT                              
         MVC   DBSELDAY(5),SVDAYTME                                             
         B     ONED20                                                           
*                                                                               
ONED22   MVI   PDMULTRD,C'N'       NOT MULTIPLE READ                            
         CLI   DBSELSTA+4,C'C'                                                  
         BNE   *+8                                                              
         CLI   PDNUMQH,1                                                        
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'Q'                                                  
         L     RE,APDDEML1                                                      
         XC    0(4,RE),0(RE)                                                    
         L     RE,APDDEML2                                                      
         XC    0(4,RE),0(RE)                                                    
*                                                                               
         CLI   PDDUROPT,C'M'       ALWAYS DO IMP BASED FOR THIS                 
         BE    ONED23                                                           
         CLI   PDMETHOD,5          METHOD=SOURCE FOR SPOT/REP                   
         BE    ONED23                                                           
         CLC   =C'NAD',PDSOURCE    VARIABLE BASE FOR NAD                        
         BE    *+10                                                             
         CLC   =C'NAW',PDSOURCE    VARIABLE BASE FOR NAW                        
         BE    *+10                                                             
         CLC   =C'CNA',PDSOURCE    VARIABLE BASE FOR CNAD AND CNAW              
         BE    *+10                                                             
         CLC   =C'ACM  ',PDSOURCE    VARIABLE BASE FOR COMM AVERAGE             
         BE    *+10                                                             
         CLC   =C'OOHC ',PDSOURCE    VARIABLE BASE FOR OOH COMM AVERAGE         
         BE    *+10                                                             
         CLC   =C'MXM  ',PDSOURCE    VARIABLE BASE FOR MINUTE BY MINUTE         
         BE    *+10                                                             
         CLC   =C'REN',PDSOURCE                                                 
         BE    *+10                                                             
         CLC   =C'NTI',PDSOURCE    VARIABLE BASE FOR NTI                        
         BE    *+10                                                             
         CLC   =C'OOH ',PDSOURCE                                                
         BNE   ONED24                                                           
         CLI   PDBASE,C'B'                                                      
         BE    *+8                                                              
         CLI   PDBASE,C'I'                                                      
         BNE   ONED24                                                           
ONED23   GOTOR SUBR01,DMCB,('IMPDEME',(RC))                                     
*                                                                               
ONED24   DS    0H                                                               
         CLC   =C'IUN',PDSOURCE                                                 
         BNE   ONED26                                                           
         OC    PODPSTRT+2(2),PODPSTRT+2 FILTER ON START AND END                 
         BZ    *+10                                                             
         MVC   DBSEL1WK,PODPSTRT   NO - MOVE IN THE START                       
         MVI   DBBTYPE,0                                                        
*                                                                               
         XC    RINVEXT,RINVEXT                                                  
         LA    RE,RINVEXT       GET REP EXTENSION ADDRESS                       
         USING DBXINVWK,RE                                                      
         MVC   DBXIWID,=C'RINV'                                                 
         DROP  RE                                                               
         LR    R3,RE                                                            
         GOTOR SUBR02,DMCB,('PUTEXTE',(RC)),(R3)                                
*                                                                               
ONED26   CLC   =C'NAD',PDSOURCE                                                 
         BNE   *+10                                                             
         XC    SVCRDATE,SVCRDATE                                                
         MVC   SVDBTYPE,DBBTYPE                                                 
         CLI   DBBTYPE,C'U'                                                     
         BNE   ONED30                                                           
         CLI   DBSELMED,C'O'                                                    
         BE    ONED30              DOESN'T APPLY TO OVERNIGHTS                  
         CLI   ALLDT,X'03'                                                      
         BE    ONED28                                                           
         MVI   DBBTYPE,0                                                        
         B     ONED30                                                           
*                                                                               
ONED28   CLC   DBSELSTA+3(2),=C'PN'                                             
         BNE   *+8                                                              
         MVI   DBSELSTA+3,C' '                                                  
         MVI   DBSELDAY,0                                                       
         XC    DBSELTIM(4),DBSELTIM                                             
         MVI   PDDUROPT,C'Y'                                                    
         MVI   DBPRGDUR,C'Y'                                                    
         MVI   DBBEST,C'A'                                                      
         MVI   DBSELDUR,X'FF'                                                   
*                                                                               
ONED30   BAS   RE,GETDEMS                                                       
*                                                                               
         CLC   =C'NAD',PDSOURCE                                                 
         BE    ONED32                                                           
         CLC   =C'NHT',PDSOURCE                                                 
         BE    ONED32                                                           
         CLC   =C'HPM',PDSOURCE                                                 
         BE    ONED32                                                           
         CLC   =C'NAW',PDSOURCE                                                 
         BE    ONED32                                                           
         CLC   =C'HPW',PDSOURCE                                                 
         BE    ONED32                                                           
         CLC   =C'CNAW',PDSOURCE                                                
         BNE   ONED36                                                           
         GOTOR SUBR02,DMCB,('ANYMVGE',(RC)) ANY MOVIE GOER DEMOS?               
         BNE   ONED35              DON'T READ MVGO UNLESS WE REQUEST IT         
         MVI   DBBTYPE,C'M'        NOW READ MVGO                                
         MVI   DBSTYPE,0           RESET STYPE                                  
         B     ONED34                                                           
*                                                                               
ONED32   CLI   DBBTYPE,0                                                        
         BNE   ONED36                                                           
         MVI   DBBTYPE,1                                                        
         MVI   PDMULTRD,C'Y'       USING DEMAND MULTIPLE TIMES                  
         XC    SVCRDATE,SVCRDATE                                                
*                                                                               
ONED34   GOTO1 DEMAND,DMCB,DBLOCK,ONEHOOK                                       
         CLC   =C'CNAW',PDSOURCE   READ ONLY BTYPES 'N' AND 'M'. DONE           
         BE    ONED35                                                           
         ZIC   RE,DBBTYPE                                                       
         LA    RE,1(RE)                                                         
         STC   RE,DBBTYPE                                                       
         CLI   DBBTYPE,15                                                       
         BL    ONED34                                                           
*                                  TEMP FIX FOR WEEKLY NAD UNIVERSES            
         CLC   =C'NAW',PDSOURCE                                                 
         BNE   ONED34A                                                          
         GOTOR SUBR02,DMCB,('ANYMVGE',(RC)) ANY MOVIE GOER DEMOS?               
         BNE   ONED34B             DON'T READ MVGO UNLESS WE REQUEST IT         
ONED34A  MVI   DBBTYPE,C'M'                                                     
*        GOTO1 (RF),(R1),DBLOCK,ONEHOOK                                         
         GOTO1 DEMAND,DMCB,DBLOCK,ONEHOOK                                       
*                                                                               
ONED34B  MVI   DBBTYPE,C'O'                                                     
         GOTO1 DEMAND,DMCB,DBLOCK,ONEHOOK                                       
         MVI   DBBTYPE,C'P'                                                     
         GOTO1 (RF),(R1),DBLOCK,ONEHOOK                                         
         MVI   DBBTYPE,C'Q'                                                     
         GOTO1 (RF),(R1),DBLOCK,ONEHOOK                                         
ONED35   MVI   PDMULTRD,C'N'                                                    
*                                                                               
ONED36   CLC   =C'MPA',PDSOURCE                                                 
         BNE   ONEDX                                                            
         LA    R4,21(R4)           NEXT PROGRAM                                 
         L     RE,APDNTIFT                                                      
         CLI   0(RE),0                                                          
         BE    ONEDX                                                            
         LA    RE,L'PDNTIFT(RE)    GET TO END                                   
         CR    R4,RE                                                            
         BNL   ONEDX                                                            
         OC    0(5,R4),0(R4)                                                    
         BZ    ONEDX                                                            
         LA    RF,5                                                             
         B     ONED10                                                           
*                                                                               
ONEDX    B     XIT                                                              
*                                                                               
NSID     NTR1                                                                   
         MVC   SVSTAT,0(R3)        SAVE CURRENT STATION                         
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         ICM   R1,15,PDSDBFAD      SIDBUFF ADDRESS                              
         LA    R0,5                NUMBER OF 256 BLOCKS                         
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         LA    RF,PDSIDMKT                                                      
         STCM  RF,15,NSIDPTR                                                    
*                                                                               
         ICM   R4,15,PDSDBFAD      INITIALIZE BLOCK                             
         USING SRBLKD,R4                                                        
         MVC   SRASIR,PDSDRCAD     SIDREC ADDRESS                               
         MVC   SRACOM,ACOMFACS                                                  
         MVC   SRACLPAC,CLPACK                                                  
         MVC   SRAMSUNP,MSUNPK                                                  
         MVC   SRADYUNP,UNDAY                                                   
         MVC   SRAUNTIM,UNTIME                                                  
         MVC   SRAMASTC,TWAMASTC                                                
*                                                                               
         MVC   SRSELSCH,PDSDSCHM                                                
         MVC   SRSELAM,PDBAGYMD      PICK UP SELECTIONS                         
         MVC   SRSELAGY,AGENCY                                                  
         MVC   SRSELMED,DBSELMED                                                
         MVI   SRSELCTY,C'U'                                                    
*                                                                               
         CLI   SRSELMED,C'C'                                                    
         BNE   *+12                                                             
         MVI   SRSELMED,C'T'                                                    
         MVI   SRSELCTY,C'C'                                                    
*                                                                               
         MVC   SRSELPER,PDSDPER                                                 
         MVC   SRSELYR,PDSDYEAR                                                 
         OC    PDSIDMKT,PDSIDMKT   NOT A PLAIN STATION                          
         BNZ   NSID02                SIDMKT SET BY MKTGRP OR MKT STA            
         OC    PODMPAMK,PODMPAMK   MAYBE WE HAVE ONE IN OPTIONS                 
         BZ    *+14                                                             
         MVC   PDSIDMKT(2),PODMPAMK  USE THE ONE IN OPTIONS                     
         B     NSID02                                                           
*                                                                               
         OC    SVSTAT,SVSTAT       SPECIFIC STATION                             
         BZ    NSID02                                                           
         MVC   WORK(4),=4X'F0'                                                  
         MVC   DUB(5),SVSTAT                                                    
         CLI   DUB+4,C' '                                                       
         BH    *+8                                                              
         MVI   DUB+4,C'T'                                                       
         GOTO1 MSPACK,DMCB,WORK,DUB,WORK+4                                      
         MVC   SRSELSTA,WORK+6                                                  
         B     NSID04                                                           
*                                                                               
NSID02   MVC   SRSELMKT,0(RF)      OR NEXT MKT IN MKTGRP                        
*                                                                               
NSID04   MVC   SRSELDPT,PDSDDPTL   USE DAYPART LIST                             
         MVC   SRSELPRG,PDSDPTPL   USE PROGRAM TYPE LIST                        
         CLI   PODDAYTM,X'FF'      TEST DAY=ALL                                 
         BE    *+10                                                             
         MVC   SRSELDAY,DBSELDAY                                                
         CLI   PODDAYTM+1,X'FF'    TEST TIME=ALL                                
         BE    *+10                                                             
         MVC   SRSELTIM,DBSELTIM                                                
*                                                                               
NSID06   DS    0H                                                               
         GOTOR SUBR01,DMCB,('SWCHSPTE',(RC))                                    
         GOTO1 RANSID,DMCB,(R4)                                                 
         GOTOR SUBR01,DMCB,('SWCHBCKE',(RC))                                    
*                                                                               
         CLI   SRMODE,SRONEREC                                                  
         BNE   NSID26                                                           
         L     R5,ASORTREC                                                      
         USING SORTDATA,R5                                                      
         MVC   SORTDPT,SRDPTNM     DAYPART CODE                                 
         MVC   PDDP,SRDPTNM                                                     
         MVC   SORTTYP,SRPRGNM     PROGRAM TYPE EXPANSION                       
         MVC   PDPRGTYP,SRPRGNM    PROGRAM TYPE EXPANSION                       
         MVC   PDCOST,SRACTCS1     SID COST                                     
         MVC   PDMKTNAM,SRERMNM    MARKET NAME                                  
*                                                                               
         MVC   SORTPER,SRACTPER       PERIOD NUMBER (WITH X'80')                
         MVC   SORTPER+1(4),SRSELPER  PERIOD DESCRIPTION                        
*                                                                               
         MVC   SORTUPDT,SPACES                                                  
         CLI   ANYUP,C'Y'          UPGRADE CONTROL                              
         BNE   NSID10                                                           
*                                                                               
         MVC   SORTPROG,SRACTPRO   USER CAN SPECIFY PROGRAM(S)                  
         CLI   SORTPROG,C' '                                                    
         BH    *+10                                                             
         MVC   SORTPROG,=CL16'VARIOUS'                                          
*                                                                               
         MVC   BLOCK(32),SPACES                                                 
         MVC   BLOCK(16),SRUPDATA                                               
         CLI   SRUPBOOK,0          IS A FROM BOOK SPECIFIED?                    
         BE    NSID08                                                           
         MVC   BLOCK+17(3),=C'BK='                                              
         ZIC   R1,SRUPBOOK+1       YES - SO SHOW THAT AS WELL                   
         BCTR  R1,0                                                             
         MHI   R1,3                                                             
         LA    R1,MONTHS(R1)                                                    
         MVC   BLOCK+20(3),0(R1)                                                
         EDIT  (1,SRUPBOOK),(2,BLOCK+23)                                        
*                                                                               
NSID08   DS    0H                                                               
         GOTO1 SQUASHER,DMCB,BLOCK,32                                           
         MVC   SORTUPDT,BLOCK                                                   
*                                                                               
NSID10   LAY   RE,SPOTDAYS         NEED REP-TYPE DAY                            
         SR    RF,RF                                                            
*                                                                               
NSID12   CLC   0(1,RE),SRACTDAY                                                 
         BE    NSID14                                                           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   NSID12                                                           
         LA    RF,9                                                             
*                                                                               
NSID14   STC   RF,SORTDP                                                        
         MVC   SORTDP+1(4),SRACTTIM                                             
         MVC   SORTDAY(1),SORTDP                                                
         MVC   SORTTIME,SORTDP+1                                                
         GOTO1 CHOPPER,DMCB,(24,SRERMNM),(16,SORTMKT),1                         
*                                                                               
         MVC   DBSELDAY,SRACTDAY   CONTROL DEMAND                               
         MVC   DBSELTIM,SRACTTIM                                                
         MVC   DBSELMK,PODMPAMK    (SPILL)                                      
         OC    PDSIDMKT,PDSIDMKT   USE ONLY SID STATIONS                        
         BNZ   NSID16                                                           
         OC    DBSELSTA,DBSELSTA   STATION CAN COME FROM SID RECORD             
         BNZ   NSID22                                                           
*                                                                               
NSID16   XC    SORTSTAT,SORTSTAT                                                
         CLI   PDMGROPT,C'Y'            TEST MKTGRPS                            
         BNE   NSID18                                                           
         MVC   SORTMKT,SRACTMKT         MOVE MARKET NUMBER                      
         MVC   DBSELMK,SRACTMKT         MOVE MARKET NUMBER                      
*                                                                               
         MVC   WORK(4),SRACTMKT         ADD NUMBER/NAME TO TABLE                
         MVC   WORK+4(24),SRERMNM                                               
*                                                                               
NSID18   MVC   SORTSTAT(5),SRERSTA                                              
         CLI   SORTSTAT+4,C' '                                                  
         BE    NSID20                                                           
         MVC   SORTSTAT+5(1),SORTSTAT+4                                         
         MVI   SORTSTAT+4,C'-'                                                  
*                                                                               
NSID20   MVC   DBSELSTA,SRERSTA                                                 
         MVC   PDNET,SRERSTA                                                    
         XC    DBSELMK,DBSELMK                                                  
*                                                                               
NSID22   LA    R1,SRACTEF1         MAY BE MORE THAN 1 COST                      
*                                                                               
NSID24   OC    0(7,R1),0(R1)                                                    
         BZ    NSID06                                                           
         MVC   PDEFFDAT,0(R1)                                                   
         BAS   RE,GETDEMS          GET THE DEMOS                                
*                                                                               
         LA    R1,7(R1)                                                         
         B     NSID24                                                           
*                                                                               
NSID26   ICM   RF,15,NSIDPTR                                                    
         LA    RF,2(RF)                                                         
         STCM  RF,15,NSIDPTR                                                    
         OC    0(2,RF),0(RF)                                                    
         BNZ   NSID02                                                           
*                                                                               
NSIDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        GET DEMOGRAPHICS                                             *         
***********************************************************************         
GETDEMS  NTR1                                                                   
         CLC   DBSELSTA+3(2),=C'LD'   NO DAY/TIME FILTERING FOR                 
         BE    *+10                    DAILY P/L/E PORTIONS                     
         CLC   DBSELSTA+3(2),=C'PD'                                             
         BE    *+10                                                             
         CLC   DBSELSTA+3(2),=C'ED'                                             
         BNE   *+14                                                             
         MVI   DBSELDAY,0                                                       
         XC    DBSELTIM(4),DBSELTIM                                             
*                                                                               
         CLI   PDDETOPT,C'Y'       FOR THE DETAIL WE ONLY CONDENSE              
         BE    GETD02               FOR THE ESTIMATED BOOK                      
         CLI   PDDETOPT,C'N'       FOR THE DETAIL TO BE SUPPRESSED              
         BE    GETD18               WE MUST DO AN UPGRADE                       
         CLI   PDSIDOPT,C'Y'       SID TO MATCH SPOT/REP SYSTEM                 
         BE    GETD18                                                           
         CLI   ANYUP,C'Y'          AVAIL TO MATCH SPOT/REP SYSTEM               
         BE    GETD18                                                           
*                                                                               
GETD02   CLI   EBOPT,C'Y'          HAVE TO HAVE EST BOOK                        
         BNE   GETD04               TO DO UPGRADES                              
         CLI   ANYUP,C'Y'                                                       
         BE    GETD18                                                           
*                                                                               
GETD04   DS    0H                                                               
         CLC   =C'WTP',DBFILE                                                   
         BNE   *+10                                                             
         MVC   DBFILE,=C'TP '                                                   
         CLC   =C'NHT-H',PDSOURCE    NHI SOURCE READS NTI FILE                  
         BE    *+10                                                             
         CLC   =C'HPM-H',PDSOURCE    NHI SOURCE READS NTI FILE                  
         BE    *+10                                                             
         CLC   =C'NHI',PDSOURCE    NHI SOURCE READS NTI FILE                    
         BE    *+10                                                             
         CLC   =C'OOH ',PDSOURCE    OOH SOURCE READS NTI FILE                   
         BNE   *+10                                                             
         MVC   DBFILE,=C'NTI'                                                   
*                                                                               
         CLC   =C'CNA',PDSOURCE   CABLE NAD AND MVGO                            
         BNE   *+14                                                             
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBBTYPE,C'N'       READ NAD DATA FIRST. FOR MVGO, WILL           
*                                 LATER READ BTYPE M TOO                        
         CLC   =C'TCAR',PDSOURCE                                                
         BNE   *+8                                                              
         MVI   DBBTYPE,C'T'                                                     
*                                                                               
         CLC   =C'WB1',PDSOURCE                                                 
         BNE   *+8                                                              
         MVI   DBBTYPE,C'V'                                                     
*                                                                               
         CLC   =C'ACMWB',PDSOURCE                                               
         BNE   *+8                                                              
         MVI   DBBTYPE,C'V'                                                     
*                                                                               
         CLC   =C'NCOS',PDSOURCE                                                
         BNE   *+8                                                              
         MVI   DBBTYPE,C'C'                                                     
*                                                                               
         CLC   =C'NHT',PDSOURCE    HISPANIC WEIGHTED                            
         BE    *+10                                                             
         CLC   =C'HPM',PDSOURCE                                                 
         BE    *+10                                                             
         CLC   =C'NHW',PDSOURCE                                                 
         BE    *+10                                                             
         CLC   =C'HPW',PDSOURCE                                                 
         BNE   *+8                                                              
         CLI   DBBTYPE,C'E'                                                     
         BNE   GETD05                                                           
         MVI   DBBTYPE,0                                                        
         CLI   DBSELSTA+4,C'H'                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,X'88'    LOWER CASE 'H'                               
*                                                                               
GETD05   CLI   DBSELSTA+4,C'C'     FOR CABLE                                    
         BNE   GETD12                                                           
         GOTOR SUBR03,DMCB,('SETCBLE',(RC))                                     
*                                                                               
GETD12   CLI   PAYOPT,C'N'                                                      
         BNE   GETD16                                                           
         LAY   RE,PAYCAB                                                        
*                                                                               
GETD14   CLI   0(RE),X'FF'                                                      
         BE    GETD16                                                           
         CLC   DBSELSTA(4),0(RE)                                                
         BE    GETDX                                                            
         LA    RE,4(RE)                                                         
         B     GETD14                                                           
*                                                                               
GETD16   CLC   =C'BBM',PDSOURCE                                                 
         BNE   GETD17                                                           
         CLI   DBBTYPE,C'W'        WEEKLY BOOK TYPE?                            
         BNE   GETD17              NO                                           
         MVI   DBBEST,C'M'         YES, ASSUME M/Y REQUEST                      
         CLI   SVBBMWK,C'Y'        WAS IS M/D/Y?                                
         BNE   *+8                 NO                                           
         MVI   DBBEST,C'N'         YES, DBBEST                                  
*                                                                               
GETD17   DS    0H                                                               
         CLC   =C'CNAD',PDSOURCE  ->LOOP THROUGH ALL WEEKS OF THE MONTH         
         BE    GETD17C                                                          
*                                                                               
         BRAS  RE,SYSCLIST         BUILD TABLE OF FUSION SYSCODES               
*                                                                               
         CLC   =C'CCV',PDSOURCE                                                 
         BNE   GETD17B                                                          
         CLI   DBBTYPE,0           STATE WAS GIVEN. USE IT                      
         BNE   GETD17B                                                          
         LAY   R2,STACODE                                                       
CCVSTAT  CLC   =X'FFFF',0(R2)      LOOP THROUGH ALL THE STATES                  
         BE    CCVSTAX                                                          
         MVC   DBBTYPE,0(R2)                                                    
         GOTO1 DEMAND,DMCB,DBLOCK,ONEHOOK                                       
         LA    R2,28(R2)                                                        
         B     CCVSTAT                                                          
CCVSTAX  B     GETDX                                                            
*                                                                               
GETD17B  GOTO1 DEMAND,DMCB,DBLOCK,ONEHOOK                                       
         B     GETDX                                                            
*                                                                               
* -CNAD-                                                                        
GETD17C  L     R2,ANACWKS          LOOP THROUGH ALL WEEKS OF THE MONTH          
         USING NEWKSD,R2                                                        
NACBK1   CLC   DBSELBK(1),NEWKSYR  COMPARE ON YEAR                              
         BNE   NACBK10                                                          
         CLC   DBSELBK+1(1),NEWKSMO  COMPARE ON MONTH                           
         BNE   NACBK10                                                          
         ZIC   R0,NEWKSFRS         FIRST WEEK OF THE MONTH                      
NACBK5   STC   R0,DBSELBK+1                                                     
         MVI   DBSTYPE,0           RESET                                        
         GOTO1 DEMAND,DMCB,DBLOCK,ONEHOOK                                       
         CLI   DBFUNCT,DBGETDEM    TP READ. HAVE DATA ONLY FOR 1ST BOOK         
         BE    NACBKX                                                           
         CLC   DBSELBK+1(1),NEWKSLST LAST WEEK OF THE MONTH                     
         BE    NACBKX                                                           
         AHI   R0,1                UPDATE WEEK                                  
         B     NACBK5                                                           
*                                                                               
NACBK10  LA    R2,NEWKSQ(R2)     NEXT ENTRY                                     
         CLI   0(R2),X'FF'                                                      
         BNE   NACBK1                                                           
NACBKX   B     GETDX                                                            
*                                                                               
*                                  GET UPGRADE DEMOS                            
GETD18   GOTOR SUBR01,DMCB,('UPGRDE',(RC))                                      
*                                                                               
GETDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        HOOK ROUTINE FOR DEMAND                                      *         
***********************************************************************         
*                                                                               
ONEHOOK  NTR1                      HOOK FOR DEMAND                              
         CLC   =C'MYS',0(R3)                                                    
         JNE   *+8                                                              
         J     *+4                                                              
*                                                                               
         BRAS  RE,SETCNA           REPORT CERTAIN STATIONS IF CNAD/CNAW         
*                                                                               
         GOTO1 DEFINE,DMCB,=C'NLIV ',DBLOCK,WORK                                
         MVC   PDACTVIW,WORK       ACTUAL VIEWING TYPE N/1/7/...                
*                                                                               
         MVC   ACURRSYC,APDSYSC    FOR FUSION AND NSI-WIRED                     
ONEH     CLC   =C'FUS',PDSOURCE    EXTRACT DEMOS ONE SYSCODE AT A TIME          
         BE    ONEH001                                                          
         CLC   =C'NSI',PDSOURCE                                                 
         BE    ONEH000                                                          
         CLC   =C'TP',PDSOURCE                                                  
         BE    ONEH000                                                          
         CLC   =C'T4',PDSOURCE                                                  
         BE    ONEH000                                                          
         B     ONEH005                                                          
ONEH000  CLI   DBBTYPE,C'W'                                                     
         BNE   ONEH005                                                          
*                                                                               
ONEH001  TM    PDFLAG1,PDF1SYSC                                                 
         BNO   ONEH005                                                          
         L     RF,ACURRSYC                                                      
         CLC   =X'FFFF',0(RF)                                                   
         BE    ONEH005                                                          
         MVC   DBSELSYC,0(RF)                                                   
*                                                                               
ONEH005  LA    RE,PDCODE                                                        
         LA    RF,PDNTILEN                                                      
         XCEFL                                                                  
*                                                                               
         CLI   PODBMED,C'N'        NETWORK CABLE                                
         BNE   *+8                                                              
         CLI   DBSELSTA+4,C'C'                                                  
         BNE   ONEHK               SET DBSELDAY,DBSELTIM FROM D/T TABLE         
         GOTOR SUBR02,DMCB,('SETCDTE',(RC))                                     
*                                                                               
ONEHK    CLC   =C'CNA',PDSOURCE                                                 
         BNE   ONEH01                                                           
         GOTOR SUBR02,DMCB,('SETCNAE',(RC))   SPECIAL PROC FOR CBL NAD          
*                                                                               
ONEH01   DS    0H                                                               
*        XC    PDPTYP(8),PDPTYP                                                 
*        XC    PDSPRO,PDSPRO                                                    
         XC    PDPTYP(L'PDPTYP+L'PDSPRO),PDPTYP                                 
*                                                                               
         L     R5,ASORTREC                                                      
         USING SORTDATA,R5                                                      
         MVC   PDDP,SORTDPT        RESTORE DEPT                                 
         MVC   PDPRGTYP,SORTTYP                                                 
*                                                                               
         CLI   FORCEBTU,C'Y'       USER ADJUST DEMO REQUESTED(TVQ)              
         BE    ONEH02                                                           
         CLI   FORCEBTU,C'O'       USER ADJUST DEMO REQUESTED(OPI)              
         BE    ONEH02                                                           
         CLI   FORCEBTU,C'I'       USER ADJUST DEMO REQUESTED(IAG)              
         BE    ONEH02                                                           
         CLI   SVDBTYPE,C'U'       BOOK TYPE "U" REQUEST                        
         BNE   ONEH04                                                           
         CLI   DBSELMED,C'O'                                                    
         BE    ONEH04              DOESN'T APPLY TO NSI OVERNIGHTS              
         CLI   ALLDT,3             FOR ALL DAYS/ALL TIMES                       
         BE    ONEH04                                                           
         GOTOR SUBR02,FULL,('BTUREADE',(RC))                                    
         CLI   GOTUTYPE,1          NOT THERE - BYPASS                           
         BNE   ONEHX                                                            
         MVC   DBAQUART,SVBTUAQ                                                 
         B     ONEH04                                                           
*                                                                               
ONEH02   DS    0C                                                               
         GOTOR SUBR02,FULL,('BTUREADE',(RC))                                    
         XC    UFILEXT,UFILEXT                                                  
         LA    RE,UFILEXT                                                       
         USING DBXUFD,RE                                                        
         MVC   DBXUFID,=C'UFIL'                                                 
         CLI   GOTUTYPE,1            FIND BOOK TYPE U?                          
         BNE   ONEH04                NO                                         
         L     RF,AMYDBLK                                                       
         MVC   DBXUFADR,4(RF)        YES, SAVE A(RECORD)                        
         DROP  RE                                                               
*                                                                               
ONEH04   CLC   =C'IUN',DBFILE                                                   
         BNE   ONEH10                                                           
         XC    PDEFFDAT,PDEFFDAT                                                
*        L     RF,DBEXTEND                                                      
         GOTOR SUBR02,DMCB,('GETEXTE',(RC)),=C'RINV'                            
         ICM   RF,15,DMCB                                                       
         USING DBXINVWK,RF                                                      
         MVC   PDDP(6),DBXIDPT                                                  
         MVI   PDDP+6,C' '                                                      
*                                                                               
         OC    PODPSTRT+2(2),PODPSTRT+2  NO END DATE LOGIC ALREADY              
         BZ    *+14                       DONE                                  
         CLC   DBXIEFFD(2),PODPSTRT+2  EFF START GT REQ END                     
         BH    ONEHX                                                            
         OC    DBXIEFFD+2(2),DBXIEFFD+2                                         
         BZ    ONEH06                                                           
         CLC   DBXIEFFD+2(2),PODPSTRT  EFF END LT REQ START                     
         BL    ONEHX                                                            
*                                                                               
ONEH06   GOTO1 DATCON,DMCB,(2,DBXIEFFD),(3,PDEFFDAT)                            
*        L     RF,DBEXTEND                                                      
         GOTOR SUBR02,DMCB,('GETEXTE',(RC)),=C'RINV'                            
         ICM   RF,15,DMCB                                                       
         MVC   PDFAIR(2),DBXIEFFD                                               
         MVC   PDLAIR(2),DBXIEFFD+2                                             
         LA    RE,PDSDDPTL                                                      
         CLI   0(RE),C'A'          DAYPART FILTERS                              
         BL    ONEH10              NO - ACCEPT RECORD                           
*                                                                               
ONEH08   LA    R0,6                MATCH TO ANY IN LIST                         
         LA    R1,DBXIDPT          TRY CURRENT ITEM                             
         CLC   0(1,RE),0(R1)                                                    
         BE    ONEH10              FOUND IT - SO ACCEPT RECORD                  
         LA    R1,1(R1)                                                         
         BCT   R0,*-14             NO - LOOK THROUGH ALL DAYPARTS               
         LA    RE,1(RE)            TRY NEXT FILTER                              
         CLI   0(RE),C'A'                                                       
         BNL   ONEH08                                                           
         B     ONEHX                                                            
         DROP  RF                                                               
*                                                                               
ONEH10   L     RE,DBAREC                                                        
         CLC   0(3,RE),=C'QNN'                                                  
         BE    *+10                                                             
         CLC   0(3,RE),=C'QN1'                                                  
         BE    *+10                                                             
         CLC   0(3,RE),=C'QN7'                                                  
         BNE   ONEH20                                                           
         USING PMKEY,RE                                                         
         CLI   PMBTYP,C'U'         TEMP FIX FOR TVQ                             
         BE    ONEH14                                                           
         CLI   PMSTAT+4,C'M'                                                    
         BNE   ONEH20                                                           
*                                                                               
         LA    RE,PMDATA                                                        
         OC    SVCRDATE,SVCRDATE   DO WE HAVE A DATE YET?                       
         BZ    ONEH12              NO, SAVE THIS ONE                            
         CLI   0(RE),1             YES, IS THIS MARKET TYPE?                    
         BNE   ONEH20              NO                                           
         CLC   6(2,RE),SVCRDATE    YES, CHECK THE DATES                         
         BNE   ONEHX               NO EQUAL - DONE                              
*                                                                               
ONEH12   MVC   SVCRDATE,6(RE)      SAVE CREATION DATE                           
         B     ONEH20                                                           
*                                                                               
ONEH14   LA    RE,PMDATA                                                        
*                                                                               
ONEH16   DS    0C                                                               
         CLI   0(RE),0                                                          
         BE    ONEH20                                                           
         CLI   0(RE),X'5E'                                                      
         BE    ONEH18                                                           
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     ONEH16                                                           
*                                                                               
ONEH18   CLC   2(3,RE),=C'NNN'                                                  
         BNE   ONEH20                                                           
         MVC   2(3,RE),=C'TVQ'                                                  
*        B     ONEH20                                                           
         DROP  RE                                                               
*                                                                               
ONEH20   L     RE,DBAREC                                                        
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   *+14                                                             
         XC    PDRUN,PDRUN                                                      
         B     ONEH21                                                           
*                                                                               
         CLI   PDMULTRD,C'Y'       DON'T INCREMENT FOR MULTIREAD                
         BE    *+12                                                             
         LA    RE,1                                                             
         STCM  RE,15,PDRUN                                                      
*                                                                               
ONEH21   CLC   DBFILE,=C'EVN'                                                   
         BE    ONEH86                                                           
*                                                                               
         GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,PDPROG                            
*                                                                               
         MVI   PDAVGTYP,X'FF'                                                   
         ICM   RE,15,DBSPANAD      SET AVERAGE TYPE FOR CABLE                   
         CLC   0(3,RE),=C'CB2'                                                  
         BE    *+10                                                             
         CLC   0(3,RE),=C'CAB'                                                  
         BNE   ONEH22                                                           
         MVC   PDAVGTYP,3(RE)                                                   
         MVC   PDTRNUM(5),109(RE)                                               
         MVC   PDTLNUM(5),125+5(RE)                                             
*                                                                               
ONEH22   CLC   =C'MXM ',PDSOURCE                                                
         BNE   ONEH30                                                           
         GOTO1 DEFINE,DMCB,=C'TRKTELNO',DBLOCK,WORK                             
         MVC   PDAVGTYP,WORK                                                    
         MVC   PDTRNUM(5),WORK+1                                                
         MVC   PDTLNUM(5),WORK+6                                                
*                                                                               
ONEH30   DS    0C                                                               
         MVC   DBLOCKA(256),DBLOCK                                              
         XC    PDMPDOL,PDMPDOL                                                  
         CLC   =C'NCOS',PDSOURCE                                                
         BNE   ONEH31                                                           
         GOTO1 DEFINE,DMCB,=C'MP$ ',DBLOCK,BLOCK                                
         MVC   PDMPDOL,BLOCK                                                    
ONEH31   GOTO1 DEFINE,DMCB,=C'TYPE',DBLOCK,BLOCK                                
         MVC   PDFILT,BLOCK                                                     
         CLI   PDNOROPT,C'N'                                                    
         BNE   *+14                                                             
         CLC   PDPROG+9(5),=C'(NOR)'                                            
         BE    ONEHX                                                            
*                                                                               
         LA    RE,PODNDFLT         NAD FILTERS                                  
         CLC   PDSOURCE(3),=C'NHW'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'HPW'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'NAW'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'NHT'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'HPM'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'CNA' CABLE NAD AND MVGO (CNAD,CNAW)               
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'NAD'                                              
         BE    *+8                                                              
         LA    RE,PODNTFLT         NTI FILTERS                                  
         LR    R0,RE               SAVE THE ADDRESS                             
         CLI   0(RE),X'FF'         ANY FILETERS                                 
         BE    ONEH32              NO - CHECK FOR PROGRAM FILTERS               
         GOTO1 DEFINE,DMCB,=C'TYPE',DBLOCK,BLOCK                                
         MVC   PDFILT,BLOCK                                                     
*                                                                               
ONEH32   OC    PDHISPLN,PDHISPLN                                                
         BZ    ONEH34                                                           
         GOTO1 DEFINE,DMCB,=C'PROG25',DBLOCK,PDLPRO                             
         GOTO1 (RF),(R1),=C'PROG06',,PDSPRO                                     
*                                                                               
ONEH34   LR    RE,R0                                                            
         ST    RE,DMCB+4                                                        
         GOTO1 VCHKFILT,DMCB,BLOCK                                              
         CLI   DMCB+4,X'FF'                                                     
         BE    ONEHX                                                            
*                                                                               
         DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'GAA',DBLOCK,BLOCK                                 
         MVC   PDGAA,BLOCK                                                      
         CLI   PDGAAOPT,0          GAA OPTION ACTIVE                            
         BE    ONEH36                                                           
         CLC   PDGAAOPT,PDGAA      EXIT IF IT MATCHES                           
         BNE   ONEHX                                                            
*                                                                               
ONEH36   DS    0H                                                               
         GOTO1 DEFINE,DMCB,=C'NTI',DBLOCK,PDCODE                                
         CLC   =C'OPI',PDSOURCE                                                 
         BE    *+14                                                             
         CLC   =C'IAGE',PDSOURCE                                                
         BNE   ONEH37                                                           
         L     RE,DBAREC                                                        
         USING PMKEY,RE                                                         
         MVC   PDCODE,PMOPIID                                                   
         DROP  RE                                                               
ONEH37   DS    0C                                                               
*                                                                               
         GOTO1 (RF),(R1),=C'MARKET',,PDMRKT                                     
         GOTO1 (RF),(R1),=C'STATION',,PDSTAT                                    
*                                                                               
         CLC   DBFILE,=C'MPA'                                                   
         BNE   *+10                                                             
         MVC   PDORGSTA,PDSTAT                                                  
*                                                                               
         CLI   DBSELSRC,C'F'                                                    
         BNE   *+10                                                             
         MVC   PDSTAT,PDNET        ALPHA STATION                                
*                                                                               
         GOTO1 (RF),(R1),=C'NET',,PDNET                                         
*                                                                               
* INACTIVE AGENCY CLEANUP JAN/06. ID MAY BE REUSED IN THE FUTURE.               
*        CLC   DBSELAGY,=C'UM'     FOR UPN                                      
*        BNE   *+10                                                             
*        CLC   PDNET(4),=C'PAR '   CHANGE PAR TO UPN                            
*        BNE   *+10                                                             
*        MVC   PDNET(4),=C'UPN '                                                
*                                                                               
         MVC   PDDAYLY,DBACTSTA+4                                               
         OC    PDSTAT,PDSTAT       IF STATION IS EMPTY, REP PAV WILL            
         BNZ   *+10                                                             
         MVC   PDSTAT,PDNET        COPY THE ONE FROM NETWORK                    
*                                                                               
*        CLC   PDSTAT(4),=C'WOTV'  WOTV - DOUBLE SWITCH                         
*        BNE   ONEH38                                                           
*        CLC   DBSELBK,=X'5C05'                                                 
*        BH    *+10                                                             
*        MVC   PDSTAT(4),=C'WOOD'                                               
*                                                                               
ONEH38   CLC   PDSTAT(4),=C'WUHQ'                                               
         BNE   *+10                                                             
         MVC   PDSTAT,=C'WOTV'                                                  
*                                                                               
         CLI   BRKOOPT,C'Y'        ARE BREAKOUTS REQUESTED                      
         BE    ONEH40              YES BYPASS FILTERS                           
         LA    RE,14                                                            
         LA    RF,PDPROG                                                        
         CLC   0(3,RF),=CL3'(B)'                                                
         BE    ONEHX                                                            
         LA    RF,1(RF)                                                         
         BCT   RE,*-14                                                          
*                                                                               
ONEH40   DS    0H                                                               
         CLI   BRKOOPT,C'Y'        ARE BREAKOUTS REQUESTED                      
         BE    ONEH44              YES BYPASS FILTERS                           
         CLI   PDLPRO,0                                                         
         BNE   ONEH42                                                           
         GOTO1 DEFINE,DMCB,=C'PROG25',DBLOCK,PDLPRO                             
*                                                                               
ONEH42   LA    RE,23                                                            
         LA    RF,PDLPRO                                                        
         CLC   0(3,RF),=CL3'(B)'                                                
         BE    ONEHX                                                            
         LA    RF,1(RF)                                                         
         BCT   RE,*-14                                                          
*                                                                               
ONEH44   CLI   BRKOOPT,C'Y'        ARE BREAKOUTS REQUESTED                      
         BE    ONEH45              YES BYPASS FILTERS                           
         LA    RE,4                                                             
         LA    RF,PDSPRO                                                        
         CLC   0(3,RF),=CL3'(B)'                                                
         BE    ONEHX                                                            
         LA    RF,1(RF)                                                         
         BCT   RE,*-14                                                          
*                                                                               
ONEH45   DS    0H                                                               
         CLI   PDDUROPT,C'M'       METHOD= SOURCE                               
         BNE   ONEH46                                                           
         GOTO1 DEFINE,DMCB,=C'FEED ',DBLOCKA,FULL                               
         CLI   FULL,C'E'           BYPASS EAST FEED                             
         BE    ONEHX                                                            
         CLI   FULL,C'W'           AND WEST FEED                                
         BE    ONEHX                                                            
*                                                                               
ONEH46   DS    0H                                                               
         GOTOR SUBR01,DMCB,('SETDYTE',(RC))                                     
         CLI   HHHUT,C'Y'                                                       
         BNE   ONEH48                                                           
         GOTOR SUBR01,DMCB,('HALFHUTE',(RC))                                    
*                                                                               
ONEH48   CLI   DAYPOPT,C'Y'                                                     
         BNE   ONEH50                                                           
         GOTOR SUBR01,DMCB,('FIGMINSE',(RC))                                    
         GOTOR SUBR01,DMCB,('DAYPDAYE',(RC))                                    
         B     ONEH50A                                                          
*                                                                               
ONEH50   GOTOR SUBR01,DMCB,('FIGMINSE',(RC))                                    
ONEH50A  DS    0X                                                               
         LA    RE,=C'WEEK'                                                      
         CLC   =C'CNA',PDSOURCE                                                 
         BE    ONEH50B                                                          
         CLC   DBFILE,=C'NAD'                                                   
         BNE   *+8                                                              
ONEH50B  LA    RE,=C'LWEEK'        USE LONG WEEK FIELD WHEN AVAILABLE           
         ST    RE,0(R1)                                                         
         GOTO1 DEFINE,(R1),,,BLOCK                                              
*                                                                               
         MVI   MAXWKS,4                                                         
         CLC   =C'CNA',PDSOURCE                                                 
         BE    *+14                                                             
         CLC   DBFILE,=C'NAD'      NAD AND NHT HAVE LONG WEEK FIELD             
         BNE   ONEH50C                                                          
         CLI   BLOCK+6,C'L'        CHECK LONG WEEK INDICATOR                    
         BNE   *+8                                                              
         MVI   MAXWKS,5                                                         
*                                                                               
ONEH50C  CLC   PDSOURCE(3),=C'NTI'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(4),=C'OOH '                                             
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'ACM'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(4),=C'OOHC'                                             
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'NHT'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'HPM'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'NHW'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'HPW'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'NAW'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'CNA'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'WB1'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'REN'  MIGHT NOT WORK                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'NAD'                                              
         BNE   ONEH64                                                           
*                                                                               
         CLC   PDSOURCE(3),=C'ACM'                                              
         BE    *+14                                                             
         CLC   PDSOURCE(4),=C'OOHC'                                             
         BNE   ONEH50D                                                          
         TM    OPTFLAG1,ACMMWTQ    ACM MINUTE WEIGHTING                         
         BO    ONEH51                                                           
*                                                                               
ONEH50D  CLI   PDDUROPT,C'M'       WEIGHT BY MINUTES (START TIME FLTR)          
         BE    ONEH51                                                           
         CLI   PDDUROPT,C'Y'       WEIGHT BY PROGRAM MINUTES                    
         BNE   ONEH64                                                           
ONEH51   L     R1,DBAREC           GET WEIGHT FROM PROGRAM MINUTES              
         USING PRKEY,R1                                                         
         CLI   PRCODE,C'P'                                                      
         BNE   ONEH58                                                           
         MVC   HALF(1),PRDW                                                     
         NI    HALF,X'F0'                                                       
         SR    RF,RF                                                            
         LA    RF,1                                                             
         CLI   HALF,0                                                           
         BNE   *+8                                                              
         LA    RF,5                                                             
         TM    HALF,X'80'                                                       
         BZ    *+8                                                              
         LA    RF,7                                                             
         STH   RF,HALF                                                          
         L     RE,DBAQUART                                                      
         USING PHELEM,RE                                                        
         SR    RF,RF                                                            
*                                                                               
ONEH52   CLI   PHCODE,PHCODEQ                                                   
         BH    ONEH64                                                           
         BE    ONEH54                                                           
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     ONEH52                                                           
*                                                                               
ONEH54   CLI   PHDURTOT,0          MISSING                                      
         BE    ONEH56                                                           
         CLI   PRSTAT+4,C'C'       CABLE TP HAS QTR HOURS HERE                  
         BE    ONEH56                                                           
         CLI   PRSTAT+4,C'Q'                                                    
         BE    ONEH56                                                           
*                                                                               
         IC    RF,PHDURTOT                                                      
         MH    RF,HALF                                                          
         ST    RF,PDMINS                                                        
         B     ONEH58                                                           
*                                                                               
ONEH56   LH    RF,DBFACTOR         MULTIPLY BY 15                               
         MHI   RF,15                                                            
         ST    RF,PDMINS                                                        
*                                                                               
ONEH58   MVC   PDWEIGHT,PDMINS     FULL WORD                                    
         MVC   DBFACTOR,PDMINS+2   HALFWORD                                     
         L     RE,DBAREC                                                        
         CLI   0(RE),C'Q'          ONLY FOR PROGRAM RECORDS                     
         BNE   ONEH64                                                           
         L     R0,PDTIME                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,3,DBSELTIM                                                    
         ICM   RF,3,DBSELTIM+2                                                  
         CHI   RE,559                                                           
         BH    *+8                                                              
         AHI   RE,2400                                                          
         CHI   RF,600              ALLOW 6AM END                                
         BH    *+8                                                              
         AHI   RF,2400                                                          
         CLI   PDTRNKET,C'Y'                                                    
         BNE   ONEH60              IT'S THE SAME AS DEFAULT                     
         SR    RE,RE                                                            
         ICM   RE,3,PDTIME+2                                                    
         CHI   RE,600              ALLOW 6AM END                                
         BH    *+8                                                              
         AHI   RE,2400                                                          
         CR    RE,RF                                                            
         BNH   *+8                                                              
         STCM  RF,3,PDTIME+2                                                    
*                                                                               
ONEH60   CLI   PDTRNKST,C'Y'                                                    
         BNE   ONEH62                                                           
         SR    RE,RE                                                            
         ICM   RE,3,PDTIME                                                      
         CHI   RE,559                                                           
         BH    *+8                                                              
         AHI   RE,2400                                                          
         SR    RF,RF                                                            
         ICM   RF,3,DBSELTIM                                                    
         CHI   RF,559                                                           
         BH    *+8                                                              
         AHI   RF,2400                                                          
         CR    RE,RF                                                            
         BNL   ONEH62                                                           
         MVC   PDTIME(2),DBSELTIM                                               
*                                                                               
ONEH62   ST    R0,DMCB                                                          
         CLC   DMCB,PDTIME                                                      
         BE    ONEH64                                                           
         GOTOR SUBR01,DMCB,('FIGMINSE',(RC))                                    
         ICM   RF,15,PDWEIGHT                                                   
         MVC   PDWEIGHT,PDMINS                                                  
         MVC   DBFACTOR(2),PDMINS+2                                             
         STCM  RF,15,PDMINS                                                     
         ST    R0,PDTIME                                                        
*        B     ONEH64                                                           
         DROP  R1                                                               
         DROP  RE                                                               
*                                                                               
ONEH64   CLC   PDSOURCE(3),=C'NAD'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'NHT'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'HPM'                                              
         BNE   ONEH66                                                           
         CLI   PDMULTRD,C'Y'                                                    
         BE    ONEH66                                                           
         LA    RF,BLOCK+1                                                       
         ZIC   R0,MAXWKS                                                        
         LA    R1,0                                                             
         CLI   0(RF),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)            COUNT NUMBER OF WEEKS                        
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                                                             
         MH    R1,PDRUN+2                                                       
         STCM  R1,15,PDRUN                                                      
*        B     ONEH66                                                           
*                                                                               
ONEH66   CLI   PDWKOPT,0           ANY WEEK FILTERING                           
         BE    ONEH72                                                           
         CLI   MAXWKS,5            LONGER WK FIELD DOESN'T INCLUDE DAY          
         BE    *+8                                                              
         NI    BLOCK,X'0F'         CLEAR TYP/HIGHEST BITS (DAYS)                
         ZIC   RF,BLOCK            YES - FIND NUMBER OF WEEKS                   
         XC    PDWEEK,PDWEEK        AND APPLY FILTERS                           
         SR    R0,R0                COUNT THE NUMBER OF WEEKS                   
WEEKS1   LTR   RF,RF                                                            
         BZ    WEEKS2                                                           
         SR    RE,RE                                                            
         SLDL  RE,1                                                             
         AR    R0,RE                                                            
         B     WEEKS1                                                           
WEEKS2   STC   R0,PDWEEK                                                        
*                                                                               
         MVC   PDWEEK+1(1),PDWKOPT                                              
         NI    PDWEEK+1,X'0F'                                                   
         TM    PDWKOPT,X'80'                                                    
         BO    ONEH68                                                           
         TM    PDWKOPT,X'40'                                                    
         BO    ONEH70                                                           
         CLC   PDWEEK(1),PDWEEK+1  EXACTLY N WEEKS                              
         BNE   ONEHX                                                            
         B     ONEH72                                                           
*                                                                               
ONEH68   CLC   PDWEEK(1),PDWEEK+1  N WEEKS OR LESS                              
         BH    ONEHX                                                            
         B     ONEH72                                                           
*                                                                               
ONEH70   CLC   PDWEEK(1),PDWEEK+1  N WEEKS OR MORE                              
         BL    ONEHX                                                            
*                                                                               
ONEH72   DS    0H                                                               
         XC    PDWEEK,PDWEEK                                                    
         ZIC   R1,MAXWKS                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDWEEK(0),BLOCK+1   MOVE 4 OR 5 WEEKS                            
         ZIC   R1,MAXWKS                                                        
         LA    RF,PDWEEK           REPLACE SPACE WITH . FOR ALIGNMENT           
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'.'                                                       
         LA    RF,1(RF)                                                         
         BCT   R1,*-16                                                          
*                                                                               
         CLC   DBFILE,=C'NTI'                                                   
         BE    ONEH74                                                           
         CLC   DBFILE,=C'RLD'                                                   
         BE    ONEH74                                                           
         CLC   DBFILE,=C'MPA'                                                   
         BE    ONEH74                                                           
         CLC   DBFILE,=C'TP '                                                   
         BE    ONEH74                                                           
         CLC   DBFILE,=C'NAD'                                                   
         BNE   ONEH86                                                           
*                                                                               
ONEH74   GOTO1 DEFINE,DMCB,=C'NTI '                                             
         MVC   PDNTI,BLOCK                                                      
*                                  FILTER BY PROGRAM                            
         L     RE,APDNTIFT                                                      
         CLI   0(RE),0                                                          
         BE    ONEH78                                                           
         LA    RF,62                                                            
*                                                                               
         TM    OPTFLAG1,EXCLNTI    OPTION TO EXCLUDE                            
         BO    ONEH76A                                                          
*                                                                               
ONEH76   CLC   PDNTI,0(RE)         INCLUDE NTI NUMBERS                          
         BE    ONEH77                                                           
         LA    RE,21(RE)                                                        
         OC    0(5,RE),0(RE)                                                    
         BZ    ONEHX                                                            
         BCT   RF,ONEH76                                                        
         B     ONEHX                                                            
*                                                                               
ONEH76A  CLC   PDNTI,0(RE)         EXCLUDE NTI NUMBERS                          
         BE    ONEHX                                                            
         LA    RE,21(RE)                                                        
         OC    0(5,RE),0(RE)                                                    
         BZ    ONEH77                                                           
         BCT   RF,ONEH76A                                                       
*                                                                               
ONEH77   MVC   PDGPROG,5(RE)       GET NAME, IF ANY                             
*                                                                               
*                                  FILTER BY PROGRAM/STATION                    
ONEH78   L     RE,APDPDFLT         NO ENTRIES, ACCEPT ALL                       
         OC    0(L'PDPDFLT,RE),0(RE)                                            
         BZ    ONEH86                                                           
         LA    RF,11                                                            
*                                                                               
ONEH80   CLI   0(RE),X'FF'         END OF TABLE                                 
         BE    ONEHX                                                            
         CLC   PDNTI,0(RE)                                                      
         BE    ONEH84                                                           
*                                                                               
ONEH82   LA    RE,9(RE)                                                         
         BCT   RF,ONEH80                                                        
         B     ONEHX                                                            
*                                                                               
ONEH84   CLC   5(4,RE),=CL4' '     NO STATION IS OKAY                           
         BE    ONEH86                                                           
         CLC   PDNET,5(RE)                                                      
         BNE   ONEH82                                                           
*                                                                               
*                                  READ FOR STATION MASTER                      
ONEH86   GOTOR SUBR03,DMCB,('USERAFFE',(RC))                                    
*                                                                               
*                                  FILTER BY DATE REQUESTED                     
         GOTOR SUBR02,DMCB,('FILRDATE',(RC))                                    
         BZ    XIT                 SKIP THIS ONE                                
*                                                                               
*                                  FILTER BY AFFILATE REQUESTED                 
         GOTOR SUBR03,DMCB,('UFFILATE',(RC))                                    
         BZ    XIT                 SKIP THIS ONE                                
*                                                                               
*                                  FILTER BY SUB PROGRAM TYPE                   
         GOTOR SUBR03,DMCB,('SUBPTYPE',(RC))                                    
         BZ    XIT                 SKIP THIS ONE                                
*                                                                               
ONEHPREM CLI   DBSELMED,C'N'       SAVE PREMIERE PROGRAMS                       
         BNE   ONEHPRMX                                                         
         CLI   PDPRMOPT,0                                                       
         BE    ONEHPRMX                                                         
         GOTO1 DEFINE,DMCB,=C'PREM',DBLOCK,PDPREM                               
         GOTO1 (RF),(R1),=C'NTI',DBLOCK,DUB                                     
         ICM   RE,15,PDSDRCAD                                                   
         LR    RF,RE                                                            
         AHI   RF,2000             L'BUFFER                                     
*                                                                               
ONEHPR05 CR    RE,RF               DON'T OVERFLOW BUFFER                        
         BNL   ONEHPRMX            STOP ADDING PROGRAMS                         
         OC    0(2,RE),0(RE)                                                    
         BZ    ONEHPR10                                                         
         CLC   DUB(6),0(RE)                                                     
         BE    ONEHPR10                                                         
         LA    RE,8(RE)                                                         
         B     ONEHPR05                                                         
*                                                                               
ONEHPR10 MVC   0(6,RE),DUB                                                      
         CLI   PDPREM,C'Y'                                                      
         BNE   *+10                                                             
         MVC   6(2,RE),PDSBOOK                                                  
         CLI   PDPRMPRT,C'Y'                                                    
         BNE   *+10                                                             
         MVC   PDPREM(2),6(RE)                                                  
         OC    6(2,RE),6(RE)       IF NO PREM YET KILL ENTRY                    
         BNZ   ONEHPRMX                                                         
         XC    0(8,RE),0(RE)                                                    
         CLI   PDPRMOPT,C'+'                                                    
         BNE   ONEHX                                                            
*                                                                               
ONEHPRMX DS    0H                                                               
*                                                                               
ONEHOPA  CLC   =C'OPA  ',PDSOURCE   DEVIDE OVERNIGHT PAV BY DBFACTOR            
         BNE   ONEHOPAX                                                         
*                                                                               
         L     RE,DBAREC            CLEAR LAST BYTE OF THE RECORD               
         USING PRKEY,RE             TO HANDLE BAD DATA FROM MERGE               
         SR    RF,RF                                                            
         ICM   RF,3,PRRLEN                                                      
         AR    RF,RE                                                            
         SHI   RF,1                                                             
         MVI   0(RF),0                                                          
         DROP  RE                                                               
*                                                                               
         XC    MATHFACS(MATHFACL),MATHFACS                                      
         LA    R1,DBLOCK                                                        
         ST    R1,MATHABLK                                                      
         MVC   MATHIFIL,DBFILE                                                  
         MVC   MATHOFIL,DBFILE                                                  
         MVC   MATHOSRC(1),DBSELSRC                                             
         MVC   MATHFCTR+2(2),DBDIVSOR                                           
         GOTO1 DEMOMATH,DMCB,=C'DIV',DBAREC,DBAREC,MATHFACS                     
ONEHOPAX DS    0H                                                               
*                                                                               
         CLC   PDSOURCE(4),=C'SQAD'                                             
         BE    ONEH87              SQAD MARKET ISN'T A MARKET                   
*                                                                               
         OC    CURRMRKT,CURRMRKT   ANY MARKET REQUESTED?                        
*        BZ    *+14                                                             
         BZ    ONEH87                                                           
         CLC   =C'CCV',PDSOURCE    FOR COUNTY COVERAGE MATCH ON COUNTY          
         BE    *+18                AND NOT DMA                                  
         CLC   CURRMRKT,PDMRKT                                                  
         BNE   XIT                                                              
         B     ONEH87                                                           
         GOTO1 DEFINE,DMCB,=C'CTY#   ',DBLOCK,CTYNUM                            
         CLC   CURRMRKT,CTYNUM                                                  
         BNE   XIT                                                              
*                                                                               
ONEH87   XC    PDMCMINF,PDMCMINF                                                
         XC    PDPODINF,PDPODINF                                                
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   ONEH87S                                                          
         GOTO1 DEFINE,DMCB,=C'MCOMSEC',DBLOCKA,PDMCMINF                         
*                                                                               
         CLC   PDMCOMSC,OPTMINCM   CK MINIMUM NUMBER OF COMM SECONDS            
         BL    XIT                                                              
*                                                                               
         MVI   PDCOMFDF+1,C' '     COMMERCIAL MINUTE FLAG                       
         CLI   PDMCOMF,C'U'                                                     
         BNE   *+12                                                             
         MVI   PDCOMFDF,C'U'       COMM CONTENT UNKNOWN                         
         B     ONEH87A                                                          
         MVI   PDCOMFDF,C'N'                                                    
         CLI   PDMCOMSC,0          DEFAULT COMM MINUTE DEFINITION:              
         BNH   ONEH87A             >1 SEC OF COMMERCIALS                        
         MVI   PDCOMFDF,C'Y'                                                    
*                                                                               
         CLI   MINCOMM,0           CHECK USER COMM MINUTE DEFINITION            
         BE    ONEH87A                                                          
         CLC   PDMCOMSC,MINCOMM    IF LESS THEN DEFINITION MINIMUM              
         BNL   ONEH87A                                                          
         MVC   PDCOMFDF,=C'N*'     ADJUST ACCORDING TO USER DEFINITION          
*                                                                               
ONEH87A  CLI   OPTMINTY,0          MINUTE TYPE FILTER                           
         BE    ONEH87D                                                          
         TM    OPTMINTY,OPTMCOMQ   FILTER ON COMMERCIAL MINUTES                 
         BNO   ONEH87B                                                          
         CLI   PDCOMFDF,C'Y'       COMM MINUTE ACCORDING TO USER DEFNTN         
         BE    ONEH87D                                                          
ONEH87B  TM    OPTMINTY,OPTMPROQ   FILTER ON PROMO MINUTES                      
         BNO   ONEH87C                                                          
         CLI   PDMPROSC,0                                                       
         BH    ONEH87D                                                          
ONEH87C  TM    OPTMINTY,OPTMPSAQ   FILTER ON PSA MINUTES                        
         BNO   XIT                 NO MATCH                                     
         CLI   PDMPSASC,0                                                       
         BNH   XIT                 NO MATCH                                     
ONEH87D  DS    0X                                                               
*                                                                               
         GOTO1 DEFINE,DMCB,=C'PODINFO',DBLOCKA,PDPODINF                         
         MVI   PDPDSAME,0                                                       
         CLC   PREVSTA,PDSTAT      SAME STATION                                 
         BNE   ONEH87E                                                          
         CLC   PREVNTI,PDNTI       AND SAME PROGRAM                             
         BNE   ONEH87E                                                          
         CLC   PREVPDNU,PDPDNUM    AND SAME POD NUMBER                          
         BNE   ONEH87E                                                          
         MVI   PDPDSAME,PDPDSAMQ   THIS MINUTE BELONGS TO SAME POD              
ONEH87E  DS    0X                  AS THE PREVIOUS MINUTE                       
         MVC   PREVNTI,PDNTI                                                    
         MVC   PREVPDNU,PDPDNUM                                                 
*                                                                               
         TM    OPTFLAG1,INCLOSQ    INCLUDE ORDERED SUSTAINERS?                  
         BO    ONEH87F             YES.                                         
         GOTO1 DEFINE,DMCB,=C'OSUSTAIN',DBLOCKA,WORK     NO.                    
         CLI   WORK,C'Y'           IS THIS AN ORDERED SUSTAINER PROG?           
         BE    XIT                 YES. EXCLUDE IT                              
ONEH87F  DS    0X                                                               
*                                                                               
ONEH87S  L     R3,PDADEMTB         SET UP DEMO BLOCK                            
         USING PDDMBUFF,R3                                                      
         XC    DUB,DUB             CHECK FOR ANY FILTERS                        
         MVC   DUB(4),PDFILT                                                    
*                                                                               
         CLC   PDSOURCE(5),=C'ACMWB'                                            
         BNE   *+16                                                             
         TM    OPTFLAG1,ACMMWTQ                                                 
         BO    *+8                                                              
         BRAS  RE,MKWGT                                                         
*                                  CONVERT NAD USA TO NTI FORMAT                
         CLC   PDSOURCE(3),=C'CNA' CABLE NAD AND MVGO (CNAD,CNAW)               
         BE    ONEH90                                                           
         CLC   PDSOURCE(3),=C'NAD'                                              
         BE    ONEH90                                                           
         CLC   PDSOURCE(3),=C'NAW'                                              
         BE    ONEH90                                                           
         CLC   PDSOURCE(3),=C'HPW'                                              
         BE    ONEH90                                                           
         CLC   PDSOURCE(3),=C'NHT'                                              
         BE    ONEH90                                                           
         CLC   PDSOURCE(3),=C'HPM'                                              
         BE    ONEH90                                                           
         LA    RF,PDNDEMS*10                                                    
         LA    RE,PODDEMO          A(DEMO CODES)                                
*                                                                               
ONEH88   CLI   0(RE),1                                                          
         BNE   *+8                                                              
         MVI   0(RE),0                                                          
         AH    RE,LNDEMCOD                                                      
         BCT   RF,ONEH88                                                        
*                                                                               
ONEH90   DS    0H                                                               
         CLC   PDSOURCE(3),=C'MPA'    MPA DOESN'T USE GETIUN                    
         BE    ONEH110                                                          
         CLC   PDSOURCE(3),=C'IUN'    IUN IS ALREADY IUN                        
         BE    ONEH110                                                          
         CLC   PDSOURCE(3),=C'CCV'    DON'T DO IUN FOR COUNTY COVG              
         BE    ONEH110                                                          
         CLC   =C'SQAD',PODBEXT    SQAD DOESN'T USE GETIUN                      
         BE    ONEH110                                                          
         CLI   DBSELMED,C'D'       DPT DOESN'T USE GETIUN                       
         BE    ONEH110                                                          
         CLI   DBSELMED,C'C'       BBM DOESN'T USE GETIUN                       
         BNE   ONEH92                                                           
         CLI   DBSELSRC,C'A'                                                    
         BE    ONEH110                                                          
         CLI   DBSELBK,95                                                       
         BH    ONEH110                                                          
*                                                                               
ONEH92   CLI   PDIUNOPT,C'Y'       USE IUN CALLS?                               
         BE    ONEH94              YEP, SKIP THE TEST FOR SID                   
         CLI   PDSIDOPT,C'Y'                                                    
         BNE   ONEH110                                                          
*                                                                               
*                                  EXTRACT IUN DEMOS INTO WORK AREA             
ONEH94   L     R4,ASORTDEM                 CLEAR LOOK-UP AREA                   
         LA    R5,8                        RTGS/IMPS/PUTS/TOTS                  
         XC    0(4*IUNDEMS,R4),0(R4)                                            
         LA    R4,4*IUNDEMS(R4)                                                 
         BCT   R5,*-10                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,DBFACTOR                                                    
         STCM  RF,15,PDWEIGHT                                                   
         MVC   DBFACTOR,=H'1'                                                   
         CLI   PROF1W+5,C'I'       IMP BASED RATINGS                            
         BNE   *+8                                                              
         MVI   DBTAPEP,C'Y'                                                     
*                                                                               
         CLI   DBSELSRC,C'F'       TAPE OPTION FOR FUSION                       
         BE    *+8                                                              
         CLI   DBSELSRC,C'N'       TAPE OPTION FOR NSI/USTV                     
         BNE   ONEH95                                                           
         CLI   DBSELMED,C'T'                                                    
         BE    *+8                                                              
         CLI   DBSELMED,C'O'       OVERNIGHTS TOO                               
         BE    *+8                                                              
ONEH95   MVI   DBTAPEP,C'N'        RESET FOR ALL OTHERS                         
*                                                                               
         L     R4,ASORTDEM           GET LOOK-UP AREA ADDRESS                   
         USING IUNWK,R4                                                         
         L     RF,SPGETIUN                                                      
         XC    ILUNVS(4*IUNDEMS),ILUNVS                                         
         CLC   PDSOURCE(3),=C'PAV' SUPPORTED BY REGETIUN ONLY                   
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'OPA' SUPPORTED BY REGETIUN ONLY                   
*        BE    *+8                                                              
*        CLI   PDOVSYS,8           REP SYSTEM?                                  
         BNE   ONEH96                                                           
         L     RF,REGETIUN                                                      
*                                                                               
ONEH96   GOTO1 (RF),DMCB,(4,DBLOCK),(R4),AEXDEMOS                               
         ICM   RF,15,PDWEIGHT                                                   
         STCM  RF,3,DBFACTOR                                                    
*                                                                               
*                                                                               
* NOW NEED TO MULTIPLY UNIVERSES BY DBFACTOR FOR CONSISTENCY *                  
*                                                                               
         USING IUNWK,R4                                                         
         L     R4,ASORTDEM                                                      
         LA    R0,4                                                             
         LA    RE,IUNOLD                                                        
         LA    RF,IUNNEW                                                        
*                                                                               
ONEH98   MVC   0(4*IUNDEMS,RF),0(RE)                                            
         LA    RE,4*IUNDEMS(RE)                                                 
         LA    RF,4*IUNDEMS(RF)                                                 
         BCT   R0,ONEH98                                                        
*                                  GET HOMES SHARES FROM ACTUAL RECORD          
         CLI   PDMETHOD,5                                                       
         BNE   ONEH98A                                                          
         GOTO1 DEMOUT,DMCB,(C'L',DEMOSHR),DBLOCK,IUNXTRA                        
         DROP  R4                                                               
*                                                                               
ONEH98A  LA    RE,DBLOCK           SAVE DBLOCK                                  
         LA    RF,DBLOCKL                                                       
         LA    R0,SVDBLK                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   DBFILE,=C'PAV'                                                   
*                                                                               
         LA    RF,1000             CLEAR IUN CONDENSE RECORD SPACE              
         L     RE,AIUNCOND                                                      
         ST    RE,DBAREC           MODIFY DBLOCK POINTER                        
         XCEF                                                                   
*                                                                               
         L     RE,DBAREC           READDRESS IUNCOND                            
         MVC   0(10,RE),OFORMAT                                                 
         CLC   DBSELBK,=X'5801'    HANDLE BASIC FORMULA CHANGE                  
         BL    *+10                                                             
         MVC   7(2,RE),=X'530B'                                                 
         CLI   DBTAPEP,C'Y'        IMP BASED DMAS                               
         BNE   *+10                                                             
         MVC   7(2,RE),=X'5A0B'                                                 
         LA    RE,23(RE)                                                        
         ST    RE,DBAQUART                                                      
*                                                                               
         LA    RE,IUNWKLEN/4                                                    
         STH   RE,DBNUMVLS         SET # OF WORKAREA VALUES                     
*                                                                               
         L     RF,DBCOMFCS         COMPRESS THE RECORD                          
         L     RF,CDEMAINT-COMFACSD(RF)                                         
         L     R4,ASORTDEM                                                      
*                                                                               
* NOTE - FORMAT ELEMENT IS IN THE KEY PORTION OF DBAREC AREA                    
*                                                                               
         GOTO1 (RF),DMCB,OREP,DBLOCK,(R4),DBAREC                                
*                                                                               
* HANDLE SPOT/REP VARIABLE PRECISION LOOKUPS                                    
         USING DBXTTID,RF                                                       
         XC    SPOTEXT,SPOTEXT                                                  
         LA    RF,SPOTEXT          GET USER FILE EXTENSION                      
         MVC   DBXTID,=C'SPOT'                                                  
         CLI   PRECOPT,C'Y'        ANY PRECISION ADJUST ACTIVE                  
         BNE   ONEH99              NO                                           
         MVI   DBXTSCTL,C'2'       SET FOR 2 DECIMAL RATINGS                    
         LR    R2,RF                                                            
         GOTOR SUBR02,DMCB,('PUTEXTE',(RC)),(R2)                                
         DROP  RF                                                               
ONEH99   DS    0C                                                               
*                                                                               
         CLI   PDMETHOD,5          METHOD=SOURCE FOR SPOT/REP                   
         BE    ONEH116                                                          
*                                                                               
* LET DRIVER HANDLE THE SORT                                                    
* -- CHANGE THE TIME TO THAT OF THE SID RECORD                                  
*                                                                               
         CLI   PDSIDOPT,C'Y'                                                    
         BNE   ONEH104                                                          
*                                                                               
         ICM   RE,15,PDSDBFAD      ADDRESS OF SID RECORD                        
         USING SRBLKD,RE                                                        
         MVC   PDTIME(4),SRACTTIM                                               
         MVC   PDDAY+1(1),DBSELDAY                                              
         MVC   PDDAY+2(3),SRERDAY                                               
*                                                                               
ONEH104  GOTO1 DEMOUT,DMCB,(C'L',AEXDEMOS),DBLOCK,ASORTDEM,0                    
         LA    RE,PODDEMO          A(DEMO CODES)                                
         LA    RF,PDDEMOS          A(DEMO VALUES)                               
         L     R1,ASORTDEM                                                      
*                                                                               
ONEH106  CLI   0(RE),X'FF'                                                      
         BE    ONEH108                                                          
         MVC   0(4,RF),0(R1)                                                    
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         AH    RE,LNDEMCOD                                                      
         B     ONEH106                                                          
*                                                                               
ONEH108  LA    RE,SVDBLK           RESTORE DBLOCK                               
         LA    RF,DBLOCKL                                                       
         LA    R0,DBLOCK                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     ONEH130                                                          
*                                                                               
         USING DBXUFD,RF                                                        
ONEH110  LA    RF,UFILEXT          GET USER FILE EXTENSION                      
         OC    DBXUFADR,DBXUFADR   ANY USER FILE ACTIVE ?                       
         BZ    ONEH114             NO                                           
         LR    R2,RF                                                            
         GOTOR SUBR02,DMCB,('PUTEXTE',(RC)),(R2)                                
         DROP  RF                                                               
*                                                                               
ONEH114  DS    0C                                                               
         CLI   PDOVSYS,3                                                        
         BNE   *+8                                                              
         MVI   DBDEMTYP,C'4'                                                    
         CLI   PDBASE,C'B'                                                      
         BE    ONEH116                                                          
         CLI   PDBASE,C'I'                                                      
         BE    ONEH116                                                          
*                                                                               
         LA    RE,PODDEMO          A(DEMO CODES)                                
         LA    RF,L'PODDEMO                                                     
         L     R0,ASVPODEM                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE               SAVE PODDEMO                                 
*                                                                               
         LA    R2,PODDEMO          DEFAULT TO IAG ORIG/REPEAT DEMO              
         BRAS  RE,IAGDEMS                                                       
*                                                                               
         LA    RF,DBLOCK                                                        
         GOTO1 DEMOUT,DMCB,(C'N',PODDEMO),(RF),PDDEMOS,PODDEMPL,0               
***      GOTO1 DEMOUT,DMCB,(C'L',PODDEMO),(RF),PDDEMOS,0,0                      
*                                                                               
***      BRAS  RE,NETHOMES                                                      
*                                                                               
         BRAS  RE,GETCOM           GET COMSCORE DEMOS                           
*                                                                               
         CLC   =C'CNA',PDSOURCE   CABLE NAD & MOVIE GOER/NAD WKLY               
         BNE   *+16                                                             
         LA    R1,PODDEMO                                                       
         LA    R2,PDDEMOS                                                       
         BAS   RE,NADCAT          SEE IF MATCH ON NAD DEMO CATEGORY             
*                                                                               
         CLC   PDSOURCE(3),=C'NAD'    INCREASE NAD IMPS                         
         BE    *+14                BY A FACTOR OF 10 TO MATCH NTI               
         CLC   PDSOURCE(3),=C'NAW'    INCREASE NAW IMPS                         
         BNE   NADMULX             BY A FACTOR OF 10 TO MATCH NTI               
         CLI   PRECOPT,C'Y'        CABLE PRECISION IS OK ALREADY                
         BE    NADMULX                                                          
         LA    RE,PODDEMO                                                       
         LA    RF,PDDEMOS                                                       
NADMUL   CLI   1(RE),C'T'          DEMO IS IMPRESSIONS                          
         BNE   *+16                                                             
         L     R1,0(RF)            MULTIPLY IT BY TEN                           
         MHI   R1,10                                                            
         ST    R1,0(RF)            AND SAVE                                     
         LA    RE,4(RE)            DO FOR REST OF DEMOS IN LIST                 
         LA    RF,4(RF)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   NADMUL                                                           
NADMULX  DS    0C                                                               
         L     RE,ASVPODEM                                                      
         LA    RF,L'PODDEMO                                                     
         LA    R0,PODDEMO                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE               RESTORE PODDEMO                              
         B     ONEH126                                                          
*                                                                               
ONEH116  L     R2,APDDEML1         DEFAULT TO IAG ORIG/REPEAT DEMO              
         BRAS  RE,IAGDEMS                                                       
         L     R2,APDDEML2                                                      
         BRAS  RE,IAGDEMS                                                       
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'N',APDDEML1),DBLOCK,PDDEMOS,APDPLGL1,0            
***      GOTO1 DEMOUT,DMCB,(C'L',APDDEML1),DBLOCK,PDDEMOS,0,0                   
*                                                                               
***      BRAS  RE,NETHOMES                                                      
*                                                                               
         BRAS  RE,GETCOM           GET COMSCORE DEMOS                           
*                                                                               
         CLC   =C'CNA',PDSOURCE   CABLE NAD & MOVIE GOER/NAD WKLY               
         BNE   *+16                                                             
         L     R1,APDDEML1                                                      
         LA    R2,PDDEMOS                                                       
         BAS   RE,NADCAT                                                        
*                                                                               
         XC    DBSELSYC,DBSELSYC   DONT ADJUST THE DIVISORS                     
         L     RE,APDD2VAL                                                      
         LA    RF,L'PDD2VAL                                                     
         XCEF                                                                   
*                                                                               
* DEIS DEC/2013:                                                                
* WE'RE NOT SURE HOW/WHY APDDEML2 CAN END UP POINTING TO A DEMOLIST             
* CONSISTING OF ONLY NULL ENTRIES, BUT WE'RE PRETTY SURE THAT THERE'S           
* NO NEED TO CALL DEMOUT IN THAT CASE, BECAUSE IT RETURNS NOTHING               
* ANYWAY.                                                                       
         L     RF,APDDEML2                                                      
         LHI   RE,3                RE = L'DEMO LIST ENTRY                       
         CLI   DBDEMTYP,C'4'                                                    
         BNE   *+8                                                              
         LHI   RE,4                                                             
         LR    R1,RE               SAVE L'DEMO LIST ENTRY                       
         BCTR  RE,0                FOR EX                                       
ONEH117A EX    RE,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),0(RF)       IS THIS ENTRY EMPTY?                         
         BNZ   ONEH117B            NO: CALL DEMOUT                              
         AR    RF,R1                                                            
         CLI   0(RF),X'FF'         END OF DEMO LIST?                            
         BE    ONEH117C            YES: WE SAW NO ENTRIES. SKIP DEMOUT.         
         B     ONEH117A            NO: KEEP EXAMINING THE DEMO LIST             
*                                                                               
ONEH117B DS    0H                                                               
         GOTO1 DEMOUT,DMCB,(C'N',APDDEML2),DBLOCK,APDD2VAL,APDPLGL2,0           
***      GOTO1 DEMOUT,DMCB,(C'L',APDDEML2),DBLOCK,APDD2VAL,0,0                  
*                                                                               
ONEH117C DS    0H                                                               
         L     RE,APDDEML2                                                      
         L     RF,APDD2VAL                                                      
         LA    R1,PDDEMOS          A(DEMO VALUES)                               
*                                                                               
ONEH118  CLI   0(RE),X'FF'                                                      
         BE    ONEH126                                                          
         CLI   1(RE),0             ANY DEMO                                     
         BNE   *+8                                                              
         MVI   3(RF),1             NO SECOND - SET TO WEIGHT BY 1               
         OC    0(4,RF),0(RF)       NEED A DENOMINATOR                           
         BNZ   *+10                                                             
         XC    0(4,R1),0(R1)       NONE THERE - ZAP NUMERATOR                   
         CLI   1(RE),C'U'          RATING/PUT/ECT FOR IMPRESSIONS               
         BNE   ONEH120                                                          
         ST    R1,FULL                                                          
         ICM   R1,15,0(RF)                                                      
         MHI   R1,10                                                            
         STCM  R1,15,0(RF)                                                      
         L     R1,FULL                                                          
*                                                                               
ONEH120  CLI   1(RE),C'Z'          IMP/PUT=SHARE FOR IMPRESSIONS                
         BNE   ONEH122                                                          
         ST    R1,FULL                                                          
         L     R1,0(RF)                                                         
         AHI   R1,500                                                           
         SR    R0,R0                                                            
         D     R0,=F'1000'                                                      
         ST    R1,0(RF)                                                         
         L     R1,FULL                                                          
*                                                                               
ONEH122  CLI   1(RE),C'P'          HOMES R/P=SHARE                              
         BNE   ONEH124                                                          
         LR    R2,R1                                                            
         ICM   R1,15,0(R2)                                                      
         MHI   R1,1000                                                          
         STCM  R1,15,0(R2)                                                      
         LR    R1,R2                                                            
*                                                                               
ONEH124  CLI   PDMETHOD,5          METHOD=SOURCE FOR SPOT/REP                   
         BNE   *+12                                                             
         AH    RE,LNDEMCOD         3-BYTE DEMOS                                 
         B     *+8                                                              
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         LA    R1,4(R1)                                                         
         B     ONEH118                                                          
*                                                                               
*--CONVERT NTI TO NAD USA                                                       
ONEH126  CLC   PDSOURCE(3),=C'NAD'                                              
         BE    ONEH130                                                          
         CLC   PDSOURCE(3),=C'NHT'                                              
         BE    ONEH130                                                          
         CLC   PDSOURCE(3),=C'HPM'                                              
         BE    ONEH130                                                          
         CLI   FORCEBTU,C'Y' TVQ                                                
         BE    ONEH130                                                          
         CLI   FORCEBTU,C'O' OPI                                                
         BE    ONEH130                                                          
         CLI   FORCEBTU,C'I' IAG                                                
         BE    ONEH130                                                          
*                                                                               
         CLI   PDMETHOD,5          METHOD=SOURCE FOR SPOT/REP                   
         BNE   ONEH127                                                          
         LA    RE,SVDBLK           RESTORE DBLOCK                               
         LA    RF,DBLOCKL                                                       
         LA    R0,DBLOCK                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
ONEH127  LA    RF,PDNDEMS*10       MAX DEMO CODES                               
         LA    RE,PODDEMO          A(DEMO CODES)                                
*                                                                               
ONEH128  CLI   0(RE),0                                                          
         BNE   *+8                                                              
         MVI   0(RE),1                                                          
         AH    RE,LNDEMCOD                                                      
         CLI   0(RE),X'FF'                                                      
         BE    ONEH130                                                          
         BCT   RF,ONEH128                                                       
*                                                                               
ONEH130  DS    0H                                                               
         OC    DBFACTOR,DBFACTOR                                                
         BZ    ONEHX                                                            
         SR    RF,RF                                                            
         ICM   RF,3,DBFACTOR       SET WEIGHT                                   
         ICM   RE,15,DBAREC                                                     
*                                                                               
         CLC   PDSOURCE(3),=C'ACM'                                              
         BE    *+14                                                             
         CLC   PDSOURCE(4),=C'OOHC'                                             
         BNE   ONEH130A                                                         
         TM    OPTFLAG1,ACMMWTQ    ACM MINUTE WEIGHTING                         
         BO    ONEH130B                                                         
*                                                                               
ONEH130A CLI   PDDUROPT,C'M'       ALLOW TOT DUR FOR "M" OPT                    
         BE    ONEH130B                                                         
         B     BADDURX             DISABLE TOTAL DURATION                       
*                                                                               
ONEH130B CLI   ALLDT,3             FOR ALL DAY/TIME REQUEST                     
         BNE   BADDURX                                                          
         CLI   0(RE),C'Q'          FOR PROGRAM RECORDS ONLY                     
         BNE   BADDURX                                                          
         CLI   DBSELSTA+4,C'C'     CABLE DURATION ARE LONG                      
         BNE   BADDURX                                                          
         CLC   PDSOURCE(3),=C'NTI'    NTI ONLY                                  
         BE    ONEH130C                                                         
         CLC   PDSOURCE(4),=C'OOH '                                             
         BE    ONEH130C                                                         
         CLC   PDSOURCE(5),=C'ACM  '    AND COMMERCIAL AVERAGE                  
         BE    ONEH130C                                                         
         CLC   PDSOURCE(5),=C'OOHC '    AND COMMERCIAL AVERAGE                  
         BNE   BADDURX                                                          
ONEH130C ICM   RE,15,DBAQUART                                                   
         CLI   2(RE),2             ONLY DO FOR TELECASTS                        
         BNE   BADDURX                                                          
BADDUR   CLI   0(RE),X'10'                                                      
         JE    BADDUR2                                                          
         JH    BADDURX                                                          
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     BADDUR                                                           
         USING PHTELEM,RE                                                       
BADDUR2  ICM   RF,3,PHTCOVR        CABLE HAS REAL DUR HERE                      
         DROP  RE                                                               
BADDURX  DS    0C                                                               
         STCM  RF,15,PDWEIGHT                                                   
         CLI   PDOVSYS,3           ONLY FOR NETWORK CURRENTLY                   
         BNE   ONEH132                                                          
*                                                                               
         CLC   =C'CNA',PDSOURCE    WEIGHT BY NUMBER OF RUN DAYS                 
         BNE   BADDURX1                                                         
         GOTOR SUBR03,DMCB,('DAYWGHTE',(RC))                                    
*                                                                               
BADDURX1 CLC   =C'CNAD',PDSOURCE       COULD ALSO BE TP READ FOR                
         BNE   ONEH131                 STA W/O PROG DATA                        
         CLI   DBFUNCT,DBGETDEM        TP READ?                                 
         BNE   ONEH131                                                          
         GOTOR SUBR03,DMCB,('GETNOWKE',(RC))                                    
         ICM   RE,15,PDWEIGHT      WEIGHT BY                                    
         ZIC   RF,WEEKNUM          NUMBER OF WEEKS IN THE MONTH                 
         B     ONEH131A                                                         
*                                                                               
ONEH131  LR    RE,RF                                                            
         ZIC   R0,MAXWKS           WEIGHT BY WEEKS                              
         SR    RF,RF                                                            
         CLC   =C'CNA',PDSOURCE    WE HAVE A CALL TO DEMAND FOR EA WEEK         
         BE    ONEH131A                                                         
         LA    R1,PDWEEK                                                        
         TM    0(R1),X'F0'         NUMERIC?                                     
         BNO   *+8                                                              
         LA    RF,1(RF)            YES. COUNT THIS WEEK                         
         LA    R1,1(R1)                                                         
         BCT   R0,*-16                                                          
ONEH131A LTR   RF,RF                                                            
         BZ    ONEH132                                                          
         MR    RE,RE                                                            
         STCM  RF,15,PDWEIGHT                                                   
*                                                                               
ONEH132  DS    0H                  WEIGHT CNAD PROG-AVG BY TOT DURATION         
         GOTOR SUBR03,DMCB,('WTCNADPE',(RC))                                    
*                                                                               
*                                  SAVE PROGRAM/STATION (PDF)                   
         GOTOR SUBR02,DMCB,('SAVPROGE',(RC))                                    
         BNE   ONEHX               SKIP THIS ONE                                
*                                                                               
*                                  FILTER BY PROGRAM DATES/TIMES                
         GOTOR SUBR02,DMCB,('FILPDATE',(RC))                                    
         BZ    ONEHX               SKIP THIS ONE                                
*                                                                               
*                                                                               
         L     RF,MPAHOOK                                                       
         LTR   RF,RF                                                            
         BZ    ONEH134                                                          
*                                                                               
         LA    RE,DBLOCK           SAVE DBLOCK                                  
         LA    RF,DBLOCKL                                                       
         LA    R0,SVDBLK                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BAS   RE,GETMPAHK                                                      
*                                                                               
         LA    RE,SVDBLK           RESTORE DBLOCK                               
         LA    RF,DBLOCKL                                                       
         LA    R0,DBLOCK                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     ONEHX               DEMOS RELEASED IN MPAHOOK                    
*                                                                               
ONEH134  DS    0H                  FOR COMMERCIAL AVERAGE DATA,                 
         CLI   PDOVSYS,3           NETWORK ONLY                                 
         BNE   ONEH135                                                          
         ICM   RE,15,DBAREC        WEIGHT IS NUM OF COMMERCIAL SECONDS          
         CLI   1(RE),C'A'          PMMEDIA OR PRMEDIA                           
         BNL   ONEH135                                                          
         TM    OPTFLAG1,ACMMWTQ    UNLESS MINUTE WEIGHTING REQUESTED            
         BO    ONEH135                                                          
         GOTO1 DEFINE,DMCB,=C'COMS ',DBLOCK,WORK                                
         SR    RE,RE                                                            
         ICM   RE,3,WORK                                                        
         STCM  RE,15,PDWEIGHT      COMMERCIAL SECONDS                           
*                                                                               
* FOR SYNDICATION, WE RECEIVE ONLY WEEKLY AVERAGES AND NO INDIVIDUAL            
* DAYS. ON A REQUEST FOR INDIVIDUAL DAYS (DBBEST NOT 'L'), DEMAND               
* RETURNS THE AVERAGE RECORD ONCE FOR EACH DAY IN THE AVERAGE. THE              
* COMMERCIAL DURATION PROVIDED IS FOR ALL THE DAYS IN THE AVERAGE, SO           
* WE NEED TO DIVIDE IT BY THE NUMBER OF DAYS.                                   
*                                                                               
         CLI   PDMEDIA,C'S'        FOR SYNDICATION                              
         BNE   ONEH135                                                          
         CLI   DBBEST,C'L'         DBBEST=L                                     
         BE    ONEH135             DON'T DIVIDE IF ONE-LINE AVERAGE             
         GOTO1 DEFINE,DMCB,=C'CNTDAYS',DBLOCK,WORK                              
         ZIC   R0,WORK             NUMBER OF DAYS                               
         LTR   R0,R0                                                            
         BZ    ONEH135                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RF,15,PDWEIGHT      TOTAL COMMERCIAL DURATION                    
         DR    RE,R0               DIVIDED BY NUMBER OF DAYS                    
         MHI   RE,2                PERFORM ROUNDING                             
         CR    RE,R0                                                            
         BL    *+8                                                              
         AHI   RF,1                                                             
         STCM  RF,15,PDWEIGHT      COMMERCIAL DURATION FOR THE IND DAY          
*                                                                               
*                                                                               
ONEH135  CLC   =C'MXM ',PDSOURCE   FOR MXM SOURCE                               
         BNE   ONEH135X                                                         
         CLI   OPTSCWGT,OPTSWS     COMMERCIAL SECONDS WEIGHTING                 
         BNE   ONEH135A                                                         
         ZIC   RE,PDMCOMSC                                                      
         B     ONEH135C                                                         
ONEH135A CLI   OPTSCWGT,OPTSWTS    TOTAL COMM SECONDS WEIGHTING                 
         BNE   ONEH135B                                                         
         ZIC   RE,PDMCOMSC         COMMERCIAL SECONDS                           
         ZIC   RF,PDMPROSC         + PROMO SECONDS                              
         AR    RE,RF                                                            
         ZIC   RF,PDMPSASC         + PSA SECONDS                                
         AR    RE,RF                                                            
         B     ONEH135C                                                         
ONEH135B CLI   OPTSCWGT,OPTSWPD    POD SECONDS WEIGHTING                        
         BNE   ONEH135X                                                         
         LHI   RE,60               MIDDLE MINUTES HAVE WEIGHT 60 SECS           
         CLI   PDPD1LST,0                                                       
         BE    ONEH135C                                                         
         ZIC   RE,PDMCOMSC         COMM SECONDS FOR 1ST AND LAST MINUTE         
         ZIC   RF,PDMPROSC         + PROMO SECONDS                              
         AR    RE,RF                                                            
         ZIC   RF,PDMPSASC         + PSA SECONDS                                
         AR    RE,RF                                                            
ONEH135C STCM  RE,15,PDWEIGHT                                                   
ONEH135X DS    0X                                                               
*                                                                               
         CLC   PREVSTA,PDSTAT                                                   
         BNE   *+14                                                             
         CLC   PREVMKT,PDMRKT                                                   
         BE    ONEH136                                                          
         GOTOR SUBR01,DMCB,('MRKTNAE',(RC))                                     
*                                                                               
ONEH136  DS    0H                                                               
*        MVC   PDNET+3(1),DBBTYPE  DISPLAY BOOK TYPE (FOR TESTING)              
*        MVC   BYTE,PDNET+3                                                     
*        NI    BYTE,X'F0'        KEEP FIRST NIBBLE                              
*        OI    BYTE,X'00'                                                       
*        BNZ   *+8                                                              
*        OI    PDNET+3,X'F0'     DISPLAY DIGIT                                  
*                                                                               
         MVC   SVSOURCE,PDSOURCE                                                
         CLC   =C'FUS',PDSOURCE  WHEN SWITCHING TO NSI WIRED FOR FUSION         
         BNE   ONEH140           DISPLAY SOURCE NSI                             
         CLI   DBSELSRC,C'N'                                                    
         BNE   ONEH140                                                          
         MVC   PDSOURCE,=CL5'NSI'                                               
                                                                                
ONEH140  L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         GOTOR SUBR01,DMCB,('SPDDAYE',(RC))                                     
         MVI   GLMODE,GLINPUT                                                   
*                                                                               
         BRAS  RE,SETPDSRC         SET PDSOURCE                                 
*                                                                               
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
         BRAS  RE,RESPDSRC         RESTORE PDSROUCE                             
*                                                                               
         XC    PDRUN,PDRUN                                                      
         MVC   PREVSTA,PDSTAT                                                   
         MVC   PREVMKT,PDMRKT                                                   
         MVC   PDSOURCE,SVSOURCE                                                
*                                                                               
ONEH150  CLC   =C'FUS',PDSOURCE    FOR FUSION AND NSI-WIRED                     
         BE    ONEH155                                                          
         CLC   =C'NSI',PDSOURCE                                                 
         BE    ONEH153                                                          
         CLC   =C'TP',PDSOURCE                                                  
         BE    ONEH153                                                          
         CLC   =C'T4',PDSOURCE                                                  
         BE    ONEH153                                                          
         B     ONEHX                                                            
ONEH153  CLI   DBBTYPE,C'W'                                                     
         BNE   ONEHX                                                            
*                                                                               
ONEH155  TM    PDFLAG1,PDF1SYSC    AND SYSCODE KEYWORD REQUESTED,               
         BNO   ONEHX                                                            
         ICM   RE,15,ACURRSYC      DO ALL SYSCODES ONE BY ONE                   
         BZ    ONEHX                                                            
         LA    RE,L'DSYSYSCD(RE)                                                
         CLC   =X'FFFF',0(RE)                                                   
         BE    *+12                                                             
         ST    RE,ACURRSYC                                                      
         B     ONEH                                                             
         XC    DBSELSYC,DBSELSYC                                                
         B     ONEHX                                                            
*                                                                               
ONEHX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* NADCAT   - FOR CABLE NAD, MATCH ON DEMO CATEGORY                              
*       IF NO MATCH ON THE RECORD, TURN ON THE HIGH ORDER OF DEMO VALUE         
*       R1-> A(DEMO EXPRESSIONS)                                                
*       R2-> A(OUTPUT AREA)                                                     
***********************************************************************         
NADCAT   DS    0H                                                               
         LR    R5,RE                                                            
         ST    R1,AREG1                                                         
         ST    R2,AREG2                                                         
         GOTOR SUBR03,DMCB,('NADCATE',(RC))                                     
         BR    R5                                                               
         EJECT                                                                  
***********************************************************************         
*        GETMPAHK                                                     *         
***********************************************************************         
*                                                                               
GETMPAHK NTR1                                                                   
         GOTOR SUBR02,DMCB,('GETMPAHE',(RC))                                    
GETMX    J     XIT                                                              
*                                                                               
* NOTE: R8 IS NOT BEING DROPPED, BECAUSE FIELDS PREVSTA AND PREVMKT             
*       ARE ADDRESSED BY R8, AND ARE NEEDED FURTHER DOWN.                       
*                                                                               
         DROP  RB,RA                                                            
         EJECT                                                                  
         EJECT                                                                  
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
OREP     DC    C'REP'                                                           
OFORMAT  DC    C'PAVUIUN',X'520B00'                                             
*                                                                               
PREVSTA  DC    CL4' '              PREVIOUS STATION                             
PREVMKT  DC    CL2' '              PREVIOUS MARKET                              
PREVNTI  DC    AL2(0)              PREVIOUS NTI#                                
PREVPDNU DC    AL2(0)              PREVIOUS POD NUMBER                          
FF       EQU   X'FF'                                                            
*                                                                               
DEMOSHR  DS    0XL3                HOMES SHARES                                 
         DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
                                                                                
SPOTDAYS DC    X'7C402010080402017F0003FF'                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
EXTTAB   DS    0D                                                               
         DC    AL4(MYIO-WORKD),AL4(AMYIO-WORKD)                                 
         DC    AL4(MYIO2-WORKD),AL4(AMYIO2-WORKD)                               
         DC    AL4(MYDBLK-WORKD),AL4(AMYDBLK-WORKD)                             
         DC    AL4(IUNCOND-WORKD),AL4(AIUNCOND-WORKD)                           
         DC    AL4(SORTREC-WORKD),AL4(ASORTREC-WORKD)                           
         DC    AL4(SORTDEM-WORKD),AL4(ASORTDEM-WORKD)                           
         DC    AL4(SVPODEM-WORKD),AL4(ASVPODEM-WORKD)                           
         DC    AL4(EXDEMOS-WORKD),AL4(AEXDEMOS-WORKD)                           
         DC    AL4(UPDEMOS-WORKD),AL4(AUPDEMOS-WORKD)                           
         DC    AL4(UPBLOCK-WORKD),AL4(AUPBLOCK-WORKD)                           
         DC    AL4(ZZZLIST-WORKD),AL4(AZZZLIST-WORKD)                           
         DC    AL4(ZZZPIV-WORKD),AL4(AZZZPIV-WORKD)                             
         DC    AL4(SVSYCDS-WORKD),AL4(ASVSYSC-WORKD)                            
EXTTABL  EQU   (*-EXTTAB)/L'EXTTAB                                              
*                                                                               
         EJECT                                                                  
DBSTATYP DC    CL7'NAD  PN'                                                     
         DC    CL7'NAD-TTN'                                                     
         DC    CL7'NAD-DDN'                                                     
*                                                                               
         DC    CL7'NAW  PN'                                                     
         DC    CL7'NAW-TTN'                                                     
         DC    CL7'NAW-DDN'                                                     
*                                                                               
         DC    CL7'NHT  PN'                                                     
         DC    CL7'NHT-TTN'                                                     
         DC    CL7'NHT-DDN'                                                     
*                                                                               
         DC    CL7'HPM  PN'                                                     
         DC    CL7'HPM-TTN'                                                     
         DC    CL7'HPM-DDN'                                                     
*                                                                               
         DC    CL7'NHW  PN'                                                     
         DC    CL7'NHW-TTN'                                                     
         DC    CL7'NHW-DDN'                                                     
*                                                                               
         DC    CL7'HPW  PN'                                                     
         DC    CL7'HPW-TTN'                                                     
         DC    CL7'HPW-DDN'                                                     
*                                                                               
         DC    X'FF'                                                            
*                                                                               
*                                  DETERMINE DBFUNCT SETTING                    
DBFUNTAB DC    CL5'NAD  '                                                       
         DC    CL5'NAD-T'                                                       
         DC    CL5'NAD-D'                                                       
*                                                                               
         DC    CL5'NAW  '                                                       
         DC    CL5'NAW-T'                                                       
         DC    CL5'NAW-D'                                                       
*                                                                               
         DC    CL5'NTI  '                                                       
         DC    CL5'NTI-N'                                                       
         DC    CL5'NHC  '                                                       
         DC    CL5'HPC  '                                                       
         DC    CL5'ACM  '                                                       
*                                                                               
         DC    CL5'TCAR '                                                       
         DC    CL5'WB1  '                                                       
         DC    CL5'ACMWB'                                                       
         DC    CL5'NCOS '                                                       
*                                                                               
         DC    CL5'NMI  '                                                       
         DC    CL5'NMI-A'                                                       
*                                                                               
         DC    CL5'NHT  '                                                       
         DC    CL5'NHT-T'                                                       
         DC    CL5'NHT-D'                                                       
*                                                                               
         DC    CL5'HPM  '                                                       
         DC    CL5'HPM-T'                                                       
         DC    CL5'HPM-D'                                                       
*                                                                               
         DC    CL5'NHW  '                                                       
         DC    CL5'NHW-T'                                                       
         DC    CL5'NHW-D'                                                       
*                                                                               
         DC    CL5'HPW  '                                                       
         DC    CL5'HPW-T'                                                       
         DC    CL5'HPW-D'                                                       
*                                                                               
         DC    CL5'CNAD '                                                       
*                                                                               
         DC    CL5'CNAW '                                                       
*                                                                               
         DC    CL5'OOH  '          PROGRAM AVERAGE                              
         DC    CL5'OOHC '          COMMERCIAL AVERAGE                           
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
DAYPTAB  DC    X'00',C'YYY'                                                     
         DC    X'40',C'MON'                                                     
         DC    X'20',C'TUE'                                                     
         DC    X'10',C'WED'                                                     
         DC    X'08',C'THU'                                                     
         DC    X'04',C'FRI'                                                     
         DC    X'02',C'SAT'                                                     
         DC    X'01',C'SUN'                                                     
         DC    X'7C',C'M-F'                                                     
         DC    X'7F',C'M-S'                                                     
         DC    X'FC',C'M-F'        INDIVIDUAL                                   
         DC    X'FF',C'ALL'                                                     
         DC    X'C1',C'VAR'        END OF TABLE                                 
*                                                                               
PAYCAB   DC    CL4'CMAX'                                                        
         DC    CL4'MAXP'                                                        
         DC    CL4'DSNY'                                                        
         DC    CL4'ENCY'                                                        
         DC    CL4'HBO '                                                        
         DC    CL4'HBOM'                                                        
         DC    CL4'ITV '                                                        
         DC    CL4'JADE'                                                        
         DC    CL4'NGN '                                                        
         DC    CL4'PLBY'                                                        
         DC    CL4'RPT '                                                        
         DC    CL4'SHOW'                                                        
         DC    CL4'SHO1'                                                        
         DC    CL4'SPCE'                                                        
         DC    CL4'STPZ'                                                        
         DC    CL4'TMC '                                                        
         DC    CL4'TMC1'                                                        
         DC    X'FFFF'                                                          
       ++INCLUDE DESTACODE                                                      
***********************************************************************         
*        GET EXTRACTION DEMOS                                         *         
*                                                                     *         
***********************************************************************         
*                                                                               
MKEXDEMS NTR1  BASE=*,LABEL=*                                                   
         L     R3,PDADEMTB         GET DEMO BLOCK                               
         USING PDDMBUFF,R3                                                      
         L     R1,AEXDEMOS                                                      
         LA    RF,PODDEMO                                                       
*                                                                               
MKEX02   CLI   0(RF),X'FF'         AT END, DONE                                 
         BE    MKEXX                                                            
         CLI   DBBTYPE,C'U'                                                     
         BE    MKEX04                                                           
         CLI   0(RF),171           IF USER ADJUST                               
         BNE   *+8                                                              
         MVI   FORCEBTU,C'Y'       FORCE BOOK TYPE "U" LOOKUP(TVQ)              
         CLI   0(RF),172           IF USER ADJUST                               
         BNE   *+8                                                              
         MVI   FORCEBTU,C'O'       FORCE BOOK TYPE "U" LOOKUP(OPI)              
*                                                                               
         CLC   =C'IAG',PDSOURCE                                                 
         BE    MKEX04                                                           
         CLI   0(RF),175           IAG ORIGINAL                                 
         BE    MKEX03                                                           
         CLI   0(RF),176           IAG REPEAT                                   
         BE    MKEX03                                                           
         CLI   0(RF),177           IAG AUTOMATIC ORIGINAL/REPEAT                
         BNE   MKEX04                                                           
MKEX03   MVI   FORCEBTU,C'I'       FORCE BOOK TYPE "U" LOOKUP(IAG)              
*                                                                               
MKEX04   MVC   0(3,R1),0(RF)       MOVE FROM PODDEMO EXDEMOS                    
         MVI   0(R1),0                                                          
         AH    RF,LNDEMCOD                                                      
         AH    R1,LNDEMCOD                                                      
         B     MKEX02                                                           
*                                                                               
MKEXX    MVI   0(R1),X'FF'         MARK END OF EXDEMOS                          
         J     XIT                                                              
         LTORG                                                                  
         DROP  R3                                                               
***********************************************************************         
*        DRIVER HOOK                                                  *         
***********************************************************************         
*                                                                               
DRHOOK   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
*                                                                               
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         JE    PRINT                                                            
         CLI   GLHOOK,GLLAST       LAST TIME                                    
         JE    LASTTIME                                                         
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         JE    HEADHK                                                           
*                                                                               
DRHOOKX  J     XIT                                                              
*                                                                               
*                                                                               
LASTTIME NI    GLINDS3,255-GLLSTLIN                                             
         MVI   GLHOOK,GLROUT                                                    
*                                                                               
LASTTX   J     DRHOOKX                                                          
*                                                                               
*                                                                               
PRINT    CLI   PRTSW,C'N'                                                       
         JNE   PRINX                                                            
         MVI   GLHOOK,GLDONT                                                    
         MVI   PRTSW,C'Y'                                                       
*                                                                               
PRINX    J     DRHOOKX                                                          
*                                                                               
*                                                                               
HEADHK   LA    R2,SPLSBK           BOOK                                         
         GOTO1 GENHEAD             LINE UP HEADLINES                            
*                                                                               
HEADX    J     DRHOOKX                                                          
*                                                                               
* FOR CNAD/CNAW, REPORT BOU/GRT/ESC/LAF  (SPEC-24119)                           
*                REPORT HI/MET/STV       (SPEC-38028)                           
*                REPORT MYS              (SPEC-49924)                           
* ON ENTRY, R2 = SOURCE                                                         
*           R3 = STATION                                                        
*                                                                               
TSTCNA   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'BOU T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'ESC T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'GRT T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'LAF T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'ION T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'HI  T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'MET T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'STV T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'MYS T',0(R3)                                                  
         JNE   NO                                                               
*                                                                               
         CLC   =C'CNAD',CURSRC                                                  
         JE    YES                                                              
         CLC   =C'CNAW',CURSRC                                                  
         JE    YES                                                              
         J     NO                                                               
         LTORG                                                                  
*                                                                               
* FOR CNAD/CNAW, REPORT BOU/GRT/ESC/LAF  (SPEC-24119)                           
*                REPORT MYS              (SPEC-49924)                           
* ON ENTRY, R2 = SOURCE                                                         
*           R3 = STATION                                                        
*                                                                               
SETCNA   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'BOU T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'ESC T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'GRT T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'LAF T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'ION T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'HI  T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'MET T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'STV T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'MYS T',0(R3)                                                  
         JNE   SCNAX                                                            
*                                                                               
         CLC   =C'CNAD',CURSRC                                                  
         JNE   SCNA02                                                           
         OI    SVFLAG,PODCNADQ                                                  
         MVC   1(4,R2),=C'NAD '                                                 
         CLI   0(R2),1             RANGE OF BOOKS?                              
         JNE   *+10                                                             
         MVC   1+PODBLNQ(4,R2),=C'NAD '                                         
         B     SCNA04                                                           
*                                                                               
SCNA02   CLC   =C'CNAW',CURSRC                                                  
         JNE   SCNAX                                                            
         OI    SVFLAG,PODCNAWQ                                                  
         MVC   1(7,R2),=C'NAW  NW'                                              
         CLI   0(R2),1             RANGE OF BOOKS?                              
         JNE   *+10                                                             
         MVC   1+PODBLNQ(7,R2),=C'NAW  NW'                                      
*                                                                               
SCNA04   MVC   SVDAYSOP,DAYSOPT                                                 
         MVI   DAYSOPT,C'I'        USE INDIVIDUAL DAY                           
         MVC   SVVAROPT,VAROPT                                                  
         MVI   VAROPT,C'Y'                                                      
         MVC   SVPODBD,PODBD                                                    
         MVC   PODBD(PODBLNQ),0(R2)                                             
*                                                                               
         CLI   PDMETHOD,4          METHOD = SOURCE?                             
         JE    SCNAX                                                            
         MVC   SVBASE,PDBASE                                                    
         MVC   SVDUROPT,PDDUROPT                                                
*                                                                               
         MVI   PDBASE,C'R'                                                      
         CLI   PDDUROPT,C'M'                                                    
         JNE   *+8                                                              
         MVI   PDDUROPT,0                                                       
*                                                                               
SCNAX    J     XIT                                                              
         LTORG                                                                  
*                                                                               
* FOR CNAD/CNAW, REPORT BOU/GRT/ESC/LAF  (SPEC-24119)                           
*                REPORT MYS              (SPEC-49924)                           
* ON ENTRY, R2 = SOURCE                                                         
*           R3 = STATION                                                        
*                                                                               
RESTCNA  NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'BOU T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'ESC T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'GRT T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'LAF T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'ION T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'HI  T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'MET T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'STV T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'MYS T',0(R3)                                                  
         JNE   RCNAX                                                            
*                                                                               
         CLC   =C'CNAD',CURSRC                                                  
         JNE   RCNA00                                                           
         MVC   1(4,R2),=C'CNAD'                                                 
         CLI   0(R2),1             RANGE OF BOOKS?                              
         JNE   *+10                                                             
         MVC   1+PODBLNQ(4,R2),=C'CNAD'                                         
         B     RCNA02                                                           
*                                                                               
RCNA00   CLC   =C'CNAW',CURSRC                                                  
         JNE   RCNAX                                                            
         MVC   1(7,R2),=C'CNAW NN'                                              
         CLI   0(R2),1             RANGE OF BOOKS?                              
         JNE   *+10                                                             
         MVC   1+PODBLNQ(7,R2),=C'CNAW NN'                                      
*                                                                               
RCNA02   MVC   DAYSOPT,SVDAYSOP                                                 
         MVC   VAROPT,SVVAROPT                                                  
         MVC   PODBD,SVPODBD                                                    
*                                                                               
         CLI   PDMETHOD,4          METHOD = SOURCE?                             
         JE    RCNAX                                                            
         MVC   PDBASE,SVBASE                                                    
         MVC   PDDUROPT,SVDUROPT                                                
*                                                                               
RCNAX    J     XIT                                                              
         LTORG                                                                  
*                                                                               
* FOR CNAD/CNAW, REPORT BOU/GRT/ESC/LAF  (SPEC-24119)                           
*                REPORT MYS              (SPEC-49924)                           
*                                                                               
SETPDSRC NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'BOU ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'ESC ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'GRT ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'LAF ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'ION ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'HI  ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'MET ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'STV ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'MYS ',PDNET                                                   
         JNE   SPDSRCX                                                          
*                                                                               
         CLC   =C'CNAD',CURSRC                                                  
         JNE   *+10                                                             
         MVC   PDSOURCE,=C'CNAD '                                               
         CLC   =C'CNAW',CURSRC                                                  
         JNE   SPDSRCX                                                          
         MVC   PDSOURCE,=C'CNAW '                                               
*                                                                               
SPDSRCX  J     XIT                                                              
         LTORG                                                                  
*                                                                               
* FOR CNAD/CNAW, REPORT BOU/GRT/ESC/LAF  (SPEC-24119)                           
*                REPORT MYS              (SPEC-49924)                           
*                                                                               
RESPDSRC NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'BOU ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'ESC ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'GRT ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'LAF ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'ION ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'HI  ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'MET ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'STV ',PDNET                                                   
         JE    *+10                                                             
         CLC   =C'MYS ',PDNET                                                   
         JNE   RPDSRCX                                                          
*                                                                               
         CLC   =C'CNAD',CURSRC                                                  
         JNE   *+10                                                             
         MVC   PDSOURCE,=C'NAD  '                                               
         CLC   =C'CNAW',CURSRC                                                  
         JNE   RPDSRCX                                                          
         MVC   PDSOURCE,=C'NAW  '                                               
*                                                                               
RPDSRCX  J     XIT                                                              
         LTORG                                                                  
*                                                                               
***********************************************************************         
*        GET ADDRESSABILTY TO STORAGE AREAS                           *         
***********************************************************************         
*                                                                               
INITIAL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   R1,EXTTAB                                                        
         LA    R0,EXTTABL                                                       
*                                                                               
INIT02   LM    RE,RF,0(R1)                                                      
         LA    RE,WORKD(RE)                                                     
         LA    RF,WORKD(RF)                                                     
         ST    RE,0(RF)                                                         
         LA    R1,L'EXTTAB(R1)                                                  
         BCT   R0,INIT02                                                        
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* INCREMENT SPOT WEEKLY BOOK                                                    
*                                                                               
WKLYSPT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'WTP',PDSOURCE                                                 
         BNE   WKS02                                                            
         MVI   BYTE,0              SUNDAY START OF THE WEEK                     
         B     WKS10                                                            
WKS02    CLC   =C'OTP',PDSOURCE                                                 
         BNE   WKS04                                                            
         MVI   BYTE,1              MONDAY START OF THE WEEK                     
         B     WKS10                                                            
WKS04    CLC   =C'OPA',PDSOURCE                                                 
         BNE   WKSNEQ              NOT A WEEKLY SPOT SOURCE                     
         MVI   BYTE,1              MONDAY START OF THE WEEK                     
*                                                                               
WKS10    DS    0X                  GET WEEK DATE                                
         GOTO1 NSIWEEK,DMCB,(C'D',PDSBOOK),(BYTE,GETDAY),ADDAY,DATCON           
         SR    R2,R2                                                            
         ICM   R2,7,DMCB+1         A(OUTPUT DATE)                               
*                                  ADD 7 DAYS TO DATE                           
         GOTO1 ADDAY,DMCB,(C'D',(R2)),WORK,7                                    
*                                  CONVERT BACK TO BOOK                         
         GOTO1 NSIWEEK,DMCB,WORK,(BYTE,GETDAY),ADDAY,DATCON                     
         MVC   PDSBOOK(1),DMCB+4   YEAR NO.                                     
         MVC   PDSBOOK+1(1),DMCB   WEEK NO.                                     
*                                                                               
WKSEQ    CR    RE,RE                                                            
         B     WKSBX                                                            
WKSNEQ   CHI   RB,0                                                             
WKSBX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* FILTER ON REQUESTED SOURCE + VIEWING TYPE COMBO                               
*                                                                               
SVTFILT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    PDSVTS,PDSVTS       NO FILTER PRESENT                            
         BZ    SVTFILTY            ALLOW EVERYTHING                             
*                                                                               
         ICM   RE,15,AVIEWTYP                                                   
         BNZ   *+6                                                              
         DC    H'0'                SHOULD ALWAYS BE FILLED IN FOR SVT           
*                                                                               
         CLI   0(RE),DBXLCLCQ      PAV LIVE COMMERCIAL                          
         JE    SVTFILTN                                                         
         CLI   0(RE),DBXLCL3Q      PAV LIVE+3 COMMERCIAL                        
         JE    SVTFILTN                                                         
         CLI   0(RE),DBXLCL7Q      PAV LIVE+7 COMMERCIAL                        
         JE    SVTFILTN                                                         
*                                                                               
SVTF05   LA    RF,PDSVTS                                                        
SVTF10   CLI   0(RF),X'FF'                                                      
         BE    SVTFILTN            NOT A REQUESTED COMBO. REJECT IT.            
         CLC   0(5,RF),1(R2)       MATCH ON SOURCE                              
         BNE   SVTF20                                                           
         CLC   5(1,RF),0(RE)       MATCH ON VIEWING TYPE                        
         BE    SVTFILTY            COMBO SOURCE+VTYPE FOUND. ALLOW IT.          
*                                                                               
SVTF20   LA    RF,6(RF)                                                         
         B     SVTF10                                                           
*                                                                               
SVTFILTY CR    RB,RB                                                            
         B     SVTFILTX                                                         
SVTFILTN CHI   RB,0                                                             
SVTFILTX J     XIT                                                              
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
         TITLE 'READ STATION RECORD TO RETRIEVE ALL PIV STATIONS'               
*                                                                               
GTALLSTA NMOD1 0,*GTALLSTA**                                                    
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
         CLI   0(R1),1             SEE WHICH ROUTINE TO RUN                     
         BE    GETNALL                                                          
         B     GETPALL                                                          
*                                                                               
GETALLXT XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL RGTALLSTA ROUTINES                                *         
***********************************************************************         
*                                                                               
GETPALL  DS    0H                                                               
         CLC   1(3,R2),=C'NTI'                                                  
         BE    GETPX                                                            
         CLC   1(4,R2),=C'OOH '                                                 
         BE    GETPX                                                            
         CLC   1(5,R2),=C'ACM  '                                                
         BE    GETPX                                                            
         CLC   1(5,R2),=C'OOHC '                                                
         BE    GETPX                                                            
         CLC   1(5,R2),=C'MXM  '                                                
         BE    GETPX                                                            
         CLC   1(3,R2),=C'NAD'                                                  
         BE    GETPX                                                            
         CLC   1(3,R2),=C'REN'                                                  
         BE    GETPX                                                            
         CLC   1(3,R2),=C'COM'                                                  
         BE    GETPX                                                            
*        L     R3,AIO2                                                          
*        LA    R3,1330(R3)                                                      
*        XCEF  0(R3),670                                                        
*        LA    R5,83               MAX STATIONS IN TABLE                        
         L     R3,AZZZPIV                                                       
         XCEF  0(R3),3400                                                       
         MVI   0(R3),X'FF'                                                      
         LA    R5,424              MAX STATIONS IN TABLE                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=CL2'SN'                                                  
         USING STAREC,R4                                                        
         LA    R4,KEY                                                           
         MVC   AIO,AIO2                                                         
         MVC   STAKAGY,AGENCY                                                   
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=C'STATION'                                             
         GOTO1 HIGH                                                             
         B     GETP04                                                           
*                                                                               
GETP02   GOTO1 SEQ                                                              
*                                                                               
GETP04   CLC   KEY(2),KEYSAVE                                                   
         BNE   GETPX                                                            
         CLC   AGENCY,STAKAGY                                                   
         BNE   GETP02                                                           
*                                                                               
         L     R4,AIO                                                           
         CLI   PODNETMB,C'T'                                                    
         BNE   *+16                                                             
         CLI   STYPE,C'N'                                                       
         BNE   GETP02                                                           
         B     *+14                                                             
         CLC   STYPE,PODNETMB                                                   
         BNE   GETP02                                                           
         PACK  DUB,SMKT            CONVERT 'MARKET' NUMBER                      
         CVB   R1,DUB                                                           
         STH   R1,DUB+4                                                         
         MVC   DUB(4),STAKCALL                                                  
         BAS   RE,LOADTAB                                                       
         BNZ   GETP02              DUPLICATE STATION DONT COUNT                 
         BCT   R5,GETP02                                                        
*                                                                               
GETPX    XC    FILENAME,FILENAME                                                
         B     GETALLXT                                                         
         DROP  R4                                                               
*                                                                               
*                                                                               
*--READ STATION RECORD TO RETREVE ALL NTI STATIONS                              
*                                                                               
GETNALL  DS    0H                  GET ALL SYNDICATORS                          
*                                                                               
         CLC   =C'NHT',1(R2)       HISPANIC WEIGHTED-ADJUST INDICATORS          
         BE    *+10                                                             
         CLC   =C'HPM',1(R2)                                                    
         BE    *+10                                                             
         CLC   =C'NHW',1(R2)                                                    
         BE    *+10                                                             
         CLC   =C'HPW',1(R2)                                                    
         BNE   GETN01                                                           
         CLI   5(R3),C'E'          BTYPE FROM NETWORK                           
         BE    *+8                                                              
         CLI   14(R2),C'E'         BTYPE FROM DATE                              
         BNE   GETN01                                                           
         MVI   PDZZZTYP,X'88'      SET TO LOWER CASE 'H'                        
         MVI   PODNETMB,X'88'                                                   
         MVI   PODNETMB+1,C'E'                                                  
GETN01   DS    0H                                                               
*                                                                               
         L     R3,AZZZLIST                                                      
         ICM   R1,15,PODNETAD                                                   
*                                                                               
         CLC   0(5,R3),1(R2)       OPTIMIZE AT LEAST ONE ZZZ                    
         BNE   *+10                                                             
         CLC   5(5,R3),0(R1)                                                    
         BNE   *+12                                                             
         LA    R3,10(R3)                                                        
         B     GETALLXT                                                         
*                                                                               
         XCEF  0(R3),3400          OTHERWISE BUILD A TABLE                      
         MVC   0(5,R3),1(R2)                                                    
         MVC   5(5,R3),0(R1)                                                    
         LA    R3,10(R3)                                                        
         MVI   0(R3),X'FF'                                                      
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(5),=C'SIN S'    ADD SIN TO ALL LOOKUPS                       
         BAS   RE,LOADTAB                                                       
*                                                                               
         ICM   R1,15,PODNETAD                                                   
         CLC   =C'ZZZ T',0(R1)                                                  
         BNE   GETN01A                                                          
         CLC   =C'NTI  ',1(R2)                                                  
         BE    GETN20                                                           
         CLC   =C'OOH  ',1(R2)                                                  
         BE    GETN20                                                           
         CLC   =C'MXM  ',1(R2)                                                  
         BE    GETN20                                                           
         CLC   =C'ACM  ',1(R2)                                                  
         BE    GETN20                                                           
         CLC   =C'OOHC ',1(R2)                                                  
         BE    GETN20                                                           
         CLC   =C'HPM  ',1(R2)                                                  
         BE    GETN20                                                           
         CLC   =C'IAG',1(R2)       IAG ESTIMATED AND ACTUAL USE THE             
         BE    GETN20              STANDARD NATIONAL CALL LETTERS               
         CLC   =C'COM  ',1(R2)                                                  
         BE    GETN20                                                           
*                                                                               
GETN01A  CLC   =C'ZZZ S',0(R1)                                                  
         BNE   *+14                                                             
         CLC   =C'OPI  ',1(R2)                                                  
         BE    GETOPIF                                                          
*                                                                               
         CLC   =C'ZZZ T',0(R1)                                                  
         BNE   *+14                                                             
         CLC   =C'OPI  ',1(R2)                                                  
         BE    GETOPIN                                                          
*                                                                               
         CLC   =C'ZZZ S',0(R1)                                                  
         BNE   GETN01B                                                          
         CLC   =C'NHT  ',1(R2)                                                  
         BE    GETNX                                                            
         CLC   =C'HPM  ',1(R2)                                                  
         BE    GETNX                                                            
*                                                                               
GETN01B  CLC   =C'ZZZ H',0(R1)                                                  
         BNE   GETN01C                                                          
         CLC   =C'NHC  ',1(R2)                                                  
         BE    GETNX                                                            
         CLC   =C'HPC  ',1(R2)                                                  
         BE    GETNX                                                            
*                                                                               
GETN01C  CLC   =C'ZZZ C',0(R1)                                                  
         BE    GETN60                                                           
         CLC   =C'NHI-T',1(R2)                                                  
         BE    GETN02                                                           
         CLC   =C'NAD',8(R2)       SEE IF NAD                                   
         BNE   GETN02                                                           
         CLC   =C'ZZZ S',0(R1)     SYNDICATORS FOR NAD ONLY                     
         BE    GETN40                                                           
         CLC   =C'ZZZ H',0(R1)     HISPANIC, FOR NAD ONLY                       
         BE    GETN40                                                           
*                                                                               
GETN02   LA    R4,KEYSAVE          NOW FIND SYNDICATORS                         
*!!!!    LA    R5,424              MAXIMUM ENTRIES                              
         LA    R5,423              MAXIMUM ENTRIES                              
*                                                                               
         USING PRKEY,R4                                                         
         XC    KEYSAVE,KEYSAVE     INIT                                         
         MVC   PRCODE(3),=C'PNN'                                                
         CLC   1(3,R2),=C'NHW'                                                  
         BE    *+10                                                             
         CLC   1(3,R2),=C'HPW'                                                  
         BNE   *+8                                                              
         MVI   PRCODE+1,C'W'                                                    
*                                                                               
GETN04   XC    PRKMKT(10),PRKMKT                                                
         MVI   PRSTAT+5,X'FF'      FORCE NEXT STATION                           
         MVC   KEY,KEYSAVE                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'NTIDIR',KEY,KEYSAVE                   
         CLI   8(R1),0                                                          
         BNE   GETNX                                                            
         CLC   KEYSAVE(3),KEY                                                   
         BNE   GETNX                                                            
         CLC   PRSTAT(5),=C'FXS S' ELIMINATE FOX SPORTS                         
         BE    GETN04                                                           
         CLC   PRSTAT(5),=C'NCA S' ELIMINATE NCA (MARCH MADNESS TAPE)           
         BE    GETN04                                                           
         CLC   PRSTAT+3(2),=C'XS'  ELIMINATE CROSS-PLATFORM                     
         BE    GETN04                                                           
         CLC   PRSTAT(5),=C'ITN S' ELIMINATE ITN                                
         BE    GETN04                                                           
         CLC   PRSTAT(5),=C'PBN S' ELIMINATE PBN                                
         BE    GETN04                                                           
         CLI   PDZZZTYP,C'H'                                                    
         BNE   GETN05                                                           
         CLC   =C'NHW',1(R2)                                                    
         BE    *+14                                                             
         CLC   =C'HPW',1(R2)                                                    
         BNE   GETN05                                                           
         BAS   RE,CKGMA            EXCLUDE GMA STATIONS                         
         BE    GETN04                                                           
*                                                                               
GETN05   CLC   =C'NHT-H',1(R2)                                                  
         BE    *+10                                                             
         CLC   =C'HPM-H',1(R2)                                                  
         BE    *+10                                                             
         CLC   =C'NHW-H',1(R2)                                                  
         BE    *+10                                                             
         CLC   =C'HPW-H',1(R2)                                                  
         BE    *+10                                                             
         CLC   =C'NHI-T',1(R2)                                                  
         BNE   GETN06                                                           
         CLI   PRSTAT+4,X'88'      LOWER CASE 'H' = HISP WEIGHTED               
         BE    *+8                                                              
         CLI   PRSTAT+4,C'H'                                                    
         BNE   GETN04                                                           
         B     GETN08                                                           
*                                                                               
GETN06   CLI   PRSTAT+4,C'D'       DAYLIES ARE OK FOR NETWORK                   
         BNE   *+12                                                             
         CLI   PODNETMB,C'T'                                                    
         BE    *+10                                                             
*                                                                               
         CLC   PRSTAT+4(1),PODNETMB                                             
         BNE   GETN04                                                           
         CLC   =C'WTBSC',PRSTAT    DELETE WTBSC BECAUSE WE                      
         BE    GETN04                HAVE TBS_C ALREADY                         
*                                                                               
* ONLY INCLUDE STATIONS ACTIVE FROM BEGINNING OF REQUEST                        
         MVC   DUB(5),PRSTAT                                                    
         XC    PRKMKT(10),PRKMKT                                                
         MVC   PRBOOK(2),PDSBOOK                                                
         ZIC   RE,PRBOOK           BACK UP 3 YEARS                              
         SHI   RE,3                                                             
         STC   RE,PRBOOK                                                        
         MVC   KEY,KEYSAVE                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'NTIDIR',KEY,KEYSAVE                   
         CLI   8(R1),0                                                          
         BNE   GETNX                                                            
         CLC   KEYSAVE(3),KEY                                                   
         BNE   GETNX                                                            
         CLC   DUB(5),PRSTAT                                                    
         BE    GETN08                                                           
         MVC   PRSTAT(5),DUB                                                    
         B     GETN04                                                           
*                                                                               
GETN08   CLC   =C'NTI  ',1(R2)     CHECK FOR PROGRAM READ                       
         BE    GETN08A                                                          
         CLC   =C'OOH  ',1(R2)     CHECK FOR PROGRAM READ                       
         BE    GETN08A                                                          
         CLC   =C'ACM  ',1(R2)                                                  
         BE    GETN08A                                                          
         CLC   =C'OOHC ',1(R2)                                                  
         BNE   GETN10                                                           
GETN08A  L     R6,ASPOOLD                                                       
         USING SPOOLD,R6                                                        
         XC    P(50),P                                                          
         MVC   P(3),=C'QNN'                                                     
         MVC   P+3(2),PRBOOK                                                    
         MVC   P+5(L'PRSTAT),PRSTAT                                             
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'NTIDIR',P,P+30                        
         LA    RE,P+30                                                          
         LA    RE,PRSTAT-PRKEY(RE)                                              
         CLC   P(10),P+30      ONLY NETS THAT HAVE PROGRAM DATA                 
         BNE   GETN04               GO INTO LIST                                
         DROP  R6                                                               
*                                                                               
GETN10   XC    DUB,DUB                                                          
         MVC   DUB(4),PRSTAT                                                    
         BAS   RE,LOADTAB                                                       
         BNZ   GETN04              DUPLICATE STATION DONT COUNT                 
         BCT   R5,GETN04                                                        
         B     GETALLXT                                                         
*                                                                               
*  THIS WILL BUILD THE NETWORK TABLE FOR PROGRAM READS                          
*  UNTIL I FIX THE ZZZ LOGIC TO STOP REREADING FOR EACH BOOK                    
*                                                                               
GETN20   ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NEBROD    TABLE OF BROADCAST STATIONS                  
         ICM   R5,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)         LENGTH OF TABLE ENTRY                        
         USING NEBRDNMD,R5                                                      
*                                                                               
         XC    DUB,DUB                                                          
GETN22   CLI   0(R5),X'FF'                                                      
         BE    GETALLXT                                                         
         TM    NEBRDFLG,NEBRDFLG_NOT_UNIQUE    EXCLUDE NON-UNIQUE STNS          
         BO    GETN23                                                           
         MVC   DUB(3),NEBRDNET                                                  
         CLC   DUB(3),=C'UPN'                                                   
         BNE   *+10                                                             
         MVC   DUB(3),=C'PAR'      DISPLAY 'PAR' TO MATCH THE NAD FILES         
         MVI   DUB+3,C' '                                                       
         BAS   RE,LOADTAB                                                       
GETN23   AR    R5,RF               DUPLICATE STATION DONT COUNT                 
         B     GETN22                                                           
         DROP  R5                                                               
*                                                                               
*  THIS WILL BUILD THE OPI SYN TABLE FOR PROGRAM READS                          
*  UNTIL I FIX THE ZZZ LOGIC TO STOP REREADING FOR EACH BOOK                    
GETOPIF  LA    R5,OPIFAST                                                       
         XC    DUB,DUB                                                          
*                                                                               
GETOPIF1 CLI   0(R5),X'FF'                                                      
         BE    GETALLXT                                                         
         MVC   DUB(4),0(R5)                                                     
         BAS   RE,LOADTAB                                                       
         LA    R5,4(R5)            DUPLICATE STATION DONT COUNT                 
         B     GETOPIF1                                                         
*                                                                               
OPIFAST  DC    C'2/T '                                                          
         DC    C'BV  '                                                          
         DC    C'CAP '                                                          
         DC    C'CTS '                                                          
         DC    C'H/T '                                                          
         DC    C'KIN '                                                          
         DC    C'MGM '                                                          
         DC    C'PRM '                                                          
         DC    C'T/B '                                                          
         DC    C'TRE '                                                          
         DC    C'WB  '                                                          
         DC    C'BUEN'                                                          
         DC    C'CAM '                                                          
         DC    C'CCO '                                                          
         DC    C'CFE '                                                          
         DC    C'CNBS'                                                          
         DC    C'COL '                                                          
         DC    C'MCA '                                                          
         DC    C'MDC '                                                          
         DC    C'PREM'                                                          
         DC    C'TCS '                                                          
         DC    C'TRIB'                                                          
         DC    C'WARN'                                                          
         DC    C'WWF '                                                          
         DC    X'FFFF'                                                          
*                                                                               
*  THIS WILL BUILD THE OPI NET TABLE FOR PROGRAM READS                          
*  UNTIL I FIX THE ZZZ LOGIC TO STOP REREADING FOR EACH BOOK                    
GETOPIN  LA    R5,OPINFAST                                                      
         XC    DUB,DUB                                                          
*                                                                               
GETOPIN1 CLI   0(R5),X'FF'                                                      
         BE    GETALLXT                                                         
         MVC   DUB(4),0(R5)                                                     
         BAS   RE,LOADTAB                                                       
         LA    R5,4(R5)            DUPLICATE STATION DONT COUNT                 
         B     GETOPIN1                                                         
*                                                                               
OPINFAST DC    C'ABC '                                                          
         DC    C'CBS '                                                          
         DC    C'FOX '                                                          
         DC    C'NBC '                                                          
         DC    C'UPN '                                                          
         DC    C'PAX '                                                          
         DC    C'WB  '                                                          
         DC    C'WBN '                                                          
         DC    X'FFFF'                                                          
*                                                                               
*-------------------------------------------                                    
* DO IT FOR ALL NAD STATIONS                                                    
*-------------------------------------------                                    
GETN40   LA    R4,KEYSAVE                                                       
         USING PMKEY,R4                                                         
         SR    R5,R5                                                            
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   PMCODE(3),=C'QNN'                NOT 'NNN'                       
         CLC   1(3,R2),=C'NHW'                                                  
         BE    GETN41                                                           
         CLC   1(3,R2),=C'HPW'                                                  
         BE    GETN41                                                           
         CLC   1(3,R2),=C'NAW'                                                  
         BNE   *+8                                                              
*                                                                               
GETN41   MVI   PMCODE+1,C'W'                                                    
         MVC   PMBOOK,PDSBOOK      START WITH THIS BOOK                         
*                                                                               
*ETN42   MVI   PMSTAT+4,X'FF'                                                   
GETN42   ZIC   RE,PMSTAT+4                                                      
         AHI   RE,1                                                             
         STC   RE,PMSTAT+4                                                      
*                                                                               
GETN44   XC    PMSTYP(8),PMSTYP                                                 
         MVC   KEY,KEYSAVE                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'NTIDIR',KEY,KEYSAVE                   
         CLI   8(R1),0                                                          
         BNE   GETNX                                                            
         CLC   KEYSAVE(3),KEY                                                   
         BNE   GETNX                                                            
         CLC   PMBOOK,PDEBOOK               END AFTER THIS BOOK                 
         BH    GETNX                                                            
         CLC   PMSTAT+3(2),=C'XS'  EXCLUDE CROSS-PLATFORM                       
         BE    GETN46                                                           
         CLI   PDZZZTYP,C'H'                                                    
         BNE   GETN44A                                                          
         CLC   =C'NHT',1(R2)                                                    
         BE    *+14                                                             
         CLC   =C'HPM',1(R2)                                                    
         BNE   GETN44A                                                          
         BAS   RE,CKGMA                                                         
         BE    GETN42              EXCLUDE GMA STATIONS FOR NHT                 
GETN44A  CLC   PMSTAT+4(1),PDZZZTYP         HAVE TO MATCH TO USE                
         BE    GETN46                                                           
*                                                                               
         CLI   PDZZZTYP,C'H'       LOOKING FOR REGULAR HISPANIC DATA            
         BNE   GETN45                                                           
         CLI   PMSTAT+4,X'88'      FOUND WEIGHTED HISPANIC STATION              
         BNE   GETN45                                                           
         MVI   PMSTAT+4,C'H'       GO READ FOR REGULAR HISPANIC STATION         
         B     GETN44                                                           
*                                                                               
GETN45   CLC   PMSTAT(3),=C'WB '                                                
         BNE   GETN42                                                           
         ZIC   R1,PMSTAT+4                                                      
         LA    R1,1(R1)                                                         
         STC   R1,PMSTAT+4                                                      
         B     GETN44                                                           
*                                                                               
GETN46   CLC   PMSTAT+3(1),PDZZZMED                                             
         BE    GETN48                                                           
         CLI   PMSTAT+3,C' '                                                    
         BNE   GETN42                                                           
*                                                                               
GETN48   XC    DUB,DUB                                                          
         MVC   DUB(5),PMSTAT                                                    
         BAS   RE,LOADTAB                                                       
         BNZ   GETN42                                                           
*                                                                               
         LA    R5,1(R5)                                                         
         CHI   R5,424                                                           
         BNL   GETNX                                                            
         B     GETN42                                                           
*                                                                               
GETNX    B     GETALLXT                                                         
         DROP  R4                                                               
*                                                                               
*-------------------------- ALL CABLE (ZZZ) --------------------------*         
*                                                                               
GETN60   CLC   =C'MXM ',1(R2)                                                   
         BNE   GETN61                                                           
         BRAS  RE,ALLMXMC          GET ALL MXM CABLE STATIONS                   
         B     GETALLXT                                                         
*                                                                               
GETN61   LA    R4,KEY              READ DIR TO GET CABLE STATIONS               
         USING PRKEY,R4                                                         
         XC    KEY,KEY             INIT                                         
         MVC   PRCODE(3),=C'PCN'   BYPASS HISPANICS                             
         CLC   =C'REN',1(R2)       SET KEY FOR RENTRAK                          
         BNE   *+10                                                             
         MVC   PRCODE(4),=C'PRN0'                                               
         CLC   1(3,R2),=C'NHC'     HISPANIC STATIONS START WITH 'H'             
         BE    *+14                                                             
         CLC   1(3,R2),=C'HPC'                                                  
         BNE   GETN62                                                           
         MVI   PRCODE+3,C'H'                                                    
*                                                                               
GETN62   MVI   PRSTAT+4,X'FF'      FORCE NEXT STATION                           
         XC    PRKMKT(10),PRKMKT                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'NTIDIR',KEYSAVE,KEY                   
         B     GETN66                                                           
*                                                                               
GETN64   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'NTIDIR',KEYSAVE,KEY                   
*                                                                               
GETN66   CLI   8(R1),0                                                          
         BNE   GETN74                                                           
         CLC   KEY(3),KEYSAVE                                                   
         BNE   GETN74                                                           
*                                                                               
         CLC   1(3,R2),=C'NHC'     HISPANIC CABLE SOURCES                       
         BE    *+14                                                             
         CLC   1(3,R2),=C'HPC'                                                  
         BNE   GETN67                                                           
         CLI   PRSTAT,C'H'         END OF HISPANIC STATIONS                     
         BNE   GETN74              STOP                                         
         B     GETHC1              GO INTERPRET HISPANIC STATION                
*                                                                               
GETN67   CLI   PRSTAT,C'H'         NOT HISPANIC SOURCE                          
         BE    GETN62              SKIP HISPANIC STATIONS                       
*                                                                               
         CLI   PRSTAT,0            IF STATION IS 3-BYTE BINARY,                 
         BNE   GETN68                                                           
         SR    R5,R5                                                            
         ICM   R5,15,PRSTAT        JUST PICK UP THE BINARY VALUE                
         B     GETN68A                                                          
*                                                                               
GETN68   PACK  DUB,PRSTAT(4)       OTHERWISE STATION MUST BE ALPHA-NUM          
         CVB   R5,DUB                                                           
*                                                                               
GETN68A  XC    DUB,DUB             PREPARE DUB FOR FOR                          
         MVI   DUB+4,C'C'           STATION CALL LETTERS                        
* RENTRAK - MOVE IN C'Q' INSTEAD- RENTRAK STORED AS QTR RECORDS                 
         CLC   =C'REN',1(R2)                                                    
         BNE   *+12                                                             
         MVI   DUB+4,C'Q'                                                       
         B     GETRC1              RENTRAK HAS ITS OWN TABLES                   
*                                                                               
         ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NECABNAM  TABLE OF CABLE STATIONS                      
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)         LENGTH OF TABLE ENTRY                        
*                                                                               
         USING NECBNAMD,RE                                                      
GETN69   CLI   0(RE),X'FF'                                                      
         BE    GETN62              SKIP UNKNOWN STATIONS                        
         CLM   R5,7,NECBNNML       COMPARE ON 3-BYTE STATION CODE               
         BE    *+10                                                             
         AR    RE,RF               TRY NEXT ENTRY IN THE TABLE                  
         B     GETN69                                                           
         MVC   DUB(4),NECBNALP     TRANSFER ALPHABETIC STATION LTRS             
         B     GETN72                                                           
         DROP  RE                                                               
*                                                                               
* CONVERT HISPANIC CALL LETTERS                                                 
GETHC1   ICM   R5,7,PRSTAT+1       POINT TO COMPRESSED STATION (PWOS)           
*                                                                               
         ICM   RF,15,ACOMFACS      GET ADDRESS FROM DEMTABS                     
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NEHCBNAM  TABLE OF HISPANIC CABLE STATIONS             
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)         LENGTH OF TABLE ENTRY                        
*                                                                               
         USING NEHCNAMD,RE                                                      
GETHC2   CLI   0(RE),X'FF'                                                      
         BE    GETN62              SKIP UNKNOWN STATIONS                        
         CLM   R5,7,NEHCNNML       COMPARE ON 3-BYTE STTAION CODE               
         BE    *+10                                                             
         AR    RE,RF               TRY NEXT ENTRY IN THE TABLE                  
         B     GETHC2                                                           
         MVC   DUB(4),NEHCNALP     TRANSFER ALPHABETIC STATION LTRS             
         B     GETN72                                                           
         DROP  RE                                                               
*                                                                               
* CONVERT RENTRAK CALL LETTERS                                                  
GETRC1   ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,RECABNAM  TABLE OF RENTRAK CABLE STATIONS              
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)         LENGTH OF TABLE ENTRY                        
*                                                                               
         USING RECBNAMD,RE                                                      
GETRC2   CLI   0(RE),X'FF'                                                      
         BE    GETN62              SKIP UNKNOWN STATIONS                        
         CLM   R5,3,RECBNNUM       COMPARE ON NUMERIC CODE                      
         BE    *+10                                                             
         AR    RE,RF               TRY NEXT ENTRY IN THE TABLE                  
         B     GETRC2                                                           
         MVC   DUB(4),RECBNALP     TRANSFER ALPHABETIC STATION LTRS             
         B     GETN72                                                           
         DROP  RE                                                               
*                                                                               
GETN72   BAS   RE,LOADTAB                                                       
         BNZ   GETN62              DUPLICATE STATIONS DONT COUNT                
         MVC   KEYSAVE,KEY                                                      
         B     GETN64                                                           
*                                                                               
GETN74   CLC   =C'NTI',1(R2)                                                    
         BE    GETN76                                                           
         CLC   =C'OOH ',1(R2)                                                   
         BE    GETN76                                                           
         CLC   =C'ACM',1(R2)                                                    
         BE    GETN76                                                           
         CLC   =C'OOHC',1(R2)                                                   
         BE    GETN76                                                           
         CLC   =C'CNA',1(R2)                                                    
         BNE   GETNAX                                                           
GETN76   CLI   PDZZZTYP,C'C'                                                    
         BNE   GETNAX                                                           
*        CLC   =C'CNA',1(R2)                                                    
*        BE    GETN78                                                           
         MVC   DUB(5),=C'ION T'    ADD ION TO LIST OF CABLE STATIONS            
         BAS   RE,LOADTAB                                                       
GETN78   MVC   DUB(5),=C'BOU T'    ADD BOU TO LIST OF CABLE STATIONS            
         BAS   RE,LOADTAB                                                       
         MVC   DUB(5),=C'MYS T'    ADD MYS TO LIST OF CABLE STATIONS            
         BAS   RE,LOADTAB                                                       
         MVC   DUB(5),=C'GRT T'    ADD GRT TO LIST OF CABLE STATIONS            
         BAS   RE,LOADTAB                                                       
         MVC   DUB(5),=C'ESC T'    ADD ESC TO LIST OF CABLE STATIONS            
         BAS   RE,LOADTAB                                                       
         MVC   DUB(5),=C'LAF T'    ADD LAF TO LIST OF CABLE STATIONS            
         BAS   RE,LOADTAB                                                       
         MVC   DUB(5),=C'HI  T'    ADD HI TO LIST OF CABLE STATIONS             
         BAS   RE,LOADTAB                                                       
         MVC   DUB(5),=C'MET T'    ADD MET TO LIST OF CABLE STATIONS            
         BAS   RE,LOADTAB                                                       
         MVC   DUB(5),=C'STV T'    ADD STV TO LIST OF CABLE STATIONS            
         BAS   RE,LOADTAB                                                       
*                                                                               
GETNAX   B     GETALLXT                                                         
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*--LOAD STATION IN TABLE CHECK FOR DUPLICATES                                   
LOADTAB  NTR1                                                                   
LOAD02   CLI   0(R3),X'FF'                                                      
         BE    LOAD04                                                           
         CLC   DUB(6),0(R3)                                                     
         BE    LOADX                                                            
*        LA    R3,8(R3)                                                         
         LA    R3,PODNETL(R3)                                                   
         B     LOAD02                                                           
*                                                                               
LOAD04   MVC   0(6,R3),DUB         STATION MARKET NUMBER                        
         MVI   PODNETL(R3),X'FF'                                                
         SR    R3,R3                                                            
*                                                                               
LOADX    LTR   R3,R3                                                            
XITGA    XIT1                                                                   
         EJECT                                                                  
*                                                                               
*--DETERMINE IF A HISPANIC STATION IS GMA                                       
CKGMA    NTR1                                                                   
*                                                                               
         ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NEHGES    TABLE OF GEN MARKET STATIONS                 
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)         LENGTH OF TABLE ENTRY                        
*                                                                               
         USING NEHGENMD,RE                                                      
CKGMA5   CLI   0(RE),X'FF'                                                      
         BE    CKGMANO                                                          
         MVC   FULL(3),NEHGENET    STATION CALL LETTERS                         
         MVI   FULL+3,C' '         PADDED WITH SPACES                           
*                                                                               
         CLC   FULL(3),=C'ION'                                                  
         BNE   *+10                                                             
         MVC   FULL(3),=C'PAX'     KEY HAS OLD CALL LETTERS 'PAX'               
*                                                                               
         CLI   0(R4),C'Q'             NHT                                       
         BNE   CKGMA10                                                          
         USING PMKEY,R4                                                         
         CLC   PMSTAT(4),FULL                                                   
         B     CKGMA20                                                          
         DROP  R4                                                               
*                                                                               
CKGMA10  CLI   0(R4),C'P'             NHW                                       
         BNE   CKGMANO                                                          
         USING PRKEY,R4                                                         
         CLC   PRSTAT(4),FULL                                                   
         B     CKGMA20                                                          
         DROP  R4                                                               
         DROP  RE                                                               
*                                                                               
CKGMA20  BE    CKGMAYES                                                         
         AR    RE,RF               NEXT ENTRY IN THE STATION TABLE              
         B     CKGMA5                                                           
CKGMAYES CR    RB,RB                                                            
         B     CKGMAX                                                           
CKGMANO  CHI   RB,0                                                             
CKGMAX   XIT1                                                                   
         EJECT                                                                  
*                                                                               
*--GET ALL CABLE STATIONS FOR MINUTE BY MINUTE                                  
ALLMXMC  NTR1                                                                   
*                                                                               
         L     R2,APODBKL          LOOP THROUGH ALL THE REQUESTED BOOKS         
         MVC   FULL,PODBBKS-PODBD(R2)                                           
*                                                                               
ALLMC05  LA    R4,KEY              READ DIR TO GET CABLE STATIONS               
         XC    KEY,KEY                                                          
         USING MXMKEY,R4                                                        
         XC    KEY,KEY             INIT                                         
         MVI   MXMCODE,MXMCODEQ                                                 
         MVI   MXMMEDIA,C'C'       CABLE                                        
         MVI   MXMSRC,C'N'         LIVE                                         
         MVC   MXMBOOK,FULL                                                     
*                                                                               
ALLMC10  MVI   MXMSTAT+4,X'FF'     FORCE NEXT STATION                           
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'NTIDIR',KEYSAVE,KEY                   
*                                                                               
ALLMC30  CLI   8(R1),0                                                          
         BNE   ALLMC100                                                         
         CLC   KEY(MXMSTAT-MXMKEY),KEYSAVE                                      
         BNE   ALLMC100                                                         
*                                                                               
         CLI   MXMSTAT,0           IF FIRST BYTE IS NOT A DIGIT,                
         BNE   ALLMC32             STATION MUST BE BINARY                       
         ICM   R5,15,MXMSTAT                                                    
         B     ALLMC35                                                          
*                                                                               
ALLMC32  PACK  DUB,MXMSTAT(4)                                                   
         CVB   R5,DUB                                                           
*                                                                               
ALLMC35  XC    DUB,DUB             PREPARE DUB FOR                              
         MVI   DUB+4,C'C'          STATION CALL LETTERS                         
*                                                                               
         ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NECABNAM  TABLE OF CABLE STATIONS                      
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)         LENGTH OF TABLE ENTRY                        
*                                                                               
         USING NECBNAMD,RE                                                      
ALLMC40  CLI   0(RE),X'FF'                                                      
         BE    ALLMC10             SKIP UNKNOWN STATIONS                        
         CLM   R5,7,NECBNNML       COMPARE ON NUMERIC CODE                      
         BE    *+10                                                             
         AR    RE,RF               TRY NEXT ENTRY IN THE TABLE                  
         B     ALLMC40                                                          
         MVC   DUB(4),NECBNALP     TRANSFER ALPHABETIC STATION LTRS             
         B     ALLMC60                                                          
         DROP  RE                                                               
*                                                                               
ALLMC60  BAS   RE,LOADTAB                                                       
         B     ALLMC10                                                          
*                                                                               
ALLMC100 CLI   PODBRNG-PODBD(R2),0     TRY NEXT BOOK                            
         BE    ALLMC110                                                         
*                                                                               
         ZIC   RF,FULL+1           INCREMENT BOOK                               
         LA    RF,1(RF)                                                         
         STC   RF,FULL+1                                                        
         GOTO1 NETUNBK,DMCB,(C'W',FULL),DUB,GETDAY,ADDAY,GETBROAD               
         CLI   DUB,X'FF'           CHECK FOR YEAR BREAK                         
         BNE   ALLMC102                                                         
         ZIC   RF,FULL                                                          
         LA    RF,1(RF)                                                         
         STC   RF,FULL                                                          
         MVI   FULL+1,X'01'                                                     
ALLMC102 CLC   FULL,PODBLNQ+PODBBKS-PODBD(R2)    CK AGAINST END BOOK            
         BNH   ALLMC05                                                          
         LA    R2,PODBLNQ(R2)      BUMP LAST START BOOK                         
*                                                                               
ALLMC110 LA    R2,PODBLNQ(R2)      BUMP BOOK TABLE                              
         CLI   0(R2),X'FF'                                                      
         BE    ALLMCX              DONE                                         
         MVC   FULL,PODBBKS-PODBD(R2)    NEXT BOOK TO PROCESS                   
         B     ALLMC05                                                          
*                                                                               
ALLMCX   B     XITGA                                                            
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
INITIAL2 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R4                                                        
         CLI   PDOVSYS,2           SPOT SYSTEM?                                 
         BNE   *+14                                                             
         MVC   RCPROG(2),=C'SP'    PREFIX FOR REPORT                            
         B     RCDONE                                                           
         CLI   PDOVSYS,3           NET SYSTEM?                                  
         BNE   *+14                                                             
         MVC   RCPROG(2),=C'NE'    PREFIX FOR REPORT                            
         B     RCDONE                                                           
         CLI   PDOVSYS,8           REP SYSTEM?                                  
         BNE   RCDONE                                                           
         MVC   RCPROG(2),=C'RE'    PREFIX FOR REPORT                            
*                                                                               
RCDONE   CLI   PDOVSYS,3           NET SYSTEM?                                  
         BE    RCDONEX               DON'T HIDE                                 
         CLI   ACTNUM,ACTLIST      CAN'T MESS UP LIST SCREEN                    
         BE    RCDONEX                                                          
         MVC   SPLNETH-8(8),=C'Stations'                                        
         OI    SPLNETH-10,X'80'    TRANSMIT                                     
         OI    SPLPSTH-30,X'2C'    PROTECT & HIDE                               
         OI    SPLPSTH-25,X'80'    TRANSMIT                                     
         OI    SPLPSTH+1,X'20'     PROTECT IT                                   
         OI    SPLPSTH+6,X'80'     TRANSMIT IT                                  
         OI    SPLPENH-10,X'2C'                                                 
         OI    SPLPENH-5,X'80'                                                  
         OI    SPLPENH+1,X'20'        "                                         
         OI    SPLPENH+6,X'80'        "                                         
RCDONEX  XIT1  ,                                                                
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF SYSCODES FOR FUSION AND NSI-WIRED                              
***********************************************************************         
*                                                                               
SYSCLIST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING WORKD,R6                                                         
         CLC   =C'FUS',PDSOURCE    FUSION AND NSI-WIRED                         
         BE    SYSCL05                                                          
         CLC   =C'NSI',PDSOURCE                                                 
         BE    SYSCL03                                                          
         CLC   =C'TP',PDSOURCE                                                  
         BE    SYSCL03                                                          
         CLC   =C'T4',PDSOURCE                                                  
         BE    SYSCL03                                                          
         B     SYSCLX                                                           
SYSCL03  CLI   DBBTYPE,C'W'                                                     
         BNE   SYSCLX                                                           
*                                                                               
SYSCL05  TM    PDFLAG1,PDF1SYSC    AND SYSCODE KEYWORD REQUESTED                
         BNO   SYSCLX                                                           
         TM    PDFLAG1,PDF1SCPV    SYSCODE LIST PROVIDED BY USER                
         BO    SYSCLX                                                           
*                                                                               
         CLC   DBSELBK,PDSYSBK     CK IF SYSCODE FOR BOOK/MARKET                
         BNE   SYSCL10             ARE ALREADY RETREIVED                        
         CLC   DBSELMK,PDSYSMK                                                  
         BNE   SYSCL10                                                          
         B     SYSCLX                                                           
*                                                                               
SYSCL10  MVC   PDSYSBK,DBSELBK     LATEST BOOK/MARKET                           
         MVC   PDSYSMK,DBSELMK                                                  
*                                                                               
         L     RE,APDSYSC                                                       
         LHI   RF,PDSYSCDL                                                      
         XCEF                                                                   
         L     RE,APDSYSC          FIRST SYSCODE IS 0, FOLLOWED BY LIST         
         MVC   L'DSYSYSCD(2,RE),=X'FFFF'                                        
*                                                                               
         LA    RE,DBLOCK           SAVE DBLOCK                                  
         LA    RF,DBLOCKL                                                       
         LA    R0,SVDBLK                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   DBFUNCT,DBGETSYS    GET SYSCODES                                 
         MVI   DBSELSRC,C'F'       FROM FUSION FILE                             
         GOTO1 DEMAND,DMCB,DBLOCK,SYSCHOOK                                      
*                                                                               
         LA    RE,DBLOCK           RESTORE DBLOCK VALUES                        
         LA    RF,DBLOCKL                                                       
         LA    R0,SVDBLK                                                        
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
SYSCLX   J     EXITX                                                            
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* SYSCODE HOOK ROUTINE                                                          
***********************************************************************         
*                                                                               
SYSCHOOK NTR1  BASE=*,LABEL=*                                                   
         USING WORKD,R6                                                         
         L     R3,DBAREC                                                        
         USING DSYKEY,R3                                                        
*                                                                               
         LA    R2,DSYFRST-DSYKEY(R3)                                            
SYSCHK2  CLI   0(R2),0             FUSION MARKET ELEMENT                        
         BE    SYSCHK4             NO MARKET NUMBER. LET IT GO THRU             
         CLI   0(R2),FANCODEQ                                                   
         BE    SYSCHK3                                                          
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     SYSCHK2                                                          
         USING FANELEM,R2                                                       
SYSCHK3  CLC   DBSELMK,FANMKT#                                                  
         JNE   EXITX                                                            
*                                                                               
SYSCHK4  LA    R2,DSYFRST-DSYKEY(R3)                                            
SYSCHK5  CLI   0(R2),0             FUSION SUBSCRIBER BASE ELEMENT               
         BE    SYSCHK7                                                          
         CLI   0(R2),FSCCODEQ                                                   
         BE    SYSCHK6                                                          
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     SYSCHK5                                                          
         USING FSCELEM,R2                                                       
SYSCHK6  CLC   FSCSBASE,FSCUNIV    IF SUBSCRIBER BASE > UNIV                    
         JH    EXITX               THEN SUPRESS IT                              
                                                                                
SYSCHK7  L     RF,APDSYSC                                                       
SYSCHK8  CLC   =X'FFFF',0(RF)                                                   
         BE    SYSCHK10                                                         
         LA    RF,L'DSYSYSCD(RF)                                                
         B     SYSCHK8                                                          
*                                                                               
SYSCHK10 MVC   0(L'DSYSYSCD,RF),DSYSYSCD                                        
         LA    RF,L'DSYSYSCD(RF)                                                
         MVC   0(2,RF),=X'FFFF'                                                 
         J     EXITX                                                            
         DROP  R6,R2,R3                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD OR UPDATE 'NLIV' EXTENT FOR DEMAND                                        
***********************************************************************         
NLIVXT   NTR1  BASE=*,LABEL=*                                                   
         USING WORKD,R6                                                         
         ICM   R4,15,AVIEWTYP      ANY REQUESTED VIEWING TYPES?                 
         BNZ   *+14                YES                                          
         CLC   =C'OOH',1(R2)                                                    
         BNE   NLIVXTX                                                          
*                                                                               
         ICM   RE,15,DBEXTEND                                                   
         BZ    NLIV15              GO ADD IT                                    
         USING DBXLIVD,RE                                                       
         LA    RF,NLIVEXT                                                       
NLIV05   CLC   DBXLIVID,NLIVEXT                                                 
         BE    NLIV20              GO CHANGE IT                                 
         ICM   RE,15,DBXLIVNX                                                   
         BZ    NLIV15              GO ADD IT                                    
         B     NLIV05                                                           
*                                                                               
NLIV15   ICM   RF,15,DBEXTEND      ADD NEW EXTENT TO BEGINNING OF LIST          
         LA    RE,NLIVEXT                                                       
         STCM  RE,15,DBEXTEND                                                   
         STCM  RF,15,4(RE)                                                      
*                                                                               
NLIV20   MVC   DBXLIVE,0(R4)       UPDATE THE EXTENT DATA                       
*                                                                               
         CLI   DBXLIVE,0           DEFAULT TO LIVE (OL OR OCL) FOR OOH          
         JNE   NLIVXTX                                                          
         CLC   =C'OOH ',1(R2)                                                   
         JNE   *+8                                                              
         MVI   DBXLIVE,SRCLOLQ                                                  
         CLC   =C'OOHC',1(R2)                                                   
         JNE   *+8                                                              
         MVI   DBXLIVE,SRCLOCLQ                                                 
*                                                                               
NLIVXTX  J     EXITX                                                            
         DROP  RE,R6                                                            
*                                  10 MAX DAY/TIMES - SEE PODDAYTM              
NLIVEXT  DC    CL4'NLIV'                                                        
         DC    XL5'00'             4(DBXLIVNX) + 1(DBXLIVE)                     
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD OR UPDATE 'CAVG'(COMMERCIAL AVERAGE) EXTENT FOR DEMAND                    
***********************************************************************         
CAVEXT   NTR1  BASE=*,LABEL=*                                                   
         USING WORKD,R6                                                         
         CLC   =C'ACM',PODBEXT                                                  
         BE    *+14                                                             
         CLC   =C'OOHC',PODBEXT                                                 
         BNE   CAVX30              REMOVE EXTEND IF NOT SOURCE ACM              
*                                                                               
         ICM   RE,15,DBEXTEND      ADD 'CAVG' EXTEND                            
         BZ    CAVX15              GO ADD IT                                    
         USING DBXCAVD,RE                                                       
         LA    RF,CAVGEXT                                                       
CAVX05   CLC   DBXCAVID,CAVGEXT                                                 
         BE    CAVX20              GO CHANGE IT                                 
         ICM   RE,15,DBXCAVNX                                                   
         BNZ   CAVX05                                                           
CAVX15   ICM   RF,15,DBEXTEND      ADD NEW EXTENT TO BEGINNING OF LIST          
         LA    RE,CAVGEXT                                                       
         STCM  RE,15,DBEXTEND                                                   
         STCM  RF,15,4(RE)                                                      
CAVX20   MVI   DBXCAV,DBXCVYQ                                                   
         B     CAVXXTX                                                          
         DROP  RE                                                               
*                                                                               
CAVX30   ICM   RE,15,DBEXTEND     REMOVE COMMERCIAL AVERAGE FLAG                
         BZ    CAVXXTX                                                          
         USING DBXCAVD,RE                                                       
CAVX35   CLC   DBXCAVID,CAVGEXT                                                 
         BE    CAVX40                                                           
         ICM   RE,15,DBXCAVNX                                                   
         BNZ   CAVX35                                                           
         B     CAVXXTX                                                          
CAVX40   MVI   DBXCAV,DBXCVNQ                                                   
*                                                                               
CAVXXTX  J     EXITX                                                            
         DROP  RE,R6                                                            
*                                                                               
CAVGEXT  DC    CL4'CAVG'                                                        
         DC    XL5'00'             4(DBXCAVNX) + 1(DBXCAV)                      
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK FOR HISPANIC CABLE STATION                                              
***********************************************************************         
CKHCAB   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING WORKD,R6                                                         
*                                                                               
         ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NEHCBCLL  TABLE OF HISPANIC CABLE STATIONS             
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)         LENGTH OF TABLE ENTRY                        
*                                                                               
         USING NEHCBCLD,RE                                                      
CKHCB10  CLI   0(RE),X'FF'         STATION FOUND IN TABLE?                      
         BE    CKHCABN             NO. EXIT.                                    
         CLC   NEHCBALP,0(R3)      COMPARE ON STATION ALPHA                     
         BE    CKHCABY                                                          
         AR    RE,RF               TRY NEXT ENTRY IN THE TABLE                  
         B     CKHCB10                                                          
         DROP  RE                                                               
*                                                                               
CKHCABY  J     EXITXY                                                           
CKHCABN  J     EXITXN                                                           
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET HOMES IMPRESSIONS FOR NET                                                 
***********************************************************************         
NETHOMES NTR1  BASE=*,LABEL=*                                                   
* USE THIS ROUTINE WHEN WE WANT TO ACTIVATE THE NEW AVERAGING LOGIC             
* EXCLUDE ZERO ENTRIES FOR P1,Z1,S1 IF YHOMES NOT ZERO                          
* THIS LOGIC DOESN'T WORK WITH STACKED DEMOS                                    
* IT ONLY APPLIES TO METHOD=SOURCE AND METHOD=AE                                
         USING WORKD,R6                                                         
         CLC   DBFILE,=C'NTI'      NETWORK FILES                                
         BE    NETH10                                                           
         CLC   DBFILE,=C'RLD'      NETWORK FILES                                
         BE    NETH10                                                           
         CLC   DBFILE,=C'NAD'                                                   
         BE    NETH10                                                           
         B     NETHX                                                            
*                                                                               
NETH10   MVC   BYTE,DBDEMTYP                                                    
         MVI   DBDEMTYP,0                                                       
         GOTO1 DEMOUT,DMCB,(C'L',NETIMPS),DBLOCK,PDHOMES                        
         MVC   DBDEMTYP,BYTE                                                    
*                                                                               
NETHX    J     EXITX                                                            
         LTORG                                                                  
NETIMPS  DS    0CL3                                                             
         DC    X'00',C'Y',X'01'    YHOMES                                       
         DC    X'00',C'I',X'01'    IHOMES                                       
         DC    X'00',C'T',X'01'    THOMES                                       
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION OF SOME TABLES                                                 
***********************************************************************         
INITPROC NTR1  BASE=*,LABEL=*                                                   
         USING WORKD,R6                                                         
*                                                                               
         L     RE,AZZZLIST         SET ZZZ AREA TO UNUSED                       
         MVI   0(RE),0                                                          
         LA    RF,3400                                                          
         XCEF                                                                   
*                                                                               
         L     RE,AZZZPIV          SET ZZZ AREA TO UNUSED                       
         MVI   0(RE),0                                                          
         LA    RF,3400                                                          
         XCEF                                                                   
*                                                                               
         L     RE,APODMKTL         SET POINTER AT BEGINNING OF LIST             
         STCM  RE,15,PODMPTR       SAVE THE ADDRESS                             
*                                                                               
         L     RE,APODPRGL         SET A(PROGRAM GROUP LIST)                    
         STCM  RE,15,PRGLPTR                                                    
*                                                                               
         MVI   SVFLAG,0                                                         
*                                                                               
         OC    PDMGRP,PDMGRP       DO WE HAVE A MARKET GROUP?                   
         JZ    IPROCX              NO, CONTINUE                                 
         MVC   PDQMGR(5),PDMGRP                                                 
         XC    PDSVMGKY,PDSVMGKY                                                
         XC    PDSIDMKT,PDSIDMKT                                                
         LA    R2,PDQMGR                                                        
*                                                                               
IPROC00  GOTOR SUBR03,DMCB,('NEXTME',(RC)),(R2)                                 
         XC    DBSELSTA,DBSELSTA                                                
         MVC   SVDAYTME,DBSELDAY                                                
         OC    PDSIDMKT,PDSIDMKT                                                
         JZ    IPROCX                                                           
         GOTOR SUBR02,DMCB,('RDRMKTE',(RC))                                     
         MVC   DBSELDAY(5),SVDAYTME                                             
         J     IPROC00                                                          
*                                                                               
IPROCX   J     XIT                                                              
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
MULSBK2  NTR1  BASE=*,LABEL=*                                                   
         USING WORKD,R6                                                         
*                                                                               
         GOTOR SUBR01,DMCB,('GETDNAME',(RC))                                    
         XC    DBLOCK(256),DBLOCK                                               
         CLI   DMAOPT,C'A'                                                      
         BL    *+10                                                             
         MVC   PROF1W+5(1),DMAOPT                                               
         CLI   PROF1W+5,C'I'                                                    
         BNE   *+8                                                              
         MVI   DBTAPEP,C'Y'                                                     
         MVC   DBSELWKN,PDWKSOPT                                                
*                                                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBAREC,AIO                                                       
*                                                                               
         BRAS  RE,NLIVXT           UPDATE NLIV EXTENT                           
         BRAS  RE,CAVEXT           UPDATE CAV EXTENT                            
*                                                                               
         CLC   =C'MXM  ',1(R2)                                                  
         BNE   MULS01                                                           
         MVI   DBFUNCT,DBGETRLD                                                 
         B     MULS04                                                           
*                                                                               
MULS01   MVI   DBFUNCT,DBGETNTI                                                 
         LAY   RF,DBFUNTAB                                                      
MULS02   CLC   1(5,R2),0(RF)                                                    
         BE    MULS04                                                           
         LA    RF,5(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   MULS02                                                           
         MVI   DBFUNCT,DBGETDEM                                                 
         CLI   PDWKOPT,0           WANT EVERYTHING IF FILTERING                 
         BE    MULS04              REALLY ONLY FOR TP FILES                     
         MVI   DBSELWKN,X'FF'                                                   
*                                  MOVE DAYS AND TIME INTO BLOCK                
MULS04   GOTOR SUBR02,DMCB,('SETDBDTE',(RC))                                    
*                                                                               
*--SET DBSELSTA CHECK NAD SOURCE FOR DBSELSTA+3 INPUT                           
         MVC   DBBTYPE,5(R3)                                                    
*                                                                               
* INACTIVE AGENCY CLEANUP JAN/06. ID MAY BE REUSED IN THE FUTURE.               
*        CLC   AGENCY,=C'UM'       DEAL WITH UPN                                
*        BNE   *+10                                                             
*        CLC   0(4,R3),=C'UPN '                                                 
*        BNE   *+10                                                             
*        MVC   0(4,R3),=C'PAR '                                                 
*                                                                               
         CLC   0(4,R3),=C'PAX '                                                 
         BNE   *+10                                                             
         MVC   0(4,R3),=C'ION '                                                 
                                                                                
         MVC   PDNET(8),0(R3)                                                   
         MVC   PDSOURCE(5),1(R2)                                                
         MVC   DBSELSTA,0(R3)                                                   
         CLI   PDSIDOPT,C'Y'       NO SPILL FOR SIDS                            
         BE    *+16                                                             
         MVC   DBSELMK,6(R3)         USE SPILL MARKET                           
         MVC   PDSPILL,6(R3)                                                    
*                                                                               
         OC    PODNETAD,PODNETAD   ALL STATION TABLE                            
         BZ    MULS12                                                           
         MVC   PDMEDIA(2),PODNETMB                                              
*                                                                               
         CLC   =C'SIN S',DBSELSTA   ALWAYS RETURN SIN                           
         BE    MULS05                                                           
*                                                                               
         CLI   PODNETMB,C'C'       ION-T IS PART OF ZZZ-C,                      
         BNE   MULS10                                                           
         CLC   =C'ION T',DBSELSTA   BUT IS A BROADCAST STATION                  
         BE    MULS05                                                           
         CLC   =C'BOU T',DBSELSTA   BUT IS A BROADCAST STATION                  
         BE    MULS05                                                           
         CLC   =C'GRT T',DBSELSTA   BUT IS A BROADCAST STATION                  
         BE    MULS05                                                           
         CLC   =C'ESC T',DBSELSTA   BUT IS A BROADCAST STATION                  
         BE    MULS05                                                           
         CLC   =C'LAF T',DBSELSTA   BUT IS A BROADCAST STATION                  
         BE    MULS05                                                           
         CLC   =C'HI  T',DBSELSTA   BUT IS A BROADCAST STATION                  
         BE    MULS05                                                           
         CLC   =C'MET T',DBSELSTA   BUT IS A BROADCAST STATION                  
         BE    MULS05                                                           
         CLC   =C'STV T',DBSELSTA   BUT IS A BROADCAST STATION                  
         BE    MULS05                                                           
         CLC   =C'MYS T',DBSELSTA   BUT IS A BROADCAST STATION                  
         BNE   MULS10                                                           
*                                                                               
MULS05   MVI   DBBTYPE,C'A'         AND HAS BOOKTYPE 'A'                        
         OC    PODNETAD,PODNETAD   ALL STATION TABLE                            
         JZ    MULS12                                                           
         TM    SVFLAG,PODCNADQ+PODCNAWQ                                         
         JZ    *+8                                                              
         MVI   PDMEDIA,C'T'                                                     
         B     MULS12                                                           
*                                                                               
MULS06   MVI   DBBTYPE,C'A'         AND HAS BOOKTYPE 'A'                        
         B     MULS12                                                           
*                                                                               
MULS10   MVC   DBSELSTA+4(1),PODNETMB                                           
         MVC   DBBTYPE,PODNETMB+1                                               
*                                                                               
MULS12   CLC   =C'REN',1(R2)       ALWAYS LOOK UP Q FOR RENTRAK                 
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'Q'                                                  
*                                                                               
         CLI   6(R2),C'F'          FOR FUSION                                   
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
*                                                                               
         CLI   DBSELSTA+4,X'40'    LOAD STATION TYPE                            
         BH    *+10                                                             
         MVC   DBSELSTA+4(1),11(R2)                                             
         CLI   DBSELSTA+4,C'S'     NO INDIVIDUAL DAYS FOR SYND YET              
         BNE   *+8                                                              
         MVI   DBDAYOPT,0          SO RESET OPTION                              
*                                                                               
         CLI   DBSELSTA+4,C'C'     IS THIS CABLE?                               
         BE    MULS26              YES, SKIP THIS                               
         CLI   DBBTYPE,C'U'        NO, IS IT BOOKTYPE U?                        
         BE    MULS26              YES, SKIP FOR THAT REASON ALSO               
         LAY   RF,DBSTATYP                                                      
*                                                                               
MULS14   CLC   1(5,R2),0(RF)                                                    
         BE    MULS16                                                           
         LA    RF,7(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BE    MULS26                                                           
         B     MULS14                                                           
*                                                                               
MULS16   DS    0H                                                               
         CLC   DBSELSTA+3(2),=C' T' FORCE PROPER SUFFIX                         
         BNE   MULS17                                                           
         CLC   PDSOURCE(3),=C'NHW'                                              
         BE    MULS16A                                                          
         CLC   PDSOURCE(3),=C'HPW'                                              
         BE    MULS16A                                                          
         CLC   PDSOURCE(3),=C'NHT'                                              
         BE    MULS16A                                                          
         CLC   PDSOURCE(3),=C'HPM'                                              
         BNE   MULS17                                                           
MULS16A  MVC   DBSELSTA+3(2),=C' H'                                             
MULS17   DS    0H                                                               
*                                                                               
         CLI   DBSELSTA+3,C' '                                                  
         BNE   MULS18                                                           
         CLI   DBSELSTA+4,X'88'          IF ALL STA REQUEST,WEIGHTED            
         BE    *+8                       HISP X'88' ALREADY SET                 
         CLI   DBSELSTA+4,C'H'                                                  
         BNE   MULS18                                                           
*        CLC   DBSELSTA+3(2),=C' H'      HISPANIC?                              
*        BNE   MULS18                                                           
         CLC   PDSOURCE(5),=C'NHW  '                                            
         BE    *+10                                                             
         CLC   PDSOURCE(5),=C'HPW  '                                            
         BE    *+10                                                             
         CLC   PDSOURCE(5),=C'NAW  '                                            
         BE    *+10                                                             
         CLC   PDSOURCE(5),=C'NHT  '                                            
         BE    *+10                                                             
         CLC   PDSOURCE(5),=C'HPM  '                                            
         BE    *+10                                                             
         CLC   PDSOURCE(5),=C'NAD  '                                            
         BE    *+10                                                             
         MVC   DBSELSTA+3(1),5(RF)       FOR NAD-T & NAD-D                      
*        MVI   DBSELSTA+4,C'H'           HISPANIC                               
         B     MULS24                                                           
*                                                                               
MULS18   CLC   DBSELSTA(5),=C'TVQSS'     FIX CALL LETTEST AND                   
         BE    *+14                                                             
         CLC   DBSELSTA(5),=C'SNETS'                                            
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'M'                                                  
         CLC   DBSELSTA+3(2),=C' S'      IGNORE DAY/TIME FOR SYND               
         BNE   MULS20                                                           
         MVC   DBSELSTA+3(2),=C' M'                                             
         CLI   DBSELDAY,X'7F'                                                   
         BNE   *+8                                                              
         MVI   DBSELDAY,0                                                       
         CLC   DBSELTIM(2),=H'600'                                              
         BNE   MULS24                                                           
         CLC   DBSELTIM+2(2),=H'545'                                            
         BNE   MULS24                                                           
         XC    DBSELTIM(4),DBSELTIM                                             
         B     MULS24                                                           
*                                                                               
MULS20   CLI   DBSELSTA+3,C' '                                                  
         BE    MULS22                                                           
         CLC   DBSELSTA+3(1),4(RF)       HAS TO BE SAME                         
         BNE   MULS24                                                           
         CLC   DBSELSTA(3),=C'HUT'       NEED THIS TO FILTER OUT                
         BE    MULS24                      UNWANTED STATIONS                    
*                                                                               
MULS22   MVC   DBSELSTA+3(2),5(RF)                                              
*                                                                               
MULS24   CLI   DBBTYPE,C'O'        O TYPE VALID FOR NAD                         
         BE    *+8                                                              
         CLI   DBBTYPE,C'U'        U TYPE VALID FOR NAD                         
         BE    *+8                                                              
         CLI   DBBTYPE,C'E'        ALLOW WEIGHTED BTYPE                         
         BE    *+10                                                             
         XC    DBBTYPE,DBBTYPE     NO BOOK TYPE FOR NAD                         
MULS24A  CLI   DBSELSTA+3,C'P'                                                  
         BE    MULS26                                                           
         CLI   DBSELSTA+3,C' '                                                  
         BE    MULS26                                                           
         XC    DBSELDAY,DBSELDAY   FOR NAD "D" AND "T" TYPE                     
         XC    DBSELTIM,DBSELTIM   NO DAYS AND TIMES IN DBLOCK                  
*                                                                               
MULS26   MVC   DBSELSRC,6(R2)                                                   
         MVC   DBSELMED,7(R2)                                                   
         MVC   DBFILE(3),8(R2)                                                  
         CLC   =C'IUN',DBFILE                                                   
         BNE   *+10                                                             
         MVC   DBSTYPE,15(R2)                                                   
         MVC   PDSTYPE,15(R2)                                                   
         CLC   =C'WTP',DBFILE                                                   
         BNE   *+16                                                             
         CLI   DBBTYPE,C'P'                                                     
         BE    *+8                                                              
         MVI   DBBTYPE,0                                                        
         CLI   DBSELMED,C'C'                                                    
         BNE   *+8                                                              
         MVI   DBFRANCO,C'Y'                                                    
*                                                                               
         CLI   DBSELSRC,C'F'       TAPE OPTION FOR FUSION                       
         BE    *+8                                                              
         CLI   DBSELSRC,C'N'       TAPE OPTION FOR NSI/USTV                     
         BNE   MULS27                                                           
         CLI   DBSELMED,C'T'                                                    
         BE    *+8                                                              
         CLI   DBSELMED,C'O'       OVERNIGHTS TOO                               
         BE    *+8                                                              
MULS27   MVI   DBTAPEP,C'N'        RESET FOR ALL OTHERS                         
*                                                                               
         MVC   DBSELSPO,PDSPTOPT                                                
         MVC   PDFILE(3),8(R2)                                                  
         CLI   DBBTYPE,0                                                        
         BNE   *+10                                                             
         MVC   DBBTYPE,14(R2)                                                   
         MVC   PDBKTYP,DBBTYPE                                                  
         MVC   PDSQART,16(R2)                                                   
         MVC   DBSELBK,PDSBOOK                                                  
         CLC   =C'WTP',DBFILE                                                   
         BNE   *+16                                                             
         CLI   DBBTYPE,C'P'                                                     
         BE    *+8                                                              
         MVI   DBBTYPE,0                                                        
*                                                                               
         CLC   =C'BBM',PODBEXT                                                  
         BNE   MULS28                                                           
         CLI   DBBTYPE,C'W'        WEEKLY BOOK TYPE?                            
         BNE   MULS30              NO                                           
         MVI   DBBEST,C'M'         YES, ASSUME M/Y REQUEST                      
         CLI   SVBBMWK,C'Y'        WAS IS M/D/Y?                                
         BNE   *+8                 NO                                           
         MVI   DBBEST,C'N'         YES, DBBEST                                  
         B     MULS30                                                           
*                                                                               
MULS28   CLC   =C'SQAD',PODBEXT                                                 
         BNE   MULS30                                                           
         MVC   DBSELSQ,PDSQART                                                  
*                                                                               
MULS30   CLC   =C'NCOS',PDSOURCE                                                
         BNE   MULS30A                                                          
         MVI   DBSELDAY,0                                                       
         XC    DBSELTIM(4),DBSELTIM                                             
*                                                                               
MULS30A  CLC   =C'OPI',PDSOURCE                                                 
         BNE   MULS30C                                                          
         CLI   DBSELSTA+4,C'S'                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'M'     SYNDICATION OPI                              
         MVI   DBSELDAY,0                                                       
         XC    DBSELTIM(4),DBSELTIM                                             
         MVI   DBFUNCT,DBGETOPI                                                 
*                                                                               
         CLI   DBSELBK+1,12        OPI BOOKS ARE ON FILE AS YR/QTR#             
         BH    *+12                                                             
         MVI   DBSELBK+1,1         Q1=WEEKS 1-12                                
         B     MULS30C                                                          
         CLI   DBSELBK+1,25                                                     
         BH    *+12                                                             
         MVI   DBSELBK+1,2         Q2=WEEKS 13-25                               
         B     MULS30C                                                          
         CLI   DBSELBK+1,37                                                     
         BH    *+12                                                             
         MVI   DBSELBK+1,3         Q3=WEEKS 26-37                               
         B     MULS30C                                                          
         MVI   DBSELBK+1,4         Q4=WEEKS >= 38                               
*                                                                               
MULS30C  CLC   =C'IAG',PDSOURCE    IAG DATA                                     
         BNE   MULS2X                                                           
         MVI   DBSELSTA+4,C'I'                                                  
         MVI   DBBTYPE,C'U'                                                     
         MVI   DBSELDAY,0          NO DAY/TIME FILTERING                        
         XC    DBSELTIM(4),DBSELTIM                                             
         CLC   =C'IAGE',PDSOURCE                                                
         BNE   MULS2X                                                           
         MVI   DBFUNCT,DBGETOPI    ESTIMATED IAG USES DBGETOPI FUNCTION         
         GOTO1 DEFINE,DMCB,=C'UYEAR',DBLOCK,HALF                                
         EDIT  HALF,(4,DUB)        YEAR YYYY                                    
         MVC   DUB+4(4),=C'0101'   DUMMY MONTH AND DAY                          
         GOTO1 NETWEEK,DMCB,DUB+2,GETDAY,ADDAY    YYMMDD TO NET WEEK            
         MVC   DBSELBK(1),DMCB+4   YEAR NO.                                     
         MVI   DBSELBK+1,0         BOOK IS YY00                                 
MULS2X   J     EXITX                                                            
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*IAGDEMS - IF IAG MKT BREAK 177 IS REQUESTED, NEED TO AUTOMATICALLY             
*          DEFAULT TO 175 IF PROGRAM IS ORIGINAL, OR 176 IF PROGRAM             
*          IS REPEAT                                                            
*        - R2 POINTS TO REQUESTED DEMOS                                         
*                                                                               
IAGDEMS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
IAGDEM10 CLI   0(R2),X'FF'                                                      
         BE    IAGDEMSX                                                         
         OC    0(4,R2),0(R2)                                                    
         BZ    IAGDEMSX                                                         
         CLI   0(R2),177                                                        
         BNE   IAGDEM20                                                         
         CLI   PDFILT+3,C'O'       ORIGINAL                                     
         BNE   *+8                                                              
         MVI   0(R2),175           IS IN MARKET BREAK 175                       
         CLI   PDFILT+3,C'R'       REPEAT                                       
         BNE   *+8                                                              
         MVI   0(R2),176           IS IN MARKET BREAK 176                       
IAGDEM20 AH    R2,LNDEMCOD                                                      
         B     IAGDEM10                                                         
*                                                                               
IAGDEMSX J     EXITX                                                            
         LTORG                                                                  
MKWGT    NTR1  BASE=*,LABEL=*      ONLY COMING HERE FOR ACMWB (8.08)            
         USING WORKD,R6                                                         
         L     R2,PDADEMTB         R2 GETS DESTROYED AFTER THIS                 
         USING PDDMBUFF,R2         DO NOT RELY ON DSECT PAST NEXT LINE          
         LA    R2,PODDEMO                                                       
         LA    R3,PODDMWGT                                                      
MW10     CLI   PDGSTACK,X'FF'       GENDER STACK?                               
         BNE   MW20                                                             
         CLI   0(R2),1              GENDER STACK ONLY HAS MKT BREAK             
         BNE   MW20                 AT THE FIRST DEMO                           
         MVC   FULL(1),BYTE                                                     
         B     *+16                                                             
MW20     MVC   BYTE,0(R2)           SAVE IT                                     
         MVC   FULL(1),0(R2)        MKT BREAK                                   
         GOTO1 DEFINE,DMCB,=C'MBINF',DBLOCK,FULL                                
         MVC   2(2,R3),FULL                                                     
         LA    R3,4(R3)                                                         
         LA    R2,4(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   MW10                                                             
*                                                                               
MWX      J     EXITX                                                            
         LTORG                                                                  
         DROP  R2                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
EXITXY   CR    RB,RB                                                            
         J     EXITX                                                            
*                                                                               
EXITXN   CHI   RB,0                                                             
         J     EXITX                                                            
*                                                                               
EXITX    XIT1  ,                                                                
         EJECT                                                                  
         TITLE 'SUB-ROUTINES - PART 1'                                          
*                                                                               
*                                                                               
SUBR01   NMOD1 0,**SR01**,RA                                                    
         USING WORKD,R6                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         USING PODBD,PODBOOK                                                    
         L     R1,0(R1)                                                         
         SRL   R1,24                                                            
         SLL   R1,2                                                             
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
MRKTNAE  EQU   (MRKTNA#-*)/4+1                                                  
GETDNAME EQU   (GETDNAM#-*)/4+1                                                 
VRECE    EQU   (VREC#-*)/4+1                                                    
FIGMINSE EQU   (FIGMINS#-*)/4+1                                                 
GETMINSE EQU   (GETMINS#-*)/4+1                                                 
DAYPDAYE EQU   (DAYPDAY#-*)/4+1                                                 
SPDDAYE  EQU   (SPDDAY#-*)/4+1                                                  
HALFHUTE EQU   (HALFHUT#-*)/4+1                                                 
IMPDEME  EQU   (IMPDEM#-*)/4+1                                                  
GETVALE  EQU   (GETVAL#-*)/4+1                                                  
CABFORME EQU   (CABFORM#-*)/4+1                                                 
SETDYTE  EQU   (SETDYT#-*)/4+1                                                  
SWCHSPTE EQU   (SWCHSPT#-*)/4+1                                                 
SWCHBCKE EQU   (SWCHBCK#-*)/4+1                                                 
UPGRDE   EQU   (UPGRD#-*)/4+1                                                   
*                                                                               
MRKTNA#  B     MRKTNA              GET A MARKET NAME                            
GETDNAM# B     GETDNAM             GET DEMO NAMES                               
VREC#    B     VREC                VALIDATE A RECORD                            
FIGMINS# B     FIGMINS                                                          
GETMINS# B     GETMINS                                                          
DAYPDAY# B     DAYPDAY                                                          
SPDDAY#  B     SPDDAY                                                           
HALFHUT# B     HALFHUT                                                          
IMPDEM#  B     IMPDEM                                                           
GETVAL#  B     GETVAL                                                           
CABFORM# B     CABFORM                                                          
SETDYT#  B     SETDYT                                                           
SWCHSPT# B     SWTCHSPT                                                         
SWCHBCK# B     SWTCHBCK                                                         
UPGRD#   B     UPGRD                                                            
         DS    0H                                                               
*                                                                               
XITS1    XMOD1                                                                  
         EJECT                                                                  
SWTCHSPT DS    0C                                                               
         CLI   PDOVSYS,2                                                        
         BE    SWTCX                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   SWTC02                                                           
         ICM   R2,15,PDAUTL                                                     
         MVI   4(R2),X'32'         SPOT SYSTEM                                  
*                                                                               
         CLC   AGENCY,=C'UT'        <<<<<<<TEMP                                 
         BE    *+10                                                             
         CLC   AGENCY,=C'KH'        <<<<<<<TEMP                                 
         BNE   *+8                                                              
         MVI   4(R2),X'33'                                                      
*                                                                               
         MVC   SYSDIR,=CL8'SPTDIR'                                              
         MVC   SYSFIL,=CL8'SPTFILE'                                             
         B     SWTCX                                                            
*                                                                               
SWTC02   GOTO1 SWITCH,DMCB,=C'SPOT'                                             
*                                                                               
SWTCX    B     XITS1                                                            
         EJECT                                                                  
SWTCHBCK DS    0C                                                               
         CLI   PDOVSYS,2                                                        
         BE    SWTBX                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   SWTB02                                                           
         ICM   R2,15,PDAUTL                                                     
         MVC   4(1,R2),PDSYSTEM      SPOT SYSTEM                                
         CLI   PDOVSYS,8                                                        
         BNE   SWTBX                                                            
         MVC   SYSDIR,=CL8'REPDIR'                                              
         MVC   SYSFIL,=CL8'REPFILE'                                             
         B     SWTBX                                                            
*                                                                               
SWTB02   GOTO1 SWITCH,DMCB,(PDSYSTEM,0)                                         
         CLI   DMCB+4,0                                                         
         BZ    SWTBX                                                            
         DC    H'0'                                                             
*                                                                               
SWTBX    B     XITS1                                                            
         EJECT                                                                  
SETDYT   DS    0C                                                               
         GOTO1 DEFINE,DMCB,=C'DAY',,BLOCK                                       
         MVC   PDDAY(1),BLOCK+1    REP FORMAT                                   
         MVC   PDDAY+1(1),BLOCK    SPOT FORMAT                                  
         MVC   PDDAY+2(3),BLOCK+2  SPOT FORMAT                                  
         CLI   PDDAY+2,C'A'                                                     
         BNL   *+10                                                             
         MVC   PDDAY+2(3),=C'VAR'                                               
*                                                                               
         CLC   =C'NTI ',PDSOURCE   CONVERT DAY TO SPOT FORMAT 01-07             
         BE    SETD01                                                           
         CLC   =C'OOH ',PDSOURCE                                                
         BE    SETD01                                                           
         CLC   =C'ACM ',PDSOURCE                                                
         BE    SETD01                                                           
         CLC   =C'OOHC',PDSOURCE                                                
         BE    SETD01                                                           
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   SETD01X                                                          
SETD01   GOTOR SUBR01,DMCB,('SPDDAYE',(RC))                                     
SETD01X  DS    0X                                                               
*                                                                               
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,BLOCK                                
         MVC   PDQHDUR,BLOCK+6     GET QH DURATION                              
         MVC   PDTIME,BLOCK+2                                                   
*        CLC   PDTIME+2(2),=H'600'                                              
*        BNE   *+8                                                              
*        MVC   PDTIME+2(2),=H'559'                                              
         OC    PDTIME+2(2),PDTIME+2     IF END TIME IS ZERO                     
         BNZ   *+10                                                             
         MVC   PDTIME+2(2),=XL2'0960'   SET TO 2400                             
*                                                                               
         CLC   =C'NTI',DBFILE   ONLY TP HAS 15 MIN BREAKS                       
         BE    SETD02                                                           
         CLC   =C'TP',DBFILE    ONLY TP HAS 15 MIN BREAKS                       
         BNE   SETDX                                                            
         CLI   DBINTMED,C'D'       DAYPART FILE                                 
         BE    SETDX                                                            
         CLI   DBSELSRC,C'F'       FUSION FILE                                  
         BE    SETDX                                                            
         CLI   PDNUMQH,0           DEFAULT IS 2                                 
         BNZ   *+8                                                              
         MVI   PDNUMQH,2                                                        
*                                                                               
         PRINT GEN                                                              
         LA    RF,DBLOCK                                                        
DB       USING DBLOCK,RF                                                        
         ZIC   R1,DB.DBDQPTR                                                    
         BCTR  R1,0                                                             
         LA    R0,DBDQLEN                                                       
         MR    R0,R0                                                            
         LA    RE,DB.DBDQKDAY                                                   
         XC    HALF,HALF                                                        
*                                   DAY/QH ENTRIES LOCATED ELSEWHERE?           
         CLC   =AL2(DBDQUXTD),DB.DBDQTAB  INSTRUCTED TO USE XTND BLOCK?         
         BNE   GETDQ262                                                         
*                                                                               
         ST    R1,FULL                                                          
         ST    RF,SAVERF                                                        
         GOTOR SUBR02,DMCB,('GETEXTE',(RC)),DB.DBDQTAB+2                        
         L     R1,FULL                                                          
         L     RF,SAVERF                                                        
*                                                                               
         ICM   RE,15,DMCB                                                       
         USING DBDQXTND,RE                                                      
         MVC   HALF,DBDQADJF                                                    
         LA    RE,DBDQXFXL(RE)                                                  
         DROP  RE                                                               
GETDQ262 EQU   *                                                                
*                                                                               
         AR    RE,R1                                                            
         MVC   STTIME(2),1(RE)                                                  
         ZIC   R0,DB.DBACTSQC      DBACTSQC 0 BASED                             
         ZIC   R1,DB.DBACTSQH      DBACTSQH 1 BASED                             
         SH    R1,HALF             ADJUST TO RECORD QH                          
         BCTR  R1,0                                                             
         SR    R0,R1               FIND OFFSET FROM START                       
         BP    *+6                                                              
         SR    R1,R1                                                            
         LR    R1,R0                                                            
         SR    R0,R0                                                            
         ZIC   RE,PDNUMQH                                                       
         DR    R0,RE               DETERMINE START QH                           
         SR    R0,R0                                                            
         MR    R0,RE                                                            
         ZIC   R0,DB.DBACTSQH                                                   
         BCTR  R1,0                DBACTSQH 1 BASED                             
         AR    R1,R0                                                            
         STC   R1,ENTIME                                                        
         AR    R1,RE               SET END QH                                   
         ZIC   R0,STTIME+1                                                      
         CR    R1,R0               IF PAST END                                  
         BL    *+10                                                             
         LR    R1,R0               SET TO PRINT END OF END QH                   
         LA    R1,1(R1)                                                         
         STC   R1,ENTIME+1                                                      
         ZIC   R1,ENTIME           RESTORE START QH                             
         ZIC   R0,STTIME                                                        
         CR    R1,R0                                                            
         BH    *+6                                                              
         LR    R1,R0                                                            
         BAS   R2,SETD04           QH TO MILITARY                               
         STCM  R1,3,STTIME                                                      
         DROP  DB                                                               
*                                                                               
         ZIC   R1,ENTIME+1                                                      
         BAS   R2,SETD04           QH TO MILITARY                               
         STCM  R1,3,ENTIME                                                      
         MVC   PDTIME(4),STTIME    GRAB START AND END TIMES                     
*                                                                               
SETD02   CLI   PDNUMQH,0           CONDENSE TIME NTI                            
         BE    SETDX                                                            
         ZIC   R1,BLOCK                                                         
         ZIC   RF,PDNUMQH                                                       
         SR    R0,R0                                                            
         DR    R0,RF                                                            
         MR    R0,RF                                                            
         LR    RE,R1                                                            
         BAS   R2,SETD04                                                        
         STCM  R1,3,STTIME                                                      
         LR    R1,RE                                                            
         ZIC   RF,PDNUMQH                                                       
         AR    R1,RF                                                            
         BAS   R2,SETD04                                                        
         STCM  R1,3,ENTIME                                                      
         MVC   PDTIME(4),STTIME                                                 
         B     SETDX                                                            
         PRINT NOGEN                                                            
*                                                                               
SETD04   SR    R0,R0                                                            
         D     R0,=F'4'                                                         
         LR    RF,R0                                                            
         AHI   R1,6                                                             
         MHI   R1,100                                                           
         MHI   R0,15                                                            
         AR    R1,R0                                                            
         BR    R2                                                               
*                                                                               
SETDX    B     XITS1                                                            
         EJECT                                                                  
CABFORM  LA    R5,NETWEXT          GET NETWORK EXTENSION BLOCK                  
         XC    NETWEXT,NETWEXT                                                  
         USING DBXNTID,R5                                                       
         MVC   DBXNID,=C'NETW'                                                  
         GOTOR SUBR02,DMCB,('PUTEXTE',(RC)),(R5)                                
         MVI   DBXNFFBK,C'Y'                                                    
         MVC   DBX0EVPH,PROF1W+11  CONTROL ZERO VPH CALCS                       
         CLI   PRECOPT,C'Y'        ARE WE USING CABLE PRECISION                 
         BNE   CABFX                                                            
         MVI   DBXNCR2,C'Y'                                                     
         MVI   DBXNNR2,C'Y'                                                     
         MVI   DBXNHOPT,C'Y'                                                    
*                                  MAGNITUDE ADJUSTMENTS FOR PUTS               
         CLC   DBFILE,=C'NTI'      NET                                          
         BE    *+14                                                             
         CLC   DBFILE,=C'RLD'      NET                                          
         BNE   PUTA10                                                           
         CLI   DBSELSTA+4,C'T'     BROADCAST                                    
         BE    PUTA20                                                           
         CLI   DBSELSTA+4,C'H'     AND HISPANIC                                 
         BE    PUTA20                                                           
         BNE   PUTAX                                                            
PUTA10   CLC   DBFILE,=C'NAD'      NAD                                          
         BNE   PUTAX                                                            
         CLI   DBSELSTA+4,C'N'     NET NAD                                      
         BE    PUTA20                                                           
         CLI   DBSELSTA+4,C'H'     AND NHT                                      
         BE    PUTA20                                                           
         B     PUTAX                                                            
PUTA20   MVI   DBXNNP1,C'Y'        1 DECIMAL PUT                                
PUTAX    DS    0H                                                               
*                                                                               
CABFX    B     XITS1                                                            
         DROP  R5                                                               
         EJECT                                                                  
GETVAL   DS    0H                                                               
         ICM   RF,15,PDSDBFAD                                                   
         LR    RE,RF                                                            
         USING SRBLKD,RE                                                        
         LA    RF,24(RF)           POINT TO FIRST ELEMENT                       
         L     RF,SROVELEM                                                      
*                                                                               
GETV02   CLI   0(RF),0             TEST END OF OVRD ELEMS                       
         BE    GETVX                                                            
         CLI   0(RF),X'DE'         TEST OVRD ELEM                               
         BE    GETV06                                                           
*                                                                               
GETV04   ZIC   R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     GETV02                                                           
*                                                                               
GETV06   CLC   2(2,RF),1(R1)       MATCH DEMO MOD/NUM                           
         BNE   GETV04                                                           
*                                                                               
* NOW FIND FIELD IN SORTDEMS AND SET OVRD VALUE *                               
         L     R4,AUPDEMOS                                                      
         L     R5,ASORTDEM                                                      
*                                                                               
GETV08   CLI   0(R4),0             MAKE SURE NOT CPP, ETC.                      
         BNE   GETV10                                                           
         CLC   1(2,R4),2(RF)       MATCH MOD/NUM                                
         BNE   GETV10                                                           
         XC    0(4,R5),0(R5)       CLEAR OLD VALUE                              
         MVC   2(2,R5),4(RF)       SET NEW VALUE                                
         B     GETVX                                                            
*                                                                               
GETV10   AH    R4,LNDEMCOD                                                      
         LA    R5,4(R5)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   GETV08                                                           
*                                                                               
GETVX    B     XITS1                                                            
         EJECT                                                                  
IMPDEM   DS    0H                                                               
         L     R3,PDADEMTB                                                      
         USING PDDMBUFF,R3                                                      
*                                                                               
         L     R0,APDDEML1         MOVE DEMO LIST TO LIST 1                     
         LA    R1,L'PODDEMO                                                     
         LA    RE,PODDEMO                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,APDDEML2         MOVE DEMO LIST TO LIST 2                     
         LA    R1,L'PODDEMO                                                     
         LA    RE,PODDEMO                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,APDPLGL1         MOVE PERSONAL LANGUAGE ATTRIBUTES            
         MVC   0(L'PODDEMPL,RE),PODDEMPL      TO LIST 1                         
         L     RE,APDPLGL2                                                      
         MVC   0(L'PODDEMPL,RE),PODDEMPL      AND LIST 2                        
         DROP  R3                                                               
*                                                                               
         L     RE,APDDEML1         DEMO LIST 1                                  
         L     RF,APDDEML2         DEMO LIST 2                                  
         L     R3,APDPLGL2         PERSONAL LANGUAGE ATTRIBUTES LIST 2          
IMPD02   LA    R1,CONVTAB                                                       
         CLI   PDBASE,C'I'         IMP BASED CALCULATIONS                       
         BE    *+8                                                              
         CLI   PDBASE,C'B'         BOOK BASED CALCULATIONS                      
         BNE   *+8                                                              
         LA    R1,IMDNTIB                                                       
*                                                                               
         CLI   PDDUROPT,C'M'       FULL RECALC REQUIRED                         
         BNE   IMPD03                                                           
         LA    R1,IMDNTIC                                                       
         CLC   PDSOURCE(3),=C'REN' RENTRAK                                      
         BNE   *+8                                                              
         LA    R1,IMDREN                                                        
         CLI   PDMETHOD,6          METHOD=AE USES DIFF FORMULAS                 
         BNE   *+8                                                              
         LA    R1,IMDNTIAE                                                      
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   *+8                                                              
         LA    R1,IMDNTIAE         USE EXACT SHARE AND PUTS                     
         CLC   2(2,RE),=H'7'       SPECIAL FOR MIDAGE,OMAGE                     
         BNE   IMPD04                                                           
         CLI   1(RE),C'T'          T7=MIDAGE                                    
         BE    *+12                                                             
         CLI   1(RE),C'O'          O7=OMAGE                                     
         BNE   IMPD04                                                           
         LA    R1,IMDNONE          NO DELAYED COMPUTATIONS                      
         B     IMPD04                                                           
*                                                                               
IMPD03   CLC   2(2,RE),=H'1'       SPECIAL FOR HOMES                            
         BNE   *+8                 AVERAGE RTG/PUT=SHARE ONLY                   
         LA    R1,IMDNTIH                                                       
*                                                                               
         CLI   PDMETHOD,5          METHOD=SOURCE FOR SPOT/REP                   
         BNE   IMPD04                                                           
         LA    R1,IMDNSIH                                                       
         CLI   2(RE),1             SPECIAL FOR HOMES                            
         BE    IMPD04                                                           
         LA    R1,IMDNONE          NONE FOR METROS                              
         CLI   2(RE),2                                                          
         BE    IMPD04                                                           
         CLI   2(RE),3                                                          
         BE    IMPD04                                                           
         LA    R1,IMDNSI                                                        
*                                                                               
IMPD04   CLI   0(R1),X'FF'         MODIFIER CONVERSION FILTERS                  
         BE    IMPD08                                                           
         CLC   1(1,RE),0(R1)                                                    
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         B     IMPD04                                                           
*                                                                               
         CLI   PDDUROPT,C'M'       ALIAS DEMOS                                  
         BNE   IMPD05A                                                          
         CLC   PDSOURCE(3),=C'NTI' ZAP NAD CAT FOR NTI ONLY                     
         BE    IMPD05A                                                          
         CLC   PDSOURCE(4),=C'OOH '                                             
         BE    IMPD05A                                                          
         CLC   PDSOURCE(5),=C'ACM  ' ZAP NAD CAT FOR COMM AVG                   
         BE    IMPD05A                                                          
         CLC   PDSOURCE(5),=C'OOHC ' ZAP NAD CAT FOR COMM AVG                   
         BE    IMPD05A                                                          
         CLC   PDSOURCE(5),=C'MXM  ' ZAP NAD CAT                                
         BE    IMPD05A                                                          
         CLC   PDSOURCE(3),=C'REN'                                              
         BE    IMPD05A                                                          
IMPD05   MVC   0(1,RF),0(RE)       OTHERWISE MAINTAIN NAD CAT                   
         B     IMPD05B                                                          
*                                                                               
IMPD05A  STM   RE,RF,DUB           ZAP NAD CATEGORIES                           
         BRAS  RE,ZAPNAD           EXCEPT FOR USER DEMOS                        
         LM    RE,RF,DUB           RESTORE REGISTERS                            
IMPD05B  MVC   1(1,RE),1(R1)       BASE CODE                                    
         MVC   1(1,RF),2(R1)       DIVISOR CODE                                 
*                                                                               
         CLI   2(R1),C' '          CAN CHANGE MODIFIER ONLY                     
         BNE   *+14                JUST MAKE SURE TO                            
         XC    0(4,RF),0(RF)       ZAP THE DIVISOR                              
         MVI   0(R3),C' '          NO PERSONAL LANGUAGE FOR DIVISOR             
*                                                                               
         CLI   2(R1),C'*'          INDICATES YHOMES                             
         BNE   IMPD05C                                                          
         MVI   1(RF),C'Y'          SET THE MODIFIER                             
         MVC   2(2,RF),=X'0001'    SET THE DEMO CODE                            
         MVI   0(RF),0             FORCE TO TOTAL US                            
         MVI   0(R3),C' '          NO PERSONAL LANGUAGE FOR YHOMES              
*                                                                               
         CLC   PDSOURCE(3),=C'NAD'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'NAW'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(4),=C'CNAW'                                             
         BE    *+10                                                             
         CLC   PDSOURCE(4),=C'CNAD'                                             
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'NHT'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'NHW'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'HPM'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'HPW'                                              
         BNE   *+10                                                             
         MVC   2(2,RF),=AL2(19)    SET THE DEMO CODE TO USA HOMES               
*                                                                               
IMPD05C  CLI   2(R1),C'#'          INDICATES YV2+                               
         BNE   IMPD05D                                                          
         MVI   1(RF),C'Y'          SET THE MODIFIER                             
         MVC   2(2,RF),=X'007F'    SET THE DEMO CODE                            
*                                                                               
IMPD05D  CLI   2(R1),C'$'          INDICATES BHOMES                             
         BNE   IMPD06                                                           
         MVI   1(RF),C'B'          SET THE MODIFIER                             
         MVC   2(2,RF),=X'0001'    SET THE DEMO CODE                            
         MVI   0(RF),0             FORCE TO TOTAL US                            
         MVI   0(R3),C' '          NO PERSONAL LANGUAGE FOR YHOMES              
         CLC   PDSOURCE(3),=C'NAD'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(3),=C'NAW'                                              
         BE    *+10                                                             
         CLC   PDSOURCE(4),=C'CNAW'                                             
         BE    *+10                                                             
         CLC   PDSOURCE(4),=C'CNAD'                                             
         BNE   *+10                                                             
         MVC   2(2,RF),=AL2(19)    SET THE DEMO CODE TO USA HOMES               
*                                                                               
IMPD06   CLI   PDMETHOD,5          METHOD=SOURCE FOR SPOT/REP                   
         BNE   *+16                                                             
         AH    RF,LNDEMCOD         SPOT HAS 3-BYTE DEMOS                        
         AH    RE,LNDEMCOD                                                      
         B     *+12                                                             
         LA    RF,4(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    R3,PODPLLQ(R3)                                                   
         CLI   0(RE),X'FF'                                                      
         BNE   IMPD02                                                           
         CLI   PDMETHOD,5                                                       
         BNE   *+8                                                              
         MVI   0(RF),X'FF'                                                      
         B     IMPDX                                                            
*                                                                               
IMPD08   XC    0(4,RF),0(RF)                                                    
         MVI   0(R3),C' '          NO PERSONAL LANGUAGE IF NO DIVISOR           
         CLC   PDSOURCE(3),=C'NTI'                                              
         JE    IMPD09                                                           
         CLC   PDSOURCE(4),=C'OOH '                                             
         JE    IMPD09                                                           
         CLC   PDSOURCE(5),=C'MXM  '                                            
         JE    IMPD09                                                           
         CLC   PDSOURCE(3),=C'REN'                                              
         JE    IMPD09                                                           
         CLC   PDSOURCE(5),=C'ACM  '                                            
         JE    IMPD09                                                           
         CLC   PDSOURCE(5),=C'OOHC '                                            
         JNE   *+8                                                              
IMPD09   MVI   0(RE),0                                                          
         B     IMPD06                                                           
*                                                                               
IMPDX    B     XITS1                                                            
         EJECT                                                                  
MRKTNA   DS    0H                                                               
         MVC   PREVSTA,PDSTAT      READ MARKET ONCE FOR DIFF STATION            
         MVC   PDMKTNAM,=CL24' '                                                
         MVC   PDMKTALF,=CL24' '                                                
         CLC   =C'MPA',DBFILE      ONLY MPA & TP HAVE MARKET NAMES              
         BE    MRKT02                                                           
         CLC   =C'TP',DBFILE                                                    
         BE    MRKT02                                                           
         CLC   =C'CTP',DBFILE      FOR COUNTY COVERAGE, GET DMA NAME            
         BNE   MRKTX                                                            
         GOTO1 DEFINE,DMCB,=C'DMAN   ',DBLOCK,PDMKTNAM                          
         B     MRKTX                                                            
*                                                                               
MRKT02   LA    RE,DBLOCK           SAVE DBLOCK                                  
         LA    RF,DBLOCKL                                                       
         LA    R0,SVDBLK2                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,DBAREC           SAVE THE KEY                                 
         MVC   SVKEY,0(RF)                                                      
*                                                                               
         L     R1,AMYIO            USE MY IOAREA                                
         ST    R1,DBAREC                                                        
*                                                                               
         XC    KEY,KEY                                                          
         SR    R3,R3                                                            
         ICM   R3,3,PDMRKT                                                      
*                                                                               
         LA    R2,KEY                                                           
         USING CTDMREC,R2                                                       
         MVI   CTDMKTYP,CTDMKTEQ   SET UP KEY                                   
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVI   CTDMKMED,C'T'       TELEVSION                                    
         CLI   PODBMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   CTDMKMED,C'C'       CANADIAN                                     
         MVC   CTDMKSRC,PODBSRC    SOURCE (A/N)                                 
         CLI   CTDMKSRC,C'F'                                                    
         BNE   *+8                                                              
         MVI   CTDMKSRC,C'N'       FUSION READS NSI MARKETS                     
         MVC   KEYSAVE2,KEY                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE  ',KEYSAVE2,KEY,0            
*                                                                               
MRKT04   DS    0H                                                               
         CLC   KEY(19),KEYSAVE2                                                 
         BNE   MRKT08                                                           
         SR    R1,R1                                                            
         ICM   R1,3,KEY+23                                                      
         CR    R1,R3                                                            
         BNE   MRKT06                                                           
         MVC   PDMKTALF,KEY+19                                                  
         B     MRKT08                                                           
*                                                                               
MRKT06   GOTO1 DATAMGR,DMCB,=C'DMRSEQ  ',=C'CTFILE  ',KEYSAVE2,KEY,0            
         B     MRKT04                                                           
*                                                                               
MRKT08   SR    RF,RF                                                            
         ICM   RF,3,PDMRKT                                                      
         LTR   RF,RF                                                            
         BZ    MRKT10                                                           
         STCM  RF,3,DBSELRMK                                                    
*                                                                               
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   PODBMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         MVI   DBFUNCT,DBGETMK                                                  
         GOTO1 DEMAND,DMCB,DBLOCK,MKTHOOK                                       
*                                                                               
MRKT10   LA    RE,SVDBLK2          RESTORE DBLOCK                               
         LA    RF,DBLOCKL                                                       
         LA    R0,DBLOCK                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,DBAREC                                                        
         MVC   0(L'SVKEY,RF),SVKEY      RESTORE THE KEY                         
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI  '),=C'DEMFIL  ',DBNDXDA,     X        
               DBAREC,0                                                         
*                                                                               
         L     RF,DBAREC                                                        
         CLC   SVKEY,0(RF)        MAKE SURE WE HAVE THE RIGHT KEY               
         BE    MRKTX                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ  '),=C'DEMFIL  ',DBNDXDA,     X        
               DBAREC,0                                                         
*                                                                               
MRKTX    B     XITS1                                                            
         EJECT                                                                  
FIGMINS  CLC   PDSOURCE(5),=C'MXM  '   FOR MXM,                                 
         BNE   FIGM20                  GET DURATION FROM DEFINE                 
         GOTO1 DEFINE,DMCB,=C'DURATION',DBLOCK,WORK                             
         XC    PDMINS,PDMINS                                                    
         MVC   PDMINS+2(2),WORK                                                 
         B     FIGMX                                                            
*                                                                               
FIGM20   MVC   PDMINS,=F'30'           FOR EVERYTHING ELSE,                     
         OC    PDTIME+2(2),PDTIME+2    COMPUTE DURATION FROM                    
         BZ    FIGMX                   END TIME-START TIME                      
         XC    DUB,DUB                                                          
         MVC   FULL,PDTIME                                                      
         LA    R2,PDTIME                                                        
         LA    R3,DUB                                                           
         BAS   RE,GETMINS                                                       
         LA    R2,2(R2)                                                         
         LA    R3,DUB+4                                                         
         CLC   0(2,R2),=H'600'     DEAL WITH 6AM END TIME                       
         BNE   *+10                                                             
         MVC   0(2,R2),=H'559'                                                  
         BAS   RE,GETMINS                                                       
         LM    R0,R1,DUB           START R0, END R1                             
*                                                                               
         CLC   FULL,PDTIME         ADJUST AND RESTORE IF 6AM END                
         BE    *+14                                                             
         MVC   PDTIME,FULL                                                      
         LA    R1,1(R1)                                                         
*                                                                               
         SR    R1,R0                                                            
         BNP   FIGMX                                                            
         STCM  R1,15,PDMINS                                                     
*                                                                               
FIGMX    B     XITS1                                                            
         EJECT                                                                  
GETMINS  NTR1                                                                   
         LH    R1,0(R2)            MILITARY TIME TO MINUTES                     
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CHI   R1,6                                                             
         BNL   *+8                                                              
         LA    R1,24(R1)                                                        
         MHI   R1,60                                                            
         AR    R1,R0                                                            
         ST    R1,0(R3)                                                         
         B     XITS1                                                            
         EJECT                                                                  
DAYPDAY  DS    0C                  WORK OUT MINUTES FROM PDTIME                 
         CLI   PODBMED,C'N'        NETWORK CABLE                                
         BNE   *+8                                                              
         CLI   DBSELSTA+4,C'C'     CABLE PROGRAM DATA?                          
         BNE   DAYP01                                                           
*                                  GET DAY AND TIME FROM EXTENSION              
         CLI   ODYTSET,C'Y'        WAS EXTEND SET?                              
         BNE   DAYP01                                                           
         GOTOR SUBR02,DMCB,('GETEXTE',(RC)),=C'ODYT'                            
         ICM   RE,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'0'                SHOULD BE THERE FOR CABLE                    
         USING DBXTLD,RE                                                        
         ZIC   RF,DBXTLIDX                                                      
         MHI   RF,5                                                             
         LA    RE,PODDAYTM         USE ORIGINAL TABLE BECAUSE                   
         AR    RE,RF               DBXTLIST HAS BEEN CHANGED TO                 
         MVC   TIMEHOLD,1(RE)      INCLUDE '7F' FOR ALL DAYS (NOT FF)           
         MVC   DAYHOLD,0(RE)               600-545 FOR ALL TIMES                
         DROP  RE                                                               
*                                                                               
DAYP01   MVC   PDTIME,TIMEHOLD     LOAD TIME FIELD                              
         XC    PDDAY,PDDAY                                                      
         MVC   PDDAY(1),DAYHOLD       DAY CODE                                  
         MVC   PDDAY+1(1),DAYHOLD     DAY CODE                                  
         LAY   R2,DAYPTAB                                                       
*                                                                               
DAYP02   CLI   0(R2),X'C1'                                                      
         BE    DAYP04                                                           
         CLC   0(1,R2),DAYHOLD                                                  
         BE    DAYP04                                                           
         LA    R2,4(R2)                                                         
         B     DAYP02                                                           
*                                                                               
DAYP04   MVC   PDDAY+2(3),1(R2)                                                 
*                                                                               
DAYPX    B     XITS1                                                            
         EJECT                                                                  
SPDDAY   DS    0H                                                               
         LAY   RF,SPOTDAYS         FIND THE CORRECT SID DAY                     
         SR    RE,RE                                                            
*                                                                               
SPDD02   CLI   0(RF),X'FF'                                                      
         BE    SPDD04                                                           
         CLC   PDDAY+1(1),0(RF)                                                 
         BE    SPDD04                                                           
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         B     SPDD02                                                           
*                                                                               
SPDD04   STC   RE,PDDAY            FORCE DAY/TIME = SID DAY/TIME                
         L     R5,ASORTREC                                                      
         USING SORTDATA,R5                                                      
         STC   RE,SORTDP                                                        
         STC   RE,SORTDAY                                                       
*                                                                               
SPDDX    B     XITS1                                                            
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        HUTS BROKEN OUT BY HALF HOUR INSTEAD OF QUARTER HOUR         *         
***********************************************************************         
*                                                                               
HALFHUT  DS    0H                  WORK OUT MINUTES FROM PDTIME                 
         CLC   DBSELSTA(3),=CL3'HUT'                                            
         BNE   HALFX                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,PDTIME                                                      
         D     R0,=F'100'                                                       
*                                                                               
*                        CHECK IF START TIME ON QUARTER HOUR BOUNDARY           
         CHI   R0,15                                                            
         BE    HALF04                                                           
         CHI   R0,45                                                            
         BE    HALF04                                                           
         CHI   R0,30                                                            
         BE    HALF02                                                           
*                                                                               
*                              ADJUST END TIME TO HALF HOUR BOUNDARY            
         SR    R1,R1                                                            
         ICM   R1,3,PDTIME+2                                                    
         AHI   R1,15                                                            
         STCM  R1,3,PDTIME+2                                                    
         B     HALFX                                                            
*                                                                               
*                              ADJUST END TIME TO HALF HOUR BOUNDARY            
HALF02   SR    R1,R1                                                            
         ICM   R1,3,PDTIME+2                                                    
         AHI   R1,55                                                            
         STCM  R1,3,PDTIME+2                                                    
         B     HALFX                                                            
*                                                                               
*                              ADJUST START TIME TO HALF HOUR BOUNDARY          
HALF04   SR    R1,R1                                                            
         ICM   R1,3,PDTIME                                                      
         SHI   R1,15                                                            
         STCM  R1,3,PDTIME                                                      
*                                                                               
HALFX    B     XITS1                                                            
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL MRKTNA ROUTINES                                   *         
***********************************************************************         
*                                                                               
MKTHOOK  NTR1                                                                   
         L     R5,DBAREC                                                        
         MVC   PDMKTNAM,=CL24' '                                                
         LA    R5,23(R5)           FIRST ELEMENT                                
*                                                                               
         ZIC   RE,1(R5)                                                         
         SHI   RE,4                FIRST 4 BYTES ARE INFO                       
         LTR   RE,RE                                                            
         BZ    MKTHX                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     MKTHX                                                            
*                                                                               
MKTHMVE  MVC   PDMKTNAM(0),4(R5)   REAL MARKET NAME                             
*                                                                               
MKTHX    B     XITS1                                                            
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO GET DEMO NAMES                                    *         
*               INPUT : R3 = A(DEMO LIST)                             *         
*                       R4 = A(USER DEMO NAME LIST)                   *         
***********************************************************************         
*                                                                               
GETDNAM  L     R5,PDADEMTB         SET UP DEMO BUFFER                           
         USING PDDMBUFF,R5                                                      
*                                                                               
*        OC    PDDMNAME,PDDMNAME                                                
*        BNZ   GETDNX                                                           
         OC    PDDMNAME(256),PDDMNAME     !!! TO CHANGE THIS. JUST TEMP         
         BNZ   GETDNX                                                           
         OC    PDDMNAME+256(PDDMNAML-256),PDDMNAME                              
         BNZ   GETDNX                                                           
*                                                                               
         LA    R3,PODDEMWK         SAVE A(DEMO LIST)                            
         XC    DBLOCK,DBLOCK       YES - CALL DEMOCON FOR DEMO NAMES            
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'PAV'                                                   
         MVC   DBSELMED,PODBMED                                                 
         MVI   DBSELMED,C'N'       DEFAULT NETWORK                              
         CLC   =C'OPI',PODBEXT     EDIT OPI MODIFIERS                           
         BNE   *+14                                                             
         MVC   DBFILE,PODBEXT                                                   
         MVI   DBSELMED,X'00'                                                   
         CLI   PDOVSYS,3           NET USES 4 BYTE DEMOS                        
         BNE   *+8                                                              
         MVI   DBDEMTYP,C'4'                                                    
*                                                                               
         LA    RF,DBLOCK                                                        
         ST    RF,DMCB+8                                                        
         CLI   PDOVSYS,2                                                        
         BNE   GETDN02                                                          
         MVI   DBSELMED,C'T'                                                    
         MVI   DMCB+8,C'S'         SPOTPAK CALL                                 
         CLC   =C'CCV',PODBEXT     EDIT COUNTY COVERAGE MODIFIERS               
         JE    GETDN01                                                          
         CLI   PODBMED,C'C'                                                     
         BNE   GETDN02                                                          
         MVI   DBSELMED,C'C'                                                    
         MVC   DBFILE,=C'TP '                                                   
         B     GETDN02                                                          
GETDN01  MVC   DBFILE,=C'CUN'      COUNTY COVERAGE                              
         MVI   DBSELMED,C'U'                                                    
*                                                                               
GETDN02  GOTO1 DEMOCON,DMCB,(PODDMWLN,(R3)),(7,PDDMNAME),,0,APDNTDMS            
*                                                                               
GETDNX   B     XITS1                                                            
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RECORD                                              *         
*               AIO3 IS USED AS A BUFFER SPACE AND CANNOT BE USED     *         
*               AS AN IO AREA IN THE WRITER.                          *         
***********************************************************************         
*                                                                               
VREC     DS    0H                                                               
         L     RF,TWAMASTC                                                      
         USING MASTD,RF                                                         
         MVC   PDAUTL,MCUTL                                                     
***      L     RE,AIO3                                                          
***      ST    RE,PDADEMTB         DEMO BUFFER ADDRESS (1037 BYTES)             
***      LA    RE,1038(RE)                                                      
         L     RE,APDFLBUF                                                      
         ST    RE,PDAFLBUF         FLOWCHART BUFFER ADDRESS (300 BYTES)         
*                                                                               
         MVI   CABWFLAG,0                                                       
*                                                                               
         LA    R2,SPLPSTH          PROGRAM DATES                                
         GOTO1 VALMED              GET MEDIA INFORMATION                        
*                                                                               
         LA    R2,SPLPSTH          PROGRAM DATES                                
         GOTO1 VALPROG                                                          
         MVI   ENMIFLAG,0                                                       
*                                                                               
         LA    R2,SPLSBKH          BOOK                                         
         MVI   MAX,10                                                           
         GOTO1 VALBOOK                                                          
*                                                                               
         LA    R2,SPLDEMH          DEMO                                         
         MVI   MAX,8                                                            
         GOTO1 VALDEMO                                                          
*                                                                               
**VALIDATE DEMOS FOR SECOND SCREEN                                              
         OC    RPT2ID,RPT2ID                                                    
         BZ    VREC01                                                           
         LA    R3,0                VALIDATE DEMOS                               
         BAS   RE,RP2GET            FROM CONTINUATION SCREEN                    
*                                                                               
VREC01   L     R3,PDADEMTB         SET UP DEMO BLOCK                            
         MVI   PDSTACK,0           CLEAR STACKS                                 
         MVI   PDGSTACK,0                                                       
         LA    R2,SPLOPTH          OPTIONS                                      
         GOTO1 VALOPTS,DMCB,0                                                   
*                                                                               
         LA    R2,SPLSBKH          REVALIDATE BOOK FOR CALENDAR                 
         MVI   MAX,10               OPTIONS                                     
         GOTO1 VALBOOK                                                          
*                                                                               
* BLOCK SHOULD STILL HAVE THE SOURCES AND BOOKS                                 
*                                                                               
         LA    RF,BLOCK                                                         
         SR    RE,RE                                                            
VREC02   LR    R1,RF                                                            
         CLI   0(RF),0                                                          
         BE    VREC06                                                           
         CLC   =C'MPA',12(RF)                                                   
         BE    VREC04                                                           
         LA    RE,1(RE)            COUNTER FOR NON-MPA SOURCES                  
*                                                                               
VREC04   LA    RF,64(RF)           NEXT SOURCE                                  
         CLC   =C'MI',13(RF)       CHECK FOR EMI/NMI SOURCES                    
         BNE   VREC02                                                           
         LA    RF,32(RF)           SKIP 2ND BOOK                                
         B     VREC02                                                           
*                                                                               
VREC06   LTR   RE,RE               ANY NON-MPA SOURCES?                         
         LA    R2,SPLNETH          NETWORKS                                     
         MVI   MAX,10                                                           
         GOTO1 VALNET                                                           
*                                                                               
         DS    0H                                                               
         LA    R2,SPLDTMH          DAYS/TIMES                                   
*                                                                               
*^^FIX FOR STEREO V1.5                                                          
* THE 1ST COMMA AFTER A NON-SEQUENTIAL ROTATOR, INDICATED BY PERIODS,           
*  GETS REPLACED WITH A SLASH (/).                                              
                                                                                
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         TM    FATSTAT6,TST6STRO+TST6STFU   FULL STEREO SESSION?                
         BNO   VREC12                        NO                                 
         TM    FATSTAT8,TST8PC32   32 BIT MODE PC SOFTWARE                      
         BO    VREC12              DON'T EXECUTE STEREO CHILD CODE              
         DROP  R1                                                               
*                                                                               
         GOTO1 ANY                 CATCH DAY/TIME=BLANK ERROR                   
*                                                                               
         ZIC   R0,SPLDTMH+5                                                     
         LA    RF,SPLDTMH+8                                                     
*                                                                               
VREC08   DS    0H                                                               
         CLI   0(RF),C'.'          LOOK FOR OCCURRENCES OF C'.'                 
         BE    *+16                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,VREC08                                                        
         B     VREC12                                                           
*                                                                               
VREC10   DS    0H                  A NON-SEQUENTIAL ROTATOR ENCOUNTERED         
         CLI   0(RF),C','           LOOK FOR FIRST COMMA AFTER IT               
         BE    *+16                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,VREC10                                                        
         B     VREC12                                                           
*                                                                               
         DS    0H                  REPLACE COMMA WITH A SLASH                   
         MVI   0(RF),C'/'                                                       
         B     VREC08                                                           
*                                                                               
VREC12   EQU   *                                                                
*^^EOFIX                                                                        
**       MVI   MAX,10                                                           
**       GOTO1 VALDYTM                                                          
*                                                                               
         LA    R2,SPLDEMH          DEMO                                         
         MVI   MAX,8                                                            
         GOTO1 VALDEMO                                                          
*                                                                               
**VALIDATE DEMOS FOR SECOND SCREEN                                              
         OC    RPT2ID,RPT2ID                                                    
         BZ    VREC13                                                           
         LA    R3,0                VALIDATE DEMOS                               
         BAS   RE,RP2GET            FROM CONTINUATION SCREEN                    
*                                                                               
VREC13   L     R3,PDADEMTB         SET UP DEMO BLOCK                            
         MVI   PDSTACK,0           CLEAR STACKS                                 
         MVI   PDGSTACK,0                                                       
*                                                                               
         LA    R2,SPLOPTH          OPTIONS                                      
         GOTO1 VALOPTS,DMCB,0                                                   
*                                                                               
         LA    R2,SPLOPT2H         OPTIONS                                      
         GOTO1 VALOPTS,DMCB,(X'FF',DUB)                                         
         L     RE,APODNET                                                       
         LA    RF,LPODNET-1                                                     
         AR    RE,RF                                                            
         MVI   0(RE),X'FF'         END NETWORK TABLE                            
*                                                                               
*DO IT AGAIN IN CASE CONTINUE=... WAS ON SECOND OPTIONS FIELD                   
**VALIDATE DEMOS FOR SECOND SCREEN                                              
         OC    RPT2ID,RPT2ID                                                    
         BZ    VREC13A                                                          
         LA    R2,SPLDEMH          DEMOS FROM 1ST SCREEN                        
         MVI   MAX,8                                                            
         GOTO1 VALDEMO                                                          
*                                  DEMOS FROM 2ND SCREEN                        
         LA    R3,0                VALIDATE DEMOS                               
         BAS   RE,RP2GET            FROM CONTINUATION SCREEN                    
*                                                                               
         L     R3,PDADEMTB                                                      
         MVI   PDSTACK,0                                                        
         MVI   PDGSTACK,0                                                       
         LA    R2,SPLOPTH          AND VALIDATE OPTIONS AGAIN TO MAKE           
         GOTO1 VALOPTS,DMCB,0      SURE STACKED DEMOS ARE SET UP                
         LA    R2,SPLOPT2H         OPTIONS                                      
         GOTO1 VALOPTS,DMCB,(X'FF',DUB)                                         
*                                                                               
*                                                                               
VREC13A  LA    R2,SPLSBKH          REVALIDATE BOOK FOR CALENDAR                 
         MVI   MAX,10               AND EXACT OPTION                            
         GOTO1 VALBOOK                                                          
*                                                                               
         LA    R2,SPLDTMH          DAYS/TIMES                                   
         MVI   MAX,10                                                           
         GOTO1 VALDYTM                                                          
*                                                                               
         LA    R2,SPLFILTH         FILTER                                       
         GOTO1 VALFILT                                                          
*                                                                               
         LA    R2,SPLTITH          TITLE                                        
         MVI   MAX,1                                                            
         GOTO1 VALTITS                                                          
*                                                                               
         MVI   PDCOLRNK,0          DIDN'T USE RANK IN COLUMNS                   
         BAS   RE,CHKCOLS          CHECK TO SEE IF RANKED                       
         BAS   RE,CHKCOLS2         CK COLUMNS IN 2ND SCREEN                     
         CLI   PDCOLRNK,0                                                       
         BZ    VREC14                                                           
*                                                                               
         BAS   RE,CHKHEDS          IF DON'T HAVE, PDCOLRNK=C'N'                 
*                                                                               
         BAS   RE,CHKDETS          IF DON'T HAVE, PDCOLRNK=C'N'                 
         BAS   RE,CHKDETS2                                                      
*                                                                               
VREC14   LA    R2,SPLLEFTH         LEFT HEADER                                  
         MVI   MAX,3                                                            
         GOTO1 VALLEFT                                                          
*                                                                               
         LA    R2,SPLMIDH          MIDLINE                                      
         MVI   MAX,1                                                            
         MVI   PDSQTROW,0          CLEAR SQUAD QUARTER INDICATOR                
         GOTO1 VALMID                                                           
*                                                                               
         LA    R2,SPLDETSH         ROWS                                         
         MVI   MAX,8                                                            
         MVI   PDPRGROW,0          CLEAR PROGRAM GROUP INDICATOR                
         GOTO1 VALROWS                                                          
*                                                                               
**VALIDATE ROWS FOR SECOND SCREEN                                               
         OC    RPT2ID,RPT2ID                                                    
         BZ    VREC20                                                           
         LA    R3,1                VALIDATE ROWS                                
         BAS   RE,RP2GET            FROM CONTINUATION SCREEN                    
*                                                                               
*                                  COLUMNS                                      
VREC20   OI    PDQSKIP,FF          SKIP READING ANY RECORDS TO BEGIN            
         LA    R2,SPLCOLSH                                                      
         MVI   MAX,12                                                           
         GOTO1 VALCOLS                                                          
*                                                                               
**VALIDATE COLUMNS FROM SECOND SCREEN                                           
         OC    RPT2ID,RPT2ID                                                    
         BZ    VREC30                                                           
         LA    R3,2                VALIDATE COLUMNS                             
         BAS   RE,RP2GET            FROM CONTINUATION SCREEN                    
*                                                                               
VREC30   DS    0H                                                               
         TM    DOWNOPT,GLDLACTV    DOWNLOADING?                                 
         BZ    VREC40              NO                                           
         CLC   AGENCY,=C'WI'       IF INITIATIVE                                
         BE    VREC32                                                           
         CLC   AGENCY,=C'MD'                                                    
         BE    VREC32                                                           
         CLC   AGENCY,=C'WR'                                                    
         BNE   VREC40                                                           
VREC32   OI    DOWNOPT2,GLD2FIX    FORCE TO DOWNFIX                             
*                                                                               
VREC40   DS    0H                                                               
         GOTO1 WRAPDRON            WRAP UP DRONE                                
*                                                                               
VRECX    B     XITS1                                                            
         EJECT                                                                  
***********************************************************************         
*        GET UPGRADE DEMOS                                            *         
***********************************************************************         
*                                                                               
UPGRD    DS    0H                                                               
         L     R5,AUPBLOCK                                                      
         USING SPDEMUPD,R5         R5=A(UPGRADE BLOCK)                          
         XC    SPDEMUPD(SPDEMUPL),SPDEMUPD                                      
         MVC   SPUPAREC,AIO1                                                    
         MVC   SPUPAFAC,ACOMFACS                                                
         MVC   SPUPAGY,AGENCY                                                   
         MVC   SPUPMED,DBSELMED                                                 
         MVC   SPUPSTA,DBSELSTA                                                 
         MVC   SPUPSPL,DBSELMK     SWEEP MARKET NUMBER                          
         MVC   SPUPDAY,DBSELDAY                                                 
         MVC   SPUPTIM,DBSELTIM                                                 
         MVC   SPUPFIL,DBFILE                                                   
         MVC   SPUPSRC,DBSELSRC                                                 
         MVC   SPUPFBK,PDBKOPT+1                                                
         MVC   SPUPBTYP,PDBKTYP                                                 
         MVC   SPUPTYPE(8),PDUPOPT+4                                            
         MVI   SPUPTPTT,C'T'                                                    
         TM    PDSOURCE+1,X'F0'    SET FOR T4/T3 FILES                          
         BNO   *+8                                                              
         MVI   SPUPTPTT,C'P'                                                    
         CLI   DBSELSRC,C'N'       PRECISION OPT FOR NSI/USTV ONLY              
         BNE   UPGR20                                                           
         CLI   DBSELMED,C'O'                                                    
         BE    *+8                                                              
         CLI   DBSELMED,C'T'                                                    
         BNE   UPGR20                                                           
         CLI   PROF1W+5,C'I'       IMP BASED RATINGS                            
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
         CLI   PDOVSYS,8                                                        
         BE    *+12                                                             
         CLI   PROF1W+7,C'Y'       NORMALIZE HPT LINE                           
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
UPGR20   CLI   PDSPTOPT,C'Y'                                                    
         BNE   *+8                                                              
         OI    SPUPOPTS,X'20'                                                   
         CLI   PDSPTOPT,C'N'                                                    
         BNE   *+8                                                              
         OI    SPUPOPTS,X'10'                                                   
*                                                                               
         CLI   EBOPT,C'Y'          INDEX 100 UPG FOR NON-EST BOOK               
         BE    *+16                                                             
         MVC   SPUPFBK,PDSBOOK                                                  
         MVC   SPUPTYPE(8),=X'0400006400000000'                                 
*                                                                               
         MVI   SPUP2YRP,C'N'       SET FOR 1/2 YEAR PUTS                        
         MVI   SPUP2YRR,C'N'         AND RTGS                                   
*                                                                               
         CLI   PDSUPOPT,C'Y'       OPTION TO TAKE UPGRADE FROM SID              
         BNE   UPGR22                                                           
*                                                                               
*                                  NEWSID                                       
         ICM   R4,15,PDSDBFAD                                                   
         USING SRBLKD,R4                                                        
         CLI   SRUPFILE,0          BUT ONLY IF SOMETHING THERE                  
         BE    UPGRX                                                            
*                                                                               
         MVC   SPUPFIL,SRUPFILE                                                 
         MVC   SPUPUDAY,SRUPDAY                                                 
         MVC   SPUPUTIM,SRUPTIM                                                 
         MVC   SPUPTYPE(8),SRUPEXP                                              
         OC    SRUPBOOK,SRUPBOOK                                                
         BZ    *+10                                                             
         MVC   SPUPFBK,SRUPBOOK                                                 
         DROP  R4                                                               
*                                                                               
UPGR22   L     RE,AEXDEMOS         MOVE EXDEMOS TO UPDEMOS                      
*        L     RF,AUPDEMOS                                                      
*        MVC   0(L'EXDEMOS,RF),0(RE)                                            
         LA    RF,L'EXDEMOS                                                     
         L     R0,AUPDEMOS                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,SPDEMUP                                                       
         CLI   PDOVSYS,8                                                        
         BNE   UPGR24                                                           
         ICM   RF,15,PDAUTL                                                     
         MVC   4(1,RF),PDSSENUM                                                 
*                                                                               
UPGR24   GOTO1 SPDEMUP,DMCB,SPDEMUPD,AUPDEMOS,ASORTDEM                          
         CLI   0(R1),0             TEST FOR ERRORS                              
         BNE   UPGRX                                                            
         DROP  R5                                                               
*                                                                               
         L     R5,ASORTREC                                                      
         USING SORTDATA,R5                                                      
         MVC   DBFACTOR,=X'0001'                                                
         MVC   PDDAY+1(1),DBSELDAY  AND PRESERVE ACTUAL DAY                     
         MVC   PDTIME(4),DBSELTIM      USE TIME FROM DBLOCK                     
*        GOTO1 =V(PRNTBL),DMCB,=C'PDDAY',PDDAY,C'DUMP',32,=C'1D'                
         MVC   PDPROG,=CL16'VARIOUS'                                            
         CLI   DAYPOPT,C'Y'                                                     
         BNE   UPGR26                                                           
         GOTOR SUBR01,DMCB,('DAYPDAYE',(RC))                                    
*                                                                               
UPGR26   GOTOR SUBR01,DMCB,('FIGMINSE',(RC))                                    
         CLI   PDSIDOPT,C'Y'                                                    
         BNE   UPGR28                                                           
         ICM   RE,15,PDSDBFAD                                                   
         USING SRBLKD,RE                                                        
         MVC   PDTIME(4),SRACTTIM                                               
         MVC   PDDAY+1(1),DBSELDAY  AND PRESERVE ACTUAL DAY                     
         MVC   PDDAY+2(3),SRERDAY                                               
         DROP  RE                                                               
*                                                                               
UPGR28   DS    0H                                                               
         CLI   PDSUPOPT,C'Y'       TEST UPGRADES FROM SID                       
         BNE   UPGR30                                                           
         CLI   ANYUP,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,GETOVRD                                                       
*                                                                               
UPGR30   DS    0H                                                               
         L     R1,PDADEMTB         SET UP DEMO BLOCK                            
         USING PDDMBUFF,R1                                                      
         LA    RE,PODDEMO          A(DEMO CODES)                                
         LA    RF,PDDEMOS          A(DEMO VALUES)                               
         L     R1,ASORTDEM                                                      
*                                                                               
UPGR32   CLI   0(RE),X'FF'                                                      
         BE    UPGR34                                                           
         MVC   0(4,RF),0(R1)                                                    
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         AH    RE,LNDEMCOD                                                      
         B     UPGR32                                                           
*                                                                               
UPGR34   DS    0H                                                               
         OC    DBFACTOR,DBFACTOR                                                
         BZ    UPGRX                                                            
         SR    RF,RF                                                            
         ICM   RF,3,DBFACTOR       SET WEIGHT                                   
         STCM  RF,15,PDWEIGHT                                                   
         MVI   DBSELDAY,X'40'                                                   
         MVC   DBSELTIM(2),=H'1700'                                             
         MVC   DBSELTIM+2(2),=H'1705'                                           
         GOTO1 DEMAND,DMCB,DBLOCK,PDHOOK                                        
*                                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINPUT                                                   
         CLI   EBOPT,C'Y'                                                       
         BNE   *+8                                                              
         MVI   PDNOWEB,C'Y'                                                     
         MVC   PDSTAT,PDNET          USE ONE FROM NETWORK                       
*                                                                               
         GOTOR SUBR01,DMCB,('SPDDAYE',(RC))                                     
*                                                                               
         MVC   DBLOCKA,DBLOCK      MAKE SURE DBLOCKA IS UPDATED                 
         GOTO1 DRIVER,DMCB,(R4)                                                 
         MVI   PDNOWEB,C'N'                                                     
         XC    PDRUN,PDRUN                                                      
*                                                                               
UPGRX    B     XITS1                                                            
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL VALREC ROUTINES                                   *         
***********************************************************************         
*                                                                               
CHKHEDS  NTR1                                                                   
         LA    R0,3                MAX # OF HEADERS                             
         LA    R2,SPLLEFTH                                                      
*                                                                               
CHKH02   CLI   5(R2),0             EMPTY?                                       
         BNE   CHKH04              WE HAVE SOMETHING, KEEP IT                   
         BAS   RE,BMPPRTS                                                       
         BCT   R0,CHKH02                                                        
         CLI   SPLMIDH+5,0         CHECK MID TOO                                
         BNE   CHKH04                                                           
         NI    PDCOLRNK,X'FD'                                                   
         B     CHKHX                                                            
*                                                                               
CHKH04   OI    PDCOLRNK,2                                                       
*                                                                               
CHKHX    B     XITS1                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
CHKDETS  NTR1                                                                   
         LA    R0,8                MAX # OF DETS                                
         LA    R2,SPLDETSH                                                      
*                                                                               
CHKD02   CLI   5(R2),0                                                          
         BE    CHKDX                                                            
         BAS   RE,CKDETAIL                                                      
         BE    CHKDX               PDCOLRNK SET TO C'Y' IN ABOVE                
         BAS   RE,BMPPRTS                                                       
         BCT   R0,CHKD02                                                        
*                                                                               
CHKDX    B     XITS1                                                            
*                                                                               
CKDETAIL NTR1                                                                   
         LA    R4,BLOCK+84                                                      
         XC    BLOCK(252),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(20,(R2)),(6,(R4)),0                                
         ZIC   R0,4(R1)            # OF LINES                                   
*                                                                               
CKDE02   CLI   12(R4),C'*'         DID WE USE AVERAGE?                          
         BNE   CKDE04                                                           
         OI    PDCOLRNK,1                                                       
         SR    RE,RE                                                            
         B     CKDEX                                                            
*                                                                               
CKDE04   LA    R4,42(R4)                                                        
         BCT   R0,CKDE02                                                        
*                                                                               
CKDEX    B     XITS1                                                            
         SPACE 2                                                                
*                                                                               
*                                                                               
CHKDETS2 NTR1                                                                   
         OC    RPT2ID,RPT2ID                                                    
         BZ    CHKD2X              NO CONTINUATION SCREEN                       
         TM    PDCOLRNK,1                                                       
         BO    CHKD2X              ALREADY SET IN FIRST SCREEN                  
         LA    R3,4                CK HEADS                                     
         BAS   RE,RP2GET                                                        
CHKD2X   B     XITS1                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
CHKCOLS  NTR1                                                                   
         LA    R0,12               MAX # OF COLS                                
         LA    R2,SPLCOLSH                                                      
*                                                                               
CHKC02   CLI   5(R2),0                                                          
         BE    CHKCX                                                            
         BAS   RE,CKCOLS                                                        
         BE    CHKCX               PDCOLRNK SET TO C'Y' IN ABOVE                
         BAS   RE,BMPPRTS                                                       
         BCT   R0,CHKC02                                                        
*                                                                               
CHKCX    B     XITS1                                                            
*                                                                               
CKCOLS   NTR1                                                                   
         LA    R4,BLOCK+84                                                      
         XC    BLOCK(252),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(20,(R2)),(6,(R4)),0                                
         ZIC   R0,4(R1)            # OF LINES                                   
         LTR   R0,R0                                                            
         BZ    CKCO08                                                           
*                                                                               
CKCO02   CLC   =C'RANK',12(R4)     DID WE USE RANK?                             
         BNE   CKCO04                                                           
         OI    PDCOLRNK,4                                                       
         B     CKCO06                                                           
*                                                                               
CKCO04   CLC   =C'ST',12(R4)       DID WE USE STACK?                            
         BNE   CKCO06                                                           
         MVI   PDSTACK,X'FF'                                                    
*                                                                               
CKCO06   LA    R4,42(R4)                                                        
         BCT   R0,CKCO02                                                        
*                                                                               
CKCO08   CLI   PDCOLRNK,0                                                       
         BZ    *+6                                                              
         SR    RE,RE                                                            
         LTR   RE,RE                                                            
*                                                                               
CKCOX    B     XITS1                                                            
         SPACE 2                                                                
*                                                                               
*                                                                               
CHKCOLS2 NTR1                                                                   
         OC    RPT2ID,RPT2ID                                                    
         BZ    CHKC2X              NO CONTINUATION SCREEN                       
         LA    R3,3               CK COLUMNS                                    
         BAS   RE,RP2GET                                                        
CHKC2X   B     XITS1                                                            
         EJECT                                                                  
*                                                                               
* RP2GET - READ IN CONTINUED REPORT                                             
*                                                                               
RP2GET   NTR1                                                                   
*                                                                               
         USING CT01RECD,R4                                                      
         XC    KEY,KEY             ESTABLISH PROGRAM RECORD KEY                 
         LA    R4,KEY                                                           
         MVI   CT01TYPE,CT01TYPQ   SET RECORD TYPE                              
         MVC   CT01AGID,AGYALPHA   SET AGENCY ID                                
         MVI   CT01SYS,3           SET FOR NETWORK                              
         MVI   CT01PRG,37          SET FOR NETWORK RES WRITER                   
         MVI   CT01PHAS,1          SET FOR PHASE 1                              
         MVC   CT01NAME,RPT2ID     SET SECOND REPORT ID                         
         DROP  R4                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO2                                                         
         MVC   FILENAME,=CL8'CTFILE'  SET FILE NAME                             
         IC    R0,USEIO                                                         
         MVI   USEIO,C'Y'                                                       
         GOTO1 READ                READ IN PROGRAM RECORD                       
         CLC   KEY(L'CT01KEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                SHOULD ALREADY HAVE BEEN VALIDATED           
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         STC   R0,USEIO                                                         
*                                                                               
         L     R0,ATIA             SAVE OFF SCREEN                              
         LHI   R1,SPLWORK-T325FFD                                               
         LR    RF,R1                                                            
         LR    RE,R7                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   SVDISP,DATADISP                                                  
         MVC   DATADISP,=H'28'                                                  
         IC    R0,MODE                                                          
         MVI   MODE,DISPREC                                                     
         GOTO1 GENPROG,DMCB,(RC)                                                
         STC   R0,MODE                                                          
         MVC   DATADISP,SVDISP                                                  
*                                                                               
         OI    SCR2IND,SCR2YES     NOW PROCESSING 2ND SCREEN       X RP         
*                                                                               
         SLL   R3,2                X 4                                          
         B     *+4(R3)                                                          
         B     RP2DEMS      R3=0-> VALIDATE DEMOS                               
         B     RP2ROWS      R3=4-> VALIDATE ROWS                                
         B     RP2COLS      R3=8-> VALIDATE COLUMNS                             
         B     RP2CKCOL    R3=12-> CHECK COLUMNS FOR RANK                       
         B     RP2CKDET    R3=16-> CHECK DETAILS                                
*                                                                               
RP2DEMS  LA    R2,SPLDEMH          -VALIDATE DEMOS-                             
         MVI   MAX,8                                                            
         PRINT GEN                                                              
         GOTO1 VALDEMO                                                          
         PRINT NOGEN                                                            
         B     RP2X                                                             
*                                                                               
RP2ROWS  LA    R2,SPLDETSH         -VALIDATE ROWS-                              
         MVI   MAX,8                                                            
**       MVI   PDPRGROW,0          MAY ALREADY BE SET ON FIRST SCREEN           
         GOTO1 VALROWS                                                          
         B     RP2X                                                             
*                                                                               
RP2COLS  LA    R2,SPLCOLSH         -VALIDATE COLUMNS-                           
         MVI   MAX,12                                                           
         GOTO1 VALCOLS                                                          
         B     RP2X                                                             
*                                                                               
RP2CKCOL BAS   RE,CHKCOLS                                                       
         B     RP2X                                                             
*                                                                               
RP2CKDET BAS   RE,CKDETAIL                                                      
         B     RP2X                                                             
*                                                                               
RP2X     DS    0H                                                               
         XI    SCR2IND,SCR2YES     FINISHED PROCESSING 2ND SCREEN               
         MVC   AIO,AIO1                                                         
         L     RE,ATIA             RESTORE SCREEN                               
         LHI   R1,SPLWORK-T325FFD                                               
         LR    RF,R1                                                            
         LR    R0,R7                                                            
         MVCL  R0,RE                                                            
         XIT1                                                                   
*                                                                               
SVDISP   DS    XL2                 DISPLACEMENT                                 
*                                                                               
         EJECT                                                                  
BMPPRTS  ZIC   RF,0(R2)            ONCE FOR INPUT                               
         AR    R2,RF                                                            
         ZIC   RF,0(R2)            AND ONCE FOR PROTECTED LABEL                 
         AR    R2,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        PDHOOK                                                       *         
***********************************************************************         
*                                                                               
PDHOOK   NTR1                                                                   
         GOTO1 DEFINE,DMCB,=C'AFFL',DBLOCK,PDTPAFFL                             
         GOTO1 (RF),(R1),=C'MARKET',,PDMRKT                                     
*                                                                               
         CLC   PREVSTA,PDSTAT      GET MARKET NAME                              
         BNE   *+14                                                             
         CLC   PREVMKT,PDMRKT                                                   
         BE    PDHOX                                                            
*                                                                               
         GOTOR SUBR01,DMCB,('MRKTNAE',(RC))                                     
*                                                                               
PDHOX    B     EXITS1                                                           
         EJECT                                                                  
***********************************************************************         
*        GET OVERRIDES                                                *         
***********************************************************************         
*                                                                               
GETOVRD  NTR1                                                                   
         ICM   R4,15,PDSDBFAD          * NEW SID *                              
         USING SRBLKD,R4                                                        
         OC    SROVELEM,SROVELEM                                                
         BZ    GETOX                                                            
*                                                                               
         L     R1,PDADEMTB                                                      
         USING PDDMBUFF,R1                                                      
         LA    R1,PODDEMO          A(DEMO CODES)                                
         ZIC   R0,NDEMOS                                                        
*                                                                               
GETO02   GOTOR SUBR01,DMCB,('GETVALE',(RC))                                     
         AH    R1,LNDEMCOD                                                      
         BCT   R0,GETO02                                                        
GETOX    B     EXITS1                                                           
         DROP  R1                                                               
                                                                                
         EJECT                                                                  
EXITS1   XIT1                                                                   
         EJECT                                                                  
*                                                                               
CONVTAB  DS    0C                                                               
IMDNTI   DC    C'RYU'                                                           
         DC    C'PZU'                                                           
         DC    C'SYZ'                                                           
         DC    X'FF'                                                            
IMDNTIH  DC    C'SRP'                                                           
         DC    X'FF'                                                            
IMDNTIB  DC    C'SRP'                                                           
         DC    X'FF'                                                            
IMDNTIC  DC    C'RYU'                                                           
         DC    C'SRP'                                                           
         DC    C'LBU'                                                           
         DC    C'TY '                                                           
         DC    C'HY '                                                           
         DC    C'VY*'              *=USA.YHOMES                                 
         DC    C'DY#'              #=nnn.YV2+                                   
         DC    C'NB '              GAA IMPRESSIONS                              
         DC    C'MB$'              GAA VPHS. $=USA.BHOMES                       
         DC    X'FF'                                                            
IMDNTIAE DC    C'RYU'              THESE FORMULAS FOR METHOD=AE                 
         DC    C'SYZ'              SAME AS IMDNTIC EXCEPT FOR SHARE             
         DC    C'PZU'              AND PUT                                      
         DC    C'LBU'                                                           
         DC    C'TY '                                                           
         DC    C'HY '                                                           
         DC    C'VY*'              *=USA.YHOMES                                 
         DC    C'DY#'              #=nnn.YV2+                                   
         DC    C'NB '              GAA IMPRESSIONS                              
         DC    C'MB$'              GAA VPHS. $=USA.BHOMES                       
         DC    X'FF'                                                            
IMDNSI   DC    C'RBU'              METHOD=SOURCE FOR NSI                        
         DC    C'PGU'                                                           
         DC    C'SBG'                                                           
         DC    X'FF'                                                            
IMDNSIH  DC    C'RBU'              HOME DEMOS FOR NSI METHOD=SOURCE             
         DC    C'PGU'                                                           
IMDNONE  DC    X'FF'                                                            
*                                                                               
IMDREN   DC    C'RYU'              RENTRAK                                      
         DC    C'TY '                                                           
         DC    C'HY '                                                           
         DC    C'DY#'              #=nnn.YV2+                                   
         DC    X'FF'                                                            
*                                                                               
STTIME   DC    XL2'0'                                                           
ENTIME   DC    XL2'0'                                                           
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         DROP  RB,RA                                                            
         EJECT                                                                  
*                                                                               
*ZAPNAD - ZAP NAD CATEGORIES EXCEPT FOR USER DEMOS                              
*       - DUB HAS ADDRESSES OF DEMO AREAS                                       
*                                                                               
ZAPNAD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LM    RE,RF,DUB                                                        
         CLI   FORCEBTU,C'Y'       TVQ USER DEMOS                               
         BNE   ZAPNAD10                                                         
         CLI   0(RE),171                                                        
         BE    ZAPNADX             DON'T ZAP                                    
         B     ZAPNAD20                                                         
*                                                                               
ZAPNAD10 CLI   FORCEBTU,C'I'       IAG USER DEMOS                               
         BNE   ZAPNAD20                                                         
         CLI   0(RE),175                                                        
         BE    ZAPNADX             DON'T ZAP                                    
         CLI   0(RE),176                                                        
         BE    ZAPNADX                                                          
         CLI   0(RE),177                                                        
         BE    ZAPNADX                                                          
*                                                                               
ZAPNAD20 MVI   0(RE),0                                                          
         MVI   0(RF),0                                                          
*                                                                               
ZAPNADX  J     EXITS1                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'SUB-ROUTINES - PART 2'                                          
SUBR02   NMOD1 0,**SR02**,RA                                                    
         USING WORKD,R6                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         USING PODBD,PODBOOK                                                    
         L     R1,0(R1)                                                         
         SRL   R1,24                                                            
         SLL   R1,2                                                             
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
*                                                                               
PAREPE   EQU   (PAREP#-*)/4+1                                                   
GETMPAHE EQU   (GETMPAH#-*)/4+1                                                 
BTUREADE EQU   (BTUREAD#-*)/4+1                                                 
EVN0E    EQU   (EVN0#-*)/4+1                                                    
RDDMKTE  EQU   (RDDMKT#-*)/4+1                                                  
RDCNTYE  EQU   (RDCNTY#-*)/4+1                                                  
RDRMKTE  EQU   (RDRMKT#-*)/4+1                                                  
FILRDATE EQU   (FILRDAT#-*)/4+1                                                 
SAVPROGE EQU   (SAVPROG#-*)/4+1                                                 
FILPDATE EQU   (FILPDAT#-*)/4+1                                                 
SETCNAE  EQU   (SETCNA#-*)/4+1                                                  
PUTEXTE  EQU   (PUTEXT#-*)/4+1                                                  
GETEXTE  EQU   (GETEXT#-*)/4+1                                                  
SETDBDTE EQU   (SETDBDT#-*)/4+1                                                 
CHKEXTE  EQU   (CHKEXT#-*)/4+1                                                  
ANYMVGE  EQU   (ANYMVG#-*)/4+1                                                  
SETCDTE  EQU   (SETCDT#-*)/4+1                                                  
*                                                                               
GETMPAH# B     GETMPAH0                                                         
PAREP#   B     PAREP                                                            
BTUREAD# B     BTUREAD                                                          
EVN0#    B     EVN0S                                                            
RDDMKT#  B     RDDMKT                                                           
RDCNTY#  B     RDCNTY                                                           
RDRMKT#  B     RDRMKT                                                           
FILRDAT# B     FILRDAT                                                          
SAVPROG# B     SAVPROG                                                          
FILPDAT# B     FILPDAT                                                          
SETCNA#  B     SETCNAD                                                          
PUTEXT#  B     PUTEXT                                                           
GETEXT#  B     GETEXT                                                           
SETDBDT# B     SETDBDT                                                          
CHKEXT#  B     CHKEXT                                                           
ANYMVG#  B     ANYMVG                                                           
SETCDT#  B     SETCDT                                                           
*                                                                               
XITS2    XMOD1                                                                  
         EJECT                                                                  
GETEL1   AH    (R5),DATADISP                                                    
FIRSTEL1 CLI   0((R5)),0                                                        
         BNE   *+10                                                             
         CLI   0((R5)),1                                                        
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0((R5))                                                   
         BCR   8,RE                                                             
NEXTEL1  SR    RF,RF                                                            
         IC    RF,1((R5))                                                       
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         CLI   1((R5)),1                                                        
         BR    RE                                                               
         AR    (R5),RF                                                          
         B     FIRSTEL1                                                         
         TITLE 'GET PARENT REP FROM REP RECORD'                                 
*                                                                               
PAREP    DS    0H                                                               
         XC    KEY,KEY             GET PARENT REP FROM REP RECORD               
         LA    R4,KEY                                                           
         USING RREPKEY,R4                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,TWAAGY                                                  
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMREAD'),(0,=CL8'REPDIR'),         X        
               KEYSAVE,KEY                                                      
         CLI   DMCB+8,0            SKIP IF THERE ARE ERRORS                     
         BNE   PARE02                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=CL8'GETREC'),(0,=CL8'REPFIL'),         X        
               KEY+28,IO,DMWORK                                                 
         CLI   DMCB+8,0            SKIP IF THERE ARE ERRORS                     
         BNE   PAREX                                                            
*                                                                               
         MVC   HALF,DATADISP                                                    
         MVC   DATADISP,=H'34'                                                  
         MVI   ELCODE,X'01'        FIND HEADER ELEMENT                          
         LA    R5,IO                                                            
         BAS   RE,GETEL1                                                        
         BNE   PARE02              ELEMENT NOT FOUND                            
*                                                                               
PARE02   DS    0H                                                               
         MVI   ELCODE,X'04'        FIND PROFILE BIT ELEMENT                     
         LA    R5,IO                                                            
         BAS   RE,GETEL1                                                        
         BNE   PAREX                                                            
         USING RREPPGMP,R5                                                      
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
*                                                                               
PARE04   CLI   0(RE),RREPQRMP      FIND THE RMP/RSR ONE                         
         BE    PARE06                                                           
         LA    RE,RREPPGML(RE)                                                  
         BCT   R0,PARE04                                                        
         B     PAREX                                                            
*                                                                               
PARE06   MVC   RMPPROF(8),2(RE)                                                 
         B     *+8                                                              
         OI    RMPPROF+RMPIMPSB,RMPIMPSA                                        
         CLI   DMAOPT,C'A'                                                      
         BNL   PAREX                                                            
         TM    RMPPROF+RMPIMPSB,RMPIMPSA                                        
         BZ    PAREX                                                            
         MVI   DMAOPT,C'I'                                                      
         DROP  R5                                                               
*                                                                               
PAREX    DS    0H                                                               
         MVC   DATADISP,HALF                                                    
         B     XITS2                                                            
         EJECT                                                                  
GETMPAH0 DS    0H                                                               
         L     R2,SVPODBOK                                                      
         L     R3,PDADEMTB                                                      
         USING PDDMBUFF,R3                                                      
         CLI   PDSOURCE+4,C'A'     ARBITRON?                                    
         BNE   *+8                 DON'T CHANGE SOURCE                          
         MVI   DBSELSRC,C'A'                                                    
         XC    SVPODDEM,SVPODDEM                                                
         LA    R1,PODDEMO          A(DEMO CODES)                                
         LA    RE,SVPODDEM                                                      
*                                                                               
GETM02   CLI   0(R1),X'FF'         END OF DEMOS?                                
         BE    GETM04                                                           
         MVC   0(1,RE),1(R1)                                                    
         LA    RE,1(RE)            NEXT SAVED DEMO                              
         CLI   1(R1),C'T'          HAS TO BE IMPRESSIONS TO REPLACE             
         BNE   *+8                                                              
         MVI   1(R1),C'P'          GET MPA INDEX                                
         AH    R1,LNDEMCOD         NEXT DEMO                                    
         B     GETM02                                                           
*                                                                               
GETM04   MVI   0(RE),X'FF'         END OF DEMOS                                 
         MVC   SVBOOK,DBSELBK                                                   
         MVC   SVDBFILE,DBFILE                                                  
         MVC   SVDBMED,DBSELMED                                                 
         MVC   SVDBAREC,DBAREC                                                  
*                                                                               
         L     R1,AMYIO            USE MY IOAREA                                
         ST    R1,DBAREC                                                        
         MVC   DBSELBK,PODBLNQ*2+12(R2)      GET MONTH/YEAR FOR MPA             
*                                                                               
* GET CORRESPONDING PROGRAM #                                                   
*                                                                               
         XC    DBSELPRG,DBSELPRG                                                
         PACK  DUB(8),PDNTI(5)                                                  
         CVB   R1,DUB                                                           
         STCM  R1,3,SVNTI                                                       
*                                                                               
* DO WE HAVE AN INDEX #?                                                        
         OC    MPAINDEX,MPAINDEX                                                
         BZ    GETM06                                                           
         MVC   DBSELPRG,MPAINDEX                                                
         B     GETM14                                                           
*                                                                               
GETM06   DS    0H                                                               
         XC    KEY,KEY             TRY TO GRAB MPA PROGRAM EQUIV                
         MVC   KEY(2),=C'DP'                                                    
         MVI   KEY+18,C'N'         NETWORK                                      
         MVC   KEY+19(1),DBSELSRC  SOURCE                                       
         STCM  R1,7,KEY+20                                                      
         BAS   RE,SYSHIGH                                                       
         CLC   KEY(23),KEYSAVE     DID WE GET IT?                               
         BNE   GETM16              NO, USE INTERNAL TABLE                       
         MVC   DBSELPRG,KEY+38     YES, USE THE PROGRAM #                       
*                                                                               
GETM14   DS    0H                                                               
         MVC   DBFILE,=C'MPA'                                                   
         MVI   DBFUNCT,11          GET DEMOS                                    
         MVI   DBRECTYP,8                                                       
         MVI   DBINTFIL,C'T'                                                    
         MVI   DBINTMED,C'P'                                                    
         MVC   DBACTSRC,DBSELSRC                                                
*                                                                               
         GOTO1 DEMAND,DMCB,DBLOCK,MPAHOOK2   GET MPA STUFF                      
*                                                                               
GETM16   XC    DBSELPRG,DBSELPRG                                                
         XC    DBSELRMK,DBSELRMK                                                
         MVC   DBAREC,SVDBAREC                                                  
         MVC   DBSELBK,SVBOOK                                                   
         MVC   DBFILE,SVDBFILE                                                  
         MVC   DBSELMED,SVDBMED                                                 
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBFUNCT,9                                                        
*                                                                               
* RESTORE THE DEMOS USED HERE                                                   
*                                                                               
         LA    R1,PODDEMO          A(DEMO CODES)                                
         LA    R2,SVPODDEM                                                      
         LA    R4,PDNDEMS                                                       
*                                                                               
GETM18   MVC   1(1,R1),0(R2)                                                    
         AH    R1,LNDEMCOD                                                      
         LA    R2,1(R2)                                                         
         BCT   R4,GETM18                                                        
*                                                                               
GETMPX   MVC   DBLOCK,SVDBLK                                                    
         B     XITS2                                                            
*                                                                               
*        MPA HOOK                                                     *         
*                                                                               
         USING PDDMBUFF,R3                                                      
MPAHOOK2 NTR1                                                                   
         GOTO1 DEMOUT,DMCB,(C'L',PODDEMO),DBLOCK,MPADEMS,0,0                    
*                                                                               
*                                   NEED MPA UNIVERSES                          
         LA    R1,PODDEMO          A(DEMO CODES)                                
MPAH02   CLI   0(R1),X'FF'                                                      
         BE    MPAH04                                                           
         MVI   1(R1),C'U'                                                       
         AH    R1,LNDEMCOD                                                      
         B     MPAH02                                                           
*                                                                               
MPAH04   GOTO1 DEMOUT,DMCB,(C'L',PODDEMO),DBLOCK,MPAUNIVS,0,0                   
*                                                                               
         LA    RE,PODDEMO          HAVE TO GET THE DECIMAL RIGHT                
         LA    R4,PDNDEMS                                                       
*                                                                               
MPAH06   MVI   1(RE),C'P'         ALWAYS IMPRESSION NEEDED                      
         AH    RE,LNDEMCOD                                                      
         BCT   R4,MPAH06                                                        
*                                                                               
         GOTO1 DEFINE,DMCB,=C'MARKET',DBLOCK,PDMRKT                             
         GOTO1 (RF),(R1),=C'STATION',,PDORGSTA                                  
*                                                                               
         OC    PDMRKT,PDMRKT                                                    
         BZ    MPAHX                                                            
         GOTOR SUBR01,DMCB,('MRKTNAE',(RC))                                     
         MVC   SVPDDEMS,PDDEMOS    SAVE DEMO VALUES                             
*                                                                               
* CALCULATE THE NPD IMPS                                                        
*                                                                               
         LA    R2,REALDEMS                                                      
         LA    R4,MPADEMS                                                       
         LA    R5,PDDEMOS          A(DEMO VALUES)                               
         LA    R1,MPAUNIVS                                                      
*                                                                               
MPAH08   CLI   0(R2),X'FF'         FINISH CHECKING DEMOS?                       
         BE    MPAH18                                                           
         CLI   0(R2),C'T'          HAS TO BE IMPRESSIONS                        
         BE    MPAH10                                                           
         CLI   0(R2),C'R'          OR RATINGS                                   
         BE    MPAH12                                                           
         B     MPAH16                                                           
*                                                                               
MPAH10   SR    RE,RE                                                            
         L     RF,0(R5)            IMPRESSIONS/RATINGS                          
         M     RE,0(R4)            INDEX FROM NPD FILE                          
         D     RE,=F'100'          DIVIDE BY A 1000 AND ROUND                   
         SR    RE,RE                                                            
         AHI   RF,5                                                             
         D     RE,=F'10'                                                        
         ST    RF,0(R5)                                                         
         B     MPAH16                                                           
*                                                                               
MPAH12   SR    RE,RE                                                            
         OC    0(4,R1),0(R1)       MAKE SURE NOT DIVIDING BY ZERO               
         BNZ   MPAH14                                                           
         XC    0(4,R5),0(R5)       ZERO THE RESULT                              
         B     MPAH16                                                           
*                                                                               
MPAH14   L     RF,0(R5)            IMPRESSIONS/RATINGS                          
         M     RE,0(R4)                                                         
         D     RE,0(R1)            DIVIDE BY MPA UNIVERSE                       
         SR    RE,RE                                                            
         AHI   RF,5                                                             
         D     RE,=F'10'                                                        
         ST    RF,0(R5)                                                         
*                                                                               
MPAH16   LA    R2,1(R2)            NEXT DEMO                                    
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         LA    R1,4(R1)                                                         
         B     MPAH08                                                           
*                                                                               
*                                  RELEASE THE DEMOS                            
MPAH18   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R4)                                                 
         CLI   ENMIFLAG,C'E'                                                    
         BE    *+10                                                             
         XC    PDRUN,PDRUN                                                      
         MVC   PDDEMOS(L'SVPDDEMS),SVPDDEMS                                     
*                                                                               
MPAHX    XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
SYSHIGH  NTR1                                                                   
         MVC   COMMAND,=C'DMRDHI  '                                             
         LA    R5,=C'CTFILE  '                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,COMMAND,(R5),KEYSAVE,KEY,0                          
*                                                                               
SYSHX    XIT1                                                                   
         EJECT                                                                  
EVN0S    LA    RE,IO               SAVE ADDRESS OF PROGRAM RECORD               
         ST    RE,APGMREC                                                       
*                                                                               
         USING NPGRECD,R4                                                       
         USING NPGELEM,R5                                                       
         XC    PDRUN,PDRUN                                                      
         XC    PDDAY+2(3),PDDAY+2                                               
         MVC   PDDAY(1),NPGRDAY                                                 
         MVC   PDDAY+1(1),NPGDAY                                                
*                                                                               
         MVC   PDLAIR,NPGKEND      MOVE END DATE                                
         MVC   PDTIME,NPGTIME                                                   
         MVC   PDPROG,NPGNAME                                                   
         MVC   PDFILT,NPGFILT                                                   
         MVI   PDFILT+3,C' '       (ONLY 3 FILTERS FOR PROGS)                   
         SR    RF,RF                                                            
         ICM   RF,3,NPGPPNO                                                     
         EDIT  (RF),(5,PDNTI),FILL=0                                            
*                                                                               
         MVC   HUTTIME,NPGKTIME                                                 
         MVC   HUTSCHEM,SCHEMOPT   PASS OPTIONS                                 
         MVC   HUT52,HUT52OPT                                                   
         CLI   HUTTYPE,0                                                        
         B     *+8                 ***TEMP***                                   
         MVI   HUTTYPE,C'A'        FORCE ASCRIBED AS DEFAULT                    
         MVC   HUTTYPE,TYPEOPT                                                  
         GOTO1 VPRGHUT                                                          
         OC    HUTOVER,HUTOVER     ANY HUT OVERRIDES                            
         BZ    EVN002                                                           
         LH    R0,HUTOVER          YES - SO APPLY PERCENT                       
         LH    R1,HUT                                                           
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         STH   R1,HUT                                                           
*                                                                               
EVN002   XC    PVEL,PVEL                                                        
         MVI   PVEL+0,X'33'                                                     
         MVI   PVEL+1,119                                                       
         MVI   PVEL+2,1            1-BYTE PER VPH                               
         MVC   PVEL+3(34),NPGVPHS                                               
         ZIC   R1,1(R5)                                                         
         AR    R1,R5                                                            
         CLI   0(R1),X'93'         TEST IF NEW PROGRAM ELEMENT FOUND            
         BNE   EVN004                                                           
         USING NPG2ELEM,R1                                                      
         MVI   PVEL+2,X'42'        PRECISION AND 2-BYTES PER VPH                
         MVC   PVEL+3(116),NPG2VPHS                                             
         MVC   PRGSTRT(2),NPG2STD                                               
         XC    PDDP,PDDP                                                        
         MVC   PDDP(L'NPG2DYP),NPG2DYP        MOVE DAYPART TO BLOCK             
         OC    NPG2DYPA,NPG2DYPA                                                
         BZ    *+10                                                             
         MVC   PDDP(L'NPG2DYPA),NPG2DYPA      2-CHAR DAYPART                    
         MVC   PDFAIR,NPG2STD      MOVE START DATE                              
         DROP  R1                                                               
*                                                                               
EVN004   MVI   PIOEOR,0                                                         
         XC    PREL,PREL                                                        
         MVC   PREL(3),=X'350902'  GENERATE RATINGS ELEMENT                     
         MVC   PREL+3(2),NPGSHARE                                               
         TM    NPGSTAT,X'80'                                                    
         BO    EVN006                                                           
         LH    R1,NPGSHARE         COMPUTE RTG=HUT X SHR                        
         MH    R1,HUT                                                           
         SR    R0,R0                                                            
*        LR    RE,R5               LOOK FOR FORMAT CONTROL                      
*CNOVSN  ZIC   RF,1(RE)                                                         
*        AR    RE,RF                                                            
*        CLI   0(RE),X'5E'        FORMAT CONTROL ELEMENT                        
*        BE    *+16                                                             
*        CLI   0(RE),0            EOR                                           
*        BNE   SCNOVSN                                                          
*        B     *+14               PROTECT ME AND USE OLD                        
*        CLC   5(2,RE),=X'5901'   NEW PRECISION                                 
         CLC   PBEL+5(2),=X'5901'                                               
         BE    *+12                                                             
         D     R0,=F'500'         OLD PRECISION                                 
         B     *+8                                                              
         D     R0,=F'50' NEW PRECISION                                          
*                                                                               
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         STH   R1,DUB                                                           
         MVC   PREL+3(2),DUB                                                    
         SR    RE,RE                                                            
         LH    RE,HUT                                                           
         MHI   RE,10                                                            
         STH   RE,HUT              INCREASE HUT PRECISION                       
         MVC   PREL+5(2),HUT                                                    
         MVC   PREL+7(2),NPGSHARE                                               
*                                                                               
EVN006   L     R2,DDPOINT                                                       
         XC    0(12,R2),0(R2)                                                   
         LA    R5,IO                                                            
         MVI   ELCODE,X'DD'        GET DD ELEMENTS                              
         BAS   RE,GETEL1                                                        
         BNE   EVN012                                                           
         USING NPGELDD,R5                                                       
         L     R2,DDPOINT                                                       
         XC    0(12,R2),0(R2)                                                   
         LA    R1,PIOEOR                                                        
*                                                                               
EVN008   CLI   NPGDCAT,1           USE NAD/NHTI IMPS IF THERE                   
         BH    *+12                                                             
         CLI   NPGDMOD,C'T'        BYPASS ANY IMPRESSION OVERRIDES              
         BE    EVN010                                                           
         CLI   NPGDMOD,C'U'        BYPASS ANY UNIVERSE OVERRIDES                
         BE    EVN010                                                           
         MVC   0(12,R2),NPGDEMEL   TEST IF DD ELEMENT                           
         LA    R2,12(R2)                                                        
         CR    R2,R1               OUT OF ROOM                                  
         BL    *+8                 JUST USE WHAT I HAVE                         
         B     EVN012                                                           
*                                                                               
EVN010   BAS   RE,NEXTEL1                                                       
         XC    0(12,R2),0(R2)                                                   
         BE    EVN008                                                           
*                                                                               
EVN012   XC    DBSELRMK,DBSELRMK                                                
         OC    PODMPAMK,PODMPAMK                                                
         BZ    *+10                                                             
         MVC   DBSELRMK,PODMPAMK                                                
         B     XITS2                                                            
         DROP  R4,R5                                                            
         TITLE 'READ FOR BOOK TYPE U'                                           
BTUREAD  DS    0H                                                               
         MVI   GOTUTYPE,0                                                       
         MVI   DBMODE,DBMDSEQ                                                   
*                                                                               
         LA    RE,DBLOCK                                                        
         LA    RF,DBLOCKL                                                       
         L     R0,AMYDBLK                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE               MOVE DBLOCK TO MYDBLK                        
*                                                                               
         L     R3,AMYDBLK          NOW USE MINE                                 
DB       USING DBLOCK,R3                                                        
*                                                                               
         CLC   =C'PIV',PDSOURCE    NO DBAREC FOR PIV                            
         BE    BTUR01X                                                          
         L     RE,DB.DBAREC                                                     
         CLI   0(RE),PRCODEQU                                                   
         BE    BTURX               DOESN'T APPLY TO TP RECORDS                  
         USING PMKEY,RE                                                         
         MVC   DB.DBSELPRG,PMPNUM                                               
         DROP  RE                                                               
         CLI   DB.DBSELSTA+4,C'C'  FOR CABLE PULL DDS PRG#                      
         BNE   BTUR01X                                                          
         MVC   DB.DBSELPRG,=X'FFFF'                                             
         GOTO1 DEFINE,DMCB,=C'NTI',DB.DBLOCK,WORK                               
         PACK  DUB,WORK(5)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,DB.DBSELPRG    STORE DDS NUMBER IN  BINARY                  
         B     BTUR01X                                                          
*                                                                               
BTUR01X  DS    0H                                                               
         MVC   DB.DBFILE,=C'NAD'                                                
         MVI   DB.DBMODE,DBMFRST                                                
         XC    DB.DBAQUART,DB.DBAQUART                                          
         XC    DB.DBCABNUM,DB.DBCABNUM                                          
         MVI   DB.DBFUNCT,DBGETNTI                                              
*                                                                               
         CLC   DB.DBSELSTA+3(2),=C'PN'                                          
         BNE   *+8                                                              
         MVI   DB.DBSELSTA+3,C' '                                               
         CLI   FORCEBTU,C'O'                                                    
         BE    BTUSOK                                                           
         CLI   FORCEBTU,C'I'                                                    
         BE    BTUSOK                                                           
         CLI   DB.DBSELSTA+4,C'T'                                               
         BNE   *+8                                                              
         MVI   DB.DBSELSTA+4,C'N'                                               
         CLI   DB.DBSELSTA+4,C'S'                                               
         BNE   *+8                                                              
         MVI   DB.DBSELSTA+4,C'M'                                               
BTUSOK   MVI   DB.DBBTYPE,C'U'                                                  
**       MVI   DBSELDAY,0                                                       
**       XC    DBSELTIM(4),DBSELTIM                                             
**       MVI   DBSELDUR,X'FF'      ALL DURATIONS                                
         MVI   DB.DBSELDAY,0                                                    
         XC    DB.DBSELTIM(4),DB.DBSELTIM                                       
****     MVI   PDDUROPT,C'Y'       INSTRUCTION REMOVED BY OPUP 4/13/10          
****                               BECAUSE IT BREAKS METHOD=SOURCE.             
****                               DON'T THINK IT'S NEEDED.                     
         MVI   DB.DBPRGDUR,C'Y'                                                 
         MVI   DB.DBBEST,C'A'                                                   
         MVI   DB.DBSELDUR,X'FF'                                                
         CLI   FORCEBTU,C'Y'       DOING NTI ADJUSTMENT                         
         BNE   BTUR04                                                           
*                                                                               
         CLI   DB.DBSELSTA+4,C'C'  CABLE                                        
         BNE   BTUR01                                                           
         LA    RE,TVQCDATE         YES - USE CABLE DATES                        
         LA    RF,6                                                             
         B     BTUR02                                                           
*                                                                               
BTUR01   ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NETVQCAL  TABLE FOR TVQ CALENDAR                       
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)         LENGTH OF TABLE ENTRY                        
*                                                                               
         USING NETVQD,RE                                                        
BTUR02   CLI   0(RE),X'FF'         NOT FOUND - ALLOW A MISS                     
         BE    BTURX                                                            
         CLC   DB.DBSELBK,NETVQEWK CURRENT OUTSIDE RANGE                        
         BH    BTUR03              GET NEXT                                     
         CLC   DB.DBSELBK,NETVQSWK TOTALLY OUTSIDE RANGE - ALLOW MISS           
         BL    BTURX                                                            
         MVC   DB.DBSELBK,NETVQMTH USE THE MONTH BOOK                           
         MVC   DB.DBACTBK,DB.DBSELBK                                            
         B     BTUR06                                                           
BTUR03   AR    RE,RF               NEXT ENTRY IN TABLE                          
         B     BTUR02                                                           
         DROP  RE                                                               
*                                                                               
BTUR04   CLI   FORCEBTU,C'I'       SETUP FOR IAG                                
         BNE   BTUR05                                                           
         MVI   DB.DBSELSTA+4,C'I'                                               
         OC    PDCODE,PDCODE       ESTIMATED IAG. CONVERT TO YEAR BOOK          
         BZ    BTUR04A                                                          
         GOTO1 DEFINE,DMCB,=C'UYEAR',AMYDBLK,HALF                               
         EDIT  HALF,(4,DUB)        YEAR YYYY                                    
         MVC   DUB+4(4),=C'0101'   DUMMY MONTH AND DAY                          
         GOTO1 NETWEEK,DMCB,DUB+2,GETDAY,ADDAY    YYMMDD TO NET WEEK            
         MVC   DB.DBSELBK(1),DMCB+4 YEAR NO.                                    
         MVI   DB.DBSELBK+1,0      KEY BOOK IS YY00                             
         MVC   DB.DBACTBK,DB.DBSELBK                                            
         MVC   DB.DBSELOPI,PDCODE                                               
         B     BTUR05C                                                          
BTUR04A  DS    0X                  ACTUAL IAG. CONVERT TO MNTHLY BK             
         GOTO1 NETUNBK,DMCB,(C'M',DB.DBSELBK),DUB,GETDAY,ADDAY,GETBROAD         
         PACK  DUB(8),DUB+2(2)                                                  
         CVB   R1,DUB                                                           
         STC   R1,DB.DBSELBK+1     MONTH BOOK                                   
         B     BTUR06                                                           
*                                                                               
BTUR05   CLI   FORCEBTU,C'O'       SETUP FOR OPTIMA                             
         BNE   BTUR06                                                           
         MVC   DB.DBFILE,=C'OPI'                                                
         OC    DB.DBSELBK,DB.DBSELBK                                            
         BNZ   *+10                                                             
         MVC   DB.DBSELBK,=AL2(JAN_03)                                          
*                                                                               
         CLI   DB.DBSELBK+1,12     OPI BOOKS ARE ON FILE AS YR/QTR#             
         BH    *+12                                                             
         MVI   DB.DBSELBK+1,1      Q1=WEEKS 1-12                                
         B     BTUR05C                                                          
         CLI   DB.DBSELBK+1,25                                                  
         BH    *+12                                                             
         MVI   DB.DBSELBK+1,2      Q2=WEEKS 13-25                               
         B     BTUR05C                                                          
         CLI   DB.DBSELBK+1,37                                                  
         BH    *+12                                                             
         MVI   DB.DBSELBK+1,3      Q3=WEEKS 26-37                               
         B     BTUR05C                                                          
         MVI   DB.DBSELBK+1,4      Q4=WEEKS >= 38                               
BTUR05C  DS    0X                                                               
*                                                                               
         MVI   DB.DBFUNCT,DBGETOPI                                              
         MVC   DB.DBSELOPI,PDCODE                                               
         CLI   DB.DBSELSTA+4,C'S'                                               
         BNE   *+12                                                             
         MVI   DB.DBSELSTA+4,C'M'                                               
         B     *+8                                                              
         MVI   DB.DBSELSTA+4,C'T'                                               
         CLI   FORCEBTU,C'I'                                                    
         BNE   *+8                                                              
         MVI   DB.DBSELSTA+4,C'I'                                               
*                                                                               
BTUR06   XC    DB.DBAQUART,DB.DBAQUART                                          
         XC    SVBTUAQ,SVBTUAQ                                                  
         L     RE,AMYIO2           USE MY IOAREA                                
         ST    RE,DB.DBAREC                                                     
         LHI   RF,2000                                                          
         XCEF                                                                   
*                                                                               
         GOTO1 DEMAND,DMCB,AMYDBLK,BTUHK                                        
*                                                                               
         OC    DB.DBAQUART,DB.DBAQUART                                          
         BNZ   XITS2                                                            
         CLI   GOTUTYPE,1                                                       
         BNE   XITS2                                                            
         MVC   DB.DBAQUART,SVBTUAQ                                              
BTURX    B     XITS2                                                            
         DROP  DB                                                               
*                                                                               
* BOOK TYPE U HOOK ROUTINE                                                      
*                                                                               
BTUHK    L     R3,AMYDBLK                                                       
         MVC   SVBTUAQ,8(R3)       SAVE A(QUARTER HOUR ELEMENT)                 
         MVI   GOTUTYPE,1                                                       
         BR    RE                                                               
         TITLE 'READ DEMOGRAPHIC MARKETS AND BUILD STATION TABLE'               
***********************************************************************         
*        CALLED FOR EACH ENTRY IN PODMKTL                             *         
*        STATIONS STORED IN PODNET                                    *         
***********************************************************************         
*                                                                               
RDDMKT   L     RE,APODNET                                                       
         CLI   0(RE),0                                                          
         BNE   RDDMX                                                            
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBAREC,AIO1                                                      
         MVC   DBFILE,PODBFIL                                                   
         MVI   DBFUNCT,DBGETMS                                                  
         MVC   DBSELSRC,PODBSRC                                                 
         MVI   DBSELMED,C'T'                                                    
         CLI   PODBMED,C'C'        MEDIA                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         CLI   PODBMED,C'W'        MEDIA                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'W'                                                    
         CLI   PODBMED,C'D'        MEDIA                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'D'                                                    
         CLI   PODBMED,C'O'        MEDIA                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'O'                                                    
         MVC   DBSELBK,PDSBOOK                                                  
         CLI   DBBTYPE,C'A'                                                     
         BNL   RDDM02                                                           
         CLI   PODBBTY,C'A'                                                     
         BL    *+10                                                             
         MVC   DBBTYPE,PODBBTY                                                  
*                                                                               
RDDM02   MVC   DBSELAGY,AGENCY                                                  
         MVC   DBSELRMK,PODMPAMK                                                
         MVC   CURRMRKT,PODMPAMK                                                
         CLI   PODBSRC,C'N'                                                     
         BNE   RDDM04                                                           
         SR    R3,R3                                                            
         ICM   R3,3,DBSELRMK                                                    
         SHI   R3,400                                                           
         STCM  R3,3,DBSELRMK                                                    
         STCM  R3,3,CURRMRKT                                                    
*                                                                               
RDDM04   CLI   PDQSPILL,C'N'                                                    
         BE    *+10                                                             
         MVC   DBSELMK,DBSELRMK                                                 
         L     R3,DBAREC                                                        
         XC    0(50,R3),0(R3)                                                   
         USING MLKEY,R3                                                         
         L     RF,APODNET                                                       
         ST    RF,CURRNET                                                       
         CLC   =C'SQAD',PODBEXT    IS THIS SQAD?                                
         BE    RDDM08              YES, DIFFERENT LOGIC                         
*                                                                               
         GOTO1 DEMAND,DMCB,DBLOCK,GETSTAT                                       
*                                                                               
RDDM06   L     RF,CURRNET                                                       
         MVI   0(RF),X'FF'         END NET TABLE                                
         MVC   DBLOCKA,DBLOCK                                                   
         B     RDDMX                                                            
*                                                                               
RDDM08   BAS   RE,GETSQAD                                                       
         B     RDDM06                                                           
*                                                                               
RDDMX    B     XITS2                                                            
         DROP  R2                                                               
*                                                                               
GETSTAT  NTR1                                                                   
         L     RF,CURRNET                                                       
         L     RE,APODNET                                                       
         LA    RE,LPODNET-PODNETL-1(RE)   CK ENOUGH SPACE                       
         CR    RF,RE                                                            
         BH    GETS06                                                           
* FOR LOCAL CABLE, FORCE TO ALWAYS GET COMBINED (AS IF SPILL=C)                 
         CLI   MLMEDIA,C'T'                                                     
         BNE   GETS01A                                                          
         CLI   MLSRC,C'F'          FUSION TOO                                   
         BE    GETS04                                                           
         CLI   MLSRC,C'N'                                                       
         BNE   *+8                                                              
         CLI   MLBTYP,C'C'                                                      
         BE    GETS04                                                           
         B     GETS01                                                           
GETS01A  CLI   MLMEDIA,C'O'        FOR CABLE OVERNIGHTS                         
         BNE   GETS01              ALLOW SPILLS                                 
         CLI   MLBTYP,C'C'                                                      
         BE    GETS04                                                           
         CLI   MLBTYP,C'U'                                                      
         BE    GETS04                                                           
         CLI   MLBTYP,C'W'                                                      
         BE    GETS04                                                           
         CLI   MLBTYP,C'Z'                                                      
         BE    GETS04                                                           
*                                                                               
GETS01   CLI   PDQSPILL,C'C'       COMBINED SPILL AND HOME                      
         BE    GETS04              YES, USE BOTH                                
         CLI   PDQSPILL,C'S'       USE JUST SPILL                               
         BNE   GETS02              NO, ELIMINATE SPILLS                         
         OC    MLKMKT,MLKMKT       JUST USE SPILLS                              
         BZ    GETS06                                                           
         B     GETS04                                                           
*                                                                               
GETS02   OC    MLKMKT,MLKMKT       DON'T USE SPILLS                             
         BNZ   GETS06                                                           
*                                                                               
GETS04   CLI   MLSRC,C'F'                                                       
         BNE   *+14                                                             
         MVC   0(5,RF),DBACTSTA                                                 
         B     GETS05                                                           
         TM    MLSTAT,X'F0'        DON'T USE MARKET STATIONS                    
         BO    GETS06                                                           
         MVC   0(5,RF),MLSTAT      COPY THE STATION                             
*                                                                               
GETS05   CLC   =C'IUN',PODBFIL                                                  
         BE    *+10                                                             
         CLC   =C'PAV',PODBFIL                                                  
         BE    *+10                                                             
         CLC   =C'TP ',PODBFIL                                                  
         BE    *+8                                                              
         MVI   5(RF),C'A'          STANDARD BOOK TYPE                           
         MVC   6(2,RF),MLKMKT      COPY SPILL MARKET                            
         L     RF,CURRNET          NEXT NETWORK ENTRY                           
         LA    RF,PODNETL(RF)                                                   
         ST    RF,CURRNET                                                       
*                                                                               
GETS06   XIT1                                                                   
***********************************************************************         
*        CALLED FOR EACH COUNTY CODE. GET ALL STATIONS FOR COUNTY     *         
*        STATIONS STORED IN PODNET                                    *         
***********************************************************************         
*                                                                               
RDCNTY   XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBAREC,AIO1                                                      
         MVC   DBFILE,PODBFIL                                                   
         MVI   DBFUNCT,DBGETMS                                                  
         MVC   DBSELSRC,PODBSRC                                                 
         MVI   DBSELMED,C'U'                                                    
         MVC   DBSELBK,PDSBOOK                                                  
         MVC   DBBTYPE,PODBBTY                                                  
*                                                                               
RDCN02   MVC   DBSELAGY,AGENCY                                                  
         MVC   DBSELMK,PODMPAMK      COUNTY NO                                  
         MVC   DBSELRMK,PODMPAMK     COUNTY NO                                  
         MVC   CURRMRKT,PODMPAMK                                                
*                                                                               
         L     R3,DBAREC                                                        
         XC    0(50,R3),0(R3)                                                   
         USING MLKEY,R3                                                         
         L     RF,APODNET                                                       
         ST    RF,CURRNET                                                       
*                                                                               
         GOTO1 DEMAND,DMCB,DBLOCK,GETCSTA                                       
*                                                                               
RDCN06   L     RF,CURRNET                                                       
         MVI   0(RF),X'FF'         END NET TABLE                                
         MVC   DBLOCKA,DBLOCK                                                   
         B     RDCNX                                                            
*                                                                               
RDCNX    B     XITS2                                                            
*                                                                               
* GET COUNTY COVERAGE STATIONS                                                  
GETCSTA  NTR1                                                                   
         L     RF,CURRNET                                                       
         L     RE,APODNET                                                       
         LA    RE,LPODNET-PODNETL-1(RE)                                         
         CR    RF,RE                                                            
         BH    GETCS06                                                          
*                                                                               
GETCS04  DS    0H                                                               
*ETCS04  TM    MLSTAT,X'F0'        DON'T USE MARKET STATIONS                    
*        BO    GETCS06             ?                                            
         MVC   0(5,RF),MLSTAT      COPY THE STATION                             
         MVC   5(1,RF),MLBTYP      STANDARD BOOK TYPE                           
         MVC   6(2,RF),MLKMKT      COPY COUNTY NO                               
         L     RF,CURRNET          NEXT NETWORK ENTRY                           
         LA    RF,PODNETL(RF)                                                   
         ST    RF,CURRNET                                                       
*                                                                               
GETCS06  XIT1                                                                   
*                                                                               
GETSQAD  NTR1                                                                   
         L     RF,CURRNET                                                       
         L     RE,APODNET                                                       
         LA    RE,LPODNET-PODNETL-1(RE)                                         
         CR    RF,RE                                                            
         BH    GETSQX                                                           
         ICM   R1,3,DBSELRMK                                                    
         CVD   R1,DUB                                                           
         UNPK  0(4,RF),DUB                                                      
         OI    3(RF),X'F0'                                                      
         MVI   4(RF),C'T'                                                       
         MVI   5(RF),C'S'          BOOKTYPE = S                                 
         CLC   =C'SQADR',PODBEXT   SQAD RADIO                                   
         BNE   *+8                                                              
         MVI   4(RF),C'R'                                                       
         CLC   =C'SQADH',PODBEXT   SQAD HISPANIC                                
         BNE   *+8                                                              
         MVI   5(RF),C'R'                                                       
         MVC   6(2,RF),DBSELRMK    COPY THE MARKET                              
*                                                                               
         L     RF,CURRNET          NEXT NETWORK ENTRY                           
         LA    RF,8(RF)                                                         
         ST    RF,CURRNET                                                       
*                                                                               
GETSQX   XIT1                                                                   
         DROP  R3                                                               
         TITLE 'READ MARKETS IN NON-SID GROUP && CONVERT ALPHA CODE'            
***********************************************************************         
*        STORE ALPHA CODE(S) IN PODMKTL                               *         
***********************************************************************         
*                                                                               
RDRMKT   ICM   R5,15,PODMPTR                                                    
         LA    R4,KEY                                                           
         USING MKTRECD,R4          MARKET RECORD ON STATION FILE                
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,PODBMED                                                  
         CLI   MKTKMED,C'O'                                                     
         BNE   *+8                                                              
         MVI   MKTKMED,C'T'        OVERNIGHT READS NSI MKT RECORDS              
         MVC   MKTKAGY,AGENCY                                                   
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,PDSIDMKT                                                    
         CVD   R1,DUB                                                           
         UNPK  MKTKMKT(4),DUB                                                   
         OI    MKTKMKT+3,X'F0'                                                  
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATIO',KEY,AMYIO                     
         CLI   8(R1),0                                                          
         BNE   RDRMX                                                            
         L     R4,AMYIO                                                         
         CLC   MKTREC(MKTKEYLQ),KEYSAVE                                         
         BNE   RDRMX                                                            
*                                                                               
         LA    R0,9                MAX NUMBER OF ALPHA CODES                    
         LA    R3,MKTALST          GET ALPHA LIST                               
*                                                                               
         CLI   0(R3),0             ANY ALPHA MARKETS                            
         BNE   RDRM02              YES                                          
         CLI   PODBSRC,C'N'        CHECK NSI                                    
         BNE   RDRM01                                                           
         SR    R1,R1               SET TO SOURCE 1 CODE                         
         ICM   R1,3,MKTRSM1                                                     
         BZ    RDRMX               SKIP ZERO NSI MARKETS                        
         B     RDRM01A                                                          
RDRM01   CLI   PODBSRC,C'A'        CHECK ARB                                    
         BNE   RDRM02                                                           
         SR    R1,R1                                                            
         ICM   R1,3,MKTRSM2        SET TO SOURCE 2 CODE                         
         BZ    RDRMX               SKIP ZERO ARB MARKETS                        
RDRM01A  CLI   PODBSRC,C'N'        NSI MARKET NEEDS ADJUSTMENT                  
         BNE   *+8                                                              
         AHI   R1,400                                                           
         STCM  R1,3,0(R5)                                                       
         LA    R5,2(R5)                                                         
         MVI   0(R5),X'FF'                                                      
         B     RDRMX                                                            
*                                                                               
RDRM02   CLI   0(R3),0             NO MORE ALPHA CODES                          
         BE    RDRMX                                                            
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CTDMREC,R2                                                       
         MVI   CTDMKTYP,CTDMKTEQ   SET UP KEY                                   
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVI   CTDMKMED,C'T'       MEDIA                                        
         CLI   PODBMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   CTDMKMED,C'C'       MEDIA                                        
         MVC   CTDMKSRC,PODBSRC    SOURCE (A/N)                                 
         CLI   PODBEXT+4,C'A'      EMI-A/NMI-A USES ARBITRON MPA                
         BNE   *+8                                                              
         MVI   CTDMKSRC,C'A'                                                    
         MVC   CTDMKMKT,0(R3)      MARKET NAME                                  
         MVC   CTDMKBKT,PODBBTY                                                 
         CLI   CTDMKBKT,0                                                       
         BNE   *+14                                                             
         L     RE,APODNET                                                       
         MVC   CTDMKBKT,5(RE)      BOOK TYPE                                    
         CLI   CTDMKBKT,C'A'       DEFAULT FOR NON-SPOT                         
         BE    RDRM04                                                           
         CLI   CTDMKBKT,0          DEFAULT FOR SPOT                             
         BNE   *+8                                                              
*                                                                               
RDRM04   MVI   CTDMKBKT,X'FF'      DEFAULT BOOK TYPE                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI  '),=C'CTFILE  ',KEY,KEY,0             
         CLC   KEY+1(22),KEYSAVE+1 CAN'T FIND IT? 1ST BYTE DESTROYED            
*        BNE   RDRMX               BAD ONE                                      
         BE    RDRM05                                                           
         CLI   KEYSAVE+CTDMKBKT-CTDMREC,X'FF' LOOKING FOR DEFAULT BTYP?         
         BE    RDRMX               BAD ONE                                      
         MVC   KEY,KEYSAVE         BTYP NOT FOUND. GO READ DEFAULT              
         MVI   CTDMKTYP,CTDMKTEQ   RESTORE FIRST BYTE                           
         B     RDRM04                                                           
*                                                                               
RDRM05   MVC   0(2,R5),CTDMKNUM    GOT IT, SAVE MARKET NUMBER                   
         CLI   PODBEXT+4,C'A'      EMI-A/NMI-A USES ARB MPA                     
         BE    RDRM06                                                           
         CLI   PODBSRC,C'N'        NSI MARKET NEEDS ADJUSTMENT                  
         BNE   RDRM06                                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,0(R5)                                                       
         AHI   R1,400                                                           
         STCM  R1,3,0(R5)                                                       
*                                                                               
RDRM06   LA    R5,2(R5)                                                         
         MVI   0(R5),X'FF'                                                      
         LA    R3,3(R3)                                                         
         BCT   R0,RDRM02                                                        
*                                                                               
RDRMX    STCM  R5,15,PODMPTR       SAVE FOR NEXT TIME IN                        
         B     XITS2                                                            
         DROP  R2                                                               
         TITLE 'FILTER BY DATES REQUESTED (EXACT OPTION)'                       
*                                                                               
FILRDAT  L     R4,APDREQDT         SEE IF ANYTHING IN TABLE                     
         CLI   0(R4),X'FF'                                                      
         BE    FILRYES             NO DATA, ACCEPT EVERYTHING                   
*                                                                               
         GOTO1 UNWEEK,DMCB,PDSBOOK,DUB                                          
*                                                                               
         SR    R4,R4                                                            
         IC    R4,PDDAY            COMPUTING DATE - GET DAY NO IN R2            
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         LA    R4,1                M-F=MON                                      
         CHI   R4,8                                                             
         BL    *+8                                                              
         LA    R4,1                M-S=MON                                      
         BCTR  R4,0                CHANGE TO 0-6                                
         GOTO1 ADDAY,DMCB,DUB,WORK,(R4)                                         
*                                                                               
         L     R4,APDREQDT         GET TABLE OF REQUESTED DATES                 
*                                                                               
FILR02   CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    FILRNO              YES, DATE CAN'T BE GOOD                      
         CLC   WORK(6),0(R4)       LOWER THAN THE START?                        
         BL    FILR04              YES, GET NEXT TABLE ENTRY                    
         CLC   WORK(6),6(R4)       NO, EQUAL OR LOWER THAN END?                 
         BNH   FILRYES             YES, DATE GOOD                               
*                                                                               
FILR04   LA    R4,12(R4)           NO, TRY AGAIN                                
         B     FILR02                                                           
*                                                                               
FILRNO   SR    R4,R4                                                            
*                                                                               
FILRYES  LTR   R4,R4                                                            
FILRX    B     XITS2                                                            
         TITLE 'SAVE TIMES AND DATES BASED ON PDF FILTER'                       
*                                                                               
SAVPROG  ICM   R2,15,PRGTPTR       GET NEXT SPACE IN TABLE                      
*                                                                               
         L     RE,APDPDFLT         ARE WE FILTERING BY PDF?                     
         OC    0(L'PDPDFLT,RE),0(RE)                                            
         BZ    SAVPREQ             NO, EXIT                                     
*                                                                               
         GOTO1 UNWEEK,DMCB,PDSBOOK,DUB                                          
*                                                                               
         SR    R4,R4                                                            
         IC    R4,PDDAY            COMPUTING DATE - GET DAY NO IN R2            
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         LA    R4,1                M-F=MON                                      
         CHI   R4,8                                                             
         BL    *+8                                                              
         LA    R4,1                M-S=MON                                      
         BCTR  R4,0                CHANGE TO 0-6                                
         GOTO1 ADDAY,DMCB,DUB,WORK,(R4)                                         
*                                                                               
         MVC   0(6,R2),WORK        SAVE AS START DATE                           
         MVC   6(6,R2),WORK        AND END DATE                                 
         MVC   12(4,R2),PDTIME     SAVE THE TIMES ALSO                          
         LA    R2,16(R2)           GET NEXT SPOT                                
         STCM  R2,15,PRGTPTR       SAVE THE ADDRESS FOR NEXT TIME               
*                                                                               
SAVPRNE  LA    R1,1                NOT EQUAL IF PDF IN USE                      
         B     SAVPX                                                            
*                                                                               
SAVPREQ  SR    R1,R1               EQUAL IF PDF NOT USED                        
*                                                                               
SAVPX    LTR   R1,R1                                                            
         B     XITS2                                                            
         TITLE 'FILTER DATES BASED ON PROGRAM DATE/TIMES TABLE (PDF)'           
*                                                                               
FILPDAT  L     R4,APDPRGTT         SEE IF ANYTHING IN TABLE                     
         CLI   0(R4),X'00'                                                      
         BE    FILPYES             NO DATA, ACCEPT EVERYTHING                   
*                                                                               
         GOTO1 UNWEEK,DMCB,PDSBOOK,DUB                                          
*                                                                               
         SR    R4,R4                                                            
         IC    R4,PDDAY            COMPUTING DATE - GET DAY NO IN R2            
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         LA    R4,1                M-F=MON                                      
         CHI   R4,8                                                             
         BL    *+8                                                              
         LA    R4,1                M-S=MON                                      
         BCTR  R4,0                CHANGE TO 0-6                                
         GOTO1 ADDAY,DMCB,DUB,WORK,(R4)                                         
*                                                                               
         L     R4,APDPRGTT         GET TABLE OF DATES/TIMES                     
*                                                                               
FILP02   CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    FILPNO              YES, DATE CAN'T BE GOOD                      
         CLC   WORK(6),0(R4)       LOWER THAN THE START?                        
         BL    FILP04              YES, GET NEXT TABLE ENTRY                    
         CLC   WORK(6),6(R4)       NO, EQUAL OR LOWER THAN END?                 
         BH    FILP04              NO, GET NEXT                                 
         OC    12(2,R4),12(R4)     YES, ARE THERE TIMES?                        
         BZ    FILPYES             NO, DATE IS GOOD                             
         CLC   PDTIME(2),12(R4)    YES, MUST BE WITHIN RANGE                    
         BL    FILP04              TOO LOW                                      
         CLC   PDTIME(2),14(R4)                                                 
         BL    FILPYES             DATE GOOD                                    
*                                                                               
FILP04   LA    R4,16(R4)           NO, TRY AGAIN                                
         B     FILP02                                                           
*                                                                               
FILPNO   SR    R4,R4                                                            
*                                                                               
FILPYES  LTR   R4,R4                                                            
FILPX    B     XITS2                                                            
         EJECT                                                                  
*                                                                               
* SPECIAL PROCESSING FOR CABLE NAD                                              
*                                                                               
SETCNAD  DS    0H                                                               
         CLC   =C'CNAW ',PDSOURCE  CABLE NAD - PROGRAM DATA                     
         BE    *+10                                                             
         CLC   =C'CNAD ',PDSOURCE                                               
         BNE   SETCN5                                                           
         CLI   DBFUNCT,DBGETNTI    ACTUALLY READ TP DATA FOR STATIONS           
         BNE   SETCN5              W/O PROGRAM DATA                             
         MVI   PDMULTRD,C'Y'                                                    
         L     RE,DBAQUART                                                      
         CLI   0(RE),X'0F'                                                      
         BNE   *+8                                                              
         MVI   PDMULTRD,C'N'                                                    
*                                                                               
*NEH00   CLC   =C'CNAWT',PDSOURCE   FOR TIME PERIOD CABLE NAD AND MVGO          
*        BE    *+10                                                             
*        CLC   =C'CNADT',PDSOURCE                                               
*        BNE   SETCNX                                                           
SETCN5   CLC   =C'CNAW',PDSOURCE    CABLE NAD - TIME PERID DATA                 
         BE    *+10                                                             
         CLC   =C'CNAD',PDSOURCE                                                
         BNE   SETCNX                                                           
         CLI   DBFUNCT,DBGETDEM     TP READ                                     
         BNE   SETCNX                                                           
         MVI   PDMULTRD,C'Y'        MAKE SURE NOT TO WEIGHT MULTI-READS         
         L     R5,DBAREC                                                        
         USING PRKEY,R5                                                         
         CLI   PRSTYP,0             FIRST RECORD (FOR SPANNING RECORDS)         
         BNE   *+8                                                              
         MVI   PDMULTRD,C'N'                                                    
*                                                                               
SETCNX   B     XITS2                                                            
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*PUTEXT - ADD NEW EXTENTION TO LIST STARTING AT DBEXTEND                        
*         DMCB+4 HOLDS ADDRESS OF EXTENSION TO ADD                              
*                                                                               
PUTEXT   DS    0H                                                               
         GOTOR SUBR02,PARAS,('CHKEXTE',(RC)),(R2)   CHCK NOT ALREADY IN         
         BE    PUTEXTX                                                          
*                                                                               
         ICM   RE,15,DBEXTEND     OLD START OF THE LIST                         
         MVC   DBEXTEND,DMCB+4    ADD NEW EXTENT TO BEGINNING OF LIST           
         ICM   RF,15,DBEXTEND     NEW START OF THE LIST                         
         STCM  RE,15,4(RF)        LINK THEM                                     
PUTEXTX  B     XITS2                                                            
         EJECT                                                                  
*                                                                               
*CHKEXT - MAKE SURE EXTEND IS NOT ALREADY IN THE LIST                           
*         DMCB+4 HOLDS ADDRESS OF EXTENSION TO CHECK                            
CHKEXT   DS    0H                                                               
         ICM   RF,15,DMCB+4       EXTEND TO CHECK                               
         ICM   RE,15,DBEXTEND     START OF EXISTING LIST                        
         BZ    CKEXTNO            NOTHING IN YET                                
         USING DBDQXTND,RE                                                      
CKEXT5   CLC   DBDQXID,0(RF)                                                    
         BE    CHKEXTX            ALREADY THERE. EXIT WITH CC=EQ                
         ICM   RE,15,4(RE)                                                      
         BZ    CKEXTNO                                                          
         B     CKEXT5                                                           
*                                                                               
CKEXTNO  CHI   RB,0               EXTEND NOT FOUND. SET CC=NEQ                  
CHKEXTX  B     XITS2                                                            
         EJECT                                                                  
*                                                                               
*GETEXT - FIND EXTENSION IN LIST STARTING AT DBEXTEND                           
*         DMCB+4 HOLDS ADDRESS OF EXTENSION ID                                  
*         UPON RETURN, DMCB WILL HOLD ADDRESS OF EXTENTION                      
*                                                                               
GETEXT   DS    0H                                                               
         ICM   RE,15,DBEXTEND                                                   
GETEXT5  BZ    NTFOUND            GO THROUGH LIST STARTING AT DBEXTEND          
         L     RF,DMCB+4                                                        
         CLC   0(4,RE),0(RF)      AND LOOK FOR ID                               
         BE    GOTEXT                                                           
         ICM   RE,15,4(RE)                                                      
         B     GETEXT5                                                          
*                                                                               
GOTEXT   ST    RE,DMCB            EXTENSION FOUND, PASS BACK ADDRESS            
         B     GETEXTX                                                          
*                                                                               
NTFOUND  XC    DMCB,DMCB          EXTENSION NOT FOUND                           
         B     GETEXTX                                                          
*                                                                               
GETEXTX  B     XITS2                                                            
         EJECT                                                                  
*                                                                               
* CK IF USER REQUESTED ANY MOVIE GOER DEMOS                                     
*                                                                               
ANYMVG   DS    0H                                                               
         L     R3,PDADEMTB                                                      
         USING PDDMBUFF,R3                                                      
         LA    RE,PODDEMO                                                       
ANYMVG5  CLI   0(RE),X'FF'                                                      
         BE    MVGN                                                             
         CLI   0(RE),167                                                        
         BE    MVGY                                                             
         CLI   0(RE),168                                                        
         BE    MVGY                                                             
         CLI   0(RE),169                                                        
         BE    MVGY                                                             
         AH    RE,LNDEMCOD                                                      
         B     ANYMVG5                                                          
*                                                                               
MVGY     CR    RB,RB                                                            
         B     ANYMVGX                                                          
MVGN     CHI   RB,0                                                             
         B     ANYMVGX                                                          
*                                                                               
ANYMVGX  B     XITS2                                                            
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*SETDBDT - MOVE DAYS AND TIME INTO DBBLOCK                                      
*                                                                               
SETDBDT  DS    0H                                                               
         MVI   DBBEST,0                                                         
         MVI   ALLDT,0                                                          
         MVC   DBPRGDUR,PDDUROPT                                                
         CLI   PDTRNKST,C'Y'                                                    
         BE    SETDB5                                                           
         CLI   DBPRGDUR,C'Y'       GET ALL DATA IF DOING MINUTE WEIGHT          
         BNE   *+8                                                              
         MVI   DBBEST,C'A'                                                      
SETDB5   MVC   DBDAYOPT,DAYSOPT    CHECK INDIVIDUAL DAYS                        
         MVC   DBSELDAY,0(R5)                                                   
         CLI   0(R5),X'FF'         CHECK DAYS = ALL                             
         BNE   SETDB6                                                           
         MVI   DBSELDAY,X'7F'                                                   
         OI    ALLDT,X'02'                                                      
         B     SETDB8                                                           
*                                                                               
SETDB6   CLI   VAROPT,C'Y'         USE VARIOUS DAYS                             
         BE    SETDB8                                                           
         CLI   DBSELDAY,X'7C'      ASK FOR EXACT MATCH FOR M-F                  
         BE    *+12                                                             
         CLI   DBSELDAY,X'7F'      AND FOR M-S                                  
         BNE   SETDB8                                                           
         MVI   DBBEST,C'L'                                                      
*                                                                               
SETDB8   MVC   DBSELTIM(4),1(R5)                                                
         CLI   1(R5),X'FF'         CHECK TIME = ALL                             
         BNE   SETDB10                                                          
         OI    ALLDT,X'01'                                                      
         MVC   DBSELTIM(2),=H'600' SET ALL TIME TO 6-545                        
         MVC   DBSELTIM+2(2),=H'545'                                            
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   *+10                                                             
         MVC   DBSELTIM+2(2),=H'559'  6-5:59 FOR MXM                            
         CLI   7(R2),C'R'          RADIO STARTS AT 5AM                          
         BNE   SETDB10                                                          
         MVC   DBSELTIM(2),=H'500' SET ALL TIME TO 5-445                        
         MVC   DBSELTIM+2(2),=H'445'                                            
*                                                                               
*                                  SEE IF 15 MINUTE SHOWS S/B INCLUDED          
SETDB10  CLI   FTNYOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   DBSELDUR,X'FF'      GET ALL DURATIONS                            
         CLI   FTNYOPT,C'O'                                                     
         BNE   *+8                                                              
         MVI   DBSELDUR,X'FE'      GET ONLY FIFTEEN MINUTE DURATIONS            
         CLI   MINDUR,0                                                         
         BE    *+10                                                             
         MVC   DBSELDUR,MINDUR                                                  
*                                                                               
         B     XITS2                                                            
         EJECT                                                                  
*                                                                               
*SETCDT  - FOR CABLE, WE SEND DAY/TIME TABLE TO DEMAND (OPTIMIZATION)           
*          THE REQUEST DAY AND TIME (DBSELDAY,DBSELTIM) ARE NOT SET             
*          SET THEM FROM DAY/TIME TABLE USING THE INDEX SET IN DEMAND           
*          THIS IS IN CASE WE REFERENCE THEM (EX:FOR CNAD WEIGHTING)            
*                                                                               
SETCDT   DS    0H                                                               
         CLI   ODYTSET,C'Y'         WAS EXTEND SET?                             
         BNE   SETCDTX                                                          
         GOTOR SUBR02,DMCB,('GETEXTE',(RC)),=C'ODYT'                            
         ICM   RE,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'0'                SHOULD BE THERE FOR CABLE                    
         USING DBXTLD,RE                                                        
         ZIC   RF,DBXTLIDX                                                      
         MHI   RF,5                                                             
         LA    RE,PODDAYTM         USE ORIGINAL TABLE BECAUSE                   
         AR    RE,RF                                                            
         MVC   DBSELDAY,0(RE)                                                   
         MVC   DBSELTIM,1(RE)                                                   
SETCDTX  B     XITS2                                                            
*                                                                               
         DROP  RB,RA                                                            
         EJECT                                                                  
       ++INCLUDE DETVQCDAT                                                      
*                                                                               
* CONVERT WEEK BACK INTO DATE                                                   
*                                                                               
UNWEEK   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R1,0(R2)            GENERATE JAN0                                
         EDIT  (R1),(2,0(R3))                                                   
         OI    0(R3),X'F0'                                                      
         MVC   2(4,R3),=C'0101'                                                 
         GOTO1 GETDAY,DMCB,0(R3),DMCB+12                                        
         ZIC   R0,0(R1)            BACK UP TO PR                                
         LA    R4,1                                                             
         SR    R4,R0                                                            
         GOTO1 ADDAY,DMCB,0(R3),DMCB+12,(R4)                                    
         ZIC   R5,1(R2)                                                         
         BCTR  R5,0                                                             
         MHI   R5,7                                                             
         ST    R5,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,DMCB+12,0(R3)                                         
*                                                                               
UNWEX    XIT1                                                                   
         LTORG                                                                  
***********************************************************************         
* CALL COMINTER TO PUT REQUEST FOR LOOKUP                                       
***********************************************************************         
PUTCOM   NTR1  BASE=*,LABEL=*                                                   
         TM    OPTFLAG1,COMSRCQ+COMDEMOQ   COMSCORE SOURCE/DEMO?                
         JNO   PUTCOMX                                                          
         CLI   PODPASS,C'1'        COMSCORE?                                    
         JNE   PUTCOMX                                                          
         TM    CSFLAG,CSFPUTQ      PUT RECORDS TO DATASET ALREADY?              
         JO    PUTCOMX                                                          
*                                                                               
         LA    R3,CSRNEXT          COMINTER PARAM BLOCK                         
         USING DBCSRND,R3                                                       
         ST    R3,ADBCSREX         A(CSRN EXTEND BLOCK)                         
*                                                                               
         MVC   DBCSRID,=C'CSRN'                                                 
         MVI   DBCSRPRE,C'1'       PRECISION                                    
         CLI   PRECOPT,C'Y'                                                     
         JNE   *+8                                                              
         MVI   DBCSRPRE,C'2'                                                    
*                                                                               
         MVC   DBCSRDNL,APDNTDMS   SET A(COMSCORE DEMO NAMES)                   
*                                                                               
         L     RF,PDADEMTB                                                      
         USING PDDMBUFF,RF                                                      
         LA    RE,PODDEMO                                                       
         ST    RE,DBCSRRDL         SET A(REQUESTED DEMO LIST)                   
         LA    RE,PDDEMOS                                                       
         ST    RE,DBCSRODV         SET A(OUTPUT DEMO VALUES)                    
         DROP  RF                                                               
*                                                                               
         OI    DBCSRFLG,DBCSRFPQ   PROGRAM AVERAGE                              
         TM    PDCALOPT,PDCALNTI   USE NEILSEN DATES?                           
         JZ    *+8                                                              
         OI    DBCSRFLG,DBCSRFNQ   YES                                          
*                                                                               
         MVI   DBDEMTYP,C'4'       4 BYTE DEMO CATEGORIES                       
*                                                                               
         OC    DBEXTEND,DBEXTEND   ANY EXTENSIONS?                              
         JNZ   *+12                                                             
         ST    R3,DBEXTEND                                                      
         J     PCOM20                                                           
*                                                                               
         L     RF,DBEXTEND         FIND FIRST AVAILABLE EXTENSION               
PCOM10   CLC   =C'CSRN',0(RF)      ALREADY ADDED IT?                            
         JE    PCOM20                                                           
         OC    4(4,RF),4(RF)       A(NEXT EXTENSION)                            
         JZ    *+12                                                             
         ICM   RF,15,4(RF)                                                      
         J     PCOM10                                                           
         STCM  R3,15,4(RF)         SET EXTENSION ADDRESS                        
*                                                                               
* LOOP THROUGH NETWORKS, SOURCE/BOOKS, DAY/TIMES, VIEWING TYPES                 
* AND CALL COMINTER FOR EACH COMBINATION                                        
*                                                                               
PCOM20   L     R2,APODNET          LOOP THROUGH NETWORKS                        
PCOM22   CLI   0(R2),X'FF'                                                      
         JE    PUTCOMX                                                          
*                                                                               
         MVC   DBCSRNET(4),0(R2)   SET NETWORK                                  
         CLI   10(R2),C'X'         COMSCORE NETWORK?                            
         JNE   *+10                                                             
         MVC   DBCSRNET(10),0(R2)                                               
*                                                                               
         CLC   =C'ZZZ',0(R2)                                                    
         JNE   PCOM24                                                           
         MVI   DBCSRNET+3,C'-'                                                  
         MVC   DBCSRNET+4(1),4(R2) ZZZ-T/ZZZ-C                                  
         J     PCOM30                                                           
*                                                                               
PCOM24   XC    WORK,WORK                                                        
         MVC   WORK(L'DBSELSTA),0(R2)        DEFAULT NEILSEN NETWORK            
         GOTO1 =V(COMSTACN),DMCB,(1,WORK),WORK+10   GET COMSCORE NET            
         JNE   *+10                                                             
         MVC   DBCSRNET,WORK+10    SET COMSCORE NETWORK                         
*                                                                               
         USING PODBD,R4                                                         
PCOM30   L     R4,APODBKL          LOOP THROUGH SOURCE/BOOKS                    
PCOM32   CLI   0(R4),X'FF'         END OF SOURCE/BOOKS?                         
         JE    PCOM80                                                           
         CLC   =C'COM',PODBEXT     COMSCORE SOURCE?                             
         JNE   PCOM70                                                           
         CLI   EXACTOPT,C'Y'       USE EXACT DATES?                             
         JNE   PCOM34                                                           
*                                                                               
         OC    PODSDTE,PODSDTE     DATE RANGE ENTERED?                          
         JZ    PCOM34                                                           
         MVC   DBCSRSD,PODSDTE     SET START DATE                               
         MVC   DBCSRED,PODSDTE     SET END = START                              
*                                                                               
         AHI   R4,PODBLNQ                                                       
         CLC   =C'COM',PODBEXT     COMSCORE SOURCE?                             
         JNE   PCOM40                                                           
         OC    PODEDTE,PODEDTE                                                  
         JZ    PCOM40                                                           
         MVC   DBCSRED,PODEDTE     SET END DATE                                 
         J     PCOM40                                                           
*                                                                               
PCOM34   GOTOR UNWEEK,DMCB,PODBBKS,DUB      CONVERT START WEEK TO DATE          
         GOTO1 DATCON,DMCB,(0,DUB),(3,DBCSRSD)                                  
         GOTO1 ADDAY,DMCB,(C'D',DUB),WORK,6      DEFAULT END DATE TO            
         GOTO1 DATCON,DMCB,(0,WORK),(3,DBCSRED)  6 DAYS LATER                   
*                                                                               
         AHI   R4,PODBLNQ                                                       
         CLC   =C'COM',PODBEXT     COMSCORE SOURCE?                             
         JNE   PCOM40                                                           
         GOTOR UNWEEK,DMCB,PODBBKS,DUB      CONVERT END WEEK TO DATE            
         GOTO1 DATCON,DMCB,(0,DUB),(3,DBCSRED)                                  
*                                                                               
PCOM40   LA    R5,PODDAYTM         LOOP THROUGH DAY/TIMES                       
PCOM42   CLI   0(R5),X'99'         END OF DAYS/TIMES?                           
         JE    PCOM70              YES - GET NEXT SOURCE/BOOK                   
         MVC   DBCSRDAY,0(R5)      SET DAY                                      
         MVC   DBCSRST,1(R5)       SET START TIME                               
         MVC   DBCSRET,3(R5)       SET END TIME                                 
*                                                                               
         CLI   0(R5),X'FF'         ALL DAYS?                                    
         JNE   *+8                                                              
         MVI   DBCSRDAY,X'7F'      SET DAY                                      
*                                                                               
         CLI   1(R5),X'FF'         ALL TIMES?                                   
         JNE   PCOM50                                                           
         MVC   DBCSRST,DBSELTIM    START TIME OF DAY                            
         MVC   DBCSRET,DBSELTIM+2  END TIME OF DAY                              
*                                                                               
PCOM50   LA    R7,PDVIEWS          LOOP THROUGH VIEWING TYPES                   
         OC    PDVIEWS,PDVIEWS     ANY VIEWING TYPES?                           
         JNZ   *+8                                                              
         MVI   1(R7),X'FF'         FAKE IT OUT FOR LIVE ONLY                    
PCOM52   CLI   0(R7),X'FF'                                                      
         JE    PCOM60                                                           
         MVC   DBCSRVT,=C'RL'      DEFAULT TO LIVE                              
         CLI   0(R7),0                                                          
         JE    PCOM54                                                           
         CLI   0(R7),DBXLCLQ                                                    
         JE    PCOM54                                                           
         CLI   0(R7),DBXLCLCQ      PAV LIVE COMMERCIAL                          
         JNE   *+14                                                             
         MVC   DBCSRVT,=C'RC'                                                   
         J     PCOM54                                                           
         CLI   0(R7),DBXLCL3Q      PAV LIVE+3 COMMERCIAL                        
         JNE   *+14                                                             
         MVC   DBCSRVT,=C'R3'                                                   
         J     PCOM54                                                           
         CLI   0(R7),DBXLCL7Q      PAV LIVE+7 COMMERCIAL                        
         JNE   PCOM56                                                           
         MVC   DBCSRVT,=C'R7'                                                   
*                                                                               
PCOM54   TM    OPTFLAG1,COMEPQ     EPISODE INFO REQUESTED?                      
         JZ    *+8                                                              
         OI    DBCSRFLG,DBCSRFEQ                                                
*                                                                               
         GOTO1 VCOMINTR,DMCB,=C'PUT',DBLOCK                                     
         OI    CSFLAG,CSFPUTQ                                                   
*                                                                               
PCOM56   AHI   R7,1                GET NEXT VIEWING TYPE                        
         J     PCOM52                                                           
*                                                                               
PCOM60   AHI   R5,5                GET NEXT DAY/TIME                            
         J     PCOM42                                                           
*                                                                               
PCOM70   AHI   R4,PODBLNQ          GET NEXT SOURCE/BOOK                         
         J     PCOM32                                                           
*                                                                               
PCOM80   AHI   R2,PODNETL          GET NEXT NETWORK                             
         J     PCOM22                                                           
         DROP  R3,R4                                                            
*                                                                               
PUTCOMX  J     XIT                                                              
         LTORG                                                                  
***********************************************************************         
* CALL COMINTER TO GET DEMOS AND SET PDDEMOS                                    
***********************************************************************         
GETCOM   NTR1  BASE=*,LABEL=*                                                   
         TM    OPTFLAG1,COMSRCQ+COMDEMOQ   COMSCORE SOURCE/DEMO?                
         JNO   GETCOMX                                                          
         CLI   PODPASS,C'2'        COMSCORE?                                    
         JNE   GETCOMX                                                          
*                                                                               
         LA    R3,CSRNEXT          COMINTER PARAM BLOCK                         
         USING DBCSRND,R3                                                       
         ST    R3,ADBCSREX         A(CSRN EXTEND BLOCK)                         
*                                                                               
         MVI   DBCSMODE,0                                                       
         MVC   DBCSRID,=C'CSRN'                                                 
         MVI   DBCSRPRE,C'1'       PRECISION                                    
         CLI   PRECOPT,C'Y'                                                     
         JNE   *+8                                                              
         MVI   DBCSRPRE,C'2'                                                    
*                                                                               
         MVC   DBCSRDNL,APDNTDMS   SET A(COMSCORE DEMO NAMES)                   
*                                                                               
         L     RF,PDADEMTB                                                      
         USING PDDMBUFF,RF                                                      
         LA    RE,PODDEMO                                                       
         ST    RE,DBCSRRDL         SET A(REQUESTED DEMO LIST)                   
         LA    RE,PDDEMOS                                                       
         ST    RE,DBCSRODV         SET A(OUTPUT DEMO VALUES)                    
         DROP  RF                                                               
*                                                                               
         MVC   DBCSRNET(L'PDSTAT),PDSTAT NETWORK                                
         MVC   DBCSRMED,PDMEDIA    MEDIA                                        
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'PDSTAT),PDSTAT         DEFAULT NEILSEN NETWORK            
         MVC   WORK+L'PDSTAT(1),PDMEDIA                                         
         CLI   PDMEDIA,C'T'                                                     
         JNE   *+8                                                              
         MVI   WORK+L'PDSTAT,C'N'                                               
         GOTO1 =V(COMSTACN),DMCB,(1,WORK),WORK+10   GET COMSCORE NET            
         JNE   *+10                                                             
         MVC   DBCSRNET,WORK+10    SET COMSCORE NETWORK                         
*                                                                               
         MVC   DBCSRPN(L'PDLPRO),PDLPRO  PROGRAM NAME                           
         MVC   DBCSRST,PDTIME      START TIME                                   
         MVC   DBCSRET,PDTIME+2    END TIME                                     
         MVC   DBCSRDAY,PDDAY+1    DAY                                          
*                                                                               
         OI    DBCSRFLG,DBCSRFPQ   PROGRAM AVERAGE                              
         TM    PDCALOPT,PDCALNTI   USE NEILSEN DATES?                           
         JZ    *+8                                                              
         OI    DBCSRFLG,DBCSRFNQ   YES                                          
*                                                                               
         MVI   DBDEMTYP,C'4'       4 BYTE DEMO CATEGORIES                       
*                                                                               
         L     RF,AVIEWTYP         CURRENT VIEWING TYPE                         
         CLI   0(RF),0             LIVE?                                        
         JNE   *+10                                                             
         MVC   DBCSRVT,=C'RL'                                                   
         CLI   0(RF),DBXLCLQ                                                    
         JNE   *+10                                                             
         MVC   DBCSRVT,=C'RL'                                                   
         CLI   0(RF),DBXLCLCQ      PAV LIVE COMMERCIAL                          
         JNE   *+10                                                             
         MVC   DBCSRVT,=C'RC'                                                   
         CLI   0(RF),DBXLCL3Q      PAV LIVE+3 COMMERCIAL                        
         JNE   *+10                                                             
         MVC   DBCSRVT,=C'R3'                                                   
         CLI   0(RF),DBXLCL7Q      PAV LIVE+7 COMMERCIAL                        
         JNE   *+10                                                             
         MVC   DBCSRVT,=C'R7'                                                   
*                                                                               
         OC    DBEXTEND,DBEXTEND   ANY EXTENSIONS?                              
         JNZ   *+12                                                             
         ST    R3,DBEXTEND                                                      
         J     GCOM20                                                           
*                                                                               
         L     RF,DBEXTEND         FIND FIRST AVAILABLE EXTENSION               
GCOM10   CLC   =C'CSRN',0(RF)      ALREADY ADDED IT?                            
         JE    GCOM20                                                           
         OC    4(4,RF),4(RF)       A(NEXT EXTENSION)                            
         JZ    *+12                                                             
         ICM   RF,15,4(RF)                                                      
         J     GCOM10                                                           
         STCM  R3,15,4(RF)         SET EXTENSION ADDRESS                        
*                                                                               
GCOM20   GOTOR UNWEEK,DMCB,PDSBOOK,DUB   GET PROGRAM RUN DATE                   
                                                                                
         ZIC   R4,PDDAY            COMPUTING DATE                               
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         LA    R4,1                M-F=MON                                      
         CH    R4,=H'8'                                                         
         BL    *+8                                                              
         LA    R4,1                M-S=MON                                      
         BCTR  R4,0                CHANGE TO 0-6                                
         GOTO1 ADDAY,DMCB,DUB,WORK,(R4)                                         
         GOTO1 DATCON,DMCB,(0,WORK),(3,DBCSRPRD)  PROGRAM RUN DATE              
*                                                                               
         GOTO1 VCOMINTR,DMCB,=C'GET',DBLOCK                                     
*                                                                               
         SAM31                                                                  
         L     R3,DBCSRAB          A(BINSRCH TABLE ENTRY)                       
         USING BRNRECD,R3                                                       
*                                                                               
         MVI   PDCSFLG,0                                                        
         CLI   BRNIRA,C'Y'         IS REPEAT AIRING?                            
         JNE   *+8                                                              
         OI    PDCSFLG,PDCSFRQ                                                  
         CLI   BRNISP,C'Y'         IS SEASON PREMIERE?                          
         JNE   *+8                                                              
         OI    PDCSFLG,PDCSFPQ                                                  
         CLI   BRNISA,C'Y'         IS SPECIAL AIRING?                           
         JNE   *+8                                                              
         OI    PDCSFLG,PDCSFSQ                                                  
*                                                                               
         XC    PDCSEN,PDCSEN       EPISODE NUMBER                               
         CLC   =C'null',BRNEN                                                   
         JE    *+10                                                             
         MVC   PDCSEN,BRNEN                                                     
         SAM24                                                                  
         DROP  R3                                                               
*                                                                               
GETCOMX  J     XIT                                                              
         LTORG                                                                  
***********************************************************************         
* CALL COMINTER TO GET COMSCORE NETWORKS ONLY                                   
***********************************************************************         
GTCSONLY NTR1  BASE=*,LABEL=*                                                   
         TM    OPTFLAG1,COMSRCQ+COMDEMOQ   COMSCORE SOURCE/DEMO?                
         JNO   NO                                                               
         CLI   PODPASS,C'2'        COMSCORE?                                    
         JNE   NO                                                               
*                                                                               
         L     R3,PDADEMTB                                                      
         USING PDDMBUFF,R3                                                      
         LA    RE,PDDEMOS          CLEAR DEMO AREA                              
         LHI   RF,4*10*PDNDEMS                                                  
         XCEF                                                                   
         DROP  R3                                                               
*                                                                               
         LA    R3,CSRNEXT          COMINTER PARAM BLOCK                         
         USING DBCSRND,R3                                                       
         ST    R3,ADBCSREX         A(CSRN EXTEND BLOCK)                         
*                                                                               
         MVC   DBCSRID,=C'CSRN'                                                 
         OI    DBCSMODE,DBCSMCOQ   COMSCORE NETWORKS ONLY                       
         MVC   DBCSRDNL,APDNTDMS   SET A(COMSCORE DEMO NAMES)                   
         MVI   DBDEMTYP,C'4'       4 BYTE DEMO CATEGORIES                       
         MVC   DBCOMFCS,ACOMFACS   A(COMFACS)                                   
*                                                                               
         L     RF,PDADEMTB                                                      
         USING PDDMBUFF,RF                                                      
         LA    RE,PODDEMO                                                       
         ST    RE,DBCSRRDL         SET A(REQUESTED DEMO LIST)                   
         LA    RE,PDDEMOS                                                       
         ST    RE,DBCSRODV         SET A(OUTPUT DEMO VALUES)                    
         DROP  RF                                                               
*                                                                               
         OC    DBEXTEND,DBEXTEND   ANY EXTENSIONS?                              
         JNZ   *+12                                                             
         ST    R3,DBEXTEND                                                      
         J     GCSO20                                                           
*                                                                               
         L     RF,DBEXTEND         FIND FIRST AVAILABLE EXTENSION               
GCSO10   CLC   =C'CSRN',0(RF)      ALREADY ADDED IT?                            
         JE    GCSO20                                                           
         OC    4(4,RF),4(RF)       A(NEXT EXTENSION)                            
         JZ    *+12                                                             
         ICM   RF,15,4(RF)                                                      
         J     GCSO10                                                           
         STCM  R3,15,4(RF)         SET EXTENSION ADDRESS                        
*                                                                               
GCSO20   GOTO1 VCOMINTR,DMCB,=C'GET',DBLOCK                                     
         CLI   DMCB,0              ANYMORE COMSCORE NETWORKS?                   
         JNE   NO                                                               
*                                                                               
         MVC   PDWEIGHT,=F'1'      NO WEIGHTING FOR COMSCORE                    
         MVC   PDSOURCE,=CL5'COM'  COMSCORE SOURCE                              
*                                                                               
         SAM31                                                                  
         L     R4,DBCSRAB          A(BINSRCH TABLE ENTRY)                       
         USING BRNRECD,R4                                                       
*                                                                               
         MVC   PDSTAT,BRNSTA       NETWORK                                      
         MVC   PDCSSTA,BRNSTA      COMSCORE NETWORK                             
         MVC   PDCSSTAN,BRNSTAN    COMSCORE NETWORK NAME                        
         MVC   PDNTILG,BRNSN       SERIES NUMBER                                
         MVC   PDCSNETN,BRNNETN    NETWORK NUMBER                               
*                                                                               
         TM    OPTFLAG1,COMSRCOQ   COM SOURCE ONLY REQUEST?                     
         JO    GCSO20E                                                          
*                                                                               
* THIS IS A REQUEST FOR BOTH COMSCORE AND NEILSEN.                              
* IF COMSCORE NETWORK IS 4 BYTES, CHECK IF IT IS IN THE LIST OF                 
* REQUESTED NETWORKS.  IF SO, NO NEED TO LOOK UP THE CORRESPONDING              
* NIELSEN NETWORK.  FOR EXAMPLE - IF REQUEST IS FOR NETWORK DSC, AND            
* DSC WAS REQUESTED, THEN NO NEED TO CONVERT IT TO TO THE NIELSEN               
* EQUIVALENT OF DISC.                                                           
*                                                                               
         CLC   BRNSTA+4(6),=CL6' '                                              
         JNE   GCSO20D                                                          
         L     RF,APODNET                                                       
GCSO20A  CLI   0(RF),X'FF'                                                      
         JE    GCSO20D                                                          
         CLC   0(4,RF),BRNSTA      WAS IT REQUESTED?                            
         JNE   GCSO20B                                                          
         MVC   PDSTAT,0(RF)                                                     
         XC    PDCSSTA,PDCSSTA                                                  
         J     GCSO20E                                                          
GCSO20B  AHI   RF,PODNETL          GET NEXT NETWORK                             
         J     GCSO20A                                                          
*                                                                               
GCSO20D  XC    WORK,WORK                                                        
         MVC   WORK(L'BRNSTA),BRNSTA        COMSCORE NETWORK                    
         GOTO1 =V(COMSTACN),DMCB,(2,WORK),WORK+10   GET NIELSEN NETWORK         
         JNE   GCSO20E                                                          
         MVC   PDSTAT,WORK+10      SET NEILSEN NETWORK                          
         MVC   PDCSSTA,WORK+10     SET COMSCORE = NIELSEN                       
*                                                                               
GCSO20E  MVC   PDNET,PDSTAT        NETWORK                                      
         MVC   DBCSRVT,BRNVTYPE    VIEWING TYPE                                 
*                                                                               
         CLC   BRNVTYPE,=C'RL'     VIEWING TYPE                                 
         JNE   *+8                                                              
         MVI   PDACTVIW,SRCLCLQ                                                 
         CLC   BRNVTYPE,=C'RC'     VIEWING TYPE                                 
         JNE   *+8                                                              
         MVI   PDACTVIW,SRCLCLCQ                                                
         CLC   BRNVTYPE,=C'R3'     VIEWING TYPE                                 
         JNE   *+8                                                              
         MVI   PDACTVIW,SRCLCL3Q                                                
         CLC   BRNVTYPE,=C'R7'     VIEWING TYPE                                 
         JNE   *+8                                                              
         MVI   PDACTVIW,SRCLCL7Q                                                
*                                                                               
         MVI   PDCSFLG,0                                                        
         CLI   BRNIRA,C'Y'         IS REPEAT AIRING?                            
         JNE   *+8                                                              
         OI    PDCSFLG,PDCSFRQ                                                  
         CLI   BRNISP,C'Y'         IS SEASON PREMIERE?                          
         JNE   *+8                                                              
         OI    PDCSFLG,PDCSFPQ                                                  
         CLI   BRNISA,C'Y'         IS SPECIAL AIRING?                           
         JNE   *+8                                                              
         OI    PDCSFLG,PDCSFSQ                                                  
*                                                                               
         XC    PDCSEN,PDCSEN       EPISODE NUMBER                               
         CLC   =C'null',BRNEN                                                   
         JE    *+10                                                             
         MVC   PDCSEN,BRNEN                                                     
*                                                                               
         XC    PDLEPI,PDLEPI       EPISODE TITLE                                
         XC    PDEPIS,PDEPIS                                                    
         CLC   =C'null',BRNET                                                   
         JE    GCSO21                                                           
         MVC   PDEPIS,BRNET                                                     
         MVC   PDLEPI,BRNET                                                     
*                                                                               
GCSO21   GOTOR SETROTH,DMCB,BRNROT,PDDAY+1     SET ROTATION                     
         XC    PDDAY+2(3),PDDAY+2  REMOVE EBCDIC DAY (FORCE LOOKUP)             
         CLI   PDDAY+1,X'40'       MONDAY?                                      
         JNE   *+8                                                              
         MVI   PDDAY,X'01'                                                      
         CLI   PDDAY+1,X'20'       TUESDAY?                                     
         JNE   *+8                                                              
         MVI   PDDAY,X'02'                                                      
         CLI   PDDAY+1,X'10'       WEDNESDAY?                                   
         JNE   *+8                                                              
         MVI   PDDAY,X'03'                                                      
         CLI   PDDAY+1,X'08'       THURSDAY?                                    
         JNE   *+8                                                              
         MVI   PDDAY,X'04'                                                      
         CLI   PDDAY+1,X'04'       FRIDAY?                                      
         JNE   *+8                                                              
         MVI   PDDAY,X'05'                                                      
         CLI   PDDAY+1,X'02'       SATURDAY?                                    
         JNE   *+8                                                              
         MVI   PDDAY,X'06'                                                      
         CLI   PDDAY+1,X'01'       SUNDAY?                                      
         JNE   *+8                                                              
         MVI   PDDAY,X'07'                                                      
*                                                                               
         MVC   PDLPRO,BRNPROG      PROGRAM NAME                                 
         MVC   PDPROG,BRNPROG      PROGRAM NAME                                 
         MVC   PDCODE,BRNPROG      PROGRAM NAME                                 
         XC    PDNTI,PDNTI         NTI CODE                                     
*                                                                               
         CLC   =C'null',BRNDATE                                                 
         JE    GCSO22                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(L'BRNDATE),BRNDATE                                          
         GOTO1 DATCON,DMCB,(10,WORK),(3,PDDATE)    PROGRAM RUN DATE             
*                                                                               
GCSO22   CLC   =C'null',BRNSTIM                                                 
         JE    GCSO24                                                           
         CLC   =C'0000',BRNSTIM                                                 
         JNE   *+10                                                             
         MVC   BRNSTIM,=C'2400'                                                 
         CLC   =C'0000',BRNETIM                                                 
         JNE   *+10                                                             
         MVC   BRNETIM,=C'2400'                                                 
*                                                                               
         MVC   WORK(L'BRNSTIM),BRNSTIM START TIME                               
*                                                                               
         LA    RE,L'BRNSTIM                                                     
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,PDTIME                                                      
*                                                                               
GCSO24   CLC   =C'null',BRNETIM                                                 
         JE    GCSO30                                                           
         MVC   WORK(L'BRNETIM),BRNETIM END TIME                                 
*                                                                               
         LA    RE,L'BRNETIM                                                     
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,PDTIME+2                                                    
         DROP  R3,R4                                                            
         SAM24                                                                  
*                                                                               
* FIND THE WEEK OF THE PROGRAM RUN DATE TO SET PDSBOOK                          
*                                                                               
         USING PODBD,R4                                                         
         L     R4,APODBKL          LOOP THROUGH SOURCE/BOOKS                    
GCSO30   CLI   0(R4),X'FF'         END OF SOURCE/BOOKS?                         
         JE    GCSOX                                                            
         CLC   =C'COM',PODBEXT     COMSCORE SOURCE?                             
         JNE   GCSO40                                                           
*                                                                               
         MVC   PDSBOOK,PODBBKS     INIT BOOK DATE                               
*                                                                               
         GOTOR UNWEEK,DMCB,PODBBKS,DUB      CONVERT START WEEK TO DATE          
         GOTO1 DATCON,DMCB,(0,DUB),(3,PARAS)                                    
         AHI   R4,PODBLNQ                                                       
         CLC   =C'COM',PODBEXT     COMSCORE SOURCE?                             
         JNE   GCSO40                                                           
         GOTOR UNWEEK,DMCB,PODBBKS,DUB      CONVERT END WEEK TO DATE            
         GOTO1 DATCON,DMCB,(0,DUB),(3,PARAS+3)                                  
*                                                                               
         CLC   PDDATE(3),PARAS     BEFORE START WEEK?                           
         JL    GCSO40                                                           
         CLC   PDDATE(3),PARAS+3   AFTER END WEEK?                              
         JH    GCSO40                                                           
*                                                                               
GCSO35   GOTO1 DATCON,DMCB,(3,PARAS),(0,DUB)   GET WEEK RUN DATE'S IN           
         GOTO1 ADDAY,DMCB,(C'D',DUB),WORK,7                                     
         GOTO1 DATCON,DMCB,(0,WORK),(3,PARAS)                                   
*                                                                               
         CLC   PDDATE(3),PARAS                                                  
         JL    GCSOX                                                            
*                                                                               
         ZIC   RF,PDSBOOK+1        BUMP BOOK TO THE FOLLOWING WEEK              
         AHI   RF,1                                                             
         STC   RF,PDSBOOK+1                                                     
*                                                                               
         CLI   PDSBOOK+1,53        HAVE WE EXCEEDED A YEAR?                     
         BL    GCSO37              NO                                           
         MVI   PDSBOOK+1,1         YES, RESET WEEK                              
*                                                                               
         ZIC   RF,PDSBOOK          YES, INCREMENT YEAR                          
         LA    RF,1(RF)                                                         
         STC   RF,PDSBOOK                                                       
*                                                                               
GCSO37   J     GCSO35                                                           
*                                                                               
GCSO40   AHI   R4,PODBLNQ          GET NEXT SOURCE/BOOK                         
         J     GCSO30                                                           
*                                                                               
GCSOX    J     YES                                                              
         LTORG                                                                  
***********************************************************************         
* DON'T PROCESS CERTAIN NETWORKS IF ALREADY INCLUDED IN ZZZ-C         *         
***********************************************************************         
*                                                                               
TSTNET   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'BOU T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'ESC T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'GRT T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'LAF T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'ION T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'HI  T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'MET T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'STV T',0(R3)                                                  
         JE    *+10                                                             
         CLC   =C'MYS T',0(R3)                                                  
         JNE   NO                                                               
*                                                                               
         L     RF,APODNET                                                       
TSTNET02 CLI   0(RF),X'FF'                                                      
         JE    NO                                                               
         CLC   =C'ZZZ C',0(RF)                                                  
         JE    YES                                                              
         AHI   RF,PODNETL                                                       
         J     TSTNET02                                                         
         LTORG                                                                  
*                                                                               
***********************************************************************         
* SET ROTATION                                                                  
* PARAM 1 = ROTATION (EBCDIC)                                                   
* PARAM 2 = A(OUTPUT IN HEX)                                                    
***********************************************************************         
SETROTH  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            ROTATION (EBCDIC)                            
         L     R3,4(R1)            A(OUTPUT)                                    
*                                                                               
         MVI   0(R3),0                                                          
         CLI   0(R2),C'Y'          MONDAY?                                      
         JNE   *+8                                                              
         OI    0(R3),X'40'                                                      
         CLI   1(R2),C'Y'          TUESDAY?                                     
         JNE   *+8                                                              
         OI    0(R3),X'20'                                                      
         CLI   2(R2),C'Y'          WEDNESDAY?                                   
         JNE   *+8                                                              
         OI    0(R3),X'10'                                                      
         CLI   3(R2),C'Y'          THURSDAY?                                    
         JNE   *+8                                                              
         OI    0(R3),X'08'                                                      
         CLI   4(R2),C'Y'          FRIDAY?                                      
         JNE   *+8                                                              
         OI    0(R3),X'04'                                                      
         CLI   5(R2),C'Y'          SATURDAY?                                    
         JNE   *+8                                                              
         OI    0(R3),X'02'                                                      
         CLI   6(R2),C'Y'          SUNDAY?                                      
         JNE   *+8                                                              
         OI    0(R3),X'01'                                                      
         J     XIT                                                              
         LTORG                                                                  
                                                                                
         TITLE 'SUB-ROUTINES - PART 3'                                          
SUBR03   NMOD1 0,**SR03**,RA                                                    
         USING WORKD,R6                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         USING PODBD,PODBOOK                                                    
         L     RE,0(R1)                                                         
         SRL   RE,24                                                            
         SLL   RE,2                                                             
         LA    RF,*+2(RE)                                                       
         BR    RF                                                               
*                                                                               
NEXTME   EQU   (NEXTM#-*)/4+1                                                   
DEMPUTE  EQU   (DEMPUT#-*)/4+1                                                  
FIGLENE  EQU   (FIGLEN#-*)/4+1                                                  
UFFILATE EQU   (UFFILAT#-*)/4+1                                                 
AFFILATE EQU   (AFFILAT#-*)/4+1                                                 
USERAFFE EQU   (USERAFF#-*)/4+1                                                 
SUBPTYPE EQU   (SUBPTYP#-*)/4+1                                                 
GETNTISE EQU   (GETNTIS#-*)/4+1                                                 
PROGSE   EQU   (PROGS#-*)/4+1                                                   
PREPE    EQU   (PREP#-*)/4+1                                                    
DDUNVE   EQU   (DDUNV#-*)/4+1                                                   
GETNOWKE EQU   (GETNOWK#-*)/4+1                                                 
NADCATE  EQU   (NADCAT#-*)/4+1                                                  
DAYWGHTE EQU   (DAYWGHT#-*)/4+1                                                 
*SHIFTWKE EQU   (SHIFTWK#-*)/4+1                                                
SETCBLE  EQU   (SETCBL#-*)/4+1                                                  
WTCNADPE EQU   (WTCNADP#-*)/4+1                                                 
GETCURRE EQU   (GETCURR#-*)/4+1                                                 
*                                                                               
NEXTM#   B     NEXTMGR                                                          
DEMPUT#  B     DEMPUT                                                           
FIGLEN#  B     FIGLEN                                                           
UFFILAT# B     UFFILAT                                                          
AFFILAT# B     AFFILAT                                                          
USERAFF# B     USERAFF                                                          
SUBPTYP# B     SUBPTYP                                                          
GETNTIS# B     GETNTIS                                                          
PROGS#   B     PROGS0                                                           
PREP#    B     PREP0                                                            
DDUNV#   B     DDUNV0                                                           
GETNOWK# B     GETNOWK0                                                         
NADCAT#  B     NADCAT0                                                          
DAYWGHT# B     DAYWGHT0                                                         
*SHIFTWK# B     SHIFTWK0                                                        
SETCBL#  B     SETCBL0                                                          
WTCNADP# B     WTCNADP0                                                         
GETCURR# B     GETCURR0                                                         
*                                                                               
XITS3    XMOD1                                                                  
         TITLE 'CONTROL MARKET GROUP READING'                                   
***********************************************************************         
*              INPUT : R2 = REQUESTED MKTGRP                          *         
*              OUTPUT: PDSIDMKT IS NEXT MARKET TO PROCESS,ELSE X'0000'*         
***********************************************************************         
*                                                                               
NEXTMGR  L     R2,4(R1)                                                         
         XC    PDSIDMKT,PDSIDMKT                                                
         OC    PDSVMGKY,PDSVMGKY   TEST FIRST TIME                              
         BNZ   NEXT06                                                           
*                                                                               
* READ MGRDEF RECORD *                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),PDBAGYMD                                                
         MVC   KEY+8(1),0(R2)      MGRPID                                       
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLI   DMCB+8,0                                                         
         BNE   *+14                                                             
         CLC   KEY(13),KEYSAVE     TEST KEY FOUND                               
         BE    *+6                                                              
         DC    H'0'                DIE ON ERRORS                                
         L     RF,AIO1                                                          
         ST    RF,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         XC    KEY,KEY             READ PASSIVE MKTGRP POINTERS                 
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(1),PDBAGYMD                                                
         MVC   KEY+8(1),0(R2)                                                   
*                                                                               
         CLI   1(R2),C' '          TEST MARKET GROUP FILTER                     
         BNH   NEXT04                                                           
         MVC   WORK(4),1(R2)                                                    
         LA    R1,WORK                                                          
         LA    R0,4                                                             
*                                                                               
NEXT02   CLI   0(R1),C'0'                                                       
         BNL   *+8                                                              
         MVI   0(R1),C'0'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,NEXT02                                                        
         PACK  DUB(3),WORK(5)                                                   
         MVC   KEY+9(2),DUB                                                     
*                                                                               
NEXT04   GOTO1 HIGH                GET FIRST MKTGRP POINTER                     
         B     NEXT08                                                           
*                                                                               
NEXT06   MVC   KEY,PDSVMGKY        RESTORE PREVIOUS KEY                         
         GOTO1 HIGH                AND REREAD                                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SEQ                 GET NEXT MARKET LIST RECORD                  
*                                                                               
NEXT08   CLC   KEY(9),KEYSAVE                                                   
         BNE   NEXTX               EXIT IF ALL DONE                             
         MVC   PDSVMGKY,KEY        SAVE KEY OF THIS RECORD                      
*                                                                               
         CLI   1(R2),C'0'          TEST MARKET GROUP NUMBER PRESENT             
         BL    NEXT12                                                           
         LA    R1,4(R2)            POINT TO LAST BYTE OF MKTGRP                 
         LA    RE,3                                                             
*                                                                               
NEXT10   CLI   0(R1),C'0'          FILTER ON MARKET GROUP NUMBER                
         BNL   *+10                                                             
         BCTR  R1,0                                                             
         BCT   RE,NEXT10                                                        
*                                                                               
         UNPK  DUB,KEY+9(3)                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R2),DUB+3                                                    
         BNE   NEXTX               EXIT IF NO MATCH                             
*                                                                               
NEXT12   DS    0H                                                               
         MVC   PDSIDMKT(2),PDSVMGKY+11  SET MARKET NUMBER FROM KEY              
*                                                                               
NEXTX    B     XITS3                                                            
         TITLE 'EXTRACT DEMOS'                                                  
*                                                                               
DEMPUT   L     R2,PDADEMTB         SET UP DEMO BLOCK                            
         USING PDDMBUFF,R2                                                      
         LA    RE,PDDEMOS          CLEAR DEMO AREA                              
         LHI   RF,4*10*PDNDEMS                                                  
         XCEF                                                                   
*                                                                               
*                                  WORK OUT LENGTH IN 1/4 HOURS                 
         GOTOR SUBR03,DMCB,('FIGLENE',(RC))                                     
         MVC   PDWEIGHT,PDQUART    USE THIS AS WEIGHT                           
         ICM   RE,15,PDWEIGHT      ADJUST FOR MULTI-DAYS                        
         CLI   PDDAY,0           M-F                                            
         BNE   *+8                                                              
         MHI   RE,5                                                             
         CLI   PDDAY,8           M-S                                            
         BNE   *+8                                                              
         MHI   RE,7                                                             
         STCM  RE,15,PDWEIGHT                                                   
*                                                                               
         XC    DBLOCK(256),DBLOCK                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=CL3'EVN'                                                 
         MVC   DBSELAGY,AGENCY                                                  
         LA    RF,PIO                                                           
         ST    RF,DBAREC                                                        
         LA    RF,PUEL                                                          
         ST    RF,DBAQUART                                                      
*                                                                               
*                                  CHANGE FIRST BYTE OF DEMOS TO 0              
*                                                                               
         LA    RE,PODDEMO          A(DEMO CODES)                                
DEMP02   CLI   0(RE),X'FF'                                                      
         BE    DEMP04                                                           
         CLI   0(RE),1                                                          
         BNE   *+8                                                              
         MVI   0(RE),0                                                          
         AH    RE,LNDEMCOD                                                      
         B     DEMP02                                                           
*                                                                               
DEMP04   GOTOR SUBR01,DMCB,('CABFORME',(RC))                                    
*                                                                               
         CLI   FORCEBTU,C'O'                                                    
         BE    *+12                                                             
         CLI   FORCEBTU,C'I'                                                    
         BNE   DEMPUTB                                                          
         USING DBXUFD,RF                                                        
         LA    RF,UFILEXT          GET USER FILE EXTENSION                      
         OC    DBXUFADR,DBXUFADR   ANY USER FILE ACTIVE ?                       
         BZ    DEMPUTB             NO                                           
         DROP  RF                                                               
         LR    R3,RF                                                            
         GOTOR SUBR02,DMCB,('PUTEXTE',(RC)),(R3)                                
DEMPUTB  DS    0C                                                               
*                                                                               
         CLI   PDOVSYS,3                                                        
         BNE   *+8                                                              
         MVI   DBDEMTYP,C'4'                                                    
         GOTO1 DEMOUT,DMCB,(C'N',PODDEMO),DBLOCK,PDDEMOS,PODDEMPL,0             
***      GOTO1 DEMOUT,DMCB,(C'L',PODDEMO),DBLOCK,PDDEMOS                        
         ZIC   R1,PODDMNUM         REMOVE X'FF'                                 
         SLL   R1,2                                                             
         LA    R1,PDDEMOS(R1)                                                   
         MVI   0(R1),X'00'                                                      
         SPACE 2                                                                
*                                                                               
         CLC   PDSOURCE(3),=C'PIV'    DECREASE NEW PIV IMPS                     
         BNE   PIVDIVX             BY A FACTOR OF 10 TO MATCH NTI               
         CLC   PBEL+5(2),=X'5901'                                               
         BNE   PIVDIVX                                                          
         CLI   PRECOPT,C'Y'        CABLE PRECISION IS OK ALREADY                
         BE    PIVDIVX                                                          
         LA    RE,PODDEMO                                                       
         LA    RF,PDDEMOS                                                       
PIVDIV   CLI   1(RE),C'T'          DEMO IS IMPRESSIONS                          
         BNE   PIVDIV1                                                          
         L     R1,0(RF)            DIVIDE IT BY TEN                             
         SR    R0,R0                                                            
         AHI   R1,5                                                             
         D     R0,=F'10'                                                        
         ST    R1,0(RF)            AND SAVE                                     
PIVDIV1  LA    RE,4(RE)            DO FOR REST OF DEMOS IN LIST                 
         LA    RF,4(RF)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   PIVDIV                                                           
PIVDIVX  DS    0C                                                               
*                                                                               
*                              CALCULATE WEEKS THAT PROGRAM RECORD RAN          
         XC    DUB,DUB                                                          
         MVC   DUB(6),PROGSTRT                                                  
         CLC   PRGSTRT(2),PODPSTRT                                              
         BL    DEMP06                                                           
         GOTO1 DATCON,DMCB,(2,PRGSTRT),(0,DUB)                                  
*                                                                               
DEMP06   GOTO1 PERVERT,DMCB,DUB,PDDATE                                          
         SR    R5,R5                                                            
         LH    R5,DMCB+12                                                       
         LTR   R5,R5               PROTECT AGAINST FILE PROGRAM                 
         BNM   *+8                                                              
         LA    R5,1                                                             
         OC    DMCB+10(2),DMCB+10                                               
         BZ    *+8                                                              
         LA    R5,1(R5)                                                         
*                                                                               
         ICM   RF,15,PDWEIGHT     MULTIPLY QUARTER HOUR AND DAY WEIGHT          
         SR    RE,RE                                                            
         MR    RE,R5                                                            
         LR    R5,RF                                                            
         STCM  R5,15,PDWEIGHT                                                   
*                                                                               
         CLC   =C'MI',PDSOURCE+1                                                
         BNE   DEMP08                                                           
         MVC   DBSELSRC,PODBSRC                                                 
         MVC   DBSELRMK,PODMPAMK                                                
*                                                                               
DEMP08   MVC   SVKEY,KEY           SAVE KEY                                     
         LA    RE,DBLOCK           SAVE DBLOCK                                  
         LA    RF,DBLOCKL                                                       
         LA    R0,SVDBLK                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,MPAHOOK          IS IT EMI?                                   
         LTR   RF,RF               0=NO, NON-ZERO=YES                           
         BZ    DEMP10                                                           
         BASR  RE,RF                                                            
*                                                                               
         LA    RE,SVDBLK           RESTORE DBLOCK                               
         LA    RF,DBLOCKL                                                       
         LA    R0,DBLOCK                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   KEY,SVKEY           RESTORE ORIGINAL RECORD                      
         GOTO1 HIGH                                                             
         B     DEMPX                                                            
*                                                                               
DEMP10   MVC   DBLOCKA,DBLOCK      MAKE SURE DBLOCKA OK                         
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
DEMPX    B     XITS3                                                            
         DROP  R2,R4                                                            
         TITLE 'FIGURE OUT LENGTH IN 1/4 HOURS'                                 
*                                                                               
FIGLEN   MVC   PDQUART,=F'2'                                                    
         OC    PDTIME+2(2),PDTIME+2                                             
         BZ    FIGLX                                                            
         XC    DUB,DUB                                                          
         LA    R2,PDTIME                                                        
         LA    R3,DUB+3                                                         
         BAS   RE,GETQ                                                          
         LA    R2,2(R2)                                                         
         LA    R3,DUB+7                                                         
         BAS   RE,GETQ                                                          
         LM    R0,R1,DUB           START R0, END R1                             
         SR    R1,R0                                                            
         BNP   FIGLX                                                            
         STCM  R1,15,PDQUART                                                    
*                                                                               
FIGLX    B     XITS3                                                            
*                                                                               
GETQ     NTR1                                                                   
         LH    R1,0(R2)            MILITARY TIME TO 1/4 HOUR                    
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CHI   R1,6                                                             
         BNL   *+8                                                              
         LA    R1,24(R1)                                                        
         SHI   R1,6                                                             
         SLL   R1,2                                                             
         LR    R2,R1                                                            
         SRDA  R0,32                                                            
         D     R0,=F'15'                                                        
         AR    R2,R1                                                            
         STC   R2,0(R3)                                                         
         XIT1                                                                   
         TITLE 'FILTER DEMOS BY USER AFFILIATES'                                
***********************************************************************         
*  IF UAFFIL REQUESTED:                                               *         
*   - STATION MASTER RECORD & USER AFFILIATE MATCH = ACCEPT           *         
*   - STATION MASTER RECORD & USER AFFILIATE NO MATCH = RECECT        *         
*   - NO STATION MASTER RECORD FOUND:                                 *         
*       IF AFFIL REQUESTED & STATION AFFILIATE MATCH = ACCEPT         *         
*       IF AFFIL REQUESTED & STATION AFFILIATE NO MATCH = REJECT      *         
*       IF AFFIL NOT REQUESTED = REJECT                               *         
*  IF UAFFIL NOT REQUESTED:                                           *         
*       IF AFFIL REQUESTED & STATION AFFILIATE MATCH = ACCEPT         *         
*       IF AFFIL REQUESTED & STATION AFFILIATE NO MATCH = REJECT      *         
*       IF AFFIL NOT REQUESTED = ACCEPT                               *         
***********************************************************************         
*                                                                               
UFFILAT  L     R4,APDUFFLT         GET TABLE OF USER AFFILIATES                 
         CLI   0(R4),X'FF'         ANY ENTRIES?                                 
         BE    UFFI04              NO, CHECK STATION AFFILIATES                 
*                                                                               
         OC    PDUAFFL,PDUAFFL     ANY USER AFFILIATE?                          
         BZ    UFFINO              SKIP THIS ONE THEN                           
*                                                                               
         LA    R1,PDUAFFL          FOUND MASTER, CHECK USER AFFILIATE           
*                                                                               
UFFI02   CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    UFFINO              YES, REJECT THIS ONE                         
         CLC   0(L'PDUAFFL,R4),0(R1)                                            
         BE    UFFIYES             FOUND IT                                     
         LA    R4,L'PDUAFFL(R4)  TRY NEXT                                       
         B     UFFI02                                                           
*                                                                               
UFFI04   GOTOR SUBR03,DMCB,('AFFILATE',(RC))                                    
         BZ    UFFINO              NO MATCH                                     
         B     UFFIYES             MATCH OR NO ENTRIES                          
*                                                                               
UFFI06   L     R4,APDAFFLT         A(STATION TABLE)                             
         CLI   0(R4),X'FF'         ANY ENTRIES?                                 
         BNE   UFFI04              YES, LOOK FOR MATCH                          
*                                                                               
UFFINO   SR    R4,R4               NO, REJECT                                   
*                                                                               
UFFIYES  LTR   R4,R4                                                            
UFFIX    B     XITS3                                                            
         TITLE 'FILTER DEMOS BY STATION AFFILIATE'                              
AFFILAT  L     R4,APDAFFLT         GET TABLE OF STATION AFFILIATES              
         CLI   0(R4),X'FF'         ANY ENTRIES?                                 
         BE    AFFIYES             NO, ACCEPT EVERYTHING                        
*                                                                               
         GOTO1 DEFINE,DMCB,=C'AFFL',DBLOCK,PDTPAFFL                             
*                                                                               
         LA    R1,PDTPAFFL         GET AFFILIATE                                
*                                                                               
AFFI02   CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    AFFINO              YES, SKIP THIS ONE                           
         CLC   0(L'PDTPAFFL,R4),0(R1)                                           
         BE    AFFIYES             FOUND IT                                     
         LA    R4,L'PDTPAFFL(R4)   TRY NEXT                                     
         B     AFFI02                                                           
*                                                                               
AFFINO   SR    R4,R4                                                            
*                                                                               
AFFIYES  LTR   R4,R4                                                            
AFFIX    B     XITS3                                                            
         TITLE 'READ MASTER STATION FOR USER AFFILIATE'                         
USERAFF  XC    PDUAFFL,PDUAFFL     CLEAR USER AFFILIATE                         
         CLI   PDOVSYS,8           REP SYSTEM?                                  
         BE    USERX               YES, NO MASTER RECORDS                       
         LA    R4,KEY                                                           
         USING STARECD,R4          MASTER RECORD ON STATION FILE                
         XC    KEY,KEY                                                          
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(STAKEYLN-1),STAKEY                                      
         MVI   STAKTYPE,C'S'                                                    
*        MVC   STAKMED,PODBMED                                                  
         MVI   STAKMED,C'T'        ALWAYS LOOK UP MEDIA 'T'                     
         MVC   STAKCALL,0(R3)                                                   
         OC    STAKCALL,=CL5' '                                                 
         MVC   STAKAGY,AGENCY                                                   
         CLI   DBSELMED,C'C'       IS THIS CANADIAN?                            
         BNE   USER02              NO                                           
         MVI   STAKMED,C'T'        YES,MEDIA SHOULD BE T NOT C                  
         MVI   STAKCALL+4,C'T'     CHANGE CALL LETTER ALSO                      
*                                                                               
USER02   MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATIO',KEY,AMYIO                     
         CLI   8(R1),0                                                          
         BNE   USERX                                                            
         L     R4,AMYIO                                                         
         CLC   STAREC(MKTKEYLQ),KEYSAVE                                         
         BNE   USERX                                                            
         MVC   PDUAFFL(L'SNETWRK),SNETWRK  SAVE THE NETWORK AFFILIATE           
         OC    PDUAFFL,=CL5' '                                                  
         CLI   DBSELMED,C'C'       IS THIS CANADIAN?                            
         BNE   USERX               NO                                           
         XC    PDUAFFL,PDUAFFL     YES, GET FROM DIFFERENT SPOT                 
         MVC   PDUAFFL(L'SCANNTWK),SCANNTWK                                     
         OC    PDUAFFL,=CL5' '                                                  
*                                                                               
USERX    B     XITS3                                                            
         DROP  R4                                                               
         TITLE 'FILTER DEMOS BY SUB PROGRAM TYPE'                               
SUBPTYP  GOTO1 DEFINE,DMCB,=C'PTYP4',DBLOCK,PDPTYP8                             
         L     R4,APDSUBPT         GET TABLE OF SUB PROGRAM TYPES               
         CLI   0(R4),X'FF'         ANY ENTRIES?                                 
         BE    SUBPYES             NO, ACCEPT EVERYTHING                        
*                                                                               
SUBP02   CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    SUBPNO              YES, SKIP THIS ONE                           
         CLC   0(4,R4),PDPTYP8+4                                                
         BE    SUBPYES             FOUND IT                                     
         LA    R4,4(R4)            TRY NEXT                                     
         B     SUBP02                                                           
*                                                                               
SUBPNO   SR    R4,R4                                                            
*                                                                               
SUBPYES  LTR   R4,R4                                                            
SUBPX    B     XITS3                                                            
         TITLE 'READ PROGRAM GROUPS AND BUILD TABLE OF NTI CODES'               
GETNTIS  L     R4,PRGLPTR                                                       
         CLI   0(R4),X'FF'         DON'T CLEAR PDNTIFT UNLESS WE HAVE           
         BE    GETNTX              PROGRAM GROUPS                               
*                                                                               
         L     RE,APDNTIFT                                                      
         LA    RF,L'PDNTIFT                                                     
         XCEFL                                                                  
*                                                                               
         L     R6,APDNTIFT         GET PLACE TO BUILD TABLE                     
         LA    R0,62               5 (NTI) + 16 (NAME) = 21 X 62=1302           
*                                                                               
GETNT02  L     R4,PRGLPTR          GET POINTER TO NEXT PROGRAM GROUP            
         CLI   0(R4),X'FF'         ANY ENTRIES?                                 
         BE    GETNTX              NO, DONE                                     
*                                                                               
         LA    R5,KEY              BUILD KEY                                    
         USING NPRGRECD,R5                                                      
         XC    KEY,KEY                                                          
         MVC   NPRGKTYP,=X'0D3C'                                                
         MVC   NPRGKAGM,PDBAGYMD                                                
         MVC   NPRGKCOD,0(R4)      GET PROGRAM GROUP CODE                       
*                                                                               
         MVC   DATADISP,=H'24'                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                 MUST HAVE IT BY NOW                          
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R5,IO               GET THE PROGRAMS                             
         MVI   ELCODE,NPRGELQ                                                   
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
*                                                                               
GETNT04  BAS   RE,NEXTEL2                                                       
         BNE   GETNT06                                                          
*                                                                               
         USING NPRGEL,R5                                                        
         MVC   0(L'NPRGENTI,R6),NPRGENTI                                        
         MVC   L'NPRGENTI(L'NPRNTINM,R6),NPRNTINM                               
         LA    R6,L'NPRGENTI+L'NPRNTINM(R6)                                     
         B     GETNT04                                                          
*                                                                               
GETNT06  MVC   PDPRGCOD,0(R4)      SET THE PROGRAM GROUP CODE                   
         LA    R4,8(R4)            GET NEXT PROGRAM GROUP                       
         STCM  R4,15,PRGLPTR       SAVE THE ADDRESS                             
         CLI   PDPRGROW,C'Y'       ARE WE DISPLAYING GROUP?                     
         BE    GETNTX              YES, EXIT NOW                                
         BCT   R0,GETNT02          NO, GET REST OF THE GROUPS                   
         XC    PDPRGCOD,PDPRGCOD   CLEAR CODE IF NOT PRINTING                   
*                                                                               
GETNTX   B     XITS3                                                            
         DROP  R5                                                               
         GETELN R5,DATADISP,ELCODE,2                                            
         EJECT                                                                  
***********************************************************************         
*        HANDLE PROGRAM RECORDS FOR NETWORK                           *         
* 4(R1) HAS A(CURRENT POSITION IN THE SOURCE/BOOK LIST)               *         
***********************************************************************         
*                                                                               
PROGS0   DS    0H                                                               
*                                                                               
         CLC   =C'IAG',PDSOURCE    DON'T NEED TO FORCE IF REQUESTED IAG         
         BE    PRGOPIX                                                          
*                                                                               
         L     RE,PDADEMTB         GET DEMO BLOCK                               
PRGOPI   CLI   0(RE),X'FF'                                                      
         BE    PRGOPIX                                                          
         CLI   0(RE),172           OPI                                          
         BNE   *+8                                                              
         MVI   FORCEBTU,C'O'                                                    
         CLI   0(RE),175           IAG ORIGINAL                                 
         BE    PRGIAG                                                           
         CLI   0(RE),176           IAG REPEAT                                   
         BE    PRGIAG                                                           
         CLI   0(RE),177           IAG AUTOMATIC ORIG/REPEAT                    
         BNE   *+8                                                              
PRGIAG   MVI   FORCEBTU,C'I'                                                    
         AH    RE,LNDEMCOD                                                      
         B     PRGOPI                                                           
PRGOPIX  DS    0X                                                               
*                                                                               
* OPUP JUN 18/09 :                                                              
*       THE STATEMENT BELOW IS INCORRECT.                                       
*       IT LOADS INTO R2 THE ADDRESS OF THE FIRST SOURCE/BOOK.                  
*       THIS IS WRONG IF THE REQUEST HAS MORE THAN ONE SOURCE/BOOK.             
***      L     R2,APODBKL          RESET R2                                     
*       INSTEAD, USE THE ADDRESS OF THE CURRENT SOURCE/BOOK THAT IS NOW         
*       PASSED IN THE PARAMETER LIST AT 4(R1).                                  
         L     R2,4(R1)                                                         
*                                                                               
         GOTOR SUBR01,DMCB,('GETDNAME',(RC))                                    
         MVI   LASTHDAY,X'FF'                                                   
         MVC   PDNET(8),0(R3)                                                   
         OC    PODNETAD,PODNETAD   ALL STATION TABLE                            
         BZ    *+10                                                             
         MVC   PDMEDIA(2),PODNETMB                                              
         MVC   PDSOURCE(5),1(R2)                                                
         MVC   PDFILE(3),8(R2)                                                  
                                                                                
         LA    RF,1000             CLEAR PROG SAVE AREA                         
         L     RE,AIUNCOND                                                      
         XCEF                                                                   
                                                                                
         LA    R4,KEY              BUILD KEY                                    
         USING NPGRECD,R4                                                       
         MVC   DATADISP,=H'24'                                                  
         XC    NPGKEY,NPGKEY                                                    
         MVC   NPGKTYP,=X'0DA0'                                                 
         MVC   NPGKAM,PDBAGYMD                                                  
         MVC   NPGKNET,6(R3)       NETWORK MARKET TABLE                         
         OC    PODNETAD,PODNETAD   ALL STATION TABLE                            
         BZ    *+10                                                             
         MVC   NPGKNET,4(R3)       NETWORK MARKET TABLE                         
         GOTO1 HIGH                                                             
         XC    SVPDCODE,SVPDCODE                                                
         B     PROG04                                                           
*                                                                               
PROG02   LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
PROG04   LA    RE,PDCODE                                                        
         LA    RF,PDNTILEN                                                      
         XCEFL                                                                  
*                                                                               
         CLC   KEY(5),KEYSAVE      CHECK NETWORK C/B                            
         BNE   PROGX                                                            
         CLC   NPGKEND,PODPSTRT    DATE FILTERS                                 
         BL    PROG02                                                           
         CLC   NPGKEND,PODPEND                                                  
         BH    PROG02                                                           
         GOTO1 DATCON,DMCB,(2,NPGKEND),(0,PDDATE)                               
*                                                                               
* ALLOW ONLY 1 PIV RECORD FOR ANY PROGRAM/BOOK                                  
* BYPASS RECORDS WHICH END PRIOR TO THIS BOOK OR HAVE A PREVIOUS                
* END DATE WHICH WAS INCLUDED IN THIS BOOK                                      
*                                                                               
         GOTO1 NETWEEK,DMCB,PDDATE,GETDAY,ADDAY                                 
         MVC   HALF(1),DMCB+4        YEAR NO.                                   
         MVC   HALF+1(1),DMCB+12     WEEK NO.                                   
*                                                                               
         CLI   FORCEBTU,C'I'                                                    
         BE    *+12                                                             
         CLI   FORCEBTU,C'O'                                                    
         BNE   *+10                                                             
         MVC   DBSELBK,HALF                                                     
*                                                                               
         CLC   HALF(2),PDSBOOK     FILTER OUT BASED ON END DATE                 
         BL    PROG02                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R5,IO                                                            
         TM    NPGCNTL-NPGRECD(R5),X'80'   SKIP DELETED RECORDS                 
         BO    PROG02                      FOR LEFT OVER PASS POINTERS          
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,NPGRLEN-NPGKEY(R5)                                          
         AR    R1,R5                                                            
         XC    0(4,R1),0(R1)                                                    
*                                                                               
         LA    R5,IO                                                            
         MVI   ELCODE,X'93'                                                     
         BRAS  RE,GETEL                                                         
         BE    PRG93A                                                           
         SR    R5,R5                                                            
         B     PRG93B                                                           
         USING NPG2ELEM,R5                                                      
PRG93A   MVC   PDPVS(1),NPG2SRC                                                 
         MVC   PDPVS+1(1),NPG2VTYP                                              
*                                                                               
PRG93B   CLI   DPTOPT,0            DAYPART OPTION                               
         BE    PRG93C                                                           
         LTR   R5,R5                                                            
         BZ    PROG02              NO '93' ELEMENT. SKIP IT                     
         CLC   DPTOPT,NPG2DYP                                                   
         BNE   PROG02                                                           
*                                                                               
PRG93C   L     RE,APVSFILT                                                      
         OC    0(2,RE),0(RE)       FILTER ON VIEWING SOURCE                     
         BZ    PROG05                                                           
         LTR   R5,R5                                                            
         BZ    PRG93NOM            NO '93' ELEMENT. NO MATCH                    
         LHI   R0,MAXPVSQ                                                       
PRG93D   OC    0(2,RE),0(RE)                                                    
         BZ    PRG93NOM            END OF FILTERS. NO MATCH                     
         CLI   0(RE),C'*'          WILD CARD SOURCE                             
         BE    *+14                                                             
         CLC   PDPVS(1),0(RE)      MATCH ON SOURCE?                             
         BNE   PRG93E              NO. TRY NEXT FILTER                          
         CLI   1(RE),C'*'          WILD CARD VIEWING TYPE                       
         BE    PRG93MCH            IS A MATCH                                   
         CLC   PDPVS+1(1),1(RE)    MATCH ON VIEWING TYPE                        
         BE    PRG93MCH            WE HAVE A MATCH                              
PRG93E   LA    RE,2(RE)            TRY NEXT FILTER                              
         BCT   R0,PRG93D                                                        
         B     PRG93NOM            END OF FILTERS. NO MATCH                     
*                                                                               
PRG93MCH CLI   PVSNEG,PVSNEGQ      MATCHED ONE OF THE FILTERS                   
         BE    PROG02              IF NEGATE, EXCLUDE PROGRAM                   
         B     PROG05              OTHERWISE, INCLUDE PROGRAM                   
PRG93NOM CLI   PVSNEG,PVSNEGQ      DIDN'T MATCH ANY FILTER                      
         BE    PROG05              IF NEGATE, INCLUDE PROGRAM                   
         B     PROG02              OTHERWISE, EXCLUDE PROGRAM                   
         DROP  R5                                                               
*                                                                               
PROG05   XC    PDAEMETH,PDAEMETH                                                
         LA    R5,IO                                                            
         MVI   ELCODE,NPGAICDQ     AUDIENCE ESTIMATOR ELEMENT?                  
         BRAS  RE,GETEL                                                         
         BNE   PROG05B                                                          
         USING NPGAEIFD,R5         YES. THIS PROG WAS CREATED FROM AE           
         MVI   PDAEMETH,1          FLAG THAT THIS IS AE,IN CASE                 
         CLI   NPGAILN,NPGAEIL1    METHODOLOGY NOT FILLED IN                    
         BNH   PROG05A                                                          
         CLI   NPGAIMET,0           DON'T USE IT IF NOT FILLED IN               
         BE    *+10                                                             
         MVC   PDAEMETH,NPGAIMET                                                
*                                                                               
PROG05A  CLI   PIVAEOPT,0                                                       
         BE    PROG06                                                           
         CLI   PIVAEOPT,PIVAEY                                                  
         BE    PROG06              INCLUDE ONLY AE                              
         B     PROG02                                                           
*                                                                               
PROG05B  CLI   PIVAEOPT,0          NO. THIS PROGRAM WAS CREATED IN SFM          
         BE    PROG06                                                           
         CLI   PIVAEOPT,PIVAEN                                                  
         BE    PROG06              INCLUDE ONLY NON-AE                          
         B     PROG02                                                           
*                                                                               
PROG06   LA    R5,IO                                                            
         LA    R4,IO                                                            
         MVC   PDCODE,NPGKPROG                                                  
         MVC   DBSELSTA(5),PDNET                                                
*                                                                               
* SEE IF PROG CODE ALREADY PROCESSED FOR THIS BOOK                              
         L     RE,AIUNCOND         POINT TO BUFFER                              
         CLC   AGENCY,=C'NE'                                                    
         BNE   PROG06AX                                                         
         CLC   HALF(1),PDSBOOK                                                  
         BE    PROG06AX                                                         
PROG06A  OC    0(6,RE),0(RE)       EOB                                          
         BZ    PROG06AX                                                         
         CLC   NPGKPROG(6),0(RE)   PROGRAM PROVIOUSLY PROCESSED                 
         BE    PROG02                                                           
         LA    RE,6(RE)                                                         
         B     PROG06A                                                          
PROG06AX DS    0C                                                               
*                                                                               
         LA    R4,KEY                                                           
         MVI   ELCODE,X'5D'        NEED A BOOK ELEMENT                          
         BRAS  RE,GETEL                                                         
         MVC   PBEL,0(R5)                                                       
         LA    R5,IO                                                            
         MVI   ELCODE,X'92'                                                     
         BRAS  RE,GETEL                                                         
*                                                                               
         USING NPGELEM,R5                                                       
         OC    PODDAYTM,PODDAYTM   DAY/TIME FILTERS                             
         BZ    PROG10                                                           
         LA    R2,PODDAYTM         MUST MATCH ON ONE IN LIST                    
*                                                                               
PROG08   CLI   0(R2),X'FF'         FOR DAY - ALL                                
         BE    PROG10                                                           
         CLC   0(1,R2),NPGDAY                OR MATCH                           
         BNE   PROG16                                                           
*                                                                               
PROG10   CLI   1(R2),X'FF'         AND TIME - ALL                               
         BE    PROG18                                                           
         MVC   WORK(4),NPGTIME                OR FIT                            
         MVC   WORK+4(4),1(R2)                                                  
         LA    R1,WORK                                                          
         LA    R0,4                                                             
*                                                                               
PROG12   LH    RE,0(R1)            CONVERT 0-6AM TO 2400-3000                   
         BZ    PROG14                                                           
         CHI   RE,600                                                           
         BNL   PROG14                                                           
         LA    RE,2400(RE)                                                      
         STH   RE,0(R1)                                                         
*                                                                               
PROG14   LA    R1,2(R1)                                                         
         BCT   R0,PROG12                                                        
         CLC   WORK(2),WORK+4      CHECK START NOT BEFORE REQ START             
         BL    PROG16                                                           
         OC    WORK+2(2),WORK+2    WAS END TIME SPECIFIED                       
         BNZ   *+10                                                             
         MVC   WORK+2(2),WORK      END=START                                    
         CLC   WORK(2),WORK+6      TEST START NOT PAST REQUEST END              
         BL    PROG18                                                           
         CLC   WORK+2(2),WORK+6     OR END ON REQUEST END (ST=ET)               
         BE    PROG18                                                           
*                                                                               
PROG16   LA    R2,5(R2)                                                         
         CLI   0(R2),X'99'         CHECK END OF TABLE                           
         BNE   PROG08                                                           
         B     PROG02                                                           
*                                                                               
PROG18   XC    DUB,DUB             CHECK FILTERS                                
         MVC   DUB(4),NPGFILT                                                   
         GOTO1 VCHKFILT,DMCB,DUB,PODPVFLT                                       
         CLI   DMCB+4,X'FF'        CHECK RETURN CODE                            
         BNE   PROG20                                                           
         B     PROG02                                                           
*                                                                               
*                               BUILD PHONEY EVN RECORD & EXTRACT DATA          
*                                                                               
PROG20   DS    0C                                                               
         CLI   FORCEBTU,C'O'       IF USER ADJUST                               
         BE    *+12                                                             
         CLI   FORCEBTU,C'I'       IF USER ADJUST FOR IAG                       
         BNE   PROG21                                                           
         GOTOR SUBR02,FULL,('BTUREADE',(RC))                                    
         LA    RE,UFILEXT                                                       
         XC    UFILEXT,UFILEXT                                                  
         USING DBXUFD,RE                                                        
         MVC   DBXUFID,=C'UFIL'                                                 
         CLI   GOTUTYPE,1            FIND BOOK TYPE U?                          
         BNE   PROG21                NO                                         
         L     RF,AMYDBLK                                                       
         MVC   DBXUFADR,4(RF)        YES, SAVE A(RECORD)                        
         DROP  RE                                                               
*                                                                               
PROG21   GOTOR SUBR02,DMCB,('EVN0E',(RC))                                       
         GOTOR SUBR03,DMCB,('DEMPUTE',(RC))                                     
* SAVE PROCESSED PROGRAM                                                        
         L     RF,AIUNCOND         POINT TO SAVE AREA END                       
         LA    RF,990(RF)                                                       
         L     RE,AIUNCOND         POINT TO SAVE AREA                           
PROG22   OC    0(6,RE),0(RE)       FIND AN OPEN SLOT                            
         BZ    PROG26                                                           
         CR    RE,RF               TO MANY ENTRIES                              
         BH    PROG24                                                           
         LA    RE,6(RE)                                                         
         B     PROG22                                                           
PROG24   LA    RF,1000             RESET THE BUFFER                             
         L     RE,AIUNCOND                                                      
         XCEF                                                                   
         L     RE,AIUNCOND                                                      
*                                                                               
PROG26   LA    R4,IO                                                            
         MVC   0(6,RE),NPGKPROG    SAVE PROCESSED PROGRAM                       
*                                                                               
         B     PROG02                                                           
*                                                                               
PROGX    B     XITS3                                                            
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE THE REPORT                                        *         
***********************************************************************         
PREP0    DS    0H                                                               
*                                                                               
         MVI   ANYUP,C'N'                                                       
         OC    PDUPOPT,PDUPOPT                                                  
         BZ    *+12                                                             
         MVI   ANYUP,C'Y'          SET UPGRADE OPTION                           
         MVI   DAYPOPT,C'Y'        FORCE DAYPART OPTION                         
*                                                                               
         GOTO1 INITDRIV            INITIALIZE DRIVER                            
         L     R1,AGLOBAL                                                       
         USING GLOBALD,R1                                                       
         ZIC   RF,GLFHEADL                                                      
         LA    RF,3(RF)            ALLOW HEADERS 1-4                            
         STC   RF,GLLHEADL                                                      
         DROP  R1                                                               
         CLI   PDOVSYS,2           GET 1W PROFILE FOR SPOT                      
         BNE   PREP02                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S01W'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'T'                                                      
         B     PREP04                                                           
*                                                                               
PREP02   CLI   PDOVSYS,3           GET N2 PROFILE FOR NETWORK                   
         BNE   PREP06                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0N2'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
*                                                                               
PREP04   GOTO1 VGETPROF,DMCB,WORK,PROF1W,DATAMGR                                
         B     PREP08                                                           
*                                                                               
PREP06   CLI   PDOVSYS,8           GET RMP PROFILE FOR REP                      
         BNE   PREP06                                                           
         GOTOR SUBR02,DMCB,('PAREPE',(RC))                                      
*                                                                               
PREP08   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         LAY   R1,DRHOOK           APPLICATION HOOK                             
         ST    R1,GLAHOOK                                                       
         OI    GLINDS,GLPALDET                                                  
         MVI   GLMODE,GLINIT                                                    
         GOTO1 DRIVER,DMCB,(R4)                                                 
         MVC   GLRNKMAX,PDRNKMAX                                                
*                                                                               
         CLI   PDOVSYS,3    *******NETWORK SYSTEM ONLY*********                 
         BNE   PREPX                                                            
         CLI   PODPSTRT,0          UNIVERSES FOR PROGRAMS                       
         BE    PREPX                                                            
         LA    R2,BLOCK                                                         
         USING GUVD,R2             FILL BLOCK FOR GETNUN                        
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVDATE,PODPSTRT                                                 
         XC    PUEL,PUEL                                                        
         MVI   PUEL,X'31'                                                       
         MVI   PUEL+1,179                                                       
         MVI   PUEL+2,X'44'                                                     
         LA    R1,PUEL+3                                                        
         ST    R1,GUVAOUT                                                       
         MVI   GUVTYPE,2           (HUNDREDS)                                   
         MVC   GUVAREC,AIO                                                      
         MVC   GUVCMFCS,ACOMFACS                                                
         GOTO1 GETNUN,DMCB,GUVBLOCK                                             
         CLI   GUVERROR,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    UNIVOPT,UNIVOPT                                                  
         BZ    PREP10              OPTION TO USE SPECIAL UNIVERSE               
         MVC   GUVAGY,AGENCY                                                    
         L     R1,UNIVOPT          NEED CODE IN PWOS                            
         CVD   R1,DUB                                                           
         L     R1,DUB+4                                                         
         SRL   R1,4                                                             
         STH   R1,DUB                                                           
         MVC   GUVCODE,DUB                                                      
         XC    GUVDATE,GUVDATE                                                  
         GOTO1 GETNUN,DMCB,GUVBLOCK                                             
         CLI   GUVERROR,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*PREP10   BAS   RE,DDUNIVS                                                      
PREP10   GOTOR SUBR03,DMCB,('DDUNVE',(RC))                                      
         OC    HOMEOPT,HOMEOPT     OPTION TO MODIFY UNIVERSES                   
         BZ    PREPX               BASED ON SUPPLIED HOMES FIGURE               
         MVC   DUB(4),PUEL+87      PICK OUT HOMES FIGURE                        
         L     R2,DUB              INTO R2                                      
         L     R3,HOMEOPT                                                       
         MHI   R3,10               GET R3 TO HUNDREDS AS WELL                   
         LA    R4,PUEL+3                                                        
         ZIC   R5,PUEL+1           R5 - DEVELOP NUMBER OF ELEMENTS              
         SRL   R5,2                     BY DIVIDING BY 4                        
*                                                                               
PREP12   L     R1,0(R4)                                                         
         MR    R0,R3                                                            
         DR    R0,R2                                                            
         ST    R1,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,PREP12                                                        
*                                                                               
*PREPX    B     XIT                                                             
PREPX    B     XITS3                                                            
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL PREP ROTUINES                                     *         
***********************************************************************         
DDUNV0   DS    0H                                                               
*                                                                               
         LA    R2,PDEL                                                          
         L     R5,AIO                                                           
         MVI   ELCODE,X'DD'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DDUNX                                                            
*                                                                               
*        USING NUNELDD,R5                                                       
DDUN02   MVC   0(12,R2),0(R5)                                                   
         LA    R2,12(R2)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    DDUN02                                                           
*                                                                               
DDUNX    ST    R2,DDPOINT                                                       
*        B     XIT                                                              
         B     XITS3                                                            
         EJECT                                                                  
***********************************************************************         
* GETNOWK0 - GET THE NUMBER OF WEEKS IN THE MONTH                     *         
*            ANACWKS IS A(NACWKS)                                     *         
***********************************************************************         
GETNOWK0 DS    0H                                                               
         MVI   WEEKNUM,0                                                        
         L     RE,ANACWKS                                                       
         USING NEWKSD,RE                                                        
NOWKS1   CLC   DBSELBK(1),NEWKSYR  COMPARE ON YEAR                              
         BH    NOWKS30                                                          
         BL    GETNOWKX            NOT FOUND                                    
         CLC   DBSELBK+1(1),NEWKSFRS  COMPARE ON FIRST WEEK                     
         BH    NOWKS30             (AVG DATA HAS START DATE OF 1ST WK)          
         BL    GETNOWKX            NOT FOUND                                    
         ZIC   R0,NEWKSFRS                                                      
         ZIC   R1,NEWKSLST                                                      
         SR    R1,R0                                                            
         AHI   R1,1                NO OF WEEKS IN THE MONTH                     
         STC   R1,WEEKNUM                                                       
         B     GETNOWKX                                                         
*                                                                               
NOWKS30  LA    RE,NEWKSQ(RE)       NEXT ENTRY                                   
         CLI   0(RE),X'FF'                                                      
         BNE   NOWKS1                                                           
GETNOWKX B     XITS3                                                            
         EJECT                                                                  
***********************************************************************         
* NADCAT0  - FOR CABLE NAD, MATCH ON DEMO CATEGORY                    *         
*  IF NO MATCH ON THE RECORD, TURN ON THE HIGH ORDER BIT OF DEMO VALUE*         
*       AREG1-> A(DEMO EXPRESSIONS)                                   *         
*       AREG2-> A(OUTPUT AREA)                                        *         
***********************************************************************         
NADCAT0  DS    0H                                                               
         L     R1,AREG1                                                         
         L     R2,AREG2                                                         
NADCAT1  CLI   0(R1),X'FF'        END OF DEMO CATEG LIST                        
         BE    NADCATX                                                          
         L     R5,DBAQUART                                                      
         CLI   0(R5),X'0F'        ZIP PAST '0F' ELEM                            
         BNE   NADCAT5                                                          
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
NADCAT5  CLI   0(R5),0                                                          
         BE    NADCAT25           END OF RECORD                                 
         CLI   0(R5),X'0F'                                                      
         BE    NADCAT25           REACHED NEXT LEVEL                            
         CLI   0(R5),SLCODEQ      X'23' ELEM                                    
         BNE   NADCAT20                                                         
         USING SLELEM,R5                                                        
         CLC   SLSECT,0(R1)                                                     
         BE    NADCAT30                                                         
         BH    NADCAT25                                                         
*                                                                               
NADCAT20 ZIC   R0,1(R5)           NEXT ELEMENT                                  
         AR    R5,R0                                                            
         B     NADCAT5                                                          
*                                                                               
NADCAT25 OI    0(R2),X'80'        DEMO CAT NOT FOUND ON THIS REC                
*                                                                               
NADCAT30 LA    R1,4(R1)           NEXT CATEGORY                                 
         LA    R2,4(R2)                                                         
         B     NADCAT1                                                          
*                                                                               
NADCATX  B     XITS3                                                            
         EJECT                                                                  
***********************************************************************         
* DAYWGHT0 - WEIGHT BY THE NUMBER OF DAYS                            *          
***********************************************************************         
DAYWGHT0 SR    R0,R0               R0 COUNTS # OF DAYS                          
         SR    RF,RF                                                            
         IC    RF,PDDAY                                                         
*        CLI   PDSOURCE+4,C'T'     TIME PERIOD REQUEST                          
         CLI   DBFUNCT,DBGETDEM                                                 
         BNE   *+8                                                              
         IC    RF,PDDAY+1          DAY BITS (FROM RECORD)                       
*                                                                               
         ZIC   RE,DBSELDAY         REQUESTED DAYS                               
         NR    RF,RE               RF=OVERLAP REQUESTED DAYS WITH DAYS          
*                                     FROM RECORD                               
         SLL   RF,1                HIGH-ORDER-BIT NOT USED IN DAY CODE          
DAYWGH1  LTR   RF,RF                                                            
         BZ    DAYWGH5                                                          
         SR    RE,RE                                                            
         SLDL  RE,1                                                             
         AR    R0,RE                                                            
         B     DAYWGH1                                                          
DAYWGH5  LR    R1,R0                                                            
         SR    R0,R0                                                            
         MVC   FULL,PDWEIGHT                                                    
         M     R0,FULL                                                          
         STCM  R1,15,PDWEIGHT                                                   
DAYWGHX  B     XITS3                                                            
         EJECT                                                                  
***********************************************************************         
* SETCBL - SET TYPE OF REQUEST FOR CABLE                              *         
***********************************************************************         
SETCBL0  DS    0H                                                               
*                                                                               
         MVI   DBSELPTT,C'P'       DEFAULT TO PROGRAM                           
         L     RE,APODINPL                                                      
         ZIC   RF,0(RE)                                                         
*                                                                               
SETC06   CLC   1(4,RE),=C'CPRO'                                                 
         BE    SETC08                                                           
         CLC   1(4,RE),=C'LTRA'                                                 
         BE    SETC08                                                           
         CLC   1(4,RE),=C'TRAC'                                                 
         BE    SETC08                                                           
         LA    RE,10(RE)                                                        
         BCT   RF,SETC06                                                        
         B     SETC10                                                           
*                                                                               
SETC08   MVI   DBSELPTT,C'T'       NORMALLY GETS TRACK                          
*                                                                               
SETC10   CLI   PDBSTOPT,C'A'       TELECAST OPTION FOR CABLE                    
         BNE   *+8                                                              
         MVI   DBSELPTT,C'E'                                                    
*                                                                               
* FOR CABLE NAD/MVGO                                                            
* TO CORRECT THE WEIGHT PROBLEM WHEN THERE IS A FILTER ON DAY OR TIME,          
* FORCE A PROGRAM AVG REQUEST TO A TRACK REQUEST                                
*                                                                               
SETC20   CLC   =C'CNAD ',PDSOURCE   FOR MONTHLY CABLE NAD,                      
         BE    *+10                                                             
         CLC   =C'CNAW ',PDSOURCE   AND WEEKLY CABLE NAD OR MVGO                
         BNE   SETCBLX                                                          
*                                                                               
         CLI   ALLDT,3              FOR ALL DAYS/ALL TIMES                      
         BE    SETCBLX                                                          
*                                                                               
         TM    OPTIND,OPTCNADP      TEST DDS ONLY OPTION TO REPORT              
         BO    SETCBLX              ON PROGRAM AVERAGE                          
*                                                                               
         CLI   DBSELPTT,C'P'        IF PROGRAM AVG REQUEST                      
         BNE   SETCBLX                                                          
*                                                                               
         MVI   DBSELPTT,C'T'        FORCE TO TRACK REQUEST                      
         B     SETCBLX                                                          
*                                                                               
SETCBLX  DS    0H                                                               
         CLI   PODBMED,C'N'        NETWORK CABLE                                
         BNE   CABDTX                                                           
*                                                                               
*---SET 'ODYT' EXTENSION FOR DAYS AND TIMES                                     
*                                                                               
CABDYTM0 DS    0H                   NOW ADD DAY/TIME TABLE TO                   
         LA    R5,ODYTEXT           LIST STARTING AT DBEXTEND                   
         XC    ODYTEXT,ODYTEXT                                                  
         USING DBXTLD,R5                                                        
         MVC   DBXTLID,=C'ODYT'                                                 
         LA    RE,DBXTLIST                                                      
         LA    RF,PODDAYTM                                                      
CABDT5   CLI   0(RF),X'99'           END OF DAY/TIME LIST                       
         BE    CABDT10                                                          
         MVC   0(L'DBXTLIST,RE),0(RF)                                           
*                                                                               
         CLI   0(RE),X'FF'         CHECK DAYS = ALL                             
         BNE   *+8                                                              
         MVI   0(RE),X'7F'         SET ALL DAYS TO M-S                          
         CLI   1(RE),X'FF'         CHECK TIME = ALL                             
         BNE   CABDT7                                                           
         MVC   1(2,RE),=H'600'     SET ALL TIME TO 6-545                        
         MVC   3(2,RE),=H'545'                                                  
         CLC   =C'MXM ',PDSOURCE                                                
         BNE   *+10                                                             
         MVC   3(2,RE),=H'559'     6-559 FOR MXM                                
*                                                                               
CABDT7   LA    RE,L'DBXTLIST(RE)                                                
         LA    RF,L'DBXTLIST(RF)                                                
         B     CABDT5                                                           
*                                                                               
CABDT10  DS    0H                                                               
         LA    R5,ODYTEXT                                                       
         GOTOR SUBR02,DMCB,('PUTEXTE',(RC)),(R5)                                
         MVI   ODYTSET,C'Y'                                                     
CABDTX   B     XITS3                                                            
         EJECT                                                                  
***********************************************************************         
*WTCNADP (WEIGHT CABLE NAD PROGRAM-AVG)                                         
*FOR MONTHLY CABLE NAD,USE TOTAL DURATION IF PROG REQUEST FOR DALL/TALL         
*DEVIDE TOTL DURATION BY NO OF ACTIVE WEEKS AND ADD REMAINDER TO 1ST WK         
***********************************************************************         
WTCNADP0 DS    0H                                                               
         CLC   =C'CNAD ',PDSOURCE   CABLE NAD MONTHLY                           
*        BE    *+10                                                             
*        CLC   =C'CNAW ',PDSOURCE   CABLE NAD WEEKLY OR MVGO                    
         BNE   WTCNADX                                                          
         CLI   DBFUNCT,DBGETNTI     MAKE SURE PROG READ                         
         BNE   WTCNADX              (COULD BE TP READ IF NO PROG DATA           
*                                    FOR STATION)                               
         CLI   DBSELPTT,C'P'        FOR PROG AVG REQUESTS                       
         BNE   WTCNADX                                                          
*                                                                               
         CLI  ALLDT,3               FOR ALL DAY/TIME REQUESTS                   
         BNE  WTCNADX                                                           
*                                                                               
         ICM   RE,15,DBAQUART                                                   
         CLI   0(RE),X'0F'          START OF LEVEL ONLY                         
         BE    WTCNAD2                                                          
         MVC   PDWEIGHT,HEADWT      OTHERWISE CONTINUATION RECD                 
         B     WTCNADX              USE WEIGHT FROM THE HEAD RECORD             
*                                                                               
WTCNAD2  CLI   0(RE),0                                                          
         BE    WTCNADX                                                          
         CLI   0(RE),X'10'                                                      
         BE    WTCNAD5                                                          
         BH    WTCNADX                                                          
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     WTCNAD2                                                          
*                                                                               
         USING PHTELEM,RE                                                       
WTCNAD5  SR    R1,R1                                                            
         ICM   R1,3,PHTCOVR        CABLE HAS REAL DUR HERE                      
         DROP  RE                                                               
*                                                                               
         LA    R0,5                5 WEEKS MAX IN A MONTH                       
         SR    RF,RF               RF=NO OF ACTIVE WEEKS                        
         MVI   FSTWKNO,0           STORE 1ST ACTIVE WEEK HERE                   
*                                                                               
         LA    RE,PDWEEK                                                        
WTCNAD7  TM    0(RE),X'F0'         NUMERIC?                                     
         BNO   WTCNAD9                                                          
         LA    RF,1(RF)            YES. UPDATE NO OF WEEKS                      
         CLI   FSTWKNO,0                                                        
         BNE   *+8                                                              
         STC   RF,FSTWKNO          SAVE 1ST ACTIVE WEEK                         
WTCNAD9  LA    RE,1(RE)                                                         
         BCT   R0,WTCNAD7                                                       
*                                                                               
         LTR   RF,RF                                                            
         BZ    WTCNADX                                                          
*                                                                               
         SR    R0,R0                                                            
         DR    R0,RF              TOTAL DURATION/NO OF ACTIVE WEEKS             
*                                                                               
         LR    R2,R1              SAVE QUOTIENT                                 
         GOTOR SUBR03,DMCB,('GETCURRE',(RC))                                    
         CLC   CURRWKNO,FSTWKNO                                                 
         BNE   *+6                                                              
         AR    R2,R0              ADD REMAINDER TO 1ST WEEK                     
         STCM  R2,15,PDWEIGHT                                                   
         MVC   HEADWT,PDWEIGHT    SAVE WEIGHT OF HEAD RECORD TO BE USED         
*                                 WITH CONTINUATION RECORDS                     
WTCNADX  B     XITS3                                                            
         EJECT                                                                  
***********************************************************************         
* GETCURR                                                                       
* GET THE WEEK NUMBER OF THE CURRENT BOOK. STORE IT IN CURRWKNO                 
* ANACWKS IS A(NACWKS)                                                          
***********************************************************************         
GETCURR0 DS    0H                                                               
         L     RE,ANACWKS                                                       
         USING NEWKSD,RE           MONTH TO WEEK TABLE                          
GETCR1   CLC   DBSELBK(1),NEWKSYR  COMPARE ON YEAR                              
         BNE   GETCR2                                                           
         CLC   DBSELBK+1(1),NEWKSLST   COMPARE ON END WK OF THE MONTH           
         BH    GETCR2                                                           
         CLC   DBSELBK+1(1),NEWKSFRS                                            
         BL    GETCR2                                                           
         ZIC   R1,DBSELBK+1        REQUESTED WEEK                               
         ZIC   R0,NEWKSFRS         FIRST WEEK OF THE MONTH                      
         SR    R1,R0                                                            
         AHI   R1,1                                                             
         STC   R1,CURRWKNO          WEEK NUMBER: 1 THROUGH 8                    
         B     GETCRX                                                           
GETCR2   LA    RE,NEWKSQ(RE)                                                    
         CLI   0(RE),X'FF'                                                      
         BNE   GETCR1                                                           
         MVI   CURRWKNO,0                                                       
*                                                                               
GETCRX   B     XITS3                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
WORKD    DSECT                                                                  
*              ADCONS FOR UNADDRESSABLE STORAGE AREAS                           
AMYIO    DS    A                                                                
AMYIO2   DS    A                                                                
AMYDBLK  DS    A                                                                
AIUNCOND DS    A                                                                
ASORTREC DS    A                                                                
ASORTDEM DS    A                                                                
ASVPODEM DS    A                                                                
AEXDEMOS DS    A                                                                
AUPDEMOS DS    A                                                                
AUPBLOCK DS    A                                                                
AZZZLIST DS    A                                                                
AZZZPIV  DS    A                                                                
ACURRSYC DS    A                                                                
ASVSYSC  DS    A                                                                
*                                                                               
RELO     DS    F                                                                
MPAHOOK  DS    A                                                                
*                                                                               
KEYSAVE2 DS    CL60                                                             
GOTUTYPE DS    C                                                                
FORCEBTU DS    C                                                                
RMPPROF  DS    CL8                                                              
WEEKNUM  DS    HL1                                                              
ANACWKS  DS    A                                                                
AREG1    DS    A                                                                
AREG2    DS    A                                                                
MYMKTSTA DS    CL5                                                              
STACOUNT DS    X                                                                
SVSCBL24 DS    CL3                                                              
SVSTA    DS    CL5                                                              
SVALPSTA DS    CL4                 ALPHA FUSION STATION                         
SVSOURCE DS    CL(L'PDSOURCE)      SAVE PDSOURCE                                
PRGSTRT  DS    XL2                 START DATE FROM PROGRAM RECORD               
DAYHOLD  DS    XL1                 DAY HOLD AREA                                
TIMEHOLD DS    XL4                 TIME HOLD                                    
ENMIFLAG DS    X                   EMI/NMI FLAG                                 
CURRNET  DS    F                                                                
*                                                                               
CSFLAG   DS    XL1                 COMSCORE FLAGS                               
CSFPUTQ  EQU   X'80'               PUT RECORD TO DATASET ALREADY                
*                                                                               
CURRMRKT DS    XL2                 CURRENT MARKET CODE                          
FSTWKNO  DS    HL1                 1ST ACTIVE WEEK OF A PROGRAM(1-5)            
CURRWKNO DS    HL1                 WEEK CURRENTLY PROCESSED(1-5)                
CTYNUM   DS    XL2                 COUNTY NUMBER                                
HEADWT   DS    HL4                 WEIGHT FROM THE HEAD RECORD                  
*                                   TO BE USED WITH CONTINUATION RECDS          
*                                   FOR CNAD                                    
*                                                                               
ANYUP    DS    C                   UPGRADE FLAG                                 
EBOPT    DS    C                   ESTIMATE BOOK FLAG                           
ALLDT    DS    X                   DALL/TALL FLAG                               
DDPOINT  DS    A                   NAD DEMO POINTER                             
RETADD   DS    A                   RETURN ADDRESS                               
ODYTSET  DS    C                   CABLE OPTIMIZATION FLAG                      
MAXWKS   DS    HL1                 LENGTH OF WEEK FIELD                         
*                                  (4 WKS FOR SHORT FLD,5 WKS FOR LONG)         
*                                  MATHFACS BLOCK                               
MATHFACS DS    0F                  DEMOMATH BLOCK                               
MATHABLK DS    A                   A(DBLOCK)                                    
MATHFCTR DC    F'0'                WEIGHTING FAVTOR                             
MATHIFIL DS    CL3                 INPUT FILE FORMAT                            
MATHOFIL DS    CL3                 OUTPUT FILE FORMAT                           
MATHOSRC DS    CL3                 OUTPUT SOURCE FORMAT                         
MATHFACL EQU   *-MATHFACS                                                       
*                                                                               
*                                        SAVE AREAS                             
SVDAYTME DS    CL5                 DBSELDAY/DBSELTIM                            
SVBOOK   DS    CL2                 DBSELBK                                      
SVDBFILE DS    CL3                 DBFILE                                       
SVDBLK   DS    CL(DBLOCKL)         DBLOCK                                       
SVDBLK2  DS    CL(DBLOCKL)         DBLOCK                                       
SVDBMED  DS    C                   DBSELMED                                     
SVNTI    DS    XL2                 NTI                                          
SVDBTYPE DS    C                   DBTYPE                                       
SVPODBOK DS    F                   CURRENT PODBOOK                              
SVDBAREC DS    A                   DBAREC                                       
SVCRDATE DS    H                   CREATION DATE                                
SVSTAT   DS    XL5                 STATION                                      
SVKEY    DS    CL30                KEY                                          
SVPDDEMS DS    XL48                PDDEMOS                                      
SVPODDEM DS    CL(PDNDEMS)         PODDEMO                                      
SVBTUAQ  DS    F                   A(QUARTER HOUR ELEMENT)                      
SVPDCODE DS    CL6                 PROGRAM CODE                                 
*                                                                               
SVDUROPT DS    CL1                                                              
SVBASE   DS    CL1                 PDBASE                                       
SVDAYSOP DS    CL1                 SV DAYSOPT                                   
SVVAROPT DS    CL1                 SV VAROPT                                    
SVPODBD  DS    CL(PODBLNQ)                                                      
CURSRC   DS    CL5                 CURRENT SOURCE                               
SVFLAG   DS    XL1                 SAVED FLAG                                   
*                                                                               
COUNTER  DS    PL2                                                              
AVIEWTYP DS    A                   POINTER TO CURRENT VIEWING TYPE              
*                                                                               
* DBEXTENDS                                                                     
RINVEXT  DS    CL162               REP EXTENSION BLOCK ('RINV')                 
SPOTEXT  DS    CL128               SPOT EXTENSION BLOCK ('SPOT')                
UFILEXT  DS    CL12                USER FILE EXTENSION BLOCK ('UFIL')           
NETWEXT  DS    CL128               NETWORK EXTENSION BLOCK ('NETW')             
ODYTEXT  DS    CL59                OPTIMIZATION DAY/TIME LIST ('ODYT')          
CSRNEXT  DS    CL150               COMINTER PARAM BLOCK ('CSRN')                
*                                                                               
MPADEMS  DS    (PDNDEMS)F                                                       
MPAUNIVS DS    (PDNDEMS)F                                                       
REALDEMS DS    XL(PDNDEMS)'FF'      ORIGINAL 22 DEMOS                           
         DS    CL256               LEAVE THIS UNTIL I FIGURE IT OUT             
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
DBLOCKL  EQU   *-DBLOCK                                                         
         EJECT                                                                  
*--EVN RECORD LAYOUT FOR PROGRAM LOOKUPS                                        
PIO      DS    0CL1381             AREA TO BUILD EVN RECORDS                    
         DS    CL22                (PHONY KEY)                                  
PUEL     DS    CL179               UNIVERSE ELEMENT                             
PVEL     DS    CL119               VPH ELEMENT                                  
PREL     DS    CL9                 RATING/HUT/SHARE ELEMENT                     
PBEL     DS    CL7                 BOOK ELEMENT                                 
PDEL     DS    CL1044              DD ELEMENTS, NAD OVERIDES                    
PIOEOR   DS    CL1                 END OF RECORD                                
         ORG   PIO+1381                                                         
*                                                                               
*        UNADDRESSABLE STORAGE                                                  
*                                                                               
         DS    0D                  ALIGN DBLOCK                                 
MYDBLK   DS    CL(DBLOCKL)         MY DBLOCK                                    
MYIO     DS    CL2000              MY IOAREA                                    
MYIO2    DS    CL2000              MY IOAREA                                    
IUNCOND  DS    XL1000              IUN DEMAINT CONDENSE AREA                    
SORTREC  DS    CL(SORTDLEN)                                                     
SVPODEM  DS    CL(L'PODDEMO)       PODDEMO SAVE AREA                            
EXDEMOS  DS    CL(L'PODDEMO)       EXTRACTION DEMOS                             
UPDEMOS  DS    CL(L'PODDEMO)       UPGRADE DEMOS                                
SORTDEM  DS    CL(IUNWKLEN)        SORTED DEMOS                                 
UPBLOCK  DS    CL(SPDEMUP2)        UPGRADE BLOCK                                
ZZZLIST  DS    CL3400              LIST OF ZZZ STATIONS                         
ZZZPIV   DS    CL3400                                                           
SVSYCDS  DS    XL(PDSYSCDL)                                                     
SVSYCDSL EQU   *-SVSYCDS                                                        
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
       ++INCLUDE NEPODFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEPODE0D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE NEGETNUND                                                      
         EJECT                                                                  
       ++INCLUDE NEPODWORK                                                      
         EJECT                                                                  
       ++INCLUDE NEPODBOOK                                                      
         EJECT                                                                  
SORTDATA DSECT                                                                  
SORTDPT  DS    CL7                                                              
SORTTYP  DS    CL7                                                              
SORTPER  DS    CL5                                                              
SORTUPDT DS    CL16                                                             
SORTPROG DS    CL16                                                             
SORTDP   DS    CL5                                                              
SORTDAY  DS    CL2                                                              
SORTTIME DS    F                                                                
SORTMKT  DS    CL16                                                             
SORTSTAT DS    CL16                                                             
SORTEFF  DS    CL3                                                              
SORTDLEN EQU   *-SORTDATA                                                       
         EJECT                                                                  
IUNDEMS  EQU   32                                                               
*                                                                               
IUNWK    DSECT                                                                  
*                                                                               
IUNVS    DS    (IUNDEMS)F                                                       
*                                                                               
IUNOLD   EQU   *                                                                
IRTGOLD  DS    (IUNDEMS)F                                                       
IIMPOLD  DS    (IUNDEMS)F                                                       
IPUTOLD  DS    (IUNDEMS)F                                                       
ITOTOLD  DS    (IUNDEMS)F                                                       
*                                                                               
IUNLEN   EQU   *-IUNWK+12          UNVS/RTGS/IMPS/PUTS/TOTS + VUTS              
*                                                                               
IUNNEW   EQU   *                                                                
IRTGNEW  DS    (IUNDEMS)F                                                       
IIMPNEW  DS    (IUNDEMS)F                                                       
IPUTNEW  DS    (IUNDEMS)F                                                       
ITOTNEW  DS    (IUNDEMS)F                                                       
*                                                                               
IUNXTRA  EQU   *                                                                
IUNSHMS  DS    F                                                                
IUNSMETA DS    F                                                                
IUNSMETB DS    F                                                                
*                                                                               
ILUNVS   DC    (IUNDEMS)F'0'                                                    
IUNWKLEN EQU   *-IUNWK                                                          
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DEDBEXTRAD                                                     
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENPROG                                                      
SRBLKD   DSECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE SPDEMUPD                                                       
*       +INCLUDE SPGENUNIVA                                                     
       ++INCLUDE NEGENPRG                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE RERMPPROF                                                      
       ++INCLUDE REGENREPA                                                      
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAUTL                                                          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDMONYREQU                                                     
       ++INCLUDE COMINTERD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'244NEPOD01   09/17/20'                                      
         END                                                                    
