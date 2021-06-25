*          DATA SET REREP3Y02  AT LEVEL 109 AS OF 01/12/11                      
*PHASE RE3Y02C                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE UNBOOK                                                                 
         TITLE 'RE3Y02 - REREP3Y02 - MEDIA OCEAN DEMO FEED'                     
**********************************************************************          
*********************************************************************           
*                                                                   *           
*   REREP3Y02 - RE3Y02  - MEDIA OCEAN DEMO FEED                     *           
*                                                                   *           
*        KATZ "CONTINENTAL" (CQ) ONLY                               *           
*                                                                   *           
*********************************************************************           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
*    JUN/10  (SMY) --- NEW INVENTORY RECORD KEY                     *           
*                                                                   *           
*    MAY/10  (SMY) --- ADD MORE STATIONS                            *           
*                                                                   *           
*    APR/10  (SMY) --- ADD HANDLING OF "PARENT PLUS" STATIONS       *           
*                        (5TH POSITION NOT T)                       *           
*                                                                   *           
*   DEC22/09 (SMY) --- SPECIAL EXTRACT FOR KATZ "CQ"                *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*    QUESTOR+1 = Y   -  DISPLAY OUTPUT RECORDS                      *           
*    QUESTOR+1 = ?   -  DISPLAY BOOK TABLE LIST                     *           
*    QUESTOR+2 = TST -  USE TST INPUT                               *           
*    QUESTOR+2 = XXX -  USE REQUEST STATION IN TST INPUT            *           
*    QUESTOR+5 = TST -  LOAD T00AA4A AS TEST                        *           
*    QUESTOR+5 = TSB -  LOAD T00AA4B AS TEST                        *           
*                                                                   *           
*********************************************************************           
*                                                                               
RE3Y02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 OVERWRKQ,**RE3Y02,R8,RR=RE                                       
*                                                                               
         LR    R9,RC               A(THIS MOD'S WORKSPACE=OVERWRKQ)             
         USING OVERWRKD,R9                                                      
*                                                                               
         ST    RE,RELO             SET RELOCATABLE ADDRESS                      
*                                                                               
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         MVI   PAGE,1                                                           
         MVI   MAXLINES,58                                                      
         L     RC,FILEC            A(FILCON WORKSPACE)                          
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2 -> C                             
*                                                                               
*&&DO                                                                           
         L     RF,ADCONLST         RETRIEVE A(COMFACS)                          
         USING ADCONSD,RF                                                       
         MVC   XCOMFACS,VCOMFACS    A(COMFACS)                                  
*                                                                               
         DROP  RF                                                               
*  TEST                                                                         
         L     R3,XCOMFACS                                                      
         LA    R4,1                                                             
         LA    R5,84                                                            
TEST0020 EQU   *                                                                
         MVC   P+1(07),=C'VCOMFAC'                                              
         EDIT  (R4),(3,P+9)                                                     
         GOTO1 HEXOUT,DMCB,0(R3),P+16,4,=C'TOG'                                 
         GOTO1 REPORT                                                           
         LA    R3,4(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,TEST0020                                                      
*   TEST END                                                                    
*&&                                                                             
         MVC   XCOMFACS,ACOMFACT    A(REFILCON COMFACS)                         
*                                                                               
*   THERE IS A COMPLETE SCREWUP IN COMFACS.  THE LINKED COMFACS                 
*        IS NOT BIG ENOUGH TO HANDLE THE ADDITIONAL REFERENCES.                 
*        AT THIS TIME, I CAN'T FIND WHERE THAT IS BEING SET UP.                 
*        'REFILCON,' THE REP REPORTER FILE CONTROLLER, IS PASSING               
*        IN A COMFACS OF THE APPROPRIATE SIZE, BUT IT DOESN'T HAVE              
*        ALL THE REQUIRED ADDRESSES.  THE FIRST 74 ENTRIES OF                   
*        VCOMFACS ARE BEING STUFFED INTO THE REFILCON COMFACS,                  
*        AND THAT IS BEING PASSED INTO THE DEMO ROUTINES.                       
*                                                                               
         BAS   RE,STUFCFAC         INSERT ADDITIONAL REFERENCES                 
*&&DO                                                                           
*  TEST                                                                         
         L     R3,XCOMFACS                                                      
         LA    R4,1                                                             
         LA    R5,84                                                            
TEST0040 EQU   *                                                                
         MVC   P+1(07),=C'ACOMFAC'                                              
         EDIT  (R4),(3,P+9)                                                     
         GOTO1 HEXOUT,DMCB,0(R3),P+16,4,=C'TOG'                                 
         GOTO1 REPORT                                                           
         LA    R3,4(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,TEST0040                                                      
*   TEST END                                                                    
*&&                                                                             
         L     R3,XCOMFACS         LOAD V(DEMO MODS)                            
         USING COMFACSD,R3                                                      
         L     RF,ADCONLST                                                      
         L     RF,VCOMFACS-ADCONSD(,RF)                                         
VCOMFAC  USING COMFACSD,RF                                                      
         MVC   CDEMEL,VCOMFAC.CDEMEL                                            
         MVC   CDEMOMTH,VCOMFAC.CDEMOMTH                                        
         MVC   CDEMADDR,VCOMFAC.CDEMADDR                                        
         MVC   CDEMTABS,VCOMFAC.CDEMTABS                                        
         DROP  VCOMFAC                                                          
*                                                                               
         DROP  R3                                                               
*   TEST                                                                        
***      MVC   P+1(05),=C'MODE='                                                
***      MVC   P+6(1),MODE                                                      
***      GOTO1 REPORT                                                           
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         B     MAIN30              ZERO IS GOOD RETURN                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     MODEEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     MODEEXIT                                                         
MODEEXIT LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(INITIAL)  ALL PROCESSING DONE HERE              
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
***      DC    AL1(PROCCONT),AL3(POST)                                          
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*                                                                               
*   STUFCFAC:  COPY ADDRESSES FROM ONE COMFACS TO ANOTHER.                      
*                                                                               
STUFCFAC NTR1                                                                   
         L     R1,XCOMFACS         A(REFILCON COMFACS)                          
         L     RF,ADCONLST         RETRIEVE A(COMFACS)                          
         USING ADCONSD,RF                                                       
         L     R2,VCOMFACS         A(COMFACS)                                   
         DROP  RF                                                               
         LA    R0,74               MOVE FIRST 74 ENTRIES                        
STUF0020 EQU   *                                                                
         OC    0(4,R2),0(R2)       ANY VCOMFACS ENTRY?                          
         BZ    STUF0080            NO  - SKIP IT                                
         OC    0(4,R1),0(R1)       AND REFIL COMFACS ENTRY?                     
         BNZ   STUF0080            YES - DON'T REPLACE IT                       
         MVC   0(4,R1),0(R2)       INSERT VCOMFACS INTO REFIL COMFAC            
STUF0080 EQU   *                                                                
         LA    R2,4(R2)            BUMP VCOMFACS                                
         LA    R1,4(R1)            BUMP REFIL COMFAC                            
         BCT   R0,STUF0020         GO BACK FOR NEXT                             
         XIT1                                                                   
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
*                                                                               
INITIAL  NTR1                                                                   
*                                                                               
*   OPEN DEMO FILES TO RETRIEVE STATION MARKET NUMBER                           
*                                                                               
*                                                                               
****     GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),DMSYS,DMFLIST                        
*                                                                               
*   TEST                                                                        
****     MVC   P+1(15),=C'DEMO FILES OPEN'                                      
****     GOTO1 REPORT                                                           
*                                                                               
         B     MAIN0000                                                         
*                                                                               
DMSYS    DC    CL8'REP'                                                         
DMFLIST  DC    0C                                                               
         DC    C'NDEMDIRN'                                                      
         DC    C'NL=DEMFN'                                                      
         DC    C'NPAVDIR '                                                      
         DC    C'NL=PAVFL'                                                      
         DC    C'X'                                                             
*                                                                               
         DS    0F                                                               
MAIN0000 EQU   *                                                                
         XC    PUTCTR,PUTCTR       CLEAR RECORD COUNTER                         
         XC    LINCTR,LINCTR       CLEAR LINE   COUNTER                         
         L     RE,=A(DOPUT)                                                     
         A     RE,RELO                                                          
         ST    RE,VDOPUT                                                        
*                                  SET A(IOAREAS)                               
         LR    RE,R9               SET A(THIS MOD'S WORKSPACE)                  
         AH    RE,=Y(IOAREA1-OVERWRKD)                                          
         ST    RE,AIOREC           SET RECORD IO AREA                           
         ST    RE,AIO1             SET GENERAL IO AREA 1                        
         AH    RE,=Y(LENIO)                                                     
         ST    RE,AIO2             SET GENERAL IO AREA 2                        
         AH    RE,=Y(LENIO)                                                     
         ST    RE,AIO3             SET GENERAL IO AREA 3                        
         AH    RE,=Y(LENIO)                                                     
         ST    RE,AIO4             SET GENERAL IO AREA 4                        
*                                                                               
*                                                                               
         GOTOX LOADER,DMCB,=CL8'T00ADD',0   DEMAND                              
         OC    DMCB+4,DMCB+4       LOADED OKAY?                                 
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - KILL JOB                               
*                                                                               
         MVC   VDEMAND,4(R1)       SET A(DEMAND)                                
         GOTOX LOADER,DMCB,=CL8'T00AD9',0   DEMOVAL                             
         OC    DMCB+4,DMCB+4       LOADED OKAY?                                 
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - KILL JOB                               
*                                                                               
         MVC   VDEMOVAL,4(R1)      SET A(DEMOVAL)                               
         GOTOX LOADER,DMCB,=CL8'T00ADF',0   DEMOUT                              
         OC    DMCB+4,DMCB+4       LOADED OKAY?                                 
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - KILL JOB                               
*                                                                               
         MVC   VDEMOUT,4(R1)       SET A(DEMOUT)                                
         GOTOX LOADER,DMCB,=CL8'T00AE0',0   DEMOCON                             
         OC    DMCB+4,DMCB+4       LOADED OKAY?                                 
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - KILL JOB                               
*                                                                               
         MVC   VDEMOCON,4(R1)      SET A(DEMOCON)                               
         L     R3,XCOMFACS         LOAD V(DEMO MODS)                            
         USING COMFACSD,R3                                                      
         MVC   CDEMAND,VDEMAND     SET COMFACS                                  
         MVC   CDEMOUT,VDEMOUT                                                  
         DROP  R3                                                               
*                                                                               
         GOTOX LOADER,DMCB,=CL8'T00A0F',0   DAYUNPK                             
         OC    DMCB+4,DMCB+4       LOADED OKAY?                                 
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - KILL JOB                               
         MVC   DAYUNPK,4(R1)       SET A(DAYUNPK)                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        MAIN PROCESSING                                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(2,TODAYDT)                                 
*                                  GET TODAY'S DATE 2-BYTE COMPRESSED           
*                                                                               
         OPEN  (STAFILE,(OUTPUT))  OPEN STATION FILE                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (DPTFILE,(OUTPUT))  OPEN DAYPART FILE                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (HD1FILE,(OUTPUT))  OPEN HD01REC FILE                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (HD2FILE,(OUTPUT))  OPEN HD02REC FILE                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,STATSOUT         OUTPUT STATION RECORDS                       
*                                                                               
         BAS   RE,DYPTSOUT         OUTPUT DAYPART RECORDS                       
*                                                                               
         BAS   RE,HD01OUT          OUTPUT HD01 AND HD02 RECORDS                 
*                                                                               
         CLOSE (STAFILE,)          CLOSE STATION FILE                           
         CLOSE (DPTFILE,)          CLOSE DAYPART FILE                           
         CLOSE (HD1FILE,)          CLOSE HD01REC FILE                           
         CLOSE (HD2FILE,)          CLOSE HD02REC FILE                           
*                                                                               
         OC    PUTCTR,PUTCTR       ANY OUTPUT FOR THIS JOB?                     
         BZ    INIT0060            NOTHING WRITTEN TO FILE                      
*                                                                               
*****    GOTO1 REPORT                                                           
         B     INIT0200                                                         
INIT0060 EQU   *                                                                
         MVC   P+01(26),=C'NO DATA WAS OUTPUT       :'                          
         MVC   P+28(26),=C'RECORDS WRITTEN =        :'                          
         EDIT  PUTCTR,(9,P+47)                                                  
         GOTO1 REPORT                                                           
         MVC   P+01(26),=C'NO OUTPUT FILE QUEUED FOR '                          
         MVC   P+27(26),=C'DOWNLOADING TO MEDIA OCEAN'                          
         GOTO1 REPORT                                                           
                                                                                
INIT0200 EQU   *                                                                
         XIT1                                                                   
*                                                                               
*                                                                               
*********************************************************************           
* STATSOUT - OUTPUT STAT RECORDS                                                
*                                                                               
*********************************************************************           
STATSOUT NTR1                                                                   
         LA    R0,INPNUMQ          NUMBER OF STATIONS IN TABLE                  
         LA    R2,INPBLOCK         START OF STATIONS                            
STAT0020 EQU   *                                                                
         MVC   MYP,SPACES          CLEAR O/P DATA LINE                          
         MVC   MYP+STATCTL(4),=C'STAT'     INSERT CONTROL ID                    
*                                                                               
         MVC   MYP+STATPWR(2),=C'CQ'       INSERT REP CODE WANTED               
*                                                                               
*              IF 5TH POSITION OF STATION IS T INSERT ONLY                      
*               FIRST 4 CHARACTERS OF STATION CALL LETTERS                      
         MVC   MYP+STATSTA(INPLNQ-1),0(R2)                                      
         CLI   4(R2),C'T'                                                       
         BE    *+10                                                             
         MVC   MYP+STATSTA(INPLNQ),0(R2)  FULL FIVE CHARACTERS                  
*                                                                               
         LA    R3,STAFILE                                                       
         GOTO1 VDOPUT              OUTPUT THE RECORD                            
*                                                                               
         LA    R2,INPLNQ(R2)       POINT TO NEXT STATION                        
         BCT   R0,STAT0020         GO BACK FOR NEXT                             
STAT0800 EQU   *                                                                
         XIT1                                                                   
*********************************************************************           
* DYPTSOUT - OUTPUT DYPT RECORDS                                                
*                                                                               
*********************************************************************           
DYPTSOUT NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'3C'           INSERT RECORD TYPE                           
         MVC   KEY+24(2),=C'CQ'    INSERT REP CODE FOR THIS EXTRACT             
         GOTO1 HIGH                                                             
         B     DYPT0040                                                         
DYPT0020 EQU   *                                                                
         GOTO1 SEQ                                                              
DYPT0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME RECTYPE/REP?                            
         BNE   DYPT0800            NO  - FINISHED                               
         GOTO1 GETRCRD             RETRIEVE RECORD                              
         L     R2,AIOREC           SET A(IOAREA)                                
         USING RRDPREC,R2                                                       
         MVC   MYP,SPACES          CLEAR O/P DATA LINE                          
         MVC   MYP+DYPTCTL(4),=C'DYPT'     INSERT CONTROL ID                    
*                                                                               
         MVC   MYP+DYPTPWR(2),RRDPKREP     INSERT REP CODE                      
*                                                                               
         MVC   MYP+DYPTDPT(1),RRDPKDPT     INSERT DAYPART CODE                  
         MVC   MYP+DYPTDPT3(3),RRDPSNAM    INSERT SHORT DYPT NAME               
         MVC   MYP+DYPTDPTL(15),RRDPLNAM   INSERT LONG  DYPT NAME               
*                                                                               
         LA    R3,DPTFILE                                                       
         GOTO1 VDOPUT              OUTPUT THE RECORD                            
*                                                                               
         B     DYPT0020            GO BACK FOR NEXT                             
DYPT0800 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
*********************************************************************           
* HD01OUT - OUTPUT HD01 RECORDS                                                 
*                                                                               
*  READ ALL INVENTORY HEADER RECORDS FOR A SINGLE REP (CQ HERE) AND             
*  SELECT ONLY THOSE WHOSE STATIONS EXIST ON THE INPBLOCK TABLE                 
*  OF STATIONS SUPPLIED BY THE AGENCY                                           
*                                                                               
*********************************************************************           
HD01OUT  NTR1                                                                   
         LA    R5,KEY                                                           
         USING INVRECD,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RINVKEY,RINVKTYQ        INSERT RECORD TYPE                       
*                                                                               
         MVC   RINVKREP,=C'CQ'         INSERT REP CODE WANTED                   
*                                                                               
         GOTO1 HIGH                                                             
         B     HD010080                                                         
HD010020 EQU   *                                                                
         GOTO1 SEQ                                                              
HD010080 EQU   *                                                                
*TESTING BELOW                                                                  
*****    L     RF,PUTCTR                                                        
*****    CHI   RF,1000                                                          
*****    BH    HDOVER              TESTING LIMITED                              
*TESTING ABOVE                                                                  
         CLC   KEY(RINVKSTA-RINVKEY),KEYSAVE     SAME RECTYPE/REP?              
         BNE   HDOVER              NO  - FINISHED                               
         OC    RINVKRTP,RINVKRTP   INVENTORY HEADER ?                           
         BNZ   HD010020            NO - NEXT                                    
*                                                                               
         LA    R0,INPNUMQ          # OF STATIONS IN TABLE                       
         LA    R2,INPBLOCK         START OF TABLE                               
HD1SLUP  EQU   *                                                                
         CLC   RINVKSTA(INPLNQ),0(R2)   STATION WE WANT ?                       
         BE    HD1SLUPX                 YES                                     
         LA    R2,INPLNQ(R2)            NEXT STATION TO TEST                    
         BCT   R0,HD1SLUP                                                       
         B     HD010020                 STATION NOT FOUND - SKIP REC            
*                                                                               
         DROP  R5                                                               
*                                                                               
HD1SLUPX EQU   *                                                                
         GOTO1 GETRCRD             RETRIEVE RECORD                              
         L     R6,AIOREC                                                        
         USING INVRECD,R6                                                       
*                                                                               
* BELOW SELECTS ONLY RECORDS CONTAINING A SPECIFIC DAY-PART CODE                
         LA    R0,6                # OF DAY-PART CODES                          
         LA    R2,RINVDP           START OF DAY-PART CODES                      
HD1DLUP  EQU   *                                                                
         CLI   0(R2),DAYPTALL      CONTINENTAL "ALL" (X) DAY-PART ?             
         BE    HD1DLUPX            YES - KEEP                                   
         LA    R2,1(R2)            NEXT DAY-PART CODE                           
         BCT   R0,HD1DLUP                                                       
         B     HD010020            Z DAY-PART CODE NOT FOUND - SKIP REC         
*                                                                               
HD1DLUPX EQU   *                                                                
* ABOVE SELECTS ONLY RECORDS CONTAINING A SPECIFIC DAY-PART CODE                
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2  EFF END DATE NULLS ?                   
         BZ    HD010100                  YES - KEEP                             
         CLC   TODAYDT,RINVPEFF+2        EFF END DATE GE TODAY'S DATE?          
         BH    HD010020                  NO - NEXT                              
*                                                                               
HD010100 EQU   *                                                                
         MVC   MYP,SPACES          CLEAR O/P DATA LINE                          
         MVC   MYP+HD01CTL(4),=C'HD01'     INSERT CONTROL ID                    
*                                                                               
         MVC   MYP+HD01PWR(L'RINVKREP),RINVKREP     INSERT REP CODE             
*                                                                               
*              IF 5TH POSITION OF STATION IS T INSERT ONLY                      
*               FIRST 4 CHARACTERS OF STATION CALL LETTERS                      
         MVC   MYP+HD01STA(INPLNQ-1),RINVKSTA                                   
         CLI   RINVKSTA+4,C'T'                                                  
         BE    *+10                                                             
         MVC   MYP+HD01STA(INPLNQ),RINVKSTA  FULL FIVE CHARACTERS               
*                                                                               
         MVC   MYP+HD01INV(L'RINVKINV),RINVKINV   INSERT INVENTORY NUM          
*                                                                               
         LA    R2,RINVPEFF                                                      
         BRAS  RE,STDTRECD         O/P HD01STDT (EFFECTIVE START DATE)          
         MVC   MYP+HD01STDT(8),WORK+32     INSERT START DATE                    
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2   END DATE NULLS ?                      
         BZ    HD010150                YES                                      
         LA    R2,RINVPEFF+2                                                    
         BRAS  RE,ENDTRECD         O/P HD01ENDT (EFFECTIVE END   DATE)          
         MVC   MYP+HD01ENDT(8),WORK+32     INSERT END   DATE                    
*                                                                               
HD010150 EQU   *                                                                
         MVC   MYP+HD01DYPT(6),RINVDP      INSERT DAY-PART CODES                
         OC    MYP+HD01DYPT(8),SPACES      CLEAR ANY BIN ZERO                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RINVPEL-RINVREC(R6)                                           
*                                                                               
HD01LUP1 EQU   *                                                                
         CLI   0(R6),0             END OF REC ?                                 
         BE    HD010700            YES - OUTPUT RECORD                          
         CLI   0(R6),X'02'         DAY TIME ELEMENT ?                           
         BE    HD01LUPX            YES - PROCESS                                
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     HD01LUP1            TEST NEXT ELEMENT                            
*                                                                               
HD01LUPX EQU   *                                                                
         USING RIDTELEM,R6                                                      
         MVC   WORK(20),SPACES     CLEAR WORK                                   
         GOTO1 DAYUNPK,DMCB,(0,RIDTDAY),(7,WORK)                                
*                                                                               
         MVC   MYP+HD01DAYS(7),WORK         INSERT DAYS                         
*                                                                               
         MVC   WORK(20),SPACES     CLEAR WORK                                   
         GOTO1 UNTIME,DMCB,(0,RIDTTIME),WORK                                    
         MVC   MYP+HD01TIME(11),WORK        INSERT TIMES                        
*                                                                               
         DROP  R6                                                               
*                                                                               
HD010700 EQU   *                                                                
         LA    R3,HD1FILE                                                       
         GOTO1 VDOPUT              OUTPUT THE RECORD                            
*                                                                               
         XC    MYHED,MYHED                                                      
         MVC   MYHED(HDLNTH),MYP   SAVE HD01 REC THRU END DATE AND              
         MVC   MYHED(4),=C'HD02'    SET AS HD02 RECORD                          
*                                                                               
*TESTING                                                                        
*****    DC    H'0'                                                             
*TESTING                                                                        
*                                                                               
         B     HD02LUP             GO DO HD02 OUTPUT                            
*                                                                               
HDOVER   EQU   *                   END OF JOB                                   
         XIT1                                                                   
*                                                                               
*********************************************************************           
* HD02LUP                                                                       
*  SET UP TABLE OF PROGRAM NAMES AND AVAIL DAY TIMES (UP TO 8)                  
*  FROM CURRENT RECORD                                                          
*********************************************************************           
HD02LUP  EQU   *                                                                
*                                                                               
         XCEF  AVORIDES,L'AVORIDES  CLEAR                                       
*                                                                               
         LA    R0,8                MAX NUMBER OF PROGRAM NAMES                  
         LA    R2,AVORIDES         POINT TO FIRST PROGRAM NAME                  
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RINVPEL-RINVREC(R6)                                           
*                                                                               
HD02LUP1 EQU   *                                                                
         CLI   0(R6),0             END OF REC ?                                 
         BE    HD02AVL             YES - GO DO AVAILS                           
         CLI   0(R6),X'03'         PROGRAM NAME ELEMENT ?                       
         BE    HD02LUPX            YES - PROCESS                                
HD02LUP2 EQU   *                                                                
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     HD02LUP1            TEST NEXT ELEMENT                            
*                                                                               
HD02LUPX EQU   *                                                                
         USING RIPGELEM,R6                                                      
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RIPGLEN         ELEMENT LENGTH                                
         SH    RF,=Y(RIPGNAME-RIPGELEM) PROGRAM NAME LENGTH                     
         BZ    HD02LUP2            LOOK FOR MORE                                
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),RIPGNAME    RETURN PROGRAM NAME                          
         OC    0(LAVORIDE,R2),SPACES                                            
*                                                                               
         LA    R2,LAVORIDE(R2)     NEXT PROGRAM NAME                            
         BCT   R0,HD02LUP2         LOOK FOR MORE                                
*                                                                               
         DROP  R6                                                               
*                                                                               
HD02AVL  EQU   *                                                                
*                                                                               
         LA    R0,8                MAX NUMBER OF AVAILS                         
         LA    R2,AVORIDES         POINT TO FIRST PROGRAM NAME                  
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RINVPEL-RINVREC(R6)                                           
*                                                                               
HD02ALP1 EQU   *                                                                
         CLI   0(R6),0             END OF REC ?                                 
         BE    HD02OUT             YES - GO OUTPUT NAMES/AVAILS                 
         CLI   0(R6),X'04'         AVAIL DAY TIME ELEMENT ?                     
         BE    HD02ALPX            YES - PROCESS                                
HD02ALP2 EQU   *                                                                
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     HD02ALP1            TEST NEXT ELEMENT                            
*                                                                               
HD02ALPX EQU   *                                                                
         USING RIAPELEM,R6                                                      
*                                                                               
         MVC   DAVDAY(L'RIADAY,R2),RIADAY      RETURN AVAIL DAY                 
         OC    DAVDAY(L'RIADAY,R2),SPACES                                       
         MVC   DAVTIME(L'RIATIME,R2),RIATIME   RETURN AVAIL DAY                 
         OC    DAVTIME(L'RIATIME,R2),SPACES                                     
*                                                                               
         LA    R2,LAVORIDE(R2)     NEXT AVAIL                                   
         BCT   R0,HD02ALP2         LOOK FOR MORE                                
*                                                                               
         DROP  R6                                                               
*                                                                               
*********************************************************************           
* HD02OUT - OUTPUT HD02 RECORDS BASED ON TABLE OF PROGRAM NAMES                 
*           AND AVAIL DAY TIMES FROM CURRENT RECORD (UP TO 8)                   
*********************************************************************           
HD02OUT  EQU   *                                                                
         LA    R0,8                MAX NUMBER OF PROGRAM NAMES                  
         LA    R2,AVORIDES         POINT TO FIRST NAME/DAY/TIME ENTRY           
*                                                                               
HD02OP40 EQU   *                                                                
         OC    0(LAVORIDE,R2),0(R2)    ANYTHING THERE ?                         
         BZ    HD010020                NO - HD02 DONE - NEXT RECORD             
         MVC   MYP,SPACES          CLEAR O/P DATA LINE                          
         MVC   MYP+HD02PGNM(27),DPGNAME(R2)  INSERT PROGRAM NAME                
         MVC   MYP+HD02AVDY(11),DAVDAY(R2)   INSERT AVAIL DAY                   
         MVC   MYP+HD02AVTM(11),DAVTIME(R2)  INSERT AVAIL TIME                  
*                                                                               
         MVC   MYP(HDLNTH),MYHED   FIRST PART OF HD02 RECORD                    
*                                                                               
         LA    R3,HD2FILE                                                       
         GOTO1 VDOPUT              OUTPUT RECORD                                
*                                                                               
*TESTING                                                                        
******   DC    H'0'                                                             
*TESTING                                                                        
         LA    R2,LAVORIDE(R2)     BUMP TO NEXT SLOT                            
         BCT   R6,HD02OP40         GO BACK FOR NEXT                             
*                                                                               
         B     HD010020            NEXT RECORD                                  
*                                                                               
*********************************************************************           
*        END OF MAIN PROCESSING                                                 
*********************************************************************           
         EJECT                                                                  
*                                                                               
GETCON   LA    RF,RCONREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETRCRD  L     RF,AIOREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
PUTCON   LA    RF,RCONREC                                                       
         B     PUTFILE                                                          
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*  DATA MANAGER INTERFACE (CHECK ERRORS)                                        
*                                                                               
DMCHECK1 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION?               
         BZ    DM020                                                            
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
         B     MODEEXIT                                                         
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    MODEEXIT                                                         
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR**********'              
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
*                                                                               
*   ROUTINE TO TRACE DATA MANAGER CALLS                                         
*                                                                               
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,MTRACDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         SPACE 3                                                                
PUTFILE  NTR1                                                                   
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    MODEEXIT            YES - DON'T REWRITE IT                       
         ST    RF,AIOAREA                                                       
         GOTO1 PREC                                                             
         B     MODEEXIT                                                         
         SPACE 3                                                                
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
*********************************************************************           
*********************************************************************           
*********************************************************************           
         EJECT                                                                  
XCOMFACS DS    A                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENALL1                                                      
*                                                                               
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
**       INCLUDE FALINKBLK                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
         LTORG                                                                  
         EJECT                                                                  
         PRINT ON                                                               
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
***********************************************************************         
OVERWRKD DSECT                                                                  
ACURPARM DS    F                                                                
*                                                                               
FRSTSTA  DS    F                                                                
CURSTA   DS    F                                                                
FRSTBK   DS    F                                                                
AFRSTBK  DS    F                                                                
CURBK    DS    F                                                                
FRSTUPG  DS    F                                                                
CURUPG   DS    F                                                                
FRSTDEM  DS    F                                                                
CURDEM   DS    F                                                                
FRSTDPT  DS    F                                                                
CURDPT   DS    F                                                                
FRSTRCD  DS    F                                                                
CURRCD   DS    F                                                                
FRSTFLT  DS    F                                                                
CURFLT   DS    F                                                                
*                                                                               
CURINV   DS    F                                                                
*                                                                               
NUMSTAS  DS    X                                                                
NUMDEMS  DS    XL2                                                              
NUMRCDS  DS    X                                                                
NUMFLTS  DS    X                                                                
NUMDPTS  DS    X                                                                
NUMBKS   DS    X                                                                
NUMUPGS  DS    X                                                                
*                                                                               
REMSTAS  DS    X                                                                
REMDEMS  DS    X                                                                
REMRCDS  DS    X                                                                
REMFLTS  DS    X                                                                
REMDPTS  DS    X                                                                
REMBKS   DS    X                                                                
REMUPGS  DS    X                                                                
*                                                                               
FLTSTART DS    XL3                                                              
FLTEND   DS    XL3                                                              
*                                                                               
SELPROFS DS    0CL10               SELLERS WORKSHEET PROFILES                   
         DS    CL1                 PROGRAM #                                    
         DS    CL1                 SPARE                                        
SELPROF  DS    CL8                 PROFILE BITS                                 
*                                                                               
NEWBOOK  DS    C                                                                
SAVESTAT DS    CL5                                                              
SAVEINV# DS    CL4                                                              
SAVEDMHD DS    CL4                                                              
SAVEBOOK DS    CL8                                                              
TODAYDT  DS    XL2                                                              
*                                                                               
INVSEQ   DS    PL5                                                              
DEMOCTR  DS    F                   MAX IS 44 DEMOS                              
PUTCTR   DS    F                   OUTPUT RECORD COUNTER                        
LINCTR   DS    F                   OUTPUT LINE   COUNTER                        
*                                                                               
DEMHDCTR DS    F                   MAX IS 11 RECORDS                            
DEMOLOOP DS    F                   FOUR SETS ON A RECORD                        
SAVERSH  DS    A                   A(R/S/H IN PROGRESS)                         
*                                                                               
AIOREC   DS    A                                                                
AIO1     DS    A                                                                
AIO2     DS    A                                                                
AIO3     DS    A                                                                
AIO4     DS    A                                                                
VFETCH   DS    V                                                                
VDEMAND  DS    V                                                                
VDEMOVAL DS    V                                                                
VDEMOUT  DS    V                                                                
VDEMOCON DS    V                                                                
DAYUNPK  DS    A                                                                
VDOPUT   DS    A                                                                
*                                                                               
         DS    0F                                                               
OVPARMS  DS    CL(4*6)             PARAMETER SAVE AREA                          
*                                                                               
FPARMS   DS    6F                  RETURN PARMETER BLK FROM GETDATA             
*                                                                               
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
RTSRVC   DS    X                   RATING SERVICE                               
RMODE    DS    CL1                 RATINGS MODE FLAG                            
COMMAND  DS    CL8                                                              
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
MYP      DS    CL132                                                            
AIOAREA  DS    A                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
ALET     DS    F                                                                
SAVEREGS DS    11F                                                              
*                                                                               
MISCFLG1 DS    X                                                                
MF1DATA  EQU   X'80'               INDICATES DATA IN FALINK BUFFER              
MF1TXT   EQU   X'40'               FETCH ONLY TEXT FOR NEW DATA REQUEST         
MF1MKT   EQU   X'40'               MARKET TEXT REQUEST IN NEW TEXT              
MF1IBKL  EQU   X'40'               INV. BOOK LIST IN VHDR,CSTA,RFCON            
MF1RST   EQU   X'40'               STATION LIST FOR RATECARD DOWNLOAD           
MF1GLOB  EQU   X'20'               ENTERING FROM GLOBBER                        
MF1GBRK  EQU   X'10'               BREAKING TO CALL GLOBBER                     
MF1TMPB2 EQU   X'02'               TEMPORARY BITS                               
MF1TMPB1 EQU   X'01'                                                            
*                                                                               
*                                  UP TO EIGHT SLOTS                            
*                                  POS  1  -  27  =  PROGRAM NAME               
*                                  POS 28  -  38  =  AVAIL DAY                  
*                                  POS 39  -  49  =  AVAIL TIME                 
*                                  POS 50         =  SPARE                      
*                                                                               
AVORIDES DS    CL400               STORAGE FOR SLOTS                            
ADDRAVOR DS    A                   ADDRESS OF NEXT AVAILABLE SLOT               
LAVORIDE EQU   50                                                               
DPGNAME  EQU   0                   DISPLACE TO PROGRAM NAME                     
DAVDAY   EQU   27                  DISPLACE TO AVAIL DAY                        
DAVTIME  EQU   38                  DISPLACE TO AVAIL TIME                       
*                                                                               
LENIO    EQU   4096                                                             
*                                                                               
IOAREA1  DS    (LENIO)X                                                         
IOAREA2  DS    (LENIO)X                                                         
IOAREA3  DS    (LENIO)X                                                         
IOAREA4  DS    (LENIO)X                                                         
*                                                                               
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
*                                                                               
LONGPARM EQU   X'FF'               FETCH INDICATOR FOR ADDR. PRAMETER           
INVREJ   EQU   RFTRBADQ            FETCH INDICATOR INV HDR REJECT               
FTCHWDTH EQU   132                                                              
*                                                                               
STALENQ  EQU   5                                                                
DPTLENQ  EQU   1                                                                
DEMLENQ  EQU   4                                                                
BKLENQ   EQU   6                                                                
UPGLENQ  EQU   11+14+1                                                          
RCDLENQ  EQU   8+1+1                                                            
FLTLENQ  EQU   6                                                                
         EJECT                                                                  
OVERWRKQ EQU   *-OVERWRKD          LENGTH OF WORKING STORAGE                    
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE RESELPROF                                                      
       ++INCLUDE REFETCHD                                                       
         PRINT ON                                                               
*                                                                               
DAYPTALL EQU   C'X'           CONTINENTAL "ALL" DAY-PART CODE                   
*                                                                               
STATEQUS EQU   0                                                                
STATCTL  EQU   0              ALWAYS 'STAT'                                     
STATPWR  EQU   4              POWER CODE                                        
STATSTA  EQU   6              STATION CALL LETTERS                              
*                                                                               
DYPTEQUS EQU   0                                                                
DYPTCTL  EQU   0              ALWAYS 'DYPT'                                     
DYPTPWR  EQU   4              POWER CODE                                        
DYPTDPT  EQU   6              DAYPART CODE                                      
DYPTDPT3 EQU   7              DAYPART SHORT NAME                                
DYPTDPTL EQU   10             DAYPART LONG NAME                                 
*                                                                               
HD01EQUS EQU   0                                                                
HD01CTL  EQU   0              ALWAYS 'HD01'                                     
HD01PWR  EQU   4              POWER CODE                                        
HD01STA  EQU   6              STATION CALL LETTERS                              
HD01INV  EQU   11             INVENTORY NUMBER                                  
HD01STDT EQU   15             INVENTORY EFFECTIVE START DATE (CCYYMMDD)         
HD01ENDT EQU   23                END DATE (CCYYMMDD) (IF PRESENT)               
HD01DYPT EQU   31             DAYPART LIST FOR PROGRAM                          
HD01DAYS EQU   39             DAY GRID                                          
HD01TIME EQU   46             TIME (STANDARD DISPLAY FORMAT)                    
*                                                                               
HD02EQUS EQU   0                                                                
HD02CTL  EQU   0              ALWAYS 'HD02'                                     
HD02PWR  EQU   4              POWER CODE                                        
HD02STA  EQU   6              STATION CALL LETTERS                              
HD02INV  EQU   11             INVENTORY NUMBER                                  
HD02STDT EQU   15             INVENTORY EFFECTIVE START DATE (CCYYMMDD)         
HD02ENDT EQU   23                END DATE (CCYYMMDD) (IF PRESENT)               
HD02AVDY EQU   31             AVAIL DAY  OVERRIDE (TEXT OR BLANK)               
HD02AVTM EQU   42             AVAIL TIME OVERRIDE (TEXT OR BLANK)               
HD02PGNM EQU   53             PROGRAM NAME                                      
*                                                                               
HDLNTH   EQU   31             LENGTH OF HD01/HD02 THRU END DATE                 
*                                                                               
         PRINT OFF                                                              
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
*                                                                               
       ++INCLUDE REGENRDP                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
INVRECD  DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
*                                                                               
***********************************************************************         
***********************************************************************         
***********************************************************************         
RE3Y02   CSECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*   START OF CALLED ROUTINES                                                    
*                                                                               
*                                                                               
STDTRECD NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATCON,DMCB,(2,0(R2)),(X'20',WORK+34)                            
*                                  CONVERT FROM COMPRESSED TO EBCDIC            
         MVC   WORK+32(2),=C'19'   INSERT CENTURY                               
         CLC   =C'50',WORK+34      CHECK FOR CENTURY                            
         BL    STDT0020            STILL IN 20TH                                
         MVC   WORK+32(2),=C'20'   INSERT 21ST CENTURY                          
STDT0020 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
ENDTRECD NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATCON,DMCB,(2,0(R2)),(X'20',WORK+34)                            
*                                  CONVERT FROM COMPRESSED TO EBCDIC            
         MVC   WORK+32(2),=C'19'   INSERT CENTURY                               
         CLC   =C'50',WORK+34      CHECK FOR CENTURY                            
         BL    ENDT0020            STILL IN 20TH                                
         MVC   WORK+32(2),=C'20'   INSERT 21ST CENTURY                          
ENDT0020 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* DOPUT - OUTPUT FILE AND PRINT LINE                                            
*    R3 --> ADDRESS OF FILE TO BE OUTPUT  A(STATFILE),A(DYPTFILE) ETC.          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DOPUT    NTR1  BASE=*,LABEL=*                                                   
         MVI   MYP+95,C':'         INSERT LINE DELIMITER                        
*                                                                               
         OC    MYP,SPACES          SET LOW-VALUE TO SPACE                       
*                                                                               
*****    LA    R3,INTFILE          A(INTFILE) FOR PUT ROUTINE                   
*                                                                               
         LA    R2,MYP                                                           
*                                                                               
         PUT   (R3),(R2)                                                        
*                                                                               
         L     RF,PUTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR                                                        
*                                                                               
         CLI   QUESTOR+1,C'Y'      DISPLAY OUTPUT?                              
         BNE   DOPUT020            NO                                           
*                                  LIMIT THE PRINT-OUTPUT                       
         L     RF,LINCTR                                                        
         CHI   RF,3000             TEST PRINT-LINE OUTPUT                       
         BH    DOPUT020            ENOUGH TEST OUTPUT                           
         LA    RF,1(RF)                                                         
         ST    RF,LINCTR                                                        
*                                                                               
         MVC   P(96),MYP                                                        
         GOTO1 REPORT                                                           
DOPUT020 EQU   *                                                                
*                                                                               
*****    MVC   MYP,SPACES          CLEAR OUTPUT RECORD                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   RECORDS FOR THESE FILES ARE BUILT IN MYP, AND ARE OUTPUT                    
*        DIRECTLY FROM THERE.  NOTHING FANCY.                                   
*                                                                               
STAFILE  DCB   DDNAME=STAFILE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00096,                                            X        
               BLKSIZE=05760,                                          X        
               MACRF=PM                                                         
         SPACE 4                                                                
*                                                                               
DPTFILE  DCB   DDNAME=DPTFILE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00096,                                            X        
               BLKSIZE=05760,                                          X        
               MACRF=PM                                                         
*                                                                               
HD1FILE  DCB   DDNAME=HD1FILE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00096,                                            X        
               BLKSIZE=05760,                                          X        
               MACRF=PM                                                         
*                                                                               
HD2FILE  DCB   DDNAME=HD2FILE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00096,                                            X        
               BLKSIZE=05760,                                          X        
               MACRF=PM                                                         
         SPACE 4                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*******************************************************************             
* TABLE CONTAINING ALL DESIRED STATIONS FOR THIS EXTRACT (REP CQ)               
*******************************************************************             
         DC    C'**STATIONS**'                                                  
*                                                                               
*   INPUT BLOCK:  THIS IS THE AREA THAT MUST BE BUILT TO                        
*        DRIVE THE INDIVIDUAL REQUEST   (CONTINENTAL)                           
*                                                                               
INPBLOCK DS    0H                                                               
*  FIRST                                                                        
INPFRST  DC    CL05'AABCT'                                                      
INPSCND  DC    CL05'AEWBT'                                                      
         DC    CL05'AWBFT'                                                      
         DC    CL05'AWBMT'                                                      
         DC    CL05'CW4 T'                                                      
         DC    CL05'DDTVT'                                                      
         DC    CL05'DLBTT'                                                      
         DC    CL05'DLOXT'                                                      
         DC    CL05'EABGT'                                                      
         DC    CL05'EABIT'                                                      
         DC    CL05'EAGMT'                                                      
         DC    CL05'EAOET'                                                      
         DC    CL05'EBBHT'                                                      
         DC    CL05'EBKOT'                                                      
         DC    CL05'EBKPT'                                                      
         DC    CL05'EBNGT'                                                      
         DC    CL05'EBOCT'                                                      
         DC    CL05'EBUPT'                                                      
         DC    CL05'ECJBT'                                                      
         DC    CL05'ECTVT'                                                      
         DC    CL05'EDAYT'                                                      
         DC    CL05'EEAUT'                                                      
         DC    CL05'EEEKT'                                                      
         DC    CL05'EEVVT'                                                      
         DC    CL05'EFFFT'                                                      
         DC    CL05'EGXAT'                                                      
         DC    CL05'EHBFT'                                                      
         DC    CL05'EHNTT'                                                      
         DC    CL05'EHSVT'                                                      
         DC    CL05'EHTMT'                                                      
         DC    CL05'EIBWT'                                                      
         DC    CL05'EIFRT'                                                      
         DC    CL05'EILXT'                                                      
         DC    CL05'EJHGT'                                                      
         DC    CL05'EKTVT'                                                      
         DC    CL05'ELBTT'                                                      
         DC    CL05'ELIOT'                                                      
         DC    CL05'ELOXT'                                                      
         DC    CL05'EMTVT'                                                      
         DC    CL05'ENDUT'                                                      
         DC    CL05'EOHLT'                                                      
         DC    CL05'EOI T'                                                      
         DC    CL05'EOLNT'                                                      
         DC    CL05'EOWTT'                                                      
         DC    CL05'EPTAT'                                                      
         DC    CL05'ERCBT'                                                      
         DC    CL05'ERDWT'                                                      
         DC    CL05'ESAWT'                                                      
         DC    CL05'ESAZT'                                                      
         DC    CL05'ESETT'                                                      
         DC    CL05'ESILT'                                                      
         DC    CL05'ETAPT'                                                      
         DC    CL05'ETOKT'                                                      
         DC    CL05'ETSNT'                                                      
         DC    CL05'ETVYT'                                                      
         DC    CL05'EVFXT'                                                      
         DC    CL05'EVLTT'                                                      
         DC    CL05'EWAYT'                                                      
         DC    CL05'EYTVT'                                                      
         DC    CL05'GBKOT'                                                      
         DC    CL05'GCTIT'                                                      
         DC    CL05'GHSVT'                                                      
         DC    CL05'GJHGT'                                                      
         DC    CL05'GLBTT'                                                      
         DC    CL05'GSAWT'                                                      
         DC    CL05'GTAPT'                                                      
         DC    CL05'GTOKT'                                                      
         DC    CL05'GTVYT'                                                      
         DC    CL05'GVIRT'                                                      
         DC    CL05'HHSVT'                                                      
         DC    CL05'IAGMT'                                                      
         DC    CL05'IAOET'                                                      
         DC    CL05'IBKOT'                                                      
         DC    CL05'IBKPT'                                                      
         DC    CL05'IBMAT'                                                      
         DC    CL05'IBNGT'                                                      
         DC    CL05'IBUPT'                                                      
         DC    CL05'IBXIT'                                                      
         DC    CL05'ICJBT'                                                      
         DC    CL05'ICTVT'                                                      
         DC    CL05'ICYBT'                                                      
         DC    CL05'IDTVT'                                                      
         DC    CL05'IEAUT'                                                      
         DC    CL05'IEEKT'                                                      
         DC    CL05'IEMTT'                                                      
         DC    CL05'IEVVT'                                                      
         DC    CL05'IFFFT'                                                      
         DC    CL05'IHBFT'                                                      
         DC    CL05'IIFRT'                                                      
         DC    CL05'IILXT'                                                      
         DC    CL05'IJHGT'                                                      
         DC    CL05'IKTVT'                                                      
         DC    CL05'ILBTT'                                                      
         DC    CL05'ILOXT'                                                      
         DC    CL05'IMTVT'                                                      
         DC    CL05'INDUT'                                                      
         DC    CL05'IOI T'                                                      
         DC    CL05'IOWTT'                                                      
         DC    CL05'IPSDT'                                                      
         DC    CL05'IPTAT'                                                      
         DC    CL05'IRCBT'                                                      
         DC    CL05'ISILT'                                                      
         DC    CL05'ITOKT'                                                      
         DC    CL05'ITSNT'                                                      
         DC    CL05'IVFXT'                                                      
         DC    CL05'IVLTT'                                                      
         DC    CL05'IVNYT'                                                      
         DC    CL05'IWAYT'                                                      
         DC    CL05'IWNYT'                                                      
         DC    CL05'JNDUT'                                                      
         DC    CL05'KAEFT'                                                      
         DC    CL05'KAKET'                                                      
         DC    CL05'KAMRT'                                                      
         DC    CL05'KAPPT'                                                      
         DC    CL05'KARDT'                                                      
         DC    CL05'KASYT'                                                      
         DC    CL05'KATCT'                                                      
         DC    CL05'KATNT'                                                      
         DC    CL05'KATVT'                                                      
         DC    CL05'KAVUT'                                                      
         DC    CL05'KBAKT'                                                      
         DC    CL05'KBCIT'                                                      
         DC    CL05'KBFXT'                                                      
         DC    CL05'KBJRT'                                                      
         DC    CL05'KBMTT'                                                      
         DC    CL05'KBMYT'                                                      
         DC    CL05'KCAUT'                                                      
         DC    CL05'KCBDT'                                                      
         DC    CL05'KCHWT'                                                      
         DC    CL05'KCITT'                                                      
         DC    CL05'KCPNT'                                                      
         DC    CL05'KDBAT'                                                      
         DC    CL05'KDEVT'                                                      
         DC    CL05'KECIT'                                                      
         DC    CL05'KESQT'                                                      
         DC    CL05'KEYCT'                                                      
         DC    CL05'KEYTT'                                                      
         DC    CL05'KFBBT'                                                      
         DC    CL05'KFDXT'                                                      
         DC    CL05'KFJXT'                                                      
         DC    CL05'KFNBT'                                                      
         DC    CL05'KFSMT'                                                      
         DC    CL05'KGBTT'                                                      
         DC    CL05'KGNST'                                                      
         DC    CL05'KGPET'                                                      
         DC    CL05'KGWCT'                                                      
         DC    CL05'KGWLT'                                                      
         DC    CL05'KGWNT'                                                      
         DC    CL05'KGWRT'                                                      
         DC    CL05'KHBBT'                                                      
         DC    CL05'KHQAT'                                                      
         DC    CL05'KHSLT'                                                      
         DC    CL05'KIEMT'                                                      
         DC    CL05'KIFIT'                                                      
         DC    CL05'KIIIT'                                                      
         DC    CL05'KIMOT'                                                      
         DC    CL05'KJBOT'                                                      
         DC    CL05'KJTLT'                                                      
         DC    CL05'KJUDT'                                                      
         DC    CL05'KKCOT'                                                      
         DC    CL05'KKTVT'                                                      
         DC    CL05'KLAST'                                                      
         DC    CL05'KLSTT'                                                      
         DC    CL05'KLTVT'                                                      
         DC    CL05'KLWYT'                                                      
         DC    CL05'KMOHT'                                                      
         DC    CL05'KMOLT'                                                      
         DC    CL05'KMVTT'                                                      
         DC    CL05'KMVUT'                                                      
         DC    CL05'KNVNT'                                                      
         DC    CL05'KNVUT'                                                      
         DC    CL05'KOAMT'                                                      
         DC    CL05'KOLNT'                                                      
         DC    CL05'KOSAT'                                                      
         DC    CL05'KOTAT'                                                      
         DC    CL05'KPXJT'                                                      
         DC    CL05'KQTVT'                                                      
         DC    CL05'KRCGT'                                                      
         DC    CL05'KRCRT'                                                      
         DC    CL05'KSANT'                                                      
         DC    CL05'KSPRT'                                                      
         DC    CL05'KSTFT'                                                      
         DC    CL05'KTBST'                                                      
         DC    CL05'KTENT'                                                      
         DC    CL05'KTIDT'                                                      
         DC    CL05'KTRET'                                                      
         DC    CL05'KTTWT'                                                      
         DC    CL05'KTULT'                                                      
         DC    CL05'KTVCT'                                                      
         DC    CL05'KTVET'                                                      
         DC    CL05'KTVMT'                                                      
         DC    CL05'KTVNT'                                                      
         DC    CL05'KTVXT'                                                      
         DC    CL05'KTVZT'                                                      
         DC    CL05'KTWTT'                                                      
         DC    CL05'KTXET'                                                      
         DC    CL05'KTXST'                                                      
         DC    CL05'KUCWT'                                                      
         DC    CL05'KULRT'                                                      
         DC    CL05'KVALT'                                                      
         DC    CL05'KVCTT'                                                      
         DC    CL05'KVEWT'                                                      
         DC    CL05'KVIAT'                                                      
         DC    CL05'KWBQT'                                                      
         DC    CL05'KWBST'                                                      
         DC    CL05'KWCAT'                                                      
         DC    CL05'KWKTT'                                                      
         DC    CL05'KWMKT'                                                      
         DC    CL05'KWUBT'                                                      
         DC    CL05'KWWTT'                                                      
         DC    CL05'KWWYT'                                                      
         DC    CL05'KWYFT'                                                      
         DC    CL05'KXLYT'                                                      
         DC    CL05'KXMCT'                                                      
         DC    CL05'KXTST'                                                      
         DC    CL05'KYLET'                                                      
         DC    CL05'LSNNT'                                                      
         DC    CL05'LVI T'                                                      
         DC    CL05'MAPPT'                                                      
         DC    CL05'NAKET'                                                      
         DC    CL05'NATNT'                                                      
         DC    CL05'NATVT'                                                      
         DC    CL05'NBJRT'                                                      
         DC    CL05'NBMTT'                                                      
         DC    CL05'NCAUT'                                                      
         DC    CL05'NCBDT'                                                      
         DC    CL05'NEYCT'                                                      
         DC    CL05'NEYTT'                                                      
         DC    CL05'NFBBT'                                                      
         DC    CL05'NFSMT'                                                      
         DC    CL05'NGNST'                                                      
         DC    CL05'NGPET'                                                      
         DC    CL05'NGWNT'                                                      
         DC    CL05'NHBBT'                                                      
         DC    CL05'NHQAT'                                                      
         DC    CL05'NHSLT'                                                      
         DC    CL05'NIMOT'                                                      
         DC    CL05'NJUDT'                                                      
         DC    CL05'NKCOT'                                                      
         DC    CL05'NKTVT'                                                      
         DC    CL05'NLAST'                                                      
         DC    CL05'NOLNT'                                                      
         DC    CL05'NOSAT'                                                      
         DC    CL05'NSPRT'                                                      
         DC    CL05'NTENT'                                                      
         DC    CL05'NTULT'                                                      
         DC    CL05'NTVXT'                                                      
         DC    CL05'NTVZT'                                                      
         DC    CL05'NTXST'                                                      
         DC    CL05'NVALT'                                                      
         DC    CL05'NVIAT'                                                      
         DC    CL05'NXLYT'                                                      
         DC    CL05'OIFIT'                                                      
         DC    CL05'OTULT'                                                      
         DC    CL05'QVIAT'                                                      
         DC    CL05'RAKET'                                                      
         DC    CL05'RARDT'                                                      
         DC    CL05'RATVT'                                                      
         DC    CL05'RCAUT'                                                      
         DC    CL05'RGNST'                                                      
         DC    CL05'RHQAT'                                                      
         DC    CL05'RKCOT'                                                      
         DC    CL05'RKTVT'                                                      
         DC    CL05'RLSTT'                                                      
         DC    CL05'ROLNT'                                                      
         DC    CL05'ROSAT'                                                      
         DC    CL05'RSPRT'                                                      
         DC    CL05'RTVNT'                                                      
         DC    CL05'RVIAT'                                                      
         DC    CL05'RWKTT'                                                      
         DC    CL05'WABGT'                                                      
         DC    CL05'WABIT'                                                      
         DC    CL05'WAGMT'                                                      
         DC    CL05'WAKAT'                                                      
         DC    CL05'WALBT'                                                      
         DC    CL05'WAOET'                                                      
         DC    CL05'WBAUT'                                                      
         DC    CL05'WBBHT'                                                      
         DC    CL05'WBBJT'                                                      
         DC    CL05'WBFWT'                                                      
         DC    CL05'WBKOT'                                                      
         DC    CL05'WBKPT'                                                      
         DC    CL05'WBMAT'                                                      
         DC    CL05'WBNGT'                                                      
         DC    CL05'WBOCT'                                                      
         DC    CL05'WBR T'                                                      
         DC    CL05'WBRET'                                                      
         DC    CL05'WBUIT'                                                      
         DC    CL05'WBUPT'                                                      
         DC    CL05'WBZVT'                                                      
         DC    CL05'WCFNT'                                                      
         DC    CL05'WCFTT'                                                      
         DC    CL05'WCIAT'                                                      
         DC    CL05'WCIVT'                                                      
         DC    CL05'WCJBT'                                                      
         DC    CL05'WCTIT'                                                      
         DC    CL05'WCTVT'                                                      
         DC    CL05'WCWJT'                                                      
         DC    CL05'WCYBT'                                                      
         DC    CL05'WDAYT'                                                      
         DC    CL05'WDAZT'                                                      
         DC    CL05'WDNIT'                                                      
         DC    CL05'WDTVT'                                                      
         DC    CL05'WEAUT'                                                      
         DC    CL05'WEEKT'                                                      
         DC    CL05'WEMTT'                                                      
         DC    CL05'WEVVT'                                                      
         DC    CL05'WFFFT'                                                      
         DC    CL05'WFXIT'                                                      
         DC    CL05'WFXPT'                                                      
         DC    CL05'WFXWT'                                                      
         DC    CL05'WGTUT'                                                      
         DC    CL05'WGXAT'                                                      
         DC    CL05'WHAGT'                                                      
         DC    CL05'WHBFT'                                                      
         DC    CL05'WHIZT'                                                      
         DC    CL05'WHNTT'                                                      
         DC    CL05'WHSVT'                                                      
         DC    CL05'WHTMT'                                                      
         DC    CL05'WIBWT'                                                      
         DC    CL05'WIFRT'                                                      
         DC    CL05'WILXT'                                                      
         DC    CL05'WJCLT'                                                      
         DC    CL05'WJETT'                                                      
         DC    CL05'WJHGT'                                                      
         DC    CL05'WJSUT'                                                      
         DC    CL05'WKBNT'                                                      
         DC    CL05'WKDHT'                                                      
         DC    CL05'WKTVT'                                                      
         DC    CL05'WLBTT'                                                      
         DC    CL05'WLEXT'                                                      
         DC    CL05'WLIOT'                                                      
         DC    CL05'WLOVT'                                                      
         DC    CL05'WLOXT'                                                      
         DC    CL05'WMTVT'                                                      
         DC    CL05'WNDUT'                                                      
         DC    CL05'WNYFT'                                                      
         DC    CL05'WOAYT'                                                      
         DC    CL05'WOHLT'                                                      
         DC    CL05'WOI T'                                                      
         DC    CL05'WOWTT'                                                      
         DC    CL05'WPDET'                                                      
         DC    CL05'WPSDT'                                                      
         DC    CL05'WPTAT'                                                      
         DC    CL05'WRCBT'                                                      
         DC    CL05'WRDWT'                                                      
         DC    CL05'WROCT'                                                      
         DC    CL05'WRSPT'                                                      
         DC    CL05'WSAWT'                                                      
         DC    CL05'WSAZT'                                                      
         DC    CL05'WSETT'                                                      
         DC    CL05'WSILT'                                                      
         DC    CL05'WTAJT'                                                      
         DC    CL05'WTAPT'                                                      
         DC    CL05'WTGST'                                                      
         DC    CL05'WTOKT'                                                      
         DC    CL05'WTVAT'                                                      
         DC    CL05'WTVYT'                                                      
         DC    CL05'WTWOT'                                                      
         DC    CL05'WUHFT'                                                      
         DC    CL05'WVFXT'                                                      
         DC    CL05'WVIRT'                                                      
         DC    CL05'WVLTT'                                                      
         DC    CL05'WVNYT'                                                      
         DC    CL05'WWAYT'                                                      
         DC    CL05'WWMBT'                                                      
         DC    CL05'WWNYT'                                                      
         DC    CL05'WYFXT'                                                      
         DC    CL05'WYOUT'                                                      
         DC    CL05'WYTVT'                                                      
         DC    CL05'WZVNT'                                                      
         DC    CL05'XAOET'                                                      
         DC    CL05'XBNGT'                                                      
         DC    CL05'XCTIT'                                                      
         DC    CL05'XGXAT'                                                      
         DC    CL05'XICUT'                                                      
         DC    CL05'XIEMT'                                                      
         DC    CL05'XJUDT'                                                      
         DC    CL05'XMVUT'                                                      
         DC    CL05'XSPRT'                                                      
         DC    CL05'XTVET'                                                      
         DC    CL05'KWBQ1'                                                      
         DC    CL05'KULR1'                                                      
         DC    CL05'WBMA1'                                                      
         DC    CL05'KGWC1'                                                      
         DC    CL05'KFNB1'                                                      
         DC    CL05'WRSP1'                                                      
         DC    CL05'WSAZ1'                                                      
         DC    CL05'KGWN1'                                                      
         DC    CL05'KBJR1'                                                      
         DC    CL05'KVAL1'                                                      
         DC    CL05'WDAY1'                                                      
         DC    CL05'WFXI1'                                                      
         DC    CL05'KOLN1'                                                      
         DC    CL05'KBMY1'                                                      
         DC    CL05'KXMC1'                                                      
         DC    CL05'KECI1'                                                      
         DC    CL05'WSIL1'                                                      
         DC    CL05'KOTA1'                                                      
         DC    CL05'KTTW1'                                                      
         DC    CL05'WGTU1'                                                      
         DC    CL05'KLTV1'                                                      
         DC    CL05'KWKT1'                                                      
         DC    CL05'KAKE1'                                                      
         DC    CL05'KAPP1'                                                      
         DC    CL05'WYFX1'                                                      
         DC    CL05'WBD T'                                                      
         DC    CL05'WBWTT'                                                      
INPNUMQ  EQU   (*-INPBLOCK)/INPLNQ                                              
INPLNQ   EQU   (INPSCND-INPFRST)                                                
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109REREP3Y02 01/12/11'                                      
         END                                                                    
**  INSERT END  >>                                                              
