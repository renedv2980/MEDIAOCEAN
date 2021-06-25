*          DATA SET REREP3Z02  AT LEVEL 112 AS OF 01/12/11                      
*PHASE RE3Z02C                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE UNBOOK                                                                 
         TITLE 'RE3Z02 - REREP3Z02 - MEDIA OCEAN DEMO FEED'                     
**********************************************************************          
*********************************************************************           
*                                                                   *           
*   REREP3Z02 - RE3Z02  - MEDIA OCEAN DEMO FEED                     *           
*                                                                   *           
*        KATZ "MILENNIUM" (SZ) ONLY                                 *           
*                                                                   *           
*********************************************************************           
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
*   DEC22/09 (SMY) --- SPECIAL EXTRACT FOR KATZ "SZ"                *           
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
RE3Z02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 OVERWRKQ,**RE3Z02,R8,RR=RE                                       
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
         MVC   MYP+STATPWR(2),=C'SZ'       INSERT REP CODE WANTED               
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
         MVC   KEY+24(2),=C'SZ'    INSERT REP CODE FOR THIS EXTRACT             
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
*  READ ALL INVENTORY HEADER RECORDS FOR A SINGLE REP (SZ HERE) AND             
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
         MVC   RINVKREP,=C'SZ'         INSERT REP CODE WANTED                   
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
         CLI   0(R2),DAYPTALL      MILLENNIUM "ALL" (Z) DAY-PART ?              
         BE    HD1DLUPX            YES - KEEP                                   
         LA    R2,1(R2)            NEXT DAY-PART CODE                           
         BCT   R0,HD1DLUP                                                       
         B     HD010020            Z DAY-PART CODE NOT FOUND - SKIP REC         
* ABOVE SELECTS ONLY RECORDS CONTAINING A SPECIFIC DAY-PART CODE                
*                                                                               
HD1DLUPX EQU   *                                                                
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
         MVC   MYP+HD01PWR(L'RINVKREP),RINVKREP   INSERT POWER CODE             
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
DAYPTALL EQU   C'Z'           MILLENNIUM "ALL" DAY-PART CODE                    
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
RE3Z02   CSECT                                                                  
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
* TABLE CONTAINING ALL DESIRED STATIONS FOR THIS EXTRACT (REP SZ)               
*******************************************************************             
         DC    C'**STATIONS**'                                                  
*                                                                               
*   INPUT BLOCK:  THIS IS THE AREA THAT MUST BE BUILT TO                        
*        DRIVE THE INDIVIDUAL REQUEST  (MILLENNIUM)                             
*                                                                               
INPBLOCK DS    0H                                                               
*  FIRST                                                                        
INPFRST  DC    CL05'DAWST'                                                      
INPSCND  DC    CL05'DBFFT'                                                      
         DC    CL05'DFORT'                                                      
         DC    CL05'DHO T'                                                      
         DC    CL05'DSWGT'                                                      
         DC    CL05'EAHUT'                                                      
         DC    CL05'EAWST'                                                      
         DC    CL05'EETMT'                                                      
         DC    CL05'EGGBT'                                                      
         DC    CL05'EHAMT'                                                      
         DC    CL05'EHP T'                                                      
         DC    CL05'EITIT'                                                      
         DC    CL05'EKRCT'                                                      
         DC    CL05'EMDTT'                                                      
         DC    CL05'ENEPT'                                                      
         DC    CL05'ENKYT'                                                      
         DC    CL05'EOAIT'                                                      
         DC    CL05'ERGTT'                                                      
         DC    CL05'ERLHT'                                                      
         DC    CL05'ESEET'                                                      
         DC    CL05'ESWGT'                                                      
         DC    CL05'ESYRT'                                                      
         DC    CL05'ESYXT'                                                      
         DC    CL05'ETVOT'                                                      
         DC    CL05'EWTIT'                                                      
         DC    CL05'EXXAT'                                                      
         DC    CL05'EXXVT'                                                      
         DC    CL05'FXIIT'                                                      
         DC    CL05'IDKYT'                                                      
         DC    CL05'IFFTT'                                                      
         DC    CL05'IOAIT'                                                      
         DC    CL05'IRGTT'                                                      
         DC    CL05'ISYXT'                                                      
         DC    CL05'KABBT'                                                      
         DC    CL05'KADNT'                                                      
         DC    CL05'KAMCT'                                                      
         DC    CL05'KAQYT'                                                      
         DC    CL05'KARKT'                                                      
         DC    CL05'KARZT'                                                      
         DC    CL05'KASNT'                                                      
         DC    CL05'KAUTT'                                                      
         DC    CL05'KAYUT'                                                      
         DC    CL05'KBSIT'                                                      
         DC    CL05'KBTVT'                                                      
         DC    CL05'KBTXT'                                                      
         DC    CL05'KBVUT'                                                      
         DC    CL05'KCBAT'                                                      
         DC    CL05'KCOYT'                                                      
         DC    CL05'KCVUT'                                                      
         DC    CL05'KCWQT'                                                      
         DC    CL05'KCYUT'                                                      
         DC    CL05'KDBCT'                                                      
         DC    CL05'KDF T'                                                      
         DC    CL05'KDFXT'                                                      
         DC    CL05'KDNLT'                                                      
         DC    CL05'KDRVT'                                                      
         DC    CL05'KDSMT'                                                      
         DC    CL05'KDVRT'                                                      
         DC    CL05'KECYT'                                                      
         DC    CL05'KEMYT'                                                      
         DC    CL05'KETKT'                                                      
         DC    CL05'KEVNT'                                                      
         DC    CL05'KEZIT'                                                      
         DC    CL05'KEZXT'                                                      
         DC    CL05'KFFXT'                                                      
         DC    CL05'KFORT'                                                      
         DC    CL05'KFTAT'                                                      
         DC    CL05'KFTYT'                                                      
         DC    CL05'KFXAT'                                                      
         DC    CL05'KFXKT'                                                      
         DC    CL05'KGANT'                                                      
         DC    CL05'KGETT'                                                      
         DC    CL05'KHMTT'                                                      
         DC    CL05'KIDKT'                                                      
         DC    CL05'KIDYT'                                                      
         DC    CL05'KIDZT'                                                      
         DC    CL05'KIONT'                                                      
         DC    CL05'KJCTT'                                                      
         DC    CL05'KJTVT'                                                      
         DC    CL05'KKFXT'                                                      
         DC    CL05'KLAFT'                                                      
         DC    CL05'KLBKT'                                                      
         DC    CL05'KLCWT'                                                      
         DC    CL05'KLKNT'                                                      
         DC    CL05'KLPNT'                                                      
         DC    CL05'KLRTT'                                                      
         DC    CL05'KMIDT'                                                      
         DC    CL05'KMSST'                                                      
         DC    CL05'KMTRT'                                                      
         DC    CL05'KMTWT'                                                      
         DC    CL05'KMYLT'                                                      
         DC    CL05'KMYST'                                                      
         DC    CL05'KMYTT'                                                      
         DC    CL05'KNDXT'                                                      
         DC    CL05'KNWAT'                                                      
         DC    CL05'KOCBT'                                                      
         DC    CL05'KODET'                                                      
         DC    CL05'KOHDT'                                                      
         DC    CL05'KOKHT'                                                      
         DC    CL05'KOKIT'                                                      
         DC    CL05'KOLOT'                                                      
         DC    CL05'KOLRT'                                                      
         DC    CL05'KOMUT'                                                      
         DC    CL05'KPEJT'                                                      
         DC    CL05'KRBCT'                                                      
         DC    CL05'KRDOT'                                                      
         DC    CL05'KRIST'                                                      
         DC    CL05'KRVUT'                                                      
         DC    CL05'KSAST'                                                      
         DC    CL05'KSFXT'                                                      
         DC    CL05'KSHVT'                                                      
         DC    CL05'KSNFT'                                                      
         DC    CL05'KSTUT'                                                      
         DC    CL05'KSVIT'                                                      
         DC    CL05'KTABT'                                                      
         DC    CL05'KTALT'                                                      
         DC    CL05'KTBYT'                                                      
         DC    CL05'KTKAT'                                                      
         DC    CL05'KTRVT'                                                      
         DC    CL05'KTSMT'                                                      
         DC    CL05'KTVIT'                                                      
         DC    CL05'KTWOT'                                                      
         DC    CL05'KUSIT'                                                      
         DC    CL05'KUVIT'                                                      
         DC    CL05'KUVUT'                                                      
         DC    CL05'KVCWT'                                                      
         DC    CL05'KVEOT'                                                      
         DC    CL05'KVIQT'                                                      
         DC    CL05'KVMYT'                                                      
         DC    CL05'KVOAT'                                                      
         DC    CL05'KVTVT'                                                      
         DC    CL05'KWOTT'                                                      
         DC    CL05'KWTXT'                                                      
         DC    CL05'KXGNT'                                                      
         DC    CL05'KXIIT'                                                      
         DC    CL05'KXPIT'                                                      
         DC    CL05'KXVAT'                                                      
         DC    CL05'KZTVT'                                                      
         DC    CL05'KZUPT'                                                      
         DC    CL05'MJTVT'                                                      
         DC    CL05'MTSMT'                                                      
         DC    CL05'NAYUT'                                                      
         DC    CL05'NBTXT'                                                      
         DC    CL05'NCYUT'                                                      
         DC    CL05'NECYT'                                                      
         DC    CL05'NFORT'                                                      
         DC    CL05'NGETT'                                                      
         DC    CL05'NIONT'                                                      
         DC    CL05'NMTRT'                                                      
         DC    CL05'NOMUT'                                                      
         DC    CL05'NRIST'                                                      
         DC    CL05'NTRVT'                                                      
         DC    CL05'NWTXT'                                                      
         DC    CL05'OJCTT'                                                      
         DC    CL05'OOMUT'                                                      
         DC    CL05'RARKT'                                                      
         DC    CL05'ROLOT'                                                      
         DC    CL05'ROLRT'                                                      
         DC    CL05'RTALT'                                                      
         DC    CL05'UXIIT'                                                      
         DC    CL05'WAAYT'                                                      
         DC    CL05'WABMT'                                                      
         DC    CL05'WAHUT'                                                      
         DC    CL05'WAWST'                                                      
         DC    CL05'WBFFT'                                                      
         DC    CL05'WBGHT'                                                      
         DC    CL05'WBJOT'                                                      
         DC    CL05'WBKBT'                                                      
         DC    CL05'WBKIT'                                                      
         DC    CL05'WBPNT'                                                      
         DC    CL05'WBQDT'                                                      
         DC    CL05'WBRCT'                                                      
         DC    CL05'WBRLT'                                                      
         DC    CL05'WBUWT'                                                      
         DC    CL05'WBXXT'                                                      
         DC    CL05'WCAVT'                                                      
         DC    CL05'WCGVT'                                                      
         DC    CL05'WCHST'                                                      
         DC    CL05'WCOVT'                                                      
         DC    CL05'WDAFT'                                                      
         DC    CL05'WDBDT'                                                      
         DC    CL05'WDHNT'                                                      
         DC    CL05'WDKAT'                                                      
         DC    CL05'WDKYT'                                                      
         DC    CL05'WEART'                                                      
         DC    CL05'WETMT'                                                      
         DC    CL05'WFFTT'                                                      
         DC    CL05'WFGXT'                                                      
         DC    CL05'WFQXT'                                                      
         DC    CL05'WFVXT'                                                      
         DC    CL05'WFXBT'                                                      
         DC    CL05'WFXST'                                                      
         DC    CL05'WFXVT'                                                      
         DC    CL05'WGBCT'                                                      
         DC    CL05'WGGBT'                                                      
         DC    CL05'WGHPT'                                                      
         DC    CL05'WGMBT'                                                      
         DC    CL05'WGMET'                                                      
         DC    CL05'WHAMT'                                                      
         DC    CL05'WHO T'                                                      
         DC    CL05'WHP T'                                                      
         DC    CL05'WICDT'                                                      
         DC    CL05'WICST'                                                      
         DC    CL05'WICUT'                                                      
         DC    CL05'WICZT'                                                      
         DC    CL05'WITIT'                                                      
         DC    CL05'WIVTT'                                                      
         DC    CL05'WIWBT'                                                      
         DC    CL05'WJKTT'                                                      
         DC    CL05'WJTCT'                                                      
         DC    CL05'WJW T'                                                      
         DC    CL05'WJZYT'                                                      
         DC    CL05'WKEFT'                                                      
         DC    CL05'WKRCT'                                                      
         DC    CL05'WLFLT'                                                      
         DC    CL05'WLMTT'                                                      
         DC    CL05'WLOST'                                                      
         DC    CL05'WLYHT'                                                      
         DC    CL05'WMBDT'                                                      
         DC    CL05'WMDNT'                                                      
         DC    CL05'WMDTT'                                                      
         DC    CL05'WMGTT'                                                      
         DC    CL05'WMMPT'                                                      
         DC    CL05'WMSNT'                                                      
         DC    CL05'WMYAT'                                                      
         DC    CL05'WMYTT'                                                      
         DC    CL05'WMYVT'                                                      
         DC    CL05'WNABT'                                                      
         DC    CL05'WNEPT'                                                      
         DC    CL05'WNKYT'                                                      
         DC    CL05'WNTZT'                                                      
         DC    CL05'WNUVT'                                                      
         DC    CL05'WNYOT'                                                      
         DC    CL05'WNYST'                                                      
         DC    CL05'WOAIT'                                                      
         DC    CL05'WOLOT'                                                      
         DC    CL05'WPCHT'                                                      
         DC    CL05'WPGHT'                                                      
         DC    CL05'WPMIT'                                                      
         DC    CL05'WPMYT'                                                      
         DC    CL05'WPNYT'                                                      
         DC    CL05'WPTYT'                                                      
         DC    CL05'WQADT'                                                      
         DC    CL05'WQPXT'                                                      
         DC    CL05'WQRFT'                                                      
         DC    CL05'WRDCT'                                                      
         DC    CL05'WRGTT'                                                      
         DC    CL05'WRLHT'                                                      
         DC    CL05'WSEET'                                                      
         DC    CL05'WSMHT'                                                      
         DC    CL05'WSTRT'                                                      
         DC    CL05'WSWGT'                                                      
         DC    CL05'WSYRT'                                                      
         DC    CL05'WSYTT'                                                      
         DC    CL05'WSYXT'                                                      
         DC    CL05'WTATT'                                                      
         DC    CL05'WTEVT'                                                      
         DC    CL05'WTTAT'                                                      
         DC    CL05'WTTET'                                                      
         DC    CL05'WTTOT'                                                      
         DC    CL05'WTTXT'                                                      
         DC    CL05'WTVOT'                                                      
         DC    CL05'WTVWT'                                                      
         DC    CL05'WTVZT'                                                      
         DC    CL05'WTWCT'                                                      
         DC    CL05'WTXLT'                                                      
         DC    CL05'WUCWT'                                                      
         DC    CL05'WUFXT'                                                      
         DC    CL05'WUTRT'                                                      
         DC    CL05'WUTVT'                                                      
         DC    CL05'WUXPT'                                                      
         DC    CL05'WVAHT'                                                      
         DC    CL05'WVAWT'                                                      
         DC    CL05'WVIIT'                                                      
         DC    CL05'WVLAT'                                                      
         DC    CL05'WVTVT'                                                      
         DC    CL05'WWSBT'                                                      
         DC    CL05'WWTIT'                                                      
         DC    CL05'WWTVT'                                                      
         DC    CL05'WWUPT'                                                      
         DC    CL05'WXCWT'                                                      
         DC    CL05'WXLVT'                                                      
         DC    CL05'WXMST'                                                      
         DC    CL05'WXXAT'                                                      
         DC    CL05'WXXVT'                                                      
         DC    CL05'WYZZT'                                                      
         DC    CL05'WZTVT'                                                      
         DC    CL05'KRBC1'                                                      
         DC    CL05'WTTO1'                                                      
         DC    CL05'KTWO1'                                                      
         DC    CL05'WICS1'                                                      
         DC    CL05'KDVR1'                                                      
         DC    CL05'KEZI1'                                                      
         DC    CL05'KMTR1'                                                      
         DC    CL05'WXLV1'                                                      
         DC    CL05'WXOW1'                                                      
         DC    CL05'KDRV1'                                                      
         DC    CL05'KNDX1'                                                      
         DC    CL05'KIDY1'                                                      
         DC    CL05'WWTV1'                                                      
         DC    CL05'WFQX1'                                                      
         DC    CL05'KFXK1'                                                      
         DC    CL05'KETK1'                                                      
         DC    CL05'KSAS1'                                                      
         DC    CL05'KCYU1'                                                      
         DC    CL05'WVVAT'                                                      
         DC    CL05'EVVAT'                                                      
         DC    CL05'NXGNT'                                                      
         DC    CL05'WXOWT'                                                      
         DC    CL05'EXOWT'                                                      
         DC    CL05'WGEMT'                                                      
         DC    CL05'EGEMT'                                                      
         DC    CL05'WREXT'                                                      
         DC    CL05'EREXT'                                                      
         DC    CL05'KTIVT'                                                      
         DC    CL05'NTIVT'                                                      
         DC    CL05'WAOWT'                                                      
         DC    CL05'EAOWT'                                                      
         DC    CL05'WSJVT'                                                      
INPNUMQ  EQU   (*-INPBLOCK)/INPLNQ                                              
INPLNQ   EQU   (INPSCND-INPFRST)                                                
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'112REREP3Z02 01/12/11'                                      
         END                                                                    
**  INSERT END  >>                                                              
