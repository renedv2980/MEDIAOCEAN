*          DATA SET REREP3P02  AT LEVEL 006 AS OF 01/10/11                      
*PHASE RE3P02C                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE UNBOOK                                                                 
         TITLE 'RE3P02 - REREP3P02 - MEDIA OCEAN DEMO FEED'                     
**********************************************************************          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
**                                                                              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
*********************************************************************           
*                                                                   *           
*   REREP3P02 - RE3P02 - MEDIA OCEAN DEMO FEED - (UNIVISION)        *           
*                                                                   *           
*********************************************************************           
*   NOTE:  WHEN RATE CARDS ARE INTRODUCED, THE 'NUMFLTS' FIELD      *           
*          MUST BE MADE SOFT TO 'TODAY'S DATE'                      *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*   MAR26/02 (BU ) --- ORIGINAL ENTRY                               *           
*                                                                   *           
*   APR15/02 (BU ) --- ADD OUTPUT FILE NAME RETRIEVAL               *           
*                                                                   *           
*   JUL02/02 (BU ) --- ADD ADDITIONAL MONTHS TO TABLE               *           
*                                                                   *           
*   AUG14/02 (BU ) --- ADD KNTV AS A STATION                        *           
*                                                                   *           
*   NOV01/02 (JRD) --- EXPAND DEMO LIST                             *           
*                      SUPPORT SPECIAL SURVEY BYTE                  *           
*                                                                   *           
*   NOV03/02 (JRD) --- BOOK FOOTNOTE AND PJ FORMULA                 *           
*                      INVENTORY TEXT                               *           
*                                                                   *           
*   MAY16/03 (JRD) --- MAKE PJ FORMULA RECORD                       *           
*                      DISABLE INVENTORY TEXT                       *           
*                                                                   *           
*   JUL22/03 (JRD) --- FIX BUG WITH PROGRAM NAMES NOT BUMPING TABLE *           
*                                                                   *           
*   JUL28/03 (JRD) --- GO BACK TO OLD DEMO BLOCK SETUP.  THE MVCL   *           
*                      WAS DESTROYING THE RATINGS.  NOTE THERE ARE  *           
*                      RARE CASES WHERE THIS CAN CAUSE A PHANTOM    *           
*                      INVENTORY BOOK TO APPEAR.  MOSTLY WHEN THE   *           
*                      FIRST PROGRAM FOR A STATION DOES NOT HAVE    *           
*                      THE FIRST BOOK IN THE REQUEST                *           
*                                                                   *           
*   AUG04/03 (JRD) --- USE DEMOVAL/UNDEM TO OUTPUT CATEGORY NAMES   *           
*                                                                   *           
*   OCT21/04 (BU ) --- OPEN DEMO FILES FOR FETCH CALL TO GET        *           
*                      STATION MARKET NUMBER                        *           
*                                                                   *           
*   MAR28/05 (BU ) --- ADD DEMFILV                                  *           
*                                                                   *           
*   APR18/06 (BU ) --- UNIVISION VERSION                            *           
*                                                                   *           
*   SEP04/07 (BU ) --- EXPAND STATION LIST                          *           
*                      PROVIDE 'HEADERS ONLY' QOPTION1 = H          *           
*                      ORIGINALLY 'REREP3U02', WHICH STILL          *           
*                      PROCESSES DETAILS FOR TWO STATIONS           *           
*                                                                   *           
*   SEP10/07 (BU ) --- REMOVE WJRB DUPLICATE FROM LIST              *           
*                                                                   *           
*   OCT03/07 (BU ) --- REMOVE KUTH DUPLICATE FROM LIST              *           
*                                                                   *           
*   OCT24/07 (DE ) --- USE SOFT DEMOFILE OPEN LISTS                 *           
*                                                                   *           
*   FEB06/08 (SKU) --- ADD PARENT PLUS STATIONS                     *           
*                                                                   *           
*    APR/10  (SMY) --- RELINK FOR NEW INVENTORY KEY                 *           
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
**  INSERT HERE >>                                                              
         ENTRY RE3P02                                                           
*         NTRY SSB                                                              
RE3P02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 OVERWRKQ,**RE3P02,R7,R8,RR=RE                                    
*                                                                               
         LR    R9,RC               A(THIS MOD'S WORKSPACE=OVERWRKQ)             
         USING OVERWRKD,R9                                                      
*                                                                               
         ST    RE,RELO             SET RELOCATABLE ADDRESS                      
*                                                                               
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         LR    RE,R9                                                            
         AHI   RE,XDMLIST-OVERWRKD                                              
         MVC   0(8,RE),=CL8'DEMOLIST'                                           
         AHI   RE,XDMSAVE-XDMLIST                                               
         MVC   0(8,RE),=CL8'DEMOSAVE'                                           
         AHI   RE,XDMRTG-XDMSAVE                                                
         MVC   0(8,RE),=CL8'DEMORTG'                                            
         AHI   RE,XDMSHR-XDMRTG                                                 
         MVC   0(8,RE),=CL8'DEMOSHR'                                            
         AHI   RE,XDMLVL-XDMSHR                                                 
         MVC   0(8,RE),=CL8'DEMOLVL'                                            
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
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),DMSYS,DMFLIST                        
*                                                                               
*   TEST                                                                        
         MVC   P+1(15),=C'DEMO FILES OPEN'                                      
         GOTO1 REPORT                                                           
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
*                                  LOAD FETCH ROUTINE                           
*                                                                               
         CLC   =C'TST',QUESTOR+5   USE 'TEST' VERSION OF REFETCH?               
         BE    INIT0020            YES - USE TEST                               
         CLC   =C'TSB',QUESTOR+5   USE 'TEST' VERSION OF REFETCH?               
         BE    INIT0022            YES - USE TEST                               
         GOTOX LOADER,DMCB,=CL8'T00AA4',0                                       
         B     INIT0040                                                         
*                                                                               
INIT0020 EQU   *                                                                
         GOTOX LOADER,DMCB,=CL8'T00AA4A',0                                      
         B     INIT0040                                                         
INIT0022 EQU   *                                                                
         GOTOX LOADER,DMCB,=CL8'T00AA4C',0                                      
         B     INIT0040                                                         
*                                                                               
INIT0040 EQU   *                                                                
         OC    DMCB+4,DMCB+4       LOADED OKAY?                                 
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - KILL JOB                               
*                                                                               
         MVC   VFETCH,4(R1)        SET A(FETCH)                                 
*************************************************************                   
***SMY****  NOTE C AT END OF T00ADD BELOW FOR TESTING *******                   
*************************************************************                   
*SMY*    GOTOX LOADER,DMCB,=CL8'T00ADDC',0   DEMAND                             
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
         MVC   VDEMOUT,4(R1)       SET A(DEMOOT)                                
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
         GOTOX LOADER,DMCB,=CL8'T00A0F',0   DAYUNPK                             
         OC    DMCB+4,DMCB+4       LOADED OKAY?                                 
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - KILL JOB                               
*                                                                               
         MVC   DAYUNPK,4(R1)       SET A(DAYUNPK)                               
*                                                                               
         OPEN  (INTFILE,(OUTPUT))  OPEN INTERIM FILE                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,DYPTSOUT         OUTPUT DAYPART RECORDS                       
         BAS   RE,DEMOSOUT         OUTPUT DEMOS AND SETUP DEMO                  
*                                  BLOCK                                        
         BAS   RE,NEWINV                                                        
         BAS   RE,GENFILE          RETRIEVE GENERATIONAL NAME                   
         CLOSE (INTFILE,)          CLOSE INTERIM FILE                           
LOOP     EQU   *                                                                
*                                                                               
         OC    PUTCTR,PUTCTR       ANY OUTPUT FOR THIS JOB?                     
         BZ    INIT0060            NOTHING WRITTEN TO FILE                      
*                                                                               
         GOTO1 =A(EDICT),DMCB,RETAREA    OUTPUT EDICT HEADER                    
*                                                                               
         MVI   FORCEHED,C'N'       DON'T PAGE BREAK                             
         MVC   P+01(26),=C'RATINGS FILE PRODUCED    :'                          
         MVC   P+28(26),=C'RECORDS WRITTEN =        :'                          
         EDIT  PUTCTR,(9,P+47)                                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+1(13),=C'RETURNED NAME'                                        
         MVC   P+15(44),RETAREA                                                 
         GOTO1 REPORT                                                           
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
*                                                                               
*   TEST DISPLAY                                                                
*                                                                               
         MVC   P+1(16),=C'BOOKS PROCESSED:'                                     
         GOTO1 REPORT                                                           
*                                                                               
         ZIC   R3,INP#BKS                                                       
         LA    R2,INPBOOKS                                                      
TFIN0020 EQU   *                                                                
         MVC   P+1(08),=C'SKIPPED:'                                             
         GOTO1 HEXOUT,DMCB,0(R2),P+12,6,=C'TOG'                                 
         CLI   DINPBKYR(R2),X'99'  SKIP BOOK INDICATOR?                         
         BE    TFIN0040            YES -                                        
         MVC   P,SPACES            CLEAR ABOVE INFO FROM PRINT LINE             
         MVC   P+1(08),=C'ACTUAL :'                                             
         CLI   0(R2),X'60'         ESTIMATE BOOK?                               
         BNE   TFIN0025            NO  -                                        
         MVC   P+1(08),=C'ESTIMATE'                                             
TFIN0025 EQU   *                                                                
         CLI   0(R2),X'44'         PROJECTION BOOK?                             
         BNE   TFIN0030            NO  - LEAVE AS ACTUAL                        
         MVC   P+1(10),=C'PROJECTION'                                           
TFIN0030 EQU   *                                                                
         MVI   WORK+2,1            SET DAY OF DATE                              
         MVC   WORK(2),DINPBKYR(R2)                                             
*                                  SET YR/MO OF DATE                            
         GOTO1 DATCON,DMCB,(3,WORK),(6,P+12)                                    
TFIN0040 EQU   *                                                                
         GOTO1 REPORT                                                           
TFIN0050 EQU   *                                                                
         LA    R2,LINPBKS(R2)                                                   
         BCT   R3,TFIN0020         GO BACK FOR NEXT                             
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
* ETOOBIG WILL CAUSE MODULE TO ABORT.                                 *         
* THIS IS TEMPORARY, UNTIL I UNDERSTAND WHAT IS HAPPENING.            *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
ETOOBIG  EQU   *                                                                
***>>>   MVC   ERROR,=Y(804)                                                    
***>>>   XIT1                                                                   
DIE      DC    H'0'                                                             
*********************************************************************           
GENFILE  NTR1                                                                   
*                                                                               
         OC    PUTCTR,PUTCTR       ANY OUTPUT FOR THIS JOB?                     
         BZ    GENF0020            NO  - NOTHING WRITTEN TO FILE                
*                                                                               
         LINKX MF=(E,PARMLST),SF=(E,LINKLST)                                    
*                                                                               
GENF0020 EQU   *                                                                
         XIT1                                                                   
*                                                                               
PARMLST  CALL  ,(DDNAME,RETAREA),MF=L                                           
LINKLST  LINKX EP=FRGETDSN,SF=L                                                 
DDNAME   DC    CL8'INTFILE'                                                     
RETAREA  DS    CL44                                                             
*********************************************************************           
DYPTSOUT NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'3C'           INSERT RECORD TYPE                           
         MVC   KEY+24(2),RCREPFL   INSERT REP CODE                              
         GOTO1 HIGH                                                             
         B     DYPT0040                                                         
DYPT0020 EQU   *                                                                
         GOTO1 SEQ                                                              
DYPT0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME RECTYPE/REP?                            
         BNE   DYPT0800            NO  - FINISHED                               
         GOTO1 GETDYPT             RETRIEVE RECORD                              
         L     R2,AIOREC           SET A(IOAREA)                                
         USING RRDPREC,R2                                                       
         MVC   MYP,SPACES          CLEAR O/P DATA LINE                          
         MVC   MYP+DYPTCTL(4),=C'DYPT'     INSERT CONTROL ID                    
         MVC   MYP+DYPTDPT(1),RRDPKDPT     INSERT DAYPART CODE                  
         MVC   MYP+DYPTDPT3(3),RRDPSNAM    INSERT SHORT DYPT NAME               
         MVC   MYP+DYPTDPTL(15),RRDPLNAM   INSERT LONG  DYPT NAME               
         GOTO1 VDOPUT              OUTPUT THE RECORD                            
         B     DYPT0020            GO BACK FOR NEXT                             
DYPT0800 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
*********************************************************************           
* DEMOSOUT - OUTPUT DEMO HEADER RECORDS AND ADD DEMOS TO                        
*            INPUT AND TEST REQUEST BLOCKS                                      
*********************************************************************           
DEMOSOUT NTR1                                                                   
         LHI   R3,1                HDR INDEX                                    
*                                                                               
         SR    R4,R4               DEMO COUNT                                   
*                                                                               
         LA    R2,*                                                             
         AHI   R2,(DEMOTAB-(*-4))                                               
         MVI   BYTE,C'R'           FIRST PASS IS FOR RATINGS                    
*                                                                               
         XC    HALF,HALF                   DEMOS ON LINE                        
         MVC   MYP,SPACES                  CLEAR O/P DATA LINE                  
         MVC   MYP+DX01CTL(4),=C'DX01'     INSERT CONTROL ID                    
*                                                                               
DEMO0010 DS    0H                                                               
         CLI   0(R2),X'FF'         END OF LIST?                                 
         BNE   DEMO0020            YES                                          
         CLI   BYTE,C'I'           PROCESSED IMRESSIONS?                        
         BE    DEMO0040            YES                                          
*                                                                               
         LA    R2,*                                                             
         AHI   R2,(DEMOTAB-(*-4))                                               
         MVI   BYTE,C'I'           SECOND PASS IS FOR IMPRESSIONS               
*                                                                               
DEMO0020 DS    0H                                                               
         MVC   0(1,R2),BYTE          MOVE IN R/I                                
         LH    RF,HALF             GET NUM OF DEMOS                             
         MHI   RF,DX01DMOL         MULTIPLY BY LENGTH                           
         LA    RE,MYP(RF)          INDEX INTO LINE                              
         CLI   BYTE,C'I'           IMRESSIONS?                                  
         BE    DEMO0022            YES                                          
*                                                                               
         MVC   DX01DMO1(DX01DMOL,RE),0(R2)                                      
         B     DEMO0023                                                         
*                                                                               
DEMO0022 DS    0H                                                               
         MVC   DX01DMO1(DX01DMOL-1,RE),1(R2)                                    
         OC    DX01DMO1(DX01DMOL,RE),SPACES                                     
*                                                                               
DEMO0023 DS    0H                                                               
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8             16 BYTE FIELD                              
         MVI   WORK+5,8            ADDRESS OF DATA                              
         CLI   BYTE,C'I'           IMRESSIONS?                                  
         BE    DEMO0024            YES                                          
*                                                                               
         MVI   WORK+5,8            ADDRESS OF DATA                              
         MVC   WORK+8(8),0(R2)                                                  
         B     DEMO0025                                                         
*                                                                               
DEMO0024 DS    0H                                                               
         MVI   WORK+5,7            ADDRESS OF DATA                              
         MVC   WORK+8(7),1(R2)                                                  
*                                                                               
DEMO0025 DS    0H                                                               
         L     RE,AIO1                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,XCOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'PAV'                                                   
         DROP  RE                                                               
*                                                                               
         GOTO1 VDEMOVAL,DMCB,(1,WORK),(1,WORK+16+8),(0,AIO1),0                  
*                                                                               
         CLI   4(R1),0             GOOD DEMO?                                   
         BE    BADDEMO             NO                                           
*                                                                               
         LA    RE,INPDEMS                                                       
         LR    RF,R4                                                            
         MHI   RF,4                                                             
         AR    RE,RF                                                            
         MVC   0(3,RE),WORK+16+8   STORE DEMOVAL BYTES                          
         AHI   RE,TSTDEMS-INPDEMS                                               
         MVC   0(3,RE),WORK+16+8   STORE DEMOVAL BYTES                          
*                                                                               
         MVI   WORK+16+8,0                                                      
         CLI   WORK+16+8+1,C'T'                                                 
         BNE   *+8                                                              
         MVI   WORK+16+8+1,C'I'                                                 
*                                                                               
         L     RE,AIO1                                                          
         USING DBLOCK,RE                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,XCOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         DROP  RE                                                               
*                                                                               
         XC    WORK+8(8),WORK+8                                                 
         GOTO1 VDEMOCON,DMCB,(1,WORK+16+8),(9,WORK+8),(0,AIO1)                  
*                                                                               
         LH    RF,HALF             GET NUM OF DEMOS                             
         MHI   RF,DX01DMOL         MULTIPLY BY LENGTH                           
         LA    RE,MYP(RF)          INDEX INTO LINE                              
         MVC   DX01DMO1(8,RE),WORK+8                                            
         OC    DX01DMO1(8,RE),SPACES                                            
*                                                                               
         AHI   R4,1                                                             
         B     DEMO0026                                                         
*                                                                               
BADDEMO  DS    0H                                                               
         MVC   P(10),=CL10'BAD DEMO: '                                          
         MVC   P+10(8),0(R2)                                                    
         GOTO1 REPORT                                                           
         B     DEMO0030                                                         
*                                                                               
DEMO0026 DS    0H                                                               
         LH    RE,HALF                                                          
         AHI   RE,1                                                             
         STH   RE,HALF                                                          
         CHI   RE,4                4 DEMOS PER LINE                             
         BL    DEMO0030                                                         
*                                                                               
         GOTO1 VDOPUT              OUTPUT THE RECORD                            
*                                                                               
         AHI   R3,1                                                             
         XC    HALF,HALF                                                        
         MVC   MYP,SPACES                  CLEAR O/P DATA LINE                  
         MVC   MYP+DX01CTL(4),=C'DX00'     INSERT CONTROL ID                    
*                                                                               
         EDIT  (R3),(2,MYP+DX01CTL+2),FILL=0                                    
*                                                                               
DEMO0030 DS    0H                                                               
         LA    R2,DX01DMOL(R2)                                                  
         B     DEMO0010                                                         
*                                                                               
DEMO0040 DS    0H                                                               
         OC    HALF,HALF           LEFT OVER DATA?                              
         BZ    DEMO0050            NO                                           
*                                                                               
         GOTO1 VDOPUT              OUTPUT THE RECORD                            
*                                                                               
DEMO0050 DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
NEWINV   NTR1                                                                   
         MVI   RMODE,C'I'          NEW INVENTORY RMODE                          
         LA    RF,TSTBLOCK         SET A(TEST INPUT BLOCK)                      
         CLC   =C'XXX',QUESTOR+2   USE 'TEST' BLOCK WITH REQ STATION            
         BNE   NINV0018            NO                                           
*                                                                               
         MVC   TSTSTA,QSTATION                                                  
         B     NINV0020            YES - USE TEST                               
*                                                                               
NINV0018 EQU   *                                                                
         CLC   =C'TST',QUESTOR+2   USE 'TEST' BLOCK INPUT?                      
         BE    NINV0020            YES - USE TEST                               
         LA    RF,INPBLOCK         NO  - SET TO PRODUCTION INPUT BLOCK          
NINV0020 EQU   *                                                                
         ST    RF,OVPARMS+4        STUFF A(TEST INPUT BLOCK)                    
         BAS   RE,SETBOOKS         SET INPUT BOOKS                              
*                                                                               
*    INITIAL TESTING WILL BE DONE WITH A PREPACKAGED TEST INPUT                 
*        BLOCK.  THIS MUST BE REPLACED BY THE DYNAMIC INSERTION                 
*        OF REAL REQUEST DATA.                                                  
*                                                                               
         L     R2,OVPARMS+4                                                     
*                                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(06),=C'NUMSTA'                                               
**       MVC   P+07(16),0(R2)                                                   
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   NUMSTAS,0(R2)       NUMBER OF STATIONS                           
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST STATION                         
         ST    RF,FRSTSTA                                                       
         ZIC   RF,NUMSTAS                                                       
         MH    RF,=Y(STALENQ)      BUMP TO DAYPARTS                             
         AR    R2,RF                                                            
*                                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(06),=C'NUMDPT'                                               
**       MVC   P+07(16),0(R2)                                                   
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   NUMDPTS,0(R2)       NUMBER OF DAYPARTS                           
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST DAYPART                         
         ST    RF,FRSTDPT                                                       
         ZIC   RF,NUMDPTS                                                       
         MH    RF,=Y(DPTLENQ)      BUMP TO FLIGHTS                              
         AR    R2,RF                                                            
*                                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(06),=C'NUMFLT'                                               
**       MVC   P+07(32),0(R2)                                                   
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   NUMFLTS,0(R2)       NUMBER OF FLIGHTS                            
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST FLIGHT                          
         ST    RF,FRSTFLT                                                       
         MVC   FLTSTART,0(R2)      SAVE START OF TOTAL FLIGHT                   
         ZIC   RF,NUMFLTS                                                       
         BCTR  RF,0                BUMP TO LAST FLIGHT                          
         MH    RF,=Y(6)                                                         
         AR    R2,RF                                                            
         MVC   FLTEND,3(R2)        SAVE END OF TOTAL FLIGHT                     
         AH    R2,=Y(FLTLENQ)      BUMP TO BOOKS                                
*                                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(06),=C'NUMBKS'                                               
***      MVC   P+07(36),0(R2)                                                   
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   NUMBKS,0(R2)        NUMBER OF BOOKS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         ST    RF,AFRSTBK          A(FIRST BOOK)                                
         S     RF,OVPARMS+4        DISPLACEMENT TO FIRST BOOK                   
         ST    RF,FRSTBK           D(FIRST BOOK)                                
         ZIC   RF,NUMBKS                                                        
         MH    RF,=Y(BKLENQ)       BUMP TO UPGRADES                             
         AR    R2,RF                                                            
*                                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(06),=C'NUMUPG'                                               
***      MVC   P+07(16),0(R2)                                                   
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   NUMUPGS,0(R2)       NUMBER OF UPGRADES                           
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST UPGRADE                         
         ST    RF,FRSTUPG                                                       
         ZIC   RF,NUMUPGS                                                       
         MH    RF,=Y(UPGLENQ)      BUMP TO DEMOS                                
         AR    R2,RF                                                            
*                                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(06),=C'NUMDEM'                                               
***      MVC   P+07(16),0(R2)                                                   
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   NUMDEMS,0(R2)       NUMBER OF DEMOS                              
         LA    R2,2(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST DEMO                            
         ST    RF,FRSTDEM                                                       
         ZICM  RF,NUMDEMS,2                                                     
         MH    RF,=Y(DEMLENQ)      BUMP TO RATE CARDS                           
         AR    R2,RF                                                            
*                                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(06),=C'NUMRCD'                                               
***      MVC   P+07(16),0(R2)                                                   
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   NUMRCDS,0(R2)       NUMBER OF RATE CARDS                         
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST RATE CARD                       
         ST    RF,FRSTRCD                                                       
         ZIC   RF,NUMRCDS                                                       
         MH    RF,=Y(RCDLENQ)                                                   
         AR    R2,RF                                                            
         EJECT                                                                  
*                                                                               
         MVC   CURSTA,FRSTSTA      SET CURRENT STATION                          
         MVC   REMSTAS,NUMSTAS     SET NUMBER OF REMAINING STATIONS             
*                                                                               
         CLI   NUMSTAS,0                                                        
         BE    MODEEXIT                                                         
*                                                                               
         CLI   NUMDPTS,0                                                        
         BE    MODEEXIT                                                         
*                                                                               
***                                                                             
***  THIS CODE IS CORRECT BUT EXPOSES A BUG IN THE PC CODE                      
***  UP TO THE BETA RELEASE 2.3.0.16 AND AS ITS HARMLESS TO                     
***  LEAVE THIS VALUE UNINTIALIZED THAT WHAT WE DID.   10/16/01                 
***                                                                             
***      OI    MISCFLG1,MF1TMPB1   SET STATION INDICATED                        
*                                  FIRST STATION IS ASSUMED                     
********************************************************                        
* LOOP THROUGH ALL THE STATIONS                                                 
********************************************************                        
LPSTA000 DS    0H                                                               
         LA    R0,FETCHBLK         CLEAR THE FETCH PARAMETER BLOCK              
         LH    R1,=Y(RFTBLKL)                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
*                                                                               
         MVC   RFTACOM,XCOMFACS    A(COMFACS)                                   
*&&DO                                                                           
*  TEST                                                                         
         L     R3,RFTACOM                                                       
         LA    R4,1                                                             
         LA    R5,84                                                            
TEST0060 EQU   *                                                                
         MVC   P+1(07),=C'COMFACS'                                              
         EDIT  (R4),(3,P+9)                                                     
         GOTO1 HEXOUT,DMCB,0(R3),P+16,4,=C'TOG'                                 
         GOTO1 REPORT                                                           
         LA    R3,4(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,TEST0060                                                      
         DC    H'0'                                                             
*   TEST END                                                                    
*&&                                                                             
         MVC   RFTAIO1,AIO3        A(2K IO AREA)                                
         MVC   RFTAIO2,AIO4        A(2K IO AREA)                                
*                                                                               
         LR    RE,R9                                                            
         AHI   RE,(FETCHWRK-OVERWRKD)                                           
         STCM  RE,15,RFTAWRK       A(6K WORK AREA)                              
         MVC   RFTCREP,RCREPFL     REP CODE                                     
*                                                                               
         MVI   RTSRVC,C'N'         SET TO NSI                                   
**JRD    MVI   RTSRVC,C' '         SET TO NSI                                   
*                                                                               
         MVC   RFTCSRC,RTSRVC      RATING SERVICE                               
         MVI   RFTAMODE,RFTAINVQ   FETCH RMODE                                  
         MVI   RFTCDCTL,RFTCDC1Q   FETCH METHOD                                 
         LA    RE,FTCHHOOK         HOOK ROUTINE                                 
         STCM  RE,15,RFTHOOKA                                                   
         OI    RFTCNTL,RFTCHDRQ    INCLUDE HEADERS                              
*                                                                               
*   TEST HEADERS ONLY REQUESTED                                                 
*                                                                               
         CLI   QOPTION1,C'H'       HEADERS ONLY?                                
         BE    HDRS0020            YES - DON'T SET OTHER FLAGS                  
*                                                                               
         OI    RFTCNTL,RFTCDEMQ    INCLUDE DEMOS                                
         OI    RFTCNTL,RFTCSLVQ    INCLUDE SHARES AND LEVELS                    
         OI    RFTCNTL,RFTCFTNQ    INCLUDE FOOTNOTES                            
**JRD    OI    RFTCNTL,RFTCTXTQ    INCLUDE TEXT                                 
**JRD    MVI   RFTCTXTT,RFTCTXIQ   FROM INVENTORY                               
**JRD    OI    RFTCNTL,RFTCRTEQ    INCLUDE RATES                                
**       OI    RFTCNTL,RFTCRNWQ    USE NEW STYLE RATES                          
*                                                                               
*                                                                               
HDRS0020 EQU   *                                                                
         MVI   RFTCTXTW,80         WRAP WIDTH                                   
*                                                                               
*  DON'T FILTER ON FLIGHTS IN 'FETCH'                                           
*                                                                               
**       GOTO1 DATCON,DMCB,(8,FLTSTART),(2,RFTCEFST)                            
**       GOTO1 DATCON,DMCB,(8,FLTEND),(2,RFTCEFEN)                              
*                                                                               
         L     RE,OVPARMS+4        POINT TO CURRENT STATIONS                    
         A     RE,CURSTA                                                        
         MVC   RFTCSTAT,0(RE)      STATION CALL LETTERS                         
         CLI   RFTCSTAT+4,C' '     NEED 'T' SET?                                
         BNE   *+8                 NO                                           
         MVI   RFTCSTAT+4,C'T'                                                  
*                                                                               
         L     RF,AIO1             SET A(IO AREA)                               
         ST    RF,ACURPARM         STORE ADDRESS OF CURRENT PARMS               
         EJECT                                                                  
*--------------------*                                                          
* BUILD DAYPART LIST *                                                          
*--------------------*                                                          
*                                                                               
*   TEST                                                                        
***      MVC   P+1(19),=C'@BUILD DAYPART LIST'                                  
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         MVI   RFTCDTMS,LONGPARM   SET PARM AS NULL TERM. LIST                  
         STCM  RF,15,RFTCDTMS+1                                                 
         ZIC   R1,NUMDPTS          SET NUMBER OF REMAINING DAYPARTS             
         L     RE,OVPARMS+4        POINT TO CURRENT DAYPART                     
         A     RE,FRSTDPT                                                       
DPTS010  DS    0H                  LOOP AND SET DAYPARTS                        
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(RFTCDTLQ*2))                                        
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         USING RFTCDTMS,RF                                                      
         XC    0(RFTCDTLQ,RF),0(RF)                                             
         MVC   RFTCDTDP,0(RE)                                                   
         LA    RF,RFTCDTLQ(RF)                                                  
         LA    RE,DPTLENQ(RE)                                                   
         DROP  RF                                                               
*                                                                               
         BCT   R1,DPTS010                                                       
*                                                                               
         XC    0(RFTCDTLQ,RF),0(RF)                                             
         LA    RF,RFTCDTLQ(RF)                                                  
         ST    RF,ACURPARM                                                      
*                                                                               
         EJECT                                                                  
*-----------------*                                                             
* BUILD BOOK LIST *                                                             
*-----------------*                                                             
*                                                                               
*   TEST                                                                        
***      MVC   P+1(19),=C'@BUILD BOOK    LIST'                                  
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         OC    NUMDEMS,NUMDEMS                                                  
         BZ    DEMS100                                                          
*                                                                               
         CLI   NUMBKS,0                                                         
         BE    BKS0100                                                          
*                                                                               
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         MVI   RFTCBKS,LONGPARM    SET PARM AS NULL TERM. LIST                  
         STCM  RF,15,RFTCBKS+1                                                  
         ZIC   R1,NUMBKS           SET NUMBER OF REMAINING BOOKS                
         L     RE,OVPARMS+4        POINT TO CURRENT BOOK                        
         A     RE,FRSTBK                                                        
BKS0010  DS    0H                  LOOP AND SET BOOKS                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(RFTCBKLQ*2))                                        
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         USING RFTCBKS,RF                                                       
         XC    0(RFTCBKLQ,RF),0(RF)                                             
         MVC   0(RFTCBKLQ,RF),0(RE)                                             
         LA    RF,RFTCBKLQ(RF)                                                  
         LA    RE,BKLENQ(RE)                                                    
         DROP  RF                                                               
*                                                                               
         BCT   R1,BKS0010                                                       
*                                                                               
         XC    0(RFTCBKLQ,RF),0(RF)                                             
         LA    RF,RFTCBKLQ(RF)                                                  
         ST    RF,ACURPARM                                                      
*                                                                               
BKS0100  DS    0H                                                               
         EJECT                                                                  
*--------------------*                                                          
* BUILD UPGRADE LIST *                                                          
*--------------------*                                                          
*                                                                               
*   TEST                                                                        
***      MVC   P+1(19),=C'@BUILD UPGRADE LIST'                                  
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLI   NUMUPGS,0                                                        
         BE    UPGS100                                                          
*                                                                               
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         STCM  RF,15,RFTCUPGA                                                   
         ZIC   R1,NUMUPGS          SET NUMBER OF REMAINING BOOKS                
         L     RE,OVPARMS+4        POINT TO CURRENT BOOK                        
         A     RE,FRSTUPG                                                       
UPGS010  DS    0H                  LOOP AND SET BOOKS                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(UPGLENQ*2))                                         
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         MVC   0(UPGLENQ-1,RF),0(RE)                                            
         LA    RF,UPGLENQ-1(RF)                                                 
         LA    RE,UPGLENQ(RE)                                                   
         BCT   R1,UPGS010                                                       
*                                                                               
         XC    0(UPGLENQ,RF),0(RF)                                              
         LA    RF,UPGLENQ(RF)                                                   
         ST    RF,ACURPARM                                                      
*                                                                               
UPGS100  DS    0H                                                               
         EJECT                                                                  
*-----------------*                                                             
* BUILD DEMO LIST *                                                             
*-----------------*                                                             
*                                                                               
*   TEST                                                                        
***      MVC   P+1(19),=C'@BUILD DEMO    LIST'                                  
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         LA    RF,DEMLIST          SET A(INDIRECT DEMO LIST AREA)               
         ZICM  R1,NUMDEMS,2        SET NUMBER OF REMAINING DEMOS                
         CHI   R1,NUMDEMQ          TOO MANY DEMOS?                              
         BNH   *+8                 NO  - SHOULD BE OKAY                         
         LA    R1,NUMDEMQ          YES - SET TO MAX NUMBER                      
*                                                                               
*   THIS IS SILLY HERE, AS THIS IS A CONTROLLED SITUATION.  ORIG                
*        CODING WAS FOR ON-LINE APPLICATION.                                    
*                                                                               
         L     RE,OVPARMS+4        POINT TO CURRENT DEMO                        
         A     RE,FRSTDEM                                                       
DEMS010  DS    0H                  LOOP AND SET DEMOS                           
         MVC   0(3,RF),0(RE)                                                    
         LA    RF,3(RF)                                                         
         LA    RE,DEMLENQ(RE)                                                   
         BCT   R1,DEMS010                                                       
*                                                                               
*   ESTABLISH INDIRECT CONTROL BLOCK CALL FOR REFETCH DEMOS                     
*                                                                               
         MVI   RFTCDMIN,X'FF'      INSERT INDIRECT INDICATOR                    
         MVC   RFTCDMNM,NUMDEMS    INSERT NUMBER OF DEMOS                       
         LA    RF,DEMLIST          SET A(DEMO LIST)                             
         STCM  RF,15,RFTCDEMA      INSERT INTO BLOCK                            
         LA    RF,DEMSAVE          SET A(DEMO SAVE AREA)                        
         STCM  RF,15,RFTCDEMB      INSERT INTO BLOCK                            
         AHI   RF,DEMORTG-DEMSAVE  SET A(RATING RETURN AREA)                    
         STCM  RF,15,RFTCDMRT      INSERT INTO BLOCK                            
         AHI   RF,DEMOSHR-DEMORTG  SET A(SHARE RETURN AREA)                     
         STCM  RF,15,RFTCDMSH      INSERT INTO BLOCK                            
         AHI   RF,DEMOLVL-DEMOSHR  SET A(LEVEL RETURN AREA)                     
         STCM  RF,15,RFTCDMLV      INSERT INTO BLOCK                            
*                                                                               
DEMS100  DS    0H                                                               
         EJECT                                                                  
*----------------------*                                                        
* BUILD RATE CARD LIST *                                                        
*----------------------*                                                        
RCDS000  DS    0H                  LOOP AND SET RATE CARDS                      
*                                                                               
*   TEST                                                                        
***      MVC   P+1(19),=C'@BUILD RATECDS LIST'                                  
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLI   NUMRCDS,0                                                        
         BE    RCDS100                                                          
*                                                                               
         MVC   RFTCRDDT(3),FLTSTART                                             
         MVC   RFTCRDDT+3(3),FLTEND                                             
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         STCM  RF,15,RFTCRDRC                                                   
         ZIC   R1,NUMRCDS          SET NUMBER OF REMAINING RATE CARDS           
         L     RE,OVPARMS+4        POINT TO CURRENT RATE CARD                   
         A     RE,FRSTRCD                                                       
RCDS010  DS    0H                  LOOP AND SET RATES                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(RFTCRTSL*2))                                        
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         USING RFTCRTES,RF                                                      
         XC    0(RCDLENQ,RF),0(RF)                                              
         MVC   0(RCDLENQ,RF),0(RE)                                              
         LA    RF,RCDLENQ(RF)                                                   
         LA    RE,RCDLENQ(RE)                                                   
         DROP  RF                                                               
*                                                                               
         BCT   R1,RCDS010                                                       
*                                                                               
         XC    0(RCDLENQ,RF),0(RF)                                              
         LA    RF,RCDLENQ(RF)                                                   
         ST    RF,ACURPARM                                                      
*                                                                               
RCDS100  DS    0H                                                               
         EJECT                                                                  
*------------*                                                                  
* FETCH CALL *                                                                  
*------------*                                                                  
*                                                                               
*   TEST                                                                        
**       MVC   P+1(19),=C'PRE-FETCH          '                                  
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLI   RMODE,C'I'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*   TEST:  DISPLAY BOOKS                                                        
***      MVC   P+1(09),=C'FETCHBLK:'                                            
***      GOTO1 REPORT                                                           
***      LA    R4,FETCHBLK                                                      
***      L     RF,=F'1496'                                                      
***      GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST:  END                                                                  
*                                                                               
         GOTO1 VFETCH,DMCB,FETCHBLK                                             
         DROP  R4                                                               
*                                                                               
*   TEST                                                                        
***      MVC   P+1(19),=C'POST FETCH         '                                  
***      GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*                                                                               
LPSTA100 DS    0H                  PROCESS NEXT STATION                         
         ZIC   R0,REMSTAS          REMAINING STATIONS                           
         BCTR  R0,0                                                             
         STC   R0,REMSTAS                                                       
         CLI   REMSTAS,0           ANYMORE STATIONS?                            
         BNH   LPSTAX              NO                                           
*                                                                               
         L     RE,CURSTA           POINT TO CURRENT STATION                     
         LA    RE,5(RE)            BUMP TO NEXT STATION                         
         ST    RE,CURSTA                                                        
         NI    MISCFLG1,FF-MF1TMPB1   SET NEW STATION                           
         B     LPSTA000               FETCH IT                                  
*                                                                               
LPSTAX   DS    0H                  END OF FETCH LOOP                            
*                                                                               
*   TEST                                                                        
**       MVC   P+1(19),=C'STATIONS FINISHED  '                                  
**       GOTO1 REPORT                                                           
***      DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*  SETBOOKS:  INSERT THE DATES INTO THE BOOK PULL LIST, BASED ON                
*        TODAY'S DATE.                                                          
*        BOOK LIST CONTAINS 5 YEARS' ACTUALS:                                   
*             CURRENT YEAR FEB/MAY/JULY/NOVEMBER:  THESE MAY NOT                
*                 BE ON FILE YET                                                
*             FOUR PRIOR YEAR FEB/MAY/JULY/NOVEMBER: SHOULD ALL                 
*                 BE ON FILE                                                    
*        BOOK LIST CONTAINS 2 YEARS' PROJECTIONS:                               
*             CURRENT YEAR FEB/MAY/JULY/NOVEMBER:  MAY OR MAY NOT BE            
*                 ON FILE, DEPENDING UPON WHAT CLIENT HAS GENERATED             
*             NEXT YEAR FEB/MAY/JULY/NOVEMBER:  SAME COMMENT.                   
*             FOR CURRENT YEAR:  DEPENDING ON 'TODAY'S DATE', THERE             
*                 MAY BE BOTH AN ESTIMATE AND AN ACTUAL BOOK FOR A              
*                 PARTICULAR PERIOD.  FOR THE MOST RECENT SWEEP,                
*                 BOTH ESTIMATE AND ACTUAL WILL BE TRANSMITTED.                 
*                 THIS WILL ENSURE THAT THERE IS NO PERIOD IN WHICH             
*                 NOTHING IS SENT.                                              
*                                                                               
SETBOOKS NTR1                                                                   
         GOTO1 DATCON,DMCB,(5,WORK),(3,WORK+6)                                  
*                                  GET TODAY'S DATE YYMMDD BINARY               
*                                                                               
*   TEST DATE ACTION: PUT IN DATE WITHIN YEAR TO FORCE EARLIEST YEAR            
*        TO ENSURE PROPER APPLICATION WITHIN BOOK LIST                          
*                                                                               
****     MVC   WORK+6(3),=X'660C01'  (YYMMDD)                                   
*                                                                               
*   TEST DATE END                                                               
*                                                                               
         LA    R2,INPBOOKS         SET A(BOOKS LIST)                            
         ZIC   RF,INP#BKS          SET LOOP CONTROL                             
         ZIC   RE,WORK+6           SET CURRENT YEAR                             
         SH    RE,=H'4'            BACK UP FOUR YEARS                           
SPBK0020 EQU   *                                                                
         STC   RE,DINPBKYR(R2)     NO  - INSERT YEAR INTO LIST                  
         CLI   DINPBKMO(R2),11     BOOK = NOVEMBER?                             
         BNE   SPBK0040            NO  - BUMP TO NEXT BOOK                      
         LA    RE,1(RE)            YES - BUMP YEAR BY 1                         
SPBK0040 EQU   *                                                                
         LA    R2,LINPBKS(R2)      BUMP TO NEXT BOOK                            
*                                                                               
*   ESTIMATE/PROJECTION BOOKS ARE PAIRED:  ESTIMATE (60) WILL BE                
*        FIRST, FOLLOWED BY PROJECTION (44).                                    
*                                                                               
         CLI   0(R2),X'60'         ESTIMATE BOOK?                               
         BE    SPBK0060            YES - PROCESS ESTIMATE BOOKS                 
         BCT   RF,SPBK0020         GO BACK FOR NEXT                             
         DC    H'0'                THIS SHOULDN'T REACH HERE EVER:              
*                                     EST BKS ARE IN THE TABLE                  
SPBK0060 EQU   *                   YES - PROCESS ESTIMATE BOOKS                 
*                                     AND PROJECTION BOOKS                      
         BCTR  RE,0                TAKE OFF LAST YEAR BUMP                      
         BCTR  RF,0                TAKE OFF FOR X'60' ESTIMATE BOOK             
*                                                                               
*   REMAINING ENTRIES IN LIST ARE ESTIMATE/PROJECTION COMBINATIONS.             
*       THESE WILL BE PROCESSED IN PAIRS.  THE LOOP IS THEREFORE                
*       HALF OF THE NUMBER OF ENTRIES REMAINING.  THE COUNT IS                  
*       DIVIDED BY 2 TO FACILITATE.  THE INPBOOK TABLE MUST ALWAYS              
*       BE SET THIS WAY.                                                        
*                                                                               
         SRL   RF,1                DIVIDE NUMBER OF BOOKS BY 2                  
*                                     PROCESSED                                 
         LR    R4,RF               SAVE LOOP CTL: EST/PROJ BOOK COUNT           
         LR    R3,R2               SAVE A(1ST ESTIMATE BOOK)                    
SPBK0080 EQU   *                                                                
         STC   RE,DINPBKYR(R2)     INSERT YEAR INTO ESTIMATE BOOK               
         STC   RE,DINPBKYR+LINPBKS(R2)                                          
*                                  INSERT SAME YEAR INTO PROJECTION             
*                                     BOOK IN LIST                              
         CLI   DINPBKMO(R2),11     BOOK = NOVEMBER?                             
         BNE   SPBK0100            NO  - BUMP TO NEXT BOOK                      
         LA    RE,1(RE)            YES - BUMP YEAR BY 1                         
SPBK0100 EQU   *                                                                
         LA    R2,LINPBKS(R2)      BUMP TO NEXT PROJECTION BOOK                 
         LA    R2,LINPBKS(R2)      BUMP TO NEXT ESTIMATE BOOK                   
         BCT   RF,SPBK0080         GO BACK FOR NEXT                             
*                                                                               
*   WHEN ALL BOOKS IN LIST ARE PROCESSED, GO BACK AND ELIMINATE                 
*        ESTIMATE BOOKS PRE CURRENT SWEEP PERIOD                                
*                                                                               
         LR    R2,R3               SET A(1ST ESTIMATE BOOK)                     
         LR    R3,R4               RESET # ESTIMATE BOOKS                       
*                                                                               
         MVC   WORK+10(1),WORK+6   INSERT CURRENT YEAR                          
         LA    RE,BOOKTABL         SET A(TABLE OF DATES/BOOKS)                  
SPBK0120 EQU   *                                                                
         CLI   0(RE),0             END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                MONTH NOT IN TABLE?                          
*                                                                               
         CLC   0(1,RE),WORK+7      TABLE = CURRENT MONTH?                       
         BE    SPBK0140            YES - TAKE CUTOFF DATE FROM TABLE            
         LA    RE,LBOOKTAB(RE)     NO  - BUMP TO NEXT ENTRY                     
         B     SPBK0120            GO BACK FOR NEXT                             
SPBK0140 EQU   *                                                                
         MVC   WORK+11(1),1(RE)    INSERT CUTOFF MONTH INTO BOOK                
         CLI   QUESTOR+1,C'?'      OPTIONAL PRINTOUT?                           
         BNE   SPBK0160            NO                                           
         MVC   P+1(16),=C'ESTIMATE CUTOFF='                                     
         GOTO1 HEXOUT,DMCB,WORK+10,P+17,2,=C'TOG'                               
         GOTO1 REPORT                                                           
SPBK0160 EQU   *                                                                
         CLI   0(R2),X'60'         ESTIMATE BOOK?                               
         BE    SPBK0170            YES - PROCESS                                
         CLI   0(R2),X'44'         PROJECTION BOOK?                             
         BE    SPBK0170            YES - PROCESS                                
         DC    H'0'                SHOULD ONLY BE ESTIMATE BOOKS HERE           
SPBK0170 EQU   *                                                                
         CLC   DINPBKYR(2,R2),WORK+10                                           
*                                  BOOK DATE PRE CUTOFF DATE?                   
         BNL   SPBK0180            NO  - KEEP ESTIMATE BOOK IN LIST             
         MVI   DINPBKYR(R2),X'99'  YES - CHANGE TO UNRETRIEVABLE DATA           
SPBK0180 EQU   *                                                                
         LA    R2,LINPBKS(R2)      BUMP TO NEXT BOOK                            
         BCT   R3,SPBK0160         GO BACK FOR NEXT                             
SPBK0200 EQU   *                                                                
*                                                                               
*   THIS TABLE CANNOT BE DISPLAYED ALL THE TIME.  IT FALLS BEFORE               
*        THE EDICT HEADER.  IT IS THEREFORE OPTIONAL FOR TESTING.               
*                                                                               
         CLI   QUESTOR+1,C'?'      DISPLAY TABLE?                               
         BNE   SPBK0800            NO                                           
*                                                                               
*   TEST DISPLAY                                                                
         ZIC   R3,INP#BKS                                                       
         LA    R2,INPBOOKS                                                      
TINP0020 EQU   *                                                                
         MVC   P+1(08),=C'INPBOOK:'                                             
         GOTO1 HEXOUT,DMCB,0(R2),P+12,6,=C'TOG'                                 
***      MVC   P+12(LINPBKS),0(R2)                                              
         GOTO1 REPORT                                                           
         LA    R2,LINPBKS(R2)                                                   
         BCT   R3,TINP0020         GO BACK FOR NEXT                             
*                                                                               
*   THIS DUMP WILL KILL THE JOB AFTER DISPLAYING THE TABLE                      
*                                                                               
         DC    H'0'                                                             
*                                                                               
SPBK0800 EQU   *                                                                
         XIT1                                                                   
*                                                                               
*   BOOKTABL:  TWO BYTES PER ENTRY:                                             
*        BYTE 1  =  CURRENT MONTH VALUE                                         
*        BYTE 2  =  CUTOFF MONTH (DROP ENTRY PRIOR TO THIS MONTH)               
*                                                                               
BOOKTABL DC    X'0100'                                                          
LBOOKTAB EQU   *-BOOKTABL                                                       
         DC    X'0200'                                                          
         DC    X'0300'                                                          
         DC    X'0400'             00 = DON'T DROP ANY BOOKS                    
         DC    X'0505'                                                          
         DC    X'0605'             05 = DROP THROUGH APRIL                      
         DC    X'0707'                                                          
         DC    X'0807'                                                          
         DC    X'0907'                                                          
         DC    X'0A07'             07 = DROP THROUGH JUNE                       
         DC    X'0B0B'                                                          
         DC    X'0C0B'             0B = DROP THROUGH OCTOBER                    
         DC    X'0000'                                                          
         EJECT                                                                  
***>>>   GETPROF INSERT                                                         
***********************************************************************         
* GETPROF - GET THE PROGRAM PROFILES                                            
*   INPUT:   P1  BYTE 1      PROGRAM #                                          
*                BYTE 2-4    A(PROFILE AREA) CL10                               
*                                                                               
*   OUPUT:   PROFILES IN PROFILE AREA                                           
*                                                                               
***********************************************************************         
*GETPROF  NTR1                                                                  
*         ZIC   R3,0(R1)                                                        
*         L     R2,0(R1)                                                        
*         XC    0(10,R2),0(R2)                                                  
*                                                                               
*K        USING RREPKEY,KEY         GET PARENT REP CODE                         
*         XC    K.RREPKEY,K.RREPKEY                                             
*         MVI   K.RREPKTYP,X'01'                                                
*         MVC   K.RREPKREP,RCREPFL                                              
*        DROP  K                                                                
*                                                                               
*        GOTO1 HIGH                                                             
*                                                                               
*        CLC   KEY(L'RREPKEY),KEYSAVE                                           
*        BE    *+6                                                              
*        DC    H'0'                NO REP RECORD                                
*                                                                               
*        GOTO1 GETREC,AIOREC                                                    
*                                                                               
*        L     R6,AIOREC                                                        
*        LA    R6,RREPELEM-RREPREC(R6)                                          
*GPROF02  CLI   0(R6),0                                                         
*         BE    GETPROFX            NO PROFILE ELEMENT                          
*         CLI   0(R6),X'04'                                                     
*         BE    GPROF04                                                         
*         ZIC   R0,1(R6)                                                        
*         AR    R6,R0                                                           
*         B     GPROF02                                                         
*                                                                               
*GPROF04  DS    0H                                                              
*         USING RREPPGMP,R6                                                     
*        ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
*         LA    R6,RREPPGM1                                                     
*         USING RREPPGM1,R6                                                     
*GPROF10  CLM   R3,1,RREPPGM1       CORRECT PROGRAM?                            
*         BE    GPROF20             YES                                         
*         LA    R6,RREPPGML(R6)                                                 
*         BCT   RF,GPROF10                                                      
*         B     GETPROFX            NOT FOUND. USE DEFAULTS.                    
*                                                                               
*GPROF20  MVC   0(10,R2),RREPPGM1   SAVE PROGRAM PROFILES UNIT                  
*         DROP  R6                                                              
*GETPROFX  EQU   *                                                              
*         XIT1                                                                  
***>>>   GETPROF INSERT                                                         
         EJECT                                                                  
*                                                                               
*   SAVE CODE AREA                                                              
*                                                                               
**       GOTO1 HIGH                                                             
**       GOTO1 SEQ                                                              
**POST     NTR1                                                                 
*                                                                               
         EJECT                                                                  
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         XIT1                                                                   
         EJECT                                                                  
GETCON   LA    RF,RCONREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETDYPT  L     RF,AIOREC                                                        
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
*                                                                               
*                                                                               
*   INPUT BLOCK:  THIS IS THE AREA THAT MUST BE BUILT TO                        
*        DRIVE THE INDIVIDUAL REQUEST                                           
*                                                                               
INPBLOCK DC    X'77'               NUMSTAS:   119                               
         DC    CL05'KABE '                                                      
         DC    CL05'KAKW '                                                      
         DC    CL05'KBTF '                                                      
         DC    CL05'KBVO '                                                      
         DC    CL05'KDTV '                                                      
         DC    CL05'KFPH1'         SATELLITE                                    
         DC    CL05'KFSF '                                                      
         DC    CL05'KFTH '                                                      
         DC    CL05'KFTR '                                                      
         DC    CL05'KFTU '                                                      
         DC    CL05'KFTV '                                                      
         DC    CL05'KKFQ '                                                      
         DC    CL05'KMEX '                                                      
         DC    CL05'KNIC '                                                      
         DC    CL05'KSTR '                                                      
         DC    CL05'KTFF '                                                      
         DC    CL05'KTFK '                                                      
         DC    CL05'KTVW '                                                      
         DC    CL05'KUNB '                                                      
         DC    CL05'KUNP '                                                      
         DC    CL05'KUNS '                                                      
         DC    CL05'KUNU '                                                      
         DC    CL05'KUVE '                                                      
         DC    CL05'KUVN '                                                      
         DC    CL05'KUVS '                                                      
         DC    CL05'KWEX '                                                      
         DC    CL05'KXLN '                                                      
         DC    CL05'WAMI '                                                      
         DC    CL05'WFPA '                                                      
         DC    CL05'WFUT1'         SATELLITE                                    
         DC    CL05'WGBO '                                                      
         DC    CL05'WJRB '                                                      
         DC    CL05'WLII '                                                      
         DC    CL05'WLTV '                                                      
         DC    CL05'WQHS '                                                      
         DC    CL05'WTNC '                                                      
         DC    CL05'WUVC '                                                      
         DC    CL05'WUVG '                                                      
         DC    CL05'WUVP '                                                      
         DC    CL05'WXFT '                                                      
         DC    CL05'WXTV '         END OF SHEET 1                               
         DC    CL05'KAJB '                                                      
         DC    CL05'KAMT '                                                      
         DC    CL05'KANG '                                                      
         DC    CL05'KBNT '                                                      
         DC    CL05'KBZO '                                                      
         DC    CL05'KCEC '                                                      
         DC    CL05'KCRP '                                                      
         DC    CL05'KDJT '                                                      
         DC    CL05'KDTF '                                                      
         DC    CL05'KELV '                                                      
         DC    CL05'KETF '                                                      
         DC    CL05'KEUS '                                                      
         DC    CL05'KEUV '                                                      
         DC    CL05'KEVC '                                                      
         DC    CL05'KEYU '                                                      
         DC    CL05'KGHB '                                                      
         DC    CL05'KINC '                                                      
         DC    CL05'KINT '                                                      
         DC    CL05'KLDO '                                                      
         DC    CL05'KLRA '                                                      
         DC    CL05'KLUZ '                                                      
         DC    CL05'KNVO '                                                      
         DC    CL05'KNVV '                                                      
         DC    CL05'KORO '                                                      
         DC    CL05'KOXO '                                                      
         DC    CL05'KPMR '                                                      
         DC    CL05'KPPP '                                                      
         DC    CL05'KSMS '                                                      
         DC    CL05'KTFD '                                                      
         DC    CL05'KTFN '                                                      
         DC    CL05'KTFQ '                                                      
         DC    CL05'KTSB '                                                      
         DC    CL05'KUCO '                                                      
         DC    CL05'KUKC '                                                      
         DC    CL05'KUOK '                                                      
         DC    CL05'KUPB '                                                      
         DC    CL05'KUTF '                                                      
         DC    CL05'KUTH '                                                      
         DC    CL05'KUTU '                                                      
         DC    CL05'KUWF '                                                      
         DC    CL05'KVER '                                                      
         DC    CL05'KVTF '                                                      
         DC    CL05'KVYE '                                                      
         DC    CL05'KWKO '                                                      
         DC    CL05'KXUN '                                                      
         DC    CL05'WFDC '                                                      
         DC    CL05'WFTT '                                                      
         DC    CL05'WIIH '                                                      
         DC    CL05'WJMF '                                                      
         DC    CL05'WLLC '                                                      
         DC    CL05'WLZE '                                                      
         DC    CL05'WMDO '                                                      
         DC    CL05'WNYI '                                                      
         DC    CL05'WOTF '                                                      
         DC    CL05'WUDT '                                                      
         DC    CL05'WUMN '                                                      
         DC    CL05'WUNI '                                                      
         DC    CL05'WUTF '                                                      
         DC    CL05'WUTH '                                                      
         DC    CL05'WUVF '                                                      
         DC    CL05'WUVN '                                                      
         DC    CL05'WVEA '                                                      
         DC    CL05'WVEN '                                                      
         DC    CL05'KEYU1'                                                      
         DC    CL05'KAKW1'                                                      
         DC    CL05'WUVF1'                                                      
         DC    CL05'KXUN1'                                                      
         DC    CL05'KUOK1'                                                      
         DC    CL05'KUNP1'                                                      
         DC    CL05'KUVE1'                                                      
         DC    CL05'KWKO1'                                                      
         DC    CL05'KBVO1'                                                      
         DC    CL05'KBTF1'                                                      
         DC    CL05'KTFF1'                                                      
         DC    CL05'KVTF1'                                                      
         DC    CL05'KNIC1'                                                      
         DC    CL05'KTSB1'                                                      
         DC    CL05'KFTU1'         END OF SHEET 2                               
*********************************************************************           
         DC    X'0F'                  NUMDPTS                                   
         DC    CL15'CDE1AP2LKWSXN3O'  DAYPARTS:  15                             
**       DC    X'01'               NUMDPTS                                      
**       DC    CL1'E'              DAYPARTS:  1                                 
**       DC    X'02'               NUMDPTS                                      
**       DC    CL2'MN'             DAYPARTS:  2                                 
*********************************************************************           
         DC    X'01'               NUMFLTS                                      
         DC    XL06'101001105001'                                               
*                                  FORMAT:  JULIAN                              
*                                  CENTURY                                      
*                                  YEAR                                         
*                                  JULIAN DAY WITHIN YEAR                       
*                                  FLIGHTS:   2                                 
*********************************************************************           
INP#BKS  EQU   *                                                                
         DC    X'3F'               NUMBKS: 63                                   
INPBOOKS EQU   *                                                                
         DC    XL6'406201C90000'                                                
LINPBKS  EQU   *-INPBOOKS                                                       
DINPBKYR EQU   1                   DISP(YR IN BOOK LIST)                        
DINPBKMO EQU   2                   DISP(MO IN BOOK LIST)                        
*                                                                               
         DC    XL6'406202C90000'                                                
         DC    XL6'406203C90000'                                                
         DC    XL6'406205C90000'                                                
         DC    XL6'406207C90000'                                                
         DC    XL6'40620AC90000'                                                
         DC    XL6'40620BC90000'                                                
         DC    XL6'406301C90000'                                                
         DC    XL6'406302C90000'                                                
         DC    XL6'406303C90000'                                                
         DC    XL6'406305C90000'                                                
         DC    XL6'406307C90000'                                                
         DC    XL6'40630AC90000'                                                
         DC    XL6'40630BC90000'                                                
         DC    XL6'406401C90000'                                                
         DC    XL6'406402C90000'                                                
         DC    XL6'406403C90000'                                                
         DC    XL6'406405C90000'                                                
         DC    XL6'406407C90000'                                                
         DC    XL6'40640AC90000'                                                
         DC    XL6'40640BC90000'                                                
         DC    XL6'406501C90000'                                                
         DC    XL6'406502C90000'                                                
         DC    XL6'406503C90000'                                                
         DC    XL6'406505C90000'                                                
         DC    XL6'406507C90000'                                                
         DC    XL6'40650AC90000'                                                
         DC    XL6'40650BC90000'                                                
         DC    XL6'406601C90000'                                                
         DC    XL6'406602C90000'                                                
         DC    XL6'406603C90000'                                                
         DC    XL6'406605C90000'                                                
         DC    XL6'406607C90000'                                                
         DC    XL6'40660AC90000'                                                
         DC    XL6'40660BC90000'                                                
         DC    XL6'606601C90000'                                                
         DC    XL6'446601C90000'                                                
         DC    XL6'606602C90000'                                                
         DC    XL6'446602C90000'                                                
         DC    XL6'606603C90000'                                                
         DC    XL6'446603C90000'                                                
         DC    XL6'606605C90000'                                                
         DC    XL6'446605C90000'                                                
         DC    XL6'606607C90000'                                                
         DC    XL6'446607C90000'                                                
         DC    XL6'60660AC90000'                                                
         DC    XL6'44660AC90000'                                                
         DC    XL6'60660BC90000'                                                
         DC    XL6'44660BC90000'                                                
         DC    XL6'606701C90000'                                                
         DC    XL6'446701C90000'                                                
         DC    XL6'606702C90000'                                                
         DC    XL6'446702C90000'                                                
         DC    XL6'606703C90000'                                                
         DC    XL6'446703C90000'                                                
         DC    XL6'606705C90000'                                                
         DC    XL6'446705C90000'                                                
         DC    XL6'606707C90000'                                                
         DC    XL6'446707C90000'                                                
         DC    XL6'60670AC90000'                                                
         DC    XL6'44670AC90000'                                                
         DC    XL6'60670BC90000'                                                
         DC    XL6'44670BC90000'                                                
*                                  FORMAT:                                      
*                                  SERVICE:  X'40' = NSI                        
*                                  SERVICE:  X'20' = ESTIMATE                   
*                                  SERVICE:  X'04' = PROJECTION                 
*                                  YEAR:     X'65' = 2001                       
*                                  MONTH:    X'0B' = NOVEMBER                   
*                                  SOURCE:      I  = INVENTORY                  
*                                               T  = TIME PERIOD                
*                                               4  = 4-WEEK AVG                 
*                                               P  = PAV                        
*                                  SURVEY:      O  = OLYMPIC                    
*                                               H  = HISPANIC                   
*                                               ETC                             
*                                  ONE BYTE  X'00'  = FLAGS                     
*********************************************************************           
         DC    X'00'               NUMUPGS                                      
*        DC    XL26'  '            NUMUPGS:   0                                 
*********************************************************************           
         DC    AL2(NUMDEMQ)        NUMDEMS:   IMPRESSIONS & RATINGS             
INPDEMS  DC    XL(NUMDEMQ)'00'     HOMES:                                       
         DC    XL(NUMDEMQ)'00'     FORMAT:                                      
         DC    XL(NUMDEMQ)'00'     BYTE 1:    QUALIFIER                         
         DC    XL(NUMDEMQ)'00'     BYTE 2:    MODIFIER (P=PUTS, ETC)            
*                                  BYTE 3:    DEMO CODE (BINARY)                
*                                  BYTE 4:    FLAGS                             
*********************************************************************           
         DC    X'00'               NUMRCDS                                      
*        DC    XL10'  '            NUMRCDS:   0                                 
*********************************************************************           
         DC    X'0000'                                                          
*********************************************************************           
TSTBLOCK DC    X'01'               NUMSTAS:   1                                 
TSTSTA   DC    CL05'KNSD '         STATION :  1                                 
*********************************************************************           
**       DC    X'01'               NUMDPTS                                      
**       DC    CL1'Z'              DAYPARTS:  1                                 
         DC    X'01'               NUMDPTS                                      
         DC    CL1'E'              DAYPARTS:  1                                 
**       DC    X'02'               NUMDPTS                                      
**       DC    CL2'MN'             DAYPARTS:  2                                 
*********************************************************************           
         DC    X'01'               NUMFLTS                                      
         DC    XL06'101001105001'                                               
*                                  FORMAT:  JULIAN                              
*                                  CENTURY                                      
*                                  YEAR                                         
*                                  JULIAN DAY WITHIN YEAR                       
*                                  FLIGHTS:   2                                 
*********************************************************************           
         DC    X'0C'               NUMBKS: 12                                   
         DC    XL6'406602C90000'                                                
         DC    XL6'406605C90000'                                                
         DC    XL6'406607C90000'                                                
         DC    XL6'40660BC90000'                                                
         DC    XL6'606602C90000'                                                
         DC    XL6'446602C90000'                                                
         DC    XL6'606605C90000'                                                
         DC    XL6'446605C90000'                                                
         DC    XL6'606607C90000'                                                
         DC    XL6'446607C90000'                                                
         DC    XL6'60660BC90000'                                                
         DC    XL6'44660BC90000'                                                
*                                  FORMAT:                                      
*                                  SERVICE:  X'40' = NSI                        
*                                  SERVICE:  X'20' = ESTIMATE                   
*                                  SERVICE:  X'04' = PROJECTION                 
*                                  YEAR:     X'65' = 2001                       
*                                  MONTH:    X'0B' = NOVEMBER                   
*                                  SOURCE:      I  = INVENTORY                  
*                                               T  = TIME PERIOD                
*                                               4  = 4-WEEK AVG                 
*                                               P  = PAV                        
*                                  SURVEY:      O  = OLYMPIC                    
*                                               H  = HISPANIC                   
*                                               ETC                             
*                                  ONE BYTE  X'00'  = FLAGS                     
*********************************************************************           
         DC    X'00'               NUMUPGS                                      
*        DC    XL26'  '            NUMUPGS:   0                                 
*********************************************************************           
         DC    AL2(NUMDEMQ)        NUMDEMS:   IMPRESSIONS & RATINGS             
TSTDEMS  DC    XL(NUMDEMQ)'00'     HOMES:                                       
         DC    XL(NUMDEMQ)'00'     FORMAT:                                      
         DC    XL(NUMDEMQ)'00'     BYTE 1:    QUALIFIER                         
         DC    XL(NUMDEMQ)'00'     BYTE 2:    MODIFIER (P=PUTS, ETC)            
*                                  BYTE 3:    DEMO CODE (BINARY)                
*                                  BYTE 4:    FLAGS                             
*********************************************************************           
         DC    X'00'               NUMRCDS                                      
*        DC    XL10'  '            NUMRCDS:   0                                 
*********************************************************************           
         DC    X'0000'                                                          
*********************************************************************           
         EJECT                                                                  
XCOMFACS DS    A                                                                
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
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
*                                                                               
INVSEQ   DS    PL5                                                              
DEMOCTR  DS    F                   MAX IS 44 DEMOS                              
PUTCTR   DS    F                   OUTPUT RECORD COUNTER                        
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
FETCHBLK DS    CL(RFTBLKL)         FETCH BLOCK                                  
*                                                                               
XDMLIST  DS    CL8                                                              
DEMLIST  DS    CL((NUMDEMQ+1)*4)                                                
XDMSAVE  DS    CL8                                                              
DEMSAVE  DS    CL((NUMDEMQ+1)*4)                                                
XDMRTG   DS    CL8                                                              
DEMORTG  DS    CL((NUMDEMQ+1)*4)                                                
XDMSHR   DS    CL8                                                              
DEMOSHR  DS    CL((NUMDEMQ+1)*4)                                                
XDMLVL   DS    CL8                                                              
DEMOLVL  DS    CL((NUMDEMQ+1)*4)                                                
XDMXX    DS    CL8                                                              
DEMOEND  DS    0H                                                               
*                                                                               
FETCHWRK DS    XL6144                                                           
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
       ++INCLUDE RESELPROF                                                      
       ++INCLUDE REFETCHD                                                       
HD01EQUS EQU   0                                                                
HD01CTL  EQU   0                                                                
HD01STA  EQU   4                                                                
HD01INV  EQU   9                                                                
HD01STDT EQU   13                                                               
HD01ENDT EQU   21                                                               
HD01DYPT EQU   29                                                               
HD01DAYS EQU   37                                                               
HD01TIME EQU   44                                                               
*                                                                               
HD02EQUS EQU   0                                                                
HD02CTL  EQU   0                                                                
HD02AVDY EQU   4                                                                
HD02AVTM EQU   15                                                               
HD02PGNM EQU   26                                                               
*                                                                               
DM01EQUS EQU   0                                                                
DM01CTL  EQU   0                                                                
DM01BOOK EQU   4                                                                
DM01DRSH EQU   12                  D(1ST SET OF RATING/SHARE/HUT)               
DM01DRTG EQU   0                   D(RATING IN SET) - 8 CHAR                    
DM01DSHR EQU   8                   D(SHARE  IN SET) - 4 CHAR                    
DM01DHUT EQU   12                  D(HUT    IN SET) - 8 CHAR                    
LDM01RSH EQU   8+4+8                                                            
*                                                                               
BINFEQUS EQU   0                                                                
BINFCTL  EQU   0                                                                
BINFBOOK EQU   4                   BOOK                                         
BINFFOOT EQU   12                  FOOTNOTE                                     
BINFUPCM EQU   44                  UPGRADE COMMENT                              
*                                                                               
BPJFEQUS EQU   0                                                                
BPJFCTL  EQU   0                                                                
BPJFBOOK EQU   4                   BOOK                                         
BPJFCOM  EQU   12                  FOOTNOTE                                     
*                                                                               
DYPTEQUS EQU   0                                                                
DYPTCTL  EQU   0                                                                
DYPTDPT  EQU   4                   DAYPART CODE                                 
DYPTDPT3 EQU   5                   DAYPART SHORT NAME                           
DYPTDPTL EQU   8                   DAYPART LONG NAME                            
*                                                                               
DX01EQUS EQU   0                                                                
DX01CTL  EQU   0                                                                
DX01DMO1 EQU   4                   1ST DEMO CATEGORY                            
DX01DMOL EQU   8                   DEMO LENGTH                                  
*                                                                               
ITXTEQUS EQU   0                                                                
ITXTCTL  EQU   0                                                                
ITXTNUM  EQU   4                   INVENTORY TEXT NUMBER                        
ITXTSUB  EQU   3                   SUB RECORD ID                                
ITXTWRAP EQU   8                   WRAPPING FLAG                                
ITXTTEXT EQU   9                   TEXT DATA                                    
*                                                                               
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
*                                                                               
       ++INCLUDE REGENRDP                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
* TF  SSB                                                             *         
***********************************************************************         
**       SPACE 1                                                                
**       DS    0L                                                               
**       DC    CL16'SSB*SSB*SSB*SSB*'                                           
*SSB      CSECT                                                                 
**         DC    H'0'                                                           
**         DC    X'FF'               OFFLINE EXTENDED                           
**         DC    XL256'00'                                                      
***********************************************************************         
***********************************************************************         
* HOOK FOR THE FETCH ROUTINE TO ADD NEW DETAIL CLUSTERS                         
***********************************************************************         
RE3P02   CSECT                                                                  
FTCHHOOK NTR1  BASE=*,LABEL=*                                                   
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
*                                                                               
*   TEST                                                                        
***      MVC   P+1(19),=C'FETCH HOOK: ERROR ='                                  
***      MVC   P+20(1),RFTERR                                                   
***      GOTO1 REPORT                                                           
***      L     RF,=F'1496'                                                      
***      GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
***      GOTO1 REPORT                                                           
*   TEST:  END                                                                  
*                                                                               
         OC    RFTERR,RFTERR                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RFTMODE,RFTNHDRQ                                                 
         BE    INVHDR                                                           
         CLI   RFTMODE,RFTNBKQ                                                  
         BE    INVBK                                                            
         CLI   RFTMODE,RFTNRTEQ                                                 
         BE    INVRCD                                                           
         CLI   RFTMODE,RFTNTXTQ                                                 
         BE    INVTXT                                                           
         B     FTCHEXIT                                                         
         EJECT                                                                  
*===============================*                                               
* PROCESS INVENTORY HEADER HOOK *                                               
*===============================*                                               
INVHDR   DS    0H                                                               
         XC    DEMOCTR,DEMOCTR     CLEAR DEMO INPUT COUNTER                     
         XC    DEMHDCTR,DEMHDCTR   CLEAR DEMO CONTROL COUNTER                   
         XC    DEMOLOOP,DEMOLOOP   CLEAR DEMO CONTROL                           
         XC    AVORIDES(200),AVORIDES   CLEAR AVAIL OVERRIDES                   
         XC    AVORIDES+200(200),AVORIDES+200                                   
*JRD                                                                            
*   TEST                                                                        
*        MVC   P+1(19),=C'INVHDR:            '                                  
*        GOTO1 REPORT                                                           
*   TEST:  END                                                                  
*                                                                               
         OC    RFTFPGMS,RFTFPGMS                                                
         BZ    IHDRNO              NOT GOOD!!!                                  
*                                                                               
         CLI   RMODE,C'D'          NEWDATA CALL?                                
         BE    IHDR0020            YES - SKIP FLIGHT CHECK                      
*                                                                               
         XC    WORK,WORK           CONVERT DATES                                
         GOTO1 DATCON,DMCB,(2,RFTFEFST),(19,WORK)                               
         OC    RFTFEFEN,RFTFEFEN                                                
         BZ    IHDR0002                                                         
         GOTO1 DATCON,DMCB,(2,RFTFEFEN),(19,WORK+3)                             
*                                                                               
IHDR0002 DS    0H                                                               
         CLI   NUMFLTS,1           SINGLE FLIGHT?                               
         BE    IHDR0020            YES- NO NEED TO CHECK FLIGHTS                
*                                                                               
         ZIC   RF,NUMFLTS          CHECK EFFECTIVE DATES AGAINST ALL            
         L     RE,FRSTFLT           REQUEST FLIGHTS                             
         A     RE,OVPARMS+4                                                     
IHDR0010 DS    0H                                                               
         CLC   WORK(3),3(RE)       INVENTORY STARTS AFTER FILTER END?           
         BH    IHDR0014            YES- FILTER FAILED                           
*                                                                               
         OC    WORK+3(3),WORK+3    OPEN ENDED INVENTORY?                        
         BZ    IHDR0020            YES                                          
         CLC   WORK+3(3),0(RE)     INVENTORY ENDS B4 FILTER START?              
         BL    IHDR0014            YES - FILTER FAILED                          
*                                                                               
         B     IHDR0020            FILTER PASSED                                
*                                                                               
IHDR0014 DS    0H                                                               
         LA    RE,FLTLENQ(RE)      CHECK NEXT FLIGHT                            
         BCT   RF,IHDR0010                                                      
         B     IHDRNO                                                           
*                                                                               
IHDR0020 DS    0H                  PROCESS HEADER                               
         TM    MISCFLG1,MF1TMPB1   NEED STATION HEADER?                         
         BNZ   IHDR0030            NO                                           
*                                  ADD NEW STATION ELEMENT                      
*        GOTO1 SETELEM,DMCB,ISTDATA                                             
*                                  STATION CALL LETTERS                         
         MVC   WORK+20(5),RFTCSTAT                                              
         CLI   WORK+24,C'T'                                                     
         BNE   *+8                                                              
         MVI   WORK+24,C' '                                                     
*                                                                               
***      LA    R1,WORK+20          SWITCH HISPANIC CALL LETTERS BACK            
***      BAS   RE,SWHISP                                                        
*                                                                               
         GOTO1 ADDDATA,DMCB,WORK+20,0,0                                         
*                                                                               
         OI    MISCFLG1,MF1TMPB1   SET STATION INDICATED                        
IHDR0030 DS    0H                                                               
         CLI   RMODE,C'D'          NEWDATA CALL?                                
         BE    IHDRYES             YES                                          
*                                                                               
IHDR0040 DS    0H                                                               
*                                  ADD NEW INVENTORY ELEMENT                    
*        GOTO1 SETELEM,DMCB,INVDATA                                             
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
*                                   INVENTORY NUMBER                            
         GOTO1 ADDDATA,DMCB,RFTFINV,0,1                                         
*                                   EFFECTIVE START DATE                        
         GOTO1 ADDDATA,DMCB,WORK,0,2                                            
*                                   EFFECTIVE END DATE                          
         OC    RFTFEFEN,RFTFEFEN                                                
         BZ    IHDR0044                                                         
         GOTO1 ADDDATA,DMCB,WORK+3,0,3                                          
*                                                                               
IHDR0044 DS    0H                                                               
         SPACE 2                                                                
*----------------*                                                              
* HEADER DAYPART *                                                              
*----------------*                                                              
         LA    R0,RFTFDPTS         SET A(FIRST DAYPART IN LIST)                 
*                                                                               
IHDDP020 DS    0H                                                               
         GOTO1 ADDDATA,DMCB,(R0),0,4                                            
         SPACE 2                                                                
*------------------*                                                            
* HEADER DAY/TIMES *                                                            
*------------------*                                                            
*                                  DAYS                                         
         GOTO1 ADDDATA,DMCB,RFTFDTDY,0,5                                        
*                                  START TIME                                   
         GOTO1 ADDDATA,DMCB,RFTFDTST,0,6                                        
*                                  END TIME                                     
         LA    R0,RFTFDTEN                                                      
         CLC   RFTFDTEN,=C'CC'                                                  
         BNE   *+8                                                              
         LA    R0,=X'00C8'                                                      
*                                                                               
         GOTO1 ADDDATA,DMCB,(R0),0,7                                            
         SPACE 2                                                                
*-----------------------------------*                                           
* HEADER OVERRIDE (AVAIL) DAY/TIMES *                                           
*-----------------------------------*                                           
         LA    RF,AVORIDES         SET A(WORKSPACE FOR O'RIDES)                 
         ST    RF,ADDRAVOR         SAVE A(NEXT SLOT)                            
*                                                                               
         LA    R0,8                                                             
         LA    R6,RFTFAVLS                                                      
IHDAV010 DS    0H                                                               
         CLC   0(L'RFTFAVLS,R6),SPACES                                          
         BH    *+12                                                             
         LA    R6,(L'RFTFAVLS/2)(R6)                                            
         B     IHDAV020                                                         
*                                                                               
         OC    0(L'RFTFAVLS,R6),SPACES                                          
*                                  1/2 THE FIELD IS DAY                         
         LA    RF,(L'RFTFAVLS/2)-1(R6)                                          
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
*                                                                               
         SR    RF,R6                                                            
         LA    RF,1(RF)                                                         
         GOTO1 ADDDATA,DMCB,(R6),(RF),8                                         
*                                                                               
*                                  THE OTHER 1/2 IS TIME                        
         LA    R6,(L'RFTFAVLS/2)(R6)                                            
         LA    RF,(L'RFTFAVLS/2)-1(R6)                                          
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
*                                                                               
         SR    RF,R6                                                            
         LA    RF,1(RF)                                                         
         GOTO1 ADDDATA,DMCB,(R6),(RF),9                                         
*                                                                               
IHDAV020 DS    0H                                                               
         LA    R6,(L'RFTFAVLS/2)(R6)                                            
         L     RF,ADDRAVOR         BUMP A(NEXT AVORIDE SLOT)                    
         LA    RF,LAVORIDE(RF)                                                  
         ST    RF,ADDRAVOR                                                      
*                                                                               
         BCT   R0,IHDAV010                                                      
         EJECT                                                                  
*----------------------*                                                        
* HEADER PROGRAM NAMES *                                                        
*----------------------*                                                        
         LA    RF,AVORIDES         SET A(WORKSPACE FOR O'RIDES)                 
         ST    RF,ADDRAVOR         SAVE A(NEXT SLOT)                            
         LA    R0,8                                                             
         LA    R6,RFTFPGMS                                                      
IHDPR010 DS    0H                                                               
         CLC   0(L'RFTFPGMS,R6),SPACES                                          
         BNH   IHDPR020                                                         
*                                                                               
         LA    RF,L'RFTFPGMS-1(R6)                                              
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
*                                                                               
         SR    RF,R6                                                            
         LA    RF,1(RF)                                                         
         OC    0(L'RFTFPGMS,R6),SPACES                                          
         GOTO1 ADDDATA,DMCB,(R6),(RF),10                                        
*                                                                               
IHDPR020 DS    0H                                                               
         LA    R6,L'RFTFPGMS(R6)                                                
*                                                                               
         L     RF,ADDRAVOR         BUMP A(NEXT AVORIDE SLOT)                    
         LA    RF,LAVORIDE(RF)                                                  
         ST    RF,ADDRAVOR                                                      
*                                                                               
         BCT   R0,IHDPR010                                                      
*                                                                               
IHDRYES  DS    0H                                                               
         GOTO1 ADDDATA,DMCB,(R6),(RF),36                                        
         B     FTCHEXIT                                                         
*                                                                               
IHDRNO   DS    0H                                                               
         MVI   RFTRETRN,INVREJ     SET FAILED APPLICATION FILTER                
         B     FTCHEXIT                                                         
         EJECT                                                                  
*=============================*                                                 
* PROCESS INVENTORY BOOK HOOK *                                                 
*=============================*                                                 
INVBK    DS    0H                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(19),=C'INVBK :            '                                  
**       GOTO1 REPORT                                                           
*   TEST:  END                                                                  
*                                                                               
         LHI   RE,NUMDEMQ*4                                                     
         LR    R1,R9                                                            
         AHI   R1,DEMORTG-OVERWRKD                                              
IBKCK010 DS    0H                                                               
         LHI   RF,254                                                           
         CHI   RE,255                                                           
         BH    *+6                                                              
         LR    RF,RE                                                            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,R1),0(R1)                                                    
         BNZ   IBK00001                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    DEMOSHR-DEMORTG(0,R1),DEMOSHR-DEMORTG(R1)                        
         BNZ   IBK00001                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    DEMOLVL-DEMORTG(0,R1),DEMOLVL-DEMORTG(R1)                        
         BNZ   IBK00001                                                         
*                                                                               
         AR    R1,RF               BUMP INDEX                                   
*                                                                               
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         CHI   RE,0                                                             
         BH    IBKCK010                                                         
         B     FTCHEXIT            NO INTERESTING DATA                          
*                                                                               
IBK00001 DS    0H                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(19),=C'IBK001:            '                                  
**       GOTO1 REPORT                                                           
*   TEST:  END                                                                  
*                                                                               
         MVI   NEWBOOK,C'Y'                                                     
*                                                                               
         OC    RFTFBK,RFTFBK       WAS IT A BOOK?                               
         BZ    INVUPG              NO - ITS AN UPGRADE                          
*                                                                               
         CLI   RMODE,C'D'          NEWDATA CALL?                                
         BNE   IBK00002            NO                                           
*                                                                               
         TM    MISCFLG1,MF1TMPB2   NEED INVENTORY HEADER?                       
         BNZ   IBK00002            NO                                           
*                                  ADD NEW INVENTORY ELEMENT                    
*        GOTO1 SETELEM,DMCB,INVDATA                                             
*                                  INVENTORY SEQUENCE NUMBER                    
         GOTO1 ADDDATA,DMCB,INVSEQ,0,11                                         
*                                                                               
         OI    MISCFLG1,MF1TMPB2   SET INVENTORY INDICATED                      
IBK00002 DS    0H                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
         ZIC   RF,NUMBKS           GET BOOK SEQUENCE NUMBER                     
         L     RE,FRSTBK                                                        
         A     RE,OVPARMS+4                                                     
IBK0010  DS    0H                                                               
         CLC   RFTFBK,0(RE)        BOOK MATCH?                                  
         BE    IBK0020             YES                                          
         LA    RE,BKLENQ(RE)                                                    
         BCT   RF,IBK0010                                                       
         DC    H'0'                UM - THIS CAN'T HAPPEN                       
*                                                                               
IBK0020  DS    0H                                                               
         TM    L'RFTFBK(RE),X'80'  OLD BOOK?                                    
         BNO   *+8                 NO                                           
         MVI   NEWBOOK,C'N'                                                     
*                                                                               
         ZIC   RE,NUMBKS                                                        
         SR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
*                                                                               
*                                  ADD NEW BOOK ELEMENT                         
*        GOTO1 SETELEM,DMCB,BKSDATA                                             
*                                  SET SEQUENCE NUMBER                          
         GOTO1 ADDDATA,DMCB,BYTE,0,12                                           
*                                                                               
         L     RE,RFTFTX1A         GET FOOTNOTE LENGTH                          
         OC    0(30,RE),SPACES                                                  
         LA    RE,30(RE)                                                        
IBK0030  CLI   0(RE),C' '                                                       
         BH    IBK0032                                                          
         BCTR  RE,0                                                             
         C     RE,RFTFTX1A                                                      
         BNL   IBK0030                                                          
         B     IBK0040             NO SIGNIFICANT FOOTNOTE                      
*                                                                               
IBK0032  DS    0H                                                               
         S     RE,RFTFTX1A                                                      
         LA    R0,1(RE)            SAVE FOOTNOTE LENGTH                         
*                                                                               
         L     RF,RFTFTX1A                                                      
         LA    RF,3(RF)            SKIP FLUFF FOR NOW                           
         SH    R0,=H'3'                                                         
         BNP   IBK0040                                                          
         GOTO1 ADDDATA,DMCB,(RF),(R0),14                                        
*                                                                               
IBK0040  DS    0H                                                               
         GOTO1 ADDDATA,DMCB,BYTE,0,13                                           
*                                                                               
*                                  PJ FORMULA                                   
         TM    RFTFBKVL,X'24'      CHECK IF P OR E BOOK                         
         BZ    IBK0060             IF NOT, SKIP UPGRADE COMMENT                 
*                                                                               
         LA    RF,RFTFUPGR         POINT RE AND RF                              
         LR    RE,RF                 AT UPGRADE COMMENT                         
         LA    RE,79(RE)           BUMP RE TO LAST CHAR OF CMMNT                
IBK0042  CLI   0(RE),C' '          FOUND SIGNIFICANT CHAR?                      
         BH    IBK0045              YES                                         
         BCTR  RE,0                 NO, DECREMENT PNTR                          
         CR    RE,RF                ARE WE BELOW 1ST CHAR?                      
         BNL   IBK0042                NO, REPEAT LOOP                           
         B     IBK0060                YES, NO SIGNIF UPGR CMMNT                 
*                                                                               
IBK0045  DS    0H                  SET LENGTH OF UPGR CMMNT                     
         SR    RE,RF                                                            
         LA    R0,1(RE)            R0 GETS UPGR-CMNT LENGTH                     
*                                                                               
         GOTO1 ADDDATA,DMCB,(RF),(R0),15                                        
*                                                                               
IBK0060  DS    0H                  DO DEMOS                                     
*                                  OUTPUT                                       
         B     INVDEM                                                           
*================================*                                              
* PROCESS INVENTORY UPGRADE HOOK *                                              
*================================*                                              
INVUPG   DS    0H                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(19),=C'INVUPG:            '                                  
**       GOTO1 REPORT                                                           
*   TEST:  END                                                                  
*                                                                               
         CLI   RMODE,C'D'          NEWDATA CALL?                                
         BNE   IUPG002             NO                                           
*                                                                               
         TM    MISCFLG1,MF1TMPB2   NEED INVENTORY HEADER?                       
         BNZ   IUPG002             NO                                           
*                                  ADD NEW INVENTORY ELEMENT                    
*        GOTO1 SETELEM,DMCB,INVDATA                                             
*                                  INVENTORY SEQUENCE NUMBER                    
         GOTO1 ADDDATA,DMCB,INVSEQ,0,16                                         
*                                                                               
         OI    MISCFLG1,MF1TMPB2   SET INVENTORY INDICATED                      
IUPG002  DS    0H                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
*        GOTO1 SETELEM,DMCB,BKSDATA                                             
*                                  SET SEQUENCE NUMBER                          
         SR    R0,R0                                                            
         L     R1,RFTFUPGA                                                      
         S     R1,RFTCUPGA         DISPLACEMENT TO UPGRADE                      
         LTR   R1,R1                                                            
         BZ    IUPG010                                                          
*                                                                               
         LA    RE,UPGLENQ-1                                                     
         DR    R0,RE                                                            
IUPG010  DS    0H                                                               
         LA    R0,1(R1)            1 BASED SEQUENCE NUMBER                      
         STC   R0,BYTE                                                          
*                                                                               
         MHI   R1,UPGLENQ          ADDRESS INPUT UPGRADE                        
         A     R1,FRSTUPG                                                       
         A     R1,OVPARMS+4                                                     
         TM    UPGLENQ-1(R1),X'80'  OLD BOOK?                                   
         BNO   *+8                  NO                                          
         MVI   NEWBOOK,C'N'                                                     
*                                                                               
         GOTO1 ADDDATA,DMCB,BYTE,0,17                                           
*                                                                               
IUPG040  DS    0H                                                               
         L     RE,RFTFTX1A         GET FOOTNOTE LENGTH                          
         OC    0(30,RE),SPACES                                                  
         LA    RE,30(RE)                                                        
IUPG050  CLI   0(RE),C' '                                                       
         BH    IUPG052                                                          
         BCTR  RE,0                                                             
         C     RE,RFTFTX1A                                                      
         BNL   IUPG050                                                          
         B     IUPG060             NO SIGNIFICANT FOOTNOTE                      
*                                                                               
IUPG052  DS    0H                                                               
         S     RE,RFTFTX1A                                                      
         LA    R0,1(RE)            SAVE FOOTNOTE LENGTH                         
*                                                                               
*&&DO                                                                           
         MVI   BYTE,3              SET FLUFF LENGTH                             
         GOTO1 ADDDATA,DMCB,BYTE,0,18                                           
*&&                                SET FOOTNOTE                                 
         L     RF,RFTFTX1A                                                      
         LA    RF,3(RF)            SKIP FLUFF FOR NOW                           
         SH    R0,=H'3'                                                         
         BNP   IUPG060                                                          
         GOTO1 ADDDATA,DMCB,(RF),(R0),19                                        
*                                                                               
IUPG060  DS    0H                  OUTPUT THEN DO DEMOS                         
         GOTO1 ADDDATA,DMCB,BYTE,0,13                                           
         B     INVDEM                                                           
         EJECT                                                                  
*========================*                                                      
* PROCESS INVENTORY DEMO *                                                      
*========================*                                                      
INVDEM   DS    0H                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(19),=C'INVDEM:            '                                  
**       GOTO1 REPORT                                                           
*   TEST:  END                                                                  
*                                                                               
         XC    DEMHDCTR,DEMHDCTR   CLEAR DEMO CONTROL COUNTER                   
*                                                                               
         LA    R5,1                DEMO SEQUENCE NUMBER                         
         LR    R2,R9                                                            
         AHI   R2,DEMORTG-OVERWRKD RATING VALUES                                
         LA    R3,DEMOSHR-DEMORTG(R2)                                           
         LA    R6,DEMOLVL-DEMOSHR(R3)                                           
*                                                                               
IDEM010  DS    0H                                                               
         CLI   NEWBOOK,C'Y'        NEW BOOK?                                    
         BE    IDEM022             YES - SHOW ALL DEMOS                         
*                                                                               
         LR    R1,R5               ADDRESS INPUT DEMO                           
         BCTR  R1,0                                                             
         MHI   R1,DEMLENQ                                                       
         A     R1,FRSTDEM                                                       
         A     R1,OVPARMS+4                                                     
         TM    3(R1),X'80'         OLD DEMO?                                    
         BO    IDEM050             YES - SKIP FOR OLD BOOK                      
*                                                                               
IDEM022  DS    0H                                                               
*                                  ADD NEW DEMO ELEMENT                         
*        GOTO1 SETELEM,DMCB,DEMDATA                                             
*                                                                               
         STC   R5,BYTE             SET SEQUENCE NUMBER                          
         GOTO1 ADDDATA,DMCB,BYTE,0,20                                           
*                                                                               
***      OC    0(4,R2),0(R2)       ANY RATING?                                  
***      BZ    IDEM030             NO                                           
         GOTO1 ADDDATA,DMCB,(R2),0,21                                           
*                                                                               
IDEM030  DS    0H                                                               
***      OC    0(4,R3),0(R3)       ANY SHARE?                                   
***      BZ    IDEM040             NO                                           
         GOTO1 ADDDATA,DMCB,(R3),0,22                                           
*                                                                               
IDEM040  DS    0H                                                               
***      OC    0(4,R6),0(R6)       ANY HUT/PUT LEVEL?                           
***      BZ    IDEM050             NO                                           
         GOTO1 ADDDATA,DMCB,(R6),0,23                                           
*                                                                               
IDEM050  DS    0H                                                               
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R6,4(R6)                                                         
*                                                                               
         LA    R5,1(R5)                                                         
         CLM   R5,3,NUMDEMS                                                     
         BNH   IDEM010                                                          
*                                                                               
         B     FTCHEXIT                                                         
*==================================*                                            
* PROCESS INVENTORY RATE CARD HOOK *                                            
*==================================*                                            
INVRCD   DS    0H                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(19),=C'INVRCD:            '                                  
**       GOTO1 REPORT                                                           
*   TEST:  END                                                                  
*                                                                               
         LA    R3,RFTFRDWK         CHECK FOR ANY NON ZERO RATE                  
R        USING RFTFRDWK,R3                                                      
         LA    RF,RFTFRDEN                                                      
         OC    R.RFTFRDRT,R.RFTFRDRT                                            
         BNZ   *+16                                                             
         LA    R3,RFTFRDSL(R3)                                                  
         BCT   RF,*-14                                                          
         B     INVRCDX                                                          
         DROP  R                                                                
*                                                                               
         CLI   RMODE,C'D'          NEWDATA CALL?                                
         BNE   IRCD002             NO                                           
*                                                                               
         TM    MISCFLG1,MF1TMPB2   NEED INVENTORY HEADER?                       
         BNZ   IRCD002             NO                                           
*                                  ADD NEW INVENTORY ELEMENT                    
*        GOTO1 SETELEM,DMCB,INVDATA                                             
*                                  INVENTORY SEQUENCE NUMBER                    
         GOTO1 ADDDATA,DMCB,INVSEQ,0,24                                         
*                                                                               
         OI    MISCFLG1,MF1TMPB2   SET INVENTORY INDICATED                      
IRCD002  DS    0H                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
*                                                                               
*        GOTO1 SETELEM,DMCB,RCDDATA                                             
*                                  SET SEQUENCE NUMBER                          
         SR    R0,R0                                                            
         ICM   R1,15,RFTFRDRC                                                   
         ICM   RF,15,RFTCRDRC                                                   
         SR    R1,RF                                                            
         LTR   R1,R1                                                            
         BZ    IRCD010                                                          
*                                                                               
         LA    RE,RCDLENQ                                                       
         DR    R0,RE                                                            
IRCD010  DS    0H                                                               
         LA    R0,1(R1)            1 BASED SEQUENCE NUMBER                      
         STC   R0,BYTE                                                          
*                                                                               
         GOTO1 ADDDATA,DMCB,BYTE,0,25                                           
*                                                                               
         GOTO1 ADDDATA,DMCB,RFTFRDWK,0,26                                       
*                                  SET 1ST WEEK DATE                            
         GOTO1 ADDDATA,DMCB,RFTFRDRT,0,27                                       
*                                  SET RATE                                     
         ICM   R2,15,RFTFRDRT                                                   
         LA    R3,RFTFRDWK                                                      
R        USING RFTFRDWK,R3                                                      
         LA    R5,RFTFRDEN-1                                                    
IRCD0020 DS    0H                                                               
         OC    RFTFRDSL(L'RFTFRDWK,R3),RFTFRDSL(R3)                             
         BZ    IRCD0040             NEXT WEEK IS OMMITTED. EXIT                 
*                                                                               
         LA    R3,RFTFRDSL(R3)                                                  
         CLM   R2,15,R.RFTFRDRT      NEW RATE?                                  
         BE    IRCD0030            NO                                           
*                                                                               
         ICM   R2,15,R.RFTFRDRT                                                 
*                                                                               
         GOTO1 ADDDATA,DMCB,R.RFTFRDWK,0,28                                     
*                                  SET 1ST WEEK DATE                            
         GOTO1 ADDDATA,DMCB,R.RFTFRDRT,0,29                                     
*                                  SET RATE                                     
IRCD0030 DS    0H                                                               
         BCT   R5,IRCD0020                                                      
*                                                                               
IRCD0040 DS    0H                                                               
         GOTO1 DATCON,DMCB,(8,R.RFTFRDWK),(0,WORK)                              
         GOTO1 ADDAY,DMCB,WORK,WORK,6                                           
         GOTO1 DATCON,DMCB,(0,WORK),(19,WORK+6)                                 
         GOTO1 ADDDATA,DMCB,WORK+6,0,30                                         
*                                  SET LAST WEEK DATE                           
         DROP  R                                                                
INVRCDX  DS    0H                                                               
         B     FTCHEXIT                                                         
         EJECT                                                                  
*=============================*                                                 
* PROCESS INVENTORY TEXT HOOK *                                                 
*=============================*                                                 
INVTXT   DS    0H                                                               
*                                                                               
*   TEST                                                                        
         MVC   P+1(19),=C'INVTXT:            '                                  
         GOTO1 REPORT                                                           
*   TEST:  END                                                                  
*                                                                               
         TM    MISCFLG1,MF1TMPB2   NEED INVENTORY HEADER?                       
         BNZ   ITXT0010            NO                                           
*                                  ADD NEW INVENTORY ELEMENT                    
*        GOTO1 SETELEM,DMCB,INVDATA                                             
*                                  INVENTORY SEQUENCE NUMBER                    
         GOTO1 ADDDATA,DMCB,INVSEQ,0,31                                         
*                                                                               
         OI    MISCFLG1,MF1TMPB2   SET INVENTORY INDICATED                      
ITXT0010 DS    0H                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
*                                                                               
         L     R3,RFTFTXTA         A(FILTERS)                                   
         SR    R0,R0                                                            
         ICM   R0,1,RFTFTXTN       LINE COUNT                                   
         BZ    ITXT0100                                                         
*                                  SPACE FILL BUFFER                            
         TR    0(80,R3),EBCDIC                                                  
         LA    R3,80(R3)                                                        
         BCT   R0,*-10                                                          
*                                                                               
         ZIC   R3,RFTFTX1N         TEXT LINE COUNT                              
         ZIC   R0,RFTFTXTN         TEXT + FILTER LINE COUNT                     
         SR    R0,R3               FILTER LINE COUNT                            
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         BNP   ITXT0038                                                         
*                                                                               
         L     R3,RFTFTXTA         A(FILTERS)                                   
ITXT0030 DS    0H                                                               
         GOTO1 =A(TXT#RECD),C'0',RR=Y         ITXT HEADER                       
*                                                                               
         MVC   MYP+ITXTTEXT(80),0(R3)      FILTER TEXT                          
*                                                                               
         GOTO1 VDOPUT                                                           
*                                                                               
         LA    R3,132(R3)                                                       
         BCT   R0,ITXT0030                                                      
*                                                                               
ITXT0038 DS    0H                                                               
         L     R3,RFTFTX1A         A(FIRST LINE)                                
         ZIC   R0,RFTFTX1N         LINE COUNT                                   
*                                                                               
         LTR   R0,R0               IDIOTS THAT PUT IN NO TEXT                   
         BZ    ITXT0100                                                         
*                                                                               
ITXT0040 DS    0H                                                               
         GOTO1 =A(TXT#RECD),C'1',RR=Y         ITXT HEADER                       
*                                                                               
         MVC   MYP+ITXTTEXT(80),0(R3)      FILTER TEXT                          
*                                                                               
         GOTO1 VDOPUT                                                           
*                                                                               
         LA    R3,132(R3)                                                       
         BCT   R0,ITXT0040                                                      
*                                                                               
ITXT0100 DS    0H                                                               
         B     FTCHEXIT                                                         
FTCHEXIT EQU   *                                                                
*                                                                               
*   TEST                                                                        
**       MVC   P+1(19),=C'FTCHEXIT     :     '                                  
**       GOTO1 REPORT                                                           
*   TEST:  END                                                                  
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
EBCDIC   DC    XL16'40404040404040404040404040404040'  00-0F **TEMP**           
         DC    XL16'40404040404040404040404040404040'  10-1F **TEMP**           
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'404040404040404040404A4B4C4D4E4F'  40-4F                    
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50-5F                    
         DC    XL16'606140404040404040406A6B6C6D6E6F'  60-6F                    
         DC    XL16'404040404040404040797A7B7C7D7E7F'  70-7F                    
         DC    XL16'40818283848586878889404040404040'  80-8F                    
         DC    XL16'40919293949596979899404040404040'  90-9F                    
         DC    XL16'40A1A2A3A4A5A6A7A8A9404040404040'  A0-AF                    
         DC    XL16'40404040404040404040404040404040'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404040404040'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0-D1                    
         DC    XL16'E040E2E3E4E5E6E7E8E9404040404040'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040'  F0-FF                    
*************************************************************                   
         EJECT                                                                  
*                                                                               
*   SETELEM:  DISPLAY DATA COMING BACK FROM FETCH                               
*                                                                               
SETELEM  NTR1  BASE=*,LABEL=*                                                   
*        L     R3,0(R1)            SET A(DATA COMING BACK)                      
*        MVC   P+1(18),=C'RETURN FROM FETCH:'                                   
*        MVC   P+20(64),0(R3)                                                   
*        GOTO1 REPORT                                                           
*        GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ADDDATA:  DISPLAY DATA COMING BACK FROM FETCH                               
*                                                                               
ADDDATA  NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            SET A(DATA COMING BACK)                      
         L     R2,8(R1)            SET A(INDEX NUMBER)                          
         LA    R4,DATANAME                                                      
         SLL   R2,4                MULTIPLY INDEX BY 16                         
         AR    R4,R2                                                            
**       MVC   P+1(18),=C'RETURN FROM FETCH:'                                   
**       MVC   P+20(8),0(R4)                                                    
**       MVC   P+30(64),0(R3)                                                   
**       GOTO1 REPORT                                                           
**       GOTO1 REPORT                                                           
         XC    P,P                 CLEAR PRINT LINE IN ALL CASES                
*                                                                               
         L     RF,8(R4)            LOAD PROCESSING ROUTINE                      
         LTR   RF,RF               ANY ADDRESS IN FIELD?                        
         BZ    AADD0800            NO  - EXIT                                   
         GOTO1 (RF)                BRANCH TO PROCESSING ROUTINE                 
***      MVC   P+1(18),=C'ROUTINE PERFORMED:'                                   
***      GOTO1 REPORT                                                           
***      GOTO1 REPORT                                                           
AADD0800 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*   DATANAME TABLE:  PROVIDES:                                                  
*        A)  LABELS FOR DEBUG TOOL FROM REFETCH (POS 1-8)                       
*        B)  ROUTINE ADDRESS TO FORMAT OUTPUT FOR DATA (POS 9-12)               
*        C)  SPARE ARGUMENT (POS 13-16)                                         
*                                                                               
         DS    0F                  FULLWORD ALIGN                               
DATANAME DC    CL8'STAT    ',A(STATRECD),F'0'       0                           
         DC    CL8'INV #   ',A(INV#RECD),F'0'       1                           
         DC    CL8'STDATE  ',A(STDTRECD),F'0'       2                           
         DC    CL8'ENDATE  ',A(ENDTRECD),F'0'       3                           
         DC    CL8'DAYPT   ',A(DYPTRECD),F'0'       4                           
         DC    CL8'DAYS    ',A(DAYSRECD),F'0'       5                           
         DC    CL8'STTIME  ',A(STTMRECD),F'0'       6                           
         DC    CL8'ENTIME  ',F'0',F'0'              7                           
         DC    CL8'AVAILDAY',A(AVDYRECD),F'0'       8                           
         DC    CL8'AVAILTIM',A(AVTMRECD),F'0'       9                           
         DC    CL8'PROGRM  ',A(PGNMRECD),F'0'       10                          
         DC    CL8'INVSEQ  ',F'0',F'0'              11                          
         DC    CL8'BK SEQ  ',A(BKSQRECD),F'0'       12                          
         DC    CL8'FOOTNOTE',A(FTN1RECD),F'0'       13                          
         DC    CL8'FOOTNOTE',A(FTN2RECD),F'0'       14                          
         DC    CL8'UPGCMMT ',A(UPCMRECD),F'0'       15                          
         DC    CL8'INVSEQ# ',F'0',F'0'              16                          
         DC    CL8'UPGRADE ',A(UPGRRECD),F'0'       17                          
         DC    CL8'UPG FOOT',A(UPFTRECD),F'0'       18                          
         DC    CL8'BKS FOOT',A(BKFTRECD),F'0'       19                          
         DC    CL8'DEMO SEQ',A(DSEQRECD),F'0'       20                          
         DC    CL8'RATING  ',A(RTGRECD),F'0'        21                          
         DC    CL8'SHARE   ',A(SHRRECD),F'0'        22                          
         DC    CL8'HUT/PUT ',A(HUTRECD),F'0'        23                          
         DC    CL8'INVSEQ# ',F'0',F'0'              24                          
         DC    CL8'RCDSEQ# ',F'0',F'0'              25                          
         DC    CL8'RCD1STWK',F'0',F'0'              26                          
         DC    CL8'RCD RATE',F'0',F'0'              27                          
         DC    CL8'1STWKRCD',F'0',F'0'              28                          
         DC    CL8'RATE RCD',F'0',F'0'              29                          
         DC    CL8'RCDLSTWK',F'0',F'0'              30                          
         DC    CL8'INVSEQ# ',F'0',F'0'              31                          
         DC    CL8'TEXT#   ',A(TXT#RECD),F'0'       32                          
         DC    CL8'N/A     ',F'0',F'0'              33                          
         DC    CL8'TEXT1   ',A(TXT1RECD),F'0'       34                          
         DC    CL8'TEXT2   ',A(TXT2RECD),F'0'       35                          
         DC    CL8'HD02RECD',A(HD02RECD),F'0'       36                          
         EJECT                                                                  
*                                                                               
*   START OF CALLED ROUTINES FOR EACH PIECE OF DATA RETURNED.                   
*                                                                               
STATRECD NTR1  BASE=*,LABEL=*                                                   
         MVC   SAVESTAT,0(R3)      SAVE STATION CALL LETTERS                    
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
INV#RECD NTR1  BASE=*,LABEL=*                                                   
         MVC   MYP,SPACES          CLEAR O/P DATA LINE                          
         MVC   MYP+HD01STA(5),SAVESTAT     INSERT STATION CALL LETTERS          
         MVC   MYP+HD01CTL(4),=C'HD01'     INSERT CONTROL ID                    
         MVC   MYP+HD01INV(4),0(R3)        INSERT INVENTORY NUMBER              
         MVC   SAVEINV#,0(R3)              SAVE INVENTORY #                     
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
STDTRECD NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATCON,DMCB,(8,0(R3)),(X'20',WORK+34)                            
*                                  CONVERT FROM JULIAN TO EBCDIC                
         MVC   WORK+32(2),=C'19'   INSERT CENTURY                               
         CLC   =C'50',WORK+34      CHECK FOR CENTURY                            
         BL    STDT0020            STILL IN 20TH                                
         MVC   WORK+32(2),=C'20'   INSERT 21ST CENTURY                          
STDT0020 EQU   *                                                                
         MVC   MYP+HD01STDT(8),WORK+32                                          
*                                  INSERT START DATE                            
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
ENDTRECD NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATCON,DMCB,(8,0(R3)),(X'20',WORK+34)                            
*                                  CONVERT FROM JULIAN TO EBCDIC                
         MVC   WORK+32(2),=C'19'   INSERT CENTURY                               
         CLC   =C'50',WORK+34      CHECK FOR CENTURY                            
         BL    ENDT0020            STILL IN 20TH                                
         MVC   WORK+32(2),=C'20'   INSERT 21ST CENTURY                          
ENDT0020 EQU   *                                                                
         MVC   MYP+HD01ENDT(8),WORK+32                                          
*                                  INSERT END  DATE                             
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
DYPTRECD NTR1  BASE=*,LABEL=*                                                   
         MVC   MYP+HD01DYPT(6),0(R3)        INSERT DAYPART(S)                   
         OC    MYP+HD01DYPT(8),SPACES       CLEAR ANY BIN ZERO                  
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
DAYSRECD NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DAYUNPK,DMCB,(0,0(R3)),(7,WORK)                                  
         MVC   MYP+HD01DAYS(7),WORK         INSERT DAYS                         
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
STTMRECD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*   THIS ROUTINE OUTPUTS START/END TIMES AS A STRING                            
*                                                                               
         GOTO1 UNTIME,DMCB,(0,0(R3)),WORK                                       
         MVC   MYP+HD01TIME(11),WORK        INSERT TIMES                        
         GOTO1 VDOPUT              OUTPUT RECORD                                
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
*   FOLLOWING THREE FIELDS ARE HD02.  THE  PROGRAM NAME WILL ALWAYS             
*        BE PRESENT.  OTHER TWO ARE OPTIONAL.                                   
*                                                                               
AVDYRECD NTR1  BASE=*,LABEL=*                                                   
**       MVC   P+1(09),=C'AVDYRECD:'                                            
**       MVC   P+12(11),0(R3)                                                   
**       GOTO1 REPORT                                                           
*                                                                               
         L     RF,ADDRAVOR         SET A(SLOT FOR AVAIL DAY)                    
         MVC   DAVDAY(11,RF),0(R3) INSERT AVAIL DAY INTO TABLE                  
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
AVTMRECD NTR1  BASE=*,LABEL=*                                                   
**       MVC   P+1(09),=C'AVTMRECD:'                                            
**       MVC   P+12(11),0(R3)                                                   
**       GOTO1 REPORT                                                           
*                                                                               
         L     RF,ADDRAVOR         SET A(SLOT FOR AVAIL TIME)                   
         MVC   DAVTIME(11,RF),0(R3) INSERT AVAIL TIME                           
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
PGNMRECD NTR1  BASE=*,LABEL=*                                                   
**       MVC   P+1(09),=C'PGNMRECD:'                                            
**       MVC   P+12(27),0(R3)                                                   
**       GOTO1 REPORT                                                           
*                                                                               
         L     RF,ADDRAVOR         SET A(SLOT FOR AVAIL TIME)                   
         MVC   DPGNAME(27,RF),0(R3) INSERT PROGRAM NAME                         
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
HD02RECD NTR1  BASE=*,LABEL=*                                                   
         LA    R3,AVORIDES                                                      
         LA    R6,8                                                             
HD02R020 EQU   *                                                                
         OC    0(LAVORIDE,R3),0(R3)      ANYTHING IN SLOT?                      
         BZ    HD02R080            NO  - FINISHED                               
         MVC   MYP+HD02CTL(4),=C'HD02'     INSERT CONTROL ID                    
         MVC   MYP+HD02PGNM(27),DPGNAME(R3)  INSERT PROGRAM NAME                
         MVC   MYP+HD02AVDY(11),DAVDAY(R3)   INSERT AVAIL DAY                   
         MVC   MYP+HD02AVTM(11),DAVTIME(R3)  INSERT AVAIL TIME                  
         OC    MYP,SPACES          SET BIN ZEROS TO SPACES                      
         GOTO1 VDOPUT              OUTPUT RECORD                                
         LA    R3,LAVORIDE(R3)     BUMP TO NEXT SLOT                            
         BCT   R6,HD02R020         GO BACK FOR NEXT                             
HD02R080 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
BKSQRECD NTR1  BASE=*,LABEL=*                                                   
*JRD                                                                            
*        MVC   P+1(09),=C'NEW BOOK:'                                            
*        EDIT  (1,0(R3)),(2,P+12)                                               
*        GOTO1 REPORT                                                           
*                                                                               
         MVC   SAVEDMHD,=C'DM00'   INITIALIZE DEMO RECORD CONTROL               
         L     R2,AFRSTBK          SET A(BOOKS REQUESTED)                       
*                                                                               
***      MVC   P+1(09),=C'AFRSTBK :'                                            
***      MVC   P+16(32),0(R2)                                                   
***      GOTO1 REPORT                                                           
*                                                                               
         ZIC   R1,0(R3)            GET BOOK INDEX                               
         BCTR  R1,0                MAKE ZERO RELATIVE                           
         MH    R1,=H'6'            MULTIPLY BY SIX (L(BOOK FIELD))              
         AR    R2,R1               DISPLACE TO 'THIS BOOK'                      
*                                                                               
***      MVC   P+1(09),=C'THISBK  :'                                            
***      MVC   P+16(06),0(R2)                                                   
***      GOTO1 REPORT                                                           
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         MVI   WORK,16             DUMMY UP 'FIELD HDR LENGTH'                  
*                                                                               
         GOTOX =V(UNBOOK),DMCB,(1,0(R2)),WORK,0,0                               
*                                                                               
*JRD                                                                            
*        MVC   P+1(09),=C'UNBOOK  :'                                            
*        MVC   P+12(3),0(R2)                                                    
*        MVC   P+16(32),WORK                                                    
*        GOTO1 REPORT                                                           
*                                                                               
         MVC   SAVEBOOK(8),WORK+8                                               
         MVC   SAVEBOOK+1(1),4(R2)                                              
*                                  TAKE BOOK FROM INPUT LIST                    
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
DSEQRECD NTR1  BASE=*,LABEL=*                                                   
*JRD                                                                            
*        MVC   P+1(09),=C'NEW DEMO:'                                            
*        EDIT  (1,0(R3)),(2,P+12)                                               
*        GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
UPCMRECD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MYP+BPJFCTL(4),=C'BPJF'                                          
         MVC   MYP+BPJFBOOK(8),SAVEBOOK                                         
*                                                                               
         L     RE,0(R1)            PJ FORMULA COMMENT                           
         L     RF,4(R1)            FOOTNOTE LENGTH                              
         CHI   RF,95-BPJFCOM                                                    
         BNH   *+8                                                              
         LHI   RF,95-BPJFCOM                                                    
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MYP+BPJFCOM(0),0(RE)                                             
*                                                                               
         GOTO1 VDOPUT                                                           
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
UPGRRECD NTR1  BASE=*,LABEL=*                                                   
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
UPFTRECD NTR1  BASE=*,LABEL=*                                                   
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
BKFTRECD NTR1  BASE=*,LABEL=*                                                   
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
RTGRECD  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,MYP+DM01DRSH     SET A(1ST SET OF R/S/H)                      
         L     RF,DEMOLOOP         CALCULATE DISPLACEMENT                       
         SR    RE,RE                                                            
         LA    R1,LDM01RSH                                                      
         MR    RE,R1               MULTIPLY BY L(SET)                           
         AR    R2,RF               DISPLACE TO A(1ST SET OF R/S/H)              
         EDIT  (4,0(R3)),(8,DM01DRTG(R2)),FILL=0                                
*                                  RTG IS 8 CHARS LONG (MAY BE THOUS)           
         ST    R2,SAVERSH                                                       
*                                                                               
*   TEST PRINT OUTPUT                                                           
***      MVC   P+1(09),=C'BUILDRTG:'                                            
***      MVC   P+10(20),0(R2)                                                   
***      MVC   P+32(4),0(R3)                                                    
***      GOTO1 REPORT                                                           
*   TEST PRINT OUTPUT END                                                       
*                                                                               
         L     RF,DEMOLOOP         INCREMENT SET W/IN RECORD                    
         LA    RF,1(RF)                                                         
         ST    RF,DEMOLOOP                                                      
         L     RF,DEMOCTR          INCREMENT TOTAL DEMOS PROCESSED              
         LA    RF,1(RF)               IN CASE LAST RECORD NOT FULL              
         ST    RF,DEMOCTR             MUST BE AWARE TO OUTPUT                   
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
SHRRECD  NTR1  BASE=*,LABEL=*                                                   
         L     R2,SAVERSH          SET A(SET W/IN RECORD)                       
         EDIT  (4,0(R3)),(4,DM01DSHR(R2)),FILL=0                                
*                                                                               
*   TEST PRINT OUTPUT                                                           
***      MVC   P+1(09),=C'BUILDSHR:'                                            
***      MVC   P+10(20),0(R2)                                                   
***      MVC   P+32(4),0(R3)                                                    
***      GOTO1 REPORT                                                           
*   TEST PRINT OUTPUT END                                                       
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
HUTRECD  NTR1  BASE=*,LABEL=*                                                   
         L     R2,SAVERSH          SET A(SET W/IN RECORD)                       
         EDIT  (4,0(R3)),(8,DM01DHUT(R2)),FILL=0                                
*                                                                               
*   TEST PRINT OUTPUT                                                           
***      MVC   P+1(09),=C'BUILDHUT:'                                            
***      MVC   P+10(20),0(R2)                                                   
***      MVC   P+32(4),0(R3)                                                    
***      GOTO1 REPORT                                                           
*   TEST PRINT OUTPUT END                                                       
*                                                                               
         CLC   DEMOCTR+2,NUMDEMS   MAX DEMOS REACHED?                           
         BNE   HUTR0020            NO  - LOOK FOR FULL RECORD                   
         XC    DEMOCTR,DEMOCTR     CLEAR COUNTER                                
         B     HUTR0040            YES - OUTPUT LAST RECORD OF SET              
HUTR0020 EQU   *                                                                
         CLI   DEMOLOOP+3,4        FOUR ENTRIES IN RECORD?                      
*                                                                               
         BNE   HUTR0100            NO  - DON'T OUTPUT YET                       
HUTR0040 EQU   *                                                                
         L     RF,DEMHDCTR         INCREMENT DEMO HDR CONTROL COUNT             
         LA    RF,1(RF)                                                         
         ST    RF,DEMHDCTR                                                      
         EDIT  (RF),(2,SAVEDMHD+2),FILL=0                                       
         MVC   MYP+DM01CTL(4),SAVEDMHD                                          
         MVC   MYP+DM01BOOK(8),SAVEBOOK                                         
         XC    DEMOLOOP,DEMOLOOP   CLEAR COUNTER                                
**       MVC   P+1(07),=C'PUTREC:'                                              
**       MVC   P+8(96),MYP                                                      
**       GOTO1 REPORT                                                           
         GOTO1 VDOPUT                                                           
HUTR0100 EQU   *                                                                
**       MVC   P+1(07),=C'BLDREC:'                                              
**       MVC   P+8(96),MYP                                                      
**       GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
FTN1RECD NTR1  BASE=*,LABEL=*                                                   
         OC    MYP+BINFFOOT(95-BINFFOOT),SPACES                                 
*                                                                               
         CLC   MYP+BINFFOOT(95-BINFFOOT),SPACES                                 
         BE    FTN1X                                                            
*                                                                               
         MVC   MYP+BINFCTL(4),=C'BINF'                                          
         MVC   MYP+BINFBOOK(8),SAVEBOOK                                         
*                                                                               
         GOTO1 VDOPUT                                                           
*                                                                               
FTN1X    DS    0H                                                               
         XIT1                                                                   
         MVC   P+1(16),=C'FOOTNOTE1:      '                                     
         MVC   P+12(5),SAVESTAT                                                 
         MVI   P+17,C'/'                                                        
         MVC   P+18(4),SAVEINV#                                                 
         MVI   P+22,C'/'                                                        
         MVC   P+23(8),SAVEBOOK                                                 
         MVC   P+34(96),0(R3)                                                   
         GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
FTN2RECD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,0(R1)            FOOTNOTE ADDR                                
         L     RF,4(R1)            FOOTNOTE LENGTH                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MYP+BINFFOOT(0),0(RE)                                            
*                                                                               
         XIT1                                                                   
         MVC   P+1(16),=C'FOOTNOTE2:      '                                     
         MVC   P+12(5),SAVESTAT                                                 
         MVI   P+17,C'/'                                                        
         MVC   P+18(4),SAVEINV#                                                 
         MVI   P+22,C'/'                                                        
         MVC   P+23(8),SAVEBOOK                                                 
         MVC   P+34(96),0(R3)                                                   
         GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
TXT#RECD NTR1  BASE=*,LABEL=*                                                   
         USING RFTBLKD,R4                                                       
*                                                                               
         MVC   MYP+ITXTCTL(4),=C'ITXT'     INSERT CONTROL ID                    
         STC   R1,MYP+ITXTSUB              SUBRECORD ID                         
*                                                                               
         ZICM  R0,RFTFTXT#,2       INVENTORY NUMBER                             
         EDIT  (R0),(4,MYP+ITXTNUM),ALIGN=LEFT                                  
*                                                                               
         MVC   MYP+ITXTWRAP(1),RFTFTXFL    WRAPPING FLAG                        
*                                                                               
         XIT1                                                                   
         MVC   P+1(16),=C'TEXT#    :      '                                     
         MVC   P+20(96),0(R3)                                                   
         GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
TXT1RECD NTR1  BASE=*,LABEL=*                                                   
         XIT1                                                                   
         MVC   P+1(16),=C'TEXT1    :      '                                     
         MVC   P+20(96),0(R3)                                                   
         GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         SPACE 3                                                                
TXT2RECD NTR1  BASE=*,LABEL=*                                                   
         XIT1                                                                   
         MVC   P+1(16),=C'TEXT2    :      '                                     
         MVC   P+20(96),0(R3)                                                   
         GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 3                                                                
*        LTORG                                                                  
         EJECT                                                                  
DOPUT    NTR1  BASE=*,LABEL=*                                                   
         MVI   MYP+95,C':'         INSERT LINE DELIMITER                        
*                                                                               
         OC    MYP,SPACES          SET LOW-VALUE TO SPACE                       
         LA    R3,INTFILE          A(INTFILE) FOR PUT ROUTINE                   
         LA    R6,MYP                                                           
*                                                                               
         PUT   (R3),(R6)                                                        
*                                                                               
         L     RF,PUTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR                                                        
*                                                                               
         CLI   QUESTOR+1,C'Y'      DISPLAY OUTPUT?                              
         BNE   DOPUT020            NO                                           
         MVC   P+1(04),=C'O/P='                                                 
         MVC   P+5(96),MYP                                                      
         GOTO1 REPORT                                                           
DOPUT020 EQU   *                                                                
*                                                                               
         MVC   MYP,SPACES          CLEAR OUTPUT RECORD                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*-----------------------------------------------------------------              
*                                                                               
* CREATES EDICT HEADER CARDS FOR EDICT PROCESSING                               
*                                                                               
*-----------------------------------------------------------------              
EDICT    NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R2,0(R1)            SET A(RETURN AREA FOR GEN FILENAME)          
*                                     DO NOT REUSE R2                           
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
*                                                                               
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
                                                                                
***>>>   MVC   P+9(11),=C'EDICT=*BIAS'                                          
         MVC   P+9(10),=C'EDICT=MOTX'                                           
*                                                                               
         MVC   P+34(4),=C'W  D'    WIDE REPORT - 132 CHARS                      
*                                     D = ?? (YI NEEDS IT)                      
         MVC   P+54(2),=C'MO'      SET TO MEDIA OCEAN                           
*                                  SEND SPECIAL PRINT LINE                      
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
* PRINT A ++DDS CARD WITH 58 BYTES OF DATA FOR EDICT XACTION PROG               
*                                                                               
         MVC   P,SPACES            CLEAR PRINTLINE                              
         LA    R5,P                                                             
         USING EDICTD,R5                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'RE'      FOR SYSTEM REP                               
                                                                                
         MVC   EDIPROG,=C'MOT'     MEDIA OCEAN FILE XFER                        
                                                                                
         MVC   EDIIDEN,=C'TRN'     XACTION FILE DATA                            
                                                                                
***      MVC   EDIRCNRP,RCREPFL    REP CODE                                     
***      MVC   EDIRCNOF,RCREPFL    OFF CODE                                     
***      MVC   EDIRCNSP,=C'AAA'    SALESPERSON CODE                             
***      MVC   EDIRCNAG,=C'BBBB'   AGENCY CODE                                  
***      MVC   EDIRCNAO,=C'CC'     CITY CODE                                    
***      MVC   EDIRCNAD,=C'DDDD'   ADVERTISER CODE                              
***      MVC   EDIRCNCT,=C'E'      CONTRACT TYPE                                
* FLIGHT START AND END DATES                                                    
***      GOTO1 DATCON,DMCB,(5,WORK),(5,EDIRCNFS)                                
***      GOTO1 DATCON,DMCB,(5,WORK),(5,EDIRCNFE)                                
* LATEST VERSION NUMBER                                                         
**       LA    R6,RCONREC                                                       
**       USING RCONSEND,R6                                                      
**       MVI   ELCODE,X'20'                                                     
**       BAS   RE,GETEL                                                         
**       BNE   EDICT20                                                          
**       CLC   RCONSRV,RCONSSV     SHOW LATEST VERSION                          
**       BH    EDICT10                                                          
**       EDIT  (1,RCONSSV),(3,EDIRCNVN),ALIGN=LEFT                              
**       B     EDICT20                                                          
**EDICT10  EDIT  (1,RCONSRV),(3,EDIRCNVN),ALIGN=LEFT                            
                                                                                
**EDICT20  MVC   EDIRCNST,RCONKSTA   STATION CALLS                              
* CONTRACT NUMBER                                                               
**         ZAP   WORK(5),=P'0'                                                  
**         MVO   WORK(5),RCONKCON                                               
**         EDIT  (P5,WORK),(8,EDIRCNHN),ALIGN=LEFT                              
***                                  SEND SPECIAL PRINT LINE                    
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(32),=C'SUB MEDIA OCEAN RATINGS DOWNLOAD'               
                                                                                
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(04),=C'DSN '                                           
         MVC   EDISYST+09(44),0(R2)                                             
*                                  INSERT FILE NAME CREATED,                    
*                                     WITH GENERATION NUMBER                    
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(04),=C'FIL '                                           
         MVC   EDISYST+09(44),0(R2)                                             
*                                  INSERT FILE NAME CREATED,                    
*                                     WITH GENERATION NUMBER                    
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(07),=C'EXT TXT'                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVI   FORCEHED,C'Y'                                                    
                                                                                
EDICTX   DS    0H                                                               
         DROP  R5                                                               
****     DROP  R6                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   RECORDS FOR THIS FILE ARE BUILT IN MYP, AND ARE OUTPUT                      
*        DIRECTLY FROM THERE.  NOTHING FANCY.                                   
*                                                                               
INTFILE  DCB   DDNAME=INTFILE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00096,                                            X        
               BLKSIZE=05760,                                          X        
               MACRF=PM                                                         
         SPACE 4                                                                
         EJECT                                                                  
         LTORG                                                                  
*******************************************************************             
* TABLE DEFINING ALL OF THE DESIRED DEMO CATEGORIES                             
*******************************************************************             
DEMOTAB  DS    0D                                                               
         DC    CL8' V2-17  '                                                    
         DC    CL8' V2-24  '                                                    
         DC    CL8' V2-34  '                                                    
         DC    CL8' V2-49  '                                                    
         DC    CL8' V2-64  '                                                    
         DC    CL8' V2+    '                                                    
         DC    CL8' V6-17  '                                                    
         DC    CL8' V6-24  '                                                    
         DC    CL8' V6-34  '                                                    
         DC    CL8' V6-49  '                                                    
         DC    CL8' V6-54  '                                                    
         DC    CL8' V6-64  '                                                    
         DC    CL8' V6+    '                                                    
         DC    CL8' V1220  '                                                    
         DC    CL8' V1224  '                                                    
         DC    CL8' V1234  '                                                    
         DC    CL8' V1249  '                                                    
         DC    CL8' V1254  '                                                    
         DC    CL8' V1264  '                                                    
         DC    CL8' V12+   '                                                    
         DC    CL8' V1820  '                                                    
         DC    CL8' A1824  '                                                    
         DC    CL8' A1834  '                                                    
         DC    CL8' A1849  '                                                    
         DC    CL8' A1854  '                                                    
         DC    CL8' A1864  '                                                    
         DC    CL8' A18+   '                                                    
         DC    CL8' V2124  '                                                    
         DC    CL8' V2134  '                                                    
         DC    CL8' V2149  '                                                    
         DC    CL8' V2154  '                                                    
         DC    CL8' V2164  '                                                    
         DC    CL8' V21+   '                                                    
         DC    CL8' A2534  '                                                    
         DC    CL8' A2549  '                                                    
         DC    CL8' A2554  '                                                    
         DC    CL8' A2564  '                                                    
         DC    CL8' A25+   '                                                    
         DC    CL8' A3549  '                                                    
         DC    CL8' A3554  '                                                    
         DC    CL8' A3564  '                                                    
         DC    CL8' A35+   '                                                    
         DC    CL8' A5054  '                                                    
         DC    CL8' A5064  '                                                    
         DC    CL8' A50+   '                                                    
         DC    CL8' A5564  '                                                    
         DC    CL8' A55+   '                                                    
         DC    CL8' A65+   '                                                    
         DC    CL8' W1217  '                                                    
         DC    CL8' W1220  '                                                    
         DC    CL8' W1224  '                                                    
         DC    CL8' W1234  '                                                    
         DC    CL8' W1249  '                                                    
         DC    CL8' W1254  '                                                    
         DC    CL8' W1264  '                                                    
         DC    CL8' W12+   '                                                    
         DC    CL8' W1820  '                                                    
         DC    CL8' W1824  '                                                    
         DC    CL8' W1834  '                                                    
         DC    CL8' W1849  '                                                    
         DC    CL8' W1854  '                                                    
         DC    CL8' W1864  '                                                    
         DC    CL8' W18+   '                                                    
         DC    CL8' W2124  '                                                    
         DC    CL8' W2134  '                                                    
         DC    CL8' W2149  '                                                    
         DC    CL8' W2154  '                                                    
         DC    CL8' W2164  '                                                    
         DC    CL8' W21+   '                                                    
         DC    CL8' W2534  '                                                    
         DC    CL8' W2549  '                                                    
         DC    CL8' W2554  '                                                    
         DC    CL8' W2564  '                                                    
         DC    CL8' W25+   '                                                    
         DC    CL8' W3549  '                                                    
         DC    CL8' W3554  '                                                    
         DC    CL8' W3564  '                                                    
         DC    CL8' W35+   '                                                    
         DC    CL8' W5054  '                                                    
         DC    CL8' W5064  '                                                    
         DC    CL8' W50+   '                                                    
         DC    CL8' W5564  '                                                    
         DC    CL8' W55+   '                                                    
         DC    CL8' W65+   '                                                    
         DC    CL8' M1217  '                                                    
         DC    CL8' M1220  '                                                    
         DC    CL8' M1224  '                                                    
         DC    CL8' M1234  '                                                    
         DC    CL8' M1249  '                                                    
         DC    CL8' M1254  '                                                    
         DC    CL8' M1264  '                                                    
         DC    CL8' M12+   '                                                    
         DC    CL8' M1820  '                                                    
         DC    CL8' M1824  '                                                    
         DC    CL8' M1834  '                                                    
         DC    CL8' M1849  '                                                    
         DC    CL8' M1854  '                                                    
         DC    CL8' M1864  '                                                    
         DC    CL8' M18+   '                                                    
         DC    CL8' M2124  '                                                    
         DC    CL8' M2134  '                                                    
         DC    CL8' M2149  '                                                    
         DC    CL8' M2154  '                                                    
         DC    CL8' M2164  '                                                    
         DC    CL8' M21+   '                                                    
         DC    CL8' M2534  '                                                    
         DC    CL8' M2549  '                                                    
         DC    CL8' M2554  '                                                    
         DC    CL8' M2564  '                                                    
         DC    CL8' M25+   '                                                    
         DC    CL8' M3549  '                                                    
         DC    CL8' M3554  '                                                    
         DC    CL8' M3564  '                                                    
         DC    CL8' M35+   '                                                    
         DC    CL8' M5054  '                                                    
         DC    CL8' M5064  '                                                    
         DC    CL8' M50+   '                                                    
         DC    CL8' M5564  '                                                    
         DC    CL8' M55+   '                                                    
         DC    CL8' M65+   '                                                    
         DC    CL8' CH2-5  '                                                    
         DC    CL8' CH2-11 '                                                    
         DC    CL8' CH6-11 '                                                    
         DC    CL8' TN1217 '                                                    
         DC    CL8' WWORK  '                                                    
         DC    CL8' HOMES  '                                                    
         DC    CL8' METRO  '                                                    
         DC    CL8' METB   '                                                    
NUMCATQ  EQU   (*-DEMOTAB)/8                                                    
NUMDEMQ  EQU   (2*NUMCATQ)                                                      
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006REREP3P02 01/10/11'                                      
         END                                                                    
**  INSERT END  >>                                                              
