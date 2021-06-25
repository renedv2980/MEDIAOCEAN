*          DATA SET SPOMS08    AT LEVEL 039 AS OF 03/14/16                      
*PHASE T23408C  <<<<=====                                                       
T23408   TITLE 'SPOMS08 - LIST OF DARE ORDERS'                                  
T23408   CSECT                                                                  
         PRINT NOGEN                                                            
BGN      NMOD1 0,*T23408*,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)                   STANDARD CODING                       
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA             BASE SCREEN + OUR SCREEN              
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE                R5=A(LOCAL SAVED STORAGE)             
         USING LSSD,R5                                                          
*                                                                               
         BAS   RE,GETPF                   GET PFKEYS                            
         ST    R3,RELO                                                          
         ST    RC,BASERC                                                        
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A5D',0  TSAR                                
         MVC   VTSAR,0(R1)                                                      
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)                                   
         MVI   FFS,X'FF'                                                        
         MVC   FFS+1(L'FFS-1),FFS                                               
*                                                                               
         LA    R3,RELOTAB                                                       
INIT3    CLI   0(R3),X'FF'                                                      
         BE    INIT5                                                            
         ICM   RF,15,0(R3)                                                      
         A     RF,RELO                                                          
         ICM   RE,15,4(R3)                                                      
         LA    RE,LSSD(RE)                                                      
         STCM  RF,15,0(RE)                                                      
         LA    R3,L'RELOTAB(R3)                                                 
         B     INIT3                                                            
*                                                                               
INIT5    OI    GLSTSTAT,APPLCDSP+RETEXTRA                                       
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(15,JDTTODAY)   GET CURRENT DATE               
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,DATEFLTR)                                   
         GOTO1 ADDAY,DMCB,DATEFLTR,DATEFLTR,F'-14'                              
         GOTO1 DATCON,DMCB,(0,DATEFLTR),(19,DATEFLT1)                           
*                                                                               
         CLI   MODE,VALKEY                VALIDATE KEY?                         
         BNE   INIT7                                                            
         GOTOR VK                                                               
         J     XIT                                                              
*                                                                               
INIT7    CLI   MODE,LISTRECS              LIST RECORDS?                         
         BE    LST                                                              
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                              *         
***********************************************************************         
LST      DS    0H                                                               
         CLI   TMPSTNUM,0                 TEMPEST RESERVED ?                    
         BE    LST01                                                            
         GOTOR TOTSAR,TSRRESQ             RESTORE TSAR                          
*                                                                               
LST01    TM    CNTL,CNTLRFSH              TEST REFRESH                          
         BNO   *+8                                                              
         OI    MISCFLG1,MF1KYCHG          MAKE THE KEY CHANGE                   
*                                                                               
         TM    CNTL,CNTLSAME              WANT TO DISPLAY SAME SCREEN           
         BNO   *+8                                                              
         NI    CNTL,X'FF'-(CNTLREAD)      TURN OFF READ SWITCH                  
*                                                                               
         NI    CNTL,X'FF'-(CNTLRFSH)      TURN OFF 'REFRESH'                    
         NI    CNTL,X'FF'-(CNTLMXIO)      TURN OFF 'REACHED MAX IO'             
         MVI   NLISTS,NLISTQ              # OF LINES TO LIST                    
         MVI   NLST,0                     NUMBER IN LIST TABLE                  
         XC    NSCR,NSCR                  NUMBER ON SCREEN                      
         MVC   BUYRC,BUYRL                SET BUYER CODE FROM LIST              
         MVI   XBYR,1                     SET TO DISPLAY FIRST BUYER            
         MVC   LLIST,=Y(LINNEXTL-LINSTTH)                                       
         MVC   AGYMD,BAGYMD                                                     
*                                                                               
         XC    IO_COUNT,IO_COUNT                                                
         XC    SVSELSID,SVSELSID                                                
         TWAXC ORLSEL1H,ORLPFLNH,PROT=Y                                         
*                                                                               
         TM    MISCFLG1,MF1KYCHG          DID THE KEY CHANGE ?                  
         BNO   LST02                      YES, START FROM BEGINNING             
         BAS   RE,SLCTK                   SELECT THE BEST KEY                   
         GOTOR BLDEST                     BUILD TABLE OF PROD/EST.              
*                                                                               
LST02    L     R3,ADKYTAB                                                       
         USING DKYD,R3                                                          
         SR    R0,R0                                                            
LST03    CLC   DKYTYP,BESTKEY             FIND TABLE ENTRY FOR THIS KEY         
         BE    LST07                                                            
         IC    R0,DKYTLN                                                        
         AR    R3,R0                      BUMP TO NEXT TABLE ENTRY              
         CLI   0(R3),X'FF'                                                      
         BNE   LST03                                                            
         DC    H'0'                       CAN'T MATCH SUB-TYPE TO TABLE         
LST07    ST    R3,AKYDAT                  R3=A(KEY DATA)                        
*                                                                               
         L     R3,ADSEQTAB                                                      
         USING SSQD,R3                                                          
*                                                                               
LST09    CLC   SSQCDE,SSQ                 FIND TABLE ENTRY FOR SEQ              
         BE    LST11                                                            
         IC    R0,SSQLN                                                         
         AR    R3,R0                      BUMP TO NEXT TABLE ENTRY              
         CLI   0(R3),X'FF'                                                      
         BNE   LST09                                                            
         DC    H'0'                       INVALID SEQUENCE                      
*                                                                               
LST11    ST    R3,ASQDAT                  R3=A(SEQUENCE DATA ENTRY)             
         LA    R3,SSQDATA                                                       
         USING SSQDATA,R3                                                       
LST12    CLI   SSQDQ,LDMKNQ               TEST MARKET IN KEY                    
         BNE   *+8                                                              
         OI    FKFLG,FKFMKT               SET MARKET# IN KEY                    
         CLI   SSQDQ,LDPRDCQ              PRODUCT CODE IN KEY                   
         BNE   *+8                                                              
         OI    FKFLG,FKFPRD               SET PRODUCT IN KEY                    
         TM    SSQDIND,SSQDIEND           TEST END OF KEY                       
         BO    LST13                                                            
         LA    R3,L'SSQDATA(R3)                                                 
         B     LST12                                                            
         DROP  R3                                                               
*                                                                               
LST13    TM    MISCFLG1,MF1KYCHG          DID THE KEY CHANGE ?                  
         BO    LST17                      YES, START FROM BEGINNING             
         TM    CNTL,CNTLMXTS              TEST 'MAX TSAR'                       
         JO    TOOMANY                                                          
         TM    CNTL,CNTLREAD              TEST TIME TO READ MORE                
         BO    LST15                                                            
         TM    CNTL,CNTLSAME              USE SAME TSAR RECORDS                 
         BO    LST39                                                            
         SR    RF,RF                                                            
         ICM   RF,3,TSRLST                GET NUMBER OF LAST LINE               
         AHI   RF,1                                                             
         CLM   RF,3,TSRTOT                TEST PASSED END                       
         BH    *+8                                                              
         STCM  RF,3,TSRFST                START AT NEXT                         
         B     LST39                                                            
*                                                                               
LST15    MVC   KEY(L'LASTKEY),LASTKEY     START WITH LAST KEY                   
         NI    CNTL,X'FF'-(CNTLMORE+CNTLREAD)                                   
         B     LST19                                                            
*                                                                               
LST17    GOTOR TOTSAR,TSRINIQ             INITIALIZE TSAR                       
         XC    KBUYRCD,KBUYRCD                                                  
         MVI   KYLN,0                     MINIMUN KEY LENGTH                    
*                                                                               
         OC    ESTTAB,ESTTAB                                                    
         JZ    NODATAR                                                          
*                                                                               
LST18    BAS   RE,FSTK                    SET FIRST KEY FOR READ HIGH           
         TM    RSO,RSOCLPR                TEST READ CLI/PRD                     
         BNO   LST19                                                            
         TM    FILTRFLG,FFLGPRD           SKIP IF ONE PRODUCT                   
         BO    LST19                                                            
         BAS   RE,PRDL                    GET NEXT CLIENT/PROD                  
         TM    FILTRFLG,FFLGCLT           CLIENT FILTER                         
         BZ    LST19                                                            
         CLI   NPRDL,0                    ANY PRODUCT CODES?                    
         JE    NODATAR                                                          
*                                                                               
LST19    MVI   RDUPDATE,C'N'                                                    
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
         GOTO1 HIGH                                                             
LST21    GOTOR CHKIO                      CHECK IO COUNT                        
         TM    CNTL,CNTLMXIO              EXCEEDED IO'S                         
         BO    LST35                                                            
*                                                                               
         SR    R1,R1                      TEST MINIMUM KEY                      
         IC    R1,KYLN                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BE    LST24                                                            
         TM    RSO,RSO1BYR                READ ONE BUYER AT A TIME              
         BNO   LST22                                                            
         CLI   NBYR,1                     IS THERE A BUYER LIST?                
         BE    LST38                      BRANCH IF NO LIST                     
         CLC   XBYR,NBYR                  TEST END OF BUYER LIST                
         BE    LST38                      ALREADY AT END                        
         SR    R1,R1                                                            
         IC    R1,XBYR                                                          
         AHI   R1,1                                                             
         STC   R1,XBYR                    UPDATE THE COUNT                      
         BCTR  R1,0                                                             
         MHI   R1,L'BUYRC                                                       
         LA    R1,BUYRL(R1)                                                     
         MVC   BUYRC,0(R1)                GET NEXT BUYER CODE                   
         B     LST18                      START AGAIN                           
*                                                                               
LST22    TM    RSO,RSOCLPR                READING CLIENT/PRODUCT                
         BNO   LST38                                                            
         TM    FILTRFLG,FFLGPRD           IF ONE PRODUCT                        
         BO    LST38                      ALL DONE                              
LST23    BAS   RE,PRDL                    GET NEXT                              
         BNE   LST38                      ALL DONE                              
         B     LST19                                                            
*                                                                               
LST24    BAS   RE,FLTRK                   FILTER THE KEY                        
         BNE   LST19                      IF NO  - KEY HAS BEEN BUMPED          
*                                                                               
         TM    RSO,RSOCLPR                READING CLIENT/PRODUCT                
         BNO   LST24D                                                           
         TM    FILTRFLG,FFLGEST           FILTER ON ESTIMATE?                   
         BZ    LST24D                                                           
         CLC   KEY+DCKEST-DOKEY(L'DCKEST),ESTNUM  MATCH ON ESTIMATE?            
         BNE   LST23                                                            
*                                                                               
LST24D   CLI   BESTKEY,MNKSTYPQ           TEST MAKEGOOD KEY                     
         BE    LST25                                                            
         MVC   DAD,KEY+(MNKDSKAD-MNKEY)                                         
         MVI   RDUPDATE,C'N'                                                    
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
         GOTO1 GETREC                     GET THE RECORD NOW                    
         GOTOR CHKIO                      TEST THE IO COUNT                     
         TM    CNTL,CNTLMXIO              TEST MAX IO                           
         BO    LST35                                                            
         B     LST27                                                            
*                                                                               
LST25    GOTOR MGOFR                      FILTER MAKEGOOD AND GET ORDER         
         TM    CNTL,CNTLMXIO              TEST MAX IO                           
         BO    LST35                                                            
         CLI   MGFLAGS,0                                                        
         BE    LST31                      GET NEXT                              
*                                                                               
LST27    BAS   RE,FLTREC                  FILTER THE RECORD                     
         TM    CNTL,CNTLMXIO              TEST MAX IO                           
         BO    LST35                                                            
         TM    RECFLG,RFOK                TEST PASSED FILTER                    
         BNO   LST31                      NOT OK, GET NEXT RECORD               
*                                                                               
         TM    RSO,RSONATV                TEST NATIVE KEY                       
         BNO   LST28                                                            
         CLC   NSCR,=Y(NLISTQ)            TEST MAX ON SCREEN                    
         BNL   LST35                                                            
         B     LST30                                                            
*                                                                               
LST28    TM    RSO,RSOCLPR                TEST READ CLI/PRD                     
         BNO   LST29                                                            
         TM    FILTRFLG,FFLGPRD           SKIP IF ONE PRODUCT                   
         BO    LST30                                                            
         CLC   KCLIPRD,KEY+(DCKCLT-DOKEY)  SAME CLIENT/PRD                      
         BE    LST30                                                            
         BAS   RE,PRDL                    GET NEXT CLIENT/PROD                  
         BNE   LST38                      ALL DONE                              
         CLC   NSCR,=Y(NLISTQ)            ENOUGH FOR SCREEN                     
         BNL   LST35                      YES, GO DISPLAY                       
         B     LST19                                                            
*                                                                               
LST29    DS    0H                                                               
*&&DO                                                                           
         TM    RSO,RSO1BYR                TEST ONE BUYER AT A TIME              
         BNO   LST30                                                            
         CLC   KBUYRCD,KEY+(DSCKBYR-DOKEY) TEST SAME BUYER                      
         BE    LST30                                                            
         MVC   KBUYRCD,KEY+(DSCKBYR-DOKEY) SAVE NEW BUYER CODE                  
         CLC   NSCR,=Y(NLISTQ)             ENOUGH FOR ONE SCREEN                
         BNL   LST35                       YES, DISPLAY                         
*&&                                                                             
LST30    GOTOR TOTSAR,TSRADDQ             ADD RECORD TO TSAR BUFFER             
         SR    R0,R0                                                            
         ICM   R0,3,NSCR                  ADD TO NUMBER ON SCREEN               
         AHI   R0,1                                                             
         STCM  R0,3,NSCR                                                        
*                                                                               
         TM    CNTL,CNTLMXTS              TSAR BUFFER AT MAX                    
         BNO   LST31                                                            
         LA    R2,ORLMEDH                                                       
         GOTOR TOTSAR,TSRSAVQ                                                   
         J     TOOMANY                    'TOO MANY RECORDS TO DIS..'           
*                                                                               
LST31    CLI   BESTKEY,MNKSTYPQ           TEST USING MAKEGOOD KEY               
         BNE   LST33                                                            
         LA    R6,KEY                                                           
         USING DAREMGND,R6                                                      
         SR    RF,RF                                                            
         ICM   RF,15,MNKORDER             BUMP ORDER NUMBER                     
         AHI   RF,1                                                             
         STCM  RF,15,MNKORDER                                                   
         XC    MNKGROUP,MNKGROUP                                                
         B     LST19                      READ HIGH TO NEXT                     
         DROP  R6                                                               
*                                                                               
LST33    GOTO1 SEQ                                                              
         B     LST21                                                            
*                                         TOO MANY IOS                          
LST35    MVC   LASTKEY,KEY                SAVE NEXT KEY FOR NEXT TIME           
         OI    CNTL,CNTLMORE              TURN ON MORE TO READ                  
*                                                                               
LST38    OC    NSCR,NSCR                  ANY  NEW SCREEN ITEMS                 
         BZ    LST39                      DISPLAY SAME AS LAST                  
         SR    R1,R1                                                            
         ICM   R1,3,TSRLST                                                      
         AHI   R1,1                                                             
         STCM  R1,3,TSRFST                SET TO START AT NEXT                  
*                                                                               
LST39    GOTOR TOTSAR,TSRSAVQ             SAVE BUFFER                           
         OC    TSRTOT,TSRTOT              TEST ANY DATA                         
         BNZ   *+16                                                             
         TM    CNTL,CNTLMORE              MORE TO READ                          
         JZ    NODATAR                                                          
         B     LST41                                                            
*                                                                               
         GOTOR TOTSAR,TSRGETQ             GET RECORDS FROM TSAR                 
*                                                                               
LST41    BAS   RE,DSPLR                   DISPLAY RECORDS                       
         BAS   RE,SETPF                   SET PF KEYS                           
*                                                                               
         TM    CNTL,CNTLSAME              TEST SAME SCREEN                      
         BO    *+10                                                             
         XC    CURSOUT,CURSOUT            RESET CURSOR                          
         NI    CNTL,X'FF'-(CNTLSAME)      TURNOFF SAME TSAR RECORDS             
*                                                                               
         L     R1,ATIOB                   SET CURSOR                            
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         SR    R2,R2                                                            
         ICM   R2,3,CURSOUT               CURSOR TO A SELECTED LINE ?           
         BNZ   *+8                                                              
         LHI   R2,(ORLSEL1H-T234FFD)                                            
         STCM  R2,3,TIOBCURD                                                    
         MVI   TIOBCURI,0                                                       
         DROP  R1                                                               
*                                                                               
         TM    CNTL,CNTLMXIO              TEST MAX IO'S                         
         BO    LST45                                                            
*                                                                               
*                  SET 'RECORD(S) nn THRU nnn OF nnn DISPLAYED'                 
         XC    BLOCK(50),BLOCK                                                  
         LA    R2,BLOCK                                                         
         EDIT  (B2,TSRFST),(5,1(R2)),ALIGN=LEFT                                 
         AHI   R0,1                                                             
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         EDIT  (B2,TSRLST),(5,1(R2)),ALIGN=LEFT                                 
         AHI   R0,1                                                             
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         EDIT  (B2,TSRTOT),(5,1(R2)),ALIGN=LEFT                                 
         AHI   R0,1                                                             
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    0(2,R2),0(R2)                                                    
         J     RCNDSPLY                                                         
*                                                                               
LST45    LA    R2,LASTKEY                                                       
         USING DOKEY,R2                                                         
         MVC   FULL,DOKORDER              GET ORDER DATE                        
         CLI   BESTKEY,DOKSTYPQ           BY ORDER NUMBER                       
         BE    LST47                                                            
         MVC   FULL,DBKORD                BY BUYER/ ORDER                       
         CLI   BESTKEY,DBKSTYPQ                                                 
         JNE   MORECDS                                                          
         TM    FILTRFLG,FFLGBUYR          IF ONE BUYER                          
         JNO   MORECDS                    MORE RECORDS TO READ                  
*                                                                               
LST47    XC    BLOCK(50),BLOCK                                                  
         XC    FULL,FFS                                                         
         SR    R1,R1                                                            
         ICM   R1,3,FULL                                                        
         CVD   R1,DUB                                                           
         ZAP   FULL,DUB                                                         
         SRP   DUB,64-3,0                                                       
         AP    DUB,=P'90'                                                       
         SRP   DUB,3,0                                                          
         AP    DUB,FULL+2(2)                                                    
         ZAP   FULL,DUB                                                         
         GOTO1 DATCON,DMCB,(6,FULL),(10,BLOCK+1)                                
         MVI   BLOCK,9                                                          
         J     RDBKDSPL                   READ BACK TO MM/DD/YY                 
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SELECT THE BEST KEY                                      *         
***********************************************************************         
SLCTK    MVI   BESTKEY,0                  SELECT BEST DIRECTORY KEY             
         MVI   FKFLG,0                    FILTER KEY CNTLS                      
         MVI   CNTL,0                     RESET CONTROL                         
         MVI   RSO,0                      READ OPTIONS                          
         XC    KCLIPRD,KCLIPRD            CLEAR CLIENT/PRODUCT                  
         TM    FLTRFLG2,FF2AOFFR          FILTERING ON ACTIVE OFFERS            
         BZ    *+12                                                             
         MVI   BESTKEY,MNKSTYPQ           USE MAKEGOOD NOTICE RECORD            
         B     SLCTK30                                                          
*                                                                               
         TM    FILTRFLG,FFLGORD           FILTER ON ORDER # ?                   
         BZ    SLCTK10                                                          
         CLI   FLTORDL,3                  AT LEAST 3 BYTES                      
         BL    SLCTK10                                                          
         MVI   BESTKEY,DOKSTYPQ           USE ORDER NUMBER-KEY                  
         B     SLCTK30                                                          
*                                                                               
SLCTK10  CLI   SSQ,C'D'                   CLIENT/PROD SEQUENCE ?                
         BNE   SLCTK30                                                          
*                                                                               
         TM    FILTRFLG,FFLGCLT+FFLGPRD   CLIENT+PRODUCT FILTER?                
         BO    SLCTK30                    - BOTH, USE CLIENT-KEY                
*                                                                               
         TM    FILTRFLG,FFLGBUYR+FFLGSTA  BUYER+STATION FILTER?                 
         BZ    SLCTK30                    - NEITHER, USE CLIENT-KEY             
         BNO   SLCTK20                    - JUST ONE                            
         MVI   BESTKEY,DSKSTYPQ           - BOTH, USE STATION-KEY               
         B     SLCTK30                                                          
*                                                                               
SLCTK20  TM    FILTRFLG,FFLGCLT           CLIENT FILTER?                        
         BO    SLCTK30                    YES, USE CLIENT-KEY                   
         TM    FILTRFLG,FFLGBUYR          BUYER FILTER?                         
         BZ    SLCTK30                    NO, USE CLIENT-KEY                    
         MVI   BESTKEY,DBKSTYPQ           YES, USE BUYER-KEY                    
*                                                                               
SLCTK30  SR    R0,R0                                                            
         L     R6,ADSEQTAB                GET KEY FROM SEQUENCE                 
         USING SSQD,R6                                                          
SLCTK40  CLI   0(R6),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SSQ,SSQCDE                 MATCH SEQUENCE                        
         BE    SLCTK50                                                          
         IC    R0,SSQLN                                                         
         AR    R6,R0                                                            
         B     SLCTK40                                                          
*                                                                               
SLCTK50  CLI   BESTKEY,0                  ANY KEY YET ?                         
         BNER  RE                                                               
*                                                                               
         MVC   BESTKEY,SSQBKY                                                   
         CLC   BESTKEY,SSQBKY                                                   
         BNE   *+16                                                             
         TM    SSQFLG,SSQFNATV            IS IT A NATIVE SEQUENCE ?             
         BNO   *+8                                                              
         OI    RSO,RSONATV                SET NATIVE KEY SEQUENCE               
*                                                                               
         TM    SSQFLG,SSQFCLPR            ONE CLI/PRD AT A TIME ?               
         BNO   *+8                                                              
         OI    RSO,RSOCLPR                SET NATIVE KEY SEQUENCE               
*                                                                               
         TM    SSQFLG,SSQFBUYR            IS IT A BUYER KEY?                    
         BNOR  RE                                                               
         CLI   NBYR,1                     IS THERE A LIST ?                     
         BH    *+10                       YES                                   
         TM    FILTRFLG,FFLGBUYR          FILTER ON BUYER ?                     
         BOR   RE                                                               
         OI    RSO,RSO1BYR                SET ONE BUYER AT A TIME               
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY RECORD                                           *         
***********************************************************************         
DSPLR    NTR1  ,                                                                
         LA    R2,ORLSEL1H                R2=A(SCREEN LINE)                     
         USING LINDSECT,R2                                                      
         LA    R3,LSTTAB                  R3=A(LIST OF ORDERS)                  
         USING LSTD,R3                                                          
         SR    R4,R4                                                            
         ICM   R4,1,NLST                  R4=NUMBER IN LIST                     
         JZ    XIT                                                              
         LA    R0,NLISTQ                  R0=MAX ON SCREEN                      
         CR    R4,R0                                                            
         BNH   *+6                                                              
         LR    R4,R0                                                            
*                                                                               
DSPLR01  MVC   BINORDER,LSTORD            SHOW THE ORDER NUMBER                 
         BAS   RE,SHWORDER                                                      
         MVC   LINORD(L'CHRORDER),CHRORDER                                      
*                                                                               
         OC    LSTODT,LSTODT                                                    
         BZ    DSPLR05                                                          
         GOTO1 DATCON,DMCB,(8,LSTODT),(7,LINODT)                                
*                                                                               
DSPLR05  MVC   BCLT,LSTCLT                CLIENT PRODUCT                        
         MVC   BPRD,LSTPRD                                                      
         BAS   RE,GETQCP                                                        
*                                                                               
         TM    FILTRFLG,FFLGCLT   CLIENT FILTER                                 
         BO    DSPLR07                                                          
         L     R6,AIO                     AIO = A(CLIENT RECORD)                
         USING CLTHDRD,R6                                                       
         MVC   SVCPROF,CPROF                                                    
         MVC   CLTOFFCE,COFFICE                                                 
DSPLR07  GOTO1 CLUNPK,DMCB,(SVCPROF+6,BCLT),LINCLT                              
*                                                                               
* GET OM PROFILE!!                                                              
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0OM'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),LINCLT                                                 
*                                                                               
         CLI   CLTOFFCE,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         L     R1,ACOMFACS         RF = A(GETPROF)                              
         L     RF,CGETPROF-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,WORK,PROFOM,DATAMGR                                    
         DROP  R6                                                               
*                                                                               
         MVC   LINPRD(3),=C'***'                                                
         CLI   BPRD,X'FF'                                                       
         BE    *+10                                                             
         MVC   LINPRD(L'QPRD),QPRD                                              
*                                                                               
         CLI   LSTPR2,0                   TEST PIGGYBACK                        
         BE    DSPLR10                                                          
         MVC   BPRD,LSTPR2                                                      
         BAS   RE,GETQCP                                                        
         MVI   LINPRD+3,C'-'                                                    
         MVC   LINPRD+4(L'QPRD),QPRD                                            
*                                                                               
DSPLR10  MVC   LINSTC,LSTSTC              STATUS CODE                           
*                                                                               
         MVC   LINBYR,LSTBYR              BUYER                                 
         OC    LINBYR,SPACES                                                    
         EDIT  (B1,LSTEST),(3,LINEST),FILL=0                                    
*                                                                               
         MVC   QCLT,LINCLT                                                      
         GOTO1 GETMKT,LSTSTA                                                    
         MVC   LSTMKT,BMKT                MARKET NUMBER TO LIST                 
         MVC   LINMKN,MKTNM               MARKET NAME TO SCREEN                 
*                                                                               
         MVC   LINSTA,QSTA                                                      
         TM    MISCFLG3,MF3RADIO   ARE WE RADIO?                                
         BNZ   *+8                                                              
         MVI   LINSTA+4,C' '       NO, REMOVE THE C'T' FOR TV                   
*                                                                               
         CLI   LINSTA+3,C'-'                                                    
         BNE   *+8                                                              
         MVI   LINSTA+3,C' '                                                    
         OI    LINSTAH+(FLDIIND-FLDHDRD),FINPALF  MAKE ALPHA                    
         MVI   LINSTAH+(FLDILEN-FLDHDRD),4        INPUT LENGTH                  
         CLI   LINSTA+4,C' '                      TEST BAND                     
         BL    *+8                                                              
         MVI   LINSTAH+(FLDILEN-FLDHDRD),5        INPUT LENGTH                  
*                                                                               
         TM    OPTNFLG,OPTNDAD                    DISPLAY DISK ADDRESS          
         BNO   DSPLR15                                                          
         MVC   LINMKN,SPACES                      USE MARKET NAME AREA          
         GOTO1 HEXOUT,DMCB,LSTDAD,LINMKN,4,0,0                                  
*                                                                               
DSPLR15  MVC   LINCLR,LSTCLR                      SET COLOR                     
         CLI   TWAOFFC,C'*'               TEST DDS TERMINAL                     
         BNE   *+8                                                              
         MVI   LINCLRH+(FLDATB-FLDHDRD),FATBPROT                                
*                                                                               
         CLI   LINCLR,C'R'              IF FXDLVD, DISPLAY YELLOW COLOR         
         BNE   DSPLR20                  BUT STILL GROUPED W/RED                 
         CLC   LINSTC,=C'FXDLVD'                                                
         BE    DSPLR17                                                          
         CLC   LINSTC,=C'EMDLVD'        OR EMDLVD TOO                           
         BE    DSPLR17                                                          
         CLC   LINSTC(2),=C'**'         OR PARTIAL CONFIRMS                     
         BNE   DSPLR20                                                          
DSPLR17  MVI   LINCLR,C'Y'                                                      
*                                                                               
DSPLR20  CLI   LSTFLT,0                   FLIGHT                                
         BE    DSPLR25                                                          
         EDIT  (B1,LSTFLT),(2,LINFLT),FILL=0                                    
*                                                                               
DSPLR25  TM    LSTFLG,LSTFSTR             TEST DISPLAY '*'                      
         BNO   *+8                                                              
         MVI   LINSTT1,C'*'                                                     
         TM    LSTFLG,LSTFMGO             TEST MG OFFER                         
         BNO   *+8                                                              
         MVI   LINOFI,C'X'                                                      
*        MVI   LINORD+8,C' '                                                    
         TM    LSTFLG,LSTTRDE             TEST TRDAE                            
         BNO   *+8                                                              
         MVI   LINORD+8,C'T'                                                    
*                                                                               
         XC    LSTACT,FFS                 REVERSE THE BITS                      
         CLC   LSTACT,FFS                                                       
         BE    DSPLR30                                                          
*                                                                               
         OC    LSTACT,LSTACT                                                    
         BZ    DSPLR30                                                          
         GOTO1 DATCON,DMCB,(8,LSTACT),(7,LINADT)                                
*                                                                               
DSPLR30  MVC   DMDSKADD,LSTDAD            SET DISK ADDRESS                      
         GOTO1 LISTMON                                                          
         LA    R2,LINNEXTL                                                      
         LA    R3,LSTLNQ(R3)                                                    
         BCT   R4,DSPLR01                                                       
         J     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO BUILD FIRST KEY FOR READ HIGH                           *         
***********************************************************************         
FSTK     NTR1  ,                                                                
         XC    KEY,KEY                    SET KEY                               
         LA    R2,KEY                                                           
         USING DAREORDD,R2                                                      
         MVI   DOKTYPE,DOKTYPQ            TYPE                                  
         MVC   DOKSUBTY,BESTKEY           SUB-TYPE                              
         MVC   KYMSK,KEY                  KEY MASK FOR FILTER DATA              
*                                                                               
         L     R3,AKYDAT                                                        
         USING DKYD,R3                                                          
         SR    R0,R0                                                            
         IC    R0,DKYNUM                  R0=NUMBER OF KEY FIELDS               
         LA    R3,DKYDATA                                                       
         USING DKYDATA,R3                                                       
         LA    R4,L'DOKTYPE+L'DOKSUBTY    R4=DEFAULT LENGTH                     
         CLI   BESTKEY,DOKSTYPQ           USING PRIMARY KEY?                    
         BNE   *+8                        NO                                    
         LA    R4,DOKAGMD-DOKEY           YES, SET R4 TO CORRECT LENGTH         
         B     FSTK30                                                           
*                                                                               
FSTK20   LA    R3,DKYLNQ(R3)                                                    
FSTK30   MVC   KYFLDLN,DKYLEN             SET LENGTH OF KEY FIELD               
         GOTOR SETKFLD,DMCB,KYMSK,DKYDATA SET FILTER FIELDS IN MASK             
         BNE   FSTK50                     NO KEY DATA                           
         SR    RE,RE                                                            
         ICM   RE,1,KYFLDLN               ANY LENGTH OVERRIDE?                  
         AR    R4,RE                                                            
         CLC   DKYLEN,KYFLDLN             LENGTH OF KEY FIELD CHANGE?           
         BNE   FSTK50                     NO, OK TO CONTINUE                    
         BCT   R0,FSTK20                                                        
*                                                                               
FSTK50   STC   R4,KYLN                    SAVE LENGTH OF MINIMUM KEY            
*                                                                               
         SR    R1,R1                                                            
         IC    R1,KYLN                    MINIMUM KEY LENGTH                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY(0),KYMSK               SET START KEY FROM MASK               
*                                                                               
         J     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO SET KEY FIELDS WITH FILTER DATA                         *         
*  PARM 1  =    A(KEY)                                                *         
*       2  =    A(DKYDATA ENTRY)                                      *         
***********************************************************************         
SETKFLD  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING DKYDATA,R3                                                       
         SR    RF,RF                                                            
         IC    RF,DKYDSP                                                        
         AR    R2,RF                      R2=A(DEST. FIELD IN KEY)              
         SR    R1,R1                                                            
         IC    R1,DKYLEN                                                        
         BCTR  R1,0                       R1=LENGTH FOR EX OF DEST.             
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,DKYSRC                SOURCE NUMBER                         
         JZ    NO                         NO FILTER ON THIS FIELD               
         MHI   R4,FDLNQ                                                         
         A     R4,ADFTAB                  R4=A(FILTER DEFINITION)               
*                                                                               
         USING FDD,R4                                                           
         SR    RF,RF                                                            
         ICM   RF,3,FDKSET                SPECIAL SET KEY ROUTINE               
         BZ    SETKFLD3                                                         
         AR    RF,RB                                                            
         BASR  RE,RF                      LET SPECIAL ROUTINE HANDLE            
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R2),0(R2)              TEST ANY DATA SET                     
         JZ    NO                                                               
         J     YES                                                              
*                                                                               
SETKFLD3 SR    R6,R6                                                            
         ICM   R6,1,FDFLBIT               IS THERE A FLAG BIT TO TEST?          
         BZ    SETKFLD5                                                         
         SR    RF,RF                                                            
         ICM   RF,3,FDFLFLD               GET DISP. TO FLAG FIELD               
         LA    RF,LSSD(RF)                                                      
         EX    R6,*+8                     TEST FILTER DATA INPUT                
         B     *+8                                                              
         TM    0(RF),0                                                          
         JNO   NO                         NO FILTER DATA                        
*                                                                               
SETKFLD5 SR    RF,RF                                                            
         ICM   RF,3,FDSRC                                                       
         JZ    NO                                                               
         LA    RF,LSSD(RF)                LOCAL AREA                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)              MOVE FILTER DATA TO KEY               
         J     YES                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*  FIRST TIME SPECIAL HOOK ROUTINES                                   *         
*  AT NTRY R1 = LENGTH OF KEY FIELD FIELD (LESS ONE)                  *         
*          R2=  A(KEY FIELD)                                          *         
***********************************************************************         
                                                                                
*   SET BINARY ORDER# FIELD                                                     
*                                                                               
SETKORD  NTR1  ,                                                                
         TM    FILTRFLG,FFLGORD           TEST FILTER BY ORDER #                
         BNO   SETKORD3                   BRANCH IF NO OREDER # FILTER          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),FLTORDBN           ADD ORDER # TO KEY                    
         CLI   FLTORDBL,4                 FULL BINARY NUMBER                    
         JE    XIT                                                              
         MVC   KYFLDLN,FLTORDBL           SET LENGTH OVERRIDE                   
         J     XIT                                                              
*                                                                               
SETKORD3 CLI   ENDDATE,0                  END DATE FILTER                       
         JE    XIT                                                              
         MVC   0(2,R2),ENDDATE            ADD END DATE TO KEY                   
         XC    2(2,R2),2(R2)              CLEAR SEQUENCE                        
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO FILTER KEY                                              *         
*   IF KEY DOES NOT PASS FILTER - SET KEY FOR NEXT READ HIGH          *         
***********************************************************************         
FLTRK    NTR1  ,                                                                
         NI    FKFLG,X'FF'-(FKFSET)       FILTER KEY CNTLS                      
         L     R3,AKYDAT                  R3=A(KEY DATA)                        
         USING DKYD,R3                                                          
         SR    R0,R0                                                            
         IC    R0,DKYNUM                  R0=NUMBER OF KEY FIELDS               
         LA    R3,DKYDATA                                                       
         USING DKYDATA,R3                                                       
*                                                                               
FLTRK3   SR    R1,R1                                                            
         IC    R1,DKYLEN                  R1=LENGTH OF KEY FIELD                
         BCTR  R1,0                                                             
         SR    R2,R2                                                            
         IC    R2,DKYDSP                  DISPLACEMENT TO DATA                  
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,DKYSRC                TEST SOURCE NUMBER                    
         BZ    FLTRK9                     NONE, FIELD IS OK                     
         MHI   R4,FDLNQ                                                         
         A     R4,ADFTAB                  R4=A(FILTER DATA)                     
         USING FDD,R4                                                           
         SR    RF,RF                                                            
         ICM   RF,3,FDKFLT                SPECIAL COMPARE ROUTINE               
         BZ    FLTRK5                                                           
         LA    R2,KEY(R2)                                                       
         AR    RF,RB                                                            
         BASR  RE,RF                      LET SPECIAL ROUTINE HANDLE            
         BE    FLTRK9                     OK, TO KEEP THIS KEY                  
         TM    FKFLG,FKFSET               KEY SET BY ROUTINE ?                  
         JO    NO                         YES, EXIT NOW                         
         B     FLTRK17                    SKIP TO NEXT                          
         DROP  R4                                                               
*                                                                               
FLTRK5   LA    RE,KYMSK(R2)                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)              TEST ANY DATA IN MASK                 
         BZ    FLTRK9                     NO, THIS FIELD IS OK                  
         LA    R6,KEY(R2)                                                       
         EX    R1,FLTRKCMP                CLC KEY FIELD,KYMSK FIELD             
         BE    FLTRK9                                                           
         BH    FLTRK11                    KEY IS HIGHER                         
         B     FLTRK13                    KEY IS LOWER                          
*                                                                               
FLTRK9   LA    R3,DKYLNQ(R3)              CHECK NEXT KEY FIELD                  
         BCT   R0,FLTRK3                                                        
         J     YES                        SET KEY OK                            
*                                                                               
FLTRK11  LA    RE,FFS                     SET FIELD TO FF'S                     
         CLI   0(R6),X'FF'                UNLESS IT IS FF'S                     
         JE    YES                        READ SEQUENTIAL                       
FLTRK13  EX    R1,*+8                     MOVE FILTER DATA TO KEY               
         B     *+10                                                             
         MVC   0(0,R6),0(RE)                                                    
         B     FLTRK17                                                          
*                                                                               
FLTRK15  SR    R1,R1                                                            
         IC    R1,DKYLEN                  CLEAR REMAINDER OF FIELDS             
         BCTR  R1,0                                                             
         SR    R2,R2                                                            
         IC    R2,DKYDSP                  DISPLACEMENT TO DATA                  
         LA    R6,KEY(R2)                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R6),0(R6)              CLEAR KEY FIELD                       
*                                                                               
FLTRK17  LA    R3,DKYLNQ(R3)                                                    
         BCT   R0,FLTRK15                                                       
         J     NO                                                               
*                                                                               
FLTRKCMP CLC   0(0,R6),0(RE)              TEST KEY FIELD EQUAL MASK             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*  HOOKS FOR FITERING SPECIAL KEY FIELDS                              *         
*  AT NTRY R1 = LENGTH OF KEY FIELD FIELD (LESS ONE)                  *         
*          R2=  A(KEY FIELD)                                          *         
***********************************************************************         
                                                                                
*                                                                               
*   FILTER BINARY ORDER# FIELD                                                  
*                                                                               
FLTKORD  NTR1  ,                                                                
         TM    FILTRFLG,FFLGORD           TEST FILTER BY ORDER #                
         BNO   FLTKORD5                   NO - TRY DATE                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),FLTORDBN           TEST KEY TO LOW KEY                   
         JE    YES                        IF IT'S EQUAL - KEEP IT               
         BH    FLTKORD3                                                         
         EX    R1,*+8                     IF IT'S LOW - SET TO START            
         B     *+10                                                             
         MVC   0(0,R2),FLTORDBN           ADD ORDER # TO KEY                    
         J     NO                                                               
*                                                                               
FLTKORD3 EX    R1,*+8                     TEST KEY TO HIGH ORDER NUMBER         
         B     *+10                                                             
         CLC   0(0,R2),FLTORDBH                                                 
         JNH   YES                        IF NOT HIGH - KEEP IT                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),FFS                ELSE BUMP PAST THIS SEQUENCE          
         J     NO                                                               
*                                                                               
FLTKORD5 CLI   ENDDATE,0                  END DATE FILTER                       
         JE    YES                        NO, KEEP THIS KEY                     
         CLC   0(2,R2),ENDDATE            TEST KEY TO LOW KEY                   
         JNL   YES                        IF IT'S EQUAL - KEEP IT               
         MVC   0(2,R2),ENDDATE            BUMP TO ENDDATE                       
         XC    2(2,R2),2(R2)              CLEAR SEQUENCE                        
         J     NO                                                               
                                                                                
*  COLOR FIELD                                                                  
*                                                                               
FLTKCLR  NTR1  ,                                                                
         SR    RF,RF                                                            
         ICM   RF,1,FLTRCLR               TEST COLOR FILTER                     
         JZ    YES                        NO FILTER, TAKE THEM ALL              
         L     R3,ADCLRTAB                                                      
FLTKCLR3 CLC   0(1,R2),0(R3)              MATCH FIELD TO COLOR TABLE            
         BE    FLTKCLR5                                                         
         LA    R3,L'CLRTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   FLTKCLR3                                                         
         B     FLTKCLR7                   NOT IN TABLE - SKIP COLOR             
*                                                                               
FLTKCLR5 EX    RF,*+8                                                           
         B     *+8                                                              
         TM    1(R3),0                    TEST COLOR SEQUENCE                   
         JNZ   YES                        MATCHED FILTER, KEEP IT               
FLTKCLR7 IC    RF,0(R2)                   RF=COLOR CODE                         
         AHI   RF,1                                                             
         STC   RF,0(R2)                   BUMP TO NEXT COLOR                    
         J     NO                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER RECORD                                            *         
*  ON ENTRY: AIO CONTAINS RECORD                                      *         
*                                                                     *         
*  ON EXIT:  YES: LSTTAB CONTAINS ONE ENTRY (LSTD DSECT)              *         
*            NO: DID NOT PASS FILTER REQS                             *         
*                                                                     *         
***********************************************************************         
FLTREC   NTR1  ,                                                                
         LA    R2,LSTTAB                  R2=A(LIST TABLE)                      
         USING LSTD,R2                                                          
         XC    LSTD(LSTLNQ),LSTD          CLEAR FIRST SPOT                      
*                                                                               
         MVI   RECFLG,0                                                         
         XC    CLRDATE,CLRDATE            CLEAR COLOR DATE                      
         XC    ADOXMETL,ADOXMETL                                                
         NI    MISCFLG4,X'FF'-MF4CNFCM-MF4AMEND                                 
         NI    MISCFLG1,X'FF'-MF1XDOSP                                          
         NI    MISCFLG2,X'FF'-MF2VAROR-MF2REVOR-MF2OFFER                        
         NI    MISCFLG2,X'FF'-MF2FAXED-MF2EMAIL-MF2CANCF                        
*                                                                               
         L     R6,AIO                                                           
         USING DOKEY,R6                                                         
*                                                                               
         CLI   DOKCMT,0                   SKIP ALL TYPES EXCEPT 0               
         JNE   NO                                                               
*                                                                               
         TM    FILTRFLG,FFLGSDAT          ANY START DATE?                       
         BZ    FLTRC010                                                         
         CLC   DOKORDER(L'FILTRSDT),FILTRSDT                                    
         JH    NO                                                               
*                                                                               
FLTRC010 MVC   LSTORD,DOKORDER            SAVE ORDER NUMBER                     
         OC    ENDDATE,ENDDATE            ANY END DATE?                         
         BZ    FLTRC020                                                         
         CLC   DOKORDER(L'ENDDATE),ENDDATE                                      
         JL    NO                                                               
*                                                                               
FLTRC020 TM    FILTRFLG,FFLGORD           ORDER NUMBER?                         
         BZ    FLTRC030                                                         
         MVC   BINORDER,DOKORDER                                                
         BAS   RE,SHWORDER                CONVERT ORDER TO CHARACTER            
         SR    R1,R1                                                            
         IC    R1,FLTORDL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CHRORDER(0),FLTORD         WE HAVE A MATCH?                      
         JNE   NO                                                               
*                                                                               
FLTRC030 MVC   LSTSTA,DOKSTA              STATION CODE                          
         TM    FILTRFLG,FFLGSTA                                                 
         BZ    FLTRC040                                                         
         CLC   LSTSTA,FLTSTA                                                    
         JNE   NO                                                               
*                                                                               
FLTRC040 MVI   ELCODE,DOIDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
*                                                                               
         USING DOIDELD,R6                                                       
         MVC   LSTBYR,DOIDBYR             BUYER                                 
         OC    LSTBYR,SPACES                                                    
         MVC   LSTCLT,DOIDCLT             CLIENT                                
         MVC   LSTPRD,DOIDPRD             PRODUCT                               
         MVC   LSTPR2,DOIDPRD2            SECOND PRODUCT                        
         MVC   LSTEST,DOIDEST             ESTIMATE                              
         MVC   LSTFLT,DOIDFLTN            FLIGHT                                
         DROP  R6                                                               
*                                                                               
         TM    FILTRFLG,FFLGBUYR          FILTERING ON BUYER                    
         BZ    FLTRC050                                                         
         SR    R0,R0                                                            
         ICM   R0,1,NBYR                  ANY BUYERS ?                          
         JZ    NO                                                               
         LA    R1,BUYRL                                                         
         CLC   LSTBYR,0(R1)               TEST ASS'T BUYER CODES                
         BE    FLTRC050                                                         
         LA    R1,3(R1)                                                         
         BCT   R0,*-14                                                          
         J     NO                                                               
*                                                                               
FLTRC050 TM    FILTRFLG,FFLGCLT           FILTER ON CLIENT?                     
         BZ    FLTRC060                                                         
         CLC   FLTCLT,LSTCLT                                                    
         JNE   NO                                                               
*                                                                               
FLTRC060 TM    FILTRFLG,FFLGPRD           FILTER ON PRODUCT?                    
         BZ    FLTRC070                                                         
         CLC   FLTPRD,LSTPRD                                                    
         BNE   *+14                                                             
         CLC   FLTPR2,LSTPR2                                                    
         BE    FLTRC070                                                         
         CLC   FLTPR2,LSTPRD                                                    
         JNE   NO                                                               
         CLC   FLTPRD,LSTPR2                                                    
         JNE   NO                                                               
*                                                                               
FLTRC070 TM    FILTRFLG,FFLGEST           FILTER ON ESTIMATE?                   
         BZ    FLTRC080                                                         
         CLC   LSTEST,ESTNUM                                                    
         JNE   NO                                                               
*                                                                               
FLTRC080 CLI   EDATEFLG,0                 ESTIMATE RANGE FILTER  ?              
         BE    FLTRC090                                                         
         LA    RF,ESTTAB                                                        
         ZIC   RE,LSTEST                                                        
         LA    RE,0(RE,RF)                POINT TO N'TH ENTRY                   
         CLC   LSTEST,0(RE)               VALID ESTIMATE?                       
         JNE   NO                                                               
*                                                                               
FLTRC090 L     R6,AIO                                                           
         MVI   ELCODE,DOI2ELQ             SECONDARY ELEMENT(POL)                
         BRAS  RE,GETEL                                                         
         BNE   FLTRC100                                                         
*                                                                               
         USING DOI2ELD,R6                                                       
         TM    DOI2FLG1,DOI2FVAR          VAR ORDER YET?                        
         BZ    *+8                                                              
         OI    MISCFLG2,MF2VAROR          YES                                   
*                                                                               
FLTRC100 TM    FLTRFLG2,FF2PORDR          FILTERING ON POOL ORDER?              
         BZ    FLTRC120                   NO - SKIP FILTER LOGIC                
         CLI   0(R6),DOI2ELQ           DID WE GET SECONDARY ELEMENT B4?         
         JNE   NO                         NO POL ELEMENT                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,FLTPORBL                                                    
         BZ    FLTRC110                   NO, THEN TEST EBCDIC                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DOI2OORD(0),FLTPORBN       HEX BYTES ARE THE SAME?               
         JNE   NO                                                               
*                                                                               
FLTRC110 MVC   BINORDER,DOI2OORD                                                
         BAS   RE,SHWORDER                CONVERT ORDER TO CHARACTER            
         SR    R1,R1                                                            
         IC    R1,FLTPORDL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CHRORDER(0),FLTPORD        WE HAVE A MATCH?                      
         JNE   NO                                                               
         DROP  R6                                                               
*                                                                               
FLTRC120 L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   FLTRC150                                                         
         USING DOSPELD,R6                                                       
*                                                                               
         TM    FLTRFLG3,FF3CSWTC   CALL LETTER SWITCH?                          
         BZ    *+12                                                             
         TM    DOSPDKYF,DOSPFCCD+DOSPFNSN+DOSPFDN2                              
         JZ    NO                                                               
*                                                                               
         TM    FLTRFLG2,FF2MKT     MARKET FILTER?                               
         BZ    FLTRC125                                                         
         OC    DOSPMKT,DOSPMKT     MARKET EXIST IN THE RECORD?                  
         BZ    FLTRC125                                                         
         CLC   DOSPMKT,MKTNUM      MATCH ON MARKET?                             
         JNE   NO                                                               
*                                                                               
FLTRC125 TM    DOSPFLG1,DOSPTRDE          TRADE ORDER ?                         
         BZ    *+8                                                              
         OI    LSTFLG,LSTTRDE                                                   
*                                                                               
         CLI   DOSPREVN,0                 GOT A REVISION NUMBER?                
         BE    *+8                                                              
         OI    MISCFLG2,MF2REVOR          YES, REVISED ORDER                    
*                                                                               
         CLI   DOSPMTHD,0          WAS ANYTHING EVER ENTERED?                   
         BNE   FLTRC140                                                         
FLTRC130 OI    MISCFLG1,MF1XDOSP                                                
*                                                                               
FLTRC140 CLI   DOSPMTHD,C'F'       WAS I PREVIOUSLY FAXED?                      
         BNE   *+8                                                              
         OI    MISCFLG2,MF2FAXED                                                
*                                                                               
         CLI   DOSPMTHD,C'E'       WAS I PREVIOUSLY EMAILED?                    
         BNE   *+8                                                              
         OI    MISCFLG2,MF2EMAIL                                                
         DROP  R6                                                               
*                                                                               
FLTRC150 TM    FLTRFLG2,FF2REVSD          FILTERING FOR REVISED ORDERS?         
         BZ    *+12                                                             
         TM    MISCFLG2,MF2REVOR          IS THIS ORDER REVISED?                
         JZ    NO                                                               
*                                                                               
         MVI   LSTCLR,C'K'                DEFAULT IS BLACK                      
         CLI   BESTKEY,DSCKSTYQ           COLOR KEY ?                           
         BNE   *+10                                                             
         MVC   LSTCLR,KEY+(DSCKSTAT-DOKEY) GET CLR FROM KEY & DATE              
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,COLELQ              FIND COLOR ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   FLTRC160                                                         
         USING COLOREL,R6                                                       
         MVC   LSTCLR,COLCOL              SAVE THE COLOR                        
         MVC   HALF,COLDATE               AND COLOR DATE                        
         XC    HALF,FFS                   COLDATE IS FF'S COMP                  
         GOTO1 DATCON,DMCB,(2,HALF),(19,CLRDATE)                                
*                                                                               
FLTRC160 L     R6,AIO                                                           
         MVI   ELCODE,MGCOLELQ            MAKEGOOD COLOR ELEMENT                
         BRAS  RE,GETEL                                                         
         BNE   FLTRC170                                                         
         USING MGCOLELD,R6                                                      
         OI    LSTFLG,LSTFMGO              SET MG FLAG                          
         OI    MISCFLG2,MF2OFFER                                                
*                                                                               
FLTRC170 L     R6,AIO                                                           
         MVI   BYTE,QUNSENT        ASSUME 'UNSENT'                              
         MVI   ELCODE,DOSTELQ      FIND STATUS ELEMENT                          
         BRAS  RE,GETEL                                                         
         BNE   FLTRC300                                                         
         OI    RECFLG,RFSTEL       HAVE STATUS ELEMENT                          
         USING DOSTELD,R6                                                       
         CLI   DOSTSTAT,DDLVRD     SKIP DELIVRED STATUS?                        
         BNE   FLTRC180                                                         
         OI    RECFLG,RFDLDT       HAVE DELIVERED DATE & TIME                   
         MVC   LSTACT,DOSTDATE                                                  
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               BUMP TO NEXT DOSTELEM                        
*                                                                               
FLTRC180 ST    R6,ADOSTETL         SAVE ADDRESS OF STATUS ELEMENT               
         TM    RECFLG,RFDLDT       DID WE HAVE A DELNOT?                        
         BNZ   *+10                                                             
         MVC   LSTACT,DOSTDATE                                                  
*                                                                               
         CLC   LSTACT,CLRDATE      LAST ACTIVITY VS. COLOR                      
         BH    *+10                                                             
         MVC   LSTACT,CLRDATE                                                   
*                                                                               
         CLI   DOSTSTAT,DSENT      AM I SENT?                                   
         BNE   FLTRC190                                                         
         MVC   LSTODT,DOSTDATE     SAVE THIS DATE                               
         TM    RECFLG,RFDLDT       HAVE DELIVERED DATE & TIME??                 
         BNZ   FLTRC290            YES                                          
         OI    LSTFLG,LSTFSTR      DISPLAY A '*'                                
         B     FLTRC290                                                         
***************                                                                 
* SPECIAL CODE FOR CONFIRMED STATUS                                             
***************                                                                 
FLTRC190 CLI   DOSTSTAT,QCFMD      CONFIRMED STATUS?                            
         BNE   FLTRC210            NO                                           
         CLI   DOSTLEN,DOSTLNQ3    IS THERE AN TYPE FIELD?                      
         BNE   FLTRC200            NO, THEN MUST CHECK DOSPELEM                 
         TM    DOSTTYPE,DCNFMCOM   CONFIRM WITH COMMENTS?                       
         BZ    *+8                                                              
         OI    MISCFLG4,MF4CNFCM   YES!!                                        
         TM    DOSTTYPE,DCNFMCAN   CANCELLED CONFIRM?                           
         BZ    *+8                                                              
         OI    MISCFLG2,MF2CANCF   YES!!                                        
         B     FLTRC28F                                                         
*                                                                               
FLTRC200 L     R6,AIO              CHECK DOSPELEM FOR CNFM W/COMMENTS           
         MVI   ELCODE,DOSPELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   FLTRC28F                                                         
         USING DOSPELD,R6                                                       
         TM    DOSPFLG1,DOSPCFCM   CONFIRM WITH COMMENT?                        
         BZ    *+8                                                              
         OI    MISCFLG4,MF4CNFCM   YES                                          
         L     R6,ADOSTETL         RESTORE R6                                   
*                                                                               
FLTRC28F TM    FLTRFLG2,FF2PCNFM          FILTER ON PARTIAL CONFIRM?            
         BZ    FLTRC240                                                         
         TM    MISCFLG4,MF4CNFCM          YES, IS IT A PARTIAL CONFIRM?         
         JZ    NO                         NO                                    
         B     FLTRC240                                                         
***************                                                                 
* SPECIAL CODE FOR CONFIRMED STATUS (END)                                       
*================================================                               
* SPECIAL CODE FOR REJECT/AMEND STATUS (START)                                  
***************                                                                 
         USING DOSTELD,R6                                                       
FLTRC210 CLI   DOSTSTAT,QRJCT      AM I REJECT?                                 
         BNE   FLTRC220                                                         
         CLI   DOSTLEN,DOSTLNQ3                                                 
         BNE   *+8                                                              
         OI    MISCFLG4,MF4AMEND                                                
*                                                                               
         TM    FLTRFLG3,FF3AMEND                                                
         BZ    FLTRC240                                                         
         TM    MISCFLG4,MF4AMEND                                                
         JZ    NO                                                               
         B     FLTRC240                                                         
*                                                                               
FLTRC220 TM    FLTRFLG3,FF3AMEND+FF3REJCT  FILTERING FOR AMEND/REJECT?          
         JNZ   NO                         YES, SKIP                             
***************                                                                 
* SPECIAL CODE FOR REJECT/AMEND STATUS (END)                                    
*================================================                               
* SPECIAL CODE FOR RECALL STATUS                                                
***************                                                                 
FLTRC230 MVI   BYTE,RFRLDLV        SET FOR 'RECALL DELIVERED'                   
         TM    RECFLG,RFDLDT       HAVE DELIVERED DATE & TIME??                 
         BNZ   *+8                                                              
         MVI   BYTE,RFRLNDL        SET 'RECALL NOT DELIVERED'                   
         CLI   DOSTSTAT,QRECALL    IT IT A RECALL ?                             
         BNE   FLTRC240                                                         
         OC    RECFLG,BYTE         SET RECALL BIT                               
* SPECIAL CODE FOR RECALL STATUS (END)                                          
***************                                                                 
FLTRC240 CLI   DOSTSTAT,DSENT      AM I SENT?                                   
         BE    FLTRC270                                                         
*                                                                               
         CLI   DOSTSTAT,DEMSENT    AM I EMAIL?                                  
         BNE   FLTRC250                                                         
         TM    MISCFLG1,MF1XDOSP   DID I FIND THE METHOD IN DOSPELEM?           
         BZ    FLTRC270                                                         
         OI    MISCFLG2,MF2EMAIL                                                
         B     FLTRC270                                                         
*                                                                               
FLTRC250 CLI   DOSTSTAT,DFXSENT    AM I FAX?                                    
         BE    *+12                                                             
         CLI   DOSTSTAT,DFXRSNT      OR FAX RESENT?                             
         BNE   FLTRC260                                                         
         TM    MISCFLG1,MF1XDOSP   DID I FIND THE METHOD IN DOSPELEM?           
         BZ    FLTRC270                                                         
         OI    MISCFLG2,MF2FAXED                                                
         B     FLTRC270                                                         
*                                                                               
FLTRC260 ZIC   R0,1(R6)                                                         
         AR    R6,R0               BUMP TO NEXT DOSTELEM                        
         CLI   DOSTEL,DORPELQ2                                                  
         BE    FLTRC260                                                         
         CLI   DOSTEL,DOSTELQ                                                   
         BE    FLTRC240                                                         
         B     FLTRC280                                                         
*                                                                               
FLTRC270 MVC   LSTODT,DOSTDATE            MOVE IN SENT DATE!                    
FLTRC280 L     R6,ADOSTETL                                                      
*                                                                               
FLTRC290 MVC   BYTE,DOSTSTAT                                                    
FLTRC300 L     R3,AORDSTAB                R3=ORDER STATUS TABLE                 
         USING ORDD,R3                                                          
         SR    R0,R0                                                            
*                                                                               
FLTRC310 CLC   BYTE,ORDSTAT               MATCH STATUS                          
         BNE   FLTRC350                                                         
         CLI   ORDTYP,0                                                         
         BE    FLTRC360                                                         
***************                                                                 
* SPECIAL CODE FOR REJECT/AMEND STATUS (START)                                  
***************                                                                 
         TM    ORDTYP,ORDTAMND                                                  
         BNO   FLTRC320                                                         
         TM    MISCFLG4,MF4AMEND                                                
         BO    FLTRC360                                                         
*                                                                               
FLTRC320 TM    ORDTYP,ORDTRJCT                                                  
         BNO   FLTRC330                                                         
         TM    MISCFLG4,MF4AMEND                                                
         BNO   FLTRC360                                                         
***************                                                                 
* SPECIAL CODE FOR REJECT/AMEND STATUS (END)                                    
*=======================                                                        
* SPECIAL CODE FOR CONFIRM/PARTIAL CONFIRM STATUS (START)                       
***************                                                                 
FLTRC330 TM    ORDTYP,ORDTCMTS            TEST WITH COMMENTS                    
         BNO   FLTRC340                                                         
         TM    MISCFLG4,MF4CNFCM          YES, ARE THERE COMMENTS ?             
         BO    FLTRC360                                                         
*                                                                               
FLTRC340 TM    ORDTYP,ORDTNCMT            TEST WITH NO COMMENTS                 
         BNO   FLTRC350                                                         
         TM    MISCFLG4,MF4CNFCM          YES, ARE THERE COMMENTS ?             
         BNO   FLTRC360                                                         
***************                                                                 
* SPECIAL CODE FOR CONFIRM/PARTIAL CONFIRM STATUS (END)                         
***************                                                                 
*                                                                               
FLTRC350 IC    R0,ORDLN                                                         
         AR    R3,R0                                                            
         CLI   ORDSTAT,X'FF'              EOT - ERR999                          
         BNE   FLTRC310                                                         
*                                                                               
FLTRC360 ST    R3,AORDNTRY                                                      
*                                                                               
         CLI   ORDSTAT,QRJCT                                                    
         BE    FLTRC370                                                         
         TM    FLTRFLG3,FF3AMEND+FF3REJCT                                       
         JNZ   NO                                                               
*                                                                               
FLTRC370 CLI   ORDSTAT,QCFMD              CONFIRMED ?                           
         BE    FLTRC380                                                         
         TM    FLTRFLG2,FF2PCNFM          FILTER ON PARTIAL CONFIRMED?          
         JO    NO                                                               
         B     FLTRC390                                                         
FLTRC380 TM    FLTRFLG2,FF2NCNFM          TEST FILTER ON NOT-CONFIRMED          
         JO    NO                                                               
*                                                                               
FLTRC390 TM    ORDIND2,ORD2XREV           EXCLUDE REVISED ?                     
         BNO   *+12                                                             
         TM    MISCFLG2,MF2REVOR          REVISED ORDER   ?                     
         JO    NO                                                               
*                                                                               
         TM    FLTRFLG2,FF2ACNFM          FILTER ON ALL CONFIRM ORDERS?         
         BNO   *+12                                                             
         TM    ORDIND2,ORD2ACFM           IS A CONFIRM TYPE ORDER?              
         JNO   NO                                                               
*                                                                               
         MVC   LSTSTC,ORDDFLT             DEFAULT DISPLAY STATUS CODE           
         ICM   R0,1,ORDNFLG               R0=NUMBER OF CNTLS TO TEST            
         BZ    FLTRC420                                                         
         LA    R4,ORDDATA                                                       
         USING ORDDATA,R4                                                       
         SR    R1,R1                                                            
FLTRC400 IC    R1,ORDFLG                  TEST STATUS BITS                      
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    MISCFLG2,0                                                       
         BO    FLTRC410                                                         
         LA    R4,L'ORDDATA(R4)                                                 
         BCT   R0,FLTRC400                                                      
         B     FLTRC420                                                         
*                                                                               
FLTRC410 MVC   LSTSTC,ORDCODE             STATUS CODE FOR DISPLAY               
         DROP  R4                                                               
*                                                                               
FLTRC420 TM    ORDIND,ORDISENT                                                  
         BNO   *+8                                                              
         OI    LSTFLG,LSTFSTR             SET TO DISPLAY '*'                    
         TM    RECFLG,RFRLNDL             TEST RECALLED - NOT DELIVERED         
         BNO   *+8                                                              
         OI    LSTFLG,LSTFSTR             ALSO GETS A '*'                       
*                                                                               
         TM    ORDIND,ORDIXCLD            TEST EXCLUDE - UNLESS FILTER          
         BNO   FLTRC430                                                         
         CLC   FLTEQAT,ORDSTAT            UNLESS FILTERED                       
         JNE   NO                                                               
*                                                                               
FLTRC430 TM    FILTRFLG,FFLGSTAT          FILTER ON STATUS?                     
         BZ    FLTRC500                                                         
         CLI   FLTEQAT,0                  FILTERING ON SENT, DEL, NONE?         
         BNE   FLTRC440                                                         
         TM    RECFLG,RFSTDT              TEST STATUS DATE IS ZERO              
         BO    FLTRC470                                                         
         J     NO                         HAS A DATE - SKIP IT                  
*                                                                               
FLTRC440 CLI   FLTEQAT,X'FF'              FILTER FOR ALL TYPES                  
         BNE   FLTRC460                   BRANCH IF NOT                         
         CLI   FLTSTAT,C'R'               FILTER ALL RECALLED ?                 
         BNE   FLTRC450                                                         
         TM    ORDIND,ORDIRCL                                                   
         BO    FLTRC470                                                         
         TM    ORDIND,ORDIRCDA            TEST RECALLED DATE ?                  
         JNO   NO                                                               
         TM    RECFLG,RFRLDLV             RECALL DELIVERED  ?                   
         BO    FLTRC470                                                         
         J     NO                                                               
*                                                                               
FLTRC450 TM    ORDIND,ORDISRCL            TEST *RECALLED                        
         BO    FLTRC470                                                         
         TM    ORDIND,ORDIRCDA            TEST RECALLED DATE ?                  
         JNO   NO                                                               
         TM    RECFLG,RFRLDLV             RECALL DELIVERED ?                    
         BZ    FLTRC470                                                         
         J     NO                                                               
*                                                                               
FLTRC460 CLC   FLTEQAT,ORDSTAT            MATCH STATUS                          
         BE    FLTRC470                                                         
         CLI   FLTEQAT,QERRORED           TEST FILTER = ORDER IN ERROR          
         JNE   NO                                                               
         CLI   ORDSTAT,QRCLUNKN           TEST STATUS UNKNOWN                   
         JNE   NO                                                               
         B     FLTRC500                                                         
*                                                                               
FLTRC470 TM    ORDIND,ORDINSTS            TEST OK TO SKIP STATUS TEST           
         BO    FLTRC500                                                         
         CLI   FLTSTAT,C'*'               FILTER ON '*SENT'                     
         BNE   FLTRC480                                                         
         TM    LSTFLG,LSTFSTR             DOES IT HAVE A '*' ?                  
         JNO   NO                                                               
         CLI   LSTSTC,C'S'                                                      
         BE    FLTRC500                                                         
         J     NO                                                               
*                                                                               
FLTRC480 CLI   FLTSTAT,C'D'                                                     
         BE    *+12                                                             
         CLI   FLTSTAT,C'S'                                                     
         BNE   FLTRC490                                                         
         TM    LSTFLG,LSTFSTR             DOES IT HAVE A '*' ?                  
         JO    NO                                                               
         CLI   ORDSTAT,DSENT                                                    
         BE    FLTRC500                                                         
         J     NO                                                               
*                                                                               
FLTRC490 CLC   FLTSTAT,LSTSTC             MATCH FILTER STATUS ?                 
         JNE   NO                                                               
*                                                                               
FLTRC500 TM    FLTRFLG2,FF2REVSD          FILTER REVISED ORDERS ?               
         BZ    FLTRC510                                                         
         TM    ORDIND,ORDIXREV            TEST EXCLUDE FROM REVISED             
         JO    NO                                                               
*                                                                               
FLTRC510 DS    0H                                                               
         TM    ORDIND,ORDIFTST            SPECIAL DATE TEST NEEDED ?            
         BNO   FLTRC520                                                         
         TM    FILTRFLG,FFLGSTAT          ANY STATUS FILTER?                    
         BO    FLTRC520                   YES, SKIP THE TEST                    
         TM    FLTRFLG2,FF2INBOX          ANY ACTIVE OFFERS ?                   
         BNO   FLTRC520                   YES, SKIP THE TEST                    
         TM    LSTFLG,LSTFMGO             TEST MG FLAG                          
         BO    FLTRC520                                                         
         CLC   LSTACTD,DATEFLT1           SKIP IF NOT ACTIVE(14DAYS)            
         JL    NO                                                               
*                                                                               
FLTRC520 DS    0H                                                               
         CLI   ORDCOLR,C' '               TEST SPECIFIC COLOR                   
         BNH   *+10                                                             
         MVC   LSTCLR,ORDCOLR                                                   
         MVI   LSTCLS,X'FF'               SET COLOR COLLATE SEQ HIGH            
         L     R1,ADCLRTAB                                                      
FLTRC530 CLC   0(1,R1),LSTCLR             MATCH TABLE TO COLOR                  
         BE    FLTRC540                                                         
         LA    R1,L'CLRTAB(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   FLTRC530                                                         
         J     NO                         NOT IN TABLE - SKIP IT                
*                                                                               
FLTRC540 MVC   LSTCLS,1(R1)               MOVE SEQUENCE CODE                    
FLTRC550 SR    R1,R1                                                            
         ICM   R1,1,FLTRCLR               TEST COLOR FILTER                     
         BZ    FLTRC560                   NO FILTER, TAKE THEM ALL              
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    LSTCLS,0                   TEST COLOR SEQUENCE                   
         JZ    NO                                                               
*                                                                               
FLTRC560 MVC   LSTDAD,DAD                 SAVE DISK ADDRESS                     
         XC    LSTACT,FFS                 MOST RECENT FIRST                     
*                                                                               
         TM    FKFLG,FKFMKT               TEST MARKET# IN KEY                   
         JNO   FLTRC570                                                         
         MVC   LSTMKT,BMKT                                                      
         MVC   LSTMKN,MKTNM                                                     
         CLC   BSTA,LSTSTA                TEST SAME STATION                     
         BNE   *+14                                                             
         OC    LSTMKT,LSTMKT              WAS THERE A MARKET#                   
         JNZ   FLTRC570                                                         
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         GOTO1 GETMKT,LSTSTA              GET THE MARKET#                       
         MVC   LSTMKT,BMKT                                                      
         MVC   LSTMKN,MKTNM                                                     
         MVC   KEY,SAVEKEY                                                      
*                                                                               
FLTRC570 TM    FKFLG,FKFPRD               NEED PRODUCT CODE ?                   
         BZ    FLTRC580                                                         
         MVC   SAVEKEY,KEY                                                      
         MVC   BCLT,LSTCLT                SET CLIENT NUMBER                     
         MVC   BPRD,LSTPRD                AND PRODUCT                           
         BRAS  RE,GETQCP                                                        
         MVC   LSTPRDC,QPRD               GET THE PRODUCT CODE                  
         MVC   KEY,SAVEKEY                                                      
*                                                                               
FLTRC580 TM    FKFLG,FKFMKT               DID WE BREAK THE SEQUENCE ?           
         BO    FLTRC590                                                         
         TM    FKFLG,FKFPRD                                                     
         BZ    FLTRC600                                                         
         TM    FILTRFLG,FFLGCLT           CLIENT FILTER?                        
         BO    FLTRC600                                                         
FLTRC590 MVI   RDUPDATE,C'N'                                                    
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
         GOTO1 READ                       RE-ESTABLISH SEQUENCE                 
*                                                                               
FLTRC600 OI    RECFLG,RFOK                                                      
         J     XIT                                                              
*                                                                               
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD ALPHA ORDER PRODUCT LIST                                     
*                                                                               
* ON EXIT:     CC      NEQ = NO PRODUCT CODES                                   
*                                                                               
***********************************************************************         
PRDL     NTR1  ,                                                                
PRDL1    LA    R2,KEY                                                           
         USING DOKEY,R2                                                         
         XC    KEY,KEY                                                          
         MVI   DCKTYPE,DCKTYPQ                                                  
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD,AGYMD                                                    
         OC    KCLIENT,KCLIENT     TEST FIRST TIME                              
         BZ    PRDL3               GET FIRST                                    
*                                                                               
PRDL2    CLC   XPRDL,NPRDL         ALREADY READ LAST PRODUCT                    
         BNL   PRDL3                                                            
         MVC   DCKCLT,KCLIENT      SET CLIENT CODE                              
         SR    R1,R1                                                            
         IC    R1,XPRDL                                                         
         AHI   R1,1                                                             
         STC   R1,XPRDL            UPDATE INDEX                                 
         BCTR  R1,0                                                             
         SLL   R1,2                X 4                                          
         LA    RF,SVCLIST(R1)                                                   
         MVC   KPRODUCT,3(RF)                                                   
         MVC   DCKPRD,KPRODUCT     PRODUCT NUMBER                               
*                                                                               
         XC    KEY+7(6),KEY+7                                                   
         LA    R4,DCKEST-DOKEY                                                  
         TM    FILTRFLG,FFLGEST    FILTER ON ESTIMATE?                          
         BZ    PRDL2A                                                           
         MVC   DCKEST,ESTNUM                                                    
         LA    R4,DCKSTA-DOKEY                                                  
PRDL2A   BCTR  R4,0                                                             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE     X'0DB5'/AGYMD/CLT/PRD/(EST)                   
         BNE   PRDL1                                                            
         J     YES                                                              
*                                                                               
PRDL3    MVI   NPRDL,0            NUMBER IN LIST                                
         MVI   XPRDL,0            CURRENT ITEM IN LIST                          
         TM    FILTRFLG,FFLGCLT   CLIENT FILTER                                 
         BNO   PRDL5                                                            
         CLC   KCLIENT,FLTCLT     TEST ALREADY FINISHED                         
         JE    NO                                                               
         MVC   DCKCLT,FLTCLT                                                    
         B     PRDL7                                                            
*                                                                               
PRDL5    SR    R1,R1                                                            
         ICM   R1,3,KCLIENT       BUMP TO NEXT CLIENT                           
         AHI   R1,1                                                             
         STCM  R1,3,DCKCLT                                                      
*                                                                               
PRDL7    GOTO1 HIGH                                                             
         CLC   KEY(DCKCLT-DOKEY),KEYSAVE                                        
         JNE   NO                 RETURN 'NO MORE'                              
         MVC   SAVEKEY,KEY        SAVE THE CURRENT KEY                          
         MVC   KCLIENT,DCKCLT     SAVE NEW CLIENT CODE                          
*                                                                               
         TM    FILTRFLG,FFLGCLT   CLIENT FILTER                                 
         BZ    PRDL7B                                                           
         CLC   KEY+DCKCLT-DOKEY(L'DCKCLT),FLTCLT                                
         JNE   NO                                                               
         B     PRDL8              NO NEED TO GET SVCLIST                        
*                                                                               
PRDL7B   MVC   FLD(5),SPACES                                                    
         GOTO1 CLUNPK,DMCB,KCLIENT,FLD    GET CLIENT CODE                       
         MVI   FLDH+(FLDILEN-FLDHDRD),2                                         
         CLI   FLD+2,C' '                                                       
         BNH   *+8                                                              
         MVI   FLDH+(FLDILEN-FLDHDRD),3                                         
         LA    R2,FLDH                                                          
         GOTO1 VALICLT            THIS WILL GET PRODUCT LIST                    
         MVC   KEY,SAVEKEY        RESTORE KEY                                   
*                                                                               
PRDL8    LA    RF,SVCLIST         COUNT NUMBER IN LIST                          
         SR    R0,R0                                                            
         CLI   0(RF),0                                                          
         BE    *+16                                                             
         LA    RF,4(RF)           3 PRODUCT CODE, 1 NUMBER                      
         AHI   R0,1                                                             
         B     *-16                                                             
*                                                                               
         STC   R0,NPRDL           SAVE NUMBER IN LIST                           
         CHI   R0,1                                                             
         BE    PRDL2              NEED AT LEAST 2 TO SORT                       
         JL    NO                                                               
         BNH   PRDL2                                                            
*                                                                               
PRDL9    IC    R0,NPRDL           SORT PRODUCT CODES                            
         BCTR  R0,0                                                             
         LA    RE,SVCLIST                                                       
         MVI   BYTE,C'N'                                                        
PRDL13   LA    RF,4(RE)                                                         
         CLC   0(3,RE),0(RF)                                                    
         BNH   PRDL15                                                           
         XC    0(4,RE),0(RF)      SWITCH THEM                                   
         XC    0(4,RF),0(RE)                                                    
         XC    0(4,RE),0(RF)                                                    
         MVI   BYTE,C'Y'          SET SEQUENCE CHANGE                           
PRDL15   LR    RE,RF                                                            
         BCT   R0,PRDL13                                                        
         CLI   BYTE,C'Y'                                                        
         BE    PRDL9              START AGAIN                                   
         LA    R2,KEY                                                           
         B     PRDL2                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT BINARY ORDER TO CHARACTER                                   *         
***********************************************************************         
SHWORDER MVC   FULL,BINORDER                                                    
         XC    FULL,FFS                                                         
*                                                                               
         TM    FULL,X'80'                 NEW STYLE?                            
         BNZ   SHWO20                                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,FULL                  DATE PORTION                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CHRORDDT,DUB                                                     
*                                                                               
         ICM   R1,3,FULL+2                SEQUENCE PORTION                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CHRORDSQ,DUB                                                     
         BR    RE                                                               
*                                                                               
SHWO20   NI    FULL,X'FF'-X'80'                                                 
         ICM   R1,15,FULL                                                       
         CVD   R1,DUB                                                           
         AP    DUB,=P'04000000'                                                 
         OI    DUB+7,X'0F'                                                      
         UNPK  CHRORDER,DUB                                                     
         BR    RE                                                               
*                                                                               
***********************************************************************         
* GET MARKET NUMBER INTO BMKT                                         *         
***********************************************************************         
GETMKT   LR    R0,RE                                                            
         XC    BMKT,BMKT                  CLEAR MARKET                          
         XC    FLDH,FLDH                  CLEAR FIELD HEADER                    
         MVC   FLD,SPACES                 AND FIELD                             
         MVC   BSTA,0(R1)                 PASS STATION                          
         GOTO1 MSUNPK,DMCB,BMKTSTA,FULL,FLD                                     
         ST    R2,SVR2                                                          
         LA    R2,FLDH                                                          
         USING FLDHDRD,R2                                                       
         MVI   FLDILEN,4                  SET FIELD LENGTH                      
         CLI   FLD+4,C' '                 TEST BAND                             
         JNH   *+8                                                              
         MVI   FLDILEN,5                  FIX LENGTH FOR BAND                   
         GOTO1 VALISTA                                                          
         L     R2,SVR2                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET QCLT AND QPRD                                                   *         
***********************************************************************         
GETQCP   NTR1  ,                                                                
*                                                                               
         XC    QPRD,QPRD                                                        
         TM    FILTRFLG,FFLGCLT           CLIENT FILTER?                        
         JO    GQCP10                                                           
         GOTOR GETQPRD                                                          
         JE    GQCPX                                                            
         J     GQCP25                                                           
*                                                                               
GQCP10   DS    0H                                                               
         LA    RF,SVCLIST                 MATCH CODE TO GET NUMBER              
GQCP20   CLC   BPRD,3(RF)                                                       
         JE    GQCP30                                                           
         LA    RF,4(RF)                   BUMP TO NEXT ENTRY                    
         CLI   0(RF),0                                                          
         JNE   GQCP20                                                           
GQCP25   MVC   QPRD,=C'???'                                                     
         J     GQCPX                                                            
*                                                                               
GQCP30   MVC   QPRD,0(RF)                                                       
*                                                                               
GQCPX    DS    0H                                                               
         J     XIT                                                              
***********************************************************************         
* GET THE PFKEY INFORMATION                                           *         
***********************************************************************         
GETPF    NTR1  ,                                                                
*                                                                               
         TM    GOTGLOB,GGLBDAR     COMING BACK FROM BUY?                        
         BZ    GETPF10                                                          
         MVC   SVSELSID,TMPSLSID                                                
         OI    CNTL,CNTLSAME                                                    
*                                                                               
GETPF10  MVI   CALLSP,0                   CLEAR PF KEY STACK                    
         GOTO1 INITIAL,DMCB,PFTABLE       INITIALIZE THE PFKEYS                 
         TM    CNTL,CNTLMXTS              IS TSAR AT MAX ?                      
         BNO   GETPF20                                                          
         CLI   TWAOFFC,C'*'               TEST DDS TERMINAL                     
         BNE   *+12                       NO, MAKE THEM START AGAIN             
         CLI   PFKEY,PFDWNQ               DOWN ?                                
         BE    *+12                       ONLY PF DOWN OPENS LIST               
         NI    ORLMEDH+4,X'FF'-X'20'      FORCE KEY CHANGE                      
         J     XIT                                                              
         NI    CNTL,X'FF'-(CNTLMXTS)      TURNOFF 'MAX TSAR'                    
         OI    CNTL,CNTLSAME              SET TO USE SAME TSAR RECORDS          
         MVC   TSRFST,=Y(1)               SET FIRST TO 1                        
         J     XIT                                                              
*                                                                               
GETPF20  CLI   PFKEY,PFUPQ                UP ?                                  
         BNE   GETPF30                                                          
         SR    RF,RF                                                            
         ICM   RF,3,TSRFST                RF=FIRST ON SCREEN                    
         AHI   RF,-(NLISTQ+1)             LESS NUMBER+1 ON A SCREEN             
         BNM   *+6                                                              
         SR    RF,RF                                                            
         STCM  RF,3,TSRLST                SAVE AS LAST                          
         J     XIT                                                              
*                                                                               
GETPF30  CLI   PFKEY,PFENTQ               ENTER ?                               
         BE    *+12                       SAME AS DOWN                          
         CLI   PFKEY,PFDWNQ               DOWN ?                                
         BNE   GETPF50                                                          
         TM    CNTL,CNTLMORE              ANY MORE DATA TO READ ?               
         BO    GETPF40                    YES, READ MORE                        
         CLC   TSRTOT,=Y(NLISTQ)          ONLY 1 SCREEN?                        
         JNH   GETPF35                    YES, ALWAYS REFRESH                   
*                                                                               
         TM    CNTL,CNTLSAME              NO, DISPLAY SAME SCREEN?              
         JO    XIT                        YES                                   
         CLC   TSRLST,TSRTOT              TEST ALREADY AT END                   
         JNE   XIT                                                              
GETPF35  XC    TSRLST,TSRLST              NO, GO BACK TO TOP                    
         OI    CNTL,CNTLRFSH              YES, SET REFRESH BIT                  
         J     XIT                                                              
*                                                                               
GETPF40  SR    RE,RE                                                            
         ICM   RE,3,TSRTOT                RE=TOTAL                              
         SR    RF,RF                                                            
         ICM   RF,3,TSRLST                RF=LAST                               
         SR    RE,RF                      RE=REMAINING                          
         CHI   RE,NLISTQ                  TEST SCREEN FULL                      
         JNL   XIT                                                              
         OI    CNTL,CNTLREAD              SET READ MORE FILE RECORDS            
         J     XIT                                                              
*                                                                               
GETPF50  CLI   PFKEY,PFRSHQ               REFRESH ?                             
         BNE   *+12                                                             
         OI    CNTL,CNTLRFSH                                                    
         J     XIT                                                              
*                                                                               
         LH    R2,CURDISP                FOR OFRLIST, SEND, BUY, MANCFM         
         AR    R2,RA                                                            
         LA    R0,ORLSEL1H                CURSOR SHOULD BE WITHIN LIST          
         CR    R2,R0                                                            
         JL    INVLDPF                    SET INVALID PF KEY                    
         LA    R0,ORLPFLNH                                                      
         CR    R2,R0                                                            
         JNL   INVLDPF                                                          
*                                                                               
         SR    R0,R0                      GET FIRST BYTE                        
         LH    R1,CURDISP                 OF SELECT FIELD FOR CURSOR            
         AHI   R1,-(ORLSEL1H-T234FFD)                                           
         LA    R3,LINLNQ                                                        
         DR    R0,R3                                                            
         MHI   R1,LINLNQ                                                        
         AHI   R1,(ORLSEL1H-T234FFD)                                            
         STCM  R1,3,CURSOUT                                                     
*                                                                               
         NI    TRNSTAT,X'FF'-(RACHANG)    GLOBBER/MULTI SELECT PROB             
         OI    CNTL,CNTLSAME              DISPLAY SAME LIST                     
*                                                                               
         CLI   PFKEY,PFBCFMQ              MANUAL CONFIRM?                       
         BNE   GETPF60                                                          
         LH    R3,CURDISP                                                       
         AR    R3,RA                                                            
         USING LINDSECT,R3                CAN ONLY MANUAL CONFIRM,              
         CLC   LINSTC(6),=CL6'FXDLVD'     FAX DLVD                              
         BE    GETPF60                                                          
         CLC   LINSTC(6),=CL6'EMDLVD'     EMAIL DLVD                            
         BE    GETPF60                                                          
*                                                                               
         CLC   LINSTC(2),=CL2'**'         PARTIAL CONFIRMS                      
         JNE   CANTCFM                                                          
         CLI   POMAUCFM,C'B'              BUYER CONFIRM PARTIAL CONFIRM         
         JNE   CANTCFM                                                          
         DROP  R3                                                               
*                                                                               
GETPF60  CLI   PFKEY,PFBUYQ               BUY ?                                 
         JE    XIT                                                              
*                                                                               
         CLI   PFKEY,PFDELAQ              DELETE?                               
         BE    GETPF140                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,PFKEY                                                         
         AHI   R0,PFACTLQ                                                       
         STC   R0,PFKEY                                                         
*                                                                               
         CLI   PFKEY,PFACTLQ              ACTUAL PFKEY TO SEND?                 
         BL    GETPF130                   NO                                    
*                                                                               
         LH    R3,CURDISP                                                       
         AR    R3,RA                                                            
         USING LINDSECT,R3                SET FIELDS FOR SEND                   
         MVI   XMITCTB,C'C'               CASH ORDER                            
         CLI   LINORD+8,C'T'                                                    
         BNE   *+8                                                              
         MVI   XMITCTB,C'T'               TRADE HERE                            
         CLI   PFKEY,PFSNDBAQ             SEND BOTH?                            
         BNE   *+8                                                              
         MVI   XMITCTB,C'B'               BOTH ORDERS                           
*                                                                               
         XC    ESTWFLT,ESTWFLT                                                  
         MVC   ESTWFLT(L'LINEST),LINEST   ESTIMATE NUMBER                       
         CLC   LINFLT,SPACES              TEST FLIGHT NUMBER                    
         BNH   GETPF70                    BRANCH IF NO FLIGHT                   
         MVI   ESTWFLT+L'LINEST,C'/'      SET FLIGHT                            
         MVC   ESTWFLT+L'LINEST+1(L'LINFLT),LINFLT                              
         B     GETPF130                                                         
*                                                                               
GETPF70  LA    R2,LINCLTH                 SET CLIENT DATA                       
         USING FLDHDRD,R2                                                       
         MVC   FLDILEN,FLDOLEN            SET FIELD LENGTH                      
         GOTO1 VALICLT                                                          
*                                                                               
         LA    R2,LINPRDH                 SET PRODUCT                           
         MVC   FLDILEN,FLDOLEN            SET FIELD LENGTH                      
         OC    8(L'LINPRD,R2),SPACES                                            
         CLC   =C'***',8(R2)              POL ORDER?                            
         BNE   GETPF90                                                          
         MVC   QPRD,=C'POL'               YES                                   
         MVI   BPRD,X'FF'                                                       
         B     GETPF110                                                         
*                                                                               
GETPF90  MVI   FLDILEN,3                                                        
         CLI   10(R2),C' '                                                      
         BH    *+8                                                              
         MVI   FLDILEN,2                                                        
         GOTO1 VALIPRD                                                          
         MVC   FLDILEN,FLDOLEN                                                  
*                                                                               
GETPF110 LA    R2,LINESTH                 ESTIMATE NUMBER                       
         MVC   FLDILEN,FLDOLEN                                                  
         OI    FLDIIND,FINPNUM            NUMERIC                               
         GOTO1 VALIEST                                                          
*                                                                               
         GOTOR GETFLTRC                                                         
         BNE   GETPF130                                                         
*                                                                               
         MVI   ESTWFLT+L'LINEST,C'/'                                            
         MVC   ESTWFLT+L'LINEST+1(2),=C'00'                                     
*                                                                               
GETPF130 OI    CTLRFLG1,CF1TSELQ          DON'T TEST THE SEL CODES              
         GOTO1 INITIAL,DMCB,PFTABLE                                             
         DC    H'0'                       DIDN'T EXPECT TO GET HERE             
*                                                                               
GETPF140 DS    0H                                                               
         LH    R3,CURDISP                                                       
         AR    R3,RA                                                            
         USING LINDSECT,R3                                                      
         CLC   LINSTC(6),=CL6'UNSENT'                                           
         JNE   CANTDELE                                                         
*                                                                               
         OI    CTLRFLG1,CF1TSELQ          DON'T TEST THE SEL CODES              
         GOTO1 INITIAL,DMCB,PFTABLE2                                            
         DC    H'0'                       DIDN'T EXPECT TO GET HERE             
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET PFKEYS                                              *          
***********************************************************************         
SETPF    NTR1  ,                                                                
         LA    R2,ORLPFLNH                                                      
         USING FLDHDRD,R2                                                       
         OI    FLDOIND,FOUTTRN            SET TRANSMIT PF FIELD                 
         MVC   ORLPFLN(L'PFDATA1),PFDATA1 SET PF KEY TEXT                       
         LA    R3,ORLPFLN+L'PFDATA1+1                                           
*                                                                               
         CLC   TSRFST,=Y(1)               TEST SCREEN STARTS AT 1               
         BE    *+14                       YES, SKIP PAGE UP                     
         MVC   0(L'PFDATA2,R3),PFDATA2                                          
         LA    R3,L'PFDATA2+1(R3)                                               
*                                                                               
         CLC   TSRLST,TSRTOT              LAST SCREEN ENDS AT LAST              
         BL    *+12                       NO,  PAGE DOWN IS OK                  
         TM    CNTL,CNTLMORE              TEST MORE DATA TO READ                
         BNO   *+14                       DOWN IS ALSO OK                       
         MVC   0(L'PFDATA3,R3),PFDATA3                                          
         LA    R3,L'PFDATA3+1(R3)                                               
*                                                                               
         MVC   0(L'PFDATA5,R3),PFDATA5    SET LAST PF KEYS                      
         J     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
RELOTAB  DS    0D                                                               
         DC    AL4(OPTTAB),AL4(AOPTTAB-LSSD)                                    
         DC    AL4(ORDSTAB),AL4(AORDSTAB-LSSD)                                  
         DC    AL4(DKYTAB),AL4(ADKYTAB-LSSD)                                    
         DC    AL4(FDTAB),AL4(ADFTAB-LSSD)                                      
         DC    AL4(LDTAB),AL4(ADLDTAB-LSSD)                                     
         DC    AL4(SEQTAB),AL4(ADSEQTAB-LSSD)                                   
         DC    AL4(CLRTAB),AL4(ADCLRTAB-LSSD)                                   
         DC    X'FF'                                                            
*                                                                               
PFDATA1  DC    C'PF2=Status 4=Ofrlist 3=Recall'                                 
PFDATA2  DC    C'5=Up'                                                          
PFDATA3  DC    C'6=Down'                                                        
PFDATA5  DC    C'7=Refresh 9=Send 10=Sendboth 11=Buy'                           
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                        *         
***********************************************************************         
PFENTQ   EQU   0                          'ENTER'                               
PFSTAQ   EQU   2                          STATUS                                
PFRCLQ   EQU   3                          RECALL                                
PFOFRQ   EQU   4                          OFFER                                 
PFUPQ    EQU   5                          UP                                    
PFDWNQ   EQU   6                          DOWN                                  
PFRSHQ   EQU   7                          REFRESH                               
PFBCFMQ  EQU   8                          MANUAL CONFIRM                        
PFSNDQ   EQU   9                          SEND                                  
PFSNDBQ  EQU   10                         SENDBOTH                              
PFBUYQ   EQU   11                         BUY                                   
PFRTNQ   EQU   12                         RETURN                                
PFACTLQ  EQU   12                                                               
PFDELAQ  EQU   13                         DELETE                                
PFSTAAQ  EQU   PFSTAQ+PFACTLQ             STATUS          14                    
PFRCLAQ  EQU   PFRCLQ+PFACTLQ             RECALL          15                    
PFOFRAQ  EQU   PFOFRQ+PFACTLQ             OFFER           16                    
PFBCFMAQ EQU   PFBCFMQ+PFACTLQ            BUYER CONFIRM   20                    
PFSNDAQ  EQU   PFSNDQ+PFACTLQ             SEND            21                    
PFSNDBAQ EQU   PFSNDBQ+PFACTLQ            SENDBOTH        22                    
*                                                                               
PFTABLE  DS    0C                                                               
* STATUS                                                                        
         DC    AL1(PF02X-*,PFSTAQ,0,0,0,PFTRETRN)                               
         DC    CL3'S',CL8' ',CL8' '                                             
PF02X    EQU   *                                                                
                                                                                
* RECALL                                                                        
         DC    AL1(PF03X-*,PFRCLQ,0,0,0,PFTRETRN)                               
         DC    CL3'R',CL8' ',CL8' '                                             
PF03X    EQU   *                                                                
                                                                                
* MG                                                                            
         DC    AL1(PF04X-*,PFOFRQ,0,0,0,PFTRETRN)                               
         DC    CL3'M',CL8' ',CL8' '                                             
PF04X    EQU   *                                                                
                                                                                
* MG                                                                            
         DC    AL1(PF04XA-*,PFOFRQ,0,0,0,PFTRETRN)                              
         DC    CL3'O',CL8' ',CL8' '                                             
PF04XA   EQU   *                                                                
                                                                                
* UP                                                                            
         DC    AL1(PF05X-*,PFUPQ,0,0,0)                                         
         DC    CL3' ',CL8' ',CL8' '                                             
PF05X    EQU   *                                                                
                                                                                
* DOWN                                                                          
         DC    AL1(PF06X-*,PFDWNQ,0,0,0)                                        
         DC    CL3' ',CL8' ',CL8' '                                             
PF06X    EQU   *                                                                
                                                                                
* REFRESH                                                                       
         DC    AL1(PF07X-*,PFRSHQ,0,0,0)                                        
         DC    CL3' ',CL8' ',CL8' '                                             
PF07X    EQU   *                                                                
                                                                                
* MANUAL CONFIRM                                                                
         DC    AL1(PF08X-*,PFBCFMQ,0,0,0,PFTRETRN)                              
         DC    CL3'C',CL8' ',CL8' '                                             
PF08X    EQU   *                                                                
                                                                                
* SEND                                                                          
         DC    AL1(PF09X-*,PFSNDQ,0,0,0,PFTRETRN)                               
         DC    CL3'X',CL8' ',CL8' '                                             
PF09X    EQU   *                                                                
                                                                                
* SENDBOTH                                                                      
         DC    AL1(PF10X-*,PFSNDBQ,0,0,0,PFTRETRN)                              
         DC    CL3'+',CL8' ',CL8' '                                             
PF10X    EQU   *                                                                
                                                                                
* BUY                                                                           
         DC    AL1(PF11X-*,PFBUYQ,0,0,0,PFTRETRN)                               
         DC    CL3'B',CL8' ',CL8' '                                             
PF11X    EQU   *                                                                
                                                                                
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,PFRTNQ,PFTRPROG,0,0,0)                               
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
                                                                                
* REFRESH                                                                       
         DC    AL1(PF13X-*,PFDELAQ,0,0,0,PFTRETRN)                              
         DC    CL3'D',CL8' ',CL8' '                                             
PF13X    EQU   *                                                                
                                                                                
* ACTUAL STATUS                                                                 
*                                                                               
         DC    AL1(PF14X-*,PFSTAAQ,PFTCPROG,0)                                  
         DC    AL1((PF14X-PF14)/KEYLNQ,0)                                       
         DC    CL3' ',CL8'ORDER',CL8'STATUS'                                    
PF14     DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T234FFD)                     
         DC    AL1(KEYTYCUR,L'LINORD-1),AL2(LINORD-LINSTT1)                     
PF14X    EQU   *                                                                
*                                                                               
* ACTUAL RECALL                                                                 
*                                                                               
         DC    AL1(PF15X-*,PFRCLAQ,PFTCPROG,0,(PF15X-PF15)/KEYLNQ,0)            
         DC    CL3' ',CL8'ORDER',CL8'RECALL'                                    
PF15     DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T234FFD)                     
         DC    AL1(KEYTYCUR,L'LINBYR-1),AL2(LINBYR-LINSTT1)                     
         DC    AL1(KEYTYWS,L'XMITCTB-1),AL2(XMITCTB-LSSD)                       
         DC    AL1(KEYTYCUR,L'LINCLT-1),AL2(LINCLT-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINPRD-1),AL2(LINPRD-LINSTT1)                     
         DC    AL1(KEYTYWS,L'ESTWFLT-1),AL2(ESTWFLT-LSSD)                       
         DC    AL1(KEYTYCUR,L'LINSTA-1),AL2(LINSTA-LINSTT1)                     
PF15X    EQU   *                                                                
*                                                                               
* ACTUAL MG                                                                     
*                                                                               
         DC    AL1(PF16X-*,PFOFRAQ,PFTCPROG,0)                                  
         DC    AL1((PF16X-PF16)/KEYLNQ,0)                                       
         DC    CL3' ',CL8'ORDER',CL8'OFFER'                                     
PF16     DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T234FFD)                     
         DC    AL1(KEYTYCUR,L'LINBYR-1),AL2(LINBYR-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINORD-1),AL2(LINORD-LINSTT1)                     
PF16X    EQU   *                                                                
*                                                                               
* ACTUAL BUYER CONFIRM                                                          
*                                                                               
         DC    AL1(PF20X-*,PFBCFMAQ,PFTCPROG,0)                                 
         DC    AL1((PF20X-PF20)/KEYLNQ,0)                                       
         DC    CL3' ',CL8'ORDER',CL8'BYRCNFM'                                   
PF20     DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T234FFD)                     
         DC    AL1(KEYTYCUR,L'LINORD-1),AL2(LINORD-LINSTT1)                     
PF20X    EQU   *                                                                
*                                                                               
* ACTUAL SEND                                                                   
*                                                                               
         DC    AL1(PF21X-*,PFSNDAQ,PFTCPROG,0)                                  
         DC    AL1((PF21X-PF21)/KEYLNQ,0)                                       
         DC    CL3' ',CL8'ORDER',CL8'SEND'                                      
PF21     DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T234FFD)                     
         DC    AL1(KEYTYCUR,L'LINBYR-1),AL2(LINBYR-LINSTT1)                     
         DC    AL1(KEYTYWS,L'XMITCTB-1),AL2(XMITCTB-LSSD)                       
         DC    AL1(KEYTYCUR,L'LINCLT-1),AL2(LINCLT-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINPRD-1),AL2(LINPRD-LINSTT1)                     
         DC    AL1(KEYTYWS,L'ESTWFLT-1),AL2(ESTWFLT-LSSD)                       
         DC    AL1(KEYTYCUR,L'LINSTA-1),AL2(LINSTA-LINSTT1)                     
PF21X    EQU   *                                                                
*                                                                               
* ACTUAL SEND BOTH                                                              
*                                                                               
         DC    AL1(PF22X-*,PFSNDBAQ,PFTCPROG,0,(PF22X-PF22)/KEYLNQ,0)           
         DC    CL3' ',CL8'ORDER',CL8'SEND'                                      
PF22     DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T234FFD)                     
         DC    AL1(KEYTYCUR,L'LINBYR-1),AL2(LINBYR-LINSTT1)                     
         DC    AL1(KEYTYWS,L'XMITCTB-1),AL2(XMITCTB-LSSD)                       
         DC    AL1(KEYTYCUR,L'LINCLT-1),AL2(LINCLT-LINSTT1)                     
         DC    AL1(KEYTYCUR,L'LINPRD-1),AL2(LINPRD-LINSTT1)                     
         DC    AL1(KEYTYWS,L'ESTWFLT-1),AL2(ESTWFLT-LSSD)                       
         DC    AL1(KEYTYCUR,L'LINSTA-1),AL2(LINSTA-LINSTT1)                     
PF22X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
PFTABLE2 DS    0H                    ANOTHER PFTABLE FOR DELETE                 
* ACTUAL DELETE                                                                 
*                                                                               
         DC    AL1(PF13Z-*,PFDELAQ,PFTCPROG,0)                                  
         DC    AL1((PF13Z-PF13)/KEYLNQ,0)                                       
         DC    CL3' ',CL8'ORDER',CL8'DELETE'                                    
PF13     DC    AL1(KEYTYTWA,L'ORLMED-1),AL2(ORLMED-T234FFD)                     
         DC    AL1(KEYTYCUR,L'LINORD-1),AL2(LINORD-LINSTT1)                     
PF13Z    EQU   *                                                                
         DROP  R7,RB                                                            
         EJECT                                                                  
***********************************************************************         
* GET THE FLIGHT RECORD                                               *         
* ON EXIT:     (CC)                EQ = FLIGHT EXISTS FOR THE EST     *         
*                                  NE = NO FLIGHT RECORD EXISTS       *         
***********************************************************************         
GETFLTRC NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY             SEE IF WE HAVE THE FLIGHT REC USING          
         LA    R4,KEY                 POL ON THE ESTIMATE                       
         USING DFLRECD,R4                                                       
         MVI   DFLKTYP,DFLKTYPQ                                                 
         MVI   DFLKSUB,DFLKSUBQ                                                 
         MVC   DFLKAGMD,AGYMD                                                   
         MVC   DFLKCLT,BCLT                                                     
         MVC   DFLKPRD,=C'POL'                                                  
         MVC   DFLKEST,BEST                                                     
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'DFLKEY),KEYSAVE    FLIGHT REC EXISTS FOR POL EST?          
         JE    YES                      YES                                     
*                                                                               
         XC    KEY,KEY             SEE IF WE HAVE THE FLIGHT REC USING          
         LA    R4,KEY                 PRIMARY PRODUCT ON THE ESTIMATE           
         USING DFLRECD,R4                                                       
         MVI   DFLKTYP,DFLKTYPQ                                                 
         MVI   DFLKSUB,DFLKSUBQ                                                 
         MVC   DFLKAGMD,AGYMD                                                   
         MVC   DFLKCLT,BCLT                                                     
         MVC   DFLKPRD,QPRD                                                     
         MVC   DFLKEST,BEST                                                     
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'DFLKEY),KEYSAVE    FLIGHT REC EXISTS FOR PRD/EST?          
         JE    YES                      YES                                     
*                                                                               
         XC    KEY,KEY             SEE IF WE HAVE THE FLIGHT REC FOR            
         LA    R4,KEY                 'ALL' PRODUCTS ON THIS ESTIMATE           
         USING DFLRECD,R4                                                       
         MVI   DFLKTYP,DFLKTYPQ                                                 
         MVI   DFLKSUB,DFLKSUBQ                                                 
         MVC   DFLKAGMD,AGYMD                                                   
         MVC   DFLKCLT,BCLT                                                     
         MVC   DFLKPRD,=C'ALL'                                                  
         MVC   DFLKEST,BEST                                                     
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'DFLKEY),KEYSAVE    FLIGHT REC EXISTS FOR EST?              
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* BUMP THE I/O COUNTER AND CHECK                                      *         
***********************************************************************         
CHKIO    NTR1  BASE=*,LABEL=*                                                   
         LH    RF,IO_COUNT                                                      
         CHI   RF,300                    CHECK EVERY 300 I/O'S                  
         BL    CHKIO3                                                           
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO              MAX IO'S                              
         MHI   R3,MXIOPCT                 GET % OF MAX                          
         D     R2,=F'100'                                                       
         CLM   R3,3,FATIOCNT                                                    
         BH    *+8                        STILL LESS THAN MAX %                 
         OI    CNTL,CNTLMXIO              SET MAX IO'S EXCEEDED                 
         DROP  R1                                                               
*                                                                               
         SR    RF,RF                      RESET COUNT FOR ANOTHER 300           
*                                                                               
CHKIO3   AHI   RF,1                                                             
         STH   RF,IO_COUNT                                                      
         J     XLO                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERTS EBCDIC ORDER NUMBER TO BINARY ORDER NUMBER                 *         
* ON ENTRY:    PARAM 1             A(EBCDIC ORDER NUMBER)             *         
* ON EXIT:     BINORDER            BINARY ORDER NUMBER                *         
***********************************************************************         
BINORDR  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
*                                                                               
         CLI   1(R2),C'3'                                                       
         BNH   BNORDR10                                                         
         PACK  DUB,0(8,R2)         NEW STYLE ORDER NUMBER                       
         SP    DUB,=P'04000000'                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,BINORDER                                                   
         OI    BINORDER,X'80'                                                   
         XC    BINORDER,=4X'FF'                                                 
         J     XIT                                                              
*                                                                               
BNORDR10 ZAP   FULL,JDTTODAY       CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         GOTO1 HEXIN,DMCB,(R2),DUB,8   SAVE AS IF ENTRY WAS HEX                 
         MVC   PACKOF4B,DUB            CONVERT IT TO PACK                       
         OI    PACKOF4B+3,X'0F'                                                 
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
         MVC   PACKOF4B+2(1),FULL+2 COPY TODAY'S CENTURY                        
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    PACKOF4B,=P'90'     CALCULATE FROM 1990                          
         SRP   PACKOF4B,4,0                                                     
         OC    PACKOF4B+1(2),DUB   STICK IN DAYS IN YEAR                        
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=4X'FF'                                                 
*                                                                               
         PACK  DUB,4(4,R2)         SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=4X'FF'                                                 
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS MAKEGOOD RECORD                                             *         
* IF USING MAKEGOOD KEY - GET MAKEGOOD INTO IO2                       *         
*                       - READ ORDER KEY                              *         
* ELSE - ORDER IS ALREADY IN IO1                                      *         
*      - GET MAKEGOOD INTO IO2 - SET MG STATUS                        *         
***********************************************************************         
MGOFR    NTR1  BASE=*,LABEL=*                                                   
         MVI   MGFLAGS,0                                                        
         MVC   SAVEKEY,KEY                                                      
*                                                                               
MGOFR3   BAS   RE,MGGET            SET MG COLOR STATUS                          
         TM    MGFLAGS,MGGREEN     TEST GREEN ** HIGHEST PRIORITY **            
         BO    MGOFR5              GET THE ORDER                                
         MVC   KEYSAVE,KEY         SAVE THIS KEY                                
         GOTO1 SEQ                                                              
         GOTOR CHKIO                                                            
         TM    CNTL,CNTLMXIO              TEST MAX IO'S                         
         BO    MGOMXIO                                                          
         CLC   KEY(MNKGROUP-MNKEY),KEYSAVE  SAME ORDER?                         
         BE    MGOFR3                                                           
         TM    MGFLAGS,MGRED+MGBLACK   ANY ACTIVE OFFER ?                       
         BNZ   MGOFR5                  GET THE ORDER                            
         MVC   KEY,SAVEKEY                                                      
         J     XIT                                                              
*                                                                               
MGOFR5   L     R6,AIO2             R6=A(MAKEGOOD NOTICE RECORD)                 
         USING DAREMGND,R6                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY              R4=A(ORDER RECORD)                           
         USING DOKEY,R4                                                         
         MVI   DOKTYPE,DOKTYPQ     RECORD TYPE                                  
         MVI   DOKSUBTY,DOKSTYPQ   SUB-TYPE                                     
         MVC   DOKAGMD,MNKAGMD     AGENCY/MEDIA                                 
         MVC   DOKORDER,MNKORDER   ORDER #                                      
         MVI   RDUPDATE,C'N'                                                    
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE  TEST CORRECT ORDER                    
         BE    *+6                                                              
         DC    H'0'               DIE!! MG DOES NOT HAVE AN ORDER!              
         MVC   DAD,KEY+(MNKDSKAD-MNKEY)                                         
         MVI   RDUPDATE,C'N'                                                    
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
         GOTO1 GETREC              GET THE ORDER RECORD                         
         MVC   KEY,SAVEKEY         RESTORE MG KEY                               
         GOTOR CHKIO                                                            
         J     YES                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
         EJECT                                                                  
MGGET    LR    R0,RE                                                            
         MVC   AIO,AIO2            READ MAKEGOOD INTO IO2                       
         MVI   RDUPDATE,C'N'                                                    
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         GOTOR CHKIO                                                            
         TM    CNTL,CNTLMXIO                                                    
         BO    MGOMXIO             EXCEEDED IO COUNT                            
         NI    MGFLAGS,X'FF'-MGDELNOT  RESET DELIVERY NOTICE FLAG               
         L     R6,AIO2                 A(RECORD)                                
         MVI   ELCODE,MNSTELQ          GET MAKEGOOD STATUS ELEMENT              
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING MNSTELD,R6                                                       
MGGET3   CLI   MNSTSTAT,MNSTDELV   DELIVERED ?                                  
         BNE   MGGET5                                                           
         OI    MGFLAGS,MGDELNOT    SET DELIVERED STATUS                         
         BRAS  RE,NEXTEL                                                        
         BE    MGGET3                                                           
         DC    H'0'                                                             
*                                                                               
MGGET5   CLI   MNSTSTAT,MNSTOKAY   OKAYED ?                                     
         BE    MGGETX                                                           
         CLI   MNSTSTAT,MNSTCAN    CANCELLED ?                                  
         BNE   *+12                                                             
         OI    MGFLAGS,MGBLACK                                                  
         B     MGGETX                                                           
*                                                                               
         CLI   MNSTSTAT,MNSTCANM   CANCELLED WITH MORE... ?                     
         BE    MGGET7                                                           
*                                                                               
         CLI   MNSTSTAT,MNSTAPP    APPROVED ?                                   
         BE    *+12                                                             
         CLI   MNSTSTAT,MNSTREJ    REJECTED ?                                   
         BNE   MGGET9                                                           
         TM    MGFLAGS,MGDELNOT    DELIVERED ?                                  
         BZ    MGGET9                                                           
*                                                                               
MGGET7   OI    MGFLAGS,MGRED       TURN RED STATUS BIT ON                       
         B     MGGETX                                                           
                                                                                
*                                                                               
MGGET9   OI    MGFLAGS,MGGREEN     TURN GREEN STATUS BIT ON                     
MGGETX   LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
MGOMXIO  MVC   KEY,SAVEKEY         MAX IO'S - RESTORE KEY                       
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R6,RB                                                            
         EJECT                                                                  
***********************************************************************         
* INTERFACE TO TSAR                                                   *         
***********************************************************************         
TOTSAR   NMOD1 0,**TOTSAR                                                       
         L     RC,BASERC                                                        
         USING GEND,RC                                                          
         LA    R4,TSARBLK                                                       
         USING TSARD,R4                                                         
         SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     TSRINI                     INITIALIZE                            
         B     TSRRES                     RESTORE                               
         B     TSRADD                     ADD                                   
         B     TSRGET                     GET                                   
         B     TSRSAV                     SAVE                                  
         B     TSRGT1                     GET ONE RECORD                        
         B     TSRPUT                     PUT                                   
*                                                                               
TSRINI   XC    TSARBLK(TSARDL),TSARBLK                                          
         XC    TSRFST,TSRFST              INITIALIZE TSAR COUNTERS              
         XC    TSRLST,TSRLST                                                    
         XC    TSRTOT,TSRTOT                                                    
         MVI   TSACTN,TSAINI                                                    
         MVC   TSACOM,ACOMFACS            A(COMFACS)                            
*                                                                               
         L     R3,ASQDAT                                                        
         USING SSQD,R3                                                          
         SR    R0,R0                                                            
         IC    R0,SSQNFLD                 R0=NUMBER OF FIELDS                   
         LA    R3,SSQDATA                                                       
         USING SSQDATA,R3                                                       
         SR    R1,R1                                                            
         SR    R2,R2                                                            
*                                                                               
TSRINI3  SR    RF,RF                                                            
         IC    RF,SSQDQ                   RF=DATA EQUATE                        
         SLL   RF,1                                                             
         A     RF,ADLDTAB                 RF=A(LIST DATA DEFINITIONS)           
         USING LDD,RF                                                           
         IC    R1,LDDLEN                  R1=LENGTH OF FIELD                    
         AR    R2,R1                      R2=RECORD LENGTH                      
*                                                                               
         TM    SSQDIND,SSQDIEND           END OF KEY                            
         BNO   *+8                                                              
         STC   R2,TSKEYL                  KEY LENGTH                            
         LA    R3,L'SSQDATA(R3)                                                 
         BCT   R0,TSRINI3                                                       
*                                                                               
         DROP  R3,RF                                                            
*                                                                               
         STCM  R2,3,TSRECL                RECORD LENGTH                         
         LA    R1,TSRREC                                                        
         ST    R1,TSAREC                                                        
         CLI   TMPSTNUM,0                 TEMPEST RESERVED?                     
         BNE   TSRINI4                    YES, RESTORE PREVIOUS                 
         MVI   TSPAGL,1                                                         
         MVI   TSPAGN,TSPEXPN                                                   
         OI    TSINDS,TSIALLOC            SET TO ALLOCATE                       
         B     TSRINI5                                                          
*                                                                               
TSRINI4  MVC   TSPAGL,TMPSTLOW                                                  
         MVC   TSPAGN,TMPSTNUM                                                  
         OI    TSINDS,TSIALLOC+TSIREUSE   SET TO RESTORE                        
*                                                                               
TSRINI5  GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0                   SET CC ON EXIT                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TMPSTLOW,TSPAGL                                                  
         MVC   TMPSTNUM,TSPAGN                                                  
         J     YES                                                              
*                                                                               
TSRRES   XC    TSARBLK(TSARDL),TSARBLK    RESTORE                               
         MVI   TSACTN,TSARES                                                    
         MVC   TSACOM,ACOMFACS            A(COMFACS)                            
         LA    R1,TSRREC                                                        
         ST    R1,TSAREC                                                        
         OI    TSINDS,TSIALLOC            USE TEMPEST                           
         MVC   TSPAGL,TMPSTLOW                                                  
         MVC   TSPAGN,TMPSTNUM                                                  
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0                   SET CC ON EXIT                        
         BE    *+6                                                              
         DC    H'0'                                                             
         J     YES                                                              
         EJECT                                                                  
*                                                                               
TSRADD   L     R3,ASQDAT                  R3=A(SEQUENCE DATA)                   
         USING SSQD,R3                                                          
         SR    R0,R0                                                            
         IC    R0,SSQNFLD                 NUMBER OF SORT FIELDS                 
         LA    R3,SSQDATA                                                       
         USING SSQDATA,R3                                                       
         LA    R2,TSRREC                  R2=A(OUTPUT FIELD)                    
         XC    TSRREC,TSRREC                                                    
         SR    R1,R1                                                            
*                                                                               
TSRADD3  SR    RF,RF                                                            
         IC    RF,SSQDQ                   RF=DATA EQUATE                        
         SLL   RF,1                                                             
         A     RF,ADLDTAB                 RF=A(LIST DATA DEFINITIONS)           
         USING LDD,RF                                                           
         IC    R1,LDDLEN                  R1=LENGTH OF FIELD                    
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         IC    RE,LDDLOC                  RE=DISPLACEMENT TO DATA               
         LA    RE,LSTTAB(RE)                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)              DATA TO OUTPUT RECORD                 
         LA    R2,1(R1,R2)                R2=TO NEXT OUTPUT FIELD               
         LA    R3,L'SSQDATA(R3)           R3=NEXT DATA EQUATE                   
         BCT   R0,TSRADD3                                                       
*                                                                               
         LA    R1,TSRREC                                                        
         ST    R1,TSAREC                                                        
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR,TSARBLK                                                    
         MVC   TSRTOT,TSPRECN                                                   
         LA    R2,MXTSAR                                                        
         CLM   R2,3,TSRTOT                TEST TOO MANY IN TSAR                 
         BH    *+12                                                             
         OI    CNTL,CNTLMXTS              SET MAX TSAR                          
         J     XIT                                                              
         CLI   TSERRS,0                                                         
         JE    XIT                                                              
         TM    TSERRS,X'80'               TEST EOF                              
         JNO   XIT                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R3,RF                                                            
         EJECT                                                                  
*                                                                               
TSRGET   LA    R7,NLISTQ                  MAX NUMBER ON SCREEN                  
         LA    R6,LSTTAB                                                        
         MVI   NLST,0                                                           
         SR    RF,RF                                                            
         ICM   RF,3,TSRFST                                                      
         BNZ   *+8                                                              
         AHI   RF,1                                                             
         STCM  RF,3,TSRFST                                                      
*                                                                               
TSRGET1  STCM  RF,3,TSRLST                SET RECORD NUMBER                     
         STCM  RF,3,TSRNUM                RECORD NUMBER IN TSAR                 
         LA    R1,TSRREC                                                        
         ST    R1,TSAREC                                                        
         MVI   TSACTN,TSAGET              FOR GET                               
         XC    TSRREC,TSRREC                                                    
*                                                                               
TSRGET3  GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0                   SET CC ON EXIT                        
         BE    TSRGET5                                                          
* THIS WON'T USUALLY HAPPEN, BUT WHEN IT DOES IT BECAUSE TSPRECN WAS            
* BLOWN AWAY!!!                                                                 
         NI    ORLMEDH+4,X'FF'-X'20'      FORCE KEY CHANGE                      
         DC    H'0',C'$ABEND'                                                   
         DC    C'TSAR RECORD NUMBER WAS BLOWN AWAY!!'                           
*                                                                               
TSRGET5  L     R3,ASQDAT                  R3=A(SEQUENCE DATA)                   
         USING SSQD,R3                                                          
         SR    R0,R0                                                            
         IC    R0,SSQNFLD                 NUMBER OF SORT FIELDS                 
         LA    R3,SSQDATA                                                       
         USING SSQDATA,R3                                                       
         LA    R2,TSRREC                  R2=A(OUTPUT FIELD)                    
         SR    R1,R1                                                            
*                                                                               
TSRGET7  SR    RF,RF                                                            
         IC    RF,SSQDQ                   RF=DATA EQUATE                        
         SLL   RF,1                                                             
         A     RF,ADLDTAB                 RF=A(LIST DATA DEFINITIONS)           
         USING LDD,RF                                                           
         IC    R1,LDDLEN                  R1=LENGTH OF FIELD                    
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         IC    RE,LDDLOC                  RE=DISPLACEMENT TO DATA               
         LA    RE,0(RE,R6)                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R2)              DATA TO LIST AREA                     
         LA    R2,1(R1,R2)                R2=TO NEXT INPUT FIELD                
         LA    R3,L'SSQDATA(R3)           R3=NEXT DATA EQUATE                   
         BCT   R0,TSRGET7                                                       
*                                                                               
         USING LSTD,R6                                                          
         MVC   LSTRECNM,TSRNUM            TSAR RECORD NUMBER                    
         DROP  R6                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,NLST                                                          
         AHI   RF,1                                                             
         STC   RF,NLST                    UPDATE NUMBER IN LIST                 
*                                                                               
         LA    R6,LSTLNQ(R6)                                                    
         BCT   R7,*+8                                                           
         J     YES                                                              
*                                                                               
         ICM   RF,3,TSRLST                GET ANOTHER ONE                       
         AHI   RF,1                                                             
         CLM   RF,3,TSRTOT                NEXT VS. TOTAL                        
         BNH   TSRGET1                                                          
         J     YES                        NO MORE                               
*                                                                               
TSRSAV   MVI   TSACTN,TSASAV                                                    
         LA    R1,TSRREC                                                        
         ST    R1,TSAREC                                                        
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TMPSTLOW,TSPAGL                                                  
         MVC   TMPSTNUM,TSPAGN                                                  
         J     YES                                                              
*                                                                               
TSRGT1   LA    R1,TSRREC                                                        
         ST    R1,TSAREC                                                        
         MVC   TSRNUM,TSRSEL                                                    
         MVI   TSACTN,TSAGET              FOR GET                               
         XC    TSRREC,TSRREC                                                    
*                                                                               
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0                   SET CC ON EXIT                        
         JE    YES                                                              
         DC    H'0'                                                             
*                                                                               
TSRPUT   DS    0H                                                               
         L     R3,ASQDAT                  R3=A(SEQUENCE DATA)                   
         USING SSQD,R3                                                          
         SR    R0,R0                                                            
         IC    R0,SSQNFLD                 NUMBER OF SORT FIELDS                 
         LA    R3,SSQDATA                                                       
         USING SSQDATA,R3                                                       
         LA    R2,TSRREC                  R2=A(OUTPUT FIELD)                    
         XC    TSRREC,TSRREC                                                    
         SR    R1,R1                                                            
*                                                                               
TSRPUT3  SR    RF,RF                                                            
         IC    RF,SSQDQ                   RF=DATA EQUATE                        
         SLL   RF,1                                                             
         A     RF,ADLDTAB                 RF=A(LIST DATA DEFINITIONS)           
         USING LDD,RF                                                           
         IC    R1,LDDLEN                  R1=LENGTH OF FIELD                    
         BCTR  R1,0                                                             
         L     RE,ALSTTAB                 A(ENTRY WE WANT TO CHANGE)            
         SR    R6,R6                                                            
         IC    R6,LDDLOC                  RE=DISPLACEMENT TO DATA               
         AR    RE,R6                                                            
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)              DATA TO OUTPUT RECORD                 
         LA    R2,1(R1,R2)                R2=TO NEXT OUTPUT FIELD               
         LA    R3,L'SSQDATA(R3)           R3=NEXT DATA EQUATE                   
         BCT   R0,TSRPUT3                                                       
*                                                                               
         LA    R1,TSRREC                                                        
         ST    R1,TSAREC                                                        
         MVC   TSRNUM,TSRSEL                                                    
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0                                                         
         JE    YES                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R3,R4,RF                                                         
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                    *         
***********************************************************************         
VK       NMOD1 0,*VALK*,R7                                                      
         L     RC,BASERC                                                        
         USING GEND,RC                                                          
         NI    DMINBTS,X'FF'-X'08' DON'T PASS BACK DELETED RECORDS              
         MVI   MISCFLG1,0                                                       
         MVI   MISCFLG3,0                                                       
*                                                                               
         XC    INBOXSRT,INBOXSRT                                                
         XC    TODO,TODO                                                        
*                                                                               
         TM    ORLMEDH+4,X'20'            ANY CHANGES                           
         BNO   VK05                                                             
         TM    ORLBUYRH+4,X'20'                                                 
         BNO   VK05                                                             
         TM    ORLCLTH+4,X'20'                                                  
         BNO   VK05                                                             
         TM    ORLORDRH+4,X'20'                                                 
         BNO   VK05                                                             
         TM    ORLSTATH+4,X'20'                                                 
         BNO   VK05                                                             
         TM    ORLEDATH+4,X'20'                                                 
         BNO   VK05                                                             
         TM    ORLOPTNH+4,X'20'                                                 
         BO    VKMED00                                                          
*                                                                               
VK05     NI    ORLMEDH+4,X'FF'-X'20'      VALIDATE ALL                          
         NI    ORLBUYRH+4,X'FF'-X'20'                                           
         NI    ORLCLTH+4,X'FF'-X'20'                                            
         NI    ORLORDRH+4,X'FF'-X'20'                                           
         NI    ORLSTATH+4,X'FF'-X'20'                                           
         NI    ORLEDATH+4,X'FF'-X'20'                                           
         NI    ORLOPTNH+4,X'FF'-X'20'                                           
         OI    MISCFLG1,MF1KYCHG                                                
***********************************************************************         
* VALIDATE THE MEDIA                                                  *         
***********************************************************************         
VKMED00  LA    R2,ORLMEDH                                                       
         CLI   5(R2),0             NEED THE MEDIA                               
         BNE   VKMED05                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPMD                             
         CLI   8(R1),0                                                          
         BE    VKMED05                                                          
         J     NEEDFLDS                                                         
*                                                                               
VKMED05  GOTO1 VALIMED                                                          
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPMD                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    4(R2),X'20'                                                      
*                                                                               
VKMED10  DS    0H                                                               
         MVC   BYTE,BAGYMD                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'02'                                                       
         BNE   *+8                                                              
         OI    MISCFLG3,MF3RADIO                                                
*                                                                               
VKMEDX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE BUYER                                                      *         
***********************************************************************         
VKBUYR00 LA    R2,ORLBUYRH                                                      
         TM    4(R2),X'20'                TEST CHANGED BUYER                    
         BO    VKBUYR30                                                         
         OC    8(L'ORLBUYR,R2),SPACES                                           
         MVI   NBYR,0                     NUMBER OF ASS'T BUYER CODES           
         MVI   XBYR,0                                                           
         MVC   BUYRC,SPACES               CLEAR BUYER CODE                      
         MVC   BUYRL,SPACES               AND LIST                              
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                 LENGTH OF INPUT FIELD                 
         BZ    VKBUYR20                   NO BUYER                              
         BCTR  R1,0                                                             
         LA    R6,ORLBUYR(R1)                                                   
         NI    CNTL,X'FF'-(CNTLBYRL)      TURNOFF BUYER LIST                    
         CLI   0(R6),C'+'                 DOES IT END WITH A '+'                
         BNE   VKBUYR03                                                         
         BCTR  R1,0                       REDUCE LENTH FOR EX                   
         OI    CNTL,CNTLBYRL              YES, THERE'S A LIST                   
         B     VKBUYR04                                                         
*                                                                               
VKBUYR03 CLI   5(R2),3                                                          
         BH    INVLFLD                    4 BYTES NOT A '+' IS NOT GOOD         
*                                                                               
VKBUYR04 EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUYRC(0),8(R2)                                                   
         GOTO1 VALIBUYR,DMCB,BUYRC        VALIDATE IT                           
         JNE   INVLFLD                                                          
*                                                                               
         MVC   BUYRL(L'BUYRC),BUYRC       BUYER CODE TO LIST                    
         MVI   NBYR,1                     SET 1 IN LIST                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,BYRDCDQ             GET BUYER ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   VKBUYR20                                                         
         USING BYRDSCD,R6                                                       
*                                                                               
         MVC   INBOXSRT,BYRINSRT          AKAT ADDED 03/13/02                   
         MVC   TODO,BYRTODO               AKAT ADDED 03/13/02                   
         TM    CNTL,CNTLBYRL              BUYER LIST ?                          
         BNO   VKBUYR20                   NOT LIST                              
         MVC   BUYRL+(L'BUYRC)(L'BUYRL-L'BUYRC),BYRBLIST                        
         OC    BUYRL,SPACES                                                     
         LA    R0,MXBYR                   COUNT NUMBER OF BUYERS                
         SR    R1,R1                                                            
         LA    R3,BUYRL                                                         
         CLC   0(3,R3),SPACES                                                   
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         LA    R3,3(R3)                                                         
         BCT   R0,*-18                                                          
         STC   R1,NBYR                    SET NUMBER IN LIST                    
*                                                                               
         BCT   R1,VKBUYR10                REDUCE NUMBER IN LIST                 
         B     VKBUYR20                   ONLY ONE, SEQUENCE IS OK              
*                                                                               
VKBUYR10 LR    R0,R1                      R=NUMBER IN LIST(LESS ONE)            
         LA    RE,BUYRL                   RE=A(FIRST IN LIST)                   
VKBUYR12 LA    RF,L'BUYRC(RE)             RF=A(SECOND IN LIST)                  
         CLC   0(L'BUYRC,RE),0(RF)        GET THEM IN SEQUENCE                  
         BH    VKBUYR14                                                         
         LR    RE,RF                                                            
         BCT   R0,VKBUYR12                                                      
         B     VKBUYR20                                                         
*                                                                               
VKBUYR14 XC    0(L'BUYRC,RE),0(RF)        SWITCH THEM                           
         XC    0(L'BUYRC,RF),0(RE)                                              
         XC    0(L'BUYRC,RE),0(RF)                                              
         B     VKBUYR10                   AND START AGAIN                       
*                                                                               
VKBUYR20 MVC   BUYRC,BUYRL         SET BUYER CODE FROM FIRST IN LIST            
         MVI   XBYR,1              SET TO DISPLAY FIRST BUYER                   
         OI    MISCFLG1,MF1KYCHG   KEY CHANGED, REDISPLAY LIST                  
         MVC   BUYRFLD,8(R2)                                                    
         NI    FILTRFLG,X'FF'-FFLGBUYR                                          
*                                                                               
VKBUYR30 CLI   5(R2),0                                                          
         BE    *+8                                                              
         OI    FILTRFLG,FFLGBUYR                                                
         OI    ORLBUYRH+4,X'20'                                                 
*                                                                               
         MVC   AGYMD,BAGYMD                                                     
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT                                                     *         
***********************************************************************         
VKCLNT   LA    R2,ORLCLTH                                                       
         TM    4(R2),X'20'                                                      
         BO    VKORD00                                                          
         CLI   5(R2),0                                                          
         BNE   VKCLT05                                                          
         NI    FILTRFLG,X'FF'-FFLGCLT                                           
         B     VKCLT10                                                          
*                                                                               
VKCLT05  CLI   5(R2),3                                                          
         JH    INVLFLD                                                          
         GOTO1 VALICLT                                                          
         MVC   FLTCLT,BCLT                                                      
         OI    FILTRFLG,FFLGCLT                                                 
*                                                                               
VKCLT10  OI    4(R2),X'20'                                                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE ORDER NUMBER                                               *         
***********************************************************************         
VKORD00  LA    R2,ORLORDRH                                                      
         TM    4(R2),X'20'         DID THIS FIELD CHANGE?                       
         BO    VKSTA00             NO                                           
         CLI   5(R2),0                                                          
         BNE   VKORDR10                                                         
         NI    FILTRFLG,X'FF'-FFLGORD                                           
         B     VKORDR50                                                         
*                                                                               
VKORDR10 TM    4(R2),X'08'         VALID NUMERIC?                               
         JZ    INVLFLD                                                          
         MVC   FLTORDL,5(R2)       EBCDIC LENGTH                                
         MVC   FLTORD,8(R2)        EBCDIC (PARTIAL) ORDER NUMBER                
         LLC   R1,FLTORDL                                                       
         SRL   R1,1                                                             
         STC   R1,FLTORDBL         BINARY LENGTH                                
*                                                                               
         MVC   WORK(8),=8C'9'      9'S FILL THE ORDER # FILTER                  
         IC    R1,FLTORDL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),FLTORD                                                   
*                                                                               
         GOTOR BINORDR,DMCB,WORK    GET BINARY ORDER NUMBER                     
         MVC   FLTORDBN,BINORDER                                                
*                                                                               
         MVC   WORK(8),=8C'0'      0'S FILL THE ORDER # FILTER                  
         LLC   R1,FLTORDL          TO GET HIGH ORDER                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),FLTORD                                                   
*                                                                               
         GOTOR BINORDR,DMCB,WORK    GET HIGH BINARY ORDER NUMBER                
         MVC   FLTORDBH,BINORDER                                                
         OI    FILTRFLG,FFLGORD                                                 
*                                                                               
         CLI   FLTORDBL,3          AMBIGUOUS BINARY FILTER LENGTH?              
         BNE   VKORDR50            NO, NO NEED TO MODIFY FLTORDBL               
         LLC   RE,FLTORDBL                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FLTORDBN(0),FLTORDBH                                             
         BE    VKORDR50                                                         
         STC   RE,FLTORDBL                                                      
*                                                                               
VKORDR50 OI    4(R2),X'20'         VALIDATED                                    
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATION                                                    *         
***********************************************************************         
VKSTA00  LA    R2,ORLSTATH                                                      
         TM    4(R2),X'20'                                                      
         BO    VKEDT00                                                          
         CLI   5(R2),0                                                          
         BNE   VKSTA05                                                          
         NI    FILTRFLG,X'FF'-FFLGSTA                                           
         B     VKSTA10                                                          
*                                                                               
VKSTA05  GOTO1 VALISTA                                                          
         MVC   FLTSTA,BSTA                                                      
         OI    FILTRFLG,FFLGSTA                                                 
*                                                                               
VKSTA10  OI    4(R2),X'20'                                                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE ESTIMATE RANGE FILTER                                      *         
***********************************************************************         
VKEDT00  LA    R2,ORLEDATH                                                      
         TM    4(R2),X'20'         DID THIS FIELD CHANGE?                       
         BO    VKEDA160            NO                                           
         MVI   EDATEFLG,0                                                       
         CLI   5(R2),0                                                          
         BE    VKEDA160                                                         
*                                                                               
         OC    ORLEDAT(L'ORLEDAT),SPACES                                        
         XC    RUNSTCH,RUNSTCH                                                  
         XC    RUNENDCH,RUNENDCH                                                
*                                                                               
         LA    R3,QRTRTAB       SET QUARTERLY DATES                             
         USING QRTRD,R3                                                         
VKEDA010 CLC   QRTRNUM,ORLEDAT                                                  
         BE    VKEDA020                                                         
         LA    R3,QRTRLNQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   VKEDA010                                                         
         B     VKEDA100                                                         
*                                                                               
VKEDA020 MVC   RUNSTCH+2(4),QRTRSTR                                             
         MVC   RUNENDCH+2(4),QRTREND                                            
         DROP  R3                                                               
*                                                                               
         CLI   5(R2),4             IF QUARTER ENTERED                           
         JNE   INVLFLD             MUST HAVE YEAR                               
         CLC   ORLEDAT+2(2),=C'00'                                              
         JL    INVLFLD                                                          
         CLC   ORLEDAT+2(2),=C'99'                                              
         JH    INVLFLD                                                          
*                                                                               
*                                         GET ADDRESS OF GETBROAD               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A1D'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GETBROAD,DMCB                                                    
*                                         GET B'DCAST CALENDAR QRTRS            
         MVC   RUNSTCH(2),ORLEDAT+2                                             
         GOTO1 DATCON,DMCB,(0,RUNSTCH),(3,WORK)                                 
         GOTO1 DATCON,DMCB,(3,WORK),(0,RUNSTCH)                                 
         MVC   RUNENDCH(2),ORLEDAT+2                                            
         GOTO1 DATCON,DMCB,(0,RUNENDCH),(3,WORK)                                
         GOTO1 DATCON,DMCB,(3,WORK),(0,RUNENDCH)                                
         GOTO1 GETBROAD,DMCB,(1,RUNSTCH),WORK,GETDAY,ADDAY                      
         MVC   RUNSTCH,WORK                                                     
         GOTO1 GETBROAD,DMCB,(1,RUNENDCH),WORK,GETDAY,ADDAY                     
         MVC   RUNENDCH,WORK+6                                                  
         OI    EDATEFLG,EDAQUART                                                
         B     VKEDA160                                                         
*                                                                               
*                                         CHECK FOR ACTUAL INPUT DATES.         
VKEDA100 LA    R3,8(R2)                                                         
         ICM   R3,8,5(R2)                                                       
         LA    R4,PERVALST                                                      
         GOTO1 PERVAL,DMCB,(R3),(R4)                                            
         TM    DMCB+4,X'03'                                                     
         JNZ   INVLFLD                                                          
         TM    DMCB+4,X'04'                                                     
         BNO   VKEDA150                                                         
*                                                                               
VKEDA110 SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         AHI   R0,-1                                                            
         AR    R3,R0                                                            
         CLI   0(R3),C'-'          FILTER ON DATE AND BEFORE?                   
         BNE   VKEDA115            NO                                           
         MVI   16(R2),C'-'         YES - PLACE THE MINUS                        
         XC    17(8,R2),17(R2)                                                  
         OI    EDATEFLG,EDAONBEF                                                
         B     *+14                                                             
VKEDA115 OI    EDATEFLG,EDAONAFT                                                
         XC    16(9,R2),16(R2)                                                  
*                                                                               
         USING PERVALD,R4                                                       
         MVC   8(8,R2),PVALCPER                                                 
         OI    6(R2),X'80'                                                      
         TM    EDATEFLG,EDAONBEF                                                
         BO    *+14                                                             
         MVC   RUNSTCH,PVALESTA                                                 
         B     VKEDA160                                                         
         MVC   RUNENDCH,PVALESTA                                                
         B     VKEDA160                                                         
*                                                                               
VKEDA150 OI    EDATEFLG,EDAWITHN                                                
         MVC   ORLEDAT,PVALCPER                                                 
         OI    6(R2),X'80'                                                      
         MVC   RUNSTCH,PVALESTA                                                 
         MVC   RUNENDCH,PVALEEND                                                
*                                                                               
VKEDA160 CLI   EDATEFLG,0                                                       
         BE    VKEDA200                                                         
         TM    FILTRFLG,FFLGBUYR   REQUIRE BUYER                                
         BNZ   *+12                                                             
         LA    R2,ORLBUYRH                                                      
         J     MISSFLD                                                          
         TM    FILTRFLG,FFLGCLT    REQUIRE CLIENT                               
         BNZ   *+12                                                             
         LA    R2,ORLCLTH                                                       
         J     MISSFLD                                                          
*                                                                               
VKEDA200 OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE OPTIONS                                                   *          
**********************************************************************          
VOPT     LA    R2,ORLOPTNH                                                      
*                                                                               
         NI    FILTRFLG,FFLGBUYR+FFLGSTA+FFLGORD+FFLGCLT                        
*                              THESE HAVE THEIR OWN FIELDS                      
         XC    FLTSTAT,FLTSTAT                                                  
         XC    FLTEQAT,FLTEQAT                                                  
         MVI   FLTRFLG2,0                                                       
         MVI   FLTRFLG3,0                                                       
         MVI   OPTNFLG,0                                                        
         MVI   FLTRCLR,0                                                        
         MVI   SSQ,0                                                            
         XC    FLTPRD,FLTPRD              CLEAR BINARY FIELDS                   
         XC    FLTPR2,FLTPR2                                                    
         MVC   FLTPRDC,SPACES             AND CHARACTER FIELDS                  
         MVC   FLTPR2C,SPACES                                                   
*                                                                               
         CLI   5(R2),0             ANY SPECIAL FILTERS?                         
         BNE   VOPT2               YES                                          
*                                                                               
VOPT00   OC    INBOXSRT,INBOXSRT   BUYER HAD INBOX/SORT PREFERENCE?             
         BNZ   VOPT1               YES                                          
         OC    TODO,TODO           BUYER HAD TODO PREFERENCE?                   
         BZ    VOPT11              NO                                           
*                                                                               
VOPT1    LA    R4,ORLOPTN                                                       
         OI    ORLOPTNH+6,X'80'    TRANSMIT OPTIONS FIELD                       
         CLI   INBOXSRT,C'I'       IS INBOX OPTION ON?                          
         BNE   VOPT1A              NO                                           
         MVC   0(6,R4),=C'INBOX='  MOVE 'INBOX=' TO OPTIONS FIELD               
         MVC   6(1,R4),INBOXSRT+1  MOVE A,B,C, OR D TO OPTIONS FIELD            
         ZIC   R1,ORLOPTNH+5       CHANGE THE INPUT LENGTH                      
         AHI   R1,7                                                             
         STC   R1,ORLOPTNH+5                                                    
         LA    R4,7(R4)            BUMP OPTIONS FIELD POINTER                   
         B     VOPT1A10                                                         
*                                                                               
VOPT1A   CLI   INBOXSRT,C'S'       IS SORT OPTION ON?                           
         BNE   VOPT1A05            NO...DISPLAY TO DO OPTIONS                   
         MVC   0(5,R4),=C'SORT='   MOVE 'SORT=' TO OPTIONS FIELD                
         MVC   5(1,R4),INBOXSRT+1  MOVE A,B,C, OR D TO OPTIONS FIELD            
         ZIC   R1,ORLOPTNH+5       CHANGE THE INPUT LENGTH                      
         AHI   R1,6                                                             
         STC   R1,ORLOPTNH+5                                                    
         LA    R4,6(R4)            BUMP OPTIONS FIELD POINTER                   
         B     VOPT1A10                                                         
*                                                                               
VOPT1A05 MVC   0(7,R4),=C'INBOX=A' MOVE 'INBOX=' TO OPTIONS FIELD               
         ZIC   R1,ORLOPTNH+5       CHANGE THE INPUT LENGTH                      
         AHI   R1,7                                                             
         STC   R1,ORLOPTNH+5                                                    
         LA    R4,7(R4)            BUMP OPTIONS FIELD POINTER                   
*                                                                               
VOPT1A10 MVI   0(R4),C','          MOVE ',' TO SCREEN                           
         LA    R4,1(R4)            BUMP OPTIONS FIELD POINTER                   
         ZIC   R1,ORLOPTNH+5       CHANGE INPUT LENGTH FOR COMMA                
         AHI   R1,1                                                             
         STC   R1,ORLOPTNH+5                                                    
*                                                                               
         CLI   TODO,0              ARE THERE ANY TODO OPTIONS?                  
         BNE   VOPT1A20            YES                                          
*                                                                               
         MVC   0(10,R4),=C'TODO=B+S+C'  MOVE IN DEFAULT TODO OPTIONS            
         ZIC   R1,ORLOPTNH+5                                                    
         AHI   R1,10                                                            
         STC   R1,ORLOPTNH+5                                                    
         B     VOPT2                                                            
*                                                                               
VOPT1A20 MVC   0(5,R4),=C'TODO='   MOVE 'TODO=' TO OPTIONS FIELD                
         ZIC   R1,ORLOPTNH+5       CHANGE THE INPUT LENGTH                      
         AHI   R1,5                                                             
         STC   R1,ORLOPTNH+5                                                    
         LA    R4,5(R4)            BUMP OPTIONS FIELD POINTER                   
         LA    R1,TODO                                                          
         LA    R3,3                MAX 3 TODO OPTIONS                           
*                                                                               
VOPT1A30 CLI   0(R1),X'40'         <= SPACE?                                    
         BNH   VOPT2               YES...DONE DISPLAYING TO DO                  
         MVC   0(1,R4),0(R1)       MOVE FROM STORAGE TO SCREEN                  
         ZIC   R2,ORLOPTNH+5       CHANGE THE INPUT LENGTH (FOR LETTER)         
         AHI   R2,1                                                             
         STC   R2,ORLOPTNH+5                                                    
         CLI   1(R1),X'40'         <= SPACE?                                    
         BNH   VOPT2               YES...DONE DISPLAYING TO DO                  
         CHI   R3,1                DON'T MOVE EXTRA '+' IN!!!                   
         BE    VOPT1A40                                                         
         MVI   1(R4),C'+'          MOVE A PLUS IN                               
         ZIC   R0,ORLOPTNH+5       CHANGE THE INPUT LENGTH (FOR + SIGN)         
         AHI   R0,1                                                             
         STC   R0,ORLOPTNH+5                                                    
VOPT1A40 AHI   R1,1                BUMP STORAGE                                 
         AHI   R4,2                BUMP SCREEN                                  
         BCT   R3,VOPT1A30                                                      
*                                                                               
VOPT2    L     R0,AIO3             CLEAR IO3 FOR OUR SCANNER BLOCK              
         LHI   R1,LIOS                                                          
         LR    RE,R1                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R2,ORLOPTNH                                                      
         GOTO1 SCANNER,DMCB,(R2),(X'8F',AIO3)                                   
         CLI   4(R1),0                                                          
         JE    INVLFLD                                                          
*                                                                               
         L     R1,ATIOB            SET UP ERROR MESSAGE CURSOR                  
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         DROP  R1                                                               
*                                                                               
         L     R3,AIO3                                                          
VOPT3    L     R1,ATIOB            SET UP ERROR CURSOR                          
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,4(R3)                                                   
         DROP  R1                                                               
*                                                                               
         CLI   0(R3),0             ANY MORE FILTER FIELDS?                      
         BNE   VOPT5               YES                                          
         CLI   1(R3),0                                                          
         BE    VOPT11              NO, NO MORE                                  
         J     INVLFLD                                                          
*                                                                               
VOPT5    L     R4,AOPTTAB          R4 = A(FILTER TABLE)                         
         USING FLTRDSCT,R4                                                      
VOPT7    CLI   FLTRNLEN,0          END OF TABLE?                                
         JE    INVLFLD             YES, NO SUCH FILTER                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,FLTRLTXT         MATCH ON THIS FILTER?                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLTRTEXT(0),12(R3)                                               
         BE    VOPT9                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLTRNLEN         NO, CHECK NEXT FILTER IN TABLE               
         AR    R4,RF                                                            
         B     VOPT7                                                            
*                                                                               
VOPT9    GOTO1 VOPTN,DMCB,(R3),(R4)                                             
         LA    R3,32(R3)                  NEXT FILTER                           
         B     VOPT3                                                            
         DROP  R4                                                               
         EJECT                                                                  
VOPT11   LA    R2,ORLOPTNH                                                      
         USING FLDHDRD,R2                                                       
         TM    FILTRFLG,FFLGCLT+FFLGSTA+FFLGORD  ANY KEY FILTERS?               
         BNZ   VOPT15                                                           
         CLI   FLDILEN,0                  ANY INPUT IN OPTIONS FIELD ?          
         BNE   VOPT17                                                           
*                                                                               
***                                                                             
* SET INBOX DEFAULTS = INBOX=A,TODO=B+S+C                                       
***                                                                             
         CLI   ORLBUYRH+5,0               ANY BUYER?                            
         BE    VOPT21A                    NO...MISSING FIELD                    
         MVC   ORLOPTN(L'INBXDFLT),INBXDFLT                                     
         LA    R1,L'INBXDFLT              NONE, SETUP DEFAULT                   
         STC   R1,FLDILEN                 SET LENGTHS                           
         STC   R1,FLDOLEN                                                       
         OI    FLDOIND,FOUTTRN            TRANSMIT                              
         MVI   SSQ,C'A'                   DEFAULT IS 'A'                        
         MVI   FLTRCLR,CLRGQ+CLRRQ+CLRKQ  GREEN + RED + BLACK                   
         OI    FLTRFLG2,FF2INBOX          SET INBOX ?                           
         B     VOPT21                     MUST HAVE BUYER                       
*                                                                               
***                                                                             
* SET INBOX DEFAULTS = SORT=A,TODO=B+S+C                                        
***                                                                             
VOPT15   CLI   FLDILEN,0                  ANY INPUT IN OPTIONS FIELD ?          
         BNE   VOPT17                                                           
         MVC   ORLOPTN(L'SORTDFLT),SORTDFLT                                     
         LA    R1,L'SORTDFLT              NONE, SETUP DEFAULT                   
         STC   R1,FLDILEN                 SET LENGTHS                           
         STC   R1,FLDOLEN                                                       
         OI    FLDOIND,FOUTTRN            TRANSMIT                              
         MVI   SSQ,C'D'                   DEFAULT IS 'D'                        
         MVI   FLTRCLR,CLRGQ+CLRRQ+CLRKQ  GREEN + RED + BLACK                   
*                                                                               
***                                                                             
* SET INBOX DEFAULTS = SORT=D IF NO SORT SET                                    
***                                                                             
VOPT17   CLI   SSQ,0                      TEST ANY SEQUENCE                     
         BNE   *+8                                                              
         MVI   SSQ,C'D'                   DEFAULT IS 'D'                        
         CLI   FLTRCLR,0                  ANY COLOR ?                           
         BNE   *+8                                                              
         MVI   FLTRCLR,CLRGQ+CLRRQ+CLRKQ  GREEN + RED + BLACK                   
*                                                                               
         CLI   SSQ,C'G'                   SEQ=G,INBOX=D OR                      
         BE    VOPT19                                                           
         CLI   SSQ,C'C'                   IF SEQUENCE > C                       
         BNH   VOPT19                                                           
         CLI   NBYR,1                     BUYER GROUP                           
         BNH   VOPT19                                                           
         L     R1,ATIOB                   SET UP ERROR MESSAGE CURSOR           
         NI    TIOBINDS-TIOBD(R1),X'FF'-TIOBSETC                                
         LA    R2,ORLBUYRH                                                      
         J     INVLFLD                                                          
*                                                                               
VOPT19   TM    FLTRFLG2,FF2INBOX          IS IT INBOX ?                         
         BNO   VOPTX                                                            
*                                                                               
VOPT21   TM    FILTRFLG,FFLGBUYR          TEST BUYER INPUT                      
         BO    VOPTX                                                            
VOPT21A  L     R1,ATIOB                   SET UP ERROR MESSAGE CURSOR           
         NI    TIOBINDS-TIOBD(R1),X'FF'-TIOBSETC                                
         LA    R2,ORLBUYRH                                                      
         J     NEEDFLDS                                                         
*                                                                               
VOPTX    OI    4(R2),X'20'                OPTIONS PVALIDATED                    
*                                                                               
         CLI   PFKEY,PFBUYQ               PF TO BUY PROGRAM                     
         JNE   XIT                                                              
         B     VR                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GO TO VALIDATION ROUTINES                                           *         
* PARM 1  =     A(SCANNER BLOCK)                                      *         
*      2  =     A(FILTER TABLE ENTRY)                                 *         
***********************************************************************         
VOPTN    NTR1  ,                                                                
         LM    R3,R4,0(R1)                                                      
         USING FLTRDSCT,R4                                                      
         SR    RF,RF                                                            
         ICM   RF,3,FLTRAROU                                                    
         AR    RF,RB                                                            
         BR    RF                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATE FILTER                                                *         
***********************************************************************         
VODAT    CLI   1(R3),0             MUST HAVE SOMETHING                          
         JE    INVLFLD                                                          
*                                                                               
         LA    R2,22(R3)                                                        
         ICM   R2,8,1(R3)                                                       
         LA    R4,PERVALST                                                      
         ICM   R4,8,=X'40'         SINGLE DATE ONLY                             
         GOTO1 PERVAL,DMCB,(R2),(R4)                                            
*                                                                               
         TM    DMCB+4,X'03'                                                     
         JNZ   INVLDATE                                                         
*                                                                               
         XC    ENDDATE,ENDDATE                                                  
         LA    R2,22(R3)                                                        
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         BCTR  R1,0                                                             
         AR    R2,R1                                                            
         CLI   0(R2),C'-'          JAN/01- IS AN END DATE(THRU JAN01)           
         BE    *+8                                                              
         OI    FILTRFLG,FFLGSDAT   JAN/01  IS A START DATE(JAN01 FRWD)          
*                                                                               
         USING PERVALD,R4                                                       
         MVC   LSDATYMD,PVALESTA   SAVE LIST START DATE IN YMD FORMAT           
         DROP  R4                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,LSDATYMD),(15,FULL)                               
         ZAP   DUB,FULL                                                         
         SRP   DUB,61,0                                                         
         SP    DUB,=P'090'                                                      
         SRP   DUB,3,0                                                          
         AP    DUB,FULL+2(2)                                                    
         CVB   R0,DUB                                                           
         LA    RF,ENDDATE          SET FOR END DATE                             
         TM    FILTRFLG,FFLGSDAT   UNLESS IT'S A START DATE                     
         BNO   *+8                                                              
         LA    RF,FILTRSDT         SET FOR START DATE                           
         STCM  R0,3,0(RF)                                                       
         XC    0(L'FILTRSDT,RF),FFS                                             
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE MARKET FILTER                                              *         
***********************************************************************         
VOMKT    CLI   1(R3),0             MUST HAVE SOMETHING                          
         JE    INVLFLD                                                          
         CLI   1(R3),4             SHOULD HAVE ESTIMATE FORMAT                  
         JH    INVLFLD                                                          
*                                                                               
         TM    3(R3),X'80'         VALID NUMERIC?                               
         JZ    INVLFLD                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         BCTR  R1,0                CHANGE IT TO LEFT ALIGNED, ZERO              
         EX    R1,*+8                 FILLED                                    
         B     *+10                                                             
         PACK  DUB,22(0,R3)                                                     
         UNPK  FLTMKT,DUB                                                       
         OI    FLTMKT+3,X'F0'                                                   
         PACK  DUB,FLTMKT                                                       
         CVB   R0,DUB                                                           
         STH   R0,MKTNUM                                                        
         OI    FLTRFLG2,FF2MKT                                                  
         J     XIT                                                              
***********************************************************************         
* VALIDATE PRODUCT FILTER                                             *         
***********************************************************************         
VOPRD    TM    FILTRFLG,FFLGCLT    NEED CLIENT FILTER FIRST                     
         BNZ   VOPRD3                                                           
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         LA    R2,ORLCLTH                                                       
         J     MISSFLD                                                          
         DROP  R1                                                               
*                                                                               
VOPRD3   SR    R0,R0                       XX(X)  OR  XX(X)-XX(X)               
         ICM   R0,1,1(R3)                                                       
         JZ    INVLFLD                                                          
         CHI   R0,7                                                             
         JH    INVLFLD                                                          
*                                                                               
         LA    R3,22(R3)                  R3 TO RIGHT SIDE                      
         LA    R4,FLTPRDC                 FIRST GET CHARACTER PRODUCT           
*                                                                               
VOPRD5   LA    R1,3                       MAX OF 3 BYTES                        
*                                                                               
VOPRD7   MVC   0(1,R4),0(R3)              MOVE 1 BYTE AT A TIME                 
         BCT   R0,*+8                     TEST END OF STRING                    
         B     VOPRD9                                                           
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         CLI   0(R3),C'-'                 STOP AT THE '-'                       
         BE    *+12                                                             
         BCT   R1,VOPRD7                                                        
         J     INVLFLD                                                          
         LA    R3,1(R3)                                                         
         BCT   R0,*+8                     CAN'T END IN A '-'                    
         J     INVLFLD                                                          
         LA    R4,FLTPR2C                 R4=A(SECOND PRODUCT)                  
         B     VOPRD5                                                           
*                                                                               
VOPRD9   CLC   FLTPRDC,=C'POL'            POL SHOULD BE ENTERED AS ***          
         JE    INVLFLD                                                          
         CLC   FLTPRDC,=C'***'                                                  
         BNE   VOPRD11                                                          
         CLC   FLTPR2C,SPACES             NO PIGGYBACKS WITH POL                
         JNE   INVLFLD                                                          
         MVC   FLTPRDC,=C'POL'            NOW MAKE IT POL                       
*                                                                               
VOPRD11  LA    R3,FLTPRDC                 R3=A(FIRST PRODUCT CODE)              
         LA    R4,FLTPRD                  R4=A(FIRST PRODUCT NUMBER)            
         BAS   RE,VOPRD13                 GET THE NUMBER                        
         CLC   FLTPR2C,SPACES             SECOND PRODUCT ?                      
         BE    VOPRDX                                                           
         LA    R3,FLTPR2C                 GET SECOND NUMBER                     
         LA    R4,FLTPR2                                                        
         BAS   RE,VOPRD13                 GET THE NUMBER                        
         B     VOPRDX                                                           
*                                                                               
VOPRD13  LA    RF,SVCLIST                 MATCH CODE TO GET NUMBER              
VOPRD15  CLC   0(L'FLTPRDC,R3),0(RF)                                            
         BNE   *+12                                                             
         MVC   0(L'FLTPRD,R4),3(RF)                                             
         BR    RE                                                               
         LA    RF,4(RF)                   BUMP TO NEXT ENTRY                    
         CLI   0(RF),0                    TEST EOT                              
         BNE   VOPRD15                                                          
         J     INVLFLD                    INVALID PRODUCT                       
*                                                                               
VOPRDX   OI    FILTRFLG,FFLGPRD                                                 
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE ESTIMATE FILTER                                            *         
***********************************************************************         
VOEST    CLI   1(R3),0             MUST HAVE SOMETHING                          
         JE    INVLFLD                                                          
         CLI   1(R3),3             SHOULD HAVE ESTIMATE FORMAT                  
         JH    INVLFLD                                                          
*                                                                               
         TM    3(R3),X'80'         VALID NUMERIC?                               
         JZ    INVLFLD                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         BCTR  R1,0                CHANGE IT TO LEFT ALIGNED, ZERO              
         EX    R1,*+8                 FILLED                                    
         B     *+10                                                             
         PACK  DUB,22(0,R3)                                                     
         UNPK  FLTEST,DUB                                                       
         OI    FLTEST+2,X'F0'                                                   
         PACK  DUB,FLTEST                                                       
         CVB   R0,DUB                                                           
         STC   R0,ESTNUM                                                        
         OI    FILTRFLG,FFLGEST                                                 
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE POL ORDER NUMBER FILTER                                    *         
***********************************************************************         
VOPORD   CLI   1(R3),0             MUST HAVE SOMETHING                          
         JE    INVLFLD                                                          
         CLI   1(R3),8             SHOULD HAVE ORDER NUMBER FORMAT              
         JH    INVLFLD                                                          
*                                                                               
         TM    3(R3),X'80'         VALID NUMERIC?                               
         JZ    INVLFLD                                                          
*                                                                               
         MVC   FLTPORDL,1(R3)      EBCDIC LENGTH                                
         MVC   FLTPORD,22(R3)      EBCDIC (PARTIAL) ORDER NUMBER                
         SR    R1,R1                                                            
         IC    R1,FLTPORDL                                                      
         SRL   R1,1                                                             
         STC   R1,FLTPORBL         BINARY LENGTH                                
*                                                                               
         MVC   22(8,R3),=8C'9'     9'S FILL THE ORDER # FILTER                  
         IC    R1,FLTPORDL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R3),FLTPORD                                                 
*                                                                               
         MVC   WORK(8),22(R3)                                                   
         GOTOR BINORDR,DMCB,WORK    GET BINARY ORDER NUMBER                     
         MVC   FLTPORBN,BINORDER    SAVE BINARY POL ORDER                       
*                                                                               
         OI    FLTRFLG2,FF2PORDR                                                
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATUS FILTER                                              *         
***********************************************************************         
         USING FLTRDSCT,R4                                                      
VOSTAT   LA    R2,FLTRTEXT         R2 = A(TEXT)                                 
         SR    R1,R1                                                            
         IC    R1,FLTRLTXT         R1= LENGTH OF TEXT                           
         AR    R2,R1                                                            
         DROP  R4                                                               
*                                                                               
         MVC   FLTSTAT,12(R3)                                                   
         MVC   FLTEQAT,0(R2)                                                    
*                                                                               
         CLC   =C'NONE',12(R3)     IF LOOKING FOR NO STATUS                     
         BNE   *+8                                                              
         MVI   FLTSTAT,C'U'        SET TO UNSENT('U')                           
         CLC   =C'*FAXSNT',12(R3)  IF LOOKING FOR *FAXSNT                       
         BNE   *+8                                                              
         MVI   FLTSTAT,C'F'        SET TO FAXSNT('F')                           
         CLC   =C'*EMSENT',12(R3)  IF LOOKING FOR *EMSENT                       
         BNE   *+8                                                              
         MVI   FLTSTAT,C'E'        SET TO EMLSNT('E')                           
*                                                                               
         OI    FILTRFLG,FFLGSTAT                                                
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE REVISION FILTER                                            *         
***********************************************************************         
VOCSWTCH CLI   1(R3),0             CAN'T HAVE  CSWITCH=?????                    
         JNE   INVLFLD                                                          
         OI    FLTRFLG3,FF3CSWTC   FILTERING ON CALL LETTER SWITCH              
         J     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* VALIDATE REVISION FILTER                                            *         
***********************************************************************         
VOREV    CLI   1(R3),0             CAN'T HAVE  REV=??????                       
         JNE   INVLFLD                                                          
         OI    FLTRFLG2,FF2REVSD   FILTERING ON REVISED ORDERS                  
         J     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PARTIAL CONFIRM FILTER                                     *         
***********************************************************************         
VOPCF    CLI   1(R3),0             CAN'T HAVE  PCFM=??????                      
         JNE   INVLFLD                                                          
         OI    FLTRFLG2,FF2PCNFM   FILTERING ON REVISED ORDERS                  
         J     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* VALIDATE NOT CONFIRMED FILTER                                      *          
***********************************************************************         
VONCF    CLI   1(R3),0             CAN'T HAVE  NCFM=??????                      
         JNE   INVLFLD                                                          
         OI    FLTRFLG2,FF2NCNFM   FILTERING ON REVISED ORDERS                  
         J     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTIVE OFFER FILTER                                        *         
***********************************************************************         
VOAOFR   CLI   1(R3),0                                                          
         JNE   INVLFLD             CAN'T HAVE ACTOFR=??????                     
         OI    FLTRFLG2,FF2AOFFR   FILTERING ON ACTIVE OFFERS                   
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE CONFIRM ORDER FILTER                                       *         
***********************************************************************         
VOACNFM  CLI   1(R3),0                                                          
         JNE   INVLFLD             CAN'T HAVE ACNFM=??????                      
         OI    FLTRFLG2,FF2ACNFM   FILTERING ON ALL CONFIRM ORDERS              
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE AMEND FILTER                                                         
***********************************************************************         
VOREJECT CLI   1(R3),0                                                          
         JNE   INVLFLD             CAN'T HAVE AMEND=??????                      
*                                                                               
         USING FLTRDSCT,R4                                                      
         LA    R2,FLTRTEXT         R2 = A(TEXT)                                 
         SR    R1,R1                                                            
         IC    R1,FLTRLTXT         R1= LENGTH OF TEXT                           
         AR    R2,R1                                                            
         DROP  R4                                                               
                                                                                
         MVC   FLTSTAT,12(R3)                                                   
         MVC   FLTEQAT,0(R2)                                                    
*                                                                               
         CLC   =C'AMEND',12(R3)    LOOKING FOR AMEND?                           
         BNE   *+12                                                             
         OI    FLTRFLG3,FF3AMEND   FILTERING ON AMENDED ORDERS                  
         J     XIT                                                              
         OI    FLTRFLG3,FF3REJCT   FILTERING ON REJECT ORDERS                   
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISPLAY DISK ADDRESS                                       *         
***********************************************************************         
VODAD    CLI   1(R3),0                                                          
         JNE   INVLFLD             CAN'T HAVE DAD=                              
         CLI   TWAOFFC,C'*'                                                     
         JNE   INVLFLD             MUST BE DDS                                  
         OI    OPTNFLG,OPTNDAD     DISPLAY DISK ADDRESS                         
         J     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DISPLAY RECORD COUNT MESSAGE                               *         
***********************************************************************         
VORCNT   CLI   1(R3),0                                                          
         JNE   INVLFLD             CAN'T HAVE RCNT=                             
         CLI   TWAOFFC,C'*'                                                     
         JNE   INVLFLD             MUST BE DDS                                  
         OI    OPTNFLG,OPTNRCNT    DISPLAY RECORD COUNT                         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE INBOX=X   AND SORT=X                                       *         
***********************************************************************         
VOINBX   OI    FLTRFLG2,FF2INBOX          SET INBOX FLAG                        
         LA    R6,INBXSEQ                                                       
         B     *+8                                                              
VOSORT   LA    R6,SORTSEQ                                                       
         CLI   SSQ,0                                                            
         BNE   INVLFLD                    ONLY ONE SEQUENCE ALLOWED             
         CLI   1(R3),1                                                          
         JNE   INVLFLD                                                          
VOSORT3  CLI   0(R6),X'FF'                TEST EOT                              
         BE    INVLFLD                                                          
         CLC   22(1,R3),0(R6)             MATCH INPUT TO TABLE                  
         BE    *+12                                                             
         LA    R6,L'SORTSEQ(R6)                                                 
         B     VOSORT3                                                          
         MVC   SSQ,1(R6)                  SET SORT SEQUENCE                     
         J     XIT                                                              
*                                                                               
INBXSEQ  DS    0CL2                       INBOX      SEQUENCE                   
         DC    C'AA'                      INBOX=A    SEQ=A                      
         DC    C'BB'                      INBOX=B    SEQ=B                      
         DC    C'CC'                      INBOX=C    SEQ=C                      
         DC    C'DG'                      INBOX=D    SEQ=G                      
         DC    X'FF'                                                            
*                                                                               
SORTSEQ  DS    0CL2                       SORT       SEQUENCE                   
         DC    C'AD'                      SORT=A     SEQ=D                      
         DC    C'BE'                      SORT=B     SEQ=E                      
         DC    C'CF'                      SORT=C     SEQ=F                      
         DC    C'DH'                      SORT=D     SEQ=H                      
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE TODO  FILTER                                               *         
***********************************************************************         
VOTODO   SR    R0,R0                                                            
         ICM   R0,1,1(R3)                 R0=LENGTH OF RIGHT SIDE               
         JZ    INVLFLD                                                          
         CLI   FLTRCLR,0                  CAN'T HAVE 2 INPUTS                   
         BNE   INVLFLD                                                          
         LA    R2,22(R3)                                                        
VOTODO3  L     R6,ADCLRTAB                                                      
VOTODO5  CLC   0(1,R2),2(R6)              INPUT VS. TODO CODE                   
         BE    VOTODO7                                                          
         LA    R6,L'CLRTAB(R6)                                                  
         CLI   0(R6),X'FF'                                                      
         JE    INVLFLD                    DOES NOT MATCH TABLE                  
         B     VOTODO5                                                          
*                                                                               
VOTODO7  OC    FLTRCLR,1(R6)              TURN ON COLOR FILTER                  
         LA    R2,1(R2)                                                         
         BCT   R0,*+8                                                           
         J     XIT                                                              
         CLI   0(R2),C'+'                 TODO=B+S+C                            
         BNE   INVLFLD                                                          
         LA    R2,1(R2)                                                         
         BCT   R0,VOTODO3                                                       
         J     INVLFLD                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                       *         
***********************************************************************         
VR       DS    0H                                                               
         LA    R2,ORLSEL1H         CHECK ALL SELECT FIELDS                      
         USING LINDSECT,R2                                                      
*                                                                               
VRSEL05  LA    R0,ORLPFLNH                                                      
         CR    R2,R0                                                            
         BNL   VRSELX                                                           
*                                                                               
         OC    LINORD,LINORD                                                    
         BNZ   VRSEL10                                                          
VRSEL07  XC    8(L'ORLSEL1,R2),8(R2)   CLEAR SELECT FIELD                       
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'             TRANSMIT                                 
         J     INVLFLD                                                          
*                                                                               
VRSEL10  DS    0H                                                               
         CLI   8(R2),C'B'                 GO TO BUY PROGRAM?                    
         BE    VRSEL25                                                          
*                                         CHECK PFKEY                           
         LH    R3,CURDISP                 R3=CURSOR POSITION                    
         AR    R3,RA                                                            
         CR    R3,R2                                                            
         BL    VRSEL100                   CURSOR BEFORE CURRENT                 
         LA    R1,LINLNQ(R2)              R1=A(START OF NEXT LINE)              
         CR    R3,R1                                                            
         BNL   VRSEL100                   MUST BE ON HIGHER LINE                
*                                                                               
VRSEL25  LR    R3,R2                                                            
         SR    R3,RA                                                            
         STCM  R3,3,CURSOUT                                                     
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ORLMEDH,,GLVSPMD                          
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC            CAN'T SWITCH TO BUY PROGRAM                  
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',LINBYR,L'LINBYR,GLVSPBYR                  
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
*                                                                               
         MVC   WORK(L'LINCLT),LINCLT                                            
         OC    WORK(L'LINCLT),SPACES                                            
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,L'LINCLT,GLVSPCLT                    
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
*                                                                               
         GOTOR BINORDR,DMCB,LINORD  GET BINARY ORDER NUMBER                     
         XC    KEY,KEY                                                          
         LA    R6,KEY              GET THE DARE ORDER RECORD                    
         USING DAREORDD,R6                                                      
         MVI   DBKTYPE,DBKTYPQ                                                  
         MVI   DBKSUBTY,DBKSTYPQ                                                
         MVC   DBKAGMD,AGYMD                                                    
         MVC   DBKBYR,LINBYR                                                    
         OC    DBKBYR,SPACES                                                    
         MVC   DBKORD,BINORDER                                                  
         MVI   RDUPDATE,C'N'                                                    
         NI    GENSTAT1,X'FF'-RDUPAPPL    TURN OFF RDUPDATE                     
         DROP  R6                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(DBKSTA-DOKEY),KEYSAVE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOIDELQ      GET PRIMARY ID ELEMENT                       
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DOIDELD,R6                                                       
*                                                                               
         GOTO1 CLPACK,DMCB,WORK,HALF                                            
         DROP  R6                                                               
*                                                                               
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,AGYMD                                                     
         MVC   CKEYCLT,HALF                                                     
         GOTO1 HIGH                                                             
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'CKEY),KEYSAVE DID I GET MY RECORD?                         
         BE    *+6                                                              
         DC    H'0'                DIE!!! RECORD SHOULD EXIST!!                 
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         L     R6,AIO2                                                          
         USING CLTHDRD,R6                                                       
         CLI   CPROF,C'0'          TRUE POL?                                    
         BNE   VRSEL30                                                          
VRSEL29  GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'POL',3,GLVSPPRD   PUT POL OUT          
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
         B     VRSEL60                                                          
         DROP  R6                                                               
*                                                                               
VRSEL30  DS    0H                                                               
         CLC   LINPRD(3),=C'***'                                                
         BE    VRSEL29                                                          
         OC    LINPRD,SPACES                                                    
         GOTO1 VGLOBBER,DMCB,=C'PUTD',LINPRD,3,GLVSPPRD                         
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
*                                                                               
         LA    R3,LINPRD                                                        
         LA    RE,L'LINPRD                                                      
VRSEL40  CLI   0(R3),C'-'                                                       
         BE    VRSEL50                                                          
         LA    R3,1(R3)                                                         
         BCT   RE,VRSEL40                                                       
         B     VRSEL60                                                          
*                                                                               
VRSEL50  DS    0H                                                               
         LA    R3,1(R3)                                                         
         GOTO1 VGLOBBER,DMCB,=C'PUTD',(R3),3,GLVSPPR2  2ND PRODUCT              
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
*                                                                               
VRSEL60  DS    0H                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',LINEST,L'LINEST,GLVSPEST                  
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',LINSTA,L'LINSTA,GLVSPSTA                  
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
*                                                                               
         XC    BLOCK(12),BLOCK                                                  
         LA    R1,BLOCK                                                         
         MVC   0(6,R1),=C'SPODAR'  FROM THE SPOT SYSTEM                         
                                                                                
         OI    6(R1),X'80'         HEADLINE CHANGE IN DARE                      
         GOTO1 VGLOBBER,DMCB,=C'PUTD',BLOCK,12,GLVNOTE                          
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
*                                                                               
         XC    BLOCK(GLVXLENQ),BLOCK                                            
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'    FROM THE SPOT SYSTEM                         
         MVC   GLVXFRPR,=C'DAR'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'SPO'    TO THE SPOT SYSTEM                           
         MVC   GLVXTOPR,=C'BUY'    BUY PROGRAM                                  
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
*                                                                               
         OC    SVSELSID,SVSELSID   CAN WE GOTO A SPECIFIC SESSION?              
         BZ    VRSEL95             NO                                           
         OI    GLVXFLG1,GLV1SIDR+GLV1SIDE   SESSION IDS OF CALLER/EE            
         MVC   GLVXSESR(2),SVSELSID                                             
         DROP  R1                                                               
*                                                                               
VRSEL95  GOTO1 VGLOBBER,DMCB,=C'PUTD',BLOCK,GLVXLENQ,GLVXCTL                    
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVSPMKG   DELETE GROUP CODE            
*                                                                               
         XC    8(L'ORLSEL1,R2),8(R2)   CLEAR SELECT FIELD                       
         OI    4(R2),X'20'             VALIDATE IT FOR NEXT TIME                
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'             TRANSMIT                                 
*                                                                               
         MVC   CONHEAD(23),=CL23'** BACK TO SPOT/DARE **'                       
         OI    CONHEADH+6,X'80'                                                 
         B     VRSELX                                                           
*                                                                               
VRSEL100 DS    0H                                                               
         XC    8(L'ORLSEL1,R2),8(R2)   CLEAR SELECT FIELD                       
         OI    4(R2),X'20'             VALIDATE IT FOR NEXT TIME                
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'             TRANSMIT                                 
*                                                                               
VRSELNX  DS    0H                                                               
         LA    R2,LINNEXTL-LINDSECT(R2)                                         
         B     VRSEL05                                                          
         DROP  R2                                                               
*                                                                               
VRSELX   DS    0H                                                               
         TM    CNTL,CNTLMORE+CNTLSAME                                           
         BZ    VRX                                                              
*                                                                               
         SR    R0,R0                                                            
         LA    R1,ORLPFLNH                                                      
*                                                                               
VRSELX10 ICM   R0,1,0(R1)                                                       
         BZ    VRSELX20                                                         
         AR    R1,R0                                                            
         B     VRSELX10                                                         
VRSELX20 MVC   1(2,R1),=X'0101'    RETRANSMIT SCREEN                            
         OI    GENSTAT2,DISTHSPG                                                
*                                                                               
VRX      DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERAL POOL                                          *         
***********************************************************************         
INBXDFLT DC    C'INBOX=A,TODO=B+S+C'                                            
*                                                                               
SORTDFLT DC    C'SORT=A,TODO=B+S+C'                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLE OF QUARTERLY START/END DATES - SEE QRTRD                      *         
***********************************************************************         
QRTRTAB  DC    C'1Q',C'0115',C'0315'                                            
         DC    C'2Q',C'0415',C'0615'                                            
         DC    C'3Q',C'0715',C'0915'                                            
         DC    C'4Q',C'1015',C'1215'                                            
         DC    X'FF'                                                            
*                                                                               
         DROP  RB,R7                                                            
         EJECT                                                                  
***********************************************************************         
* WRITE/ADD COLOR PASSIVE KEY, AND UPDATE COLOR ELEMENT                         
*                                                                               
* ON ENTRY: PARAM1                 NEW COLOR(1 CHAR)                            
*           DISKADDR IS SET                                                     
*           BINORDER IS SET                                                     
*           BUYER SAVED IN WORK                                                 
*                                                                               
***********************************************************************         
COLPASKY NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         MVC   BYTE,0(R2)                                                       
*                                                                               
         L     R6,AIO                                                           
         USING DOKEY,R6                                                         
         MVC   PREVKEY,DOKEY                                                    
         MVI   ELCODE,COLELQ                                                    
         BAS   RE,GETEL                                                         
         BE    CPK020                                                           
*                                                                               
* ADD NEW COLOR ELEMENT THEN ADD PASSIVE KEY                                    
*                                                                               
         LA    R2,COLOELEM                                                      
         USING COLOREL,R2                                                       
         XC    COLOELEM,COLOELEM                                                
         MVI   COLEL,COLELQ                                                     
         MVI   COLELLEN,COLLENQ                                                 
         MVC   COLCOL,BYTE                                                      
         MVC   COLDATE,CFFDATE                                                  
         DROP  R2                                                               
*                                                                               
         L     R6,AIO                                                           
         USING DOKEY,R6                                                         
         LA    R6,DORFRST                                                       
CPK005   CLI   0(R6),0             IF NO MORE ELEMENTS                          
         BE    CPK010              ADD AT TO THE END                            
         CLI   0(R6),COLELQ        ELSE ADD AFTER ALL ELEMENTS                  
         BNL   CPK010              < X'13'                                      
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CPK005                                                           
CPK010   GOTO1 RECUP,DMCB,(C'S',AIO),COLOELEM,(R6)                              
         DROP  R6                                                               
*                                                                               
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVI   DSCKTYPE,DSCKTYPQ   X'0D'                                        
         MVI   DSCKSTYP,DSCKSTYQ   X'B8'                                        
         MVC   DSCKAGMD,BAGYMD                                                  
         MVC   DSCKBYR,WORK        BUYER IS SAVED IN WORK                       
         OC    DSCKBYR,SPACES                                                   
         MVC   DSCKSTAT,BYTE                                                    
         MVC   DSCKDATE,CFFDATE                                                 
         MVC   DSCKORDR,BINORDER                                                
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BE    CPKUNDEL                                                         
         B     CPKADD                                                           
         DROP  R4                                                               
*                                                                               
* FROM HERE, COLOR ELEMENT EXISTS                                               
*                                                                               
         USING COLOREL,R6                                                       
CPK020   LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVI   DSCKTYPE,DSCKTYPQ   X'0D'                                        
         MVI   DSCKSTYP,DSCKSTYQ   X'B8                                         
         MVC   DSCKAGMD,BAGYMD                                                  
         MVC   DSCKBYR,WORK        BUYER IS SAVED IN WORK                       
         OC    DSCKBYR,SPACES                                                   
         MVC   DSCKSTAT,COLCOL                                                  
         MVC   DSCKDATE,COLDATE                                                 
         MVC   DSCKORDR,BINORDER                                                
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   CPKUPDAT            NO PASSIVE KEY (MUST RESTORE KEY)            
*                                                                               
         CLC   DSCKSTAT,BYTE                                                    
         BNE   CPKDELET            TURN DELETE BIT ON                           
         CLC   DSCKDATE,CFFDATE                                                 
         BNE   CPKDELET            TURN DELETE BIT ON                           
*                                                                               
* UNDELETE THE KEY AND UPDATE                                                   
*                                                                               
CPKUNDEL TM    KEY+13,X'80'                                                     
         BZ    CPKXIT              JUST EXIT                                    
CPKWRITE NI    KEY+13,X'FF'-X'80'  ELSE UNDELETE,                               
         GOTO1 WRITE               UPDATE KEY THEN                              
         B     CPKXIT              EXIT                                         
*                                                                               
CPKDELET DS    0H                                                               
         TM    KEY+13,X'80'        DELETE BIT ON?                               
         BNZ   CPKUPDAT            NO                                           
         OI    KEY+13,X'80'        YES: TURN DELETE BIT ON                      
         GOTO1 WRITE               AND UPDATE KEY                               
*                                                                               
*  UPDATE COLOR ELEMENT AND WRITE/ADD PASSIVE KEY                               
CPKUPDAT DS    0H                                                               
         MVC   COLCOL,BYTE                                                      
         MVC   COLDATE,CFFDATE                                                  
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE THE KEY                              
         MVC   DSCKSTAT,BYTE                   NEW COLOR                        
         MVC   DSCKDATE,CFFDATE                TODAYS DATE                      
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   CPKADD              KEY DOESN'T EXISTS, ADD IT                   
         CLC   DISKADDR,KEY+14     SAME DISK ADDRESS AS WELL?                   
         BE    CPKUNDEL            KEY EXISTS, UNDELETE IT                      
         MVC   KEY+14(4),DISKADDR  DIFFERENT KEY                                
         B     CPKWRITE                                                         
*                                  ELSE ADD                                     
CPKADD   DS    0H                                                               
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVC   KEY,KEYSAVE                                                      
         MVC   DSCKSTAT,BYTE                                                    
         MVC   DSCKDATE,CFFDATE                                                 
         MVC   KEY+14(4),DISKADDR                                               
         GOTO1 ADD                                                              
         DROP  R4                                                               
*                                                                               
CPKXIT   L     R6,AIO              RESTORE KEY BECAUSE A WRITE CLOBBERS         
         USING DOKEY,R6                WHAT IS IN AIO                           
         MVC   DOKEY,PREVKEY                                                    
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF PRODUCT/ESTIMATES THAT ARE WITHIN DATE RANGE                   
*                                                                               
*  ON EXIT : CC       EQ = NO VALID ESTIMATE                                    
*                                                                               
***********************************************************************         
BLDEST   NTR1  BASE=*,LABEL=*                                                   
         XC    ESTTAB,ESTTAB                                                    
         MVI   ESTTAB,X'FF'                                                     
         CLI   EDATEFLG,0          ANY ESTIMATE RANGE ?                         
         JE    BLDESTX                                                          
         MVI   ESTTAB,0                                                         
*                                                                               
         LA    R4,KEY              CHECK CLIENT RECORD FOR BUY TYPE             
         USING ESTHDRD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,AGYMD                                                     
         MVC   EKEYCLT,FLTCLT                                                   
         MVC   EKEYPRD,=C'POL'                                                  
         TM    FILTRFLG,FFLGPRD    TEST PRODUCT FILTER                          
         BZ    *+10                                                             
         MVC   EKEYPRD,FLTPRDC     ADD PRODUCT TO KEY                           
*                                                                               
         GOTO1 HIGH                                                             
BLDEST10 CLC   KEY(EKEYEST-EKEY),KEYSAVE  AGYMD/CLT/PRD                         
         BNE   BLDESTX                                                          
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         TM    EDATEFLG,EDAQUART   QUARTER FILTER?                              
         BZ    BLDEST30                                                         
         CLC   RUNSTCH,EEND                                                     
         BH    BLDEST70                                                         
         CLC   RUNENDCH,ESTART                                                  
         BL    BLDEST70                                                         
         B     BLDEST60                                                         
*                                                                               
BLDEST30 TM    EDATEFLG,EDAONAFT   ON OR AFTER FILTER?                          
         BZ    BLDEST35                                                         
         CLC   RUNSTCH,ESTART                                                   
         BH    BLDEST70                                                         
         B     BLDEST60                                                         
*                                                                               
BLDEST35 TM    EDATEFLG,EDAONBEF   ON OR BEFORE FILTER?                         
         BZ    BLDEST40                                                         
         CLC   RUNENDCH,EEND                                                    
         BL    BLDEST70                                                         
         B     BLDEST60                                                         
*                                                                               
BLDEST40 TM    EDATEFLG,EDAWITHN   ONLY WITHIN THE DATE FILTER?                 
         BNZ   *+6                                                              
         DC    H'0'                MUST BE ONE OF THESE FILTERS!                
         CLC   RUNSTCH,ESTART                                                   
         BL    BLDEST70                                                         
         CLC   RUNENDCH,EEND                                                    
         BH    BLDEST70                                                         
*                                                                               
BLDEST60 LA    R3,ESTTAB                                                        
         ZIC   R1,EKEYEST          R1 = ESTIMATE NUMBER                         
         LA    R3,0(R1,R3)         POINT TO N'TH ENTRY                          
         MVC   0(1,R3),EKEYEST     MOVE N'TH ESTIMATE NUMBER THERE              
BLDEST70 GOTO1 SEQ                 READ NEXT ESTIMATE!                          
         B     BLDEST10                                                         
*                                                                               
BLDESTX  J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,RB                                                            
         EJECT                                                                  
*                                                                               
NODATARQ EQU   812                 NO DATA TO REPORT                            
*                                                                               
RCNDSPLQ EQU   136                 RECORD(S) XX THRU XX OF XX DISPLAY..         
RDBKDSPQ EQU   137                 READ ORDERS BACK TO MM/DD/YY                 
MORRCDQ  EQU   138                 MORE RECORDS TO READ                         
TOOMANYQ EQU   139                 TOO MANY RECORDS TO DISPLAY                  
ENDLSTQ  EQU   140                 END OF LIST - ENTER TO RETURN TO 1ST         
SELDSPQ  EQU   64                  SELECTION DISPLAYED                          
*                                                                               
***********************************************************************         
* GENERAL ERROR MESSAGES                                                        
***********************************************************************         
*                                                                               
CANTDELE MVI   GERROR1,CANTDEL     CAN'T DELETE RECORD                          
         MVI   GETMSYS,255                                                      
         J     ERREXIT                                                          
CANTDEL  EQU   216                                                              
*                                                                               
MISSFLD  MVI   GERROR1,MISSING                                                  
         J     ERREXIT                                                          
*                                                                               
CANTSWTC MVI   GERROR1,NOBUYPRG    CAN'T SWITCH TO THE BUY PROGRAM              
         J     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         J     ERREXIT                                                          
*                                                                               
INVLDATE MVI   GERROR1,INVDATE                                                  
         J     ERREXIT                                                          
*                                                                               
INVLDEST MVI   GERROR1,INVDEST                                                  
         J     ERREXIT                                                          
*                                                                               
RECXISTS MVI   GERROR1,RECEXIST    RECORD ALREADY EXISTS                        
         J     ERREXIT                                                          
***********************************************************************         
* ERROR MESSAGES (SYSTEM 2)                                                     
***********************************************************************         
NOPOLPRD MVI   GERROR1,CNTBEPOL    PRODUCT CAN'T BE POL                         
         J     ER2EXIT                                                          
*                                                                               
NODATAR  LHI   RF,NODATARQ         NO DATA TO REPORT                            
         STCM  RF,3,GERROR                                                      
         L     R1,ATIOB                   CURSOR TO BUYER FIELD                 
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         LA    R2,ORLBUYRH                                                      
         J     ER2EXIT                                                          
*                                                                               
ER2EXIT  MVI   GETMSYS,2           SPOT MESSAGES                                
         J     ERREXIT                                                          
***********************************************************************         
* ERROR MESSAGES (SYSTEM 23)                                                    
***********************************************************************         
INVLDPF  MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
         J     ERREXIT                                                          
*                                                                               
CANTSNDU MVI   GERROR1,UNDRORDR    CAN'T SEND UNDARED/NODARED ORDER             
         J     ERREXIT                                                          
*                                                                               
CANTWAIT MVI   GERROR1,WAITRESP    ORDER SENT ALRDY,WAITING FOR RESPONS         
         J     ERREXIT                                                          
*                                                                               
CANTSNDC MVI   GERROR1,CONFORDR    CAN'T SEND THIS CONFIRMED ORDER              
         J     ERREXIT                                                          
*                                                                               
NEEDRJCT MVI   GERROR1,GETRJCTD    ORDER HAS TO BE REJECTED FIRST BY...         
         J     ERREXIT                                                          
*                                                                               
NTDARAGY MVI   GERROR1,NOTDARAG    AGENCY IS NOT A VALID DARE TRADING..         
         J     ERREXIT                                                          
*                                                                               
TOOMANY  MVI   GERROR1,TOOMANYQ    TOO MANY RECORDS TO DISPLAY                  
         J     ERREXIT                                                          
*                                                                               
CANTCFM  MVI   GERROR1,CNTMNCFM    CAN ONLY MANUAL CONFIRM FXDLVD ORDER         
         J     ERREXIT                                                          
***********************************************************************         
* ERRORS WITH REPLACEMENT TEXT (&1)                                   *         
***********************************************************************         
BYRALRDY MVI   GERROR1,GOTBYRAL    BUYER &1 ALREADY ASSIGNED TO THIS ..         
         J     ERRRTEXT                                                         
RCNDSPLY TM    OPTNFLG,OPTNRCNT    DISPLAY RECORD COUNT                         
         JNO   RCNDS2                                                           
         MVI   GERROR1,RCNDSPLQ    RECORD(S) XX THRU XX OF XX DISPLAY..         
         OI    GENSTAT2,USMYOK                                                  
         MVI   OKNO,0                                                           
         J     INFRTXT2                                                         
RDBKDSPL MVI   GERROR1,RDBKDSPQ    READ RECORDS BACK TO MM/DD/YY                
         OI    GENSTAT2,USMYOK                                                  
         MVI   OKNO,0                                                           
         J     INFRTXT2                                                         
MORECDS  MVI   GERROR1,MORRCDQ     MORE RECORDS TO READ                         
         OI    GENSTAT2,USMYOK                                                  
         MVI   OKNO,0                                                           
         J     INFRTXT2                                                         
***********************************************************************         
* GENERAL INFO MESSAGES                                                         
***********************************************************************         
NEEDFLDS MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         J     INFEXIT                                                          
*                                                                               
NTRCHNGS MVI   GERROR1,RCDSPCHA    RECORD DISPLAYED - NOW ENTER CHANGES         
         J     INFEXIT                                                          
*                                                                               
RECCHNGD MVI   GERROR1,RCWASCHA    RECORD WAS CHANGED - ENTER NEXT RE..         
         J     INFEXIT                                                          
*                                                                               
RECADDED MVI   GERROR1,NEWRECRD    NEW RECORD HAS BEEN ADDED TO THE F..         
         J     INFEXIT                                                          
*                                                                               
MAKESELS MVI   GERROR1,LSTDISPL    LIST DISPLAYED - SELECT OR HIT ENT..         
         J     INFEXIT                                                          
*                                                                               
RCNDS2   TM    CNTL,CNTLMORE       ANY MORE                                     
         JO    MAKESELS                                                         
         SR    RF,RF                                                            
         ICM   RF,3,TSRTOT         TOTAL IN TSAR                                
         CHI   RF,NLISTQ           TEST ONLY ONE SCREEN                         
         JNH   MAKESELS                                                         
         CLC   TSRLST,TSRTOT                                                    
         JNE   MAKESELS                                                         
         MVI   OKNO,0                                                           
         MVI   GERROR1,ENDLSTQ     END OF LIST - ENTER TO RETURN TO 1ST         
         J     MYINFXIT                                                         
*                                                                               
***********************************************************************         
* INFO MESSAGES (SYSTEM 23)                                                     
***********************************************************************         
TOOMNYIO MVI   GERROR1,MAXNMIOS    MAXIMUM NUMBER OF I/O'S EXCEEDED             
         L     R1,ATIOB            SOUND AN ALARM                               
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBALRM                                                
         J     MYINFXIT                                                         
         DROP  R1                                                               
***********************************************************************         
* MESSAGE ROUTINES                                                              
***********************************************************************         
*                                                                               
INFRTXT2 SR    R2,R2                                                            
INFRTEXT SR    R1,R1                                                            
         J     *+8                                                              
ERRRTEXT LA    R1,1                                                             
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    R3,GETTXTCB                                                      
         USING GETTXTD,R3                                                       
         MVI   GTMSYS,23                                                        
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         LTR   R1,R1                                                            
         JZ    MYINFXIT                                                         
         J     ERREXIT                                                          
         DROP  R3                                                               
*                                                                               
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
XLO      SR    R0,R0                                                            
         CR    R0,RB                                                            
         J     XIT                                                              
XHI      SR    R0,R0                                                            
         CR    RB,R0                                                            
         J     XIT                                                              
YES      CR    RB,RB                                                            
         J     XIT                                                              
NO       LTR   RB,RB                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* FILTER TABLE ENTRIES                                                          
*                                                                               
*        BYTE  0                   LENGTH OF FILTER ENTRY                       
*        BYTES 1-4                 A(ROUTINE FOR THE FILTER)                    
*        BYTE  5                   LENGTH OF FILTER TEXT                        
*        BYTES ????                TEXT FOR FILTER                              
*        BYTE  ???                 STATUS BYTE                                  
***********************************************************************         
OPTTAB   DS    0C                                                               
         DC    AL1(OPTCSWTC-*),AL2(VOCSWTCH-VK),AL1(OPTCSWTC-1-(*+1))           
         DC    C'CSW'                                                           
         DC    AL1(0)                                                           
OPTCSWTC EQU   *                                                                
*                                                                               
         DC    AL1(OPTMKTX-*),AL2(VOMKT-VK),AL1(OPTMKTX-1-(*+1))                
         DC    C'MKT'                                                           
         DC    AL1(0)                                                           
OPTMKTX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTPRDX-*),AL2(VOPRD-VK),AL1(OPTPRDX-1-(*+1))                
         DC    C'PRD'                                                           
         DC    AL1(0)                                                           
OPTPRDX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTESTX-*),AL2(VOEST-VK),AL1(OPTESTX-1-(*+1))                
         DC    C'EST'                                                           
         DC    AL1(0)                                                           
OPTESTX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTOR2X-*),AL2(VOPORD-VK),AL1(OPTOR2X-1-(*+1))               
         DC    C'PORD'                                                          
         DC    AL1(0)                                                           
OPTOR2X  EQU   *                                                                
*                                                                               
         DC    AL1(OPTDATEX-*),AL2(VODAT-VK),AL1(OPTDATEX-1-(*+1))              
         DC    C'DATE'                                                          
         DC    AL1(0)                                                           
OPTDATEX EQU   *                                                                
*                                                                               
         DC    AL1(OPTDADX-*),AL2(VODAD-VK),AL1(OPTDADX-1-(*+1))                
         DC    C'DAD'                                                           
         DC    AL1(0)                                                           
OPTDADX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTRCNTX-*),AL2(VORCNT-VK),AL1(OPTRCNTX-1-(*+1))             
         DC    C'RCNT'                                                          
         DC    AL1(0)                                                           
OPTRCNTX EQU   *                                                                
*                                                                               
         DC    AL1(OPTINBXX-*),AL2(VOINBX-VK),AL1(OPTINBXX-1-(*+1))             
         DC    C'INBOX'                                                         
         DC    AL1(0)                                                           
OPTINBXX EQU   *                                                                
*                                                                               
         DC    AL1(OPTSORTX-*),AL2(VOSORT-VK),AL1(OPTSORTX-1-(*+1))             
         DC    C'SORT'                                                          
         DC    AL1(0)                                                           
OPTSORTX EQU   *                                                                
*                                                                               
         DC    AL1(OPTTODOX-*),AL2(VOTODO-VK),AL1(OPTTODOX-1-(*+1))             
         DC    C'TODO'           TODO=B+S+C                                     
         DC    AL1(0)                                                           
OPTTODOX EQU   *                                                                
*                                                                               
         DC    AL1(OPTUNSX-*),AL2(VOSTAT-VK),AL1(OPTUNSX-1-(*+1))               
         DC    C'UNSENT'                                                        
         DC    AL1(QUNSENT)                                                     
OPTUNSX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTNONX-*),AL2(VOSTAT-VK),AL1(OPTNONX-1-(*+1))               
         DC    C'NONE'                                                          
         DC    AL1(QUNSENT)                                                     
OPTNONX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTSNTX-*),AL2(VOSTAT-VK),AL1(OPTSNTX-1-(*+1))               
         DC    C'*SENT'                                                         
         DC    AL1(DSENT)                                                       
OPTSNTX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTDLVX-*),AL2(VOSTAT-VK),AL1(OPTDLVX-1-(*+1))               
         DC    C'SENT'                                                          
         DC    AL1(DSENT)                                                       
OPTDLVX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTDL2X-*),AL2(VOSTAT-VK),AL1(OPTDL2X-1-(*+1))               
         DC    C'DEL'                                                           
         DC    AL1(QSENT)                                                       
OPTDL2X  EQU   *                                                                
*                                                                               
         DC    AL1(OPTEMLX-*),AL2(VOSTAT-VK),AL1(OPTEMLX-1-(*+1))               
         DC    C'*EMSENT'                                                       
         DC    AL1(DEMSENT)                                                     
OPTEMLX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTFX3X-*),AL2(VOSTAT-VK),AL1(OPTFX3X-1-(*+1))               
         DC    C'*FAXSNT'                                                       
         DC    AL1(DFXSENT)                                                     
OPTFX3X  EQU   *                                                                
*                                                                               
         DC    AL1(OPTFXRX-*),AL2(VOSTAT-VK),AL1(OPTFXRX-1-(*+1))               
         DC    C'*FAXSNT'                                                       
         DC    AL1(DFXRSNT)                                                     
OPTFXRX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTFXCFX-*),AL2(VOSTAT-VK),AL1(OPTFXCFX-1-(*+1))             
         DC    C'BYRCNF'                                                        
         DC    AL1(QBYRCNFM)                                                    
OPTFXCFX EQU   *                                                                
*                                                                               
         DC    AL1(OPTEM2X-*),AL2(VOSTAT-VK),AL1(OPTEM2X-1-(*+1))               
         DC    C'EMAIL'                                                         
         DC    AL1(DEMDLVD)                                                     
OPTEM2X  EQU   *                                                                
*                                                                               
         DC    AL1(OPTFAXX-*),AL2(VOSTAT-VK),AL1(OPTFAXX-1-(*+1))               
         DC    C'FAX'                                                           
         DC    AL1(DFXDLVD)                                                     
OPTFAXX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTFXCX-*),AL2(VOSTAT-VK),AL1(OPTFXCX-1-(*+1))               
         DC    C'FXC'                                                           
         DC    AL1(QFAXCNCL)                                                    
OPTFXCX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTFX2X-*),AL2(VOSTAT-VK),AL1(OPTFX2X-1-(*+1))               
         DC    C'CANFAX'                                                        
         DC    AL1(QFAXCNCL)                                                    
OPTFX2X  EQU   *                                                                
*                                                                               
         DC    AL1(OPTAPPX-*),AL2(VOSTAT-VK),AL1(OPTAPPX-1-(*+1))               
         DC    C'OPEN'                                                          
         DC    AL1(QAPP)                                                        
OPTAPPX  EQU    *                                                               
*                                                                               
         DC    AL1(OPTCF3X-*),AL2(VOSTAT-VK),AL1(OPTCF3X-1-(*+1))               
         DC    C'CFMPND'                                                        
         DC    AL1(QCFMDPND)                                                    
OPTCF3X  EQU   *                                                                
*                                                                               
         DC    AL1(OPTUNDX-*),AL2(VOSTAT-VK),AL1(OPTUNDX-1-(*+1))               
         DC    C'UNDARE'                                                        
         DC    AL1(QUNDARE)                                                     
OPTUNDX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTNOTX-*),AL2(VOSTAT-VK),AL1(OPTNOTX-1-(*+1))               
         DC    C'NOTDARE'                                                       
         DC    AL1(QNODARE)                                                     
OPTNOTX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTNTDX-*),AL2(VOSTAT-VK),AL1(OPTNTDX-1-(*+1))               
         DC    C'NTDAR'                                                         
         DC    AL1(QNODARE)                                                     
OPTNTDX EQU    *                                                                
*                                                                               
         DC    AL1(OPTERRX-*),AL2(VOSTAT-VK),AL1(OPTERRX-1-(*+1))               
         DC    C'ERROR'                                                         
         DC    AL1(QERRORED)                                                    
OPTERRX  EQU    *                                                               
*                                                                               
         DC    AL1(OPTEMPTX-*),AL2(VOSTAT-VK),AL1(OPTEMPTX-1-(*+1))             
         DC    C'EMPTY'                                                         
         DC    AL1(QEMPTY)                                                      
OPTEMPTX EQU    *                                                               
*                                                                               
         DC    AL1(FLTRRCAX-*),AL2(VOSTAT-VK),AL1(FLTRRCAX-1-(*+1))             
         DC    C'RCLAPP'                                                        
         DC    AL1(QRCLAPPR)                                                    
FLTRRCAX EQU   *                                                                
*                                                                               
         DC    AL1(OPTRCDX-*),AL2(VOSTAT-VK),AL1(OPTRCDX-1-(*+1))               
         DC    C'RCLDNT'                                                        
         DC    AL1(QRCLDELN)                                                    
OPTRCDX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTRCCX-*),AL2(VOSTAT-VK),AL1(OPTRCCX-1-(*+1))               
         DC    C'RCLCFM'                                                        
         DC    AL1(QRCLCONF)                                                    
OPTRCCX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTRCRX-*),AL2(VOSTAT-VK),AL1(OPTRCRX-1-(*+1))               
         DC    C'RCLREJ'                                                        
         DC    AL1(QRCLREJD)                                                    
OPTRCRX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTRCUX-*),AL2(VOSTAT-VK),AL1(OPTRCUX-1-(*+1))               
         DC    C'RCLUNK'                                                        
         DC    AL1(QRCLUNKN)                                                    
OPTRCUX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTRCTX-*),AL2(VOSTAT-VK),AL1(OPTRCTX-1-(*+1))               
         DC    C'RCLTRN'                                                        
         DC    AL1(QRCLTRNS)                                                    
OPTRCTX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTAMDX-*),AL2(VOREJECT-VK),AL1(OPTAMDX-1-(*+1))             
         DC    C'AMEND'                                                         
         DC    AL1(QRJCT)                                                       
OPTAMDX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTREJX-*),AL2(VOREJECT-VK),AL1(OPTREJX-1-(*+1))             
         DC    C'REJ'                                                           
         DC    AL1(QRJCT)                                                       
OPTREJX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTRJ2X-*),AL2(VOREJECT-VK),AL1(OPTRJ2X-1-(*+1))             
         DC    C'RJCTED'                                                        
         DC    AL1(QRJCT)                                                       
OPTRJ2X  EQU    *                                                               
*                                                                               
         DC    AL1(OPTAR1X-*),AL2(VOSTAT-VK),AL1(OPTAR1X-1-(*+1))               
         DC    C'*RCL'                                                          
         DC    X'FF'               ALL *RECALL STATUSES                         
OPTAR1X  EQU   *                                                                
*                                                                               
         DC    AL1(OPTARCX-*),AL2(VOSTAT-VK),AL1(OPTARCX-1-(*+1))               
         DC    C'RCL'                                                           
         DC    X'FF'               ALL RECALL STATUSES                          
OPTARCX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTRC1X-*),AL2(VOSTAT-VK),AL1(OPTRC1X-1-(*+1))               
         DC    C'*RECALL'                                                       
         DC    X'FF'               ALL *RECALL STATUSES                         
OPTRC1X  EQU   *                                                                
*                                                                               
         DC    AL1(OPTRCLX-*),AL2(VOSTAT-VK),AL1(OPTRCLX-1-(*+1))               
         DC    C'RECALL'                                                        
         DC    X'FF'               ALL RECALL STATUSES                          
OPTRCLX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTREVX-*),AL2(VOREV-VK),AL1(OPTREVX-1-(*+1))                
         DC    C'REV'                                                           
         DC    X'FF'               ALL REVISION STATUSES                        
OPTREVX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTPCFX-*),AL2(VOPCF-VK),AL1(OPTPCFX-1-(*+1))                
         DC    C'PCFM'                                                          
         DC    X'FF'               ALL PARTIAL CONFIRM STATUSES                 
OPTPCFX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTNC2X-*),AL2(VONCF-VK),AL1(OPTNC2X-1-(*+1))                
         DC    C'NOTCFM'                                                        
         DC    X'FF'               ALL NON CONFIRM STATUSES                     
OPTNC2X  EQU   *                                                                
*                                                                               
         DC    AL1(OPTNCFX-*),AL2(VONCF-VK),AL1(OPTNCFX-1-(*+1))                
         DC    C'NCFM'                                                          
         DC    X'FF'               ALL NON CONFIRM STATUSES                     
OPTNCFX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTAOFX-*),AL2(VOAOFR-VK),AL1(OPTAOFX-1-(*+1))               
         DC    C'ACTOFR'                                                        
         DC    X'FF'               ALL ACTIVE OFFERS                            
OPTAOFX  EQU   *                                                                
*                                                                               
         DC    AL1(OPTCNFX-*),AL2(VOACNFM-VK),AL1(OPTCNFX-1-(*+1))              
         DC    C'CONF'             ALL CONFIRMED TYPE ORDERS                    
         DC    X'FF'                                                            
OPTCNFX  EQU    *                                                               
*                                                                               
         DC    AL1(OPTCF2X-*),AL2(VOACNFM-VK),AL1(OPTCF2X-1-(*+1))              
         DC    C'CNFRMD'           ALL CONFIRMED TYPE ORDERS                    
         DC    X'FF'                                                            
OPTCF2X  EQU    *                                                               
         DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
* ORDER STATUS TABLE                                                            
***********************************************************************         
ORDSTAB  DS    0H                                                               
*                                  ** UNSENT **                                 
ORDUNST  DC    AL1(QUNSENT,ORDUNSTX-*+1),CL6'UNSENT'                            
         DC    AL1(0,0,ORD2XREV)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDUNSTX-*)/L'ORDDATA)                                      
ORDUNSTX EQU   *                                                                
*                                  ** SENT **                                   
ORDSENT  DC    AL1(DSENT,ORDSENTX-*+1),CL6'SENT'                                
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDSENTX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVSNT'                                          
         DC    AL1(MF2VAROR),C'VARSNT'                                          
ORDSENTX EQU   *                                                                
*                                  ** EMAIL SENT **                             
ORDEMLS  DC    AL1(DEMSENT,ORDEMLSX-*+1),CL6'EMSENT'                            
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXSX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REMSNT'                                          
         DC    AL1(MF2VAROR),C'VEMSNT'                                          
ORDEMLSX EQU   *                                                                
*                                  ** FAX SENT **                               
ORDFAXS  DC    AL1(DFXSENT,ORDFAXSX-*+1),CL6'FAXSNT'                            
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXSX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'RFXSNT'                                          
         DC    AL1(MF2VAROR),C'VFXSNT'                                          
ORDFAXSX EQU   *                                                                
*                                  ** FAX RESENT ONLY SHOWS AS FAX SENT         
ORDFAXRS DC    AL1(DFXRSNT,ORDFXRSX-*+1),CL6'FAXSNT'                            
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFXRSX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'RFXSNT'                                          
         DC    AL1(MF2VAROR),C'VFXSNT'                                          
ORDFXRSX EQU   *                                                                
*                                  ** APPROVED **                               
ORDAPPR  DC    AL1(QAPP,ORDAPPRX-*+1),CL6'OPENED'                               
         DC    AL1(0,ORDINSTS,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDAPPRX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVOPN'                                          
         DC    AL1(MF2VAROR),C'VAROPN'                                          
ORDAPPRX EQU   *                                                                
*                                  ** CONFIRM PENDING **                        
ORDCFPD  DC    AL1(QCFMDPND,ORDCFPDX-*+1),CL6'CFMPND'                           
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDCFPDX-*)/L'ORDDATA)                                      
ORDCFPDX EQU   *                                                                
*                                  ** CONFIRMED **                              
ORDCNFM  DC    AL1(QCFMD,ORDCNFMX-*+1),CL6'CNFRMD'                              
         DC    AL1(ORDTNCMT,ORDIFTST+ORDINSTS,ORD2ACFM)                         
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDCNFMX-*)/L'ORDDATA)                                      
         DC    AL1(MF2OFFER),C'OFFERS'                                          
         DC    AL1(MF2CANCF),C'CANCFM'                                          
         DC    AL1(MF2REVOR),C'REVCNF'                                          
         DC    AL1(MF2VAROR),C'VARCNF'                                          
ORDCNFMX EQU   *                                                                
*                                  ** CONFIRMED WITH COMMENTS **                
ORDCNFC  DC    AL1(QCFMD,ORDCNFCX-*+1),CL6'**PCFM'                              
         DC    AL1(ORDTCMTS,ORDINSTS,ORD2ACFM)                                  
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDCNFCX-*)/L'ORDDATA)                                      
         DC    AL1(MF2OFFER),C'OFFERS'                                          
         DC    AL1(MF2REVOR),C'**RPCF'                                          
         DC    AL1(MF2VAROR),C'**VPCF'                                          
ORDCNFCX EQU   *                                                                
*                                  ** ERROR **                                  
ORDERR   DC    AL1(QERRORED,ORDERRX-*+1),CL6'ERROR'                             
         DC    AL1(0,ORDIXREV+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDERRX-*)/L'ORDDATA)                                       
         DC    AL1(MF2EMAIL),C'EMERR '                                          
         DC    AL1(MF2FAXED),C'FXERR '                                          
ORDERRX  EQU   *                                                                
*                                  ** EMAIL DELIVERED **                        
ORDEMLD  DC    AL1(DEMDLVD,ORDEMLDX-*+1),CL6'EMDLVD'                            
         DC    AL1(0,ORDINSTS,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDEMLDX-*)/L'ORDDATA)                                      
ORDEMLDX EQU   *                                                                
*                                  ** FAX DELIVERED **                          
ORDFAXD  DC    AL1(DFXDLVD,ORDFAXDX-*+1),CL6'FXDLVD'                            
         DC    AL1(0,ORDINSTS,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXDX-*)/L'ORDDATA)                                      
ORDFAXDX EQU   *                                                                
*                                  ** FAX CANCELLED **                          
ORDFAXC  DC    AL1(QFAXCNCL,ORDFAXCX-*+1),CL6'FXERR '                           
         DC    AL1(0,ORDINSTS,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDFAXCX-*)/L'ORDDATA)                                      
ORDFAXCX EQU   *                                                                
*                                  ** MANUAL CONFIRMED **                       
ORDCNFX  DC    AL1(QBYRCNFM,ORDCNFXX-*+1),CL6'BYRCNF'                           
         DC    AL1(0,ORDINSTS+ORDIFTST,ORD2ACFM)                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDCNFXX-*)/L'ORDDATA)                                      
         DC    AL1(MF2OFFER),C'OFFERS'                                          
ORDCNFXX EQU   *                                                                
*                                  ** REJECTED **                               
ORDRJCT  DC    AL1(QRJCT,ORDRJCTX-*+1),CL6'RJCTED'                              
         DC    AL1(ORDTRJCT,0,ORD2REJ)                                          
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRJCTX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVREJ'                                          
         DC    AL1(MF2VAROR),C'VARREJ'                                          
ORDRJCTX EQU   *                                                                
*                                                                               
ORDAMND  DC    AL1(QRJCT,ORDAMNDX-*+1),CL6'AMEND'                               
         DC    AL1(ORDTAMND,0,ORD2REJ)                                          
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDAMNDX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVAMD'                                          
         DC    AL1(MF2VAROR),C'REVAMD'                                          
ORDAMNDX EQU   *                                                                
*                                  ** EMPTY **                                  
ORDEMPT  DC    AL1(QEMPTY,ORDEMPTX-*+1),CL6'EMPTY'                              
         DC    AL1(0,ORDIXCLD,0)                                                
         DC    C'K'                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDEMPTX-*)/L'ORDDATA)                                      
ORDEMPTX EQU   *                                                                
*                                  ** NOT DARED **                              
ORDNODA  DC    AL1(QNODARE,ORDNODAX-*+1),CL6'NTDARE'                            
         DC    AL1(0,ORDIFTST+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDNODAX-*)/L'ORDDATA)                                      
ORDNODAX EQU   *                                                                
*                                  ** RECALLED **                               
ORDRECA  DC    AL1(QRECALL,ORDRECAX-*+1),CL6'RECALL'                            
         DC    AL1(0,ORDIRCDA+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRECAX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRECAX EQU   *                                                                
*                                  ** RECALLED APPROVED **                      
ORDRCAP  DC    AL1(QRCLAPPR,ORDRCAPX-*+1),CL6'RECALL'                           
         DC    AL1(0,ORDIRCL,0)                                                 
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCAPX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCAPX EQU   *                                                                
*                                  ** RECALLED - CONFIRMED **                   
ORDRCCF  DC    AL1(QRCLCONF,ORDRCCFX-*+1),CL6'RECALL'                           
         DC    AL1(0,ORDISENT+ORDISRCL+ORDINSTS,0)                              
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCCFX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCCFX EQU   *                                                                
*                                  ** RECALLED - DELIVERED **                   
ORDRCDE  DC    AL1(QRCLDELN,ORDRCDEX-*+1),CL6'RECALL'                           
         DC    AL1(0,ORDIRCL,0)                                                 
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCDEX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCDEX EQU   *                                                                
*                                  ** RECALLED - REJECTED **                    
ORDRCRJ  DC    AL1(QRCLREJD,ORDRCRJX-*+1),CL6'RECALL'                           
         DC    AL1(0,ORDISENT+ORDISRCL+ORDINSTS,0)                              
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCRJX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCRJX EQU   *                                                                
*                                  ** RECALLED UNKNOWN **                       
ORDRCUK  DC    AL1(QRCLUNKN,ORDRCUKX-*+1),CL6'ERROR'                            
         DC    AL1(0,ORDIXREV+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCUKX-*)/L'ORDDATA)                                      
ORDRCUKX EQU   *                                                                
*                                  ** UNDARED **                                
ORDUNDA  DC    AL1(QUNDARE,ORDUNDAX-*+1),CL6'UNDARD'                            
         DC    AL1(0,ORDIFTST+ORDINSTS,0)                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDUNDAX-*)/L'ORDDATA)                                      
ORDUNDAX EQU   *                                                                
*                                  ** RECALLED - TRANSMITTED **                 
ORDRCTR  DC    AL1(QRCLTRNS,ORDRCTRX-*+1),CL6'RECALL'                           
         DC    AL1(0,ORDIRCL,0)                                                 
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCTRX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCTRX EQU   *                                                                
*                                  ** RECALLED - WIP **                         
ORDRCWP  DC    AL1(QRCLWIP,ORDRCWPX-*+1),CL6'RECALL'                            
         DC    AL1(0,ORDIRCL,0)                                                 
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDRCWPX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVRCL'                                          
         DC    AL1(MF2VAROR),C'VARRCL'                                          
ORDRCWPX EQU   *                                                                
*                                  ** SENT PENDING **                           
ORDSNTP  DC    AL1(QSNTPNDG,ORDSNTPX-*+1),CL6'SENT'                             
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDSNTPX-*)/L'ORDDATA)                                      
         DC    AL1(MF2FAXED),C'FAXSNT'                                          
         DC    AL1(MF2EMAIL),C'EMLSNT'                                          
         DC    AL1(MF2REVOR),C'REVSNT'                                          
         DC    AL1(MF2VAROR),C'VARSNT'                                          
ORDSNTPX EQU   *                                                                
*                                  ** SENT CANCELLED PCFM **                    
ORDSNTC  DC    AL1(QSNTXCNF,ORDSNTCX-*+1),CL6'**PCFM'                           
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDSNTCX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'**RPCF'                                          
         DC    AL1(MF2VAROR),C'**VPCF'                                          
ORDSNTCX EQU   *                                                                
*                                  ** SENT CANCELLED RJCT **                    
ORDSNTR  DC    AL1(QSNTXREJ,ORDSNTRX-*+1),CL6'RJCTED'                           
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDSNTRX-*)/L'ORDDATA)                                      
         DC    AL1(MF2REVOR),C'REVREJ'                                          
         DC    AL1(MF2VAROR),C'VARREJ'                                          
ORDSNTRX EQU   *                                                                
*                                  ** SENT **                                   
ORD2BSN  DC    AL1(QTOBESNT,ORD2BSNX-*+1),CL6'SENT'                             
         DC    AL1(0,ORDISENT,0)                                                
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORD2BSNX-*)/L'ORDDATA)                                      
         DC    AL1(MF2FAXED),C'FAXSNT'                                          
         DC    AL1(MF2EMAIL),C'EMLSNT'                                          
         DC    AL1(MF2REVOR),C'REVSNT'                                          
         DC    AL1(MF2VAROR),C'VARSNT'                                          
ORD2BSNX EQU   *                                                                
*                                                                               
ORDEND   DC    AL1(255,ORDENDX-*+1),CL6'ERR999'                                 
         DC    AL1(0,0,0)                                                       
         DC    C' '                                                             
         DC    AL2(0)                                                           
         DC    AL1((ORDENDX-*)/L'ORDDATA)                                       
ORDENDX  EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* KEY TABLE - SEE DKYD                                                          
***********************************************************************         
DKYTAB   DS   0X                                                                
DOK      DC   AL1(DOKSTYPQ,DOKX-*+1)                                            
         DC   AL1((DOKX-*+1)/DKYLNQ)                                            
         DC   AL1(L'DOKAGMD,DOKAGMD-DOKEY,FDAGMQ)     AGENCY/MEDIA              
         DC   AL1(L'DOKORDER,DOKORDER-DOKEY,FDORDQ)   ORDER NUMBER              
         DC   AL1(L'DOKSTA,DOKSTA-DOKEY,FDSTAQ)       STATION                   
DOKX     EQU  *                                                                 
*                                                                               
DBK      DC   AL1(DBKSTYPQ,DBKX-*+1)                                            
         DC   AL1((DBKX-*+1)/DKYLNQ)                                            
         DC   AL1(L'DBKAGMD,DBKAGMD-DOKEY,FDAGMQ)     AGENCY/MEDIA              
         DC   AL1(L'DBKBYR,DBKBYR-DOKEY,FDBYRQ)       BUYER                     
         DC   AL1(L'DBKORD,DBKORD-DOKEY,FDORDQ)       ORDER NUMBER              
         DC   AL1(L'DBKSTA,DBKSTA-DOKEY,FDSTAQ)       STATION                   
DBKX     EQU  *                                                                 
*                                                                               
DCK      DC   AL1(DCKSTYPQ,DCKX-*+1)                                            
         DC   AL1((DCKX-*+1)/DKYLNQ)                                            
         DC   AL1(L'DCKAGMD,DCKAGMD-DOKEY,FDAGMQ)     AGENCY MEDIA              
         DC   AL1(L'DCKCLT,DCKCLT-DOKEY,FDCLTQ)       CLIENT                    
         DC   AL1(L'DCKPRD,DCKPRD-DOKEY,FDPRDQ)       PRODUCT                   
         DC   AL1(L'DCKEST,DCKEST-DOKEY,FDESTQ)       ESTIMATE                  
         DC   AL1(L'DCKSTA,DCKSTA-DOKEY,FDSTAQ)       STATION                   
         DC   AL1(L'DCKPRD2,DCKPRD2-DOKEY,0)          PRODUCT 2                 
         DC   AL1(L'DCKFLTNM,DCKFLTNM-DOKEY,0)        FLIGHT NUMBER             
DCKX     EQU  *                                                                 
*                                                                               
DVK      DC   AL1(DVKSTYPQ,DVKX-*+1)                                            
         DC   AL1((DVKX-*+1)/DKYLNQ)                                            
         DC   AL1(L'DVKAGMD,DVKAGMD-DOKEY,FDAGMQ)     AGENCY/MEDIA              
         DC   AL1(L'DVKPORD,DVKPORD-DOKEY,FDPONQ)     POL ORDER NUMBER          
         DC   AL1(L'DVKBORD,DVKBORD-DOKEY,0)          BRAND ORDER #             
DVKX     EQU  *                                                                 
*                                                                               
DSK      DC   AL1(DSKSTYPQ,DSKX-*+1)                                            
         DC   AL1((DSKX-*+1)/DKYLNQ)                                            
         DC   AL1(L'DSKAGMD,DSKAGMD-DOKEY,FDAGMQ)     AGENCY MEDIA              
         DC   AL1(L'DSKBYR,DSKBYR-DOKEY,FDBYRQ)       BUYER                     
         DC   AL1(L'DSKSTA,DSKSTA-DOKEY,FDSTAQ)       STATION                   
         DC   AL1(L'DSKORD,DSKORD-DOKEY,FDORDQ)       ORDER NUMBER              
DSKX     EQU  *                                                                 
*                                                                               
DSCK     DC   AL1(DSCKSTYQ,DSCKX-*+1)                                           
         DC   AL1((DSCKX-*+1)/DKYLNQ)                                           
         DC   AL1(L'DSCKAGMD,DSCKAGMD-DOKEY,FDAGMQ)   AGENCY MEDIA              
         DC   AL1(L'DSCKBYR,DSCKBYR-DOKEY,FDBYRQ)     BUYER                     
         DC   AL1(L'DSCKSTAT,DSCKSTAT-DOKEY,FDCLRQ)   COLOR STATUS              
         DC   AL1(L'DSCKDATE,DSCKDATE-DOKEY,0)        DATE                      
         DC   AL1(L'DSCKORDR,DSCKORDR-DOKEY,0)        ORDER NUMBER              
DSCKX    EQU  *                                                                 
*                                                                               
MNK      DC   AL1(MNKSTYPQ,MNKX-*+1)                                            
         DC   AL1((MNKX-*+1)/DKYLNQ)                                            
         DC   AL1(L'MNKAGMD,MNKAGMD-MNKEY,FDAGMQ)     AGENCY MEDIA              
         DC   AL1(L'MNKBYR,MNKBYR-MNKEY,FDBYRQ)       BUYER                     
         DC   AL1(L'MNKORDER,MNKORDER-MNKEY,FDORDQ)   ORDER NUMBER              
         DC   AL1(L'MNKGROUP,MNKGROUP-MNKEY,0)        MAKEGOOD GROUP            
MNKX     EQU  *                                                                 
*                                                                               
         DC   X'FF'                                                             
         EJECT                                                                  
***********************************************************************         
* FILTER DATA - SEE FDD                                               *         
***********************************************************************         
FDTAB    DS   0XL(FDLNQ)                                                        
         ORG  FDTAB+FDLNQ                                                       
*                                                                               
*                                  AGENCY MEDIA                                 
FDAGMQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(0),AL2(0)                                                     
         DC   AL1(0),AL2(AGYMD-LSSD)                                            
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  ORDER NUMBER                                 
FDORDQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(0),AL2(0)                                                     
         DC   AL1(0),AL2(0)                                                     
         DC   AL2(SETKORD-BGN)                                                  
         DC   AL2(FLTKORD-BGN)                                                  
*                                                                               
*                                  STATION                                      
FDSTAQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(FFLGSTA),AL2(FILTRFLG-LSSD)                                   
         DC   AL1(0),AL2(FLTSTA-LSSD)                                           
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  BUYER                                        
FDBYRQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(FFLGBUYR),AL2(FILTRFLG-LSSD)                                  
         DC   AL1(0),AL2(BUYRC-LSSD)                                            
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  CLIENT                                       
FDCLTQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(FFLGCLT),AL2(FILTRFLG-LSSD)                                   
         DC   AL1(0),AL2(FLTCLT-LSSD)                                           
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  PRODUCT                                      
FDPRDQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(FFLGPRD),AL2(FILTRFLG-LSSD)                                   
         DC   AL1(0),AL2(FLTPRD-LSSD)                                           
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  ESTIMATE                                     
FDESTQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(FFLGEST),AL2(FILTRFLG-LSSD)                                   
         DC   AL1(0),AL2(ESTNUM-LSSD)                                           
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  POL ORDER NUMBER                             
FDPONQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(FF2PORDR),AL2(FLTRFLG2-LSSD)                                  
         DC   AL1(0),AL2(FLTPORBN-LSSD)                                         
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  COLOR                                        
FDCLRQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(0),AL2(0)                                                     
         DC   AL1(0),AL2(0)                                                     
         DC   AL2(0)                                                            
         DC   AL2(FLTKCLR-BGN)                                                  
         EJECT                                                                  
***********************************************************************         
* LIST DATA DEFINITIONS  (SEE LDD)                                    *         
***********************************************************************         
LDTAB    DS   0XL(LDLNQ)                                                        
         ORG  LDTAB+LDLNQ                                                       
*                                                                               
LDORDQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTORD,LSTORD-LSTD)      ORDER                              
*                                                                               
LDBYRQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTBYR,LSTBYR-LSTD)      BUYER                              
*                                                                               
LDSTCQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTSTC,LSTSTC-LSTD)      STATUS                             
*                                                                               
LDCLTQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTCLT,LSTCLT-LSTD)      CLIENT                             
*                                                                               
LDPRDQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTPRD,LSTPRD-LSTD)      PRODUCT                            
*                                                                               
LDPRDCQ  EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTPRDC,LSTPRDC-LSTD)    PRODUCT CODE                       
*                                                                               
LDPR2Q   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTPR2,LSTPR2-LSTD)      PRODUCT - 2                        
*                                                                               
LDESTQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTEST,LSTEST-LSTD)      ESTIMATE                           
*                                                                               
LDSTAQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTSTA,LSTSTA-LSTD)      STATION                            
*                                                                               
LDMKTQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTMKT,LSTMKT-LSTD)      MARKET                             
*                                                                               
LDMKNQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTMKN,LSTMKN-LSTD)      MARKET NAME                        
*                                                                               
LDFLTQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTFLT,LSTFLT-LSTD)      FLIGHT                             
*                                                                               
LDODTQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTODT,LSTODT-LSTD)      ORDER DATE                         
*                                                                               
LDACTQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTACT,LSTACT-LSTD)      ACTIVITY DATE + TIME               
*                                                                               
LDACTDQ  EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTACTD,LSTACTD-LSTD)    ACTIVITY DATE                      
*                                                                               
LDFLGQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTFLG,LSTFLG-LSTD)      FLAG                               
*                                                                               
LDCLRQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTCLR,LSTCLR-LSTD)      COLOR CODE                         
*                                                                               
LDCLSQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTCLS,LSTCLS-LSTD)      COLOR SEQUENCE                     
*                                                                               
LDDADQ   EQU  ((*-LDTAB)/LDLNQ)                                                 
         DC   AL1(L'LSTDAD,LSTDAD-LSTD)      DISK ADDRESS                       
         EJECT                                                                  
***********************************************************************         
* SORT OPTION TABLE (SEE SSQD)                                        *         
***********************************************************************         
SEQTAB   DS   0X                                                                
SSEQA    DS   0X                               SEQUENCE 'A'   INBOX=A           
         DC   C'A',AL1(SSEQAX-SSEQA,DSCKSTYQ,SSQFBUYR)                          
         DC   AL1((SSEQAX-*)/L'SSQDATA)                                         
         DC   AL1(LDCLSQ,0)                    COLOR SEQUENCE                   
         DC   AL1(LDBYRQ,0)                    BUYER                            
         DC   AL1(LDMKNQ,0)                    MARKET                           
         DC   AL1(LDSTAQ,0)                    STATION                          
         DC   AL1(LDCLTQ,0)                    CLIENT                           
         DC   AL1(LDORDQ,SSQDIEND)             ORDER                            
*                                                                               
         DC   AL1(LDSTCQ,0)                    STATUS                           
         DC   AL1(LDPRDQ,0)                    PRODUCT                          
         DC   AL1(LDPR2Q,0)                    PRODUCT-2                        
         DC   AL1(LDESTQ,0)                    ESTIMATE                         
         DC   AL1(LDFLTQ,0)                    FLIGHT                           
         DC   AL1(LDODTQ,0)                    ORDER DATE                       
         DC   AL1(LDACTQ,0)                    ACTIVITY DATE+TIME               
         DC   AL1(LDFLGQ,0)                    FLAG                             
         DC   AL1(LDCLRQ,0)                    COLOR CODE                       
         DC   AL1(LDDADQ,0)                    DISK ADDRESS                     
SSEQAX   EQU  *                                                                 
*                                                                               
SSEQB    DS   0X                               SEQUENCE 'B'   INBOX=B           
         DC   C'B',AL1(SSEQBX-SSEQB,DSCKSTYQ,SSQFBUYR)                          
         DC   AL1((SSEQBX-*)/L'SSQDATA)                                         
         DC   AL1(LDCLSQ,0)                    COLOR SEQUENCE                   
         DC   AL1(LDBYRQ,0)                    BUYER                            
         DC   AL1(LDCLTQ,0)                    CLIENT                           
         DC   AL1(LDMKNQ,0)                    MARKET                           
         DC   AL1(LDSTAQ,0)                    STATION                          
         DC   AL1(LDORDQ,SSQDIEND)             ORDER                            
*                                                                               
         DC   AL1(LDSTCQ,0)                    STATUS                           
         DC   AL1(LDPRDQ,0)                    PRODUCT                          
         DC   AL1(LDPR2Q,0)                    PRODUCT - 2                      
         DC   AL1(LDESTQ,0)                    ESTIMATE                         
         DC   AL1(LDFLTQ,0)                    FLIGHT                           
         DC   AL1(LDODTQ,0)                    ORDER DATE                       
         DC   AL1(LDACTQ,0)                    ACTIVITY DATE+TIME               
         DC   AL1(LDFLGQ,0)                    FLAG                             
         DC   AL1(LDCLRQ,0)                    COLOR CODE                       
         DC   AL1(LDDADQ,0)                    DISK ADDRESS                     
SSEQBX   EQU  *                                                                 
*                                                                               
SSEQC    DS   0X                               SEQUENCE 'C'   INBOX=C           
         DC   C'C',AL1(SSEQCX-SSEQC,DSCKSTYQ,SSQFBUYR)                          
         DC   AL1((SSEQCX-*)/L'SSQDATA)                                         
         DC   AL1(LDACTQ,0)                    ACTIVITY DATE+TIME               
         DC   AL1(LDORDQ,SSQDIEND)             ORDER                            
*                                                                               
         DC   AL1(LDBYRQ,0)                    BUYER                            
         DC   AL1(LDODTQ,0)                    ORDER DATE                       
         DC   AL1(LDSTCQ,0)                    STATUS                           
         DC   AL1(LDCLTQ,0)                    CLIENT                           
         DC   AL1(LDPRDQ,0)                    PRODUCT                          
         DC   AL1(LDPR2Q,0)                    PRODUCT - 2                      
         DC   AL1(LDESTQ,0)                    ESTIMATE                         
         DC   AL1(LDSTAQ,0)                    STATION                          
         DC   AL1(LDMKTQ,0)                    MARKET                           
         DC   AL1(LDFLTQ,0)                    FLIGHT                           
         DC   AL1(LDFLGQ,0)                    FLAG                             
         DC   AL1(LDCLRQ,0)                    COLOR CODE                       
         DC   AL1(LDCLSQ,0)                    COLOR SEQUENCE                   
         DC   AL1(LDDADQ,0)                    DISK ADDRESS                     
SSEQCX   EQU  *                                                                 
*                                                                               
SSEQD    DS   0X                               SEQUENCE 'D'   SORT=A            
         DC   C'D',AL1(SSEQDX-SSEQD,DCKSTYPQ,SSQFCLPR)                          
         DC   AL1((SSEQDX-*)/L'SSQDATA)                                         
         DC   AL1(LDCLTQ,0)                    CLIENT                           
         DC   AL1(LDPRDCQ,0)                   PRODUCT CODE                     
         DC   AL1(LDESTQ,0)                    ESTIMATE                         
         DC   AL1(LDSTAQ,0)                    STATION                          
         DC   AL1(LDFLTQ,0)                    FLIGHT                           
         DC   AL1(LDORDQ,SSQDIEND)             ORDER                            
*                                                                               
         DC   AL1(LDPRDQ,0)                    PRODUCT                          
         DC   AL1(LDSTCQ,0)                    STATUS                           
         DC   AL1(LDBYRQ,0)                    BUYER                            
         DC   AL1(LDPR2Q,0)                    PRODUCT - 2                      
         DC   AL1(LDMKTQ,0)                    MARKET                           
         DC   AL1(LDODTQ,0)                    ORDER DATE                       
         DC   AL1(LDACTQ,0)                    ACTIVITY DATE+TIME               
         DC   AL1(LDFLGQ,0)                    FLAG                             
         DC   AL1(LDCLRQ,0)                    COLOR CODE                       
         DC   AL1(LDCLSQ,0)                    COLOR SEQUENCE                   
         DC   AL1(LDDADQ,0)                    DISK ADDRESS                     
SSEQDX   EQU  *                                                                 
*                                                                               
SSEQE    DS   0X                               SEQUENCE 'E'   SORT=B            
         DC   C'E',AL1(SSEQEX-SSEQE,DBKSTYPQ,SSQFNATV)                          
         DC   AL1((SSEQEX-*)/L'SSQDATA)                                         
         DC   AL1(LDBYRQ,0)                    BUYER                            
         DC   AL1(LDORDQ,SSQDIEND)             ORDER                            
*                                                                               
         DC   AL1(LDSTAQ,0)                    STATION                          
         DC   AL1(LDSTCQ,0)                    STATUS                           
         DC   AL1(LDCLTQ,0)                    CLIENT                           
         DC   AL1(LDPRDQ,0)                    PRODUCT                          
         DC   AL1(LDPR2Q,0)                    PRODUCT - 2                      
         DC   AL1(LDESTQ,0)                    ESTIMATE                         
         DC   AL1(LDMKTQ,0)                    MARKET                           
         DC   AL1(LDFLTQ,0)                    FLIGHT                           
         DC   AL1(LDODTQ,0)                    ORDER DATE                       
         DC   AL1(LDACTQ,0)                    ACTIVITY DATE+TIME               
         DC   AL1(LDFLGQ,0)                    FLAG                             
         DC   AL1(LDCLRQ,0)                    COLOR CODE                       
         DC   AL1(LDCLSQ,0)                    COLOR SEQUENCE                   
         DC   AL1(LDDADQ,0)                    DISK ADDRESS                     
SSEQEX   EQU  *                                                                 
*                                                                               
SSEQF    DS   0X                               SEQUENCE 'F'   SORT=C            
         DC   C'F',AL1(SSEQFX-SSEQF,DSKSTYPQ,SSQFNATV)                          
         DC   AL1((SSEQFX-*)/L'SSQDATA)                                         
         DC   AL1(LDBYRQ,0)                    BUYER                            
         DC   AL1(LDSTAQ,0)                    STATION                          
         DC   AL1(LDORDQ,SSQDIEND)             ORDER                            
*                                                                               
         DC   AL1(LDSTCQ,0)                    STATUS                           
         DC   AL1(LDCLTQ,0)                    CLIENT                           
         DC   AL1(LDPRDQ,0)                    PRODUCT                          
         DC   AL1(LDPR2Q,0)                    PRODUCT - 2                      
         DC   AL1(LDESTQ,0)                    ESTIMATE                         
         DC   AL1(LDMKTQ,0)                    MARKET                           
         DC   AL1(LDFLTQ,0)                    FLIGHT                           
         DC   AL1(LDODTQ,0)                    ORDER DATE                       
         DC   AL1(LDACTQ,0)                    ACTIVITY DATE+TIME               
         DC   AL1(LDFLGQ,0)                    FLAG                             
         DC   AL1(LDCLRQ,0)                    COLOR CODE                       
         DC   AL1(LDCLSQ,0)                    COLOR SEQUENCE                   
         DC   AL1(LDDADQ,0)                    DISK ADDRESS                     
SSEQFX   EQU  *                                                                 
*                                                                               
SSEQG    DS   0X                               SEQUENCE 'G'   INBOX=D           
         DC   C'G',AL1(SSEQGX-SSEQG,DSCKSTYQ,SSQFBUYR)                          
         DC   AL1((SSEQGX-*)/L'SSQDATA)                                         
         DC   AL1(LDCLSQ,0)                    COLOR SEQUENCE                   
         DC   AL1(LDBYRQ,0)                    BUYER                            
         DC   AL1(LDACTDQ,0)                   ACTIVITY DATE                    
         DC   AL1(LDORDQ,SSQDIEND)             ORDER                            
*                                                                               
         DC   AL1(LDACTQ,0)                    ACTIVITY DATE+TIME               
         DC   AL1(LDODTQ,0)                    ORDER DATE                       
         DC   AL1(LDSTCQ,0)                    STATUS                           
         DC   AL1(LDCLTQ,0)                    CLIENT                           
         DC   AL1(LDPRDQ,0)                    PRODUCT                          
         DC   AL1(LDPR2Q,0)                    PRODUCT - 2                      
         DC   AL1(LDESTQ,0)                    ESTIMATE                         
         DC   AL1(LDSTAQ,0)                    STATION                          
         DC   AL1(LDMKTQ,0)                    MARKET                           
         DC   AL1(LDFLTQ,0)                    FLIGHT                           
         DC   AL1(LDFLGQ,0)                    FLAG                             
         DC   AL1(LDCLRQ,0)                    COLOR CODE                       
         DC   AL1(LDDADQ,0)                    DISK ADDRESS                     
SSEQGX   EQU  *                                                                 
*                                                                               
SSEQH    DS   0X                               SEQUENCE 'H'   SORT=D            
         DC   C'H',AL1(SSEQHX-SSEQH,DOKSTYPQ,SSQFNATV)                          
         DC   AL1((SSEQHX-*)/L'SSQDATA)                                         
         DC   AL1(LDORDQ,SSQDIEND)             ORDER                            
*                                                                               
         DC   AL1(LDSTCQ,0)                    STATUS                           
         DC   AL1(LDBYRQ,0)                    BUYER                            
         DC   AL1(LDCLTQ,0)                    CLIENT                           
         DC   AL1(LDPRDQ,0)                    PRODUCT                          
         DC   AL1(LDPR2Q,0)                    PRODUCT - 2                      
         DC   AL1(LDESTQ,0)                    ESTIMATE                         
         DC   AL1(LDSTAQ,0)                    STATION                          
         DC   AL1(LDMKTQ,0)                    MARKET                           
         DC   AL1(LDFLTQ,0)                    FLIGHT                           
         DC   AL1(LDODTQ,0)                    ORDER DATE                       
         DC   AL1(LDACTQ,0)                    ACTIVITY DATE+TIME               
         DC   AL1(LDFLGQ,0)                    FLAG                             
         DC   AL1(LDCLRQ,0)                    COLOR CODE                       
         DC   AL1(LDCLSQ,0)                    COLOR SEQUENCE                   
         DC   AL1(LDDADQ,0)                    DISK ADDRESS                     
SSEQHX   EQU  *                                                                 
         DC   X'FF'                                                             
         EJECT                                                                  
***********************************************************************         
* COLOR TABLE                                                         *         
***********************************************************************         
CLRGQ    EQU   X'01'                      GREEN                                 
CLRRQ    EQU   X'02'                      RED                                   
CLRKQ    EQU   X'04'                      BLACK                                 
*                                                                               
*                                         COLOR/BIT SEQ/CODE                    
CLRTAB   DS    0XL3                                                             
         DC    C'G',AL1(CLRGQ),C'B'       GREEN - BUYER TO DO                   
         DC    C'R',AL1(CLRRQ),C'S'       RED   - SELLER TO DO                  
         DC    C'K',AL1(CLRKQ),C'C'       BLACK - COMPLETE                      
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* LOCAL SAVED STORAGE                                                           
***********************************************************************         
LSSD     DSECT                                                                  
CNTL     DS    XL1                 CONTROL FLAG                                 
CNTLRFSH EQU   X'80'                 REFRESH LIST                               
CNTLMORE EQU   X'40'                 MORE FILE RECORDS TO READ                  
CNTLREAD EQU   X'20'                 READ MORE FILE RECORDS - NOW               
CNTLSAME EQU   X'10'                 DISPLAY SAME TSAR RECORDS                  
CNTLMXIO EQU   X'08'                 REACHED MAX IO'S                           
CNTLMXTS EQU   X'04'                 REACHED TSAR MAX                           
CNTLBYRL EQU   X'02'                 BUYER LIST                                 
CNTLPFKY EQU   X'01'                 USER JUST PFKEY                            
*                                                                               
RSO      DS    XL1                 READ SEQUENCE OPTIONS                        
RSONATV EQU    X'08'                 USING A NATIVE KEY SEQUENCE                
RSO1BYR EQU    X'04'                 READ ONE BUYER AT A TIME                   
RSOCLPR EQU    X'02'                 READ ONE CLI/PRD AT A TIME                 
*                                                                               
RUNSTCH  DS    CL6                 START OF ESTIMATE DATE (EBCDIC)              
RUNENDCH DS    CL6                 END OF ESTIMATE DATE (EBCDIC)                
GETBROAD DS    V                   ADDRESS OF GETBROAD                          
ESTNUM   DS    XL1                 ESTIMATE NUMBER                              
*                                                                               
MGFLAGS  DS    X                   MAKEGOOD COLOR FLAG                          
MGDELNOT EQU   X'08'                GOT DELIVERY NOTICE                         
MGGREEN  EQU   X'04'                GREEN MAKEGOOD EXISTS                       
MGRED    EQU   X'02'                RED MAKEGOOD EXISTS                         
MGBLACK  EQU   X'01'                BLACK MAKEGOOD EXISTS                       
*                                                                               
IO_COUNT DS    H                   I/O COUNTER                                  
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS CNTLS                          
MF1KYCHG EQU   X'80'                 A KEY FIELD HAS BEEN CHANGED               
MF1RECNF EQU   X'40'                 RECORD WAS NOT FOUND                       
MF1CHNGD EQU   X'20'                 RECORD WAS CHANGED                         
MF1NODAR EQU   X'10'                 SEND A NOTDARE ON PQ                       
MF1XDOSP EQU   X'02'                 DOSP EXT. FIELDS NOT PRESENT/SETUP         
*                                                                               
MISCFLG2 DS    XL1                 MISCELLANEOUS CNTLS 2                        
MF2VAROR EQU   X'80'                 WE GOT A VAR ORDER HERE                    
MF2REVOR EQU   X'40'                 WE GOT A REVISED ORDER HERE                
MF2OFFER EQU   X'20'                 CONFIRMED ORDER HAS AN OFFER               
MF2FAXED EQU   X'10'                 PREVIOUSLY FAXED                           
MF2EMAIL EQU   X'08'                 PREVIOUSLY EMAILED                         
MF2AMEND EQU   X'04'                 AMEND STATUS                               
MF2CANCF EQU   X'02'                 CANCELLED CONFIRMED                        
*                                                                               
MISCFLG3 DS    XL1                 MISCELLANEOUS CNTLS 3                        
MF3RADIO EQU   X'80'                 WE ARE DOING RADIO                         
*                                                                               
MISCFLG4 DS    XL1                                                              
MF4CNFCM EQU   X'80'                 WE GOT A CONFIRM WITH COMMENT              
MF4AMEND EQU   X'40'                 AMEND STATUS                               
*                                                                               
RECFLG   DS    XL1                 RECORD CNTLS                                 
RFSTEL   EQU   X'80'                 HAVE STATUS ELEMENT                        
RFRLDLV  EQU   X'40'                 RECALL DELIVERED                           
RFRLNDL  EQU   X'20'                 RECALL NOT DELIVERED                       
RFSTDT   EQU   X'10'                 HAVE A STATUS DATE AND TIME                
RFDLDT   EQU   X'08'                 HAVE A DELIVERED DATE AND TIME             
RFOK     EQU   X'04'                 RECORD PASSED FILTER TEST                  
*                                                                               
FILTRFLG DS    XL1                 VARIOUS FILTER BITS FOR LIST MODE            
FFLGBUYR EQU   X'80'                 FILTER ON THE BUYER                        
FFLGSDAT EQU   X'40'                 FILTER ON THE START DATE                   
FFLGSTA  EQU   X'20'                 FILTER ON THE STATION                      
FFLGCLT  EQU   X'10'                 FILTER ON THE CLIENT                       
FFLGPRD  EQU   X'08'                 FILTER ON THE PRODUCT                      
FFLGEST  EQU   X'04'                 FILTER ON THE ESTIMATE                     
FFLGORD  EQU   X'02'                 FILTER ON THE ORDER NUMBER                 
FFLGSTAT EQU   X'01'                 FILTER ON THE STATUS                       
*                                                                               
FLTRFLG2 DS    XL1                 VARIOUS FILTER BITS FOR LIST MODE            
FF2PORDR EQU   X'80'                 FILTER ON THE POL ORDER NUMBER             
FF2REVSD EQU   X'40'                 FILTER ON REVISED ORDERS                   
FF2PCNFM EQU   X'20'                 FILTER ON PARTIAL CONFIRMS                 
FF2NCNFM EQU   X'10'                 FILTER ON NOT CONFIRMS                     
FF2AOFFR EQU   X'08'                 FILTER ON ACTIVE OFFERS                    
FF2ACNFM EQU   X'04'                 FILTER ON ALL CONFIRM ORDERS               
FF2INBOX EQU   X'02'                 INBOX SELECT                               
FF2MKT   EQU   X'01'                 FILTER ON MARKET                           
*                                                                               
FLTRFLG3 DS    XL1                 VARIOUS FILTER BITS FOR LIST MODE            
FF3REJCT EQU   X'80'                 FILTER ON REJECT AND AMENDED               
FF3AMEND EQU   X'40'                 FILTER ON AMENDED                          
FF3CSWTC EQU   X'20'                 FILTER ON CALL LETTER SWITCH               
*                                                                               
FLTRCLR  DS    XL1                 COLOR FILTER                                 
*                                                                               
EDATEFLG DS    XL1                 TYPE OF ESTIMATE RANGE FILTER                
EDAQUART EQU   X'80'                 FILTER ON QUARTERS (INTERSECT)             
EDAONAFT EQU   X'40'                 FILTER ON ON AND AFTER                     
EDAONBEF EQU   X'20'                 FILTER ON ON AND BEFORE                    
EDAWITHN EQU   X'10'                 FILTER ON ONLY WITHIN THE DATES            
*                                                                               
ENDDATE  DS    XL2                 END DATE                                     
FILTRSDT DS    XL2                 FILTER START DATE IN ORDER FORM              
FLTSTA   DS    XL3                 FILTER FOR STATION (BINARY)                  
FLTCLT   DS    XL2                 FILTER FOR CLIENT  (BINARY)                  
FLTPRD   DS    XL1                 FILTER FOR PRODUCT (BINARY)                  
FLTPR2   DS    XL1                 FILTER FOR PRODUCT 2 (BINARY)                
FLTPRDC  DS    XL3                 PRODUCT CODE                                 
FLTPR2C  DS    XL3                 PRODUCT CODE 2                               
FLTEST   DS    CL3                 FILTER TEXT FOR ESTIMATE (IE: 009)           
FLTORDL  DS    XL1                 LENGTH OF ORDER NUMBER FILTER                
FLTORD   DS    CL8                 FILTER FOR ORDER NUMBER                      
FLTORDBL DS    XL1                 LENGTH OF BINARY ORDER NUMBER FILTER         
FLTORDBN DS    0XL4                FILTER FOR BINARY ORDER NUMBER               
FLTORBDT DS    XL2                   DATE PORTION                               
FLTORBSQ DS    XL2                   SEQUENCE PORTION                           
FLTORDBH DS    XL4                 FILTER FOR HIGH ORDER NUMBER                 
FLTPORDL DS    XL1                 L(POL ORDER NUMBER) FILTER                   
FLTPORD  DS    CL8                 FILTER FOR POL ORDER NUMBER                  
FLTPORBL DS    XL1                 L(BINARY POL ORDER NUMBER) FILTER            
FLTPORBN DS    0XL4                FILTER FOR BINARY POL ORDER NUMBER           
FLTPOBDT DS    XL2                   DATE PORTION                               
FLTPOBSQ DS    XL2                   SEQUENCE PORTION                           
FLTSTAT  DS    CL1                 FILTER TEXT FOR STATUS                       
FLTEQAT  DS    CL1                 FILTER EQUATE FOR STATUS                     
FLTMKT   DS    CL4                 FILTER MARKET                                
*                                                                               
MKTNUM   DS    XL2                                                              
LSDATYMD DS    CL6                 LIST START DATE                              
ESTWFLT  DS    CL7                 ESTIMATE WITH FLIGHT                         
XMITCTB  DS    CL1                 TRANSMIT CASH/TRADE/BOTH                     
*                                                                               
OPTNFLG  DS    XL1                 OPTION FLAG                                  
OPTNDAD  EQU   X'80'                DISPLAY DISK ADDRESS                        
OPTNRCNT EQU   X'40'                DISPLAY RECORD COUNT MESSAGE                
*                                                                               
FKFLG    DS    XL1                 FILTER KEY CNTLS                             
FKFSET   EQU   X'80'                KEY SET IN ROUTINE                          
FKFMKT   EQU   X'40'                MARKET IS IN SORT KEY                       
FKFPRD   EQU   X'20'                PRODUCT IS IN SORT KEY                      
*                                                                               
SSQ      DS    CL1                 SEQUENCE CODE                                
*                                                                               
BUYRFLD  DS    CL4                 BUYER FIELD                                  
MXBYR    EQU   5                   MAX NUMBER OF BUYERS IN LIST                 
NBYR     DS    XL1                 NUMBER OF BUYERS IN LIST                     
XBYR     DS    XL1                 NUMBER OF THIS BUYER IN  LIST                
BUYRC    DS    CL3                 BUYER CODE                                   
BUYRL    DS    XL(MXBYR*L'BUYRC)   LIST OF BUYER CODES                          
*                                                                               
KBUYRCD  DS    CL3                 BUYER CODE FOR KEY COMPARE                   
*                                                                               
KCLIPRD  DS    0XL3                CLIENT/PRODUCT                               
KCLIENT  DS    XL2                 CLIENT CODE FOR KEY COMPARE                  
KPRODUCT DS    XL1                 PRODUCT FOR KEY COMAPRE                      
*                                                                               
NPRDL    DS    XL1                 NUMBER IN PRODUCT LIST                       
XPRDL    DS    XL1                 NUMBER OF CURRENT ITEM IN LIST               
*                                                                               
BINORDER DS    0XL4                BINARY ORDER NUMBER                          
BINORDDT DS    XL2                  DATE PORTION                                
BINORDSQ DS    XL2                  SEQUENCE PORTION                            
*                                                                               
CHRORDER DS    0CL8                CHARACTER ORDER NUMBER                       
CHRORDDT DS    CL4                  DATE PORTION                                
CHRORDSQ DS    CL4                  SEQUENCE PORTION                            
*                                                                               
PROFOM   DS    CL16                OM PROFILE                                   
POMUSEOM EQU   PROFOM               - USES ORDER MANAGER?                       
POMAUCFM EQU   PROFOM+2             - PARITAL CONFIRM WORKFLOW                  
POMMGREP EQU   PROFOM+15            - CREATE MKGD TRANSACTION REPORT?           
*                                                                               
DATEFLTR DS    CL6                                                              
DATEFLT1 DS    XL3                 TODAYS PWOS DATE MINUS 14                    
CFFDATE  DS    XL2                 TODAYS DATE COMPRESSED                       
*                                                                               
CURSOUT  DS    XL2                 DISPLACEMENT TO OUTPUT CURSOR                
SVR2     DS    F                                                                
*                                                                               
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
*                                                                               
LASTKEY  DS    XL(L'DOKEY)         LAST KEY EXAMINED                            
PREVKEY  DS    XL(L'DOKEY)         LAST KEY EXAMINED                            
SAVEKEY  DS    XL(L'EKEY)                                                       
COLOELEM DS    XL(COLELQ)                                                       
*                                                                               
PERVALST DS    XL(L'PVALOUTB)      PERVAL STORAGE AREA                          
*                                                                               
FLDH     DS    CL8                 FIELD HEADER AND FIELD DATA                  
FLD      DS    CL60                                                             
*                                                                               
INBOXSRT DS    CL2                                                              
TODO     DS    CL3                                                              
*                                                                               
BASERC   DS    A                                                                
RELO     DS    A                                                                
VTSAR    DS    A                                                                
VGLOBBER DS    A                                                                
AOPTTAB  DS    A                   A(OPTION TABLE)                              
AORDSTAB DS    A                   A(ORDER TYPE TABLE)                          
ADKYTAB  DS    A                   A(KEY CONTROL TABLE)                         
ADFTAB   DS    A                   A(FILTER DATA TABLE)                         
ADLDTAB  DS    A                   A(LIST DATA DEFINITIONS)                     
ADSEQTAB DS    A                   A(SORT SEQUENCE TABLE)                       
ADCLRTAB DS    A                   A(COLOR TABLE)                               
*                                                                               
ADOXMETL DS    A                   A(DOXMETEL)                                  
ADOSTETL DS    A                   A(DOSTETEL)                                  
AORDNTRY DS    A                   A(IN ORDER TYPE ENTRY)                       
*                                                                               
FFS      DS    XL16            FF'S                                             
NLISTQ   EQU   (ORLPFLNH-ORLSEL1H)/LINLNQ                                       
*                                                                               
BESTKEY  DS    XL1                                                              
AKYDAT   DS    F                   A(KEY DATA ENTRY)                            
ASQDAT   DS    F                   A(SEQUENCE DATA ENTRY)                       
KYLN     DS    XL1                 LENGTH OF MINIMUM KEY                        
KYMSK    DS    XL(L'DOKEY)         FILTER KEY MASK                              
KYFLDLN  DS    XL1                 KEY FIELD LENGTH(IF ORDER <8 )               
AGYMD    DS    XL1                 AGENCY MEDIA                                 
DAD      DS    XL4                 DISK ADDRESS                                 
DISKADDR DS    XL4                                                              
CLRDATE  DS    XL3                 DATE ON COLOR ELEMENT                        
*                                                                               
STATION  DS    CL(L'LINSTA)                                                     
ESTIMATE DS    CL(L'LINEST)                                                     
*                                                                               
ESTTAB   DS    XL256                                                            
APEST    DS    F                     A(NEXT PROD/ESTIMATE) ENTRY                
NUMPEST  DS    XL2                   NUMBER IN PRODUCT/ESTIMATE TABLE           
NXTPEST  DS    XL2                   NUMBER OF NEXT ENTRY TO BE USED            
PRDEST   DS    0XL2                  PRODUCT NUMBER/ESTIMATE NUMBER             
         DS    (MXPEST)XL(L'PRDEST)  TABLE OF PROD/ESTIMATE NUMBERS             
*                                                                               
NSCR     DS    XL2                   NUMBER ON SCREEN                           
NLST     DS    XL1                   NUMBER IN LIST                             
NSELECT  DS    XL2                   INDEX OF SELECTED ORDER FROM LIST          
LSTTAB   DS    (NLISTQ+1)XL(LSTLNQ)  LIST TABLE(SEE LST11)                      
ALSTTAB  DS    A                                                                
*                                                                               
TSRREC   DS    XL100               TSAR RECORD                                  
TSRTOT   DS    XL2                 TOTAL NUMBER OF RECORDS                      
TSRFST   DS    XL2                 NUMBER OF FIRST IN LIST                      
TSRLST   DS    XL2                 NUMBER OF LAST  IN LIST                      
TSRSEL   DS    XL2                 NUMBER OF SELECTED IN LIST                   
*                                                                               
TSRINIQ  EQU   1                          INITIALIZE                            
TSRRESQ  EQU   2                          RESTORE                               
TSRADDQ  EQU   3                          ADD                                   
TSRGETQ  EQU   4                          GET-ENOUGH TO FILL SCREEN             
TSRSAVQ  EQU   5                          SAVE                                  
TSRGT1Q  EQU   6                          GET-ONE RECORD                        
TSRPUTQ  EQU   7                          PUT BY RECORD NUMBER                  
*                                                                               
TSARBLK  DS    XL(TSARDL)          TSAR BLOCK                                   
*                                                                               
         ORG   LSSD+L'SYSSPARE                                                  
*                                                                               
MXPEST   EQU   1000                  MAX IN PRDEST TABLE                        
MXTSAR   EQU   1400                  MAX IN TSAR BUFFER                         
MXIOPCT  EQU   30                    % OF MAX IO ALLOWED                        
                                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER ORDER LIST ENTRY                                     *         
***********************************************************************         
LSTD     DSECT                                                                  
LSTORD   DS    XL(L'DOKORDER)             ORDER                                 
LSTSTC   DS    XL(L'LINSTC)               STATUS CODE                           
LSTBYR   DS    XL(L'DOIDBYR)              BUYER                                 
LSTCLT   DS    XL(L'DOIDCLT)              CLIENT                                
LSTPRD   DS    XL(L'DOIDPRD)              PRODUCT NUMBER                        
LSTPR2   DS    XL(L'DOIDPRD2)             PRODUCT - 2 NUMBER                    
LSTPRDC  DS    CL3                        PRODUCT CODE                          
LSTEST   DS    XL(L'DOIDEST)              ESTIMATE                              
LSTSTA   DS    XL(L'DOISTA)               STATION                               
LSTMKT   DS    XL(L'BMKT)                 MARKET                                
LSTMKN   DS    XL(L'MKTNM)                MARKET NAME                           
LSTFLT   DS    XL(L'DOIDFLTN)             FLIGHT                                
LSTACT   DS    0XL(L'DOSTDATE+L'DOSTTIME)  ACTIVITY DATE + TIME                 
LSTACTD  DS    XL(L'DOSTDATE)              ACTIVITY DATE                        
LSTACTT  DS    XL(L'DOSTTIME)                       TIME                        
*                                                                               
LSTODT   DS    XL(L'DOSTDATE)             ORDER DATE                            
LSTFLG   DS    XL1                        STATUS - CNTLS                        
LSTFSTR  EQU   X'80'                       DISPLAY A '*'                        
LSTFMGO  EQU   X'40'                       MG OFFER - DISPLAY 'X'               
LSTTRDE  EQU   X'20'                       TRADE ORDER DISPLAY 'T'              
LSTCLR   DS    CL1                        COLOR                                 
LSTCLS   DS    XL1                        COLOR COLLATING SEQUENCE              
LSTDAD   DS    XL4                        DISK ADDRESS                          
LSTRECNM DS    XL2                        RECORD NUMBER IN TSAR BUFFER          
*                                                                               
LSTLNQ   EQU   *-LSTD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER SCREEN LINE                                          *         
***********************************************************************         
LINDSECT DSECT                                                                  
LINSELH  DS    CL(L'ORLSEL1H)                                                   
LINSEL   DS    CL(L'ORLSEL1)                                                    
LINSTTH  DS    CL(L'ORLSTT1H)                                                   
LINSTT1  DS    C                                                                
LINSTC   DS    CL(L'ORLSTT1-1)                                                  
LINOFIH  DS    CL(L'ORLOFI1H)                                                   
LINOFI   DS    CL(L'ORLOFI1)                                                    
LINSTAH  DS    CL(L'ORLSTA1H)                                                   
LINSTA   DS    CL(L'ORLSTA1)                                                    
***LINMKTH  DS    CL(L'ORLMKT1H)                                                
***LINMKT   DS    CL(L'ORLMKT1)                                                 
LINMKNH  DS    CL(L'ORLMKN1H)                                                   
LINMKN   DS    CL(L'ORLMKN1)                                                    
LINCLTH  DS    CL(L'ORLCLT1H)                                                   
LINCLT   DS    CL(L'ORLCLT1)                                                    
LINPRDH  DS    CL(L'ORLPRD1H)                                                   
LINPRD   DS    CL(L'ORLPRD1)                                                    
LINESTH  DS    CL(L'ORLEST1H)                                                   
LINEST   DS    CL(L'ORLEST1)                                                    
LINFLTH  DS    CL(L'ORLFLT1H)                                                   
LINFLT   DS    CL(L'ORLFLT1)                                                    
LINODTH  DS    CL(L'ORLODT1H)                                                   
LINODT   DS    CL(L'ORLODT1)                                                    
LINADTH  DS    CL(L'ORLADT1H)                                                   
LINADT   DS    CL(L'ORLADT1)                                                    
LINBYRH  DS    CL(L'ORLBYR1H)                                                   
LINBYR   DS    CL(L'ORLBYR1)                                                    
LINORDH  DS    CL(L'ORLORD1H)                                                   
LINORD   DS    CL(L'ORLORD1)                                                    
LINCLRH  DS    CL(L'ORLCLR1H)                                                   
LINCLR   DS    CL(L'ORLCLR1)                                                    
LINNEXTL DS    0C                                                               
LINLNQ   EQU   *-LINDSECT                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER FILTER TABLE                                         *         
***********************************************************************         
FLTRDSCT DSECT                                                                  
FLTRNLEN DS    XL1                 LENGTH OF FILTER ENTRY                       
FLTRAROU DS    XL2                 DISPLACEMENT TO FILTER ROUTINE               
FLTRLTXT DS    XL1                 LENGTH OF FILTER TEXT                        
FLTRTEXT DS    0C                  FILTER TEXT                                  
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER QUARTERLY DATES                                                
***********************************************************************         
QRTRD    DSECT                                                                  
QRTRNUM  DS    CL2                 QUARTER NUMBER                               
QRTRSTR  DS    CL4                 START MONTH/DAY                              
QRTREND  DS    CL4                 END MONTH/DAY                                
QRTRLNQ  EQU   *-QRTRD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER KEY DATA TABLE                                                 
***********************************************************************         
DKYD     DSECT                                                                  
DKYTYP   DS   XL1                  SUB-TYPE                                     
DKYTLN   DS   XL1                  LENGTH OF TABLE ENTRY                        
DKYNUM   DS   XL1                  NUMBER OF KEY FIELDS                         
DKYDATA  DS   0X                   KEY FIELD DATA                               
DKYLEN   DS   XL1                  LENGTH OF KEY FIELD                          
DKYDSP   DS   XL1                  DISPLACEMENT TO KEY FIELD                    
DKYSRC   DS   XL1                  SOURCE DATA EQUATE                           
DKYLNQ   EQU  *-DKYDATA                                                         
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER FILLTER DATA  TABLE                                            
***********************************************************************         
FDD      DSECT                                                                  
FDFLBIT  DS    AL1                 FLAG BITS TO TEST FOR FILTER DATA            
FDFLFLD  DS    AL2                 DISPLACEMENT TO THE FLAG FIELD               
         DS    XL1                 N/D                                          
FDSRC    DS    AL2                 DISPLACEMENT TO DATA                         
FDKSET   DS    AL2                 DISP. TO SPECIAL FIRST TIME HOOK             
FDKFLT   DS    AL2                 DISP. TO SPECIAL FILTER ROUTINE              
FDLNQ    EQU   *-FDD                                                            
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER PRODUCT/ESTIMATE TABLE                                         
***********************************************************************         
PRDESTD  DSECT                                                                  
PRD#     DS    XL1                 PRODUCT NUMBER                               
EST#     DS    XL1                 ESTIMATE NUMBER                              
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER ORDER STATUS TABLE                                             
***********************************************************************         
QUNSENT  EQU   1                                                                
QSENT    EQU   2                                                                
QFAXSNT  EQU   3                                                                
*                                                                               
ORDD     DSECT                                                                  
ORDSTAT  DS    XL1                 ORDER STATUS                                 
ORDLN    DS    XL1                 LENGTH OF TABLE ENTRY                        
ORDDFLT  DS    CL6                 DEFAULT CODE TO DISPLAY                      
ORDTYP   DS    XL1                 SECONDARY TYPE COMPARE                       
ORDTCMTS EQU   X'80'               WITH COMMENTS                                
ORDTNCMT EQU   X'40'               NO COMMENTS                                  
ORDTAMND EQU   X'20'               AMEND                                        
ORDTRJCT EQU   X'10'               REJECT                                       
ORDIND   DS    XL1                 INDICATORS                                   
ORDIFTST EQU   X'80'               STATUS FILTER TEST                           
ORDISENT EQU   X'40'               SEND '*' TO LINSTT1                          
ORDIRCL  EQU   X'20'               INCLUDE IF RECALL FILTER                     
ORDISRCL EQU   X'10'               INCLUDE IF *RECALL FILTER                    
ORDIRCDA EQU   X'08'               INCLUDE/EXCLUDE RECALL BY DATE               
ORDIXREV EQU   X'04'               EXCLUDE IF 'REVISED' FILTER                  
ORDIXCLD EQU   X'02'               EXCLUDE UNLESS FILTERED BY                   
ORDINSTS EQU   X'01'               NO STATUS TEST                               
ORDIND2  DS    XL1                 SECOND INDICATOR                             
ORD2XREV EQU   X'80'               EXCLUDE FROM REVISED                         
ORD2ACFM EQU   X'40'               INCLUDE ON CONFIRM FILTER                    
ORD2REJ  EQU   X'20'               INCLUDE ON REJECT FILTER                     
ORDCOLR  DS    CL1                 COLOR CODE                                   
         DS    XL2                 N/D                                          
ORDNFLG  DS    XL1                 NUMBER OF CNTLS TO TEST                      
ORDDATA  DS    0XL7                                                             
ORDFLG   DS    XL1                 FLAG                                         
ORDCODE  DS    CL6                 CODE                                         
         EJECT                                                                  
***********************************************************************         
* DSECT TO LIST DATA DEFINITIONS                                      *         
***********************************************************************         
LDD      DSECT                                                                  
LDDLEN   DS    XL1                        LENGTH OF DATA                        
LDDLOC   DS    XL1                        DISPLACEMENT TO DATA                  
LDLNQ    EQU   *-LDDLEN                                                         
                                                                                
                                                                                
***********************************************************************         
* DSECT TO COVER SORT SEQUENCE CONTROL TABLE                          *         
***********************************************************************         
SSQD     DSECT                                                                  
SSQCDE   DS    CL1                 SEQUENCE CODE                                
SSQLN    DS    XL1                 LENGTH OF TABLE                              
SSQBKY   DS    XL1                 KEY TYPE EQUATE                              
SSQFLG   DS    XL1                 FLAG                                         
SSQFNATV EQU   X'80'                NATIVE KEY SEQUENCE                         
SSQFBUYR EQU   X'40'                ONE BUYER AT A TIME                         
SSQFCLPR EQU   X'20'                ONE CLIENT/PRODUCT AT A TIME                
SSQNFLD  DS    XL1                 NUMBER OF FIELDS                             
SSQDATA  DS    0XL2                FIELD DATA                                   
SSQDQ    DS    XL1                 DATA EQUATE                                  
SSQDIND  DS    XL1                 INDICATORS                                   
SSQDIEND EQU   X'80'                END OF KEY                                  
         EJECT                                                                  
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSE9D          (OUR LIST SCREEN)                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSD9D          (OUR DISPLAY SCREEN)                         
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* CTGENEDICT                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
* DDCOMFACSD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* DDGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
DBLOCKD  DSECT                                                                  
* DEDBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
EDICTD   DSECT                                                                  
* EDIDDSHD                                                                      
         PRINT OFF                                                              
       ++INCLUDE EDIDDSHD                                                       
         PRINT ON                                                               
* EDILINKD                                                                      
         PRINT OFF                                                              
       ++INCLUDE EDILINKD                                                       
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* SPADAVCOM                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPADAVCOM                                                      
         PRINT ON                                                               
* SPADBUYER                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPADBUYER                                                      
         PRINT ON                                                               
* SPDARDARED                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPDARDARED                                                     
         PRINT ON                                                               
* SPOMSWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPOMSWORKD                                                     
         PRINT ON                                                               
                                                                                
AGYHDRD  DSECT                                                                  
* SPGENAGY                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
BUYRECD  DSECT                                                                  
* SPGENBUY                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
CLTHDRD  DSECT                                                                  
* SPGENCLT                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
* SPGENDRFLT                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRFLT                                                     
         PRINT ON                                                               
* SPGENDRMKN                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRMKN                                                     
         PRINT ON                                                               
* SPGENDRORD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRORD                                                     
         PRINT ON                                                               
ESTHDRD  DSECT                                                                  
* SPGENEST                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
GOALRECD DSECT                                                                  
* SPGENGOAL                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
MKTHDRD  DSECT                                                                  
* SPGENMKT                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
PRDHDRD  DSECT                                                                  
* SPGENPRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENPRD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039SPOMS08   03/14/16'                                      
         END                                                                    
