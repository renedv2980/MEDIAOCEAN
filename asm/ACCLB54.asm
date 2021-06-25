*          DATA SET ACCLB54    AT LEVEL 225 AS OF 08/16/00                      
*PHASE T62154A                                                                  
*&&      SET   NOP=N                                                            
CLB54    TITLE '- PC COMMS - DATA SOURCE LIST'                                  
CLB54    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB54**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,BORELO                                                        
         L     RC,ALINK                                                         
         USING LINKD,RC                                                         
*                                                                               
         CLI   LINKMODE,ROUTSN                                                  
         BNL   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,LINKMODE                                                      
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETMAP              SET A(MAP TABLE)                             
         B     RCVFST              FIRST FOR RECEIVE                            
         B     RCVHDRF             FIRST FOR MAP HEADER RECEIVE                 
         B     RCVDATA             DATA RECEIVE                                 
         B     RCVHDRL             LAST FOR MAP HEADER RECEIVE                  
         B     RCVLST              LAST FOR RECEIVE                             
         B     SND                 SEND                                         
         B     SNDEL               SEND ELEMENT                                 
         B     SNDFLD              SEND ELEMENT FIELD                           
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET A(MAP TABLE)                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMAP   DS    0H                                                               
         LA    RF,MAPTAB           SET A(MAP TABLE)                             
         ST    RF,AMAPTAB                                                       
         GOTO1 AVALTAB             ?? FOR NOW                                   
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR RECEIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RCVFST   DS    0H                  SWITCH INTO CONTROL SYSTEM                   
         GOTO1 VSWITCH,BOPARM,CON,0                                             
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AFILTAB          SET AFILNTRY FOR AIO                         
RFST02   CLI   0(R1),0             (TABLE IN ACGENINI, NO DSECT)                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   CON,1(R1)                                                        
         BE    RFST04                                                           
         LH    RF,4(R1)                                                         
         LA    R1,5(RF,R1)                                                      
         B     RFST02                                                           
RFST04   LA    R1,6(R1)                                                         
         ST    R1,AFILNTRY                                                      
*                                                                               
         MVI   REQINDS,0                                                        
         MVC   LANG,CULANG         SET LANGUAGE                                 
*&&UK*&& CLI   LANG,LANGEUK                                                     
*&&US*&& CLI   LANG,LANGEUS                                                     
         BNE   *+8                                                              
         MVI   LANG,0                                                           
         LA    RF,CTRYTAB          SET COUNTRY MASK                             
         USING CTRYTABD,RF                                                      
RCST06   CLC   CTRYCODE,CUCTRY                                                  
         BE    RCST08                                                           
         LA    RF,CTRYTABL(RF)                                                  
         CLI   CTRYTAB,EOT                                                      
         BNE   RCST06                                                           
         DC    H'0'                                                             
RCST08   MVC   CTRYMASK,CTRYFLT                                                 
         DROP  RF                                                               
*                                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR MAP HEADER RECEIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRF  DS    0H                                                               
         ICM   RF,15,ORCVHDRF                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ELEMENT DATA RECEIVE                                                *         
***********************************************************************         
         SPACE 1                                                                
RCVDATA  DS    0H                                                               
         ICM   RF,15,ORCVDATA                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
RCVMODE  DS    0H                  IF WRITING RECORD - TEST CAN DO              
         CLI   RECMODE,RECMWRTQ                                                 
         BNE   EXITY                                                            
         B     EXITY                                                            
*        L     R2,AIO3             DOES NOT WORK                                
*        USING DTFPHD,R2                                                        
*        GOTO1 VDMGR,BODMCB,DMKEY,GENFIL,(1,DTFPHD)                             
*        TM    DTFOPEN,X'80'                                                    
*        BZ    EXITY                                                            
*        MVC   FVMSGNO,=AL2(AE$CHANA)  CHANGE NOT ALLOWED                       
*        B     EXITN                                                            
*        DROP  R2                                                               
         SPACE 1                                                                
RCVDIE   LA    RF,DATA                                                          
         DC    H'0'                                                             
         SPACE 1                                                                
*                                                                               
RCVAGY   CLI   DATA,C'Y'           TEST AGENCY=Y                                
         BNE   *+8                                                              
         OI    REQINDS,REQIAGY                                                  
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR MAP HEADER RECEIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRL  DS    0H                                                               
         L     RF,AMHEL                                                         
         TM    MHINDS-MHELD(RF),MHIRECEL                                        
         BZ    RHDRL02                                                          
         CLI   RECMODE,RECMWRTQ                                                 
         BNE   RHDRL02                                                          
         TM    REQINDS,REQIRDS                                                  
         BZ    *+8                                                              
         BAS   RE,ADDELDS                                                       
         TM    REQINDS,REQIRKW                                                  
         BZ    *+8                                                              
         BAS   RE,ADDELKW                                                       
*                                                                               
RHDRL02  DS    0H                                                               
         ICM   RF,15,ORCVHDRL                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RECEIVE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVLST   DS    0H                                                               
         CLI   RECMODE,RECMWRTQ                                                 
         BNE   EXITY                                                            
         BAS   RE,WRTDS                                                         
         BAS   RE,WRTKW                                                         
         BAS   RE,DELKW                                                         
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RECEIVED DATA SOURCE RECORD ID                                      *         
*                                                                     *         
* PUT LAST RECORD TO FILE (IF THERE WAS ONE)                          *         
* INITIALIZE IO1 FOR NEW RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
RCVDS    DS    0H                  DATA SOURCE RECORD ID                        
         CLI   RECMODE,RECMWRTQ                                                 
         BNE   EXITY                                                            
*                                                                               
         BAS   RE,WRTDS            WRITE PREVIOUS RECORD                        
*                                                                               
         L     R2,AIO1                                                          
         USING DSRECD,R2                                                        
         XC    DSRECD(DSRFST+1-DSRECD),DSRECD                                   
         MVI   DSKMIN,DSKMINQ                                                   
         MVI   DSKREC,DSKRECQ                                                   
         MVC   DSKID,DATA                                                       
         TM    REQINDS,REQIAGY                                                  
         BZ    *+10                                                             
         MVC   DSKAGY,CUAALF                                                    
         MVC   DSRLEN,=AL2(DSRFST+1-DSRECD)                                     
         DROP  R2                                                               
*                                                                               
         OI    REQINDS,REQIRDS                                                  
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* RECEIVE RECORD NOT REQUIRED                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVNREQD DS    0H                                                               
         CLI   DATA,C'Y'                                                        
         BNE   EXITY                                                            
         L     R2,AIO1                                                          
         USING DSRECD,R2                                                        
         LA    RE,DSKSDEL                                                       
         TM    REQINDS,REQIAGY     TEST AGENCY LEVEL                            
         BZ    *+12                                                             
         OI    DSRSTAT,DSKSNREQ    YES - NOT REQUIRED=YES                       
         B     EXITY                                                            
         OI    DSRSTAT,DSKSDEL     NO - DELETE=YES                              
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RECEIVED KEYWORD LIST KEY                                           *         
*                                                                     *         
* PUT LAST RECORD TO FILE (IF THERE WAS ONE)                          *         
* INITIALIZE IO1 FOR NEW RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
RCVKW    DS    0H                                                               
         CLI   RECMODE,RECMWRTQ                                                 
         BNE   EXITY                                                            
*                                                                               
         BAS   RE,WRTDS            WRITE OUTSTANDING DATA SOURCE REC            
*                                                                               
         L     R2,AIO1             INITIALIZE KEYWORD RECORD                    
         USING KWRECD,R2                                                        
         XC    KWRECD(KWRFST+1-KWRECD),KWRECD                                   
         MVI   KWKMIN,KWKMINQ                                                   
         MVI   KWKREC,KWKRECQ                                                   
         TM    REQINDS,REQIAGY                                                  
         BZ    *+10                                                             
         MVC   KWKAGY,CUAALF                                                    
         MVC   KWRLEN,=AL2(KWRFST+1-KWRECD)                                     
         DROP  R2                                                               
*                                                                               
         OI    REQINDS,REQIRKW                                                  
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RECIEVE DSRCELD TRAILING SPACES                                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING DSRCELD,ELEM                                                     
RCVSPACE DS    0H                                                               
         XR    RF,RF                                                            
         IC    RF,DSRCLN           RF = L(ELEMENT)                              
         LA    R1,DSRCELD(RF)      R1 = A(END OF ELEMENT)                       
         XR    RE,RE                                                            
         ICM   RE,1,DATA                                                        
         BZ    EXITY                                                            
         AR    RF,RE                                                            
         STC   RF,DSRCLN           SET NEW L(ELEMENT)                           
*                                                                               
         MVI   0(R1),C' '          PUT TRAILING SPACES IN                       
         LA    R1,1(R1)                                                         
         BCT   RE,*-8                                                           
*                                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD ELEMENT TO DATA SOURE RECORD                         *         
*                                                                     *         
* NTRY: ELEMENT IN ELEM                                               *         
*       RECORD IN IO1                                                 *         
***********************************************************************         
         SPACE 1                                                                
ADDELDS  NTR1  ,                                                                
         GOTO1 VHELLO,BOPARM,(C'P',GENFIL),AIO1,ELEM,ADDCODE                    
         CLI   12(R1),0                                                         
         BE    EXITY                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO ADD ELEMENT TO KEYWORD LIST RECORD                       *         
*                                                                     *         
* NTRY: ELEMENT IN ELEM                                               *         
*       RECORD IN IO1                                                 *         
***********************************************************************         
         SPACE 1                                                                
ADDELKW  NTR1  ,                                                                
         L     R2,AIO1                                                          
         USING KWRECD,R2                                                        
*                                                                               
         XR    RF,RF               TEST RECORD WILL BE TOO BIG                  
         ICM   RF,3,KWRLEN                                                      
         XR    RE,RE                                                            
         IC    RE,ELEM+(DSRCLN-DSRCELD)                                         
         AR    RF,RE                                                            
         CH    RF,=Y(2000)                                                      
         BNL   AKW04                                                            
*                                                                               
AKW02    DS    0H                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GENFIL),AIO1,ELEM,ADDEND                     
         CLI   12(R1),0                                                         
         BE    EXITY                                                            
         DC    H'0'                                                             
*                                                                               
AKW04    DS    0H                                                               
         BAS   RE,WRTKW            WRITE THIS RECORD TO FILE                    
*                                                                               
         IC    RE,KWKSEQ           INCREMENT SEQUENCE NUMBER                    
         LA    RE,1(RE)            AND CREATE NEW EMPTY RECORD                  
         STC   RE,KWKSEQ                                                        
         CLI   KWKSEQ,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   KWRLEN,=AL2(KWRFST+1-KWRECD)                                     
         MVI   KWRFST,0                                                         
*                                                                               
         B     AKW02               ADD ELEMENT TO NEW RECORD                    
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE DATA SOURCE RECORD TO FILE                         *         
*                                                                     *         
* NTRY: RECORD IN AIO1                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
WRTDS    NTR1  ,                                                                
         TM    REQINDS,REQIRDS     TEST RECEIVED DATA SOURCE KEY                
         BZ    EXITY                                                            
         NI    REQINDS,FF-REQIRDS                                               
*                                                                               
         TM    REQINDS,REQIAGY     TEST AGENCY LEVEL                            
         BZ    WDS10                                                            
         L     R2,AIO1                                                          
         USING DSRECD,R2                                                        
         TM    DSRSTA,DSKSNREQ     TEST NOT REQUIRED                            
         BZ    WDS02                                                            
         MVI   DSRFST,0            ISN'T - DON'T NEED ANY ELS                   
         MVC   DSRLEN,=AL2(DSRFST+1-DSRECD)                                     
         B     WDS10                                                            
*                                                                               
WDS02    DS    0H                                                               
KEY      USING DSRECD,IOKEY                                                     
         MVC   KEY.DSKEY,DSKEY                                                  
         XC    KEY.DSKAGY,KEY.DSKAGY                                            
         GOTO1 AIO,IOREAD+IOGENDIR+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOGENFIL+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 FLTDS,BOPARM,AIO2                                                
         GOTO1 UNMDS,BOPARM,AIO1,AIO2                                           
*                                                                               
WDS10    DS    0H                                                               
         GOTO1 WRTREC                                                           
*                                                                               
WRTDSX   B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE KEYWORD RECORD TO FILE                             *         
*                                                                     *         
* NTRY: RECORD IN AIO1                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
WRTKW    NTR1  ,                                                                
         TM    REQINDS,REQIRKW     TEST RECEIVED KEYWORD LIST KEY               
         BZ    EXITY                                                            
*                                                                               
         L     R2,AIO1                                                          
         USING KWRECD,R2                                                        
*                                                                               
         CLC   KWRLEN,=AL2(KWRFST+1-KWRECD)                                     
         BE    WRTKWX                                                           
         GOTO1 WRTREC                                                           
*                                                                               
WRTKWX   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE UNWANTED KEYWORD RECORDS                          *         
*                                                                     *         
* NTRY: RECORD KEY IN AIO1                                            *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
DELKW    NTR1  ,                                                                
         L     R2,AIO1                                                          
REC      USING KWRECD,R2                                                        
KEY      USING KWRECD,IOKEY                                                     
         MVC   KEY.KWKEY,REC.KWKEY                                              
*                                                                               
         CLC   REC.KWRLEN,=AL2(KWRFST+1-KWRECD)                                 
         BE    DELKW02                                                          
         IC    RE,KEY.KWKSEQ                                                    
         LA    RE,1(RE)                                                         
         STC   RE,KEY.KWKSEQ                                                    
*                                                                               
DELKW02  DS    0H                                                               
         GOTO1 AIO,IOREAD+IOLOCK+IOGENDIR+IO1                                   
         BNE   DELKWX                                                           
         OI    KEY.KWKSTA,KWKSDEL                                               
         GOTO1 AIO,IOWRITE+IOGENDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGENFIL+IOGETRUP+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    REC.KWRSTA,KWKSDEL                                               
         GOTO1 AIO,IOGENFIL+IOPUT+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         IC    RE,KEY.KWKSEQ       BUMP SEQUENCE NUMBER                         
         LA    RE,1(RE)                                                         
         STC   RE,KEY.KWKSEQ                                                    
         CLI   KEY.KWKSEQ,0                                                     
         BNE   DELKW02                                                          
         DC    H'0'                UNLIKELY                                     
*                                                                               
DELKWX   DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE RECORD                                             *         
*                                                                     *         
* NTRY: IO1 = RECORD TO BE WRITTEN                                    *         
* EXIT: RECORD WRITTEN/ADDED TO FILE                                  *         
***********************************************************************         
         SPACE 1                                                                
WRTREC   NTR1  ,                                                                
         PUSH USING                                                             
DIR      USING DSRECD,IOKEY                                                     
         L     R2,AIO1                                                          
NEW      USING DSRECD,R2                                                        
         L     R3,AIO2                                                          
OLD      USING DSRECD,R3                                                        
*                                                                               
         MVC   DIR.DSKEY,NEW.DSKEY                                              
         GOTO1 AIO,IORDD+IOGENDIR+IO2                                           
         BE    WREC02                                                           
         TM    IOERR,IOEDEL                                                     
         BO    WREC02                                                           
         TM    IOERR,IOERNF        TEST RECORD NOT ON FILE                      
         BO    *+6                                                              
         DC    H'0'                                                             
         TM    NEW.DSRSTA,DSKSDEL  TEST NEW RECORD DELETED                      
         BO    WRTRECX             YES - DON'T ADD IT THEN                      
         GOTO1 AIO,IOADD+IOGENFIL+IO1                                           
         BE    WRTRECX                                                          
         DC    H'0'                                                             
*                                                                               
WREC02   DS    0H                  TEST CHANGE IN STATUS AREA                   
         CLC   DIR.DSKSTA,NEW.DSRSTA                                            
         BE    WREC04                                                           
         GOTO1 AIO,IORDUPD+IOGENDIR+IO2   YES - UPDATE DIRECTORY REC            
         MVC   DIR.DSKSTA,NEW.DSRSTA                                            
         GOTO1 AIO,IOWRITE+IOGENDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WREC04   DS    0H                                                               
         GOTO1 AIO,IOGETRUP+IOGENFIL+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,NEW.DSRECD       COPY RECORD TO IO2                           
         XR    RF,RF                                                            
         ICM   RF,3,NEW.DSRLEN                                                  
         LA    R0,OLD.DSRECD                                                    
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 AIO,IOPUT+IOGENFIL+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WRTRECX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
         CLI   RECMODE,RECMRDQ     TEST READING RECORD                          
         BNE   EXITY                                                            
*                                                                               
         BAS   RE,SNDDS                                                         
         BAS   RE,SNDKW                                                         
*                                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SEND ELEMENT                                                        *         
***********************************************************************         
         SPACE 1                                                                
SNDEL    DS    0H                                                               
         ICM   RF,15,OSNDEL                                                     
         BNZR  RF                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* SEND ELEMENT FIELD                                                  *         
***********************************************************************         
         SPACE 1                                                                
SNDFLD   DS    0H                                                               
         ICM   RF,15,OSNDFLD                                                    
         BNZR  RF                                                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SEND DATA SOURCE RECORDS                                 *         
***********************************************************************         
         SPACE 1                                                                
SNDDS    NTR1  ,                                                                
         LA    R2,IOKEY                                                         
         USING DSRECD,R2                                                        
         XR    R4,R4               R4 = RECORD ID NUMBER                        
*                                                                               
SDS02    DS    0H                  READ DDS LEVEL RECORD                        
         XC    DSKEY,DSKEY                                                      
         MVI   DSKMIN,DSKMINQ                                                   
         MVI   DSKREC,DSKRECQ                                                   
         STCM  R4,3,DSKID                                                       
         LA    R1,IOHIGH+IOGENDIR+IO1                                           
         TM    REQINDS,REQIAGY                                                  
         BO    *+8                                                              
         LA    R1,IORDEL(R1)       READ FOR DELETED IF NOT AGENCY               
         GOTO1 AIO                                                              
         BE    *+12                                                             
         TM    IOERR,IOEDEL                                                     
         BZ    SNDDSX                                                           
         CLC   DSKEY(DSKID-DSKEY),IOKEYSAV                                      
         BNE   SNDDSX                                                           
         ICM   R4,3,DSKID                                                       
         OC    DSKAGY,DSKAGY                                                    
         BNZ   SDS08                                                            
         GOTO1 AIO,IOGET+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 FLTDS,BOPARM,AIO1   FILTER RECORD                                
         BNE   SDS08                                                            
*                                                                               
         TM    REQINDS,REQIAGY                                                  
         BZ    SDS04                                                            
         MVC   DSKAGY,CUAALF       READ FOR AGENCY LEVEL RECORD                 
         GOTO1 AIO,IOREAD+IOGENDIR+IO2                                          
         BNE   SDS04                                                            
         GOTO1 AIO,IOGET+IOGENFIL+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MRGDS,BOPARM,AIO1,AIO2 MERGE THE RECORDS                         
*                                                                               
SDS04    DS    0H                                                               
         GOTO1 SNDDSREC,BOPARM,AIO1                                             
*                                                                               
SDS08    DS    0H                                                               
         CLM   R4,3,EFFS                                                        
         BE    SNDDSX                                                           
         LA    R4,1(R4)            BUMP TO NEXT ID NUMBER                       
         B     SDS02                                                            
*                                                                               
SNDDSX   DS    0H                                                               
         B     EXITY                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO SEND DATA SOURCE RECORD                                  *         
*                                                                     *         
* NTRY: P1 = A(RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
SNDDSREC NTR1  ,                                                                
*                                  SEND KEY DETAILS                             
         L     R2,0(R1)                                                         
         USING DSRECD,R2                                                        
         GOTO1 ASNDHDR,BOPARM,3                                                 
         GOTO1 ASNDDATA,BOPARM,1,DSKID                                          
         TM    DSRSTAT,DSKSDEL+DSKSNREQ                                         
         BZ    SDSREC02                                                         
         GOTO1 ASNDDATA,BOPARM,2,=C'Y'   NOTREQUIRED=YES                        
SDSREC02 DS    0H                                                               
*                                  SEND ELEMENT DETAILS                         
         GOTO1 ASNDREC,BOPARM,('DSRFST-DSRECD',DSRECD)                          
*                                                                               
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER DSRECD                                            *         
*  - REMOVE NSRCELDS AND DSRCELDS FOR DIFFERENT LANGUAGES             *         
*  - FILTER RECORD ON COUNTRY (IF AGENCY LEVEL)                       *         
***********************************************************************         
         SPACE 1                                                                
FLTDS    NTR1  ,                                                                
         L     R2,0(R1)                                                         
         USING DSRECD,R2                                                        
         LA    R3,DSRFST                                                        
*                                                                               
         MVI   BOBYTE1,0           SET TO USE DEFAULT LANG                      
         XR    RF,RF                                                            
         USING NSRCELD,R3                                                       
FDS02    CLI   NSRCEL,0                                                         
         BE    FDS10                                                            
         CLI   NSRCEL,NSRCELQ                                                   
         BNE   FDS08                                                            
         CLC   NSRCLANG,LANG       TEST FOR CONNECTED LANGUAAGE                 
         BNE   FDS08                                                            
         MVC   BOBYTE1,NSRCLANG                                                 
         B     FDS10                                                            
FDS08    IC    RF,NSRCLN                                                        
         BXH   R3,RF,FDS02                                                      
         DROP  R3                                                               
*                                                                               
FDS10    LA    R3,DSRFST           REMOVE TXTELS FOR DIFFERENT LANGS            
FDS12    CLI   0(R3),0                                                          
         BE    FDS20                                                            
*                                                                               
         USING NSRCELD,R3          CHECK NAME ELEMENT                           
         CLI   NSRCEL,NSRCELQ                                                   
         BNE   FDS14                                                            
         CLC   NSRCLANG,BOBYTE1                                                 
         BE    FDS18                                                            
         MVI   NSRCEL,FF                                                        
         B     FDS18                                                            
         DROP  R3                                                               
*                                                                               
         USING DSRCELD,R3          CHECK DATA SOURCE ELEMENT                    
FDS14    CLI   DSRCEL,DSRCELQ                                                   
         BNE   FDS18                                                            
         CLC   DSRCLANG,BOBYTE1    TEST FOR REQUIRED LANGUAGE                   
         BE    *+12                                                             
         MVI   DSRCEL,FF           NO - DELETE ELEMENT                          
         B     FDS18                                                            
         MVI   DSRCLANG,0          YES - RESET LANGUAGE TO 0                    
         DROP  R3                                                               
*                                                                               
FDS18    IC    RF,1(R3)                                                         
         BXH   R3,RF,FDS12                                                      
*                                                                               
FDS20    DS    0H                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GENFIL),('FF',DSRECD),0                      
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    REQINDS,REQIAGY     TEST AGENCY LEVEL                            
         BZ    FLTDSY                                                           
         LA    R3,DSRFST           YES - APPLY COUNTRY FILTER                   
         USING CFLTELD,R3                                                       
         XR    RF,RF                                                            
FDS22    CLI   CFLTEL,0                                                         
         BE    FLTDSY                                                           
         CLI   CFLTEL,CFLTELQ                                                   
         BNE   FDS28                                                            
         MVC   BOFULL1,CFLTMASK                                                 
         OC    BOFULL1,CTRYMASK                                                 
         BNZ   FLTDSY                                                           
         B     FLTDSN                                                           
FDS28    IC    RF,CFLTLN                                                        
         BXH   R3,RF,FDS22                                                      
         DROP  R3                                                               
*                                                                               
FLTDSY   B     EXITY                                                            
*                                                                               
FLTDSN   B     EXITN                                                            
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MERGE DSRECDS                                            *         
*                                                                     *         
* NTRY: P1 = A(DDS LEVEL RECORD)                                      *         
*       P2 = A(AGENCY LEVEL RECORD)                                   *         
* EXIT: DDS LEVEL RECORD = MERGED RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
MRGDS    NTR1  ,                                                                
         LM    R4,R5,0(R1)                                                      
DDS      USING DSRECD,R4                                                        
AGY      USING DSRECD,R5                                                        
         OC    DDS.DSRSTA,AGY.DSRSTA                                            
         TM    DDS.DSRSTA,DSKSNREQ  TEST NOT REQUIRED BY AGENCY                 
         BO    MRGDSX                                                           
*                                                                               
         LA    R3,AGY.DSRFST                                                    
MDS02    CLI   0(R3),0                                                          
         BE    MRGDSX                                                           
*                                                                               
         CLI   0(R3),DEFELQ                                                     
         BNE   MDS04                                                            
         GOTO1 VHELLO,BOPARM,(C'D',GENFIL),('DEFELQ',DDS.DSRECD),0              
         GOTO1 (RF),(R1),(C'P',GENFIL),DDS.DSRECD,(R3),ADDCODE                  
         CLI   12(R1),0                                                         
         BE    MDS08                                                            
         DC    H'0'                                                             
*                                                                               
MDS04    CLI   0(R3),DSRCELQ                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'D',GENFIL),('DSRCELQ',DDS.DSRECD),     *        
               (L'DSRCTYP,DSRCTYP-DSRCELD(R3))                                  
         GOTO1 (RF),(R1),(C'P',GENFIL),DDS.DSRECD,(R3),ADDCODE                  
         CLI   12(R1),0                                                         
         BE    MDS08                                                            
         DC    H'0'                                                             
*                                                                               
MDS08    XR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         BXH   R3,RF,MDS02                                                      
*                                                                               
MRGDSX   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UNMERGE DSRECDS                                          *         
*                                                                     *         
* NTRY: P1 = A(AGENCY LEVLEL RECORD)                                  *         
*       P2 = A(DDS LEVEL RECORD)                                      *         
* EXIT: AGENCY LEVEL RECORD IS SHORTEND                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
UNMDS    NTR1  ,                                                                
         LM    R4,R5,0(R1)                                                      
AGY      USING DSRECD,R4                                                        
DDS      USING DSRECD,R5                                                        
*                                                                               
         LA    R3,AGY.DSRFST                                                    
UDS02    CLI   0(R3),0                                                          
         BE    UDS10                                                            
*                                                                               
         CLI   0(R3),DEFELQ                                                     
         BNE   UDS04                                                            
         GOTO1 VHELLO,BOPARM,(C'G',GENFIL),('DEFELQ',DDS.DSRECD),0              
         B     UDS06                                                            
*                                                                               
UDS04    CLI   0(R3),DSRCELQ                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'G',GENFIL),('DSRCELQ',DDS.DSRECD),     *        
               (L'DSRCTYP,DSRCTYP-DSRCELD(R3))                                  
*                                                                               
UDS06    L     RF,12(R1)           COMPARE ELEMENTS IDENTICAL                   
         IC    RE,1(R3)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BNE   UDS08                                                            
         CLC   0(0,R3),0(RF)                                                    
         MVI   0(R3),FF                                                         
*                                                                               
UDS08    XR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         BXH   R3,RF,UDS02                                                      
*                                                                               
UDS10    DS    0H                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GENFIL),('FF',AGY.DSRECD),0                  
         CLI   AGY.DSRFST,0                                                     
         BNE   UNMDSX                                                           
         OI    AGY.DSRSTA,DSKSDEL  NO ELEMENTS - DON'T NEED RECORD              
*                                                                               
UNMDSX   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SEND KEY WORD LIST                                       *         
***********************************************************************         
         SPACE 1                                                                
SNDKW    NTR1  ,                                                                
         GOTO1 ASNDHDR,BOPARM,4    KEY WORD LIST CODE                           
*                                                                               
         TM    REQINDS,REQIAGY                                                  
         BZ    SNDKW02                                                          
         GOTO1 SNDKWREC,BOPARM,1   TRY AGENCY LEVEL RECORD                      
         BE    SNDKWX                                                           
*                                                                               
SNDKW02  DS    0H                                                               
         XR    RF,RF                                                            
         IC    RF,LANG             SEND GLOBAL LEVEL RECORDS                    
         GOTO1 SNDKWREC,BOPARM,0,(RF)                                           
         BE    SNDKWX                                                           
         GOTO1 SNDKWREC,BOPARM,0,0                                              
*                                                                               
SNDKWX   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO SEND KEY WORD RECORDS                                    *         
*                                                                     *         
* NTRY: P1 = 0 TO SEND GLOBAL LEVEL LIST                              *         
*        NON 0 TO SEND AGENCY LEVEL LIST                              *         
*       P2 = LANGUAGE CODE                                            *         
* EXIT:  CC 0 EQUAL IF ANYTHING SENT                                  *         
***********************************************************************         
         SPACE 1                                                                
SNDKWREC NTR1  ,                                                                
         LM    R3,R4,0(R1)                                                      
*                                                                               
         LA    R2,IOKEY                                                         
         USING KWRECD,R2                                                        
         XC    KWKEY,KWKEY                                                      
         MVI   KWKMIN,KWKMINQ                                                   
         MVI   KWKREC,KWKRECQ                                                   
         LTR   R3,R3               TEST AGENCY LEVEL                            
         BZ    *+14                                                             
         MVC   KWKAGY,CUAALF       YES - SET AGENCY ALPHA CODE                  
         B     *+8                                                              
         STC   R4,KWKLANG          NO - SET LANGUAGE                            
         MVI   BOBYTE1,0                                                        
*                                                                               
         LA    R1,IOHIGH+IOGENDIR+IO1                                           
         B     *+8                                                              
SKWREC02 LA    R1,IOSEQ+IOGENDIR+IO1                                            
         GOTO1 AIO                                                              
         BNE   SKWRECX                                                          
         CLC   KWKEY(KWKSEQ-KWKEY),IOKEYSAV                                     
         BNE   SKWRECX                                                          
         GOTO1 AIO,IOGET+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   BOBYTE1,1                                                        
         GOTO1 ASNDREC,BOPARM,('KWRFST-KWRECD',AIO1)                            
         B     SKWREC02                                                         
*                                                                               
SKWRECX  CLI   BOBYTE1,1           SET CC=EQUAL IF ANYTHING SENT                
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SEND TEXT PLUS NUMBER OF TRAILING SPACES                            *         
***********************************************************************         
         SPACE 1                                                                
SNDTEXT  DS    0H                                                               
         LH    R0,DATALEN                                                       
         LTR   R0,R0               DON'T SEND IF ZERO LENGTH                    
         BZ    EXITN                                                            
*                                                                               
         LA    RF,DATA             FIND NUMBER OF TRAILING SPACES               
         AH    RF,DATALENX                                                      
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         LTR   R0,R0                                                            
         BZ    STEXT02                                                          
         GOTO1 ASNDDATA,BOPARM,AMDEL,((R0),DATA)                                
*                                                                               
STEXT02  DS    0H                                                               
         LH    RE,DATALEN                                                       
         SR    RE,R0                                                            
         BZ    EXITN                                                            
         STC   RE,BOWORK1                                                       
         GOTO1 ASNDDATA,BOPARM,22,BOWORK1                                       
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
CON      DC    C'CON'                                                           
DMKEY    DC    C'DMKEY  '                                                       
GENFIL   DC    C'GENFIL '                                                       
ADDEND   DC    C'ADD=END'                                                       
ADDCODE  DC    C'ADD=CODE'                                                      
EFFS     DC    X'FFFFFFFF'                                                      
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
***********************************************************************         
* COUNTRY MASK TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
CTRYTABD DSECT                                                                  
CTRYCODE DS    XL1                 COUNTRY CODE                                 
CTRYFLT  DS    XL4                 COUNTRY FILTER MASK VALUE                    
CTRYTABL EQU    *-CTRYTABD                                                      
         SPACE 1                                                                
CLB54    CSECT                                                                  
CTRYTAB  DS    0X                                                               
         DC    AL1(CTRYGBR),AL4(CFLTUK)                                         
         DC    AL1(CTRYUSA),AL4(CFLTUSA)                                        
         DC    AL1(CTRYGER),AL4(CFLTGER)                                        
         DC    AL1(CTRYFRA),AL4(CFLTFRA)                                        
         DC    AL1(CTRYSPA),AL4(CFLTSPA)                                        
         DC    AL1(CTRYITA),AL4(CFLTITA)                                        
         DC    AL1(CTRYHOL),AL4(CFLTHOL)                                        
         DC    AL1(CTRYCAN),AL4(CFLTCAN)                                        
         DC    AL1(CTRYIRE),AL4(CFLTIRE)                                        
         DC    AL1(CTRYSCA),AL4(CFLTSCA)                                        
CTRYTABX DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* MAP TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
*                                                                               
M#DSREQ  DS    0X                  ** DATA SOURCE REQUEST **                    
         DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#DSR)         ELEMENT CODE                                 
         DC    AL2(M#DSREQX+1-M#DSREQ) DISP TO NEXT ELEMENT HEADER              
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(26)             MAPPING CODE                                 
         DC    CL5'RMODE'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDIMODE)        INDICATORS                                   
         DC    AL2(RCVMODE-CLB54)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'AGY'            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVAGY-CLB54)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'DIE'            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(6)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVDIE-CLB54)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'DIE'            TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(6)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVDIE-CLB54)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#DSREQX DC    AL1(0)              END-OF-ELEMENT FIELDS                        
*                                                                               
M#DSC    DS    0X                  ** DATA SOURCE KEY **                        
         DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(03)             ELEMENT CODE                                 
         DC    AL2(M#DSCX+1-M#DSC) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'DSKID'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(L'DSKID)        DATA LENGTH                                  
         DC    AL1(DSKID-DSKEY)    DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(RCVDS-CLB54)    RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'NREQD'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(DSKSTA-DSKEY)   DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVNREQD-CLB54) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#DSCX   DC    AL1(0)              END-OF-ELEMENT FIELDS                        
*                                                                               
M#KWL    DS    0X                  ** KEYWORD LIST **                           
         DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(04)             ELEMENT CODE                                 
         DC    AL2(M#KWLX+1-M#KWL) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVKW-CLB54)    FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
M#KWLX   DC    AL1(0)              END-OF-ELEMENT FIELDS                        
*                                                                               
M#CFLT   DS    0X                  ** COUNTRY FILTER ELEMENT **                 
         DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(CFLTELQ)        ELEMENT CODE                                 
         DC    AL2(M#CFLTX+1-M#CFLT) DISP TO NEXT ELEMENT HEADER                
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(CFLTELQ,CFLTLNQ) ELEMENT CODE/LENGTH                         
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'MASK'           TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(L'CFLTMASK)     DATA LENGTH                                  
         DC    AL1(CFLTMASK-CFLTELD) DATA DISPLACEMENT                          
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#CFLTX  DC    AL1(0)              END-OF-ELEMENT FIELDS                        
*                                                                               
M#KEY    DS    0X                  ** KEY DATA ELEMENT **                       
         DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(KEYELQ)         ELEMENT CODE                                 
         DC    AL2(M#KEYX+1-M#KEY) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(KEYELQ,KEYLNQ)  ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'SECT'           TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE                                    
         DC    AL1(L'KEYSECT)      DATA LENGTH                                  
         DC    AL1(KEYSECT-KEYELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'BLF'            TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'KEYBODYF)     DATA LENGTH                                  
         DC    AL1(KEYBODYF-KEYELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD+MDINNULL) INDICATORS                                
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'FLD'            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'KEYPANEL)     DATA LENGTH                                  
         DC    AL1(KEYPANEL-KEYELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD+MDINNULL) INDICATORS                                
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'INDS1'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'KEYINDS1)     DATA LENGTH                                  
         DC    AL1(KEYINDS1-KEYELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#KEYX   DC    AL1(0)              END-OF-ELEMENT FIELDS                        
*                                                                               
M#NSRC   DS    0X                  ** SOURCE NAME ELEMENT **                    
         DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(NSRCELQ)        ELEMENT CODE                                 
         DC    AL2(M#NSRCX+1-M#NSRC) DISP TO NEXT ELEMENT HEADER                
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(NSRCELQ,NSRCLNQ) ELEMENT CODE/LENGTH                         
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'NAME'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(NSRCTEXT-NSRCELD) DATA DISPLACEMENT                          
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#NSRCX  DC    AL1(0)              END-OF-ELEMENT FIELDS                        
*                                                                               
M#DEF    DS    0X                  ** GENERAL DEFAULT VALUES **                 
         DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(DEFELQ)         ELEMENT CODE                                 
         DC    AL2(M#DEFX+1-M#DEF) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(DEFELQ,DEFLNQ)  ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'INDS1'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'DEFINDS1)     DATA LENGTH                                  
         DC    AL1(DEFINDS1-DEFELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#DEFX   DC    AL1(0)              END-OF-ELEMENT FIELDS                        
*                                                                               
M#KSRC   DS    0X                  ** KEY SOURCE DATA **                        
         DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(KSRCELQ)        ELEMENT CODE                                 
         DC    AL2(M#KSRCX+1-M#KSRC) DISP TO NEXT ELEMENT HEADER                
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(KSRCELQ,KSRCLNQ) ELEMENT CODE/LENGTH                         
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'TYP'            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'KSRCTYP)      DATA LENGTH                                  
         DC    AL1(KSRCTYP-KSRCELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'WMIN'           TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'KSRCWMIN)     DATA LENGTH                                  
         DC    AL1(KSRCWMIN-KSRCELD) DATA DISPLACEMENT                          
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'WMAX'           TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'KSRCWMAX)     DATA LENGTH                                  
         DC    AL1(KSRCWMAX-KSRCELD) DATA DISPLACEMENT                          
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'HMIN'           TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'KSRCHMIN)     DATA LENGTH                                  
         DC    AL1(KSRCHMIN-KSRCELD) DATA DISPLACEMENT                          
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'HMAX'           TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'KSRCHMAX)     DATA LENGTH                                  
         DC    AL1(KSRCHMAX-KSRCELD) DATA DISPLACEMENT                          
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#KSRCX  DC    AL1(0)              END-OF-ELEMENT FIELDS                        
*                                                                               
M#DSRC   DS    0X                  ** SOURCE DATA **                            
         DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(DSRCELQ)        ELEMENT CODE                                 
         DC    AL2(M#DSRCX+1-M#DSRC) DISP TO NEXT ELEMENT HEADER                
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(DSRCELQ,DSRCLNQ) ELEMENT CODE/LENGTH                         
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'TYP'            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'DSRCTYP)      DATA LENGTH                                  
         DC    AL1(DSRCTYP-DSRCELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'WTH'            TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'DSRCWTH)      DATA LENGTH                                  
         DC    AL1(DSRCWTH-DSRCELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'HGT'            TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'DSRCHGT)      DATA LENGTH                                  
         DC    AL1(DSRCHGT-DSRCELD) DATA DISPLACEMENT                           
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'ALGN'           TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'DSRCALGN)     DATA LENGTH                                  
         DC    AL1(DSRCALGN-DSRCELD) DATA DISPLACEMENT                          
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'IND1'           TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'DSRCIND1)     DATA LENGTH                                  
         DC    AL1(DSRCIND1-DSRCELD) DATA DISPLACEMENT                          
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(20)             MAPPING CODE                                 
         DC    CL5'TLEN'           TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'DSRCTLEN)     DATA LENGTH                                  
         DC    AL1(DSRCTLEN-DSRCELD) DATA DISPLACEMENT                          
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(21)             MAPPING CODE                                 
         DC    CL5'TEXT'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(DSRCTEXT-DSRCELD) DATA DISPLACEMENT                          
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(SNDTEXT-CLB54)  SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(22)             MAPPING CODE                                 
         DC    CL5'SPACE'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVSPACE-CLB54) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#DSRCX  DC    AL1(0)              END-OF-ELEMENT FIELDS                        
*                                                                               
M#DSRX   DS    0X                  ** SOURCE DATA **                            
         DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(DSRXELQ)        ELEMENT CODE                                 
         DC    AL2(M#DSRXX+1-M#DSRX) DISP TO NEXT ELEMENT HEADER                
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(DSRXELQ,DSRXLNQ) ELEMENT CODE/LENGTH                         
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(21)             MAPPING CODE                                 
         DC    CL5'TEXT'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(DSRXTEXT-DSRXELD) DATA DISPLACEMENT                          
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(SNDTEXT-CLB54)  SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(22)             MAPPING CODE                                 
         DC    CL5'SPACE'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVSPACE-CLB54) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#DSRXX  DC    AL1(0)              END-OF-ELEMENT FIELDS                        
*                                                                               
MAPTABX  DC    X'00'               E-O-T                                        
         EJECT                                                                  
* GEGENPCB                                                                      
*        PRINT OFF                                                              
       ++INCLUDE GEGENPCB                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* SAVED WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   OSVALS                                                           
         DS    (L'OSVALS-(*-OSVALS))X                                           
         EJECT                                                                  
* ACCLBLINK                                                                     
       ++INCLUDE ACCLBLINK                                                      
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINKD    DSECT                                                                  
         ORG   OVRWS                                                            
REQINDS  DS    XL1                 REQUEST INDICATORS                           
REQIAGY  EQU   X'80'               AGENCY LEVEL RECORDS                         
REQIRDS  EQU   X'40'               RECEIVED DATA SOURCE KEY#                    
REQIRKW  EQU   X'20'               RECEIVED KEYWORD LIST NUMBER                 
LANG     DS    XL1                 LANGUAGE CODE                                
CTRYMASK DS    XL4                 COUNTRY CODE MASK                            
         DS    (L'OVRWS-(*-OVRWS))X                                             
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'225ACCLB54   08/16/00'                                      
         END                                                                    
