*          DATA SET DERENTPI   AT LEVEL 003 AS OF 09/13/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE DERNTPIA                                                                 
         TITLE '- DEMO CONVERSION - RENTRAK (BASED ON CABLE MIT)'               
*                                                                               
**********************************************************************          
* INPUT-OUTPUT LOGIC:                                                           
* ------------------                                                            
* '00' - EXTENDED CABLE MIT                                                     
* '01' - UNIVERSES.  ONE FOR EACH CABLE NET            H-P0-P0                  
* '03' - HUTS. ONE FOR EACH CABLE NET.                 H-P2-P2                  
*             P2 ->  PNN HUT*C  '*'= UNIQUE FOR EACH CABLE NET                  
* '04' - PROGRAM. PRG AUDIENCES (Q'S) AND PRG PUTS     H-P0-P0-P2-P2            
*             P0 ->  QNN PRG RECDS AUDIENCE DEMOS                               
*             P2 ->  APPEND PUTS ON Q PRG RECD AFTER AUD AND UNIVS              
* '05' - USAGE RECDS FOR EACH CABLE NET.               H-P0-P0                  
*             P2 ->  PNN NET-C   TIME PERIOD USAGE VIEWG TO CABLE NET           
*            NOTE:  IGNORE '05'-P2 RECORDS WHICH HAVE NO H'S SINCE              
*                   THEY APPEAR TO BE DUPLICATES OF '03'-P2'S                   
*        ---------------------------------                                      
* REGISTERS:                                                                    
* ---------                                                                     
*        R2  - AIREC (INTERD)                                                   
*        R8  - DEMCOND GLOBAL WORKING STORAGE                                   
*        R3  - *AVAILABLE*                                                      
*        R4  - *AVAILABLE*                                                      
*        RA  - *AVAILABLE*                                                      
*        RB  - BASE REG                                                         
*        RC  - ARREC (INPUT TAPE RECORD)                                        
*                                                                               
*        R0  - WORK                                                             
*        R1  - WORK                                                             
*        R5  - WORK                                                             
*        R6  - WORK                                                             
*        R7  - WORK                                                             
*        RE  - WORK                                                             
*        RF  - WORK                                                             
*                                                                               
*NOTE:   DEDEMTIME CONTAINS THE VHRTOQH ROUTINES                                
**********************************************************************          
         EJECT                                                                  
DERENTPI CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DERENTPI                                                       
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
         L     RC,ARREC            RC = INPUT RECORD FROM TAPE (VL)             
         USING R_RECD,RC                                                        
         L     R2,AIREC            R2 = INTERIM RECD  (INTERD)                  
         USING INTERD,R2           KEY, VALUES, AND DEMO VALUES                 
         B     *+4(R1)             ROUTINE HOOK                                 
         SPACE 1                                                                
         B     READ                PROCESS INPUT TAPE                           
         B     CNVWR               SORT RECORD HOOK                             
         B     ENDJOB              E-O-F ON INPUT                               
         B     EXIT                                                             
         EJECT                                                                  
* *********************************************************************         
* READ - GET INPUT TAPE RECORDS ONE AT A TIME AND PROCESS. BUILD                
*        INTERIM RECDS.                                                         
* *********************************************************************         
READ     DS    0H                                                               
         CLI   INTAPESW,1          TEST FOR OPENING INPUT TAPE                  
         BE    READ25                                                           
         MVI   NOTAVAL,0           DATA NOT AVAIL SWITCH                        
         MVI   MYMODE,X'FF'        SET TO 1ST-TIME-THRU (GET RDR 1ST)           
         OPEN  (IN1,(INPUT))                                                    
                                                                                
READ25   L     R7,ARREC                                                         
         GET   IN1,(R7)            GET NEXT RECORD                              
*                                                                               
READ40   DS    0H                  DETERMINE/BRANCH TO RECD TYPE                
         XC    INTKEY,INTKEY                                                    
*                                                                               
READ41   ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,VWTYPTTB  GET A(VIEWING TYPES DESCRIPTION)             
         ICM   R5,15,DMCB          A(TABLE) RETURNED IN P1                      
         BNZ   *+6                 TEMP                                         
         DC    H'0'                BAD TABLEID PASSED                           
         USING VWTYPTD,R5                                                       
READ41A  CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                VIEWING TYPE NOT FOUND                       
         CLC   R_PLAYBACK,VWTYPE                                                
         BE    *+12                                                             
         LA    R5,VWTYPTL(R5)                                                   
         B     READ41A                                                          
         MVC   INTKSRC,VWTKSRC                                                  
         MVI   INTRTYP,C'P'        TIME PERIOD RECORDS                          
         MVC   INTMRKT,=H'310'     CABLE MKT FROM WEEKLY CABLE MIT              
         MVC   INTSTA,R_NETWORK_CODE                                            
         MVI   INTSTA+4,C'Q'       QHR HOUR RECORD                              
* GET BOOK INTO YYMMDD FORMAT                                                   
         MVC   TAPEWK,R_AIRDATE+8  START DATE EXCLUDING CENTURY                 
         MVC   TAPEWK+2(2),R_AIRDATE                                            
         MVC   TAPEWK+4(2),R_AIRDATE+3                                          
         GOTO1 VNETWEEK,DMCB,TAPEWK,VGETDAY,VADDAY                              
         MVC   INTBOOK(1),4(R1)       YEAR                                      
         MVC   INTBOOK+1(1),8(R1)     WEEK NUMBER                               
         MVC   INTIBOOK,INTBOOK                                                 
*                                                                               
         GOTO1 VGETDAY,DMCB,TAPEWK,DAYOFWK                                      
         MVI   INTSTYP,0                                                        
         MVI   INTMTYP,0                                                        
         LA    RE,DAYTAB                                                        
READ60   CLC   DAYOFWK,0(RE)        CONVERT DAY CODE TO INTERNAL CODE           
         BE    READ68                                                           
         LA    RE,L'DAYTAB(RE)                                                  
         CLI   0(RE),X'FF'         END OF TABLE                                 
         BNE   READ60                                                           
         DC    H'0'                                                             
READ68   MVC   INTDAYWK,3(RE)      INTERNAL CODE                                
*COMPUTE MILITARY TIME FOR NUMERIC MANIPULATIONS                                
         PACK  DUB,R_AIRTIME(4)    START HOUR                                   
         CVB   RF,DUB                                                           
         STCM  RF,3,INTSTIM        START TIME IN MILITARY                       
         LA    RF,15(RF)           BUMP QTR HOUR DURATION                       
         STCM  RF,3,INTETIM        END TIME                                     
         MVI   INTADUR,15          QTR HOUR RECORD                              
         MVI   INTDUR,1            QTR HOUR RECORD                              
         MVI   INTDURM,15                                                       
         MVC   INTDURM2,=Y(15)     2-BYTE DURATION FIELD                        
         GOTO1 VHRTOQH,DMCB,INTSTIM,INTSQH                                      
         GOTO1 VHRTOQH,DMCB,INTETIM,INTEQH                                      
         MVI   INTBTYP,0                                                        
*NOT SURE IF I NEED TO SET THIS FOR OPHASE                                      
**       MVI   INTORIG,C'0'                                                     
* NOTE- WE ARE NOT DEALING WITH CORRECTIONS NOW...NIELSEN WEEKLY CAB            
* MIT INSERTS CORRECTION DATE INTO INCRDAT - I BELIEVE WE DONT NEED TO          
* WORRY ABOUT THIS UNTIL WE PROCESS PAV                                         
*        MVC   INTANET            SET TO CALL LETTER IN TABLE IF NEEDED         
*                                                                               
         XC    INTPNUM,INTPNUM                                                  
                                                                                
         MVC   PACK16(10),R_PROGRAM_CODE   TO PACKED W/OUT SIGN                 
         MVI   PACK16+11,C'0'                                                   
         PACK  DUB,PACK16(11)      PACK FIELD TO 6 CHARS                        
         MVC   INTPNUM(5),DUB+2    5 CHAR PRG NUM (DROP SIGN)                   
                                                                                
         MVC   INTPNAME,R_PROGRAM_NAME                                          
         LA    RE,R_AHH                                                         
         STCM  RE,15,DMCB+4                                                     
         LA    RE,INTACCS                                                       
         STCM  RE,15,DMCB                                                       
         BAS   RE,SLOTDEM                                                       
         B     EXIT                                                             
*                                                                               
RELEASE  DS    0H                  RTN TO CNTLR- WITH A RECD TO RELEASE         
EXIT     XMOD1 1                                                                
XIT      XIT1                      END OF PROCEDURE- RETURN TO CALLER           
*                                                                               
         EJECT                                                                  
* *********************************************************************         
* SLOTDEM -    SLOT DEMOS IN APPROPRIATE BUCKETS IN BUFFER                      
*        INPUT PARMS:  DMCB    - A(OUTPUT BUFER)                                
*                      DMCB+4  - A(1ST DEMO)                                    
* *********************************************************************         
*                                                                               
SLOTDEM  NTR1                                                                   
*                                                                               
SLOT15   L     R1,=A(SLOTTAB)      FIND DEMO NUMBER IN TABLE                    
SLOT18   CLI   0(R1),X'FF'         END TABLE?                                   
         BE    SLOTDEMX                                                         
         ZICM  R5,0(R1),(3)        INPUT RECORD DEMO DISPLACEMENT               
         ZICM  RE,2(R1),(3)        DEMDISP DISPLACEMENT                         
         MHI   RE,4                4 BYTE OUTPUT BUCKETS                        
         A     RE,DMCB             RE=A(OUTPUT SLOT)                            
         L     RF,DMCB+4           A(INPUT DEMOS)                               
         MHI   R5,4                10 BYTES INPUT DEMOS                         
         A     R5,DMCB+4           R5=A(INPUT SLOT)                             
*                                                                               
SLOT19C  MVC   0(4,RE),0(R5)       SAVE DEMO IN BUCKET                          
         AHI   R1,4                                                             
         B     SLOT18                                                           
*                                                                               
SLOTDEMX B     XIT                                                              
         EJECT                                                                  
********************************************************************            
CNVWR    DS    0H                                                               
         MVI   BYPSORT,0           RELEASE RECD                                 
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* EOF ON INPUT FILE                                                             
**********************************************************************          
*                                                                               
ENDJOB   DS    0H                                                               
         CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         EJECT                                                                  
* --------------------------------------------------------------------          
* LITERAL POOL                                                                  
* --------------------------------------------------------------------          
         LTORG                                                                  
         SPACE 2                                                                
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               MACRF=GM,                                               X        
               EODAD=ENDJOB                                                     
         EJECT                                                                  
**********************************************************************          
*DAYTAB  - INPUT TAPE DAY CODE TO INTERIM DAY CODE                              
**********************************************************************          
DAYTAB   DS    0CL4                       DAY CONVERSION TABLE                  
         DC    CL3'MON',X'10'                                                   
         DC    CL3'TUE',X'20'                                                   
         DC    CL3'WED',X'30'                                                   
         DC    CL3'THU',X'40'                                                   
         DC    CL3'FRI',X'50'                                                   
         DC    CL3'SAT',X'60'                                                   
         DC    CL3'SUN',X'70'                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*                    WORKING STORAGE                                            
**********************************************************************          
         DS    0D                                                               
PACK8    DS    PL8                                                              
PACK16   DS    PL16                                                             
*                                                                               
BYTE     DS    C                                                                
TAPEWK   DS    CL6                 CABLE MKT=310                                
DAYOFWK  DS    CL3                                                              
NOTAVAL  DS    C                   DATA NOT AVAILABLE                           
*                                                                               
MYMODE   DC    X'00'                                                            
         EJECT                                                                  
**********************************************************************          
* RT..=  INPUT TAPE  DEMO BUCKET DEFINITIONS                                    
**********************************************************************          
*DEMO        BUCKET                                                             
*-----       ------                                                             
RTFAHOME   EQU   0                                                              
RTFAHH1834 EQU   1                                                              
RTFAHH1844 EQU   2                                                              
RTFAHH2554 EQU   3                                                              
RTFAHH1824 EQU   4                                                              
RTFAHH2534 EQU   5                                                              
RTFAHH3544 EQU   6                                                              
RTFAHH4554 EQU   7                                                              
RTFAHH5564 EQU   8                                                              
RTFAHH65O  EQU   9                                                              
RTFAHH3564 EQU   10                                                             
*                                                                               
RTFUHOME   EQU   11                                                             
RTFUHH1834 EQU   12                                                             
RTFUHH1844 EQU   13                                                             
RTFUHH2554 EQU   14                                                             
RTFUHH1824 EQU   15                                                             
RTFUHH2534 EQU   16                                                             
RTFUHH3544 EQU   17                                                             
RTFUHH4554 EQU   18                                                             
RTFUHH5564 EQU   19                                                             
RTFUHH65O  EQU   20                                                             
RTFUHH3564 EQU   21                                                             
*                                                                               
RTFRHOME   EQU   22                                                             
RTFRHH1834 EQU   23                                                             
RTFRHH1844 EQU   24                                                             
RTFRHH2554 EQU   25                                                             
RTFRHH1824 EQU   26                                                             
RTFRHH2534 EQU   27                                                             
RTFRHH3544 EQU   28                                                             
RTFRHH4554 EQU   29                                                             
RTFRHH5564 EQU   30                                                             
RTFRHH65O  EQU   31                                                             
RTFRHH3564 EQU   32                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
* RI  =  INTERIM RECD DEMO DEFINITIONS                                          
*--NOTE--THIS SET OF BUCKET POSITION EQUATES MUST BE IN SYNC WITH THE           
*        DEMO DISPLACEMENT TABLE FOR THIS FILE.                                 
**********************************************************************          
RIFAHOME   EQU   0                                                              
RIFAHH1834 EQU   1                                                              
RIFAHH1844 EQU   2                                                              
RIFAHH2554 EQU   3                                                              
RIFAHH1824 EQU   4                                                              
RIFAHH2534 EQU   5                                                              
RIFAHH3544 EQU   6                                                              
RIFAHH4554 EQU   7                                                              
RIFAHH5564 EQU   8                                                              
RIFAHH65O  EQU   9                                                              
RIFAHH3564 EQU   10                                                             
RIFUHOME   EQU   11                                                             
RIFUHH1834 EQU   12                                                             
RIFUHH1844 EQU   13                                                             
RIFUHH2554 EQU   14                                                             
RIFUHH1824 EQU   15                                                             
RIFUHH2534 EQU   16                                                             
RIFUHH3544 EQU   17                                                             
RIFUHH4554 EQU   18                                                             
RIFUHH5564 EQU   19                                                             
RIFUHH65O  EQU   20                                                             
RIFUHH3564 EQU   21                                                             
RIFRHOME   EQU   22                                                             
RIFRHH1834 EQU   23                                                             
RIFRHH1844 EQU   24                                                             
RIFRHH2554 EQU   25                                                             
RIFRHH1824 EQU   26                                                             
RIFRHH2534 EQU   27                                                             
RIFRHH3544 EQU   28                                                             
RIFRHH4554 EQU   29                                                             
RIFRHH5564 EQU   30                                                             
RIFRHH65O  EQU   31                                                             
RIFRHH3564 EQU   32                                                             
*                                                                               
*                                                                               
*DISPS TO PUTS AND UNIVERSES IN DEMO DISP TABLE                                 
*                                                                               
NDEMS    EQU   41                  TOTAL NUMBER DEMOS ON OUTPUT FILE            
RIPUTSQ  EQU   NDEMS*4             NUMBER DEMOS* 4BYTE BUCKETS                  
RIUNVSQ  EQU   NDEMS*4*2           *2  (AFTER PUTS)                             
RIOTHSQ  EQU   NDEMS*4*3           *3  (AFTER UNIVERSES)                        
*                                                                               
NDEMOQ   EQU   1                                                                
*                                                                               
         EJECT                                                                  
**********************************************************************          
*SLOTTAB-TRANSLATE INPUT DEMOS TO INTERIM RECORD SLOTS:                         
**********************************************************************          
SLOTTAB  DS    0H                                                               
         DC    AL2(RTFAHOME,RIFAHOME)        AVG AUDIENCE                       
         DC    AL2(RTFAHH1834,RIFAHH1834)                                       
         DC    AL2(RTFAHH1844,RIFAHH1844)                                       
         DC    AL2(RTFAHH2554,RIFAHH2554)                                       
         DC    AL2(RTFAHH1824,RIFAHH1824)                                       
         DC    AL2(RTFAHH2534,RIFAHH2534)                                       
         DC    AL2(RTFAHH3544,RIFAHH3544)                                       
         DC    AL2(RTFAHH4554,RIFAHH4554)                                       
         DC    AL2(RTFAHH5564,RIFAHH5564)                                       
         DC    AL2(RTFAHH65O,RIFAHH65O)                                         
         DC    AL2(RTFAHH3564,RIFAHH3564)                                       
         DC    AL2(RTFUHOME,RIFUHOME)                                           
         DC    AL2(RTFUHH1834,RIFUHH1834)    UNIVERSES                          
         DC    AL2(RTFUHH1844,RIFUHH1844)                                       
         DC    AL2(RTFUHH2554,RIFUHH2554)                                       
         DC    AL2(RTFUHH1824,RIFUHH1824)                                       
         DC    AL2(RTFUHH2534,RIFUHH2534)                                       
         DC    AL2(RTFUHH3544,RIFUHH3544)                                       
         DC    AL2(RTFUHH4554,RIFUHH4554)                                       
         DC    AL2(RTFUHH5564,RIFUHH5564)                                       
         DC    AL2(RTFUHH65O,RIFUHH65O)                                         
         DC    AL2(RTFUHH3564,RIFUHH3564)                                       
         DC    AL2(RTFRHOME,RIFRHOME)        RATINGS                            
         DC    AL2(RTFRHH1834,RIFRHH1834)                                       
         DC    AL2(RTFRHH1844,RIFRHH1844)                                       
         DC    AL2(RTFRHH2554,RIFRHH2554)                                       
         DC    AL2(RTFRHH1824,RIFRHH1824)                                       
         DC    AL2(RTFRHH2534,RIFRHH2534)                                       
         DC    AL2(RTFRHH3544,RIFRHH3544)                                       
         DC    AL2(RTFRHH4554,RIFRHH4554)                                       
         DC    AL2(RTFRHH5564,RIFRHH5564)                                       
         DC    AL2(RTFRHH65O,RIFRHH65O)                                         
         DC    AL2(RTFRHH3564,RIFRHH3564)                                       
         DC    X'FFFF',X'FFFF'                                                  
         EJECT                                                                  
*        DEINTD                                                                 
       ++INCLUDE DEINTD                                                         
         SPACE 1                                                                
*        DEINTCMITD                                                             
       ++INCLUDE DEINTCMITD                                                     
         EJECT                                                                  
         PRINT OFF                                                              
*        DDDPRINT                                                               
*        DEDEMFILE                                                              
*        DEDEMCNVD                                                              
*        DEDEMTABD                                                              
*        DDCOMFACS                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMCNVD                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
         PRINT ON                                                               
*        DERENTRAKD                                                             
       ++INCLUDE DERENTRAKD                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DERENTPI  09/13/13'                                      
         END                                                                    
