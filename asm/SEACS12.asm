*          DATA SET SEACS12    AT LEVEL 041 AS OF 08/11/00                      
*PHASE TA0D12A                                                                  
ACS12    TITLE '- SECURITY ACCESS - AGENCY COPY'                                
ACS12    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC12**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY            R2=A(RECORD KEY)                             
         USING SAPGREC,R2                                                       
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         SPACE 1                                                                
         CLI   APMODE,APMVALK                                                   
         BE    VALFLDS                                                          
         CLI   APMODE,APMDISR                                                   
         BE    COPYRECS                                                         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     *+6                                                              
EXITY    CR    RB,RB                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGENCY ALPHA FIELDS                                        *         
***********************************************************************         
         SPACE 1                                                                
VALFLDS  XC    DSPFNAM,DSPFNAM                                                  
         OI    DSPFNAMH+FHOID,FHOITR                                            
         XC    DSPTNAM,DSPTNAM                                                  
         OI    DSPTNAMH+FHOID,FHOITR                                            
         XC    DSPPRGD,DSPPRGD                                                  
         OI    DSPPRGDH+FHOID,FHOITR                                            
         XC    DSPACC,DSPACC                                                    
         OI    DSPACCH+FHOID,FHOITR                                             
         XC    DSPOCO,DSPOCO                                                    
         OI    DSPOCOH+FHOID,FHOITR                                             
         XC    DSPFCO,DSPFCO                                                    
         OI    DSPFCOH+FHOID,FHOITR                                             
*                                                                               
         GOTO1 VALAGY,DSPFROMH                                                  
         BNE   EXIT                                                             
         MVC   FROMAGY,FVIFLD                                                   
*                                                                               
         XC    FLTOVPG,FLTOVPG                                                  
         GOTO1 VALALL,DSPSYSH                                                   
         BNE   VFLDS02                                                          
         GOTO1 (RF),DSPPRGH                                                     
         BE    VFLDS04                                                          
         MVC   FVMSGNO,=AL2(CE#PFNVS)                                           
         B     EXIT                                                             
VFLDS02  GOTO1 AVALOVPG,PARM,DSPSYSH,(1,DSPPRGH),DSPPRGDH                       
         MVC   FLTOVPG,APHALF                                                   
         BE    VFLDS04                                                          
         CLC   FVMSGNO,=AL2(CE#PGNVS)                                           
         BNE   EXIT                                                             
         GOTO1 VALALL,DSPPRGH                                                   
         BE    VFLDS04                                                          
         MVC   FVMSGNO,=AL2(CE#PGNVS)                                           
         B     EXIT                                                             
*                                                                               
VFLDS04  GOTO1 VALAGY,DSPTOH                                                    
         BNE   EXIT                                                             
         MVC   TOAGY,FVIFLD                                                     
         MVI   APINDS,APIOKDIS                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE FIELD='ALL'                                     *         
***********************************************************************         
         SPACE 1                                                                
VALALL   NTR1  ,                                                                
         USING FHD,R1                                                           
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   EXIT                                                             
         CLC   FVIFLD(L'CT@ALL),CT@ALL                                          
         BNE   EXIT                                                             
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD+1)                                                   
         EX    RE,*+4                                                           
         XC    FHDA(0),FHDA                                                     
         MVC   FHDA(L'CT@ALL),CT@ALL                                            
         OI    FHOI,FHOITR                                                      
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AGENCY ALPHA                                    *         
*                                                                     *         
* NTRY: R1=A(FIELD HEADER)                                            *         
* EXIT: FVIFLD=AGENCY ALPHA CODE                                      *         
***********************************************************************         
         SPACE 1                                                                
VALAGY   NTR1  ,                                                                
         LR    R6,R1                                                            
         USING DSPFROMH,R6                                                      
         OI    DSPFROMH+FHOID,FHOITR                                            
         MVI   FVMINL,2                                                         
         MVI   FVMAXL,2                                                         
         GOTO1 AFVAL,DSPFROMH                                                   
         BNE   EXIT                                                             
         LA    R2,IOKEY                                                         
         USING CT5REC,R2                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,FVIFLD                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#INVAA)                                           
         B     EXIT                                                             
*                                                                               
         GOTO1 FINDEL,CTDSCELQ                                                  
         BNE   VALAGYX                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CTDSC-CTDSCD(R3)                                         
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
*                                                                               
*&&UK                                                                           
         GOTO1 FINDEL,CTDSTELQ                                                  
         BNE   VALAGYX                                                          
         MVC   DSPFNAM,CTDSTNAM-CTDSTD(R3)                                      
*&&                                                                             
*&&US                                                                           
         GOTO1 FINDEL,CTORGELQ                                                  
         BNE   VALAGYX                                                          
         MVC   DSPFNAM,CTORGNAM-CTORGD(R3)                                      
*&&                                                                             
*                                                                               
VALAGYX  B     EXITY                                                            
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* COPY RECORDS ACCROSS                                                *         
***********************************************************************         
         SPACE 1                                                                
COPYRECS GOTO1 COPY,PARM,('SAASSUBQ',DSPACCH)                                   
         GOTO1 (RF),(R1),('SAOCSUBQ',DSPOCOH)                                   
         GOTO1 (RF),(R1),('SAFCSUBQ',DSPFCOH)                                   
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(CI#ACTCP)                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* COPY ACROSS ALL RECORDS FOR A PARTICULAR SUB-TYPE                   *         
*                                                                     *         
* NTRY: P1=(RECORD SUB-TYPE, A(FIELD))                                *         
***********************************************************************         
         SPACE 1                                                                
COPY     NTR1  ,                                                                
         XR    R6,R6                                                            
         ICM   R6,7,1(R1)          R6=A(FIELD)                                  
         USING FHD,R6                                                           
*                                                                               
         LA    R2,FROMKEY                                                       
         USING SAASKEY,R2                                                       
         XC    SAASKEY,SAASKEY                                                  
         MVI   SAASTYP,SAASTYPQ                                                 
         MVC   SAASSUB,0(R1)                                                    
         MVC   SAASAGY,FROMAGY                                                  
         MVC   SAASOVPG,FLTOVPG                                                 
*                                                                               
         ZAP   TOTALCOP,PZERO                                                   
         ZAP   TOTALDUP,PZERO                                                   
*                                                                               
COPY02   MVC   IOKEY,FROMKEY                                                    
         GOTO1 AIO,IOCONFIL+IOHI+IO1                                            
         L     R2,AIOAREA1                                                      
         CLC   SAASKEY(SAASOVPG-SAASKEY),FROMKEY                                
         BNE   COPY10                                                           
         CLI   FLTOVS,0                                                         
         BE    COPY04                                                           
         CLC   SAASOVS,FLTOVS                                                   
         BNE   COPY10                                                           
         CLI   FLTPGM,0                                                         
         BE    COPY04                                                           
         CLC   SAASPGM,FLTPGM                                                   
         BNE   COPY10                                                           
*                                                                               
COPY04   MVC   FROMKEY,SAASKEY                                                  
         LA    R2,IOKEY                                                         
         MVC   SAASKEY,FROMKEY                                                  
         MVC   SAASAGY,TOAGY                                                    
*                                                                               
         GOTO1 AIO,IOCONFIL+IORDD+IO2                                           
         CLI   IOERR,IOERNF                                                     
         BE    COPY06                                                           
         AP    TOTALDUP,PONE                                                    
         B     COPY08                                                           
*                                                                               
COPY06   L     R2,AIOAREA1                                                      
         MVC   SAASAGY,TOAGY                                                    
         GOTO1 ASETACT,SAASREC                                                  
         GOTO1 AIO,IOCONFIL+IOADD+IO1                                           
         AP    TOTALCOP,PONE                                                    
*                                                                               
COPY08   LA    R2,FROMKEY                                                       
         ICM   RF,3,SAASOVPG                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,SAASOVPG                                                    
         B     COPY02                                                           
*                                                                               
COPY10   XC    APELEM,APELEM                                                    
         XR    R4,R4                                                            
         LA    R3,APELEM                                                        
*                                                                               
         EDIT  (P4,TOTALCOP),(4,1(R3)),ALIGN=LEFT,ZERO=NOBLANK                  
         LR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         STC   R4,0(R3)                                                         
         AR    R3,R4                                                            
*                                                                               
         EDIT  (P4,TOTALDUP),(4,1(R3)),ALIGN=LEFT,ZERO=NOBLANK                  
         LR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         STC   R4,0(R3)                                                         
         AR    R3,R4                                                            
*                                                                               
         LA    R2,FROMKEY                                                       
         LH    RF,=AL2(CS#ACPDP)                                                
         CLI   SAASSUB,SAASSUBQ                                                 
         BE    COPY12                                                           
         LH    RF,=AL2(CS#OCPDP)                                                
         CLI   SAASSUB,SAOCSUBQ                                                 
         BE    COPY12                                                           
         LH    RF,=AL2(CS#FCPDP)                                                
COPY12   GOTO1 VGETTXT,PARM,(RF),FHD,('GTMSCR',0),0,APELEM,0                    
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND AN ELEMENT                                          *         
*                                                                     *         
* NTRY: RECORD IN IOAREA1, R1=ELEMENT CODE                            *         
* EXIT: R3=A(ELEMENT), RF=L(ELEMENT)                                  *         
***********************************************************************         
         SPACE 1                                                                
FINDEL   L     R3,AIOAREA1         SET R3 TO 1ST ELEMENT                        
         LA    R3,SAASDATA-SAASREC(R3)                                          
         XR    RF,RF                                                            
*                                                                               
FEL02    CLI   0(R3),0             TEST E-O-R                                   
         BE    FINDELN                                                          
         IC    RF,1(R3)            RF=L(ELEMENT)                                
         CLM   R1,1,0(R3)          MATCH ON ELEMENT CODE                        
         BER   RE                                                               
         BXH   R3,RF,FEL02         BUMP R3 TO NEXT ELEMENT                      
*                                                                               
FINDELN  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
FF       EQU   X'FF'                                                            
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
         EJECT                                                                  
* SEACSDICT                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSDICT                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SEACSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSEED                                                       
         EJECT                                                                  
WORKD    DSECT                     ** DSECT TO COVER LOCAL W/S **               
         ORG   APLOCAL                                                          
DUB      DS    D                                                                
FULL     DS    F                                                                
PARM     DS    6A                                                               
HALF     DS    H                                                                
WORK     DS    XL64                                                             
FROMAGY  DS    CL2                 FROM AGENCY                                  
FROMKEY  DS    XL25                FROM RECORD KEY                              
FLTOVPG  DS    0XL2                                                             
FLTOVS   DS    XL1                 FILTER ON SYSTEM                             
FLTPGM   DS    XL1                 FILTER ON PROGRAM                            
TOAGY    DS    CL2                 TO AGENCY                                    
TOKEY    DS    XL25                TO RECORD KEY                                
TOTALCOP DS    PL4                 TOTAL ADDED                                  
TOTALDUP DS    PL4                 TOTAL DUPLICATES                             
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041SEACS12   08/11/00'                                      
         END                                                                    
