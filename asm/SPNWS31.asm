*          DATA SET SPNWS31    AT LEVEL 102 AS OF 02/27/07                      
*PHASE T20731C,*                                                                
         TITLE 'T20731 - !@#$'                                                  
T20731   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20731**,RA,RR=RE,CLEAR=YES                                    
         USING TWAD,R5             R5=A(TWA)                                    
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         ST    RE,APRELO                                                        
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         LA    R2,IOKEY                                                         
         USING BWHRECD,R2                                                       
         LA    R3,APRECKEY                                                      
         USING BWDRECD,R3                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   XC    BWHKEY,BWHKEY       SET UP HEADER KEY                            
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
*                                                                               
         GOTO1 AVALMED,DETMEDH     VALIDATE MEDIA                               
         BNE   VALKX                                                            
         MVC   BWHKAGMD,BAGYMD                                                  
*                                                                               
         GOTO1 AVALBYR,DETBYRH     VALIDATE BUYER                               
         BNE   VALKX                                                            
         MVC   BWHKBYR,BBYR                                                     
         OC    BYRPW,BYRPW         BUYER PASSWORD                               
         BZ    VALK10                                                           
         GOTO1 AVALPWD                                                          
         BNE   VALKX                                                            
*                                                                               
VALK10   GOTO1 AVALCAM,DETNUMH     VALIDATE CAMPAIGN NUMBER                     
         BNE   VALKX                                                            
         MVC   BWHKCAM,BCAM                                                     
*                                                                               
         GOTO1 AVALSTA,DETSTAH     VALIDATE STATION                             
         BNE   VALKX                                                            
         MVC   BWHKMKT,BMKT        STATION'S MARKET                             
*                                                                               
         GOTO1 AIO,DIRHID+IO1      READ CAMPAIGN MARKET HEADER POINTER          
         BNE   VALKX                                                            
         CLC   BWHKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                  
         BNE   VALKX                                                            
         BAS   RE,IOCHECK                                                       
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AIO,FILGET1          READ CAMPAIGN MARKET HEADER POINTER         
         BNE   VALKX                                                            
*                                                                               
         L     R2,AIOAREA1                                                      
         SR    R0,R0                                                            
         LA    R6,BWHFSTEL         R6 = A(1ST ELEMENT)                          
         USING BWHEL,R6                                                         
VALK20   CLI   0(R6),0                                                          
         BE    VALK99                                                           
         CLI   0(R6),BWHELCDQ      STATION ELEMENT?                             
         BE    VALK30                                                           
VALK25   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VALK20                                                           
*                                                                               
VALK30   CLC   QSTA,BWHSTA                                                      
         BNE   VALK25                                                           
         MVC   SVSTASEQ,BWHSEQ     SAVE STATION SEQUENCE NUMBER                 
         DROP  R6                                                               
*                                                                               
VALKYES  MVC   FVMSGNO,=AL2(FVFOK) SET OK RETURN CODE                           
         MVC   APRECKEY,BWHKEY                                                  
         B     VALKX                                                            
*                                                                               
VALK98   MVC   FVMSGNO,=AL2(FVFSKHC)    SELECTED RECORD KEY HAS CHANGED         
         LA    R1,DETSTAH          IN PARTICULAR, STATION                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
*                                                                               
VALK99   MVC   FVMSGNO,=AL2(FVFERNF)    RECORD NOT FOUND                        
*                                                                               
VALKX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK IO ERROR BITS                                      *         
* IN : IOERR  - IO ERROR RETURN BYTE                                  *         
* OUT: APINDS - APPLICATION INDICATORS BYTE                           *         
*      CC     - EQ  OK                                                *         
*             - NE  NON RECOVERABLE ERROR                             *         
***********************************************************************         
         SPACE 1                                                                
IOCHECK  TM    IOERR,IOEDSK        NON-RECOVERABLE DISK ERROR                   
         BO    IOCH99                                                           
*                                                                               
         MVI   APINDS,0                                                         
         OI    APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
*                                                                               
         TM    IOERR,IOEEOF        END-OF-FILE                                  
         BO    *+12                                                             
         TM    IOERR,IOERNF        RECORD NOT FOUND                             
         BZ    *+12                                                             
         NI    APINDS,FF-APIOKDIS-APIOKCHA-APIOKDEL                             
         OI    APINDS,APIOKADD                                                  
*                                                                               
         TM    IOERR,IOEDEL         RECORD IS DELETED                           
         BZ    *+12                                                             
         NI    APINDS,FF-APIOKADD-APIOKCHA-APIOKDEL                             
         OI    APINDS,APIOKRES                                                  
*                                                                               
         CR    RE,RE                                                            
         B     IOCHX                                                            
*                                                                               
IOCH99   MVC   FVMSGNO,=AL2(FVFIOER)    IO ERROR                                
         LA    R1,DETMEDH                                                       
         ST    R1,FVADDR                                                        
         LTR   RE,RE                                                            
*                                                                               
IOCHX    BR    RE                                                               
*                                                                               
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALREC   OC    BYRPW,BYRPW         TEST BUYER PASSWORD REQUIRED                 
         BZ    VALR10                                                           
         GOTO1 AVALPWD             YES-VALIDATE IT                              
         BNE   VALRX                                                            
*                                                                               
VALR10   DS    0H                                                               
         L     R3,AIOAREA2                                                      
         L     R5,ATWA                                                          
         XC    BWDRECD(256),BWDRECD                                             
         MVC   BWDKEY,APRECKEY                                                  
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKSEQ,BWHKSEQ                                                  
*                                                                               
         XC    BWDKEL,BWDKEL                                                    
         MVI   BWDKELCD,BWDELCDQ                                                
         MVC   BWDKELST,SVSTASEQ                                                
*                                                                               
         L     R2,AMINBLK                                                       
         USING MINBLKD,R2                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,BWDELCDQ                                                 
         MVC   MINEKEY+1(L'SVSTASEQ),SVSTASEQ                                   
         DROP  R2                                                               
*                                                                               
         MVC   IOKEY(13),BWDKEY                                                 
         LA    R8,1                                                             
VALR15   LA    R1,MINHI3           ALL DETAILS WITH STATION CODE                
*                                                                               
VALR20   GOTO1 AMIN                                                             
         BNE   VALRX                                                            
*                                                                               
         L     R2,AMINBLK                                                       
         USING MINBLKD,R2                                                       
         L     R6,MINELEM                                                       
         USING BWDEL,R6                                                         
         CLI   BWDELCD,BWDELCDQ                                                 
         BH    VALRX                                                            
         BL    VALR30                                                           
         CLC   BWDSTACD,SVSTASEQ                                                
         BH    VALRX                                                            
         BL    VALR30                                                           
*                                                                               
         LA    R1,MINDEL3                                                       
         GOTO1 AMIN                                                             
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,BWDELCDQ                                                 
         MVC   MINEKEY+1(L'SVSTASEQ),SVSTASEQ                                   
         B     VALR15                                                           
         DROP  R2                                                               
*                                                                               
VALR30   LA    R1,MINSEQ3                                                       
         B     VALR20                                                           
*                                                                               
VALRX    B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
LOCALD   DSECT                     ** LOCAL WORKING STORAGE **                  
*                                                                               
ASPWEL   DS    A                                                                
ADEMEL   DS    A                                                                
ACOMEL   DS    A                                                                
*                                                                               
SAVER1   DS    F                   SAVED REGISTERS                              
SAVER8   DS    F                                                                
SAVER9   DS    F                                                                
SAVERE   DS    F                                                                
SAVERF   DS    F                                                                
*                                                                               
CAMPST   DS    CL6                                                              
CAMPND   DS    CL6                                                              
CMPSTDAY DS    XL1                 CAMPAIGN START DAY                           
*                                                                               
SVQPRD1  DS    CL3                                                              
SVBPRD1  DS    XL1                                                              
SVQPRD2  DS    CL3                                                              
SVBPRD2  DS    XL1                                                              
SVSTASEQ DS    XL1                                                              
*                                                                               
DEMNAMES DS    XL24                4 DEMO NAMES                                 
STA      DS    CL8                                                              
WK1END   DS    XL1                                                              
WK2END   DS    XL1                                                              
LEN2     DS    CL3                                                              
LINE     DS    CL4                                                              
LKEYCOMP DS    XL1                                                              
LFLAG    DS    XL1                                                              
LPRTHDR  EQU   X'80'                                                            
LPRTSTA  EQU   X'40'                                                            
LKEY2    EQU   X'01'                                                            
*                                                                               
         DS    0D                                                               
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSFCD                                                       
         EJECT                                                                  
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* SCHEDULE RECORD DSECTS                                              *         
***********************************************************************         
         SPACE 1                                                                
BHDRECD  DSECT                                                                  
*                                                                               
BHDREC   DS    0CL165              BROADCAST HEADER RECORD                      
BHDRECTY DS    CL3                                                              
BHDCLT   DS    CL8                                                              
BHDREF   DS    CL8                                                              
BHDCMPST DS    CL6                                                              
BHDCMPND DS    CL6                                                              
BHDDEM1  DS    CL6                                                              
BHDDEM2  DS    CL6                                                              
BHDDEM3  DS    CL6                                                              
BHDDEM4  DS    CL6                                                              
BHDCLT2  DS    CL8                                                              
BHDQMED  DS    CL1                                                              
BHDQCLT  DS    CL3                                                              
BHDQPRD1 DS    CL3                                                              
BHDQPRD2 DS    CL3                                                              
BHDQEST  DS    CL3                                                              
BHDQCAMP DS    CL5                                                              
BHDQBYR  DS    CL3                                                              
         SPACE 1                                                                
MHDRECD  DSECT                                                                  
*                                                                               
MHDREC   DS    0CL165              MARKET HEADER RECORD                         
MHDRECTY DS    CL3                                                              
MHDCLT   DS    CL8                                                              
MHDREF   DS    CL8                                                              
MHDMKT   DS    CL4                                                              
MHDBYR   DS    CL3                                                              
         SPACE 1                                                                
SHDRECD  DSECT                                                                  
*                                                                               
SHDREC   DS    0CL165              STATION HEADER RECORD                        
SHDRECTY DS    CL3                                                              
SHDREF   DS    CL8                                                              
SHDMKT   DS    CL4                                                              
SHDSTA   DS    CL6                                                              
SHDREMK  DS    CL90                                                             
         SPACE 1                                                                
SSLRECD  DSECT                                                                  
*                                                                               
SSLREC   DS    0CL165              STATION SCHEDULE LINE RECORD                 
SSLRECTY DS    CL3                                                              
SSLREF   DS    CL8                                                              
SSLMKT   DS    CL4                                                              
SSLSTA   DS    CL6                                                              
SSLLINE  DS    CL4                                                              
SSLDAYS  DS    CL7                                                              
SSLSPCL  DS    CL3                                                              
SSLTIM1  DS    CL5                                                              
SSLTIM2  DS    CL5                                                              
SSLDPT   DS    CL1                                                              
SSLLEN   DS    CL3                                                              
SSLPRG   DS    CL17                                                             
SSLCOST  DS    CL9                                                              
SSLDEM1  DS    CL9                                                              
SSLDEM2  DS    CL9                                                              
SSLDEM3  DS    CL9                                                              
SSLDEM4  DS    CL9                                                              
SSLWKS   DS    CL(13*2)                                                         
SSLCMPST DS    CL6                                                              
SSLCMPND DS    CL6                                                              
SSLLEN2  DS    CL3                                                              
         SPACE 1                                                                
LCMRECD  DSECT                                                                  
*                                                                               
LCMREC   DS    0CL165              SCHEDULE LINE COMMENT RECORD                 
LCMRECTY DS    CL3                                                              
LCMREF   DS    CL8                                                              
LCMMKT   DS    CL4                                                              
LCMSTA   DS    CL6                                                              
LCMLINE  DS    CL4                                                              
LCMSEQ   DS    CL2                                                              
LCMTEXT  DS    CL60                                                             
         EJECT                                                                  
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'102SPNWS31   02/27/07'                                      
         END                                                                    
