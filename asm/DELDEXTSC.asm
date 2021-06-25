*          DATA SET DELDEXTSC  AT LEVEL 060 AS OF 05/12/05                      
*PHASE DELDEXSA DELDEXSC                                                        
*INCLUDE LOGIO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'DELDEXT - LOAD/DUMP EXTERNAL ROUTINE FOR DEMO FILES'            
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
*        PUT   FILOUT,(R3)                                                      
         GOTO1 =V(HEXOUT),DMCB,0(R3),P+5,20                                     
         MVC   P+5(30),0(R3)                                                    
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
*        OPEN  (FILOUT,(OUTPUT))                                                
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
*                                                                               
         CLI   0(R3),C'S'                                                       
         BE    DMXRECS                                                          
*                                                                               
         CLI   0(R3),C'M'                                                       
         BNE   DMXKEEP                                                          
         CLI   5(R3),BSINDEQU      BSINDEQU = X'02'                             
         BE    DMXRECM1                                                         
         CLI   5(R3),MLINDEQU      MLINDEQU = X'00'                             
         BE    DMXRECM2                                                         
         B     DMXKEEP                                                          
*                                                                               
         USING SBKEY,R3                                                         
DMXRECS  DS    0H                                                               
         CLC   =C'SCA',SBCODE                                                   
         BNE   DMXKEEP                                                          
         CLI   SBBTYP,C'W'         NOT WEEKLY!!!!!  KEEP WEEKLY                 
         BE    DMXKEEP                                                          
         MVC   HALF,=X'6903'       NOV/04 BOOK                                  
         CLC   SBBOOK,HALF                                                      
         BNE   DMXKEEP                                                          
*                                                                               
         MVC   MKT,SBRMKT          IN THIS MARKET                               
         BAS   RE,VALMKT                                                        
         CLI   BADPTR,1            IT ISNT, TAKE IT OUT                         
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         USING BSKEY,R3                                                         
DMXRECM1 DS    0H                  "M1" RECORDS                                 
         CLC   =C'MCA',BSCODE                                                   
         BNE   DMXKEEP                                                          
         CLI   BSBTYP,C'W'          NOT WEEKLY!!!!! KEEP WEEKLY!!               
         BE    DMXKEEP                                                          
         MVC   HALF,=X'6903'                                                    
         XC    HALF,=X'FFFF'                                                    
         CLC   BSBOOK,HALF                                                      
         BNE   DMXKEEP                                                          
*                                                                               
         MVC   MKT,BSRMKT          IN THIS MARKET                               
         BAS   RE,VALMKT                                                        
         CLI   BADPTR,1            IT ISNT, TAKE IT OUT                         
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         USING MLKEY,R3                                                         
DMXRECM2 DS    0H                  "M2" RECORDS                                 
         CLC    =C'MCA',MLCODE                                                  
         BNE   DMXKEEP                                                          
         CLI   MLBTYP,C'W'          NOT WEEKLY, KEEP WEEKLY!!!                  
         BE    DMXKEEP                                                          
         MVC   HALF,=X'6903'                                                    
         XC    HALF,=X'FFFF'                                                    
         CLC   MLBOOK,HALF                                                      
         BNE   DMXKEEP                                                          
*                                                                               
         MVC   MKT,MLRMKT          IN THIS MARKET                               
         BAS   RE,VALMKT                                                        
         CLI   BADPTR,1            IT ISNT, TAKE IT OUT                         
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
VALMKT   NTR1                                                                   
         MVI   BADPTR,0                                                         
         LA    R1,BADMKT                                                        
VAL10    CLC   MKT,0(R1)                                                        
         BNE   VAL20                                                            
         MVI   BADPTR,1                                                         
         B     VALX                                                             
VAL20    LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   VAL10                                                            
         B     VALX                                                             
*                                                                               
VALX     XIT1                                                                   
*                                                                               
DMXEOF   DS    0H                                                               
         GOTO1 VPRINTER                                                         
         MVC   P+5(34),=C'TOTAL NUMBERS OF RECORDS PURGED'                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVI   P+5,C'-'                                                         
         MVC   P+6(31),P+5                                                      
         GOTO1 VPRINTER                                                         
         MVI   P+5,C'='                                                         
         GOTO1 VPRINTER                                                         
*        CLOSE FILOUT                                                           
         B     DMXIT                                                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
FILNAME  DC    CL7' '                                                           
SUBFILE  DC    C' '                                                             
         EJECT                                                                  
BADMKT   DS    0C                                                               
         DC    AL2(9602)           SASKATOON CTV                                
         DC    AL2(4350)           SHERBROOKE CM                                
         DC    X'FFFF'                                                          
         EJECT                                                                  
*********************************************************************           
STATAB   DS    0CL5                                                             
         DC    CL5'WGRZT'                                                       
         DC    CL5'WIVBT'                                                       
         DC    CL5'WNEDT'                                                       
         DC    CL5'WUTVT'                                                       
         DC    X'FFFF'                                                          
*                                                                               
         SPACE 2                                                                
* ROUTINE TABLE                                                                 
*                                                                               
LOGIO    DC    V(LOGIO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
         SPACE 2                                                                
* BUCKET TABLE                                                                  
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
RECIN    DC    F'0',CL20'RECORDS IN'                                            
ARBRECS  DC    F'0',CL20'ARB RECORDS'                                           
NSIRECS  DC    F'0',CL20'NSI RECORDS'                                           
ORECS    DC    F'0',CL20'OTHER RECORDS'                                         
RECKEEP  DC    F'0',CL20'RECORDS KEPT'                                          
BUCKETS  EQU   (*-BUCKTAB)/L'BUCKTAB                                            
         SPACE 2                                                                
*ILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=FB,LRECL=255,BLKSIZE=255,           
*              MACRF=PM                                                         
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
RELO     DS    A                                                                
HALF     DS    H                                                                
WORK     DS    CL60                                                             
ELCODE   DS    C                                                                
GOODPTR  DS    F                                                                
MKT      DS    XL2                                                              
STN      DS    CL5                                                              
BADPTR   DS    XL1                                                              
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* DEDEMFILE                                                                     
       ++INCLUDE DEDEMFILE                                                      
         SPACE 2                                                                
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060DELDEXTSC 05/12/05'                                      
         END                                                                    
