*          DATA SET SXLDEXTSD1 AT LEVEL 004 AS OF 05/16/00                      
*PHASE SXEXTSD1,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'DMLDEXT - PEEL SUPER DESK DATA OFF THE XSPOT FILE'              
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALIZE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITIALIZE                     
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALIZE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         MVI   CLTTAB,X'FF'                                                     
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   L     R3,AREC                                                          
         CLI   FLAG2,1                                                          
         BE    *+8                                                              
         BAS   RE,BLDCTAB          BUILD CLIENT TABLE                           
         B     GETTYPE             GET RECORD TYPE                              
*                                                                               
RECTYPA  MVC   BAMCLT,2(R3)        XX XX A/M CLT CLT                            
         B     DMXKX                                                            
*                                                                               
RECTYPB  MVC   BAMCLT,19(R3)       XX XX 17X A/M CLT CLT                        
         B     DMXKX                                                            
*                                                                               
DMXKX    BAS   RE,CHKBCLT          IS CLIENT IN CLIENT LIST?                    
         BNE   DMXPURGE                                                         
DMXKXP   DS    0H                                                               
         GOTO1 =V(HEXOUT),DMCB,(R3),P,32,=C'TOG'                                
         GOTO1 VPRINTER            PRINT KEY                                    
         B     DMXKEEP                                                          
*                                                                               
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
***********************************************************************         
*        PRINT OUT RECORD                                             *         
***********************************************************************         
*                                                                               
RECPRNT  NTR1                                                                   
         LA    R4,=CL20'GOAL RECORD'                                            
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'HEADER REC'                                             
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        GET CLIENTS FROM DATA SET AND BUILD CLIENT TABLE             *         
***********************************************************************         
*                                                                               
BLDCTAB  NTR1                                                                   
         GOTO1 =V(DATCON),DMCB,(5,0),(10,TODAY)                                 
         MVI   FLAG2,1                                                          
         USING CLTTABD,R4                                                       
         LA    R4,CLTTAB                                                        
         OPEN  SDCLT                                                            
         GET   SDCLT,SDDAY                                                      
         CLC   TODAY,SDDAY                                                      
         BE    BCTAB10                                                          
         MVC   ERRMESS(L'DAYERR),DAYERR                                         
         LA    R5,L'DAYERR-1       LENGTH OF ERROR MESSAGE                      
         BAS   RE,PRTERR           PRINT ERROR                                  
         DC    H'00'               MUST BE TODAY'S DATE                         
BCTAB10  GET   SDCLT,SDREC                                                      
         CLI   SDREC,C'C'                                                       
         BNE   BCTAB10                                                          
         MVC   CLTAGMD(6),SDREC+1  CLIENT TABLE                                 
         MVI   CLTLNQ(R4),X'FF'    MARK TEMPORARY END OF TABLE                  
         LA    R4,CLTLNQ(R4)                                                    
         B     BCTAB10                                                          
         DROP  R4                                                               
*                                                                               
EOFCLT   CLOSE SDCLT                                                            
         B     EXIT                                                             
***********************************************************************         
*        GET RECORD TYPE                                              *         
***********************************************************************         
*                                                                               
GETTYPE  CLI   0(R3),X'01'         BAD RECORDS                                  
         BE    DMXPURGE                                                         
         DC    H'0'                                                             
         LA    R2,ODTAB            SET DEFAULT TABLE ADDR                       
         CLI   0(R3),X'0D'                                                      
         BE    GETTYP05                                                         
         LA    R2,OETAB                                                         
         CLI   0(R3),X'0E'                                                      
         BE    GETTYP05                                                         
         MVC   ERRMESS(L'UNDEFERR),UNDEFERR                                     
         LA    R5,L'UNDEFERR-1     LENGTH                                       
         BAS   RE,PRTERR           PRINT ERROR                                  
         DC    H'00'               NOT 0D OR 0E                                 
GETTYP05 MVC   BYTE,1(R3)                                                       
         B     GETTYP15                                                         
*                                                                               
GETTYP10 CLC   0(1,R2),BYTE        LOOK FOR MATCH IN TAB                        
         BNE   GETTYP13                                                         
         BR    RF                  BRANCH TO ROUTINE                            
GETTYP13 LA    R2,1(R2)                                                         
         CLC   =X'FFFF',0(R2)      CK EOT                                       
         BE    GETTYP50                                                         
         CLI   0(R2),X'FF'         CK END OF TYPE                               
         BNE   GETTYP10                                                         
         LA    R2,1(R2)                                                         
GETTYP15 ICM   RF,15,0(R2)         ADDRESS OF ROUTINE                           
         LA    R2,4(R2)            ADVANCE TO RECORD TYPES                      
         B     GETTYP10                                                         
*                                                                               
GETTYP50 MVC   ERRMESS(L'UNDEFERR),UNDEFERR                                     
         LA    R5,L'UNDEFERR-1     LENGTH                                       
         BAS   RE,PRTERR           PRINT ERROR                                  
         DC    H'00'               IF NO MATCH IN TABLE                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        GET BCLT FROM CLIENT LIST                                    *         
***********************************************************************         
GETBCLT  NTR1                                                                   
         LA    R5,CLTTAB           MATCH EBCDIC CLT IN TABLE TO GET             
         USING CLTTABD,R5          BINARY CLIENT CODE                           
GBCLT10  CLI   0(R5),X'FF'                                                      
         BE    NO                                                               
         CLC   CLTALPH,QCLT                                                     
         BE    GBCLT20                                                          
         LA    R5,CLTLNQ(R5)                                                    
         B     GBCLT10                                                          
GBCLT20  MVC   BCLT,CLTCODE                                                     
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        CHECK IF CLIENT IS IN CLIENT LIST                            *         
***********************************************************************         
CHKBCLT  NTR1                                                                   
         LA    R5,CLTTAB                                                        
         USING CLTTABD,R5                                                       
CBCLT10  CLI   0(R5),X'FF'                                                      
         BE    NO                                                               
*        CLC   CLTAGMD,BAGYMD      CORRECT MEDIA?                               
*        BNE   CBCLT20                                                          
         CLC   CLTCODE,BCLT                                                     
         BE    CBCLTX                                                           
CBCLT20  LA    R5,CLTLNQ(R5)                                                    
         B     CBCLT10                                                          
CBCLTX   B     EXIT                                                             
*                                                                               
***********************************************************************         
* PRTERR   PRINT ERRORS BEFORE DYING                                  *         
***********************************************************************         
*                                                                               
PRTERR   NTR1                                                                   
         MVC   P(80),STARS                                                      
         GOTO1 VPRINTER                                                         
         MVC   P(27),=C'THE PROGRAM DUMPED BECAUSE:'                            
         GOTO1 VPRINTER                                                         
         EX    R5,*+4                                                           
         MVC   P(0),ERRMESS                                                     
         GOTO1 VPRINTER                                                         
         MVC   P(80),STARS                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
***********************************************************************         
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
*                                                                               
DATADISP DC    H'0024'                                                          
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
SDCLT    DCB   DDNAME=SDCLT,DSORG=PS,RECFM=FB,MACRF=GM,EODAD=EOFCLT             
         SPACE                                                                  
*                                                                               
***********************************************************************         
*        TAB OF REC TYPES ACCORDING TO AGY AND CLT POSITION IN RECORD *         
***********************************************************************         
*                                                                               
ODTAB    DC    AL4(RECTYPA),X'0CFF'        0DXX A/M CLT CLT                     
         DC    AL4(RECTYPB),X'39FFFF'      0DXX 17X A/M CLT CLT                 
*                                                                               
OETAB    DC    AL4(RECTYPA),X'030405FFFF'  0EXX A/M CLT CLT                     
*                                                                               
STARS    DC    80C'*'                                                           
UNDEFERR DC    C'RECORD TYPE NOT DEFINED'                                       
DAYERR   DC    C'CLIENT DATASET MUST BE GENERATED TODAY'                        
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
CLTTABD  DSECT                                                                  
CLTAGMD  DS    XL1                                                              
CLTCODE  DS    XL2                                                              
CLTALPH  DS    CL3                                                              
CLTLNQ   EQU   *-CLTTABD                                                        
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
CLTTAB   DS    CL601               20 ENTRIES 6 BYTES EACH                      
BYTE     DS    X                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
*                                                                               
BAMCLT   DS    0XL3                                                             
BAGYMD   DS    XL1                                                              
BCLT     DS    XL2                                                              
QCLT     DS    CL3                                                              
TODAY    DS    CL8                                                              
SDDAY    DS    CL9                                                              
ERRMESS  DS    CL40                                                             
SDREC    DS    CL9                                                              
*                                                                               
FLAG     DS    X                                                                
FLAG2    DS    X                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
*                                                                               
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SXLDEXTSD105/16/00'                                      
         END                                                                    
