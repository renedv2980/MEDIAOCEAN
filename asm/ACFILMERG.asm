*          DATA SET ACFILMERG  AT LEVEL 033 AS OF 05/01/02                      
*PHASE ACCMERGA                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'DATA SET MERGE'                                                 
*---------------------------------------------------------------------*         
*        DO NOT DELETE/CHANGE THIS PROGRAM!                           *         
*        RUN DATE -->                                                 *         
*---------------------------------------------------------------------*         
ACLDXCN3 CSECT                                                                  
BGN      DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**CN3**,R9,R8                                                  
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
                                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        INITIALIZE                                                   *         
*---------------------------------------------------------------------*         
         RELOC RELO                                                             
         LA    RE,ATYPES                                                        
INIT01   L     RF,0(RE)            RELOCATE A TYPES                             
         A     RF,RELO                                                          
         ST    RF,0(RE)                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   INIT01                                                           
                                                                                
         OPEN  (MYSRT1,(INPUT))    OPEN MY OUTPUT TAPE                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OPEN  (MYSRT2,(INPUT))    OPEN MY OUTPUT TAPE                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OPEN  (MYTOUT,(OUTPUT))    OPEN MY OUTPUT TAPE                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        GET RECORDS FROM SORTER AND CREATE TAPEOUT                   *         
*        WILL SIMULATE A TAPE MERGE - COMPARES ARE HARDCODED          *         
*---------------------------------------------------------------------*         
RECMERGE DS    0H                                                               
         L     RE,ASRT1            CLEAR OUT SORT AREA                          
         LA    RF,2500                                                          
         XCEFL (RE),(RF)                                                        
                                                                                
         L     RE,ASRT2            CLEAR OUT SORT SAVE AREA                     
         LA    RF,2500                                                          
         XCEFL (RE),(RF)                                                        
                                                                                
         L     R2,AMYSRT1          GET THE FIRST RECORD FROM FIRST              
         L     R4,ASRT1            DATASET                                      
         GET   (R2),(R4)                                                        
                                                                                
         L     R2,AMYSRT2          GET THE FIRST RECORD FROM SECOND             
         L     R4,ASRT2            DATASET                                      
         GET   (R2),(R4)                                                        
                                                                                
MRG050   DS    0H                                                               
         L     R2,ASRT1                                                         
         L     R3,ASRT2                                                         
         CLC   9(84,R2),9(R3)      DO FIRST COMPARE                             
         BE    MRG100              IF EQUAL MUST DO SECOND COMPARE              
         BL    MRG150              IF LOW PUT OUT RECORD ONE                    
         BH    MRG160              IF HIGH PUT OUT RECORD TWO                   
                                                                                
MRG100   CLC   4(2,R2),4(R3)       DO SECOND COMPARE                            
         BE    MRG150              IF EQUAL PUT OUT RECORD ONE                  
         BL    MRG150              IF LOW PUT OUT RECORD ONE                    
         BH    MRG160              IF HIGH PUT OUT RECORD TWO                   
                                                                                
MRG150   ST    R2,ADDPUT           STORE ADDRESS OF RECORD ONE                  
         BAS   RE,TAPPUT           AND PUT THIS RECORD TO TAPE                  
         L     R2,AMYSRT1          GET NEXT RECORD FROM DATASET ONE             
         L     R4,ASRT1                                                         
         GET   (R2),(R4)                                                        
         B     MRG050              GO THROUGH COMPARE LOGIC                     
                                                                                
MRG160   ST    R3,ADDPUT           STORE ADDRESS OF RECORD TWO                  
         BAS   RE,TAPPUT           AND PUT THIS RECORD TO TAPE                  
         L     R2,AMYSRT2          GET NEXT RECORD FROM DATASET TWO             
         L     R4,ASRT2                                                         
         GET   (R2),(R4)                                                        
         B     MRG050              GO THROUGH COMPARE LOGIC                     
                                                                                
MENDSR1  DS    0H                  IF EOF FOR DATASET ONE                       
         TM    ENDED,YES           DID I ALREADY COMPLETE DATASET TWO           
         BO    MENDXIT             IF YES EXIT                                  
         L     R3,ASRT2            ELSE PUT OUT RECORD IN BUFFER FROM           
         ST    R3,ADDPUT           DATASET TWO                                  
         BAS   RE,TAPPUT                                                        
         OI    ENDED,YES           SET SWITCH FOR EOF ON A DATASET              
MEND1A   L     R2,AMYSRT2          AND PUT OUT THE REST OF THE RECORDS          
         L     R4,ASRT2            ON DATASET TWO                               
         GET   (R2),(R4)                                                        
         BAS   RE,TAPPUT                                                        
         B     MEND1A                                                           
                                                                                
MENDSR2  DS    0H                  IF EOF FOR DATASET TWO                       
         TM    ENDED,YES           DID I ALREADY COMPLETE DATASET ONE           
         BO    MENDXIT             IF YES EXIT                                  
         L     R3,ASRT1            ELSE PUT OUT RECORD IN BUFFER FROM           
         ST    R3,ADDPUT           DATASET ONE                                  
         BAS   RE,TAPPUT                                                        
         OI    ENDED,YES           SET SWITCH FOR EOF ON A DATASET              
MEND2A   L     R2,AMYSRT1          AND PUT OUT THE REST OF THE RECORDS          
         L     R4,ASRT1            FROM DATASET ONE                             
         GET   (R2),(R4)                                                        
         BAS   RE,TAPPUT                                                        
         B     MEND2A                                                           
                                                                                
MENDXIT  DS    0H                                                               
         CLOSE (MYSRT1)            CLOSE INPUT DATASET ONE                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLOSE (MYSRT2)            CLOSE INPUT DATASET TWO                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLOSE (MYTOUT)            CLOSE MERGED OUTPUT DATASET                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ROUTINE TO PUT RECORDS TO TAPEOUT                            *         
*---------------------------------------------------------------------*         
TAPPUT   NTR1                                                                   
         L     R3,AMYTOUT                                                       
         L     R2,ADDPUT                                                        
         PUT   (R3),(R2)                                                        
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DITTO OUTPUT RECORDS                                         *         
*---------------------------------------------------------------------*         
DMPPUT   CP    PDUMP,MAXDUMP                                                    
         BHR   RE                                                               
                                                                                
         NTR1                                                                   
         L     R5,ADDPUT                                                        
         SR    RF,RF                                                            
         ICM   RF,3,0(R5)                                                       
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   DMPX                                                             
         AP    PDUMP,=P'1'                                                      
         GOTO1 PRNTBL,DMCB,(0,0),(R5),C'DUMP',(RF),=C'2D'                       
                                                                                
DMPX     DS    0H                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        WORK AREA                                                    *         
*---------------------------------------------------------------------*         
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
AIPTDCB  DC    F'0'                A(INPUT FILE DCB)                            
AMYSRT1  DC    A(MYSRT1)           ADDR OF RECOVERY TAPE DCB                    
AMYSRT2  DC    A(MYSRT2)           ADDR OF RECOVERY TAPE DCB                    
AMYTOUT  DC    A(MYTOUT)           ADDR OF RECOVERY TAPE DCB                    
ASRT1    DC    A(SRT1)                                                          
ASRT2    DC    A(SRT2)                                                          
AOUTREC  DC    A(OUTREC)                                                        
PRNTBL   DC    V(PRNTBL)                                                        
PRINT    DC    V(PRINT)                                                         
HEXIN    DC    V(HEXIN)                                                         
HEXOUT   DC    V(HEXOUT)                                                        
VPRINTER DS    V(PRINTER)          VPRINTER                                     
         DC    X'FF'                                                            
                                                                                
ADDPUT   DC    A(0)                                                             
                                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
         DS    CL8                                                              
WORK     DS    CL20                                                             
         DS    CL8                                                              
BYTE     DS    CL1                                                              
                                                                                
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'5000'                                                        
                                                                                
ENDED    DC    XL1'00'                                                          
YES      EQU   X'80'                                                            
                                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        LITERAL DECLARATIONS                                         *         
*---------------------------------------------------------------------*         
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SORT AREA                                                    *         
*---------------------------------------------------------------------*         
         DS    0D                                                               
         DC    CL8'**SRT1**'                                                    
SRT1     DS    CL(2500)                                                         
                                                                                
*---------------------------------------------------------------------*         
*        SORT AREA                                                    *         
*---------------------------------------------------------------------*         
         DS    0D                                                               
         DC    CL8'**SRT2**'                                                    
SRT2     DS    CL(2500)                                                         
                                                                                
*---------------------------------------------------------------------*         
*        RECOVERY AREA                                                *         
*---------------------------------------------------------------------*         
         DS    0D                                                               
         DC    CL8'*OUTREC*'                                                    
OUTREC   DS    CL(2500)                                                         
                                                                                
*---------------------------------------------------------------------*         
*        DCB'S FOR RECOVERY TAPE                                      *         
*---------------------------------------------------------------------*         
*                                                                               
MYSRT1   DCB   DDNAME=MYSRT1,RECFM=VB,DSORG=PS,MACRF=(GM),             X        
               BLKSIZE=32760,LRECL=4004,BUFNO=2,EODAD=MENDSR1                   
                                                                                
*---------------------------------------------------------------------*         
*        DCB'S FOR RECOVERY TAPE                                      *         
*---------------------------------------------------------------------*         
*                                                                               
MYSRT2   DCB   DDNAME=MYSRT2,RECFM=VB,DSORG=PS,MACRF=(GM),             X        
               BLKSIZE=32760,LRECL=4004,BUFNO=2,EODAD=MENDSR2                   
                                                                                
*---------------------------------------------------------------------*         
*        DCB'S FOR RECOVERY TAPE                                      *         
*---------------------------------------------------------------------*         
*                                                                               
MYTOUT   DCB   DDNAME=MYTOUT,RECFM=VB,DSORG=PS,MACRF=(PM),             X        
               BLKSIZE=32760,LRECL=4004,BUFNO=2                                 
                                                                                
*---------------------------------------------------------------------*         
*        DCB DECLARATION                                              *         
*---------------------------------------------------------------------*         
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        INCLUDES                                                     *         
*---------------------------------------------------------------------*         
                                                                                
* DDDPRINT                                                                      
* DMWRKRD                                                                       
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033ACFILMERG 05/01/02'                                      
         END                                                                    
