*          DATA SET ACFILCOMP  AT LEVEL 035 AS OF 05/01/02                      
*PHASE ACCCOMPA                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'DATA SET COMPARE'                                               
*---------------------------------------------------------------------*         
*        DO NOT DELETE/CHANGE THIS PROGRAM!                           *         
*        RUN DATE -->                                                 *         
*---------------------------------------------------------------------*         
ACCCOMP  CSECT                                                                  
BGN      DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**CCC**,R9,R8                                                  
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
                                                                                
MRG025   L     R2,AMYSRT1          GET THE FIRST RECORD FROM FIRST              
         L     R4,ASRT1            DATASET                                      
         GET   (R2),(R4)                                                        
                                                                                
         L     R2,AMYSRT2          GET THE FIRST RECORD FROM SECOND             
         L     R4,ASRT2            DATASET                                      
         GET   (R2),(R4)                                                        
                                                                                
MRG050   DS    0H                                                               
         L     R2,ASRT1                                                         
         CLI   4(R2),X'0B'         IF IT IS HIGHER THAN THIS I HAVE             
         BH    MENDXIT             PROCESSED ALL THE RECORDS I NEED             
         CLI   4(R2),X'03'         PROCESS NEW BATCH OR                         
         BE    MRG060                                                           
         CLI   4(R2),X'0B'         OLD BATCH                                    
         BNE   MRG025                                                           
MRG060   L     R3,ASRT2                                                         
         CLC   4(42,R2),4(R3)      DO FIRST COMPARE                             
         BE    MRG100              IF EQUAL MUST DO SECOND COMPARE              
         BL    MRG150              IF LOW PUT OUT RECORD ONE                    
         BH    MRG160              IF HIGH PUT OUT RECORD TWO                   
                                                                                
MRG100   BAS   RE,ELCOMP                                                        
         L     R2,AMYSRT1          AND PUT OUT THE REST OF THE RECORDS          
         L     R4,ASRT1            FROM DATASET ONE                             
         GET   (R2),(R4)                                                        
         L     R2,AMYSRT2          AND PUT OUT THE REST OF THE RECORDS          
         L     R4,ASRT2            ON DATASET TWO                               
         GET   (R2),(R4)                                                        
         B     MRG050              IF HIGH PUT OUT RECORD TWO                   
                                                                                
MRG150   DS    0H                                                               
         L     R2,ASRT1            DATASET                                      
         MVC   P+2(14),=C'EXTRA TAPE ONE'                                       
         BAS   RE,PRTMISS          AND PUT THIS RECORD TO TAPE                  
         L     R2,AMYSRT1          AND PUT OUT THE REST OF THE RECORDS          
         L     R4,ASRT1            FROM DATASET ONE                             
         GET   (R2),(R4)                                                        
         B     MRG050              GO THROUGH COMPARE LOGIC                     
                                                                                
MRG160   L     R2,ASRT2            DATASET                                      
         MVC   P+2(14),=C'EXTRA TAPE TWO'                                       
         BAS   RE,PRTMISS          AND PUT THIS RECORD TO TAPE                  
         L     R2,AMYSRT2          AND PUT OUT THE REST OF THE RECORDS          
         L     R4,ASRT2            ON DATASET TWO                               
         GET   (R2),(R4)                                                        
         B     MRG050              GO THROUGH COMPARE LOGIC                     
                                                                                
MENDSR1  DS    0H                  IF EOF FOR DATASET ONE                       
         TM    ENDED,YES           DID I ALREADY COMPLETE DATASET TWO           
         BO    MENDXIT             IF YES EXIT                                  
         L     R2,ASRT2            ELSE PUT OUT RECORD IN BUFFER FROM           
         MVC   P+2(14),=C'EXTRA TAPE TWO'                                       
         BAS   RE,PRTMISS                                                       
         OI    ENDED,YES           SET SWITCH FOR EOF ON A DATASET              
MEND1A   DS    0H                                                               
         L     R2,AMYSRT2          AND PUT OUT THE REST OF THE RECORDS          
         L     R4,ASRT2            ON DATASET TWO                               
         GET   (R2),(R4)                                                        
         L     R2,ASRT2            ELSE PUT OUT RECORD IN BUFFER FROM           
         MVC   P+2(14),=C'EXTRA TAPE TWO'                                       
         BAS   RE,PRTMISS                                                       
         B     MEND1A                                                           
                                                                                
MENDSR2  DS    0H                  IF EOF FOR DATASET TWO                       
         TM    ENDED,YES           DID I ALREADY COMPLETE DATASET ONE           
         BO    MENDXIT             IF YES EXIT                                  
         L     R2,ASRT1            ELSE PUT OUT RECORD IN BUFFER FROM           
         MVC   P+2(14),=C'EXTRA TAPE ONE'                                       
         BAS   RE,PRTMISS                                                       
         OI    ENDED,YES           SET SWITCH FOR EOF ON A DATASET              
MEND2A   DS    0H                                                               
         L     R2,AMYSRT1          AND PUT OUT THE REST OF THE RECORDS          
         L     R4,ASRT1            ON DATASET TWO                               
         GET   (R2),(R4)                                                        
         L     R2,ASRT1            ELSE PUT OUT RECORD IN BUFFER FROM           
         MVC   P+2(14),=C'EXTRA TAPE ONE'                                       
         BAS   RE,PRTMISS                                                       
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
                                                                                
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         EDIT  (P4,MISCOUNT),(6,P+20),0                                         
         MVC   P+2(14),=C'RECORD COUNT ='                                       
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ROUTINE TO COMPARE ELEMENTS ON RECORDS OF EQUAL KEY          *         
*---------------------------------------------------------------------*         
ELCOMP   NTR1                                                                   
         L     R3,ASRT1                                                         
         LA    R3,4(R3)                                                         
         L     R4,ASRT2                                                         
         LA    R4,4(R4)                                                         
         CLC   ACTRLEN-ACTRECD(2,R3),ACTRLEN-ACTRECD(R4)                        
         BNE   ELC10                                                            
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         LA    R4,ACTRFST-ACTRECD(R4)                                           
ELC05    CLI   0(R3),0                                                          
         BE    EXIT                                                             
         ZIC   R5,1(R3)                                                         
         ZIC   R6,1(R4)                                                         
         CR    R5,R6                                                            
         BNE   ELC10                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(R4)                                                    
         BNE   ELC10                                                            
         ZIC   R5,1(R3)                                                         
         AR    R3,R5                                                            
         ZIC   R6,1(R4)                                                         
         AR    R4,R6                                                            
         B     ELC05                                                            
                                                                                
ELC10    MVC   P+2(9),=C'DIFFERENT'                                             
         L     R2,ASRT1                                                         
         LA    R2,4(R2)                                                         
         GOTO1 HEXOUT,DMCB,(R2),P+20,42,=C'TOG'                                 
         MVC   P+2(9),=C'DIFFERENT'                                             
         L     R2,ASRT2                                                         
         LA    R2,4(R2)                                                         
         GOTO1 HEXOUT,DMCB,(R2),P+20,42,=C'TOG'                                 
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ROUTINE TO PRINT KEYS OF MISSING OR EXTRA RECORDS            *         
*---------------------------------------------------------------------*         
PRTMISS  NTR1                                                                   
         ST    R2,ADDPUT              FOR DMPPUT                                
         LA    R2,4(R2)                                                         
         GOTO1 HEXOUT,DMCB,(R2),P+20,42,=C'TOG'                                 
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
         BAS   RE,DMPPUT                                                        
         GOTO1 VPRINTER                                                         
         AP    MISCOUNT,=P'1'                                                   
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
DMPPUT   NTR1                                                                   
         L     R5,ADDPUT                                                        
         SR    RF,RF                                                            
         ICM   RF,3,0(R5)                                                       
         GOTO1 PRNTBL,DMCB,(0,0),(R5),C'DUMP',(RF),=C'2D'                       
                                                                                
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
VPRINTER DC    V(PRINTER)          VPRINTER                                     
         DC    X'FF'                                                            
                                                                                
ADDPUT   DC    A(0)                                                             
                                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
         DS    CL8                                                              
WORK     DS    CL20                                                             
         DS    CL8                                                              
BYTE     DS    CL1                                                              
                                                                                
MISCOUNT DC    PL4'0'                                                           
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
                                                                                
* ACGENFILE                                                                     
* DDDPRINT                                                                      
* DMWRKRD                                                                       
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035ACFILCOMP 05/01/02'                                      
         END                                                                    
