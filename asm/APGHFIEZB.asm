*          DATA SET APGHFIEZB  AT LEVEL 085 AS OF 06/28/04                      
*PHASE ACHFEZBA                                                                 
         TITLE 'APG HOOK FOR DOREMUS PROFIT AND LOSS REPORT'                    
*--------------------------------------------------------------------*          
*                                                                               
*--------------------------------------------------------------------*          
ACHFEZB  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         ST    R5,HKRELO                                                        
         USING SRECD,R5                                                         
         L     R5,HOOKAREC                                                      
         L     R6,VBIGPRNT                                                      
         USING BIGPRNTD,R6                                                      
         CLI   HOOKNUM,3           HOOKS BEFORE SORT                            
         BE    PUTSORT                                                          
         CLI   HOOKNUM,4           HOOKS AFTER SORT                             
         BE    BLDOFTAB                                                         
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        COLLAPSE ALL THREE REPORTS INTO ONE                          *         
*---------------------------------------------------------------------*         
PUTSORT  EQU   *                                                                
         CLI   QOPT7,C'1'                                                       
         BNE   PUTSRT03                                                         
         L     R7,HOOKAREC                                                      
         LA    R0,SRLNQL                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'DUMP3B'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
         USING SRECD,R5                                                         
PUTSRT03 L     R5,HOOKAREC                                                      
         CLI   SRHDRC,X'04'                                                     
         BH    PUTSRT04                                                         
         MVC   SRHDRC,=X'0401'                                                  
         MVC   SRROW1,=X'0401'                                                  
         MVC   SRROW2,=X'0401'                                                  
         MVC   SRROW3,=X'0401'                                                  
         MVC   SRROW4,=X'0401'                                                  
         MVC   SRROW5,=X'0401'                                                  
         B     PUTSRT08                                                         
                                                                                
PUTSRT04 MVC   SRHDRC,=X'0801'                                                  
         MVC   SRROW1,=X'0801'                                                  
         MVC   SRROW2,=X'0801'                                                  
         MVC   SRROW3,=X'0801'                                                  
         MVC   SRROW4,=X'0801'                                                  
         MVC   SRROW5,=X'0801'                                                  
                                                                                
PUTSRT08 EQU   *                                                                
         CLI   QOPT7,C'1'                                                       
         BNE   PUTSRT15                                                         
         L     R7,HOOKAREC                                                      
         LA    R0,SRLNQL                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'DUMP3A'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
*---------------------------------------------------------------------*         
*        ELIMINATE RECORDS BASED ON OFFICE GROUPING                   *         
*---------------------------------------------------------------------*         
PUTSRT15 CLC   QSELECT(2),=CL2' '                                               
         BE    PUTSRT70                                                         
                                                                                
         CLI   QOPT7,C'2'                                                       
         BNE   PUTSRT20                                                         
         L     R7,HOOKAREC                                                      
         LA    R0,SRLNQL                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'DUMP3B'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
PUTSRT20 LA    R3,GRPNTR                                                        
PUTSRT25 CLI   0(R3),X'FF'                                                      
         BE    XIT                                                              
         CLC   QSELECT(2),0(R3)                                                 
         BE    PUTSRT30                                                         
         LA    R3,8(R3)            BUMP UP TO NEXT ENTRY                        
         B     PUTSRT25                                                         
                                                                                
PUTSRT30 L     R2,4(R3)            LOAD ADDRESS POINTING TO OFFICE LIST         
         A     R2,HKRELO                                                        
         LA    R2,36(R2)           BUMP PAST NAME                               
                                                                                
PUTSRT35 CLI   0(R2),0             END OF LIST?                                 
         BE    XIT_NO                                                           
                                                                                
         CLC   QUNIT(2),=C'2D'                                                  
         BE    PUTSRT40                                                         
         CLC   QUNIT(2),=C'1C'                                                  
         BNE   PUTSRT45                                                         
PUTSRT40 CLC   ACTACC+3(2),0(R2)                                                
         BE    PUTSRT70            FOUND SO OK                                  
         B     PUTSRT60                                                         
                                                                                
PUTSRT45 EQU   *                                                                
         CLC   QUNIT(2),=C'SE'                                                  
         BNE   PUTSRT50                                                         
         CLC   CURROFC(2),0(R2)                                                 
         BE    PUTSRT70            FOUND SO OK                                  
         B     PUTSRT60                                                         
                                                                                
PUTSRT50 EQU   *                                                                
         CLC   QUNIT(2),=C'12'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CURROFC(2),0(R2)                                                 
         BE    PUTSRT70            FOUND SO OK                                  
                                                                                
PUTSRT60 LA    R2,3(R2)            BUMP PAST COMMA TO NEXT CHAR                 
         B     PUTSRT35                                                         
                                                                                
PUTSRT70 EQU   *                                                                
         CLC   SRROW1,=X'0801'                                                  
         BNE   XIT                                                              
         MVC   SRACC1,SPACES                                                    
         B     XIT                 FOUND SO OK                                  
                                                                                
PUTSRT99 CLI   QOPT7,C'2'                                                       
         BNE   XIT                                                              
         L     R7,HOOKAREC                                                      
         LA    R0,SRLNQL                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'DUMP3A'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
XIT      SR    RE,RE                                                            
XIT_NO   LTR   RE,RE                                                            
         XMOD1 1                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        BUILD TABLE OF OFFICE CODES AND OFFICE NAMES                 *         
*---------------------------------------------------------------------*         
BLDOFTAB EQU   *                                                                
         CLI   ONEXONLY,NO                                                      
         BNE   GETOFFNM                                                         
         MVI   ONEXONLY,YES                                                     
         L     R3,=A(OFFTAB)       ADDRESS OF OFFICE TABLE                      
         A     R3,HKRELO                                                        
         ST    R3,OFFPT                                                         
         XC    0(2,R3),0(R3)                                                    
         MVC   2(36,R3),SPACES                                                  
         MVI   NOFF,0                                                           
                                                                                
         USING OFFRECD,R2                                                       
         L     R2,=A(IO1)                                                       
         A     R2,HKRELO                                                        
         ST    R2,AIOWRK                                                        
         MVC   COMMAND,DMRDHI                                                   
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,RCCOMPFL                                                 
BLDOF12  EQU   *                                                                
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',AIOWRK,AIOWRK,0                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   COMMAND,DMRSEQ                                                   
         L     R2,AIOWRK                                                        
         CLC   OFFKCPY,RCCOMPFL                                                 
         BH    GETOFFNM                                                         
         TM    OFFKSTAT,OFFSLIST                                                
         BO    BLDOF12                                                          
                                                                                
         SR    R1,R1                                                            
         IC    R1,NOFF             BUMP UP COUNT                                
         AH    R1,=H'01'                                                        
         STC   R1,NOFF                                                          
         CLI   NOFF,MAXOFF         END OF TABLE?                                
         BNH   *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   0(2,R3),OFFKOFF                                                  
         MVC   2(36,R3),SPACES                                                  
         AH    R2,DATADISP                                                      
                                                                                
BLDOF15  CLI   0(R2),0                                                          
         BE    BLDOF90                                                          
         CLI   0(R2),X'20'                                                      
         BE    BLDOF20                                                          
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         B     BLDOF15                                                          
                                                                                
BLDOF20  SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         SH    R1,=H'03'                                                        
         EXMVC R1,2(R3),2(R2)                                                   
BLDOF90  LA    R3,38(R3)           BUMP UP IN TABLE                             
         L     R2,AIOWRK                                                        
         B     BLDOF12                                                          
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        FIND THE OFFICE NAME FOR THIS OFFICE CODE                    *         
*---------------------------------------------------------------------*         
GETOFFNM EQU   *                                                                
         CLI   QOPT7,C'4'                                                       
         BNE   GETOFF02                                                         
         L     R7,HOOKAREC                                                      
         LA    R0,SRLNQL                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'DUMP4B'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
GETOFF02 EQU   *                                                                
         USING SRECD,R5                                                         
         L     R5,HOOKAREC                                                      
                                                                                
         CLC   0(2,R5),=X'0801'                                                 
         BNE   GETOFF03                                                         
         SH    R5,=H'18'                                                        
         MVC   SRNAM1,SPACES                                                    
         B     GETOFF20                                                         
                                                                                
GETOFF03 SH    R5,=H'18'                                                        
         SR    R1,R1                                                            
         ICM   R1,1,NOFF                                                        
         BZ    GETOFF20                                                         
         ICM   R3,15,OFFPT                                                      
         BZ    GETOFF08                                                         
         CLC   SRACC1,0(R3)                                                     
         BE    GETOFF10                                                         
         L     R3,=A(OFFTAB)                                                    
         A     R3,HKRELO                                                        
GETOFF05 CLC   SRACC1,0(R3)                                                     
         BE    GETOFF10                                                         
         LA    R3,38(R3)           BUMP UP IN TABLE                             
         BCT   R1,GETOFF05                                                      
GETOFF08 MVC   SRNAM1,=CL36'UNKNOWN OFFICE'                                     
         B     GETOFF20                                                         
                                                                                
GETOFF10 MVC   SRNAM1,2(R3)        RESTORE 1C OFFICE NAME                       
         ST    R3,OFFPT                                                         
                                                                                
GETOFF20 EQU   *                                                                
         LA    R3,QENDTAB                                                       
GETOFF25 CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   QEND+2(2),0(R3)                                                  
         BE    GETOFF30                                                         
         LA    R3,4(R3)                                                         
         B     GETOFF25                                                         
                                                                                
GETOFF30 EQU   *                                                                
         XC    DIVWORK,DIVWORK                                                  
         ZAP   HALF,2(2,R3)                                                     
         ZAP   DIVWORK,SRAMT15                                                  
         DP    DIVWORK,HALF                                                     
         ZAP   SRAMT15,DIVANS                                                   
         ZAP   DIVWORK,SRAMT16                                                  
         DP    DIVWORK,HALF                                                     
         ZAP   SRAMT16,DIVANS                                                   
                                                                                
         CLI   QOPT7,C'4'                                                       
         BNE   GETOFF99                                                         
         L     R7,HOOKAREC                                                      
         LA    R0,SRLNQL                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'DUMP4A'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
GETOFF99 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------*            
*        LITERAL POOL                                              *            
*------------------------------------------------------------------*            
         LTORG                                                                  
         EJECT                                                                  
*------------------------------------------------------------------*            
*        CONSTANT POOL                                             *            
*------------------------------------------------------------------*            
ONEXONLY DC    AL1(NO)                                                          
NOFF     DS    XL1                                                              
MAXOFF   EQU   99                  MAX NUMBER OF OFFICES IN TABLE               
COMMAND  DS    CL6                                                              
HKRELO   DS    A                                                                
OFFPT    DS    A                                                                
AIOWRK   DS    A                                                                
                                                                                
MAXDUMP  DS    PL4'100'                                                         
BDUMP    DS    PL4'0'                                                           
ADUMP    DS    PL4'0'                                                           
                                                                                
         DS    0D                                                               
DIVWORK  DS    0PL10                                                            
DIVANS   DS    PL8                                                              
DIVREM   DS    PL2                                                              
                                                                                
GRPNTR   DS    0A                  GROUP CODE POINTERS                          
         DC    C'CS',A(GRPCS)                                                   
         DC    C'DC',A(GRPDC)                                                   
         DC    C'DD',A(GRPDD)                                                   
         DC    C'DO',A(GRPDO)                                                   
         DC    C'DR',A(GRPDR)                                                   
         DC    C'FA',A(GRPFA)                                                   
         DC    C'GE',A(GRPGE)                                                   
         DC    C'IN',A(GRPIN)                                                   
         DC    C'SG',A(GRPSG)                                                   
         DC    C'NY',A(GRPNY)                                                   
         DC    X'FF'                                                            
                                                                                
*        GROUP NAMES / OFFICE LIST                                              
GRPCS    DC    CL36'CORPORATE SVC./OH'                                          
         DC    C'NA',X'0000'                                                    
                                                                                
GRPDC    DC    CL36'DOMESTIC, NO FINANCIAL PRINTING'                            
         DC    C'NA,NJ,NN,NT,NU,N1,N9,SF',X'0000'                               
                                                                                
GRPDD    DC    CL36'DOMESTIC OFFICES'                                           
         DC    C'NA,NJ,NN,NT,NU,N1,N9,FP,SF',X'0000'                            
                                                                                
GRPDO    DC    CL36'REGIONAL OFFICES'                                           
         DC    C'NJ,SF',X'0000'                                                 
                                                                                
GRPDR    DC    CL36'DOMESTIC WITHOUT SAN FRAN'                                  
         DC    C'NA,NJ,NN,NT,NU,N1,N9,FP',X'0000'                               
                                                                                
GRPGE    DC    CL36'DOREMUS GENERAL'                                            
         DC    C'NN,NT,N9',X'0000'                                              
                                                                                
GRPFA    DC    CL36'FINANCIAL OFFICES'                                          
         DC    C'FP,NU,N1',X'0000'                                              
                                                                                
GRPIN    DC    CL36'INTERNATIONAL OFFICES'                                      
         DC    C'IX,I7,I8',X'0000'                                              
                                                                                
GRPSG    DC    CL36'SHERMANS GROUP'                                             
         DC    C'NN,N9',X'0000'                                                 
                                                                                
GRPNY    DC    CL36'NY + DPRIME'                                                
         DC    C'PR,NA,NJ,NN,NT,NU,N1,N9',X'0000'                               
                                                                                
*------------------------------------------------------------------*            
*        TABLE OF DIVISION FACTORS BASED ON QEND                   *            
*------------------------------------------------------------------*            
QENDTAB  DC    0C                                                               
         DC    CL2'01',PL2'11'                                                  
         DC    CL2'02',PL2'10'                                                  
         DC    CL2'03',PL2'09'                                                  
         DC    CL2'04',PL2'08'                                                  
         DC    CL2'05',PL2'07'                                                  
         DC    CL2'06',PL2'06'                                                  
         DC    CL2'07',PL2'05'                                                  
         DC    CL2'08',PL2'04'                                                  
         DC    CL2'09',PL2'03'                                                  
         DC    CL2'10',PL2'02'                                                  
         DC    CL2'11',PL2'01'                                                  
         DC    CL2'12',PL2'01'                                                  
         DC    X'FF'                                                            
*------------------------------------------------------------------*            
*        OFFICE CODE (CL2)                                         *            
*        OFFICE NAME (CL36)                                        *            
*------------------------------------------------------------------*            
OFFTAB   DC    (MAXOFF)XL38'00'                                                 
                                                                                
         DS    0F                                                               
         DC    CL8'**AIO1**'                                                    
IO1      DS    CL2001                                                           
         EJECT                                                                  
*------------------------------------------------------------------*            
*        SORT RECORD LAYOUT                                        *            
*------------------------------------------------------------------*            
SRECD    DSECT                                                                  
SRHEAD   DS    0C                                                               
SRHDRC   DS    CL2                                                              
         DS    CL16                                                             
SRREC    DS    0C                                                               
SRROW1   DS    CL2                 REPORT NUMBER/COPY                           
SRACC1   DS    CL2                 ROW 1 OFFICE CODE                            
         DS    CL12                                                             
SRROW2   DS    CL2                 REPORT NUMBER/COPY                           
SRACC2   DS    CL1                 ACCOUNT LEVEL 1 (SUPERLEDGER)                
         DS    CL13                                                             
SRROW3   DS    CL2                 REPORT NUMBER/COPY                           
SRACC3   DS    CL1                 ACCOUNT LEVEL 2 (SUPERLEDGER)                
         DS    CL13                                                             
SRROW4   DS    CL2                 REPORT NUMBER/COPY                           
SRACC4   DS    CL1                 ACCOUNT LEVEL 3 (SUPERLEDGER)                
         DS    CL13                                                             
SRROW5   DS    CL2                 REPORT NUMBER/COPY                           
SRACC5   DS    CL1                 ACCOUNT LEVEL 3 (SUPERLEDGER)                
         DS    CL13                                                             
         DS    CL2                 REPORT NUMBER/COPY                           
SRBINZ   DS    XL2                 BINARY ZERO                                  
SRNAM1   DS    CL36                OFFICE NAME                                  
SRNAM2   DS    CL36                ROW 2 NAME                                   
SRNAM3   DS    CL36                ROW 3 NAME                                   
SRNAM4   DS    CL36                ROW 4 NAME                                   
SRNAM5   DS    CL36                ROW 5 NAME                                   
SRAMT1   DS    PL8                 BUCKETS                                      
SRAMT2   DS    PL8                 BUCKETS                                      
SRAMT3   DS    PL8                 BUCKETS                                      
SRAMT4   DS    PL8                 BUCKETS                                      
SRAMT5   DS    PL8                 BUCKETS                                      
SRAMT6   DS    PL8                 BUCKETS                                      
SRAMT7   DS    PL8                 BUCKETS                                      
SRAMT8   DS    PL8                 BUCKETS                                      
SRAMT9   DS    PL8                 BUCKETS                                      
SRAMT10  DS    PL8                 BUCKETS                                      
SRAMT11  DS    PL8                 BUCKETS                                      
SRAMT12  DS    PL8                 BUCKETS                                      
SRAMT13  DS    PL8                 BUCKETS                                      
SRAMT14  DS    PL8                 BUCKETS                                      
SRAMT15  DS    PL8                 BUCKETS                                      
SRAMT16  DS    PL8                 BUCKETS                                      
SRLNQ    EQU   *-SRREC                                                          
SRLNQL   EQU   *-SRHEAD                                                         
                                                                                
*------------------------------------------------------------------*            
*        INCLUDED DSECTS                                           *            
*------------------------------------------------------------------*            
                                                                                
*        ACAPGGEND                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT   ON                                                             
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT   ON                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085APGHFIEZB 06/28/04'                                      
         END                                                                    
