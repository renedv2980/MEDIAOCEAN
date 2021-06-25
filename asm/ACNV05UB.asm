*          DATA SET ACNV05UB   AT LEVEL 016 AS OF 09/19/00                      
*PHASE ACNV5UBA ACNV05UB                                                        
         TITLE 'BDNWW CONVERSION HOOK'                                          
                                                                                
ACNV05UB CSECT                                                                  
         PRINT NOGEN                                                            
         USING ACNVD,R9                                                         
         NMOD1 0,*HOOK*,RA                                                      
         EJECT                                                                  
*                                                                               
CNVIN    CLI   MODE,INITTAB        INITIALIZE TABLE                             
         BNE   CNVBFR                                                           
         L     R1,=A(ACTABL)                                                    
         ST    R1,AACTABL                                                       
         BAS   RE,DYNTAB           BUILD DYNAMIC TABLE                          
         BAS   RE,BLDTAB           BUILD ACCOUNT TABLE                          
         B     XIT                                                              
*                                                                               
CNVBFR   CLI   MODE,PROCBFR        BEFORE CONVERSION                            
         BNE   CNVAFT                                                           
         MVC   NEW1RLVS,=X'0204060C'  NEW LEDGER STRUCTURE FOR 1R               
         B     XIT                                                              
*                                                                               
CNVAFT   CLI   MODE,PROCAFT        AFTER CONVERSION                             
         BNE   XIT                                                              
         BAS   RE,LDGUP            UPDATE LEDGER RECORD                         
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                   RETURN TO CONVERSION TO COMPLETE             
         EJECT                                                                  
***********************************************************************         
* PROCESS LEDGER RECORD                                               *         
***********************************************************************         
*                                                                               
         USING LDGRECD,R3                                                       
LDGUP    CLI   RECTYP,ACRTLDG                                                   
         BNER  RE                                                               
LDGUP1   NTR1  ,                                                                
         LA    R4,LDGTAB                                                        
         L     R3,AOUT                                                          
         USING LDGRECD,R3                                                       
LDGUP3   CLC   LDGKUNT(2),0(R4)                                                 
         BE    LDGUP5                                                           
         LA    R4,L'LDGTAB(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   LDGUP3                                                           
         B     XIT                                                              
*                                                                               
LDGUP5   SR    R1,R1                                                            
         LA    R3,LDGRFST          GET TO FIRST ELEMENT                         
LDGUP7   CLI   0(R3),0                                                          
         BE    XIT                                                              
         CLI   0(R3),ACLELQ        ACCOUNT LENGTHS ELEMENT                      
         BE    LDGUP11                                                          
LDGUP9   IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     LDGUP7                                                           
*                                                                               
         USING ACLELD,R3                                                        
LDGUP11  ICM   RF,15,4(R4)         RF=A(NEW LENGTHS ELEMENT)                    
         BZ    LDGUP9                                                           
         MVC   ACLVALS(L'ACLVALS*4),0(RF)                                       
         B     LDGUP9                                                           
         DROP  R3                                                               
*                                                                               
         DS    0F                                                               
LDGTAB   DS    0XL8                                                             
         DC    C'1R',AL1(0),AL1(0),AL4(LVL1R)                                   
         DC    C'16',AL1(0),AL1(0),AL4(LVL16)                                   
         DC    X'FF'                                                            
*                                                                               
LVL1R    DC    AL1(02),CL15'OFFICE'                                             
         DC    AL1(04),CL15'DEPARTMENT'                                         
         DC    AL1(06),CL15'TITLE'                                              
         DC    AL1(12),CL15'STAFF'                                              
*                                                                               
LVL16    DC    AL1(02),CL15'OFFICE'                                             
         DC    AL1(04),CL15'DEPARTMENT'                                         
         DC    AL1(06),CL15'TITLE'                                              
         DC    AL1(12),CL15'STAFF'                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD DYNAMIC TABLE                                                 *         
***********************************************************************         
                                                                                
DYNTAB   NTR1  ,                                                                
         XC    ACCNUM,ACCNUM          CLEAR NUMBER IN TABLE                     
         L     R2,AACTABL                                                       
         USING TABD,R2                                                          
         LA    R3,DKEY                                                          
         USING ACTRECD,R3                                                       
         MVC   DKEY,SPACE             READ ACCOUNTS IN 1R                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKULA(2),=C'1R'                                                
*                                                                               
DYN3     GOTO1 ADMGR,DMHI                                                       
         CLC   DIR(3),DKEY                                                      
         BNE   DYN8                                                             
         LA    R3,DIR                                                           
*                                                                               
         CLC   ACTKACT+4(3),SPACE     MAKE SURE AT LEAST SUBDEP LEVEL           
         BE    DYN7                   IF NOT GET NEXT                           
*                                                                               
         MVC   TABOLD,ACTKULA         COPY OLD ACCOUNT                          
         MVC   TABNEW,SPACE           COPY NEW ACCOUNT                          
         MVC   TABNULOD,TABOULOD      UNIT/LEDGER/OFFICE/DEPT                   
         MVC   TABNPER(L'TABOPER),TABOPER  PERSON                               
         MVC   TABNSUB(1),TABOSUB        FIRST CHAR OF SUBDEPT                  
         MVC   TABNSUB+1(1),TABOSUB+2    SECOND CHAR OF SUBDEPT                 
*                                                                               
         CLC   TABOSUB,=C'DNB'        DNB IS THE EXCEPTION                      
         BNE   DYN5                                                             
         MVC   TABNSUB,TABOSUB        DNB CHANGED TO DN                         
*                                                                               
DYN5     LA    R2,TABLNQ(R2)                                                    
         MVI   0(R2),X'FF'                                                      
         L     RF,ACCNUM              INCREMENT # OF ACCOUNTS                   
         LA    RF,1(RF)               STORED IN TABLE                           
         ST    RF,ACCNUM                                                        
*                                                                               
DYN7     MVC   DKEY,SPACE             SKIP TO NEXT ACCOUNT                      
         MVC   DKEY(L'ACTKCULA),DIR                                             
         LA    R3,DKEY                                                          
         LA    R1,ACTKACT+(L'ACTKACT-1)                                         
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(R1)                                                         
         B     DYN3                                                             
*                                                                               
DYN8     MVC   TABOLD,=CL14'1602OOOTH99999'                                     
         MVC   TABNEW,=CL14'1602OOOH99999 '                                     
*                                                                               
         LA    R2,TABLNQ(R2)                                                    
         L     RF,ACCNUM              INCREMENT # OF ACCOUNTS                   
         LA    RF,1(RF)               STORED IN TABLE                           
         ST    RF,ACCNUM                                                        
*                                                                               
         MVC   TABOLD,=CL14'1699000001    '                                     
         MVC   TABNEW,=CL14'169900001     '                                     
*                                                                               
         LA    R2,TABLNQ(R2)                                                    
         L     RF,ACCNUM              INCREMENT # OF ACCOUNTS                   
         LA    RF,1(RF)               STORED IN TABLE                           
         ST    RF,ACCNUM                                                        
*                                                                               
         MVC   TABOLD,=CL14'169900000     '                                     
         MVC   TABNEW,=CL14'16990000      '                                     
*                                                                               
         LA    R2,TABLNQ(R2)                                                    
         MVI   0(R2),X'FF'                                                      
         L     RF,ACCNUM              INCREMENT # OF ACCOUNTS                   
         LA    RF,1(RF)               STORED IN TABLE                           
         ST    RF,ACCNUM                                                        
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* FORMAT THE ACCOUNT TABLE                                            *         
***********************************************************************         
                                                                                
BLDTAB   NTR1  ,                                                                
         L     RF,ACCNUM           SET NUMBER IN TABLE                          
         CH    RF,=Y(MXACC)        TEST MAX                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         LA    R1,ACXTAB                                                        
         ST    R1,ACCTAB           SET A(ACCOUNT CONVERSION)                    
         L     R2,AACTABL                                                       
*                                                                               
BLDTAB3  MVC   ACCO-ACCS(L'ACCO,R1),0(R2)  OLD ACCOUNT                          
         MVC   ACCN-ACCS(L'ACCN,R1),14(R2) NEW ACCOUNT                          
         MVC   ACCNME-ACCS(L'ACCNME,R1),SPACE                                   
         ZAP   ACCH-ACCS(L'ACCH,R1),=P'0'                                       
         LA    R2,L'ACTABL(R2)                                                  
         LA    R1,ACCLNQ(R1)                                                    
         BCT   RF,BLDTAB3                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND WORK AREA                                             *         
***********************************************************************         
                                                                                
AACTABL  DS    A                   ADDRESS OF ACCOUNT CONVERSION TABLE          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
MXACC    EQU   8000                MAXIMUM NUMBER OF ACCOUNT                    
ACXTAB   DS    (MXACC)CL(ACCLNQ)      EXPANDED TABLE FOR CONVERSION             
         EJECT                                                                  
***********************************************************************         
* ACCOUNT CONVERSION TABLE                                            *         
***********************************************************************         
                                                                                
ACTABL   DS    0CL28   OLD/NEW                                                  
         DS    (MXACC)CL(L'ACTABL)                                              
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
                                                                                
TABD     DSECT                                                                  
TABOLD   DS    0CL14               OLD ACCOUNT                                  
TABOULOD DS    CL6                 UNIT/LEDGER                                  
TABOSUB  DS    CL3                 SUBDEPARTMENT                                
TABOPER  DS    CL5                 PERSON                                       
*                                                                               
TABNEW   DS    0CL14               NEW ACCOUNT                                  
TABNULOD DS    CL6                 UNIT / LEDGER / OFFICE / DEPARTMENT          
TABNSUB  DS    CL2                 SUBDEPT                                      
TABNPER  DS    CL6                 PERSON                                       
TABLNQ   EQU   *-TABD                                                           
*                                                                               
ACNVD    DSECT                                                                  
* ACNVWORK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACNVWORK                                                       
         PRINT ON                                                               
* ACNVDSECT                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACNVDSECT                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACNV05UB  09/19/00'                                      
         END                                                                    
