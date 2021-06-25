*          DATA SET ACNV05BD   AT LEVEL 115 AS OF 09/19/00                      
*PHASE ACNV5BDA ACNV05BD                                                        
         TITLE 'BDNWW CONVERSION HOOK'                                          
                                                                                
ACNV05BD CSECT                                                                  
         PRINT NOGEN                                                            
         USING ACNVD,R9                                                         
         NMOD1 0,*HOOK*,RA                                                      
         EJECT                                                                  
***********************************************************************         
* CONVERSION INITIALIZATION ROUTINE                                   *         
***********************************************************************         
                                                                                
CNVIN    CLI   MODE,INITTAB                                                     
         BNE   XIT                                                              
*        BNE   HOOK                                                             
         L     R1,=A(ACTABL)                                                    
         ST    R1,AACTABL                                                       
         MVC   NEW1RLVS,=X'0204060C'  NEW LEDGER STRUCTURE FOR 1R               
*        BAS   RE,NAMSRT           FIND DUPLICATE NAMES                         
*        BAS   RE,NAMOUT           CREATE FILE OF DUPLICATES                    
         BAS   RE,DYNTAB           BUILD DYNAMIC TABLE                          
         BAS   RE,BLDTAB           BUILD ACCOUNT TABLE                          
*                                                                               
XIT      XIT1  ,                   RETURN TO CONVERSION TO COMPLETE             
         EJECT                                                                  
***********************************************************************         
* BUILD NAME SORT TABLE                                               *         
***********************************************************************         
                                                                                
NAMSRT   NTR1  ,                                                                
         XC    SRTNUM,SRTNUM       CLEAR NUMBER IN TABLE                        
         L     R3,=A(SRTTAB)                                                    
         USING SRTD,R3                                                          
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         MVC   DKEY,SPACE          READ ACCOUNTS IN 1R                          
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKULA(2),=C'1R'                                                
*                                                                               
NAMSRT3  GOTO1 ADMGR,DMHI                                                       
         CLC   DIR(3),DKEY                                                      
         BNE   NAMSRT15                                                         
         LA    R2,DIR                                                           
         TM    ACTKSTAT,ACTSABLP      TEST BALANCE ELEMENT                      
         BNO   NAMSRT13                                                         
         L     R2,AIO3                                                          
         GOTO1 ADMGR,DMGET                                                      
         LA    R4,ACTRFST                                                       
         SR    R0,R0                                                            
NAMSRT5  CLI   0(R4),NAMELQ        GET THE NAME ELEMENT                         
         BE    NAMSRT7                                                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    NAMSRT13                                                         
         B     NAMSRT5                                                          
*                                                                               
         USING NAMELD,R4                                                        
NAMSRT7  MVC   SRTREC,SPACE                                                     
         SR    R0,R0                                                            
         IC    R0,NAMLN                                                         
         SH    R0,=H'2'                                                         
         LA    R1,SRTLAST                                                       
         LA    RF,NAMEREC                                                       
NAMSRT9  CLI   0(RF),C','          STOP AT ','                                  
         BE    NAMSRT11                                                         
         CLI   0(RF),C' '             OR   ' '                                  
         BE    NAMSRT11                                                         
         MVC   0(1,R1),0(RF)       NAME TO SORT RECORD                          
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,NAMSRT9                                                       
*                                                                               
NAMSRT11 MVC   SRTOLD,ACTKULA      OLD ACCOUNT                                  
         SR    R1,R1                                                            
         IC    R1,NAMLN            SAVE FULL NAME                               
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   SRTNAME(0),NAMEREC                                               
         MVC   SRTPER,ACTKULA      SAVE PERSON CODE                             
         MVC   SRTFLT1,ACTRSAF1    SAVE FILTER 1                                
*                                                                               
         LA    R3,SRTLNQ(R3)                                                    
         MVI   0(R3),X'FF'                                                      
         L     RF,SRTNUM              INCREMENT # OF ACCOUNTS                   
         LA    RF,1(RF)               STORED IN TABLE                           
         ST    RF,SRTNUM                                                        
         CH    RF,=Y(MXSRT)                                                     
         BNH   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
NAMSRT13 MVC   DKEY,SPACE             SKIP TO NEXT ACCOUNT                      
         MVC   DKEY(L'ACTKCULA),DIR                                             
         LA    R2,DKEY                                                          
         LA    R1,ACTKACT+(L'ACTKACT-1)                                         
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(R1)                                                         
         B     NAMSRT3                                                          
*                                                                               
NAMSRT15 L     RF,=A(SRTTAB)       SORT BY NAME                                 
         L     R2,SRTNUM           NUMBER                                       
         LA    R3,SRTLNQ           RECORD LENGTH                                
         LA    R4,SRTKLNQ          KEY LENGTH                                   
         GOTO1 XSORT,DMCB,(RF),(R2),(R3),(R4),0                                 
*                                                                               
         L     R3,=A(SRTTAB)       FIND SAME NAME                               
NAMSRT17 SR    R0,R0                                                            
         LA    R4,SRTLNQ(R3)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   NAMSRT19                                                         
         MVI   SRTLAST,X'FF'                                                    
         B     NAMSRT23                                                         
*                                                                               
NAMSRT19 CLC   SRTLAST(SRTKLNQ),0(R4) TEST SAME NAME/CODE                       
         BNE   NAMSRT21                                                         
         AH    R0,=H'1'            COUNT MATCHES                                
         LA    R4,SRTLNQ(R4)                                                    
         CLI   0(R4),X'FF'         TEST EOT                                     
         BNE   NAMSRT19                                                         
*                                                                               
NAMSRT21 LTR   R0,R0               TEST ANY MATCHES                             
         BNZ   *+8                                                              
         MVI   SRTLAST,X'FF'       MARK FOR DELETION                            
         LR    R3,R4                                                            
         B     NAMSRT17                                                         
*                                                                               
NAMSRT23 L     RF,=A(SRTTAB)       RE-SORT TO MOVE DUPS TO TOP                  
         L     R2,SRTNUM           NUMBER                                       
         LA    R3,SRTLNQ           RECORD LENGTH                                
         LA    R4,SRTKLNQ          KEY LENGTH                                   
         GOTO1 XSORT,DMCB,(RF),(R2),(R3),(R4),0                                 
*                                                                               
         L     R3,=A(SRTTAB)       COUNT WHAT'S LEFT                            
         L     R0,SRTNUM                                                        
         SR    RF,RF                                                            
         CLI   0(R3),X'FF'                                                      
         BE    *+16                                                             
         LA    R3,SRTLNQ(R3)                                                    
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         ST    RF,ACCNUM           NUMBER IN TABLE                              
         CH    RF,=Y(MXACC)        TEST MAX                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         L     R3,=A(SRTTAB)                                                    
         LA    R1,ACXTAB                                                        
         ST    R1,ACCTAB           SET A(ACCOUNT CONVERSION)                    
*                                                                               
NAMSRT25 MVC   ACCO-ACCS(L'ACCO,R1),SRTOLD OLD ACCOUNT                          
         MVC   ACCN-ACCS(L'ACCN,R1),SRTOLD NEW ACCOUNT                          
         CLI   SRTFLT1,C'2'          TEST FILTER 1=2                            
         BNE   *+8                                                              
         MVI   ACCN-ACCS+7(R1),C'5'   SECOND POSITION OF SUBDEPT                
         MVC   ACCNME-ACCS(L'ACCNME,R1),SRTNAME   FULL NAME                     
         ZAP   ACCH-ACCS(L'ACCH,R1),=P'0'                                       
         LA    R3,SRTLNQ(R3)                                                    
         LA    R1,ACCLNQ(R1)                                                    
         BCT   RF,NAMSRT25                                                      
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* CREATE FILE OF DUPLICATE NAMES                                      *         
***********************************************************************         
                                                                                
NAMOUT   NTR1  ,                                                                
         OPEN  (FOUT,(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     R3,ACCNUM           NUMBER IN TABLE                              
         L     R4,ACCTAB           A(ACCOUNT CONVERSION)                        
*                                                                               
NAMOUT3  MVC   OUTR,SPACE                                                       
         LA    R5,OUTR                                                          
         MVC   0(L'ACCO,R5),ACCO-ACCS(R4) OLD ACCOUNT                           
         LA    R5,(L'ACCO+1)(R5)                                                
         MVC   0(L'ACCN,R5),ACCN-ACCS(R4)   NEW ACCOUNT                         
         LA    R5,(L'ACCN+1)(R5)                                                
         MVC   0(L'ACCNME,R5),ACCNME-ACCS(R4) FULL NAME                         
         LA    R5,(L'ACCNME+1)(R5)                                              
*                                                                               
         PUT   FOUT,OUTR                                                        
         LA    R4,ACCLNQ(R4)                                                    
         BCT   R3,NAMOUT3                                                       
         CLOSE (FOUT)                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD DYNAMIC TABLE                                                 *         
***********************************************************************         
                                                                                
DYNTAB   NTR1  ,                                                                
         XC    ACCNUM,ACCNUM       CLEAR NUMBER IN TABLE                        
         L     R2,AACTABL                                                       
         USING TABD,R2                                                          
         LA    R3,DKEY                                                          
         USING ACTRECD,R3                                                       
         MVC   DKEY,SPACE          READ ACCOUNTS IN 1R                          
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKULA(2),=C'1R'                                                
*                                                                               
DYN3     GOTO1 ADMGR,DMHI                                                       
         CLC   DIR(3),DKEY                                                      
         BNE   XIT                                                              
         LA    R3,DIR                                                           
         TM    ACTKSTAT,ACTSABLP      TEST BALANCE ELEMENT                      
         BNO   DYN7                                                             
         MVC   TABOLD,ACTKULA         COPY OLD ACCOUNT                          
         MVC   TABNEW,SPACE                                                     
         CLI   ACTKSAF1,C'2'          TEST FILTER 1=2                           
         BNE   DYN7                                                             
         MVC   TABNEW,TABOLD                                                    
         MVI   TABNSUB+1,C'5'         SECOND POSITION OF SUBDEPT                
*                                                                               
         LA    R2,TABLNQ(R2)                                                    
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
* CHANGE ACCOUNT FOR SPECIAL RECORDS                                  *         
***********************************************************************         
                                                                                
HOOK     CLI   MODE,CHNGACC           TEST MODE                                 
         BNE   HOOK200                                                          
         CLI   RECTYP,ACRTOTHR        TEST SPECIAL RECORD                       
         BNH   XIT                                                              
         CLC   SRCHARG(2),=C'1R'      TEST 1R ACCOUNT                           
         BNE   XIT                                                              
         CLC   SRCHARG+7(7),SPACE     TEST HIGH LEVEL ACCOUNT                   
         BNE   XIT                                                              
         MVC   ACCN(2),SRCHARG        ONLY FIX HIGH LEVELS                      
         MVC   ACCN+2(4),SRCHARG+3                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORD BEFORE CONVERSION                                    *         
***********************************************************************         
                                                                                
HOOK200  CLI   MODE,PROCBFR                                                     
         BNE   HOOK300                                                          
         CLI   RECTYP,ACRTSTD        STANDARD HOURS?                            
         BNE   HOOK210                                                          
         L     R2,AINP                                                          
         USING STDRECD,R2                                                       
         CLC   STDKPER,SPACE         LOWEST HANDLED FOR FREE                    
         BNE   XIT                                                              
         CLC   STDKDPT,SPACE         DELETE HIGHEST LEVEL                       
         BE    HOOKDX                                                           
         MVC   STDKOFC,STDKDPT       SHIFT ACC                                  
         CLC   STDKSBD,SPACE                                                    
         BE    XIT                                                              
         MVC   STDKDPT,STDKSBD                                                  
         B     XIT                                                              
*                                                                               
HOOK210  CLI   RECTYP,ACRTCAP        COST ALLOCATION PROFILE?                   
         BNE   HOOK220                                                          
         L     R2,AINP                                                          
         USING CAPRECD,R2                                                       
         CLC   CAPKPER,SPACE         LOWEST HANDLED FOR FREE                    
         BNE   XIT                                                              
         CLC   CAPKOFC(22),SPACE     KEEP HIGHEST LEVEL                         
         BE    XIT                                                              
         B     HOOKDX                                                           
*                                                                               
*                                                                               
HOOK220  CLI   RECTYP,ACRTBUD        BUDGET                                     
         BNE   XIT                                                              
         L     R2,AINP                                                          
         USING BUDRECD,R2                                                       
         CLC   BUDKUNT(2),=C'1R'                                                
         BNE   XIT                                                              
         CLC   BUDKACT+1(11),SPACE                                              
         BE    HOOKDX                                                           
         B     XIT                                                              
*                                                                               
HOOKDX   OI    HKSTA,HKSDEL          DELETE                                     
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS LEDGER RECORD                                               *         
***********************************************************************         
                                                                                
HOOK300  CLI   MODE,PROCAFT        REGULAR HIGH LEVEL ACCOUNTS                  
         BNE   XIT                                                              
         L     R2,AOUT                                                          
         USING ACTRECD,R2                                                       
         CLC   ACTKUNT(2),=C'1R'                                                
         BNE   XIT                                                              
         CLI   RECTYP,ACRTLDG                                                   
         BNE   HOOK500                                                          
*                                                                               
         USING LDGRECD,R3                                                       
         SR    R1,R1                                                            
         L     R3,AOUT                                                          
         LA    R3,LDGRFST          GET TO FIRST ELEMENT                         
         USING ACLELD,R3                                                        
HOOK330  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                BAD LEDGER RECORD                            
         CLI   ACLEL,ACLELQ        ACCOUNT LENGTHS ELEMENT                      
         BE    HOOK350                                                          
         IC    R1,ACLLN                                                         
         AR    R3,R1                                                            
         B     HOOK330                                                          
*                                                                               
HOOK350  MVI   ACLVLEN,2                                                        
         MVC   ACLVDESC,=CL15'OFFICE'                                           
         LA    R3,L'ACLVALS(R3)                                                 
         MVI   ACLVLEN,4                                                        
         MVC   ACLVDESC,=CL15'STAFF TYPE'                                       
         LA    R3,L'ACLVALS(R3)                                                 
         MVI   ACLVLEN,6                                                        
         MVC   ACLVDESC,=CL15'SUB-DEPT'                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT RECORDS - LEVEL 2 & 3                               *         
***********************************************************************         
                                                                                
HOOK500  CLC   ACTKACT+5(7),SPACE     LOWEST LEVEL HANDLED IN BASE              
         BNE   XIT                                                              
         CLC   ACTKACT+1(4),SPACE     DELETE LEVEL 1                            
         BE    HOOKDX                                                           
         MVC   ACCN,SPACE                                                       
         MVC   ACCN(2),ACTKULA                                                  
         MVC   ACCN+2(4),ACTKACT+1                                              
*                                                                               
         L     R2,AOUT                REPLACE ACCOUNT                           
         MVC   ACTKULA,ACCN                                                     
         MVC   NEWULA,ACCN                                                      
*                                                                               
         CLC   ACTKACT+2(2),SPACE     TEST OFFICE LEVEL                         
         BE    XIT                                                              
*                                                                               
         MVC   SRRCKEY+3(4),ACTKACT   ADD SUB-DEPT                              
         LA    R1,SHELLREC                                                      
         ST    R1,HKNREC              SET ADDRESS OF NEW RECORD                 
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND WORK AREA                                             *         
***********************************************************************         
                                                                                
AACTABL  DS    A                   ADDRESS OF ACCOUNT CONVERSION TABLE          
*                                                                               
SRTNUM   DC    F'0'                                                             
OUTR     DS    CL80                                                             
*                                                                               
* SHELL RECORD FOR SUB-DEPT                                                     
*                                                                               
SHELLREC DS    0H                                                               
SRRCLN   DC    X'00630000'         FOR PUT                                      
SRRCKEY  DC    X'BE',CL41'1RAABB00'                                             
         DC    X'005F'                                                          
         DC    X'00'                                                            
         DC    CL5' '                                                           
         DC    XL6'00'                                                          
SRRCNAME DC    X'2009',CL7'SUBDEPT'                                             
SRRC     DC    X'301D000040'                                                    
         DC    XL6'00'                                                          
         DC    X'970101970101'                                                  
         DC    X'400040404000400000000000'                                      
         DC    X'00'               EOR                                          
*                                                                               
FOUT     DCB   DDNAME=FOUT,DSORG=PS,MACRF=(PM),                        X        
               RECFM=FB,LRECL=80,BLKSIZE=80                                     
         LTORG                                                                  
         EJECT                                                                  
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
MXSRT    EQU   8000                                                             
SRTTAB   DS    (MXSRT)CL(SRTLNQ)                                                
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
                                                                                
TABD     DSECT                                                                  
TABOLD   DS    CL14                OLD ACCOUNT                                  
TABNEW   DS    0CL14               NEW ACCOUNT                                  
TABNUL   DS    CL2                                                              
TABNOFD  DS    CL4                 OFFICE / DEPT                                
TABNSUB  DS    CL2                 SUBDEPT                                      
TABNPER  DS    CL6                 PERSON                                       
TABLNQ   EQU   *-TABD                                                           
*                                                                               
*                                                                               
SRTD     DSECT                                                                  
SRTREC   DS    0CL(SRTLNQ)         NAME SORT RECORD                             
SRTLAST  DS    CL36                LAST NAME                                    
SRTPER   DS    CL13                U/L +FIRST 11 OF CODE                        
SRTKLNQ  EQU   *-SRTLAST                                                        
SRTOLD   DS    CL14                OLD CODE                                     
SRTNAME  DS    CL36                NAME                                         
SRTFLT1  DS    CL1                 FILTER 1                                     
SRTLNQ   EQU   *-SRTLAST                                                        
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
**PAN#1  DC    CL21'115ACNV05BD  09/19/00'                                      
         END                                                                    
