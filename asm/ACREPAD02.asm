*          DATA SET ACREPAD02  AT LEVEL 013 AS OF 08/17/11                      
*PHASE ACAD02A                                                                  
CHECKS   TITLE 'CHECK ACCOUNT ADDRESSES'                                        
         PRINT NOGEN                                                            
ACAD02   CSECT                                                                  
         NMOD1 0,*ACAD02*,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACAD02D,RC                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    RQF00                                                            
         CLI   MODE,PROCACC                                                     
         BE    PAC00                                                            
         CLI   MODE,REQLAST                                                     
         BE    RQL00                                                            
         CLI   MODE,RUNLAST                                                     
         BE    RUN00                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING MASTD,R5                                                         
RUNF     DS    0H                                                               
         L     R5,ADMASTC                                                       
         MVC   DUB,=CL8'ADFORM'                                                 
         MVC   DUB+6(1),MCTEST3    LOAD EITHER LIVE OR TEST                     
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   RUNF10                                                           
         MVC   DUB,=CL8'ADFORM'                                                 
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
RUNF10   MVC   ADFORM,4(R1)        A(ADFORM)                                    
         MVC   MCAPHAS3,4(R1)      SET UP PATCHABILITY                          
*                                                                               
         ICM   R0,15,0(R1)         LENGTH OF PHASE                              
         ICM   RF,15,4(R1)         A(ADFORM)                                    
         STCM  RF,15,MCUSRDMP                                                   
         AR    RF,R0                                                            
         LA    RF,4095(RF)                                                      
         STCM  RF,15,MCUSRDMP+4                                                 
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
RQF00    MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
PAC00    DS    0H                                                               
         MVC   MESSAGE,SPACES                                                   
         MVC   E8CITY,SPACES                                                    
         MVC   E8STATE,SPACES                                                   
         MVC   E8ZIP,SPACES                                                     
         MVC   E8CTRY,SPACES                                                    
         ZAP   TOTACCS,=P'0'                                                    
*                                                                               
         XC    TPADDR(4*L'TPADDR),TPADDR                                        
         XC    SVADDR(3*L'SVADDR),SVADDR                                        
*                                                                               
         CLI   QOPT3,C'Y'             SUPPRESS LOCKED ACCTS?                    
         BNE   ADDR02                 NO                                        
         L     R2,ADACC               YES                                       
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   ADDR02                                                           
         USING RSTELD,R2                                                        
         TM    RSTSTAT1,RSTSACIL      SKIP IF LOCKED                            
         BO    XIT                                                              
*                                                                               
ADDR02   ICM   R2,15,ADACCADD         A(ADDRESS ELEMENT)                        
         BZ    ADDR50                 NO ADDRESS                                
         USING ADRELD,R2                                                        
         SR    R0,R0                                                            
         ICM   R0,1,ADRNUM            NUMBER OF LINES                           
         BZ    ADDR50                 NO ADDRESS                                
         CLI   ADRNUM,1               ONLY ONE LINE                             
         BNE   *+14                                                             
         CLC   ADRADD1,SPACES         AND IT'S SPACES                           
         BE    ADDR50                 NO ADDRESS                                
         LA    R5,TPADDR                                                        
         LA    R4,ADRADD1                                                       
*                                                                               
ADDR03   MVC   0(L'TPADDR,R5),0(R4)   UP TO 4 LINES OF ADDRESS                  
         LA    R4,L'ADRADD1(R4)                                                 
         LA    R5,L'TPADDR(R5)                                                  
         BCT   R0,ADDR03                                                        
         DROP  R2                                                               
*                                                                               
         LA    R2,TPADDR              POINT TO ADDRESS                          
         LA    R3,L'TPADDR            LENGTH OF ONE ADDRESS LINE                
         LA    R4,TPADDR+(3*L'TPADDR) FOURTH ADDRESS LINE                       
         CLC   ALPHAID,=C'H7'              FOR MINDSHARE U.S....                
         BNE   ADDR10                                                           
         MVC   SVADDR(3*L'TPADDR),TPADDR+L'TPADDR                               
         OC    TPADDR+(1*L'TPADDR),SPACES  MAKE SURE STATE IS UPPERCASE         
         OC    TPADDR+(2*L'TPADDR),SPACES  IN EITHER ADDR LINE 2,3 OR 4         
         OC    TPADDR+(3*L'TPADDR),SPACES                                       
*                                                                               
ADDR10   CR    R2,R4                  POINTING TO FIRST LINE?                   
         BE    ADDR25                 MUST BE AN ERROR                          
         GOTO1 ADFORM,DMCB,((R3),(R4)),(30,E8CITY),                    +        
               E8STATE,(9,E8ZIP),(2,E8CTRY),0                                   
         TM    0(R1),X'01'            01=FOREIGN, NO VALIDATION DONE            
         BO    ADDR60                                                           
         TM    0(R1),X'80'            80=SERIOUS ERROR                          
         BO    ADDR15                                                           
         CLC   E8CITY,SPACES                                                    
         BNE   ADDR20                                                           
ADDR15   CLC   0(L'TPADDR,R4),SPACES  IF ZERO, THEN NO ADDRESS LINE             
         BNH   *+8                                                              
         AHI   R3,L'TPADDR            SEND ANOTHER LINE                         
         AHI   R4,-L'TPADDR           BACK UP                                   
         B     ADDR10                                                           
*                                                                               
ADDR20   TM    0(R1),X'02'            02=STATE DOES NOT MATCH ZIP               
         BO    ADDR30                                                           
         MVC   E8ADR1,0(R2)                                                     
         LA    R2,L'TPADDR(R2)                                                  
         CR    R2,R4                                                            
         BE    ADDR75                                                           
         MVC   E8ADR2,0(R2)                                                     
         B     ADDR75                                                           
*                                                                               
ADDR25   MVC   MESSAGE,INVADDR                                                  
         AP    INVADDC,=P'1'                                                    
         B     ADDRPRT                                                          
*                                                                               
ADDR30   MVC   MESSAGE,ZIPNOMAT                                                 
         CLI   E8CTRY,C'C'                                                      
         BNE   *+10                                                             
         MVC   MESSAGE,POSNOMAT                                                 
         MVC   MESSAGE+38(2),E8STATE                                            
         AP    INVZIPC,=P'1'                                                    
         B     ADDRPRT                                                          
*                                                                               
ADDR50   MVC   MESSAGE,NOADDR                                                   
         AP    NOADDRC,=P'1'                                                    
         B     ADDRPRT                                                          
*                                                                               
ADDR60   MVC   MESSAGE,FOREIGN                                                  
         AP    ADDOKC,=P'1'                                                     
         B     ADDRPRT                                                          
*                                                                               
ADDR75   MVC   MESSAGE,ADDOK                                                    
         CLC   ALPHAID,=C'H7'              FOR MINDSHARE U.S....                
         BNE   *+10                                                             
         MVC   TPADDR+(1*L'TPADDR)(3*L'TPADDR),SVADDR RESTORE ADDR CASE         
         AP    ADDOKC,=P'1'                          TO LINES 2,3,4             
         B     ADDRPRT                                                          
*------------------------------------------------------------------             
ADDRPRT  CLC   MESSAGE,ADDOK                                                    
         BNE   ADDRP01               ALWAYS PRINT BAD ADDRESSES                 
         CLI   QOPT1,C'A'            DO WE WANT TO PRINT ACCOUNTS?              
         BNE   XIT                   NO                                         
*                                                                               
ADDRP01  L     R2,ADACC                                                         
         USING TRNRECD,R2                                                       
         MVC   P(L'TRNKULA),TRNKULA                                             
         DROP  R2                                                               
         MVC   P+L'ADRADD1(L'MESSAGE),MESSAGE                                   
         GOTO1 ACREPORT                                                         
*                                                                               
         CLC   MESSAGE,ADDOK                                                    
         BNE   ADDRP05               ALWAYS PRINT BAD ADDRESSES                 
         CLI   QOPT1,C'A'            DO WE WANT TO PRINT GOOD ONES?             
         BNE   ADDRP20               NO                                         
*                                                                               
ADDRP05  LA    R2,TPADDR             POINT TO ADDRESS                           
         LA    R4,4                                                             
ADDRP10  OC    0(L'TPADDR,R2),0(R2)  IF ZERO, THEN NO ADDRESS LINE              
         BZ    ADDRP20                                                          
         MVC   P+2(L'ADRADD1),0(R2)                                             
         GOTO1 ACREPORT                                                         
         LA    R2,L'TPADDR(R2)                                                  
         BCT   R4,ADDRP10                                                       
         GOTO1 ACREPORT                                                         
*                                                                               
ADDRP20  GOTO1 ACREPORT                                                         
*------------------------------------------------------------------             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
RQL00    GOTO1 ACREPORT                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
*                                                                               
         AP    TOTACCS,INVADDC                                                  
         AP    TOTACCS,INVZIPC                                                  
         AP    TOTACCS,NOADDRC                                                  
         EDIT  TOTACCS,(10,P),ZERO=NOBLANK                                      
         MVC   P+11(6),=CL6'OUT OF'                                             
         AP    TOTACCS,ADDOKC                                                   
         EDIT  TOTACCS,(10,P+18),ZERO=NOBLANK,ALIGN=LEFT                        
         MVC   P+29(41),=C'ACCOUNT ADDRESSES ARE INCORRECTLY ENTERED'           
         GOTO1 ACREPORT                                                         
*                                                                               
         GOTO1 ACREPORT                                                         
         MVC   P(27),=CL27'NON-MATCHING POSTAL CODES ='                         
         EDIT  INVZIPC,(10,P+28),ZERO=NOBLANK,ALIGN=LEFT                        
         GOTO1 ACREPORT                                                         
         MVC   P(11),=CL11'NO ADDRESS='                                         
         EDIT  NOADDRC,(10,P+12),ZERO=NOBLANK,ALIGN=LEFT                        
         GOTO1 ACREPORT                                                         
         MVC   P(14),=CL14'OTHER INVALID='                                      
         EDIT  INVADDC,(10,P+15),ZERO=NOBLANK,ALIGN=LEFT                        
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
RUN00    CLI   QOPT2,C'C'            PRINTED COUNTRY LIST WANTED?               
         BNE   XIT                                                              
*                                                                               
         GOTO1 ACREPORT                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,3                                                       
*                                                                               
         GOTO1 ADFORM,DMCB,0         CALL FOR COUNTRY TABLE                     
         ICM   R1,15,0(R1)           PICK UP ADDRESS OF COUNTRY TAB             
*                                                                               
         USING CTRYTABD,R1                                                      
RUN10    CLI   CTRYC,X'FF'                                                      
         BE    RUN20                                                            
         ZIC   R2,CTRYNL                                                        
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P(0),CTRYN                                                       
         MVC   P+40(L'CTRYC),CTRYC      PRINT OUT THE TABLE                     
         GOTO1 ACREPORT                                                         
         AHI   R1,CTRYOLQ+1    OVERHEAD +1 FOR EX MOVE                          
         AR    R1,R2           NAME LENGTH-1 FOR EX MOVE                        
         B     RUN10                                                            
         DROP  R1                                                               
*                                                                               
RUN20    GOTO1 ACREPORT                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MESSAGES AND CONSTANTS                                                        
***********************************************************************         
ADDOK    DC    CL50'--ADDRESS IN CORRECT FORM--'                                
FOREIGN  DC    CL50'--FOREIGN ADDRESS - ALLOWED, BUT NOT VALIDATED--'           
INVADDR  DC    CL50'**INVALID ADDRESS**'                                        
NOADDR   DC    CL50'**NO ADDRESS**'                                             
POSNOMAT DC    CL50'**POSTAL CODE DOES NOT MATCH PROVINCE(  )**'                
ZIPNOMAT DC    CL50'**ZIP CODE DOES NOT MATCH STATE CODE (  )**'                
*                                                                               
ADFORM   DS    V                                                                
***********************************************************************         
* PROGRAM DATA FIELDS                                                           
***********************************************************************         
MESSAGE  DC    CL50' '                                                          
*                                                                               
TPADDR   DC    4CL(L'ADRADD1)' '                                                
SVADDR   DC    3CL(L'ADRADD1)' '                                                
*                                                                               
E8ADR1   DC    CL(L'ADRADD1)' '                                                 
E8ADR2   DC    CL(L'ADRADD1)' '                                                 
E8CITY   DC    CL30' '                                                          
E8STATE  DC    CL2' '                                                           
E8ZIP    DC    CL9' '                                                           
E8CTRY   DC    CL3' '                                                           
*                                                                               
INVADDC  DC    PL6'0'                                                           
INVZIPC  DC    PL6'0'                                                           
NOADDRC  DC    PL6'0'                                                           
ADDOKC   DC    PL6'0'                                                           
TOTACCS  DC    PL8'0'                                                           
*                                                                               
         GETEL R2,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ACAD02D  DSECT                                                                  
ELCODE   DS    XL1                                                              
         EJECT                                                                  
***********************************************************************         
* ACADFORMD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACADFORMD                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPAD02 08/17/11'                                      
         END                                                                    
