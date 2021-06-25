*          DATA SET ACREPZ302A AT LEVEL 040 AS OF 08/16/00                      
*PHASE ACZ302A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'FIND MISSING 1R ACCOUNTS USED AS CONTRAS'                       
         PRINT NOGEN                                                            
ACZ302   CSECT                                                                  
         NMOD1 0,**ACZ3**,RR=R7                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZ3D,RC                                                         
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
                                                                                
RQF00    CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
         MVI   FCRDACC,C'N'                                                     
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDHIST,C'N'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRESET,C'Y'                                                     
         MVC   PAGE,=H'1'                                                       
*                                                                               
         LA    R2,AKEY                                                          
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,RCCOMPFL                                                 
         MVC   TRNKUNT(2),=C'SJ'                                                
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,AKEY,ADIR                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
REQF04   GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,AKEY,ADIR                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AKEYSV,AKEY                 SAVE THE KEY LAST READ               
         LA    R2,ADIR                                                          
         CLC   TRNKCPY,RCCOMPFL                                                 
         BNE   XIT                                                              
         CLC   TRNKUNT(2),=C'SJ'                                                
         BNE   XIT                                                              
         CLC   TRNKCUNT(2),=C'1R'                                               
         BNE   REQF04                                                           
         CLC   TRNKDATE,SPACES                                                  
         BNH   REQF04                                                           
         CLC   TRNKDATE,=X'990101'                                              
         BL    REQF04                                                           
         MVC   WORK,SPACES                                                      
         LA    R4,WORK                                                          
         LA    R3,CONCODE                                                       
         USING CONTRD,R4                                                        
         MVC   CONTACT,TRNKCACT                                                 
         MVI   CONSTAT1,0                                                       
         GOTO1 BINADD,DMCB,(R4),(R3)                                            
         OC    RECADDR,RECADDR                                                  
         BZ    REQF04                                                           
*                                                                               
         LA    R6,P                                                             
         USING PLINED,R6                                                        
         MVC   PLINED(PLINLEN),SPACES                                           
         MVC   PACCT,TRNKUNT                                                    
         MVC   PCACCT,TRNKCUNT                                                  
         MVC   PWC,TRNKOFF                                                      
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(0,PDTE)                                
         MVC   PTREF,TRNKREF                                                    
         GOTO1 ACREPORT                                                         
         B     REQF04                                                           
*                                                                               
XIT      XIT1  1                                                                
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
                                                                                
RNL00    CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         GOTO1 ACREPORT                                                         
         LA    R5,RECCNT                                                        
*                                                                               
RNL3     MVC   P+1(16),4(R5)                                                    
         EDIT  (P4,0(R5)),(9,P+20),COMMAS=YES                                   
         GOTO1 ACREPORT                                                         
         LA    R5,L'RECCNT(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   RNL3                                                             
*                                                                               
         LA    R5,CONCODE          ADDR OF TABLE                                
         USING BIND,R5                                                          
         L     R3,BININ            NUMBER OF TABLE ENTRIES                      
         LTR   R3,R3                                                            
         BNP   XIT                                                              
         LA    R4,BINTABLE         1ST TABLE ENTRY                              
RNL08    TM    CONSTAT1,CONSTFND                                                
         BO    RNL12                                                            
         MVC   P+1(12),CONTACT       MISSING CONTRA                             
         GOTO1 ACREPORT                                                         
*                                                                               
RNL12    LA    R4,CONTLEN(R4)                                                   
         BCT   R3,RNL08                                                         
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
*              ADD ITEM TO BINSRCH TABLE                                        
*              P1                  A(ITEM TO BE ADDED)                          
*              P2                  A(TABLE)                                     
****************************************************************                
*                                                                               
         USING BIND,R5                                                          
         USING CONTRD,R4                                                        
BINADD   NTR1                                                                   
         XC    RECADDR,RECADDR                                                  
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTABLE         A(TABLE)                                     
         L     R4,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         MVC   RECADDR,DMCB        A(RECORD FOUND)                              
         L     R4,DMCB                                                          
         CLI   DMCB,1                                                           
         BNE   BIN10               ALREADY IN TABLE                             
         LA    R2,AKEY                                                          
         USING CACRECD,R2                                                       
         MVC   CACKEY,SPACES                                                    
         MVC   CACKCPY,RCCOMPFL                                                 
         MVC   CACKUNT(2),=C'1R'                                                
         MVC   CACKACT,CONTACT                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,AKEY,ADIR                             
         CLI   8(R1),0                                                          
         BE    BIN08                                                            
         MVC   AKEY,AKEYSV                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,AKEY,ADIR                             
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
BIN08    MVC   AKEY,AKEYSV                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,AKEY,ADIR                             
         CLI   8(R1),0                                                          
         BE    BIN12                                                            
         DC    H'0'                                                             
BIN10    TM    CONSTAT1,CONSTFND   1R ON THE FILE?                              
         BZ    XIT                                                              
BIN12    OI    CONSTAT1,CONSTFND   FOUND                                        
         XC    RECADDR,RECADDR                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
                                                                                
HELLO    DC    V(HELLO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
AIO      DC    A(IOA)                                                           
*                                                                               
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
*                                                                               
RECCNT   DS    0CL20                                                            
CHARCD   DC    PL4'0',CL16'RECORDS CHANGED'                                     
ADDRCD   DC    PL4'0',CL16'RECORDS ADDED'                                       
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* IO AREAS AND TABLES                                                 *         
***********************************************************************         
*                                                                               
CONCODE  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(CONTLEN)        RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(CONTKLN)        KEY LENGTH                                   
         DC    AL4(CONTMAX)        MAX IN TABLE                                 
CONTTAB  DS    CL(CONTMAX*CONTLEN) THE TABLE                                    
*                                                                               
IOA      DS    XL2000                                                           
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
                                                                                
ACZ3D    DSECT                                                                  
AKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
ADIR     DS    CL(ACCKDA-ACCKEY)   TRANSACTION DIRECTORY RECORD                 
ADA      DS    XL(L'ACCKDA)        DISK ADDRESS                                 
AKEYSV   DS    CL(L'ACCKEY)        DIRECTORY KEY                                
ADIRSV   DS    CL(ACCKDA-ACCKEY)   TRANSACTION DIRECTORY RECORD                 
RECADDR  DS    A                                                                
*                                                                               
CONTRD   DSECT                                                                  
CONTACT  DS    CL12                ACCOUNT                                      
CONTKLN  EQU   *-CONTRD                                                         
CONSTAT1 DS    XL1                                                              
CONSTFND EQU   X'80'               FOUND                                        
CONTLEN  EQU   *-CONTRD                                                         
CONTMAX  EQU   4500                                                             
*                                                                               
*                                                                               
PLINED   DSECT                                                                  
         DS    CL2                                                              
PACCT    DS    CL14                                                             
         DS    CL2                                                              
PWC      DS    CL2                                                              
         DS    CL2                                                              
PCACCT   DS    CL14                                                             
         DS    CL2                                                              
PDTE     DS    CL14                                                             
         DS    CL2                                                              
PTREF    DS    CL6                                                              
         DS    CL2                                                              
PTTYPE   DS    CL2                                                              
PLINLEN  EQU   *-PLINED                                                         
*                                                                               
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040ACREPZ302A08/16/00'                                      
         END                                                                    
