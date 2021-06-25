*          DATA SET ACREPXL02S AT LEVEL 086 AS OF 05/01/02                      
*PHASE ACXL02A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
***********************************************************************         
*   DO NOT DELETE THIS BOOK!!!!                                       *         
*   READS PERSON RECS AND CHECKS FOR TIME AND SALARY                            
***********************************************************************         
         TITLE 'PERSON RECORD FILE FIX'                                         
ACXL02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXL**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXLD,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         L     RF,GETOPT                                                        
         MVC   0(2,RF),=X'07FE'                                                 
         ZAP   COUNT,=P'0'                                                      
         ZAP   COUNTALL,=P'0'                                                   
         MVC   OFFICE,SPACES                                                    
         MVC   DEPT,SPACES                                                      
         MVC   SUBDPT,SPACES                                                    
*                                                                               
         LA    RE,VTYPES           RELOCATE VTYPES                              
         LA    R0,ADCONS                                                        
         LA    RF,VTYPLNQ                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDACC,C'Y'                                                     
         MVI   FIRST,C'Y'          FIRST TIME THROUGH                           
         SPACE 1                                                                
         USING PERRECD,R4                                                       
         L     R4,AIO1                                                          
         MVC   SVKEY,SPACES                                                     
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,X'0F'                                                    
         MVC   PERKCPY,QCOMPANY                                                 
         CLI   QOPT1,C'A'                                                       
         BNE   *+8                                                              
         MVI   PERKCPY,X'00'       READ FOR ALL COMPANIES                       
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   SVKEY,0(R4)                                                      
         B     *+10                                                             
*                                                                               
REQF100  MVC   COMMAND,=CL8'DMRSEQ'                                             
         L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,COMMAND,=CL8'ACCFIL',(R4),(R4)                      
         CLI   QOPT1,C'A'          READ FOR ALL                                 
         BE    REQF102                                                          
         MVC   SVCO,PERKCPY                                                     
         CLI   FIRST,C'Y'                                                       
         BNE   REQF100A                                                         
*        BAS   RE,GETLDG                                                        
*        L     R4,AIO1                                                          
*        MVC   0(L'SVKEY,R4),SVKEY                                              
*        MVC   COMMAND,=CL8'DMREAD'                                             
*        GOTO1 DATAMGR,DMCB,COMMAND,=CL8'ACCFIL',(R4),(R4)                      
REQF100A MVI   FIRST,C'N'                                                       
         CLC   SVKEY(PERKCODE-PERKEY),PERKEY  SAME CO?                          
         BE    REQF101                                                          
         MVC   P(20),=C'TOTAL PERSON CODES: '                                   
         EDIT  (P8,COUNT),(12,P+25),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 ACREPORT                                                         
         B     REQFX                                                            
REQF101  MVC   PERSON,PERKCODE                                                  
         MVC   SVKEY,0(R4)                                                      
         MVC   SVCO,PERKCPY                                                     
         B     REQF110                                                          
*                                                                               
REQF102  CLC   SVKEY(PERKCPY-PERKEY),PERKEY   STILL '0F'                        
         BE    REQF103                                                          
         GOTO1 ACREPORT                                                         
         MVC   P(13),=C'COMPANY CODE '                                          
         GOTO1 HEXOUT,DMCB,SVCO,P+14,L'SVCO                                     
         GOTO1 ACREPORT                                                         
         MVC   P(20),=C'TOTAL PERSON CODES: '                                   
         GOTO1 ACREPORT                                                         
         EDIT  (P8,COUNT),(12,P+25),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P(13),=C'GRAND TOTAL: '                                          
         EDIT  (P8,COUNTALL),(12,P+15),COMMAS=YES,ZERO=NOBLANK                  
         GOTO1 ACREPORT                                                         
         B     REQFX                                                            
REQF103  CLI   FIRST,C'Y'                                                       
         BNE   REQF103A                                                         
         MVC   SVCO,PERKCPY                                                     
         BAS   RE,GETLDG                                                        
         L     R4,AIO1                                                          
         MVC   0(L'SVKEY,R4),SVKEY                                              
         MVC   COMMAND,=CL8'DMREAD'                                             
         GOTO1 DATAMGR,DMCB,COMMAND,=CL8'ACCFIL',(R4),(R4)                      
         B     REQF104                                                          
REQF103A CLC   SVKEY(PERKCODE-PERKEY),PERKEY SAME CO?                           
         BE    REQF104                                                          
         GOTO1 ACREPORT                                                         
         MVC   P(13),=C'COMPANY CODE '                                          
         GOTO1 HEXOUT,DMCB,SVCO,P+14,L'SVCO                                     
         GOTO1 ACREPORT                                                         
         MVC   P(20),=C'TOTAL PERSON CODES: '                                   
         EDIT  (P8,COUNT),(12,P+25),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         ZAP   COUNT,=P'0'         RESET COUNTER FOR NEXT CO                    
         MVC   SVKEY,0(R4)                                                      
         MVC   SVCO,PERKCPY                                                     
*        BAS   RE,GETLDG                                                        
         L     R4,AIO1                                                          
         MVC   0(L'SVKEY,R4),SVKEY                                              
         MVC   COMMAND,=CL8'DMREAD'                                             
         GOTO1 DATAMGR,DMCB,COMMAND,=CL8'ACCFIL',(R4),(R4)                      
REQF104  MVC   SVCO,PERKCPY                                                     
         MVC   PERSON,PERKCODE                                                  
         MVC   SVKEY,0(R4)                                                      
         MVI   FIRST,C'N'          FIRST TIME OVER                              
*                                                                               
         USING EMPELD,R4                                                        
REQF110  AH    R4,DATADISP                                                      
REQF111  CLI   0(R4),0                                                          
         BE    REQF100                                                          
         CLI   0(R4),EMPELQ                                                     
         BE    *+16                                                             
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     REQF111                                                          
         OC    EMPTRM,EMPTRM       EMPLOYEE TERMINATED                          
         BZ    REQF100                                                          
         MVC   SVTRMDT,EMPTRM      SAVE TERM DATE                               
         DROP  R4                                                               
*                                                                               
         USING LOCELD,R4                                                        
REQF115  CLI   0(R4),0                                                          
         BE    REQF100                                                          
         CLI   0(R4),LOCELQ                                                     
         BNE   REQF116                                                          
         OC    LOCEND,LOCEND       IS THERE AN END DATE                         
         BZ    REQF116                                                          
         CLC   LOCEND,SVTRMDT      EQUAL TO TERM DATE                           
         BE    *+16                                                             
REQF116  SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     REQF115                                                          
*                                                                               
         MVC   OFFICE,LOCOFF       SAVE OFFICE                                  
         MVC   DEPT,LOCDEPT                                                     
         MVC   SUBDPT,LOCSUB                                                    
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),LOCEND                                                   
         OC    LOCSALKD,LOCSALKD   ANY SAL LOCK DATE                            
         BZ    *+10                                                             
         MVC   WORK(3),LOCSALKD                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+10)                                 
         GOTO1 ADDAY,DMCB,(C'D',WORK+10),(0,WORK+20),1  ADD 1 DAY               
         GOTO1 DATCON,DMCB,(0,WORK+20),(1,SVDATE)                               
*&&DO                                                                           
         ZIC   R1,LEVELNA                                                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),LOCOFF                                                  
         LA    R3,1(R1,R3)                                                      
         ZIC   R1,LEVELNB                                                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),LOCDEPT                                                 
         LA    R3,1(R1,R3)                                                      
         ZIC   R1,LEVELNC                                                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),LOCSUB                                                  
         MVC   STDATE,LOCSTART                                                  
*&&                                                                             
*                                                                               
REQF120  DS    0H                                                               
*        BAS   RE,CHKTIME                                                       
         BAS   RE,CHKSAL                                                        
         BNE   REQF130                                                          
*                                                                               
         MVC   P(8),PERSON                                                      
         MVC   P+11(2),OFFICE                                                   
         MVC   P+16(3),DEPT                                                     
         MVC   P+25(3),SUBDPT                                                   
         GOTO1 DATCON,DMCB,(1,SVTRMDT),(11,P+34)                                
*        GOTO1 DATCON,DMCB,(1,TMSDATE),(11,P+12)                                
*        MVC   P+25(8),TMSLOC                                                   
         GOTO1 ACREPORT                                                         
         AP    COUNT,=P'1'                                                      
         AP    COUNTALL,=P'1'                                                   
REQF130  L     R4,AIO1                                                          
         MVC   0(L'SVKEY,R4),SVKEY                                              
         MVC   COMMAND,=CL8'DMREAD'                                             
         GOTO1 DATAMGR,DMCB,COMMAND,=CL8'ACCFIL',(R4),(R4)                      
         B     REQF100                                                          
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------------*          
*       CHECK FOR TIME AT SAVED LOCATION OUTSIDE                                
*       OF DATE                                                                 
*--------------------------------------------------------------------*          
         USING TSWRECD,R5                                                       
CHKTIME  NTR1                                                                   
         MVC   TMSDATE,SPACES                                                   
         MVC   TMSLOC,SPACES                                                    
*                                                                               
CHK10    L     R5,AIO2                                                          
         MVC   SVKEY2,SPACES                                                    
         MVC   TSWKEY,SPACES                                                    
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,SVCO                                                     
         MVC   TSWKPER,PERSON                                                   
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   SVKEY2,0(R5)                                                     
         B     *+10                                                             
*                                                                               
CHK20    MVC   COMMAND,=CL8'DMRSEQ'                                             
         L     R5,AIO2                                                          
         GOTO1 DATAMGR,DMCB,COMMAND,=CL8'ACCDIR',(R5),(R5)                      
         CLC   SVKEY2(TSWKEND-TSWKEY),TSWKEY                                    
         BNE   CHKNO                                                            
         MVC   SVKEY2,0(R5)                                                     
*                                                                               
         ZICM  R1,TSWKEND,3                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,WORK                                                        
         CLC   WORK(3),STDATE      IS WEEK ENDING DTE HIGHER                    
         BL    CHK20                                                            
         CLC   TSWKODS,LOCATION    AND A DIFFERENT LOC?                         
         BE    CHK20                                                            
         MVC   DA,TSWKDA                                                        
         L     R5,AIO2                                                          
         MVC   COMMAND(8),=C'GETREC'                                            
         GOTO1 DATAMGR,DMCB,COMMAND,=CL8'ACCMST',DA,(R5),DMWORK                 
*                                                                               
         USING TIMRECD,R5                                                       
         L     R5,AIO2                                                          
*        CLC   TIMKREF,=C'*TIME*'                                               
*        BNE   CHKNO                                                            
         CLC   TIMKPEDT,STDATE                                                  
         BNL   CHKYES                                                           
*                                                                               
CHKNO    B     XNO                                                              
CHKYES   MVC   TMSDATE,WORK                                                     
         MVC   TMSLOC,TIMKACT                                                   
         B     XYES                                                             
         DROP  R5                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*       CHECK SALARY AFTER TERMINATION DATE                                     
*--------------------------------------------------------------------*          
         USING PHIRECD,R5                                                       
CHKSAL   NTR1                                                                   
         MVI   SEQNUM,0            START AT 0                                   
*                                                                               
         L     R5,AIO2                                                          
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ                                                 
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,SVCO                                                     
         MVC   PHIKOFC,OFFICE                                                   
         MVC   PHIKDPT,DEPT                                                     
         MVC   PHIKSBD,SUBDPT                                                   
         MVC   PHIKPER,PERSON                                                   
         MVC   PHIKMOA,X'0000'                                                  
         MVC   PHIKSEQ,SEQNUM      GET MOST CURRENT                             
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   SVKEY2,0(R5)                                                     
         B     *+10                                                             
*                                                                               
CHKS20   MVC   COMMAND,=CL8'DMRSEQ'                                             
         L     R5,AIO2                                                          
         GOTO1 DATAMGR,DMCB,COMMAND,=CL8'ACCFIL',(R5),(R5)                      
         CLC   SVKEY2(PHIKMOA-PHIKEY),PHIKEY                                    
         BNE   CHSALNO                                                          
         ZICM  R1,PHIKMOA,3                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,WORK                                                        
         CLC   WORK(2),SVDATE                                                   
         BL    CHKS20                                                           
*                                                                               
         USING PDEELD,R5                                                        
         AH    R5,DATADISP                                                      
CHKS30   CLI   0(R5),0                                                          
         BE    CHKS20                                                           
         CLI   0(R5),PDEELQ                                                     
         BE    *+16                                                             
CHKS35   SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     CHKS30                                                           
*                                                                               
         CLC   PDEDTE,SVDATE                                                    
         BNH   CHKS35                                                           
         ZAP   WORK(8),PDEAMT                                                   
         AP    WORK(8),PDEADJ                                                   
         CP    WORK(8),=P'0'                                                    
         BE    CHKS35                                                           
         CLC   PDEDTE,=X'980101'    ON OR AFTER JAN01/98                        
         BL    CHKS35                                                           
*                                                                               
CHSALYS  B     XYES                                                             
CHSALNO  B     XNO                                                              
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        GET LEDGER LENGTHS                                           *         
*---------------------------------------------------------------------*         
         USING LDGRECD,R5                                                       
GETLDG   NTR1                                                                   
         L     R5,AIO2                                                          
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,SVCO        COMPANY                                      
         MVI   LDGKUNT,C'1'                                                     
         MVI   LDGKLDG,C'R'                                                     
         MVC   SVKEY2,0(R5)                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCFIL',(R5),(R5)                     
         CLC   SVKEY2(L'SVKEY2),LDGKEY                                          
         BNE   GETLX                                                            
*                                                                               
         USING ACLELD,R5                                                        
         AH    R5,DATADISP                                                      
GETL10   CLI   0(R5),0                                                          
         BE    GETLX                                                            
         CLI   0(R5),ACLELQ                                                     
         BE    *+16                                                             
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     GETL10                                                           
*                                                                               
         LR    R6,R5                                                            
         ZIC   R1,1(R6)            TOTAL EL LENGTH                              
         SH    R1,=Y(ACLLN1Q)      - CODE AND LENGTH                            
         SR    R0,R0                                                            
         SR    R3,R3                                                            
         LA    R3,L'ACLVALS(R3)    LENGTH OF EACH ACCT DESC AND LENGTH          
         DR    R0,R3                                                            
         STC   R1,NUMLEVS          SAVE NUMBER OF ACCOUNT LEVELS                
*                                                                               
         ZIC   R7,NUMLEVS                                                       
         AH    R6,=Y(ACLLN1Q)      BUMP TO FIRST LENGTH                         
         LA    R3,ABCDLEN          FIELD TO CONTAIN ALL 4 LENGTHS               
         XC    ABCDLEN,ABCDLEN                                                  
         SR    R1,R1               ACUMULATES HOW MUCH TO SUBTRACT              
         ZIC   R2,0(R6)                                                         
GETLDG5  STC   R2,0(R3)                                                         
         AR    R1,R2               ACCUMULATE LENGTHS                           
         LA    R3,1(R3)                                                         
         LA    R6,L'ACLVALS(R6)    NEXT LENGTH                                  
         ZIC   R2,0(R6)                                                         
         SR    R2,R1                                                            
         BCT   R7,GETLDG5                                                       
*                                                                               
GETLX    B     EXIT                                                             
*---------------------------------------------------------------------*         
*        DUMP OUT RECORDS                                             *         
*---------------------------------------------------------------------*         
DMPGET   NTR1                                                                   
         LA    R3,=C'GET'                                                       
         B     DUMP                                                             
                                                                                
DMPPUT   NTR1                                                                   
         LA    R3,=C'PUT'                                                       
                                                                                
         USING PERRECD,R4                                                       
DUMP     L     R4,AIO1                                                          
         CLI   QOPT7,C'D'                                                       
         BNE   XIT                                                              
         SR    R2,R2                                                            
         ICM   R2,3,PERRLEN                                                     
         GOTO1 PRNTBL,DMCB,(3,(R3)),(R4),C'DUMP',(R2),=C'2D'                    
XIT      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              ROUTINE TO DELETE AN ELEMENT                                     
*-------------------------------------------------------------------*           
DELEL    NTR1                                                                   
         L     R4,AIO1                                                          
         GOTO1 HELLO,DMCB,(C'D',ACCOUNT),(ELCODE,(R4)),0                        
         B     EXIT                                                             
*-------------------------------------------------------------------*           
*              ADD ELEMENT TO RECORD IN AIO1                          *         
*-------------------------------------------------------------------*           
*                                                                               
ADDEL    NTR1                                                                   
         L     R4,AIO1                                                          
         GOTO1 HELLO,DMCB,(C'P',ACCOUNT),(R4),(R5)                              
         B     EXIT                                                             
*                                                                               
******************************************************************              
* EXTERNAL ADDRESS LIST                                          *              
******************************************************************              
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(IO1)                                                           
         DC    A(IO2)                                                           
         DC    V(HELLO)                                                         
         DC    V(PRNTBL)                                                        
         SPACE 3                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'9000'                                                        
ACCOUNT  DC    CL8'ACCOUNT'                                                     
                                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* IO AREAS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**IO1***'                                                      
IO1      DS    CL2000                                                           
*                                                                               
         DS    0D                                                               
         DC    C'**IO2***'                                                      
IO2      DS    CL2000                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              PRINT LINE DSECT                                       *         
***********************************************************************         
*                                                                               
PLINED   DSECT                                                                  
PPERSON  DS    CL8                                                              
         DS    CL3                                                              
POFFC    DS    CL2                                                              
         DS    CL3                                                              
PDEPT    DS    CL6                                                              
         DS    CL3                                                              
PSUB     DS    CL6                                                              
         DS    CL3                                                              
PSTART   DS    CL8                                                              
         DS    CL3                                                              
PEND     DS    CL8                                                              
         DS    CL3                                                              
PSTAT    DS    CL10                                                             
         EJECT                                                                  
***********************************************************************         
*              CONVERSION TABLE                                       *         
***********************************************************************         
*                                                                               
ACXLD    DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
AIO1     DS    A                                                                
AIO2     DS    A                                                                
HELLO    DS    A                                                                
PRNTBL   DS    A                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
ANEXT    DS    A                                                                
*                                                                               
COMMAND  DS    CL8                                                              
PERSON   DS    CL8                                                              
LOCATION DS    CL8                                                              
SVDATE   DS    CL3                                                              
STDATE   DS    CL3                                                              
SVKEY    DS    CL42                                                             
SVKEY2   DS    CL42                                                             
ELEM     DS    XL255                                                            
DA       DS    XL4                                                              
NAMELN   DS    XL1                                                              
COUNT    DS    PL8                                                              
COUNTALL DS    PL8                                                              
CONAME   DS    CL7                                                              
ELCODE   DS    XL1                                                              
SVCO     DS    XL1                                                              
FIRST    DS    CL1                                                              
SEQNUM   DS    XL1                                                              
NUMLEVS  DS    XL1                                                              
ABCDLEN  DS    0CL4                                                             
LEVELNA  DS    CL1                                                              
LEVELNB  DS    CL1                                                              
LEVELNC  DS    CL1                                                              
LEVELND  DS    CL1                                                              
TMSDATE  DS    XL3                                                              
TMSLOC   DS    XL12                                                             
SVTRMDT  DS    XL3                                                              
OFFICE   DS    CL2                                                              
DEPT     DS    CL3                                                              
SUBDPT   DS    CL3                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              OTHER INCLUDES                                         *         
***********************************************************************         
*                                                                               
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086ACREPXL02S05/01/02'                                      
         END                                                                    
