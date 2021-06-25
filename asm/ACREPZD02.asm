*          DATA SET ACREPZD02  AT LEVEL 097 AS OF 01/04/00                      
**********************************************************************          
*        QOPT1=C' '  DEFAULT PRINTS DETAIL LINE OF EACH MODE ONES    *          
*                    AND DOES NOT PRINT MODES IF IT IS PASSED        *          
*                    MULTIPLE TIMES.                                 *          
*        QOPT1=C'1'  PRINTS DETAIL OF EACH MODE THAT IS BEING PASSED *          
*                    I,E PRINT ALL ADDRESSES EVERY TIME A MODE IS    *          
*                    PASSED, EVEN IF IT IS PASSED MULTIPLE TIMES.    *          
*        QOPT1=C'2'  PRINT DETAILS AND ADDRESSES ONLY ONES BUT       *          
*                    DISPLAY THE NAMES OF ALL THE MODES IN THE ORDER *          
*                    THEY ARE PASSED DURING THE RUN OF A JOB.        *          
*        QOPT2=C' '  PRINT DETAILED INFO OF RECD/ELEM WHOSE ADDRESS  *          
*                    IS BEING PASSED.                                *          
*        QOPT2=C'1'  PRINT WETHER ADDRESS IS BEING PASSED OR NOT     *          
*                    NO DETAIL LINES THAT IS RECD/ELEM INFO IS       *          
*                    PRINTED.                                        *          
*                                                                    *          
**********************************************************************          
         EJECT                                                                  
*                                                                               
*PHASE ACZD02A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'PRINT WHAT YOU GET AT DIFF MODES'                               
ACZD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZD**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZDD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
         LA    R2,MDNAMTAB         POINT TO MODE NAMES                          
         USING MDNAMD,R2                                                        
PROCMD   CLI   MDNAME,X'FF'        IS IT END OF TABLE                           
         BE    EXIT                                                             
         CLC   MODE,MDNUM          IS IT IN TABLE                               
         BNE   PROCMD10                                                         
         CLI   QOPT1,C'1'          WANT TO PRINT MODES MULTIPLE TIMES           
         BE    PROCMD5                                                          
*                                                                               
         CLI   MDPRNT,X'00'        PRINT MODES ONLY ONES                        
         BE    PROCMD5                                                          
         CLI   QOPT1,C'2'          PRINT 1 DETAIL AND REST JUST NAMES           
         BNE   PROCMD10                                                         
         BAS   RE,PRNTLINE         PRINT LINE                                   
         MVC   P(L'MDNAME),MDNAME                                               
         GOTO1 ACREPORT                                                         
         BAS   RE,PRNTLINE         PRINT LINE                                   
         B     PROCMD10                                                         
*                                                                               
PROCMD5  MVI   FORCEHED,C'Y'       FORCE NEW PAGE FOR EVERY MODE                
         BAS   RE,PRNTLINE         PRINT LINE                                   
         MVC   P(L'MDNAME),MDNAME                                               
         CLC   MDNAME,=CL10'PROCSBAC'                                           
         BNE   *+10                                                             
         MVC   P+20(L'ADRSMSG),ADRSMSG                                          
*        MVC   P+20(58),=C'ADSUBAC IS A RECORD AT THIS LEVEL, ELEMENT AX        
               T OTHER LEVELS'                                                  
         GOTO1 ACREPORT                                                         
         MVC   SVMDNM,MDNAME       SAVE MODE NAME                               
         BAS   RE,PRNTLINE                                                      
*                                                                               
         BAS   RE,GTADDRS          PRINT RECDS/ELEMS AT GIVEN ADDRS             
         MVI   MDPRNT,X'01'                                                     
PROCMD10 LA    R2,MDNAMLNQ(R2)     BUMP TO NEXT TABLE ENTRY                     
         B     PROCMD              PROCESS NEXT MODE                            
*                                                                               
EXIT     XMOD1 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
         MVI   FCPRCDIR,C'Y'       PASS DIRECTORY RECORDS TO APPLIC             
         MVI   FCPRCFIL,C'Y'       PASS FILE RECORDS TO APPLIC                  
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE PRINTS RECORDS OR ELEMENTS THAT ARE PASSED BY MONACC    *          
**********************************************************************          
         SPACE 1                                                                
GTADDRS  NTR1                                                                   
         LA    R5,ADCOMP           R2=START OF ADDRESSES                        
         LA    R3,ADTAB            R3=START OF ADDRESS TABLE                    
         USING ADTABD,R3                                                        
GTADRS5  CLI   0(R3),X'FF'         IS IT END OF TAB                             
         BE    GTADDRSX                                                         
*                                                                               
         L     R2,0(R5)                                                         
         CLC   0(4,R5),=XL4'00'    IS ADDRESS PASSED TO US                      
         BE    GTADRS6                                                          
         CLI   0(R2),0      1'ST BYTE OF RECD/ELEM SHOULD NOT BE ZERO           
         BE    GTADRS6                                                          
*                                                                               
         CLI   QOPT2,C'1'          WANT DETAIL OF RECD/ELEM  C'1' NO            
         BNE   GTADRS7             PRINT DETAILS OF RECD/ELEM                   
         MVC   P(L'ADNAME),ADNAME                                               
         MVC   P+20(20),=CL20'ADDRESS PASSED'                                   
         GOTO1 ACREPORT                                                         
         B     GTADRS20                                                         
*                                                                               
GTADRS6  DS    0H                                                               
         MVC   P(L'ADNAME),ADNAME                                               
         MVC   P+20(20),=CL20'ADDRESS NOT PASSED'                               
         GOTO1 ACREPORT                                                         
         B     GTADRS20                                                         
*                                                                               
GTADRS7  DS    0H                                                               
         CLC   SVMDNM,=CL10'PROCSBAC'   IN PROCSBAC ADSUBAC IS A RECORD         
*                                  ELSEWHERE IT IS AN ELEMENT                   
         BNE   GTADRS8                                                          
         CLC   ADNAME,=CL10'ADSUBAC'                                            
         BE    *+12                                                             
*                                                                               
GTADRS8  CLI   ADTYPE,X'80'        IS IT RECORDS ADDRESS                        
         BNE   GTADRS10                                                         
*                                                                               
         MVC   MSG,ADNAME          PRINT WHAT ADD BEING PASSED                  
         SR    R6,R6                                                            
         ICM   R6,3,ACTRLEN-ACTRECD(R2)  GET LENGTH OF RECORD                   
         GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
         B     GTADRS20                                                         
*                                                                               
GTADRS10 DS    0H                                                               
         MVC   MSG,ADNAME          PRINT NAME OF AN ADDRESS                     
         ZIC   R6,1(R2)            GET LENGTH OF AN ELEMENT                     
         GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
GTADRS20 LA    R3,ADTABLNQ(R3)     BUMO TO NEXT TABLE ENTRY                     
         LA    R5,4(R5)            BUMP TO NEXT ADDRESS                         
         B     GTADRS5                                                          
GTADDRSX XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT HORIZONTAL LINE                                              *          
**********************************************************************          
PRNTLINE NTR1                                                                   
         MVI   P,C'-'                    DRAW LINE                              
         MVC   P+1(LINELNQ),P                                                   
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
ADUMP    DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
*                                                                               
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(HELLO)            HELLO MODULE                                 
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
*        CLI   QOPT7,C'Y'                                                       
*        BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
ADRSMSG  DC    C'ADSUBAC IS A RECORD AT THIS LEVEL, AND ELEMENT AT OTHEX        
               R LEVELS'                                                        
**********************************************************************          
*               ADDRESS TABLE                                        *          
**********************************************************************          
        SPACE 1                                                                 
ADTAB    DS    0H                                                               
         DC    CL10'ADCOMP',X'80'       COMPANY RECORD                          
         DC    CL10'ADUNIT',X'80'       UNIT RECORD                             
         DC    CL10'ADLEDGER',X'80'     LEDGER RECORD                           
         DC    CL10'ADHEIRA',X'80'      LEVEL 1 ACCOUNT RECORD                  
         DC    CL10'ADHEIRB',X'80'      LEVEL 2 ACCOUNT RECORD                  
         DC    CL10'ADHEIRC',X'80'      LEVEL 3 ACCOUNT RECORD                  
         DC    CL10'ADHEIRD',X'80'      LEVEL 4 ACCOUNT RECORD                  
         DC    CL10'ADACC',X'80'        CURRENT ACCOUNT RECORD                  
         DC    CL10'ADSUBAC',X'00'      SUB-ACCOUNT RECORD                      
         DC    CL10'ADTRANS',X'00'      TRANSACTION ELEMENT                     
         DC    CL10'ADCMPEL',X'00'      COMPANY ELEMENT                         
         DC    CL10'ADCMPNAM',X'00'     COMPANY NAME ELEMENT                    
         DC    CL10'ADCMPADD',X'00'     COMPANY ADDRESS ELEMENT                 
         DC    CL10'ADUNTNAM',X'00'     UNIT NAME ELEMENT                       
         DC    CL10'ADLDGEL',X'00'      LEDGER ELEMENT                          
         DC    CL10'ADLDGHIR',X'00'     LEDGER HEIRARCHY ELEMENT                
         DC    CL10'ADLDGNAM',X'00'     LEGDER NAME ELEMENT                     
         DC    CL10'ADLVANAM',X'00'     LEVEL A ACCOUNT NAME ELEMENT            
         DC    CL10'ADLVAADD',X'00'     LEVEL A ACCOUNT ADDRESS ELEMENT         
         DC    CL10'ADLVASUP',X'00'     LEVEL A ACCOUNT PROFILE ELEMENT         
         DC    CL10'ADLVASTA',X'00'     LEVEL A ACCOUNT STATUS ELEMENT          
         DC    CL10'ADLVABAL',X'00'     LEVEL A ACCOUNT BALANCE ELEMENT         
         DC    CL10'ADLVBNAM',X'00'     LEVEL B ACCOUNT NAME ELEMENT            
         DC    CL10'ADLVBADD',X'00'     LEVEL B ACCOUNT ADDRESS ELEMENT         
         DC    CL10'ADLVBSUP',X'00'     LEVEL B ACCOUNT PROFILE ELEMENT         
         DC    CL10'ADLVBSTA',X'00'     LEVEL B ACCOUNT STATUS ELEMENT          
         DC    CL10'ADLVBBAL',X'00'     LEVEL B ACCOUNT BALANCE ELEMENT         
         DC    CL10'ADLVCNAM',X'00'     LEVEL C ACCOUNT NAME ELEMENT            
         DC    CL10'ADLVCADD',X'00'     LEVEL C ACCOUNT ADDRESS ELEMENT         
         DC    CL10'ADLVCSUP',X'00'     LEVEL C ACCOUNT PROFILE ELEMENT         
         DC    CL10'ADLVCSTA',X'00'     LEVEL C ACCOUNT STATUS ELEMENT          
         DC    CL10'ADLVCBAL',X'00'     LEVEL C ACCOUNT BALANCE ELEMENT         
         DC    CL10'ADLVDNAM',X'00'     LEVEL D ACCOUNT NAME ELEMENT            
         DC    CL10'ADLVDADD',X'00'     LEVEL D ACCOUNT ADDRESS ELEMENT         
         DC    CL10'ADLVDSUP',X'00'     LEVEL D ACCOUNT PROFILE ELEMENT         
         DC    CL10'ADLVDSTA',X'00'     LEVEL D ACCOUNT STATUS ELEMENT          
         DC    CL10'ADLVDBAL',X'00'     LEVEL D ACCOUNT BALANCE ELEMENT         
         DC    CL10'ADACCNAM',X'00'     CURRENT ACCOUNT NAME ELEMENT            
         DC    CL10'ADACCADD',X'00'     CURRENT ACCOUNT ADDRESS ELEMENT         
         DC    CL10'ADACCJOB',X'00'     CURRENT ACCOUNT PROFILE ELEMENT         
         DC    CL10'ADACCSTA',X'00'     CURRENT ACCOUNT STATUS ELEMENT          
         DC    CL10'ADACCBAL',X'00'     CURRENT ACCOUNT BALANCE ELEMENT         
         DC    X'FF'                                                            
        EJECT                                                                   
**********************************************************************          
* MODE NAMES TABLE                                                   *          
**********************************************************************          
        SPACE 1                                                                 
MDNAMTAB DS    0H                                                               
*        DC    CL10'RUNFRST',AL1(1),X'00'                                       
*        DC    CL10'REQFRST',AL1(2),X'00'                                       
         DC    CL10'COMPFRST',AL1(3),X'00'                                      
         DC    CL10'UNITFRST',AL1(4),X'00'                                      
         DC    CL10'LEDGFRST',AL1(5),X'00'                                      
         DC    CL10'LEVAFRST',AL1(6),X'00'                                      
         DC    CL10'LEVBFRST',AL1(7),X'00'                                      
         DC    CL10'LEVCFRST',AL1(8),X'00'                                      
         DC    CL10'ACCFRST',AL1(9),X'00'                                       
         DC    CL10'SBACFRST',AL1(10),X'00'                                     
         DC    CL10'RUNLAST',AL1(11),X'00'                                      
         DC    CL10'REQLAST',AL1(12),X'00'                                      
         DC    CL10'COMPLAST',AL1(13),X'00'                                     
         DC    CL10'UNITLAST',AL1(14),X'00'                                     
         DC    CL10'LEDGLAST',AL1(15),X'00'                                     
         DC    CL10'LEVALAST',AL1(16),X'00'                                     
         DC    CL10'LEVBLAST',AL1(17),X'00'                                     
         DC    CL10'LEVCLAST',AL1(18),X'00'                                     
         DC    CL10'ACCLAST',AL1(19),X'00'                                      
         DC    CL10'SBACLAST',AL1(20),X'00'                                     
         DC    CL10'PROCACC',AL1(21),X'00'                                      
         DC    CL10'PROCHIST',AL1(22),X'00'                                     
         DC    CL10'PROCTRNS',AL1(23),X'00'                                     
*        DC    CL10'DISKERR',AL1(24),X'00'                                      
*        DC    CL10'GOTONXTR',AL1(25),X'00'                                     
*        DC    CL10'WRITACC',AL1(26),X'00'                                      
*        DC    CL10'WRITRANS',AL1(27),X'00'                                     
         DC    CL10'PROCSORT',AL1(28),X'00'                                     
         DC    CL10'ANALFRST',AL1(29),X'00'                                     
         DC    CL10'ANALLAST',AL1(30),X'00'                                     
         DC    CL10'PROCWORK',AL1(31),X'00'                                     
         DC    CL10'PROCLEVA',AL1(33),X'00'                                     
         DC    CL10'PROCLEVB',AL1(34),X'00'                                     
         DC    CL10'PROCLEVC',AL1(35),X'00'                                     
*        DC    CL10'WRITLEDG',AL1(36),X'00'                                     
*        DC    CL10'WRITMEDS',AL1(37),X'00'                                     
*        DC    CL10'WRITLEVA',AL1(38),X'00'                                     
*        DC    CL10'WRITLEVB',AL1(39),X'00'                                     
*        DC    CL10'WRITLEVC',AL1(40),X'00'                                     
         DC    CL10'PROCCOMM',AL1(41),X'00'                                     
         DC    CL10'PROCEST',AL1(42),X'00'                                      
         DC    CL10'LASTEST',AL1(43),X'00'                                      
         DC    CL10'PROCLEVD',AL1(44),X'00'                                     
         DC    CL10'PROCSBAC',AL1(45),X'00'                                     
         DC    CL10'PROCORD',AL1(46),X'00'                                      
         DC    CL10'PROCACL',AL1(47),X'00'                                      
         DC    CL10'OFFLAST',AL1(48),X'00'                                      
         DC    CL10'PROCOFA',AL1(49),X'00'                                      
         DC    CL10'OFALAST',AL1(50),X'00'                                      
*        DC    CL10'WRITOFA',AL1(51),X'00'                                      
         DC    CL10'OFFIRST',AL1(52),X'00'                                      
         DC    CL10'PROCCBUK',AL1(53),X'00'                                     
         DC    CL10'PROCOBUK',AL1(54),X'00'                                     
         DC    CL10'OFACFRST',AL1(55),X'00'                                     
         DC    CL10'OFACLAST',AL1(56),X'00'                                     
         DC    CL10'SCACFRST',AL1(57),X'00'                                     
         DC    CL10'SPRCTRNS',AL1(58),X'00'                                     
         DC    CL10'SCACLAST',AL1(59),X'00'                                     
         DC    CL10'SACTLAST',AL1(60),X'00'                                     
         DC    CL10'PROCTIME',AL1(61),X'00'                                     
         DC    CL10'WCTFRST',AL1(62),X'00'                                      
         DC    CL10'WCTLAST',AL1(63),X'00'                                      
         DC    CL10'PROCTRNF',AL1(249),X'00'                                    
         DC    CL10'PROCTRND',AL1(250),X'00'                                    
*        DC    CL10'PROCSPCL',AL1(251),X'00'                                    
*        DC    CL10'PROCRQST',AL1(252),X'00'                                    
*        DC    CL10'PROCSPEC',AL1(253),X'00'                                    
*        DC    CL10'PROCRCVR',AL1(254),X'00'                                    
*        DC    CL10'PROCOPTS',AL1(255),X'00'                                    
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACZDD    DSECT                                                                  
VTYPES   DS    0A                                                               
PRNTBL   DS    V                   PRINT DATA                                   
VHELLO   DS    V                   DOWNLOAD MODULE                              
VTYPLNQ  EQU   *-VTYPES                                                         
LINELNQ  EQU   90                                                               
*                                                                               
MSG      DS    CL10                DUMP MESSAGE                                 
SVMDNM   DS    CL10                SAVE MODE NAME HERE                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN MODE NAME TABLE                                  *         
***********************************************************************         
         SPACE 1                                                                
MDNAMD   DSECT                                                                  
MDNAME   DS    CL10                  MODE NAME PASSED BY MONACC                 
MDNUM    DS    AL1                   MODE NUMBER ASSOCIATED WITH NAME           
MDPRNT   DS    XL1                   PRINT MODE ONLY ONES                       
MDNAMLNQ EQU   *-MDNAMD              TRANSACTION REFERENCE                      
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ADDRESS TABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
ADTABD   DSECT                                                                  
ADNAME   DS    CL10                ADDRESS NAME                                 
ADTYPE   DS    XL1                 ADDRESS TYPE RECORD/ELEMENT                  
ADTABLNQ EQU   *-ADTABD                                                         
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
* ACBIGPRINTD                                                                   
* ACREPWORKD                                                                    
* ACGENFILE                                                                     
* ACGENMODES                                                                    
* DDLOGOD                                                                       
* ACMASTD                                                                       
* DDMASTD                                                                       
* DDBIGBOX                                                                      
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'097ACREPZD02 01/04/00'                                      
         END                                                                    
