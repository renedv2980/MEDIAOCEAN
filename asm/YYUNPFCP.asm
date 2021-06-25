*          DATA SET YYUNPFCP   AT LEVEL 235 AS OF 03/30/07                      
*PHASE YYUNPFCA YYUNPFCP                                                        
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DATTIM                                                                 
*INCLUDE ADDAY                                                                  
**********************************************************************          
*                                                                    *          
* COPY & REPLACE (1)AGENCY'S USER PROFILE RECS TO (N)AGENCIES        *          
* {NOTE: ONLY THE AGENCY LEVEL PROFILE/NOT USERID # ONES}            *          
*                                                                    *          
**********************************************************************          
         TITLE 'YYUNPFCP-COPY USER PROFILE FROM ONE AGENCY TO ANOTHER'          
YYUNPFCP CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**PFCP**,RA,WORK=A(WORKC),CLEAR=YES                  
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     BAS   RE,GENINIT          GENERAL INTIALISATION                        
         BAS   RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
*                                                                               
         GOTO1 VSORTER,PARM,SORTCRD1,SORTCRD2,0                                 
*                                                                               
*READ CTFILE AND PUT RECS TO SORTER, DROP TAGYLST' USER PROFILE RECORDS         
*                                                                               
         BAS   RE,READCTFL                                                      
*                                                                               
*READ SOURCE AGENCY'S USER PROFILE AND COPY TO OTHERS                           
*                                                                               
         BAS   RE,COPYPROF                                                      
*                                                                               
         BAS   RE,WRITEOUT         WRITE RECORDS FROM SORTER TO OUTPUT          
*                                                                               
MXIT     XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1                                                                   
         MVC   P(80),=CL80'GENINIT'                                             
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE JCL CARD DATA LINES AS DEFINED IN CARDTBL                  *         
***********************************************************************         
         SPACE 1                                                                
VALCARDS NTR1                                                                   
         MVC   P(80),=CL80'VALCARD'                                             
         GOTO1 VPRINTER                                                         
*                                                                               
VC10     GOTO1 VCARDS,DMCB,P,=C'RE00'                                           
         CLC   =C'/*',P            IF END OF JCL                                
         BE    VCEND                 CHECK REQUIRED CARDS INPUT                 
*                                                                               
         CLC   =C'AGY=',P          AGY=??                                       
         BNE   VC30                                                             
         MVC   SAGENCY,P+4                                                      
         B     VC10                                                             
*                                                                               
VC30     CLC   =C'TAGY=',P         TAGY=??                                      
         BNE   VC50                                                             
         LA    RF,TAGYLST                                                       
         LA    RE,TAGYLSTQ                                                      
         OC    0(L'TAGYLST,RF),0(RF)      ANY MORE EMPTY ENTRY?                 
         BZ    *+14                       YES - SAVE THE AGENCY                 
         AHI   RF,L'TAGYLST                                                     
         BCT   RE,*-14                    CHECK FOR NEXT EMPTY ENTRY            
         DC    H'0'                       EXCESS THE MAX # OF AGENCIES          
         MVC   0(L'TAGYLST,RF),P+5                                              
         B     VC10                                                             
*                                                                               
VC50     CLC   =C'DSPACE=',P       DSPACE=?                                     
         BNE   VCEND                                                            
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),P+7                                        
         B     VC10                                                             
*                                                                               
VCEND    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* READ CTFILE RECORDS AND PUT TO SORTER                               *         
***********************************************************************         
READCTFL NTR1  ,                                                                
         ZAP   LINE,=P'99'                                                      
         MVC   P(11),=C'READ CTFILE'                                            
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,FLISTCTF,IO                         
*                                                                               
         MVI   SQFLAG,0                                                         
         LA    R2,IO                                                            
         USING CTUREC,R2                                                        
         XC    CTUKEY,CTUKEY                                                    
         XC    IOKEY,IOKEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CTUKEY,CTUKEY                        
         B     RCTF030                                                          
*                                                                               
RCTF010  MVC   CTUKEY(L'CTUKEY),IOKEY                                           
         CLI   SQFLAG,0                                                         
         BE    RCTF020                                                          
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTUKEY,CTUKEY                        
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   RCTF200                                                          
         DC    H'0'                                                             
         MVI   SQFLAG,0                                                         
*                                                                               
RCTF020  EQU   *                                                                
         GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,CTUKEY,CTUKEY                        
*                                                                               
RCTF030  CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   RCTF200                                                          
         DC    H'0'                                                             
         CLC   =X'FFFFFFFF',CTUKEY        SKIP FFFFFF REC                       
         BNE   RCTF060                                                          
         SR    R1,R1                                                            
         IC    R1,IOKEY                                                         
         AHI   R1,1                                                             
         XC    CTUKEY,CTUKEY                                                    
         STC   R1,CTUKEY                                                        
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CTUKEY,CTUKEY                        
RCTF060  MVC   IOKEY(L'CTUKEY),CTUKEY                                           
*                                                                               
         MVC   P,SPACES                                                         
*                                                                               
         DROP  R2                                                               
         USING CTUREC,R2                                                        
         CLI   CTUKTYP,CTUKTYPQ                                                 
         BNE   RCTF100                                                          
*                              DROP USER PROFILE IN AGENCY LIST                 
         LA    R3,TAGYLST                                                       
RCTF070  OC    0(L'TAGYLST,R3),0(R3)    END OF LIST?                            
         BZ    RCTF080                                                          
         CLC   CTUKAGY,0(R3)                                                    
         BNE   RCTF075                                                          
         MVC   P(12),=C'DROP PROFILE'                                           
         GOTO1 VPRINTER                                                         
         BRAS  RE,PRINTREC                                                      
         B     RCTF010             SKIP THIS RECORD                             
*                                                                               
RCTF075  AHI   R3,L'TAGYLST                                                     
         B     RCTF070                                                          
RCTF080  EQU   *                                                                
         DROP  R2                                                               
*                                                                               
RCTF100  EQU   *                                                                
         BAS   RE,PUTSORT                                                       
         B     RCTF010                                                          
*                                                                               
RCTF200  MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(13),=C'END OF CTFILE'                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* READ SOURCE AGENCY'S USER PROFILE AND COPY TO OTHERS                *         
***********************************************************************         
COPYPROF NTR1                                                                   
         LA    R2,IO                                                            
         USING CTUREC,R2                                                        
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ                                                 
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CTUKEY,CTUKEY                        
*                                                                               
CPPF10   CLI   CTUKTYP,CTUKTYPQ    USER PROFILE RECORD?                         
         BNE   CPPFX               EXIT                                         
         CLC   CTUKAGY,SAGENCY                                                  
         BNE   CPPF80              NEXT RECORD                                  
         BRAS  RE,PRINTREC                                                      
*                                                                               
*OVERRIDE THE AGENCY ALPHA FOR EACH AGENCY IN THE LIST                          
*                                                                               
         LA    R3,TAGYLST                                                       
CPPF30   OC    0(L'TAGYLST,R3),0(R3)    END OF LIST?                            
         BZ    CPPF70                                                           
         MVC   CTUKAGY,0(R3)                                                    
         BAS   RE,PUTSORT                                                       
         BRAS  RE,PRINTREC                                                      
         AHI   R3,L'TAGYLST                                                     
         B     CPPF30                                                           
*                                                                               
CPPF70   MVC   CTUKAGY,SAGENCY     RESTORE THE SOURCE AGENCY                    
*                                                                               
CPPF80   GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,CTUKEY,CTUKEY                        
         B     CPPF10                                                           
         DROP  R2                                                               
*                                                                               
CPPFX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* WRITE RECORDS FROM SORTER TO OUTPUT FILE                            *         
***********************************************************************         
WRITEOUT NTR1  ,                                                                
         OPEN  (TAPEOUT,OUTPUT)                                                 
*                                                                               
WOUT010  GOTO1 VSORTER,PARM,SORTGET                                             
         ICM   R2,15,4(R1)                                                      
         BZ    WOUT100                                                          
         CLC   WORK(25),4(R2)       CHECK DUPLICATE KEY                         
         BNE   WOUT020                                                          
         BAS   RE,DUPEKEY           REPORT DUPLICATE KEY & DIFF. DATA           
         B     WOUT010                                                          
*                                                                               
WOUT020  MVC   WORK(25),4(R2)       SAVE RECORD KEY                             
         PUT   TAPEOUT,(R2)                                                     
         B     WOUT010                                                          
*                                                                               
WOUT100  CLOSE (TAPEOUT)                                                        
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(12),=C'END OF WRITE'                                           
         GOTO1 VPRINTER                                                         
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* REPORT DUPLICATE KEY                                                *         
***********************************************************************         
         SPACE 1                                                                
DUPEKEY  NTR1                                                                   
         MVC   P(24),=C'WARNING: DUPLICATE KEY: '                               
         MVC   P+24(08),11(R2)                                                  
         LA    R2,4(R2)                                                         
         GOTO1 VPRINTER                                                         
         GOTO1 VHEXOUT,PARM,(R2),P,25,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* PUT RECORD TO SORTER                                                *         
***********************************************************************         
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         SR    RE,RE                                                            
         ICM   RE,3,IO+X'19'                                                    
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL           SET TAPEOUT RECORD LENGTH                    
         GOTO1 VSORTER,PARM,SORTPUT,IOL                                         
         XIT1  ,                                                                
         SPACE 2                                                                
***********************************************************************         
* PRINT OUT A RECORD                                                            
***********************************************************************         
PRINTREC NTR1                                                                   
*                                                                               
         LR    R4,R2                                                            
         SR    R3,R3                                                            
         ICM   R3,3,25(R4)                                                      
*                                                                               
         GOTO1 VHEXOUT,DMCB,0(R4),P,25,=C'TOG'                                  
         MVC   P+54(25),0(R4)                                                   
         GOTO1 VPRINTER                                                         
         B     PR020                                                            
*                                                                               
PR020    CHI   R3,20                                                            
         BNH   PR050                                                            
*                                                                               
         GOTO1 VHEXOUT,DMCB,0(R4),P,20,=C'TOG'                                  
         MVC   P+54(20),0(R4)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         AHI   R3,-20                                                           
         AHI   R4,20                                                            
         B     PR020                                                            
*                                                                               
PR050    LTR   R3,R3                                                            
         BNP   PRX                                                              
*                                                                               
         GOTO1 VHEXOUT,DMCB,0(R4),P,(R3),=C'TOG'                                
         AHI   R3,-1                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+54(0),0(R4)                                                    
         GOTO1 VPRINTER                                                         
         BASR  RE,RF                                                            
*                                                                               
PRX      XIT1                                                                   
***********************************************************************         
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
* AGENCY DEFAULT EXCEPTIONS                                                     
*                                                                               
AGYTAB   DC    CL2'SM'                                                          
         DC    CL2'NM'                                                          
         DC    CL2'PM'                                                          
         DC    CL2'SD'                                                          
         DC    CL2'NR'                                                          
         DC    CL2'MP'                                                          
         DC    CL2'SF'                                                          
         DC    CL2'PD'                                                          
         DC    CL2'SG'                                                          
         DC    CL2'GR'                                                          
         DC    CL2'GV'                                                          
         DC    CL2'GB'                                                          
         DC    CL2'SY'                                                          
AGYTABX  DC    X'00'                                                            
         SPACE 2                                                                
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 3                                                                
UTL      DC    F'0',X'0A'          FOR DATAMGR (MUST SPECIFY SYSTEM)            
         SPACE 2                                                                
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VGETFACT DC    V(GETFACT)                                                       
VCARDS   DC    V(CARDS)                                                         
VSORTER  DC    V(SORTER)                                                        
*                                                                               
         SPACE 1                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
SORTCRD1 DC    C'SORT FIELDS=(5,25,A),FORMAT=BI,WORK=1 '                        
SORTCRD2 DC    C'RECORD TYPE=V,LENGTH=(2100,,,,) '                              
SORTPUT  DC    C'PUT'                                                           
SORTGET  DC    C'GET'                                                           
*                                                                               
FLISTCTF DC    CL8'NCTFILE '                                                    
         DC    C'X'                                                             
*                                                                               
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
         SPACE 1                                                                
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
SAGENCY  DS    CL2                 AGENCY TO BE COPIED                          
TAGYLST  DS    (TAGYLSTQ)CL2       AGENCY LIST TO BE REPLACED                   
         DS    CL2                                                              
TAGYLSTQ EQU   20                                                               
*                                                                               
SQFLAG   DS    XL1                                                              
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CT5KEY)                                                     
IOL      DS    XL4                                                              
IO       DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    (64*1024)X                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'235YYUNPFCP  03/30/07'                                      
         END                                                                    
