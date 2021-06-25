*          DATA SET ACLNK11    AT LEVEL 014 AS OF 07/29/20                      
*PHASE T61F11A                                                                  
ACLNK11  TITLE '- AccPak Organizer uploads'                                     
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=CODE,REQUEST=*,SYSTEM=ACCSYSQ,LINKIO=Y,     +        
               WORKERKEY=AMUP,RLEN=500,IDF=Y,                          +        
               BLOCKS=(B#SAVED,SAVED,B#WORKD,WORKD)                             
                                                                                
CODE     NMOD1 0,**AL11**                                                       
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(global literals)                        
                                                                                
         LR    R7,R1                                                            
         USING LP_D,R7             R7=A(DDLINK control block)                   
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(save w/s)                               
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(global w/s)                             
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
         CLI   LP_CMODE,RINIREQQ   Test 'initialize' mode                       
         JNE   EXITY                                                            
                                                                                
         LA    R0,SAVED            Initialize SAVED first time through          
         LHI   R1,SAVEL                                                         
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,LP_ACOM          Extract A(LINKIO) from COMFACS               
         MVC   LINKIO,CLINKIO-COMFACSD(RF)                                      
         GOTOR (#GETLDG,AGETLDG),DMCB,LDGSJL,LDGSJ                              
         GOTOR (#GETLDG,AGETLDG),DMCB,LDGSRL,LDGSR                              
         GOTOR (#GETLDG,AGETLDG),DMCB,LDG1CL,LDG1C                              
         GOTOR (#GETLDG,AGETLDG),DMCB,LDG29L,LDG29                              
                                                                                
         L     R1,ACPYREC          Extract company status values                
         AHI   R1,CPYRFST-CPYRECD                                               
         SR    R0,R0                                                            
         USING CPYELD,R1                                                        
CODE02   CLI   CPYEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CPYEL,CPYELQ                                                     
         JE    *+14                                                             
         IC    R0,CPYLN                                                         
         AR    R1,R0                                                            
         J     CODE02                                                           
         MVC   CPYSTA6,CPYSTAT6    Set variables used by ACLDCPTR               
         MVC   CPYSTA7,CPYSTAT7                                                 
         MVC   CPYSTA9,CPYSTAT9                                                 
         MVC   CPYSTAC,CPYSTATC                                                 
         MVC   CPYCTRY,LP_CTRY                                                  
         MVC   CPYLANG,LP_LANG                                                  
         J     EXITY                                                            
                                                                                
EXITR5   CR    RE,RE               Good exit with R5 intact                     
         XIT1  REGS=(R5)                                                        
                                                                                
EXITCLR  GOTOR CLRWRK              Clear values for next upload record          
         MVI   RECFLAG,0           Reset record flag                            
                                                                                
EXITY    LHI   RF,1                Good exit from module                        
         J     EXITCC                                                           
                                                                                
EXITN    LHI   RF,2                Bad exit from module                         
                                                                                
EXITCC   CHI   RF,1                Exit setting condition code                  
                                                                                
EXIT     XIT1  ,                   Exit with condition code intact              
         EJECT                                                                  
***********************************************************************         
* Full client upload                                                  *         
***********************************************************************         
                                                                                
UCLUREC  LKREQ H,I#CFMCLU,NEXTREQ=UCLIREC,ROUTINE=UCUPL,NEWREC=Y                
                                                                                
Action   LKREQ F,001,(D,B#SAVED,C_ACTN),CHAR,TEXT=(*,RECALIT)                   
Token    LKREQ F,300,(D,B#SAVED,C_TOKN),CHAR,TEXT=(*,TOKNLIT)                   
CheckSum LKREQ F,301,(D,B#SAVED,C_CKSM),HEXD,TEXT=(*,CKSMLIT)                   
                                                                                
CliCode  LKREQ F,002,(D,B#SAVED,C_CODE),CHAR,TEXT=(*,CCODLIT)                   
CliName  LKREQ F,004,(D,B#SAVED,C_NAME),VCHR,TEXT=(*,CNAMLIT),         +        
               LOWERCASE=Y                                                      
                                                                                
SRName   LKREQ F,330,(D,B#SAVED,C_SRNAME),VCHR,TEXT=(*,SRANLIT),       +        
               LOWERCASE=Y                                                      
SRFilt1  LKREQ F,331,(D,B#SAVED,C_SRFLT1),CHAR,TEXT=(*,SRF1LIT)                 
SRFilt2  LKREQ F,332,(D,B#SAVED,C_SRFLT2),CHAR,TEXT=(*,SRF2LIT)                 
SRFilt3  LKREQ F,333,(D,B#SAVED,C_SRFLT3),CHAR,TEXT=(*,SRF3LIT)                 
SRFilt4  LKREQ F,334,(D,B#SAVED,C_SRFLT4),CHAR,TEXT=(*,SRF4LIT)                 
SRFilt5  LKREQ F,335,(D,B#SAVED,C_SRFLT5),CHAR,TEXT=(*,SRF5LIT)                 
                                                                                
1CName   LKREQ F,336,(D,B#SAVED,C_1CNAME),VCHR,TEXT=(*,OCANLIT),       +        
               LOWERCASE=Y                                                      
1CFilt1  LKREQ F,337,(D,B#SAVED,C_1CFLT1),CHAR,TEXT=(*,OCF1LIT)                 
1CFilt2  LKREQ F,338,(D,B#SAVED,C_1CFLT2),CHAR,TEXT=(*,OCF2LIT)                 
1CFilt3  LKREQ F,339,(D,B#SAVED,C_1CFLT3),CHAR,TEXT=(*,OCF3LIT)                 
1CFilt4  LKREQ F,340,(D,B#SAVED,C_1CFLT4),CHAR,TEXT=(*,OCF4LIT)                 
1CFilt5  LKREQ F,341,(D,B#SAVED,C_1CFLT5),CHAR,TEXT=(*,OCF5LIT)                 
                                                                                
NumBef   LKREQ F,014,(I,B#SAVED,A_NUM),CHAR,TEXT=(*,NBEFLIT),          +        
               ARRAY=S,OLEN=L'NUMBEF                                            
NumAft   LKREQ F,015,,CHAR,TEXT=(*,NAFTLIT),OLEN=L'NUMAFT                       
NumTyp   LKREQ F,016,(D,B#SAVED,N_NUMTYP),CHAR,TEXT=(*,NTYPLIT),       +        
               ARRAY=E,OLEN=L'NUMTYPE                                           
                                                                                
Addr1    LKREQ F,017,(D,B#SAVED,A_ADDR1),CHAR,TEXT=(*,ADR1LIT),        +        
               LOWERCASE=Y                                                      
Addr2    LKREQ F,018,(D,B#SAVED,A_ADDR2),CHAR,TEXT=(*,ADR2LIT),        +        
               LOWERCASE=Y                                                      
Addr3    LKREQ F,019,(D,B#SAVED,A_ADDR3),CHAR,TEXT=(*,ADR3LIT),        +        
               LOWERCASE=Y                                                      
Addr4    LKREQ F,020,(D,B#SAVED,A_ADDR4),CHAR,TEXT=(*,ADR4LIT),        +        
               LOWERCASE=Y                                                      
Addr5    LKREQ F,021,(D,B#SAVED,A_ADDR5),CHAR,TEXT=(*,ADR5LIT),        +        
               LOWERCASE=Y                                                      
                                                                                
OAddr1   LKREQ F,022,(D,B#SAVED,O_OADDR1),CHAR,TEXT=(*,OAD1LIT),       +        
               LOWERCASE=Y                                                      
OAddr2   LKREQ F,023,(D,B#SAVED,O_OADDR2),CHAR,TEXT=(*,OAD2LIT),       +        
               LOWERCASE=Y                                                      
OAddr3   LKREQ F,024,(D,B#SAVED,O_OADDR3),CHAR,TEXT=(*,OAD3LIT),       +        
               LOWERCASE=Y                                                      
OAddr4   LKREQ F,025,(D,B#SAVED,O_OADDR4),CHAR,TEXT=(*,OAD4LIT),       +        
               LOWERCASE=Y                                                      
OAddr5   LKREQ F,026,(D,B#SAVED,O_OADDR5),CHAR,TEXT=(*,OAD5LIT),       +        
               LOWERCASE=Y                                                      
                                                                                
CBilGrp  LKREQ F,040,(D,B#SAVED,P_BILGRP),CHAR,TEXT=(*,BGRPLIT)                 
CRcvblCo LKREQ F,041,(D,B#SAVED,TRASH1),HEXD,TEXT=(*,TRASHLIT)                  
CRcvblUL LKREQ F,042,(D,B#SAVED,TRASH2),CHAR,TEXT=(*,TRASHLIT)                  
CRcvblAc LKREQ F,044,(D,B#SAVED,P_RECVBL),CHAR,TEXT=(*,RECVLIT)                 
CCostCo  LKREQ F,045,(D,B#SAVED,TRASH1),HEXD,TEXT=(*,TRASHLIT)                  
CCostUL  LKREQ F,046,(D,B#SAVED,TRASH2),CHAR,TEXT=(*,TRASHLIT)                  
CCostAc  LKREQ F,048,(D,B#SAVED,P_COSTAC),CHAR,TEXT=(*,COSTLIT)                 
CGAOffic LKREQ F,049,(D,B#SAVED,P_GAOFF),CHAR,TEXT=(*,GAOFLIT)                  
CBilType LKREQ F,050,(D,B#SAVED,P_BILTYP),CHAR,TEXT=(*,BTYPLIT)                 
CBilAmt  LKREQ F,051,(D,B#SAVED,P_BILAMT),LBIN,TEXT=(*,BAMTLIT)                 
CUforA   LKREQ F,052,(D,B#SAVED,P_UFORA),CHAR,TEXT=(*,UFRALIT)                  
CUblblWC LKREQ F,053,(D,B#SAVED,P_UWORK),CHAR,TEXT=(*,UWRKLIT)                  
CBilPrt  LKREQ F,054,(D,B#SAVED,P_BILPRT),CHAR,TEXT=(*,BPRTLIT),       +        
               LOWERCASE=Y                                                      
CBilNar1 LKREQ F,055,(D,B#SAVED,P_BILNR1),CHAR,TEXT=(*,BNR1LIT),       +        
               LOWERCASE=Y                                                      
CBilNar2 LKREQ F,056,(D,B#SAVED,P_BILNR2),CHAR,TEXT=(*,BNR2LIT),       +        
               LOWERCASE=Y                                                      
CBilNar3 LKREQ F,057,(D,B#SAVED,P_BILNR3),CHAR,TEXT=(*,BNR3LIT),       +        
               LOWERCASE=Y                                                      
                                                                                
CSecNum  LKREQ F,060,(D,B#SAVED,S_RSECY),LBIN,TEXT=(*,SECYLIT)                  
CFilt1   LKREQ F,061,(D,B#SAVED,S_RFILT1),CHAR,TEXT=(*,FLT1LIT)                 
CFilt2   LKREQ F,062,(D,B#SAVED,S_RFILT2),CHAR,TEXT=(*,FLT2LIT)                 
CFilt3   LKREQ F,063,(D,B#SAVED,S_RFILT3),CHAR,TEXT=(*,FLT3LIT)                 
CFilt4   LKREQ F,064,(D,B#SAVED,S_RFILT4),CHAR,TEXT=(*,FLT4LIT)                 
CFilt5   LKREQ F,065,(D,B#SAVED,S_RFILT5),CHAR,TEXT=(*,FLT5LIT)                 
CCstCtr  LKREQ F,066,(D,B#SAVED,S_RCCTR),LBIN,TEXT=(*,CCTRLIT)                  
CSt1X80  LKREQ F,069,(D,B#SAVED,S_RSTAT1),MB80,TEXT=(*,S180LIT)                 
CSt1X40  LKREQ F,070,(D,B#SAVED,S_RSTAT1),MB40,TEXT=(*,S140LIT)                 
CSt1X20  LKREQ F,071,(D,B#SAVED,S_RSTAT1),MB20,TEXT=(*,S120LIT)                 
CSt1X10  LKREQ F,072,(D,B#SAVED,S_RSTAT1),MB10,TEXT=(*,S110LIT)                 
CSt1X08  LKREQ F,073,(D,B#SAVED,S_RSTAT1),MB08,TEXT=(*,S108LIT)                 
CSt1X04  LKREQ F,074,(D,B#SAVED,S_RSTAT1),MB04,TEXT=(*,S104LIT)                 
CSt1X02  LKREQ F,075,(D,B#SAVED,S_RSTAT1),MB02,TEXT=(*,S102LIT)                 
CSt1X01  LKREQ F,076,(D,B#SAVED,S_RSTAT1),MB01,TEXT=(*,S101LIT)                 
CSt2X80  LKREQ F,077,(D,B#SAVED,S_RSTAT2),MB80,TEXT=(*,S280LIT)                 
CSt2X40  LKREQ F,078,(D,B#SAVED,S_RSTAT2),MB40,TEXT=(*,S240LIT)                 
CSt2X20  LKREQ F,079,(D,B#SAVED,S_RSTAT2),MB20,TEXT=(*,S220LIT)                 
CSt2X10  LKREQ F,080,(D,B#SAVED,S_RSTAT2),MB10,TEXT=(*,S210LIT)                 
CSt2X08  LKREQ F,081,(D,B#SAVED,S_RSTAT2),MB08,TEXT=(*,S208LIT)                 
CSt2X04  LKREQ F,082,(D,B#SAVED,S_RSTAT2),MB04,TEXT=(*,S204LIT)                 
CSt2X02  LKREQ F,083,(D,B#SAVED,S_RSTAT2),MB02,TEXT=(*,S202LIT)                 
CSt2X01  LKREQ F,084,(D,B#SAVED,S_RSTAT2),MB01,TEXT=(*,S201LIT)                 
CSt3X80  LKREQ F,085,(D,B#SAVED,S_RSTAT3),MB80,TEXT=(*,S380LIT)                 
CSt3X40  LKREQ F,086,(D,B#SAVED,S_RSTAT3),MB40,TEXT=(*,S340LIT)                 
CSt3X20  LKREQ F,087,(D,B#SAVED,S_RSTAT3),MB20,TEXT=(*,S320LIT)                 
CSt3X10  LKREQ F,088,(D,B#SAVED,S_RSTAT3),MB10,TEXT=(*,S310LIT)                 
CSt3X08  LKREQ F,089,(D,B#SAVED,S_RSTAT3),MB08,TEXT=(*,S308LIT)                 
CSt3X04  LKREQ F,090,(D,B#SAVED,S_RSTAT3),MB04,TEXT=(*,S304LIT)                 
CSt3X02  LKREQ F,091,(D,B#SAVED,S_RSTAT3),MB02,TEXT=(*,S302LIT)                 
CSt3X01  LKREQ F,092,(D,B#SAVED,S_RSTAT3),MB01,TEXT=(*,S301LIT)                 
CSt4X80  LKREQ F,093,(D,B#SAVED,S_RSTAT4),MB80,TEXT=(*,S480LIT)                 
CSt4X40  LKREQ F,094,(D,B#SAVED,S_RSTAT4),MB40,TEXT=(*,S440LIT)                 
CSt4X20  LKREQ F,095,(D,B#SAVED,S_RSTAT4),MB20,TEXT=(*,S420LIT)                 
CSt4X10  LKREQ F,096,(D,B#SAVED,S_RSTAT4),MB10,TEXT=(*,S410LIT)                 
CSt4X08  LKREQ F,097,(D,B#SAVED,S_RSTAT4),MB08,TEXT=(*,S408LIT)                 
CSt4X04  LKREQ F,098,(D,B#SAVED,S_RSTAT4),MB04,TEXT=(*,S404LIT)                 
CSt4X02  LKREQ F,099,(D,B#SAVED,S_RSTAT4),MB02,TEXT=(*,S402LIT)                 
CSt4X01  LKREQ F,100,(D,B#SAVED,S_RSTAT4),MB01,TEXT=(*,S401LIT)                 
CSt5X80  LKREQ F,101,(D,B#SAVED,S_RSTAT5),MB80,TEXT=(*,S580LIT)                 
CSt5X40  LKREQ F,102,(D,B#SAVED,S_RSTAT5),MB40,TEXT=(*,S540LIT)                 
CSt5X20  LKREQ F,103,(D,B#SAVED,S_RSTAT5),MB20,TEXT=(*,S520LIT)                 
CSt5X10  LKREQ F,104,(D,B#SAVED,S_RSTAT5),MB10,TEXT=(*,S510LIT)                 
CSt5X08  LKREQ F,105,(D,B#SAVED,S_RSTAT5),MB08,TEXT=(*,S508LIT)                 
CSt5X04  LKREQ F,106,(D,B#SAVED,S_RSTAT5),MB04,TEXT=(*,S504LIT)                 
CSt5X02  LKREQ F,107,(D,B#SAVED,S_RSTAT5),MB02,TEXT=(*,S502LIT)                 
CSt5X01  LKREQ F,108,(D,B#SAVED,S_RSTAT5),MB01,TEXT=(*,S501LIT)                 
CSt6X80  LKREQ F,109,(D,B#SAVED,S_RSTAT6),MB80,TEXT=(*,S680LIT)                 
CSt6X40  LKREQ F,110,(D,B#SAVED,S_RSTAT6),MB40,TEXT=(*,S640LIT)                 
CSt6X20  LKREQ F,111,(D,B#SAVED,S_RSTAT6),MB20,TEXT=(*,S620LIT)                 
CSt6X10  LKREQ F,112,(D,B#SAVED,S_RSTAT6),MB10,TEXT=(*,S610LIT)                 
CSt6X08  LKREQ F,113,(D,B#SAVED,S_RSTAT6),MB08,TEXT=(*,S608LIT)                 
CSt6X04  LKREQ F,114,(D,B#SAVED,S_RSTAT6),MB04,TEXT=(*,S604LIT)                 
CSt6X02  LKREQ F,115,(D,B#SAVED,S_RSTAT6),MB02,TEXT=(*,S602LIT)                 
CSt6X01  LKREQ F,116,(D,B#SAVED,S_RSTAT6),MB01,TEXT=(*,S601LIT)                 
CCCSTR   LKREQ F,117,(D,B#SAVED,S_RCSTR),LBIN,TEXT=(*,CSTRLIT)                  
CBBFDt   LKREQ F,118,(D,B#SAVED,S_RBBFD),PDAT,TEXT=(*,BBFDLIT)                  
CLTDate  LKREQ F,119,(D,B#SAVED,S_RLTDT),PDAT,TEXT=(*,LTDTLIT)                  
CCostGp  LKREQ F,120,(D,B#SAVED,S_RCSTG),CHAR,TEXT=(*,CSGPLIT)                  
CSMedCd  LKREQ F,121,(D,B#SAVED,S_RSMDC),CHAR,TEXT=(*,SMEDLIT)                  
COffice  LKREQ F,122,(D,B#SAVED,S_ROFFC),CHAR,TEXT=(*,OFFCLIT)                  
CX1099   LKREQ F,123,(D,B#SAVED,S_RX1099),LBIN,TEXT=(*,X109LIT)                 
CDftTsk  LKREQ F,124,(D,B#SAVED,S_RDFTTT),CHAR,TEXT=(*,DFSKLIT)                 
CMail    LKREQ F,125,(D,B#SAVED,S_RMAIL),LBIN,TEXT=(*,MAILLIT)                  
CLBilDt  LKREQ F,127,(D,B#SAVED,S_RLBILD),LBIN,TEXT=(*,LBDTLIT)                 
CLSTX80  LKREQ F,128,(D,B#SAVED,S_RLSTAT),MB80,TEXT=(*,LS80LIT)                 
CLSTX40  LKREQ F,129,(D,B#SAVED,S_RLSTAT),MB40,TEXT=(*,LS40LIT)                 
CLSTX20  LKREQ F,130,(D,B#SAVED,S_RLSTAT),MB20,TEXT=(*,LS20LIT)                 
CLSTX10  LKREQ F,131,(D,B#SAVED,S_RLSTAT),MB10,TEXT=(*,LS10LIT)                 
CLSTX08  LKREQ F,132,(D,B#SAVED,S_RLSTAT),MB08,TEXT=(*,LS08LIT)                 
CLSTX04  LKREQ F,133,(D,B#SAVED,S_RLSTAT),MB04,TEXT=(*,LS04LIT)                 
CLSTX02  LKREQ F,134,(D,B#SAVED,S_RLSTAT),MB02,TEXT=(*,LS02LIT)                 
CLSTX01  LKREQ F,135,(D,B#SAVED,S_RLSTAT),MB01,TEXT=(*,LS01LIT)                 
                                                                                
PersnC1  LKREQ F,140,(D,B#SAVED,P_PERS),CHAR,TEXT=(*,PPERLIT)                   
LstAct1  LKREQ F,141,(D,B#SAVED,P_ADAT),PDAT,TEXT=(*,PDATLIT)                   
PersnC2  LKREQ F,142,(D,B#SAVED,P_PER2),CHAR,TEXT=(*,PPR2LIT)                   
LstAct2  LKREQ F,143,(D,B#SAVED,P_ADAT2),PDAT,TEXT=(*,PPD2LIT)                  
                                                                                
SeqNum   LKREQ F,160,(I,B#SAVED,A_SCM),UBIN,TEXT=(*,SSEQLIT),ARRAY=S,  +        
               OLEN=L'S_SEQ                                                     
PrtOnBil LKREQ F,161,,MB80,TEXT=(*,POBLLIT),OLEN=0                              
PrtOnEst LKREQ F,162,,MB40,TEXT=(*,POESLIT),OLEN=0                              
SeqAmtI  LKREQ F,163,,MB20,TEXT=(*,SAMTLIT),OLEN=0                              
OMatCom  LKREQ F,164,,MB10,TEXT=(*,OMATLIT),OLEN=0                              
PrtBef   LKREQ F,165,,MB08,TEXT=(*,PBEFLIT),OLEN=0                              
PrtAft   LKREQ F,166,,MB04,TEXT=(*,PAFTLIT),OLEN=0                              
POFootLn LKREQ F,167,,MB02,TEXT=(*,POFTLIT),OLEN=0                              
OrdPrtX  LKREQ F,168,,MB01,TEXT=(*,ORDXLIT),OLEN=1                              
ScmCdNm  LKREQ F,169,,CHAR,TEXT=(*,SCMCLIT),ARRAY=E,OLEN=L'S_CDTX,     +        
               DELIM=X'FF'                                                      
                                                                                
OnlnMemo LKREQ F,180,(D,B#SAVED,O_MEMO),VCHR,TEXT=(*,OMEMLIT),         +        
               LOWERCASE=Y                                                      
                                                                                
FBill    LKREQ F,210,(D,B#SAVED,M_FBILL),CHAR,TEXT=(*,LBILLIT)                  
LBill    LKREQ F,211,(D,B#SAVED,M_LBILL),CHAR,TEXT=(*,FBILLIT)                  
RBill    LKREQ F,212,(D,B#SAVED,M_RBILL),CHAR,TEXT=(*,RBILLIT)                  
                                                                                
FFTType  LKREQ F,220,(I,B#SAVED,A_FFT),UBIN,OLEN=L'F_TYPE,             +        
               TEXT=(*,FTYPLIT),ARRAY=S                                         
FFTSeq#  LKREQ F,221,,LBIN,OLEN=L'F_SEQ,TEXT=(*,FSEQLIT)                        
FFTData  LKREQ F,222,,CHAR,OLEN=L'F_DATA,TEXT=(*,FTXTLIT),             +        
               LOWERCASE=Y,ARRAY=E                                              
                                                                                
         LKREQ E                                                                
                                                                                
UCUPL    GOTOR SETELS,CPETAB       Call element upload routines                 
                                                                                
         OC    M_PMDEL,M_PMDEL     Test media element fields present            
         JZ    UCUPL02                                                          
         GOTOR PMDUPL              Yes - initialize PMD element                 
                                                                                
UCUPL02  XC    APUTRTN,APUTRTN     Set no put routine                           
         J     CUPL0010            Process upload data                          
         EJECT                                                                  
***********************************************************************         
* Client part of elemental client upload                              *         
***********************************************************************         
                                                                                
UCLIREC  LKREQ H,I#CFMCU,NEXTREQ=UPRUREC,ROUTINE=CUPL,NEWREC=Y                  
                                                                                
Action   LKREQ F,001,(D,B#SAVED,C_ACTN),CHAR,TEXT=(*,RECALIT)                   
Token    LKREQ F,002,(D,B#SAVED,C_TOKN),CHAR,TEXT=(*,TOKNLIT)                   
CheckSum LKREQ F,003,(D,B#SAVED,C_CKSM),HEXD,TEXT=(*,CKSMLIT)                   
                                                                                
CliCode  LKREQ F,004,(D,B#SAVED,C_CODE),CHAR,TEXT=(*,CCODLIT)                   
CliName  LKREQ F,005,(D,B#SAVED,C_NAME),VCHR,TEXT=(*,CNAMLIT),         +        
               LOWERCASE=Y                                                      
                                                                                
SRName   LKREQ F,006,(D,B#SAVED,C_SRNAME),VCHR,TEXT=(*,SRANLIT),       +        
               LOWERCASE=Y                                                      
SRFilt1  LKREQ F,007,(D,B#SAVED,C_SRFLT1),CHAR,TEXT=(*,SRF1LIT)                 
SRFilt2  LKREQ F,008,(D,B#SAVED,C_SRFLT2),CHAR,TEXT=(*,SRF2LIT)                 
SRFilt3  LKREQ F,009,(D,B#SAVED,C_SRFLT3),CHAR,TEXT=(*,SRF3LIT)                 
SRFilt4  LKREQ F,010,(D,B#SAVED,C_SRFLT4),CHAR,TEXT=(*,SRF4LIT)                 
SRFilt5  LKREQ F,011,(D,B#SAVED,C_SRFLT5),CHAR,TEXT=(*,SRF5LIT)                 
                                                                                
1CName   LKREQ F,012,(D,B#SAVED,C_1CNAME),VCHR,TEXT=(*,OCANLIT),       +        
               LOWERCASE=Y                                                      
1CFilt1  LKREQ F,013,(D,B#SAVED,C_1CFLT1),CHAR,TEXT=(*,OCF1LIT)                 
1CFilt2  LKREQ F,014,(D,B#SAVED,C_1CFLT2),CHAR,TEXT=(*,OCF2LIT)                 
1CFilt3  LKREQ F,015,(D,B#SAVED,C_1CFLT3),CHAR,TEXT=(*,OCF3LIT)                 
1CFilt4  LKREQ F,016,(D,B#SAVED,C_1CFLT4),CHAR,TEXT=(*,OCF4LIT)                 
1CFilt5  LKREQ F,017,(D,B#SAVED,C_1CFLT5),CHAR,TEXT=(*,OCF5LIT)                 
                                                                                
         LKREQ E                                                                
                                                                                
CUPL     LARL  RE,CPUT             Set address of record put routine            
         ST    RE,APUTRTN                                                       
                                                                                
K        USING ACTRECD,IOKEY                                                    
CUPL0010 MVC   K.ACTKEY,SPACES     Build key of client record                   
         MVC   K.ACTKCPY,LP_AGYB                                                
         MVC   K.ACTKUNT(L'ACTKUNT+L'ACTKLDG),LDGSJL                            
         MVC   K.ACTKACT(L'C_CODE),C_CODE                                       
                                                                                
CUPL0012 ICM   R1,15,A_FFT         Upload free-form text array                  
         JZ    CUPL0030                                                         
         LLH   R0,LW_NUMN-LW_D(R1)                                              
         AHI   R1,LW_LN2Q                                                       
CUPL0020 MVC   F_TYPE(L'F_TYPE+L'F_SEQ),0(R1)                                   
         MVC   F_DATA,L'F_TYPE+L'F_SEQ(R1)                                      
         GOTOR FFTUPL              Add to stack                                 
         AHI   R1,L'F_SEQ+L'F_TYPE+L'F_DATA                                     
         JCT   R0,CUPL0020                                                      
                                                                                
CUPL0030 ICM   R1,15,A_NUM         Upload bill number array                     
         JZ    CUPL0050                                                         
         OC    M_PMDEL,M_PMDEL     Test PMD fields uploaded                     
         JZ    *+6                                                              
         DC    H'0'                Yes - you can't have both                    
         LLH   R0,LW_NUMN-LW_D(R1)                                              
         AHI   R1,LW_LN2Q                                                       
CUPL0040 MVC   N_NUMBEF(L'N_NUMBEF+L'N_NUMAFT+L'N_NUMTYP),0(R1)                 
         GOTOR NUMUPL              Add to stack                                 
         AHI   R1,L'N_NUMBEF+L'N_NUMAFT+L'N_NUMTYP                              
         JCT   R0,CUPL0040                                                      
                                                                                
CUPL0050 ICM   R1,15,A_SCM         Upload standard comment array                
         JZ    CUPL0070                                                         
         LLH   R0,LW_NUMN-LW_D(R1)                                              
         AHI   R1,LW_LN2Q                                                       
CUPL0060 MVC   S_SEQ(L'S_SEQ+L'S_TYPE+L'S_CDTX),0(R1)                           
         GOTOR SCMUPL              Add to stack                                 
         AHI   R1,L'S_SEQ+L'S_TYPE+L'S_CDTX                                     
         JCT   R0,CUPL0060                                                      
                                                                                
CUPL0070 LLC   R0,C_NAME           Get length of SJ name                        
         CHI   R0,1                Have name?                                   
         JH    CUPL0074                                                         
         CLC   LP_QMAPN,=AL2(I#CFMCLU)       Full clt upload?                   
         JE    CUPL0072                                                         
         CLC   LP_QMAPN,=AL2(I#CFMCU)        Clt upload?                        
         JE    CUPL0072                                                         
         CLC   LP_QMAPN,=AL2(I#CFMPRU)       Full prd upload?                   
         JE    CUPL0073                                                         
         CLC   LP_QMAPN,=AL2(I#CFMPU)        Prd upload?                        
         JE    CUPL0073                                                         
         J     CUPL0076                      Leave name field empty             
                                                                                
CUPL0072 MVC   C_NAME+1(L'C_CODE),C_CODE     Default to clt code                
         LA    R0,L'C_CODE                   Set length to clt code             
         J     CUPL0074                                                         
CUPL0073 MVC   C_NAME+1(L'P_CODE),P_CODE     Default to prd code                
         LA    R0,L'P_CODE                   Set length to prd code             
                                                                                
CUPL0074 MVI   C_NAMEL,NAMELQ      Complete SJ name element                     
         AHI   R0,NAMEREC-NAMELD                                                
         STC   R0,C_NAME                                                        
                                                                                
CUPL0076 GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOACCDIR+IO2'                         
         MVC   SVDIR,IOKEY         Save actual key of record                    
         JNE   CUPL0090                                                         
         CLI   C_ACTN,ACTNADDQ     Test adding a record                         
         JNE   CUPL0080                                                         
         GOTOR ERROR,DMCB,AE$RECAE,(14,K.ACTKUNT)                               
                                                                                
CUPL0080 CLI   C_ACTN,ACTNRESQ     Test restoring a record                      
         JNE   CUPL0210                                                         
         GOTOR ERROR,DMCB,AE$RECND,(14,K.ACTKUNT)                               
                                                                                
CUPL0090 TM    IOERR,IOEDEL        Test directory record is deleted             
         JNZ   CUPL0100                                                         
         TM    IOERR,IOERNF        Test record not found                        
         JNZ   *+6                                                              
         DC    H'0'                No - can't handle other errors               
         XC    IOKEY,IOKEY         Build basic directory record                 
         MVC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         CLI   C_ACTN,ACTNCHAQ     Test changing a record                       
         JNE   CUPL0200                                                         
         GOTOR ERROR,DMCB,AE$RECNF,(14,K.ACTKUNT)                               
         J     CUPL0200                                                         
                                                                                
CUPL0100 CLI   C_ACTN,ACTNRESQ     Can only restore a deleted record            
         JE    *+6                                                              
         DC    H'0'                                                             
         NI    K.ACTRSTAT,FF-(ACTSDELT)                                         
         J     CUPL0210                                                         
                                                                                
CUPL0200 L     R1,AIO2                                                          
         GOTOR INIREC              Build basic client record                    
         J     CUPLX                                                            
                                                                                
CUPL0210 GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOACCMST+IO2'                        
         JE    CUPLX                                                            
         TM    IOERR,IOEDEL        Test record is deleted                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XC    K.ACTKSTA,K.ACTKSTA                                              
         L     R1,AIO2                                                          
         GOTOR INIREC              Build basic client record                    
                                                                                
CUPLX    OC    APUTRTN,APUTRTN     Test via full upload                         
         JZ    CPUT                Yes - upload record now                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Client/product record maintanance                                   *         
***********************************************************************         
                                                                                
         USING CPWORKD,RC          RC=A(local storage)                          
PPUT     DS    0H                                                               
CPUT     SR    R0,R0               Validate SR changes                          
         ICM   R0,1,C_SRNAME       Get/test length of SR name                   
         JZ    *+16                                                             
         MVI   C_SRNAML,NAMELQ     Complete SR name element                     
         AHI   R0,NAMEREC-NAMELD                                                
         STC   R0,C_SRNAME                                                      
                                                                                
         OC    P_RECVBL,P_RECVBL   Validate SR a/c if code/name given           
         JNZ   *+14                                                             
         OC    C_SRNAML(C_SRLENQ),C_SRNAML                                      
         JZ    CPUT0010                                                         
         GOTOR GETACT,DMCB,(5,P_RECV),C_SRNAML,LDGSR,FLAGSR                     
                                                                                
CPUT0010 SR    R0,R0               Validate 1C changes                          
         ICM   R0,1,C_1CNAME       Get/test length of 1C name                   
         JZ    CPUT0020                                                         
         MVI   C_1CNAML,NAMELQ     Complete 1C name element                     
         AHI   R0,NAMEREC-NAMELD                                                
         STC   R0,C_1CNAME                                                      
                                                                                
CPUT0020 OC    C_1CNAML(C_1CLENQ),C_1CNAML                                      
         JNZ   *+14                                                             
         OC    P_COSTAC,P_COSTAC                                                
         JZ    CPUT0030                                                         
         GOTOR GETACT,DMCB,(6,P_COST),C_1CNAML,LDG1C,FLAG1C                     
                                                                                
         MVC   C_ANACOD,P_COST                                                  
         MVC   C_ANACOD+(ACTKUNT-ACTRECD)(L'LDG29L),LDG29L                      
         MVC   C_ANANAM,C_1CNAML                                                
         GOTOR GETACT,DMCB,(7,C_ANACOD),C_ANANAM,LDG29,FLAG29                   
                                                                                
CPUT0030 L     R2,AIO2             Point to old record                          
         USING ACTRECD,R2                                                       
                                                                                
         TM    RECFLAG,RECFUERR    Exit if any errors                           
         JNZ   EXITCLR                                                          
                                                                                
         L     R2,AIO2             Point to old record                          
         USING ACTRECD,R2                                                       
         LA    R3,ACTRFST          Point to first element on old record         
                                                                                
         GOTOR INIPTR              Initialize pointer element                   
                                                                                
         CLI   C_ACTN,ACTNCHAQ     Are we changing a record?                    
         JNE   CADD                                                             
                                                                                
N        USING ACTRECD,CPIO                                                     
         MVC   N.ACTRECD(ACTRFST-ACTRECD),ACTRECD                               
         LA    R5,N.ACTRFST        Point to first element on new record         
                                                                                
CPUT0040 CLI   0(R3),0             Test end of old record                       
         JE    CPUT0220                                                         
                                                                                
         CLI   0(R3),PTRELQ        Test pointer element                         
         JNE   CPUT0050            No                                           
         ST    R3,APTRELO          Save A(existing PTR element)                 
         USING PTRELD,R3                                                        
K        USING RAPRECD,PTRCODE                                                  
         MVC   SVRDT,K.RAPKDATE    Save old pointer date/time                   
         XC    K.RAPKDATE(L'RAPKDATE+L'RAPKTIME),K.RAPKDATE                     
         DROP  K,R3                                                             
                                                                                
CPUT0050 MVI   SVELEFLG,0          Init saved element flag                      
         LA    R1,CPETAB           Point to record element table                
         USING ELETABD,R1                                                       
         BASR  RE,0                                                             
         CLI   ELETABD,ELETEOTQ    Test end of regular element table            
         JE    CPUT0080                                                         
         CLC   ELETELE,0(R3)       Match element code to list                   
         JE    *+10                                                             
         AHI   R1,ELETABL          Bump to next table entry                     
         BR    RE                                                               
         LLH   RE,ELETSDSP                                                      
         LA    RE,SAVED(RE)        Point to element in SAVED                    
         MVC   SVELEFLG,ELETFLAG   Save element flag                            
         DROP  R1                                                               
                                                                                
         CLI   0(RE),0             Test this element was uploaded               
         JE    CPUT0080            No - copy it from old record                 
         CLC   1(1,RE),1(R3)       Test same element length                     
         JNE   CPUT0060                                                         
         LLC   R1,1(RE)            Get compare length                           
         BCTR  R1,0                                                             
         LARL  RB,CPUTCLC1         Match new element to old                     
         EX    R1,0(RB)            Compare old/new elements                     
         JNE   CPUT0060            No change - take old one                     
         MVI   0(RE),0             Set this element was used                    
         J     CPUT0070                                                         
                                                                                
CPUT0060 LR    R1,RE               Point to new element                         
         GOTOR MOVEEL              Move new to record                           
         MVI   0(R1),FF            Set this element was used                    
         J     CPUT0200                                                         
                                                                                
CPUT0070 GOTOR COPYEL              Copy old to new                              
         J     CPUT0200                                                         
                                                                                
CPUT0080 CLI   0(R3),SCMELQ        Deal with comment elements                   
         JNE   CPUT0100                                                         
         OC    S_CDTX,S_CDTX       Any comments present?                        
         JZ    CPUT0200            no, bump to next element                     
         GOTOR MOVEELS             Move comment element from the stack          
         JNE   CPUT0180                                                         
         J     CPUT0170                                                         
                                                                                
CPUT0100 CLI   0(R3),FFTELQ        Deal with free form text elements            
         JNE   CPUT0120                                                         
         GOTOR MOVEELS                                                          
         JE    CPUT0170                                                         
         J     CPUT0180                                                         
                                                                                
CPUT0120 CLI   0(R3),NUMELQ        Deal with number elements                    
         JNE   CPUT0140                                                         
         GOTOR MOVEELS             Move stacked elements                        
         JE    CPUT0170                                                         
         OC    M_PMDEL,M_PMDEL     Test PMD element uploaded                    
         JZ    CPUT0180            No - copy this element                       
         J     CPUT0170            Bump over the NUMELs on the record           
                                                                                
CPUT0140 CLI   0(R3),PMDELQ        Deal with production media elements          
         JNE   CPUT0180                                                         
         OC    A_NUM,A_NUM         Test NUMELs are present                      
         JNZ   CPUT0200            Yes - this element can be dropped            
         OC    M_PMDEL,M_PMDEL     Test this element was uploaded               
         JZ    CPUT0200            No - drop it                                 
         GOTOR MOVEEL,M_PMDEL      Move in new element                          
         MVI   M_PMDEL,FF          Set this element has been used               
         J     CPUT0200            Bump to next input element                   
                                                                                
CPUT0170 LLC   R0,1(R3)            Bump over the input stacked elements         
         AR    R3,R0                                                            
         CLC   BYTE1,0(R3)         Test end of these elements                   
         JNE   CPUT0040            Yes - process next element                   
         J     CPUT0170            Else bump to next and test again             
                                                                                
CPUT0180 TM    SVELEFLG,ELETDNCQ   Copy this element?                           
         JNZ   CPUT0200                                                         
         GOTOR COPYEL              Copy other unknown elements                  
                                                                                
CPUT0200 LLC   R0,1(R3)            Bump to next input element                   
         AR    R3,R0                                                            
         J     CPUT0040                                                         
                                                                                
CPUT0220 LA    R6,CPETAB           Point to standard element table              
         USING ELETABD,R6                                                       
CPUT0230 CLI   ELETABD,ELETEOTQ    Test all entries processed                   
         JE    CPUT0240                                                         
         LLH   R1,ELETSDSP                                                      
         LA    R1,SAVED(R1)        Point to element in SAVED                    
         TM    0(R1),FF            Test element present/used                    
         JNM   CPUT0232                                                         
         GOTOR MOVEEL              Yes - add it                                 
         MVI   0(R1),FF            Flag element as used                         
CPUT0232 AHI   R6,ELETABL          Bump to next table entry                     
         J     CPUT0230                                                         
         DROP  R6                                                               
                                                                                
CPUT0240 GOTOR MOVEEL,M_PMDEL      Add PMD element if necessary                 
                                                                                
         SR    R0,R0               Test anything in stack                       
         ICM   R0,3,STACKL                                                      
         JZ    CPUT0250                                                         
         L     R1,AIO8                                                          
CPUT0242 GOTOR MOVEEL              Move stacked elements one at a time          
         AR    R1,RF               Point to next element                        
         SR    R0,RF               Decrement total length                       
         JNZ   CPUT0242            Until exhausted                              
         XC    STACKL,STACKL       Clear element stack                          
                                                                                
CPUT0250 MVI   0(R5),0             Set record terminator                        
         AHI   R5,1                Bump over terminator                         
         LA    R0,N.ACTRECD                                                     
         SR    R5,R0               Calculate record length                      
         STCM  R5,3,N.ACTRLEN      Set record length                            
         GOTOR SETACS,N.ACTRECD    Build account status area                    
                                                                                
         CLC   ACTRLEN,N.ACTRLEN   Test any change to the record                
         JNE   CPUT0260                                                         
         LLH   R1,ACTRLEN                                                       
         LLH   RF,N.ACTRLEN                                                     
         LA    R0,ACTRECD                                                       
         LA    RE,N.ACTRECD                                                     
         CLCL  R0,RE                                                            
         JE    CPUT0310            No changes made - no I/O to file             
                                                                                
         ICM   R3,15,APTRELO       Point to old pointer element                 
         JZ    CPUT0260                                                         
         USING PTRELD,R3           Restore saved date/time                      
K        USING RAPRECD,PTRCODE                                                  
         MVC   K.RAPKDATE(L'RAPKDATE+L'RAPKTIME),SVRDT                          
         DROP  K,R3                                                             
         GOTOR BLDPAS,DMCB,('BPLADEL',ACTRECD),('ACRTACTH',LDGSJ)               
                                                                                
CPUT0260 GOTOR SETPDT              Set new pointer date/time                    
         LA    R0,ACTRECD          Copy record back to original I/O             
         LHI   R1,IODDWQ                                                        
         LA    RE,N.ACTRECD                                                     
         LLH   RF,N.ACTRLEN                                                     
         MVCL  R0,RE                                                            
         MVC   IODASAV,IODA                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOACCMST+IO2'                        
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   IODASAV,IODA                                                     
         JE    *+10                                                             
         MVC   IODA,IODASAV        Restore Primary Disk Address                 
         CLC   SVDIR,IOKEY         Test change of record key                    
         JE    CPUT0300                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOACCDIR+IO2'                         
         JE    CPUT0300                                                         
         DC    H'0'                                                             
         DROP  N                                                                
                                                                                
CPUTCLC1 CLC   0(0,RE),0(R3)       Match new element to old                     
         EJECT                                                                  
***********************************************************************         
* Build and add a new client/product record                           *         
***********************************************************************         
                                                                                
CADD     CLI   C_ACTN,ACTNADDQ     Are we adding a new record?                  
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR ADDELS,CPETAB       Add elements to record                       
                                                                                
         SR    R0,R0               Add other stacked elements                   
         ICM   R0,3,STACKL                                                      
         JZ    CADD0020                                                         
         L     R1,AIO8                                                          
CADD0010 GOTOR MOVEEL              Move stacked elements one at a time          
         AR    R1,RF               Point to next element                        
         SR    R0,RF               Decrement total length                       
         JNZ   CADD0010            Until exhausted                              
                                                                                
CADD0020 GOTOR MOVEEL,M_PMDEL      Move in PMD element if necessary             
                                                                                
         MVI   0(R5),0             Set record terminator                        
         AHI   R5,1                Bump over terminator                         
         LA    R0,ACTRECD                                                       
         SR    R5,R0               Calculate record length                      
         STCM  R5,3,ACTRLEN        Set record length                            
         GOTOR SETPDT              Set pointer date/time                        
         GOTOR SETACS,ACTRECD      Build account status area                    
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOACCMST+IO2'                           
         JE    CPUT0300                                                         
         DC    H'0'                                                             
                                                                                
CPUT0300 GOTOR BLDPAS,DMCB,('BPLAADD',AIO2),('ACRTACTH',LDGSJ)                  
                                                                                
***********************************************************************         
* Add/change subsidiary account records                               *         
***********************************************************************         
                                                                                
CPUT0310 GOTOR PUTACT,DMCB,(5,FLAGSR),LDGSR                                     
         GOTOR PUTACT,DMCB,(6,FLAG1C),LDG1C                                     
         GOTOR PUTACT,DMCB,(7,FLAG29),LDG29                                     
                                                                                
         GOTOR UPDPAS              Update passive pointers                      
                                                                                
***********************************************************************         
* Send token back as the good response                                *         
***********************************************************************         
                                                                                
         GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',I#CFMCU)             
         GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTRAW',1),         +        
               ('LD_CHARQ',C_TOKN),(L'C_TOKN,0)                                 
         J     EXITCLR                                                          
         DROP  R2,RC                                                            
                                                                                
CPWORKD  DSECT ,                   ** CPUT local w/s **                         
CPIO     DS    XL(IOLENQ)          Saved record                                 
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Full product upload                                                 *         
***********************************************************************         
                                                                                
UPRUREC  LKREQ H,I#CFMPRU,NEXTREQ=UPROREC,ROUTINE=UPUPL,NEWREC=Y                
                                                                                
Action   LKREQ F,001,(D,B#SAVED,C_ACTN),CHAR,TEXT=(*,RECALIT)                   
Token    LKREQ F,300,(D,B#SAVED,C_TOKN),CHAR,TEXT=(*,TOKNLIT)                   
CheckSum LKREQ F,301,(D,B#SAVED,C_CKSM),HEXD,TEXT=(*,CKSMLIT)                   
                                                                                
CliCode  LKREQ F,002,(D,B#SAVED,C_CODE),CHAR,TEXT=(*,CCODLIT)                   
PrdCode  LKREQ F,003,(D,B#SAVED,P_CODE),CHAR,TEXT=(*,PCODLIT)                   
PrdName  LKREQ F,005,(D,B#SAVED,C_NAME),VCHR,TEXT=(*,PNAMLIT),         +        
               LOWERCASE=Y                                                      
                                                                                
SRName   LKREQ F,330,(D,B#SAVED,C_SRNAME),VCHR,TEXT=(*,SRANLIT),       +        
               LOWERCASE=Y                                                      
SRFilt1  LKREQ F,331,(D,B#SAVED,C_SRFLT1),CHAR,TEXT=(*,SRF1LIT)                 
SRFilt2  LKREQ F,332,(D,B#SAVED,C_SRFLT2),CHAR,TEXT=(*,SRF2LIT)                 
SRFilt3  LKREQ F,333,(D,B#SAVED,C_SRFLT3),CHAR,TEXT=(*,SRF3LIT)                 
SRFilt4  LKREQ F,334,(D,B#SAVED,C_SRFLT4),CHAR,TEXT=(*,SRF4LIT)                 
SRFilt5  LKREQ F,335,(D,B#SAVED,C_SRFLT5),CHAR,TEXT=(*,SRF5LIT)                 
                                                                                
1CName   LKREQ F,336,(D,B#SAVED,C_1CNAME),VCHR,TEXT=(*,OCANLIT),       +        
               LOWERCASE=Y                                                      
1CFilt1  LKREQ F,337,(D,B#SAVED,C_1CFLT1),CHAR,TEXT=(*,OCF1LIT)                 
1CFilt2  LKREQ F,338,(D,B#SAVED,C_1CFLT2),CHAR,TEXT=(*,OCF2LIT)                 
1CFilt3  LKREQ F,339,(D,B#SAVED,C_1CFLT3),CHAR,TEXT=(*,OCF3LIT)                 
1CFilt4  LKREQ F,340,(D,B#SAVED,C_1CFLT4),CHAR,TEXT=(*,OCF4LIT)                 
1CFilt5  LKREQ F,341,(D,B#SAVED,C_1CFLT5),CHAR,TEXT=(*,OCF5LIT)                 
                                                                                
Addr1    LKREQ F,017,(D,B#SAVED,A_ADDR1),CHAR,TEXT=(*,ADR1LIT),        +        
               LOWERCASE=Y                                                      
Addr2    LKREQ F,018,(D,B#SAVED,A_ADDR2),CHAR,TEXT=(*,ADR2LIT),        +        
               LOWERCASE=Y                                                      
Addr3    LKREQ F,019,(D,B#SAVED,A_ADDR3),CHAR,TEXT=(*,ADR3LIT),        +        
               LOWERCASE=Y                                                      
Addr4    LKREQ F,020,(D,B#SAVED,A_ADDR4),CHAR,TEXT=(*,ADR4LIT),        +        
               LOWERCASE=Y                                                      
Addr5    LKREQ F,021,(D,B#SAVED,A_ADDR5),CHAR,TEXT=(*,ADR5LIT),        +        
               LOWERCASE=Y                                                      
                                                                                
OAddr1   LKREQ F,022,(D,B#SAVED,O_OADDR1),CHAR,TEXT=(*,OAD1LIT),       +        
               LOWERCASE=Y                                                      
OAddr2   LKREQ F,023,(D,B#SAVED,O_OADDR2),CHAR,TEXT=(*,OAD2LIT),       +        
               LOWERCASE=Y                                                      
OAddr3   LKREQ F,024,(D,B#SAVED,O_OADDR3),CHAR,TEXT=(*,OAD3LIT),       +        
               LOWERCASE=Y                                                      
OAddr4   LKREQ F,025,(D,B#SAVED,O_OADDR4),CHAR,TEXT=(*,OAD4LIT),       +        
               LOWERCASE=Y                                                      
OAddr5   LKREQ F,026,(D,B#SAVED,O_OADDR5),CHAR,TEXT=(*,OAD5LIT),       +        
               LOWERCASE=Y                                                      
                                                                                
CBilGrp  LKREQ F,040,(D,B#SAVED,P_BILGRP),CHAR,TEXT=(*,BGRPLIT)                 
CRcvblCo LKREQ F,041,(D,B#SAVED,TRASH1),HEXD,TEXT=(*,TRASHLIT)                  
CRcvblUL LKREQ F,042,(D,B#SAVED,TRASH2),CHAR,TEXT=(*,TRASHLIT)                  
CRcvblAc LKREQ F,044,(D,B#SAVED,P_RECVBL),CHAR,TEXT=(*,RECVLIT)                 
CCostCo  LKREQ F,045,(D,B#SAVED,TRASH1),HEXD,TEXT=(*,TRASHLIT)                  
CCostUL  LKREQ F,046,(D,B#SAVED,TRASH2),CHAR,TEXT=(*,TRASHLIT)                  
CCostAc  LKREQ F,048,(D,B#SAVED,P_COSTAC),CHAR,TEXT=(*,COSTLIT)                 
CGAOffic LKREQ F,049,(D,B#SAVED,P_GAOFF),CHAR,TEXT=(*,GAOFLIT)                  
CBilType LKREQ F,050,(D,B#SAVED,P_BILTYP),CHAR,TEXT=(*,BTYPLIT)                 
CBilAmt  LKREQ F,051,(D,B#SAVED,P_BILAMT),LBIN,TEXT=(*,BAMTLIT)                 
CUforA   LKREQ F,052,(D,B#SAVED,P_UFORA),CHAR,TEXT=(*,UFRALIT)                  
CUblblWC LKREQ F,053,(D,B#SAVED,P_UWORK),CHAR,TEXT=(*,UWRKLIT)                  
CBilPrt  LKREQ F,054,(D,B#SAVED,P_BILPRT),CHAR,TEXT=(*,BPRTLIT),       +        
               LOWERCASE=Y                                                      
CBilNar1 LKREQ F,055,(D,B#SAVED,P_BILNR1),CHAR,TEXT=(*,BNR1LIT),       +        
               LOWERCASE=Y                                                      
CBilNar2 LKREQ F,056,(D,B#SAVED,P_BILNR2),CHAR,TEXT=(*,BNR2LIT),       +        
               LOWERCASE=Y                                                      
CBilNar3 LKREQ F,057,(D,B#SAVED,P_BILNR3),CHAR,TEXT=(*,BNR3LIT),       +        
               LOWERCASE=Y                                                      
                                                                                
CSecNum  LKREQ F,060,(D,B#SAVED,S_RSECY),LBIN,TEXT=(*,SECYLIT)                  
CFilt1   LKREQ F,061,(D,B#SAVED,S_RFILT1),CHAR,TEXT=(*,FLT1LIT)                 
CFilt2   LKREQ F,062,(D,B#SAVED,S_RFILT2),CHAR,TEXT=(*,FLT2LIT)                 
CFilt3   LKREQ F,063,(D,B#SAVED,S_RFILT3),CHAR,TEXT=(*,FLT3LIT)                 
CFilt4   LKREQ F,064,(D,B#SAVED,S_RFILT4),CHAR,TEXT=(*,FLT4LIT)                 
CFilt5   LKREQ F,065,(D,B#SAVED,S_RFILT5),CHAR,TEXT=(*,FLT5LIT)                 
CCstCtr  LKREQ F,066,(D,B#SAVED,S_RCCTR),LBIN,TEXT=(*,CCTRLIT)                  
CSt1X80  LKREQ F,069,(D,B#SAVED,S_RSTAT1),MB80,TEXT=(*,S180LIT)                 
CSt1X40  LKREQ F,070,(D,B#SAVED,S_RSTAT1),MB40,TEXT=(*,S140LIT)                 
CSt1X20  LKREQ F,071,(D,B#SAVED,S_RSTAT1),MB20,TEXT=(*,S120LIT)                 
CSt1X10  LKREQ F,072,(D,B#SAVED,S_RSTAT1),MB10,TEXT=(*,S110LIT)                 
CSt1X08  LKREQ F,073,(D,B#SAVED,S_RSTAT1),MB08,TEXT=(*,S108LIT)                 
CSt1X04  LKREQ F,074,(D,B#SAVED,S_RSTAT1),MB04,TEXT=(*,S104LIT)                 
CSt1X02  LKREQ F,075,(D,B#SAVED,S_RSTAT1),MB02,TEXT=(*,S102LIT)                 
CSt1X01  LKREQ F,076,(D,B#SAVED,S_RSTAT1),MB01,TEXT=(*,S101LIT)                 
CSt2X80  LKREQ F,077,(D,B#SAVED,S_RSTAT2),MB80,TEXT=(*,S280LIT)                 
CSt2X40  LKREQ F,078,(D,B#SAVED,S_RSTAT2),MB40,TEXT=(*,S240LIT)                 
CSt2X20  LKREQ F,079,(D,B#SAVED,S_RSTAT2),MB20,TEXT=(*,S220LIT)                 
CSt2X10  LKREQ F,080,(D,B#SAVED,S_RSTAT2),MB10,TEXT=(*,S210LIT)                 
CSt2X08  LKREQ F,081,(D,B#SAVED,S_RSTAT2),MB08,TEXT=(*,S208LIT)                 
CSt2X04  LKREQ F,082,(D,B#SAVED,S_RSTAT2),MB04,TEXT=(*,S204LIT)                 
CSt2X02  LKREQ F,083,(D,B#SAVED,S_RSTAT2),MB02,TEXT=(*,S202LIT)                 
CSt2X01  LKREQ F,084,(D,B#SAVED,S_RSTAT2),MB01,TEXT=(*,S201LIT)                 
CSt3X80  LKREQ F,085,(D,B#SAVED,S_RSTAT3),MB80,TEXT=(*,S380LIT)                 
CSt3X40  LKREQ F,086,(D,B#SAVED,S_RSTAT3),MB40,TEXT=(*,S340LIT)                 
CSt3X20  LKREQ F,087,(D,B#SAVED,S_RSTAT3),MB20,TEXT=(*,S320LIT)                 
CSt3X10  LKREQ F,088,(D,B#SAVED,S_RSTAT3),MB10,TEXT=(*,S310LIT)                 
CSt3X08  LKREQ F,089,(D,B#SAVED,S_RSTAT3),MB08,TEXT=(*,S308LIT)                 
CSt3X04  LKREQ F,090,(D,B#SAVED,S_RSTAT3),MB04,TEXT=(*,S304LIT)                 
CSt3X02  LKREQ F,091,(D,B#SAVED,S_RSTAT3),MB02,TEXT=(*,S302LIT)                 
CSt3X01  LKREQ F,092,(D,B#SAVED,S_RSTAT3),MB01,TEXT=(*,S301LIT)                 
CSt4X80  LKREQ F,093,(D,B#SAVED,S_RSTAT4),MB80,TEXT=(*,S480LIT)                 
CSt4X40  LKREQ F,094,(D,B#SAVED,S_RSTAT4),MB40,TEXT=(*,S440LIT)                 
CSt4X20  LKREQ F,095,(D,B#SAVED,S_RSTAT4),MB20,TEXT=(*,S420LIT)                 
CSt4X10  LKREQ F,096,(D,B#SAVED,S_RSTAT4),MB10,TEXT=(*,S410LIT)                 
CSt4X08  LKREQ F,097,(D,B#SAVED,S_RSTAT4),MB08,TEXT=(*,S408LIT)                 
CSt4X04  LKREQ F,098,(D,B#SAVED,S_RSTAT4),MB04,TEXT=(*,S404LIT)                 
CSt4X02  LKREQ F,099,(D,B#SAVED,S_RSTAT4),MB02,TEXT=(*,S402LIT)                 
CSt4X01  LKREQ F,100,(D,B#SAVED,S_RSTAT4),MB01,TEXT=(*,S401LIT)                 
CSt5X80  LKREQ F,101,(D,B#SAVED,S_RSTAT5),MB80,TEXT=(*,S580LIT)                 
CSt5X40  LKREQ F,102,(D,B#SAVED,S_RSTAT5),MB40,TEXT=(*,S540LIT)                 
CSt5X20  LKREQ F,103,(D,B#SAVED,S_RSTAT5),MB20,TEXT=(*,S520LIT)                 
CSt5X10  LKREQ F,104,(D,B#SAVED,S_RSTAT5),MB10,TEXT=(*,S510LIT)                 
CSt5X08  LKREQ F,105,(D,B#SAVED,S_RSTAT5),MB08,TEXT=(*,S508LIT)                 
CSt5X04  LKREQ F,106,(D,B#SAVED,S_RSTAT5),MB04,TEXT=(*,S504LIT)                 
CSt5X02  LKREQ F,107,(D,B#SAVED,S_RSTAT5),MB02,TEXT=(*,S502LIT)                 
CSt5X01  LKREQ F,108,(D,B#SAVED,S_RSTAT5),MB01,TEXT=(*,S501LIT)                 
CSt6X80  LKREQ F,109,(D,B#SAVED,S_RSTAT6),MB80,TEXT=(*,S680LIT)                 
CSt6X40  LKREQ F,110,(D,B#SAVED,S_RSTAT6),MB40,TEXT=(*,S640LIT)                 
CSt6X20  LKREQ F,111,(D,B#SAVED,S_RSTAT6),MB20,TEXT=(*,S620LIT)                 
CSt6X10  LKREQ F,112,(D,B#SAVED,S_RSTAT6),MB10,TEXT=(*,S610LIT)                 
CSt6X08  LKREQ F,113,(D,B#SAVED,S_RSTAT6),MB08,TEXT=(*,S608LIT)                 
CSt6X04  LKREQ F,114,(D,B#SAVED,S_RSTAT6),MB04,TEXT=(*,S604LIT)                 
CSt6X02  LKREQ F,115,(D,B#SAVED,S_RSTAT6),MB02,TEXT=(*,S602LIT)                 
CSt6X01  LKREQ F,116,(D,B#SAVED,S_RSTAT6),MB01,TEXT=(*,S601LIT)                 
CCCSTR   LKREQ F,117,(D,B#SAVED,S_RCSTR),LBIN,TEXT=(*,CSTRLIT)                  
CBBFDt   LKREQ F,118,(D,B#SAVED,S_RBBFD),PDAT,TEXT=(*,BBFDLIT)                  
CLTDate  LKREQ F,119,(D,B#SAVED,S_RLTDT),PDAT,TEXT=(*,LTDTLIT)                  
CCostGp  LKREQ F,120,(D,B#SAVED,S_RCSTG),CHAR,TEXT=(*,CSGPLIT)                  
CSMedCd  LKREQ F,121,(D,B#SAVED,S_RSMDC),CHAR,TEXT=(*,SMEDLIT)                  
COffice  LKREQ F,122,(D,B#SAVED,S_ROFFC),CHAR,TEXT=(*,OFFCLIT)                  
CX1099   LKREQ F,123,(D,B#SAVED,S_RX1099),LBIN,TEXT=(*,X109LIT)                 
CDftTsk  LKREQ F,124,(D,B#SAVED,S_RDFTTT),CHAR,TEXT=(*,DFSKLIT)                 
CMail    LKREQ F,125,(D,B#SAVED,S_RMAIL),LBIN,TEXT=(*,MAILLIT)                  
CLBilDt  LKREQ F,127,(D,B#SAVED,S_RLBILD),LBIN,TEXT=(*,LBDTLIT)                 
CLSTX80  LKREQ F,128,(D,B#SAVED,S_RLSTAT),MB80,TEXT=(*,LS80LIT)                 
CLSTX40  LKREQ F,129,(D,B#SAVED,S_RLSTAT),MB40,TEXT=(*,LS40LIT)                 
CLSTX20  LKREQ F,130,(D,B#SAVED,S_RLSTAT),MB20,TEXT=(*,LS20LIT)                 
CLSTX10  LKREQ F,131,(D,B#SAVED,S_RLSTAT),MB10,TEXT=(*,LS10LIT)                 
CLSTX08  LKREQ F,132,(D,B#SAVED,S_RLSTAT),MB08,TEXT=(*,LS08LIT)                 
CLSTX04  LKREQ F,133,(D,B#SAVED,S_RLSTAT),MB04,TEXT=(*,LS04LIT)                 
CLSTX02  LKREQ F,134,(D,B#SAVED,S_RLSTAT),MB02,TEXT=(*,LS02LIT)                 
CLSTX01  LKREQ F,135,(D,B#SAVED,S_RLSTAT),MB01,TEXT=(*,LS01LIT)                 
                                                                                
PersnC1  LKREQ F,140,(D,B#SAVED,P_PERS),CHAR,TEXT=(*,PPERLIT)                   
LstAct1  LKREQ F,141,(D,B#SAVED,P_ADAT),PDAT,TEXT=(*,PDATLIT)                   
PersnC2  LKREQ F,142,(D,B#SAVED,P_PER2),CHAR,TEXT=(*,PPR2LIT)                   
LstAct2  LKREQ F,143,(D,B#SAVED,P_ADAT2),PDAT,TEXT=(*,PPD2LIT)                  
                                                                                
Number   LKREQ F,150,(D,B#SAVED,O_NUM),CHAR,TEXT=(*,ONUMLIT)                    
Profile  LKREQ F,151,(D,B#SAVED,O_PROF),CHAR,OLEN=2,TEXT=(*,OPROLIT)            
Net      LKREQ F,153,(D,B#SAVED,O_NET),CHAR,TEXT=(*,ONETLIT)                    
                                                                                
SeqNum   LKREQ F,160,(I,B#SAVED,A_SCM),UBIN,TEXT=(*,SSEQLIT),ARRAY=S,  +        
               OLEN=L'S_SEQ                                                     
PrtOnBil LKREQ F,161,,CHAR,TEXT=(*,POBLLIT),OLEN=0                              
PrtOnEst LKREQ F,162,,CHAR,TEXT=(*,POESLIT),OLEN=0                              
SeqAmtI  LKREQ F,163,,CHAR,TEXT=(*,SAMTLIT),OLEN=0                              
OMatCom  LKREQ F,164,,CHAR,TEXT=(*,OMATLIT),OLEN=0                              
PrtBef   LKREQ F,165,,CHAR,TEXT=(*,PBEFLIT),OLEN=0                              
PrtAft   LKREQ F,166,,CHAR,TEXT=(*,PAFTLIT),OLEN=0                              
POFootLn LKREQ F,167,,CHAR,TEXT=(*,POFTLIT),OLEN=0                              
OrdPrtX  LKREQ F,168,,CHAR,TEXT=(*,ORDXLIT),OLEN=1                              
ScmCode  LKREQ F,169,,CHAR,TEXT=(*,SCMCLIT),ARRAY=E,OLEN=L'S_CDTX,     +        
               DELIM=X'FF'                                                      
                                                                                
OnlnMemo LKREQ F,180,(D,B#SAVED,O_MEMO),VCHR,TEXT=(*,OMEMLIT),         +        
               LOWERCASE=Y                                                      
                                                                                
AnalAct  LKREQ F,190,(D,B#SAVED,A_SULA),CHAR,TEXT=(*,ANACLIT)                   
AnalNam  LKREQ F,191,(D,B#SAVED,A_SNAME),CHAR,TEXT=(*,ANAMLIT)                  
                                                                                
FBill    LKREQ F,210,(D,B#SAVED,M_FBILL),CHAR,TEXT=(*,LBILLIT)                  
LBill    LKREQ F,211,(D,B#SAVED,M_LBILL),CHAR,TEXT=(*,FBILLIT)                  
RBill    LKREQ F,212,(D,B#SAVED,M_RBILL),CHAR,TEXT=(*,RBILLIT)                  
                                                                                
FFTType  LKREQ F,220,(I,B#SAVED,A_FFT),UBIN,OLEN=L'F_TYPE,             +        
               TEXT=(*,FTYPLIT),ARRAY=S                                         
FFTSeq#  LKREQ F,221,,LBIN,OLEN=L'F_SEQ,TEXT=(*,FSEQLIT)                        
FFTData  LKREQ F,222,,CHAR,OLEN=L'F_DATA,TEXT=(*,FTXTLIT),             +        
               LOWERCASE=Y,ARRAY=E                                              
                                                                                
         LKREQ E                                                                
                                                                                
UPUPL    GOTOR SETELS,CPETAB       Call element upload routines                 
         J     PUPL0010            Build record key                             
         EJECT                                                                  
***********************************************************************         
* Product record                                                      *         
***********************************************************************         
                                                                                
UPROREC  LKREQ H,I#CFMPU,NEXTREQ=UPPREL,ROUTINE=PUPL,NEWREC=Y                   
                                                                                
Action   LKREQ F,001,(D,B#SAVED,C_ACTN),CHAR,TEXT=(*,RECALIT)                   
Token    LKREQ F,002,(D,B#SAVED,C_TOKN),CHAR,TEXT=(*,TOKNLIT)                   
CheckSum LKREQ F,003,(D,B#SAVED,C_CKSM),HEXD,TEXT=(*,CKSMLIT)                   
CliCode  LKREQ F,004,(D,B#SAVED,C_CODE),CHAR,TEXT=(*,CCODLIT)                   
PrdCode  LKREQ F,005,(D,B#SAVED,P_CODE),CHAR,TEXT=(*,PCODLIT)                   
PrdName  LKREQ F,006,(D,B#SAVED,C_NAME),VCHR,TEXT=(*,PNAMLIT),         +        
               LOWERCASE=Y                                                      
                                                                                
SRName   LKREQ F,007,(D,B#SAVED,C_SRNAME),VCHR,TEXT=(*,SRANLIT),       +        
               LOWERCASE=Y                                                      
SRFilt1  LKREQ F,008,(D,B#SAVED,C_SRFLT1),CHAR,TEXT=(*,SRF1LIT)                 
SRFilt2  LKREQ F,009,(D,B#SAVED,C_SRFLT2),CHAR,TEXT=(*,SRF2LIT)                 
SRFilt3  LKREQ F,010,(D,B#SAVED,C_SRFLT3),CHAR,TEXT=(*,SRF3LIT)                 
SRFilt4  LKREQ F,011,(D,B#SAVED,C_SRFLT4),CHAR,TEXT=(*,SRF4LIT)                 
SRFilt5  LKREQ F,012,(D,B#SAVED,C_SRFLT5),CHAR,TEXT=(*,SRF5LIT)                 
                                                                                
1CName   LKREQ F,013,(D,B#SAVED,C_1CNAME),VCHR,TEXT=(*,OCANLIT),       +        
               LOWERCASE=Y                                                      
1CFilt1  LKREQ F,014,(D,B#SAVED,C_1CFLT1),CHAR,TEXT=(*,OCF1LIT)                 
1CFilt2  LKREQ F,015,(D,B#SAVED,C_1CFLT2),CHAR,TEXT=(*,OCF2LIT)                 
1CFilt3  LKREQ F,016,(D,B#SAVED,C_1CFLT3),CHAR,TEXT=(*,OCF3LIT)                 
1CFilt4  LKREQ F,017,(D,B#SAVED,C_1CFLT4),CHAR,TEXT=(*,OCF4LIT)                 
1CFilt5  LKREQ F,018,(D,B#SAVED,C_1CFLT5),CHAR,TEXT=(*,OCF5LIT)                 
                                                                                
         LKREQ E                                                                
                                                                                
K        USING ACTRECD,IOKEY                                                    
PUPL     LARL  RE,PPUT             Set address of record put routine            
         ST    RE,APUTRTN                                                       
                                                                                
K        USING ACTRECD,IOKEY                                                    
PUPL0010 MVC   K.ACTKEY,SPACES     Build key of product record                  
         MVC   K.ACTKCPY,LP_AGYB                                                
         MVC   K.ACTKUNT(L'ACTKUNT+L'ACTKLDG),LDGSJL                            
         MVC   K.ACTKACT(L'C_CODE),C_CODE                                       
         LLC   R1,LDGSJ+(LDGTLVA-LDGTABD)                                       
         LA    R1,K.ACTKACT(R1)                                                 
         MVC   0(L'P_CODE,R1),P_CODE                                            
         J     CUPL0012            All other code is same as client             
         EJECT                                                                  
***********************************************************************         
* PPREL upload                                                        *         
***********************************************************************         
                                                                                
UPPREL   LKREQ H,I#CFMPPR,NEXTREQ=URSTEL,ROUTINE=PPRUPL,NEWREC=N                
                                                                                
CBilGrp  LKREQ F,001,(D,B#SAVED,P_BILGRP),CHAR,TEXT=(*,BGRPLIT)                 
CRcvblAc LKREQ F,002,(D,B#SAVED,P_RECVBL),CHAR,TEXT=(*,RECVLIT)                 
CCostAc  LKREQ F,003,(D,B#SAVED,P_COSTAC),CHAR,TEXT=(*,COSTLIT)                 
CGAOffic LKREQ F,004,(D,B#SAVED,P_GAOFF),CHAR,TEXT=(*,GAOFLIT)                  
CBilType LKREQ F,005,(D,B#SAVED,P_BILTYP),CHAR,TEXT=(*,BTYPLIT)                 
CBilAmt  LKREQ F,006,(D,B#SAVED,P_BILAMT),LBIN,TEXT=(*,BAMTLIT)                 
CUforA   LKREQ F,007,(D,B#SAVED,P_UFORA),CHAR,TEXT=(*,UFRALIT)                  
CUblblWC LKREQ F,008,(D,B#SAVED,P_UWORK),CHAR,TEXT=(*,UWRKLIT)                  
CBilPrt  LKREQ F,009,(D,B#SAVED,P_BILPRT),CHAR,TEXT=(*,BPRTLIT),       +        
               LOWERCASE=Y                                                      
CBilNar1 LKREQ F,010,(D,B#SAVED,P_BILNR1),CHAR,TEXT=(*,BNR1LIT),       +        
               LOWERCASE=Y                                                      
CBilNar2 LKREQ F,011,(D,B#SAVED,P_BILNR2),CHAR,TEXT=(*,BNR2LIT),       +        
               LOWERCASE=Y                                                      
CBilNar3 LKREQ F,012,(D,B#SAVED,P_BILNR3),CHAR,TEXT=(*,BNR3LIT),       +        
               LOWERCASE=Y                                                      
                                                                                
         LKREQ E                                                                
                                                                                
I        USING PPRELD,P_PPREL                                                   
PPRUPL   MVI   I.PPREL,PPRELQ      Upload PPREL                                 
         MVI   I.PPRLN,PPRLN1Q     Set short element                            
         CLC   I.PPRNARRP,SPACES                                                
         JNH   *+8                                                              
         MVI   I.PPRLN,PPRLNXQ     Set longest known element length             
         CLC   I.PPRRECVA,SPACES   Test debtor account given                    
         JNH   PPRUPL02                                                         
         MVC   I.PPRRECVC,LP_AGYB  Set CUL of debtor account                    
         MVC   I.PPRRECVU(L'PPRRECVU+L'PPRRECVL),LDGSRL                         
PPRUPL02 CLC   I.PPRCOSTA,SPACES   Test client costing account given            
         JNH   PPRUPL04                                                         
         MVC   I.PPRCOSTC,LP_AGYB  Set CUL of client costing account            
         MVC   I.PPRCOSTU(L'PPRCOSTU+L'PPRCOSTL),LDG1CL                         
PPRUPL04 CR    RE,RE               Set condition code to equal                  
         BR    RE                                                               
         DROP  I                                                                
         EJECT                                                                  
***********************************************************************         
* RSTEL upload                                                        *         
***********************************************************************         
                                                                                
URSTEL   LKREQ H,I#CFMRST,NEXTREQ=UADREL,ROUTINE=RSTUPL,NEWREC=N                
                                                                                
CSecNum  LKREQ F,001,(D,B#SAVED,S_RSECY),LBIN,TEXT=(*,SECYLIT)                  
CFilt1   LKREQ F,002,(D,B#SAVED,S_RFILT1),CHAR,TEXT=(*,FLT1LIT)                 
CFilt2   LKREQ F,003,(D,B#SAVED,S_RFILT2),CHAR,TEXT=(*,FLT2LIT)                 
CFilt3   LKREQ F,004,(D,B#SAVED,S_RFILT3),CHAR,TEXT=(*,FLT3LIT)                 
CFilt4   LKREQ F,005,(D,B#SAVED,S_RFILT4),CHAR,TEXT=(*,FLT4LIT)                 
CFilt5   LKREQ F,006,(D,B#SAVED,S_RFILT5),CHAR,TEXT=(*,FLT5LIT)                 
CCstCtr  LKREQ F,007,(D,B#SAVED,S_RCCTR),LBIN,TEXT=(*,CCTRLIT)                  
CSt1X80  LKREQ F,008,(D,B#SAVED,S_RSTAT1),MB80,TEXT=(*,S180LIT)                 
CSt1X40  LKREQ F,009,(D,B#SAVED,S_RSTAT1),MB40,TEXT=(*,S140LIT)                 
CSt1X20  LKREQ F,010,(D,B#SAVED,S_RSTAT1),MB20,TEXT=(*,S120LIT)                 
CSt1X10  LKREQ F,011,(D,B#SAVED,S_RSTAT1),MB10,TEXT=(*,S110LIT)                 
CSt1X08  LKREQ F,012,(D,B#SAVED,S_RSTAT1),MB08,TEXT=(*,S108LIT)                 
CSt1X04  LKREQ F,013,(D,B#SAVED,S_RSTAT1),MB04,TEXT=(*,S104LIT)                 
CSt1X02  LKREQ F,014,(D,B#SAVED,S_RSTAT1),MB02,TEXT=(*,S102LIT)                 
CSt1X01  LKREQ F,015,(D,B#SAVED,S_RSTAT1),MB01,TEXT=(*,S101LIT)                 
CSt2X80  LKREQ F,016,(D,B#SAVED,S_RSTAT2),MB80,TEXT=(*,S280LIT)                 
CSt2X40  LKREQ F,017,(D,B#SAVED,S_RSTAT2),MB40,TEXT=(*,S240LIT)                 
CSt2X20  LKREQ F,018,(D,B#SAVED,S_RSTAT2),MB20,TEXT=(*,S220LIT)                 
CSt2X10  LKREQ F,019,(D,B#SAVED,S_RSTAT2),MB10,TEXT=(*,S210LIT)                 
CSt2X08  LKREQ F,020,(D,B#SAVED,S_RSTAT2),MB08,TEXT=(*,S208LIT)                 
CSt2X04  LKREQ F,021,(D,B#SAVED,S_RSTAT2),MB04,TEXT=(*,S204LIT)                 
CSt2X02  LKREQ F,022,(D,B#SAVED,S_RSTAT2),MB02,TEXT=(*,S202LIT)                 
CSt2X01  LKREQ F,023,(D,B#SAVED,S_RSTAT2),MB01,TEXT=(*,S201LIT)                 
CSt3X80  LKREQ F,024,(D,B#SAVED,S_RSTAT3),MB80,TEXT=(*,S380LIT)                 
CSt3X40  LKREQ F,025,(D,B#SAVED,S_RSTAT3),MB40,TEXT=(*,S340LIT)                 
CSt3X20  LKREQ F,026,(D,B#SAVED,S_RSTAT3),MB20,TEXT=(*,S320LIT)                 
CSt3X10  LKREQ F,027,(D,B#SAVED,S_RSTAT3),MB10,TEXT=(*,S310LIT)                 
CSt3X08  LKREQ F,028,(D,B#SAVED,S_RSTAT3),MB08,TEXT=(*,S308LIT)                 
CSt3X04  LKREQ F,029,(D,B#SAVED,S_RSTAT3),MB04,TEXT=(*,S304LIT)                 
CSt3X02  LKREQ F,030,(D,B#SAVED,S_RSTAT3),MB02,TEXT=(*,S302LIT)                 
CSt3X01  LKREQ F,031,(D,B#SAVED,S_RSTAT3),MB01,TEXT=(*,S301LIT)                 
CSt4X80  LKREQ F,032,(D,B#SAVED,S_RSTAT4),MB80,TEXT=(*,S480LIT)                 
CSt4X40  LKREQ F,033,(D,B#SAVED,S_RSTAT4),MB40,TEXT=(*,S440LIT)                 
CSt4X20  LKREQ F,034,(D,B#SAVED,S_RSTAT4),MB20,TEXT=(*,S420LIT)                 
CSt4X10  LKREQ F,035,(D,B#SAVED,S_RSTAT4),MB10,TEXT=(*,S410LIT)                 
CSt4X08  LKREQ F,036,(D,B#SAVED,S_RSTAT4),MB08,TEXT=(*,S408LIT)                 
CSt4X04  LKREQ F,037,(D,B#SAVED,S_RSTAT4),MB04,TEXT=(*,S404LIT)                 
CSt4X02  LKREQ F,038,(D,B#SAVED,S_RSTAT4),MB02,TEXT=(*,S402LIT)                 
CSt4X01  LKREQ F,039,(D,B#SAVED,S_RSTAT4),MB01,TEXT=(*,S401LIT)                 
CSt5X80  LKREQ F,040,(D,B#SAVED,S_RSTAT5),MB80,TEXT=(*,S580LIT)                 
CSt5X40  LKREQ F,041,(D,B#SAVED,S_RSTAT5),MB40,TEXT=(*,S540LIT)                 
CSt5X20  LKREQ F,042,(D,B#SAVED,S_RSTAT5),MB20,TEXT=(*,S520LIT)                 
CSt5X10  LKREQ F,043,(D,B#SAVED,S_RSTAT5),MB10,TEXT=(*,S510LIT)                 
CSt5X08  LKREQ F,044,(D,B#SAVED,S_RSTAT5),MB08,TEXT=(*,S508LIT)                 
CSt5X04  LKREQ F,045,(D,B#SAVED,S_RSTAT5),MB04,TEXT=(*,S504LIT)                 
CSt5X02  LKREQ F,046,(D,B#SAVED,S_RSTAT5),MB02,TEXT=(*,S502LIT)                 
CSt5X01  LKREQ F,047,(D,B#SAVED,S_RSTAT5),MB01,TEXT=(*,S501LIT)                 
CSt6X80  LKREQ F,048,(D,B#SAVED,S_RSTAT6),MB80,TEXT=(*,S680LIT)                 
CSt6X40  LKREQ F,049,(D,B#SAVED,S_RSTAT6),MB40,TEXT=(*,S640LIT)                 
CSt6X20  LKREQ F,050,(D,B#SAVED,S_RSTAT6),MB20,TEXT=(*,S620LIT)                 
CSt6X10  LKREQ F,051,(D,B#SAVED,S_RSTAT6),MB10,TEXT=(*,S610LIT)                 
CSt6X08  LKREQ F,052,(D,B#SAVED,S_RSTAT6),MB08,TEXT=(*,S608LIT)                 
CSt6X04  LKREQ F,053,(D,B#SAVED,S_RSTAT6),MB04,TEXT=(*,S604LIT)                 
CSt6X02  LKREQ F,054,(D,B#SAVED,S_RSTAT6),MB02,TEXT=(*,S602LIT)                 
CSt6X01  LKREQ F,055,(D,B#SAVED,S_RSTAT6),MB01,TEXT=(*,S601LIT)                 
CCCSTR   LKREQ F,056,(D,B#SAVED,S_RCSTR),LBIN,TEXT=(*,CSTRLIT)                  
CBBFDt   LKREQ F,057,(D,B#SAVED,S_RBBFD),PDAT,TEXT=(*,BBFDLIT)                  
CLTDate  LKREQ F,058,(D,B#SAVED,S_RLTDT),PDAT,TEXT=(*,LTDTLIT)                  
CCostGp  LKREQ F,059,(D,B#SAVED,S_RCSTG),CHAR,TEXT=(*,CSGPLIT)                  
CSMedCd  LKREQ F,060,(D,B#SAVED,S_RSMDC),CHAR,TEXT=(*,SMEDLIT)                  
COffice  LKREQ F,061,(D,B#SAVED,S_ROFFC),CHAR,TEXT=(*,OFFCLIT)                  
CX1099   LKREQ F,062,(D,B#SAVED,S_RX1099),LBIN,TEXT=(*,X109LIT)                 
CDftTsk  LKREQ F,063,(D,B#SAVED,S_RDFTTT),CHAR,TEXT=(*,DFSKLIT)                 
CMail    LKREQ F,064,(D,B#SAVED,S_RMAIL),LBIN,TEXT=(*,MAILLIT)                  
CLBilDt  LKREQ F,065,(D,B#SAVED,S_RLBILD),LBIN,TEXT=(*,LBDTLIT)                 
CLSTX80  LKREQ F,066,(D,B#SAVED,S_RLSTAT),MB80,TEXT=(*,LS80LIT)                 
CLSTX40  LKREQ F,067,(D,B#SAVED,S_RLSTAT),MB40,TEXT=(*,LS40LIT)                 
CLSTX20  LKREQ F,068,(D,B#SAVED,S_RLSTAT),MB20,TEXT=(*,LS20LIT)                 
CLSTX10  LKREQ F,069,(D,B#SAVED,S_RLSTAT),MB10,TEXT=(*,LS10LIT)                 
CLSTX08  LKREQ F,070,(D,B#SAVED,S_RLSTAT),MB08,TEXT=(*,LS08LIT)                 
CLSTX04  LKREQ F,071,(D,B#SAVED,S_RLSTAT),MB04,TEXT=(*,LS04LIT)                 
CLSTX02  LKREQ F,072,(D,B#SAVED,S_RLSTAT),MB02,TEXT=(*,LS02LIT)                 
CLSTX01  LKREQ F,073,(D,B#SAVED,S_RLSTAT),MB01,TEXT=(*,LS01LIT)                 
                                                                                
         LKREQ E                                                                
                                                                                
I        USING RSTELD,S_RSTEL      Upload RSTEL                                 
RSTUPL   NTR1  LABEL=*                                                          
         OI    I.RSTCOSTG,C' '     Ensure cost group not binary zeroes          
         CLI   C_ACTN,ACTNADDQ     Test adding a new record                     
         JNE   RSTUPL02                                                         
         GOTOR VDATCON,DMCB,(5,0),(1,I.RSTBDATE)                                
         MVC   I.RSTTDATE,I.RSTBDATE                                            
RSTUPL02 MVI   I.RSTEL,RSTELQ                                                   
*&&DO                                                                           
         MVI   I.RSTLN,RSTLN2Q                                                  
         OC    I.RSTELD+RSTLN2Q(RSTLN3Q-RSTLN2Q),I.RSTELD+RSTLN2Q               
         JZ    *+8                                                              
*&&                                                                             
         MVI   I.RSTLN,RSTLN3Q                                                  
         J     EXITY                                                            
         DROP  I                                                                
         EJECT                                                                  
***********************************************************************         
* ADREL upload                                                        *         
***********************************************************************         
                                                                                
UADREL   LKREQ H,I#CFMADR,NEXTREQ=UOADEL,ROUTINE=ADRUPL,NEWREC=N                
                                                                                
Addr1    LKREQ F,001,(D,B#SAVED,A_ADDR1),CHAR,TEXT=(*,ADR1LIT),        +        
               LOWERCASE=Y                                                      
Addr2    LKREQ F,002,(D,B#SAVED,A_ADDR2),CHAR,TEXT=(*,ADR2LIT),        +        
               LOWERCASE=Y                                                      
Addr3    LKREQ F,003,(D,B#SAVED,A_ADDR3),CHAR,TEXT=(*,ADR3LIT),        +        
               LOWERCASE=Y                                                      
Addr4    LKREQ F,004,(D,B#SAVED,A_ADDR4),CHAR,TEXT=(*,ADR4LIT),        +        
               LOWERCASE=Y                                                      
Addr5    LKREQ F,005,(D,B#SAVED,A_ADDR5),CHAR,TEXT=(*,ADR5LIT),        +        
               LOWERCASE=Y                                                      
                                                                                
         LKREQ E                                                                
                                                                                
I        USING ADRELD,A_ADREL      Upload ADREL                                 
ADRUPL   MVI   I.ADREL,ADRELQ                                                   
         LHI   R0,5                                                             
         LHI   RF,ADRLN5Q                                                       
         LA    R1,I.ADRADD5                                                     
ADRUPL02 CLC   0(L'ADRADD1,R1),SPACES                                           
         JH    ADRUPL04                                                         
         SHI   R1,L'ADRADD1                                                     
         SHI   RF,L'ADRADD1                                                     
         JCT   R0,ADRUPL02                                                      
         BR    RE                                                               
ADRUPL04 STC   RF,I.ADRLN          Set length of element                        
         STC   R0,I.ADRNUM         Set number of address lines                  
         CR    RE,RE                                                            
         BR    RE                                                               
         DROP  I                                                                
         EJECT                                                                  
***********************************************************************         
* OADEL upload                                                        *         
***********************************************************************         
                                                                                
UOADEL   LKREQ H,I#CFMOAD,NEXTREQ=UNUMEL,ROUTINE=OADUPL,NEWREC=N                
                                                                                
OAddr1   LKREQ F,001,(D,B#SAVED,O_OADDR1),CHAR,TEXT=(*,OAD1LIT),       +        
               LOWERCASE=Y                                                      
OAddr2   LKREQ F,002,(D,B#SAVED,O_OADDR2),CHAR,TEXT=(*,OAD2LIT),       +        
               LOWERCASE=Y                                                      
OAddr3   LKREQ F,003,(D,B#SAVED,O_OADDR3),CHAR,TEXT=(*,OAD3LIT),       +        
               LOWERCASE=Y                                                      
OAddr4   LKREQ F,004,(D,B#SAVED,O_OADDR4),CHAR,TEXT=(*,OAD4LIT),       +        
               LOWERCASE=Y                                                      
OAddr5   LKREQ F,005,(D,B#SAVED,O_OADDR5),CHAR,TEXT=(*,OAD5LIT),       +        
               LOWERCASE=Y                                                      
                                                                                
         LKREQ E                                                                
                                                                                
I        USING ADRELD,O_OADEL      Upload OADEL                                 
OADUPL   MVI   I.ADREL,OADELQ                                                   
         LHI   R0,5                                                             
         LHI   RF,ADRLN5Q                                                       
         LA    R1,I.ADRADD5                                                     
OADUPL02 CLC   0(L'ADRADD1,R1),SPACES                                           
         JH    OADUPL04                                                         
         SHI   R1,L'ADRADD1                                                     
         SHI   RF,L'ADRADD1                                                     
         JCT   R0,ADRUPL02                                                      
OADUPL04 STC   RF,I.ADRLN          Set length of element                        
         STC   R0,I.ADRNUM         Set number of address lines                  
         CR    RE,RE                                                            
         BR    RE                                                               
         DROP  I                                                                
         EJECT                                                                  
***********************************************************************         
* NUMEL upload                                                        *         
***********************************************************************         
                                                                                
UNUMEL   LKREQ H,I#CFMNUM,NEXTREQ=UPMDEL,ROUTINE=NUMUPL02,NEWREC=N              
                                                                                
NumBef   LKREQ F,001,(D,B#SAVED,N_NUMBEF),CHAR,TEXT=(*,NBEFLIT)                 
NumAft   LKREQ F,002,(D,B#SAVED,N_NUMAFT),CHAR,TEXT=(*,NAFTLIT)                 
NumTyp   LKREQ F,003,(D,B#SAVED,N_NUMTYP),CHAR,TEXT=(*,NTYPLIT)                 
                                                                                
         LKREQ E                                                                
                                                                                
I        USING NUMELD,N_NUMEL                                                   
NUMUPL   NTR1  LABEL=*                                                          
                                                                                
NUMUPL02 MVI   I.NUMEL,NUMELQ      Upload NUMEL                                 
         MVI   I.NUMLN,NUMLN2Q                                                  
         GOTOR STACK,I.NUMEL       Add element to stack                         
         XC    N_NUMEL,N_NUMEL     Clear for next upload                        
         J     EXITY                                                            
         DROP  I                                                                
         EJECT                                                                  
***********************************************************************         
* PMDEL upload                                                        *         
***********************************************************************         
                                                                                
UPMDEL   LKREQ H,I#CFMPMD,NEXTREQ=UOTHEL,ROUTINE=PMDUPL,NEWREC=N                
                                                                                
FrstBill LKREQ F,001,(D,B#SAVED,M_FBILL),CHAR,TEXT=(*,LBILLIT)                  
LastBill LKREQ F,002,(D,B#SAVED,M_LBILL),CHAR,TEXT=(*,FBILLIT)                  
RsetBill LKREQ F,003,(D,B#SAVED,M_RBILL),CHAR,TEXT=(*,RBILLIT)                  
                                                                                
         LKREQ E                                                                
                                                                                
I        USING PMDEL,M_PMDEL                                                    
PMDUPL   MVI   I.PMDEL,PMDELQ      Upload PMDEL                                 
         MVI   I.PMDLN,PMDLN1Q                                                  
         CR    RE,RE                                                            
         BR    RE                                                               
         DROP  I                                                                
         EJECT                                                                  
***********************************************************************         
* OTHEL upload                                                        *         
***********************************************************************         
                                                                                
UOTHEL   LKREQ H,I#CFMOTH,NEXTREQ=USCMEL,ROUTINE=OTHUPL,NEWREC=N                
                                                                                
Number   LKREQ F,001,(D,B#SAVED,O_NUM),CHAR,TEXT=(*,ONUMLIT)                    
Profile  LKREQ F,002,(D,B#SAVED,O_PROF),CHAR,OLEN=2,TEXT=(*,OPROLIT)            
Net      LKREQ F,003,(D,B#SAVED,O_NET),CHAR,TEXT=(*,ONETLIT)                    
                                                                                
         LKREQ E                                                                
I        USING OTHEL,O_OTHEL                                                    
OTHUPL   MVI   I.OTHEL,OTHELQ      Upload OTHEL                                 
         MVI   I.OTHLN,L'O_OTHEL                                                
         CR    RE,RE                                                            
         BR    RE                                                               
         DROP  I                                                                
         EJECT                                                                  
***********************************************************************         
* SCMEL upload                                                        *         
***********************************************************************         
                                                                                
USCMEL   LKREQ H,I#CFMSCM,NEXTREQ=UFFTEL,ROUTINE=SCMUPL02,NEWREC=N              
                                                                                
SeqNum   LKREQ F,001,(D,B#SAVED,S_SEQ),UBIN,TEXT=(*,SSEQLIT)                    
PrtOnBil LKREQ F,002,(D,B#SAVED,S_TYPE),MB80,TEXT=(*,POBLLIT)                   
PrtOnEst LKREQ F,003,(D,B#SAVED,S_TYPE),MB40,TEXT=(*,POESLIT)                   
SeqAmtI  LKREQ F,004,(D,B#SAVED,S_TYPE),MB20,TEXT=(*,SAMTLIT)                   
OMatCom  LKREQ F,005,(D,B#SAVED,S_TYPE),MB10,TEXT=(*,OMATLIT)                   
PrtBef   LKREQ F,006,(D,B#SAVED,S_TYPE),MB08,TEXT=(*,PBEFLIT)                   
PrtAft   LKREQ F,007,(D,B#SAVED,S_TYPE),MB04,TEXT=(*,PAFTLIT)                   
POFootLn LKREQ F,008,(D,B#SAVED,S_TYPE),MB02,TEXT=(*,POFTLIT)                   
OrdPrtX  LKREQ F,009,(D,B#SAVED,S_TYPE),MB01,TEXT=(*,ORDXLIT)                   
ScmCode  LKREQ F,010,(D,B#SAVED,S_CDTX),CHAR,TEXT=(*,SCMCLIT),         +        
               DELIM=X'FF'                                                      
                                                                                
         LKREQ E                                                                
                                                                                
I        USING SCMELD,S_SCMEL                                                   
SCMUPL   NTR1  LABEL=*             Enter here for array processing              
                                                                                
SCMUPL02 MVI   I.SCMEL,SCMELQ      Upload SCMEL                                 
         MVI   I.SCMLN,SCMCODE+L'SCMCODE-SCMELD                                 
         CLC   S_CDTX(4),=C'BIL='                                               
         JE    SCMUPL2A                                                         
         CLI   S_TYPE,X'84'                                                     
         JNE   SCMUPL03                                                         
                                                                                
SCMUPL2A MVI   I.SCMTYPE,X'84'     INDICATE IT IS A STANDARD COMMENT            
         MVC   S_CDTX(L'S_CDTX-4),S_CDTX+4                                      
         MVC   S_CDTX+L'S_CDTX-4(4),SPACES                                      
         MVC   TMPCOMM,S_CDTX      SAVE COMMENT CODE                            
                                                                                
         MVC   S_CDTX(6),SPACES    RIGHT-ALIGN COMMENT CODE                     
         LA    RF,TMPCOMM+5                                                     
         LA    RE,S_CDTX                                                        
         LA    R1,6                                                             
SCMUPL2C CLI   0(RF),C' '                                                       
         JNE   SCMUPL2F                                                         
         SHI   RF,1                                                             
         AHI   RE,1                                                             
         JCT   R1,SCMUPL2C                                                      
                                                                                
SCMUPL2F SHI   R1,1                                                             
         LARL  RB,SCMUPL2M                                                      
         EX    R1,0(RB)                                                         
         J     SCMUPL03                                                         
SCMUPL2M MVC   0(0,RE),TMPCOMM                                                  
                                                                                
SCMUPL03 LA    R1,S_CDTX+L'S_CDTX-1                                             
         LA    R0,L'S_CDTX                                                      
*        LA    R0,L'S_CDTX-1                                                    
                                                                                
SCMUPL04 CLI   0(R1),C' '          Get length of text string                    
         JH    SCMUPL05                                                         
         BCTR  R1,0                                                             
         JCT   R0,SCMUPL04                                                      
         J     EXITN                                                            
                                                                                
SCMUPL05 LA    R0,S_CDTX                                                        
         SR    R1,R0                                                            
         AHI   R1,SCMNARR+1-SCMELD                                              
         STC   R1,I.SCMLN          Set element length                           
                                                                                
SCMUPL06 GOTOR STACK,I.SCMEL       Add element to stack                         
         XC    S_SCMEL,S_SCMEL     Clear for next upload                        
         J     EXITY                                                            
         DROP  I                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* FFTEL upload                                                        *         
***********************************************************************         
                                                                                
UFFTEL   LKREQ H,I#CFMFFT,NEXTREQ=UOMEEL,ROUTINE=FFTUPL02,NEWREC=N              
                                                                                
FFTType  LKREQ F,001,(D,B#SAVED,F_TYPE),UBIN,TEXT=(*,FTYPLIT)                   
FFTSeq   LKREQ F,002,(D,B#SAVED,F_SEQ),UBIN,TEXT=(*,FSEQLIT)                    
FFTData  LKREQ F,003,(D,B#SAVED,F_DATA),CHAR,TEXT=(*,FTXTLIT),         +        
               LOWERCASE=Y                                                      
                                                                                
         LKREQ E                                                                
                                                                                
I        USING FFTELD,F_FFTEL                                                   
FFTUPL   NTR1  LABEL=*                                                          
                                                                                
FFTUPL02 MVI   I.FFTEL,FFTELQ      Upload FFTEL                                 
         LA    R1,F_DATA+L'F_DATA-1                                             
         LA    R0,L'F_DATA-1                                                    
FFTUPL04 CLI   0(R1),C' '                                                       
         JH    FFTUPL06                                                         
         BCTR  R1,0                                                             
         JCT   R0,FFTUPL04                                                      
FFTUPL06 AHI   R1,1                                                             
         LA    R0,F_FFTEL                                                       
         SR    R1,R0                                                            
         STC   R1,I.FFTLN          Set element length                           
         SHI   R1,FFTDATA-FFTELD                                                
         STC   R1,I.FFTDLEN        Set length of text                           
         GOTOR STACK,I.FFTEL       Save in element stack                        
         XC    F_FFTEL,F_FFTEL     Clear for next upload                        
         J     EXITY                                                            
         DROP  I                                                                
         EJECT                                                                  
***********************************************************************         
* OMEEL upload                                                        *         
***********************************************************************         
                                                                                
UOMEEL   LKREQ H,I#CFMOME,NEXTREQ=UPACEL,ROUTINE=OMEUPL,NEWREC=N                
                                                                                
OnlnMemo LKREQ F,001,(D,B#SAVED,O_MEMO),VCHR,TEXT=(*,OMEMLIT),         +        
               LOWERCASE=Y                                                      
                                                                                
         LKREQ E                                                                
                                                                                
I        USING OMEELD,O_OMEEL                                                   
OMEUPL   MVI   I.OMEEL,OMEELQ      Upload OMEEL                                 
         LLC   R0,I.OMELN          Get length of comment                        
         AHI   R0,OMEMO-OMEELD     Adjust for overhead                          
         STC   R0,I.OMELN          and store back                               
         CR    RE,RE                                                            
         BR    RE                                                               
         DROP  I                                                                
         EJECT                                                                  
***********************************************************************         
* PACEL upload                                                        *         
***********************************************************************         
                                                                                
UPACEL   LKREQ H,I#CFMPAC,NEXTREQ=USANEL,ROUTINE=PACUPL,NEWREC=N                
                                                                                
PersnC1  LKREQ F,001,(D,B#SAVED,P_PERS),CHAR,TEXT=(*,PPERLIT)                   
LstAct1  LKREQ F,002,(D,B#SAVED,P_ADAT),PDAT,TEXT=(*,PDATLIT)                   
PersnC2  LKREQ F,003,(D,B#SAVED,P_PER2),CHAR,TEXT=(*,PPR2LIT)                   
LstAct2  LKREQ F,004,(D,B#SAVED,P_ADAT2),PDAT,TEXT=(*,PPD2LIT)                  
                                                                                
         LKREQ E                                                                
                                                                                
I        USING PACELD,P_PACEL                                                   
PACUPL   MVI   I.PACEL,PACELQ      Upload PACEL                                 
         MVI   I.PACLN,PACLNQ                                                   
         OC    I.PACPERS2(L'PACPERS2+L'PACDATE2),I.PACPERS2                     
         JZ    *+8                                                              
         MVI   I.PACLN,PACLNQ2                                                  
         CR    RE,RE                                                            
         BR    RE                                                               
         DROP  I                                                                
         EJECT                                                                  
***********************************************************************         
* SANEL upload                                                        *         
***********************************************************************         
                                                                                
USANEL   LKREQ H,I#CFMSAN,NEXTREQ=ACCR_U,ROUTINE=PACUPL,NEWREC=N                
                                                                                
AnalAct  LKREQ F,001,(D,B#SAVED,A_SULA),CHAR,TEXT=(*,ANACLIT)                   
AnalNam  LKREQ F,002,(D,B#SAVED,A_SNAME),CHAR,TEXT=(*,ANAMLIT)                  
                                                                                
         LKREQ E                                                                
                                                                                
I        USING SANELD,A_SANEL                                                   
SANUPL   MVI   I.SANEL,SANELQ      Upload SANEL                                 
         MVI   I.SANLN,L'A_SANEL                                                
         MVC   I.SANCODE(L'LP_AGYB),LP_AGYB                                     
         BR    RE                                                               
         DROP  I                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Account record upload                                               *         
***********************************************************************         
                                                                                
ACCR_U   LKREQ H,I#CFMACU,NEXTREQ=UGO,ROUTINE=ACCRUL,NEWREC=Y                   
                                                                                
UntLdg   LKREQ F,004,(D,B#SAVED,A_UNTLDG),CHAR,TEXT=(*,ULDGLIT)                 
ACCode   LKREQ F,005,(D,B#SAVED,A_ACCODE),CHAR,TEXT=(*,AMISLIT)                 
ACName   LKREQ F,006,(D,B#SAVED,A_ACNAME),VCHR,TEXT=(*,NMISLIT),       +        
               LOWERCASE=Y                                                      
ACFilt1  LKREQ F,007,(D,B#SAVED,A_ACFLT1),CHAR,TEXT=(*,FLT1LIT)                 
ACFilt2  LKREQ F,008,(D,B#SAVED,A_ACFLT2),CHAR,TEXT=(*,FLT2LIT)                 
ACFilt3  LKREQ F,009,(D,B#SAVED,A_ACFLT3),CHAR,TEXT=(*,FLT3LIT)                 
ACFilt4  LKREQ F,010,(D,B#SAVED,A_ACFLT4),CHAR,TEXT=(*,FLT4LIT)                 
ACFilt5  LKREQ F,011,(D,B#SAVED,A_ACFLT5),CHAR,TEXT=(*,FLT5LIT)                 
Optn001  LKREQ F,100,(D,B#SAVED,A_OPT001),CHAR,TEXT=(*,OPT1LIT)                 
Token    LKREQ F,300,(D,B#SAVED,C_TOKN),CHAR,TEXT=(*,TOKNLIT)                   
         LKREQ E                                                                
                                                                                
ACCRUL   MVC   A_ACCMPC,LP_AGYB                                                 
         MVI   A_ACNAML,NAMELQ                                                  
         SR    R0,R0               Build Account name                           
         ICM   R0,1,A_ACNAME                                                    
         JZ    *+16                                                             
         MVI   A_ACNAML,NAMELQ     Complete SR name element                     
         AHI   R0,NAMEREC-NAMELD                                                
         STC   R0,A_ACNAME                                                      
         OI    A_ACCSW1,HILVACCQ   High Level account                           
                                                                                
         CLC   LDGSRL,A_UNTLDG                                                  
         JNE   *+12                                                             
         LA    RF,LDGSR                                                         
         J     ACCRU30                                                          
         CLC   LDG1CL,A_UNTLDG                                                  
         JNE   *+12                                                             
         LA    RF,LDG1C                                                         
         J     ACCRU30                                                          
         CLC   LDG29L,A_UNTLDG                                                  
         JNE   *+12                                                             
         LA    RF,LDG29                                                         
         J     ACCRU30                                                          
                                                                                
         J     EXITCLR             Do not handle other accounts                 
                                                                                
ACCRU30  GOTOR GETACT,DMCB,(5,A_ACCKEY),A_ACNAML,(RF),FLAGSR                    
         JNE   EXITCLR                                                          
         GOTOR PUTACT,DMCB,(5,FLAGSR),A_UNTLDG                                  
         GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',I#CFMAU)             
         GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTRAW',1),         +        
               ('LD_CHARQ',C_TOKN),(L'C_TOKN,0)                                 
         J     EXITCLR                                                          
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* UGO upload (call record I/O routine if no errors encountered)       *         
***********************************************************************         
                                                                                
UGO      LKREQ *,I#CFMUGO,NEXTREQ=ULAST,ROUTINE=GOROUT,NEWREC=N                 
                                                                                
GOROUT   TM    RECFLAG,RECFUERR    Test any errors                              
         JNZ   EXITCLR             Yes - nothing to do                          
         GOTOR APUTRTN             Call record put routine                      
                                                                                
ULAST    LKREQ X                   End of request list                          
         EJECT                                                                  
***********************************************************************         
* Initialize balance element                                          *         
***********************************************************************         
                                                                                
I        USING ABLELD,A_ABLEL                                                   
INIABL   MVI   I.ABLEL,ABLELQ      Initialize balance element                   
         MVI   I.ABLLN,L'A_ABLEL                                                
         ZAP   I.ABLFRWD,PZERO                                                  
         ZAP   I.ABLDR,PZERO                                                    
         ZAP   I.ABLCR,PZERO                                                    
         XC    I.ABLTXS,I.ABLTXS                                                
         BR    RE                                                               
                                                                                
***********************************************************************         
* Initialize peel-off element                                         *         
***********************************************************************         
                                                                                
I        USING APOELD,A_APOEL                                                   
INIAPO   MVI   I.APOEL,APOELQ      Initialize peel-off element                  
         MVI   I.APOLN,L'A_APOEL                                                
         XC    I.APOPLDT,I.APOPLDT                                              
         XC    I.APOLBDT,I.APOLBDT                                              
         ZAP   I.APODR,PZERO                                                    
         ZAP   I.APOCR,PZERO                                                    
         BR    RE                                                               
                                                                                
***********************************************************************         
* Initialize Record Activity Element                                  *         
***********************************************************************         
                                                                                
I        USING RACELD,A_RACEL                                                   
INIRAC   ST    RE,FULL1            Save return address                          
         MVI   I.RACEL,RACELQ      Initialize peel-off element                  
         MVI   I.RACLN,RACLNQ                                                   
         MVI   I.RACTYPE,RACTADD                                                
         CLI   C_ACTN,ACTNADDQ     Test adding a record                         
         JE    *+8                                                              
         MVI   I.RACTYPE,RACTCHA                                                
*                                                                               
         L     RF,ATWA                                                          
         USING TWAD,RF             RA=A(TWA & SAVED W/S)                        
         MVC   I.RACUSER,TWAUSRID                                               
         MVC   I.RACTERM,TWATRM                                                 
         DROP  RF                                                               
*                                                                               
         USING COMFACSD,R1                                                      
         L     R1,ACOMFACS         A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         JZ    INIRACX                                                          
         USING XTRAINFD,RF                                                      
         OC    XIPID,XIPID                                                      
         JZ    INIRACX                                                          
         MVC   I.RACPERS,XIPID                                                  
         DROP  RF                                                               
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(1,I.RACDATE)                                 
         TIME  BIN                                                              
         SRDL  R0,32                                                            
         D     R0,F100                                                          
         STCM  R1,7,I.RACTIME                                                   
INIRACX  L     RE,FULL1                                                         
         BR    RE                                                               
                                                                                
***********************************************************************         
* Initialize pointer element                                          *         
***********************************************************************         
                                                                                
I        USING PTRELD,P_PTREL                                                   
INIPTR   XC    I.PTRELD(L'P_PTREL),I.PTRELD                                     
         TM    CPYSTA6,CPYSRAPP    Test create passives                         
         BZR   RE                                                               
         MVI   I.PTREL,PTRELQ      Initialize activity pointer element          
         MVI   I.PTRLN,L'P_PTREL                                                
         MVI   I.PTRTYPE,PTRTRAP                                                
K        USING RAPRECD,I.PTRCODE                                                
         XC    K.RAPKEY,K.RAPKEY                                                
         MVI   K.RAPKTYP,RAPKTYPQ                                               
         MVC   K.RAPKCPY,LP_AGYB                                                
         MVC   K.RAPKACT,IOKEY                                                  
         MVI   K.RAPKRTYP,RAPKRCLI Set client pointer                           
         OC    P_CODE,P_CODE                                                    
         JZ    *+8                                                              
         MVI   K.RAPKRTYP,RAPKRPRO Set product pointer                          
         BR    RE                                                               
         DROP  I,K                                                              
                                                                                
***********************************************************************         
* Set current date/time in pointer element                            *         
***********************************************************************         
                                                                                
SETPDT   NTR1  LABEL=*                                                          
         ICM   R2,15,APTRELN       Point to pointer element                     
         JZ    EXITY                                                            
         USING PTRELD,R2                                                        
K        USING RAPRECD,PTRCODE                                                  
         GOTOR VDATCON,DMCB,(5,0),(2,K.RAPKDATE)                                
         TIME  BIN                                                              
         SRDL  R0,32                                                            
         D     R0,F100                                                          
         STCM  R1,7,K.RAPKTIME                                                  
         J     EXITY                                                            
         DROP  K,R2                                                             
                                                                                
***********************************************************************         
* Add element to stack in I/O area 8                                  *         
***********************************************************************         
                                                                                
STACK    NTR1  LABEL=*                                                          
         L     R2,AIO8             Point to start of element stack              
         LLH   R3,STACKL           Get current space used                       
         AR    R2,R3               Point to next save area                      
         LLC   R0,1(R1)            R0=element length                            
         AR    R3,R0                                                            
         CHI   R3,IOLENQ           Test stack is full                           
         JNH   *+6                                                              
         DC    H'0'                Yes - need to make this bigger               
         STH   R3,STACKL           Set updated stack length                     
         LR    R3,R0                                                            
         AHI   R3,1                Pad an extra binary zero at end              
         LR    RE,R1               Point to element                             
         LR    RF,R0               Set element length                           
         MVCL  R2,RE               Move element to stack and pad                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Initialize an I/O area for element adding                           *         
***********************************************************************         
                                                                                
INIREC   NTR1  LABEL=*                                                          
         LR    R2,R1               Point to I/O area                            
R        USING ACTRECD,R2                                                       
         LA    R0,R.ACTRECD                                                     
         LHI   R1,IOLENQ                                                        
         SR    RF,RF                                                            
         MVCL  R0,RE               Start with a clean I/O area                  
         MVC   R.ACTKEY,IOKEY      Set key fields only                          
         LHI   R0,ACTRFST+1-ACTRECD                                             
         STCM  R0,3,R.ACTRLEN      Set initial length (empty)                   
         J     EXIT                                                             
         DROP  R                                                                
                                                                                
***********************************************************************         
* Call LINKIO to put errors                                           *         
***********************************************************************         
                                                                                
ERROR    NTR1  LABEL=*             Send an error via LINKIO                     
         LM    RF,R0,0(R1)         RF=error number,R0=(L'text,A(text))          
         STCM  RF,3,WORK                                                        
         TM    RECFLAG,RECFTOKN    Test record map and token sent               
         JNZ   ERROR02             Yes                                          
         LLH   RF,LP_QMAPN                                                      
         GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(RF))                
         GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTRAW',1),         +        
               ('LD_CHARQ',C_TOKN),(L'C_TOKN,0)                                 
         OI    RECFLAG,RECFTOKN    Set record token sent                        
                                                                                
ERROR02  GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTERR',2),WORK,(R0)         
         OI    RECFLAG,RECFUERR    Set we have an error                         
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Clear upload values                                                 *         
***********************************************************************         
                                                                                
CLRWRK   STM   RE,R1,12(RD)        Clear upload values                          
         LA    R0,Q_VALUES                                                      
         LHI   R1,Q_VALUEL                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Move elements to output record from stack                           *         
* Ntry:- R3=A(element code for stack search)                          *         
* Exit:- CC=equal if stacked element(s) moved else CC=not equal       *         
***********************************************************************         
                                                                                
MOVEELS  NTR1  LABEL=*                                                          
         MVC   BYTE1,0(R3)         Set current element code                     
         SR    R0,R0                                                            
         ICM   R0,3,STACKL         Get/test stacked length                      
         JZ    EXITN                                                            
         L     R1,AIO8             Point to start of element stack              
MOVEELS2 CLI   0(R1),0             Test end of stacked elements                 
         JE    EXITN                                                            
         CLC   0(1,R1),BYTE1       Test same as input element                   
         JE    MOVEELS4            Yes - go move it/them                        
         LLC   RF,1(R1)                                                         
         AR    R1,RF               Point to next element                        
         J     MOVEELS2                                                         
                                                                                
MOVEELS4 GOTOR MOVEEL              Move element from stack to record            
         MVI   0(R1),FF            Set element moved                            
         AR    R1,RF               Point to next stack element                  
         SR    R0,RF               Decrement stack length                       
         JZ    EXITR5              Exit to caller when exhausted                
         CLC   0(1,R1),BYTE1       Test another element to process              
         JE    MOVEELS4            Yes - add it and bump                        
         J     EXITR5              No - exit to caller                          
                                                                                
***********************************************************************         
* Add element (pointed to by R1) to output record (pointed to by R5)  *         
***********************************************************************         
                                                                                
MOVEEL   CLI   0(R1),0             Test new element present                     
         BER   RE                                                               
         LLC   RF,1(R1)            RF=element length                            
         CLI   0(R1),FF            Test element deleted                         
         BER   RE                                                               
         LARL  RB,MOVEMVC                                                       
         BCTR  RF,0                                                             
         EX    RF,0(RB)            Move element to output record                
         J     MOVEELX                                                          
                                                                                
MOVEMVC  MVC   0(0,R5),0(R1)                                                    
                                                                                
***********************************************************************         
* Copy element (pointed to by R3) to output record (pointed to by R5) *         
***********************************************************************         
                                                                                
COPYEL   LARL  RB,COPYMVC                                                       
         LLC   RF,1(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,0(RB)            Move element to output record                
         J     COPYELX                                                          
                                                                                
COPYMVC  MVC   0(0,R5),0(R3)                                                    
                                                                                
***********************************************************************         
* Save address of pointer element on output record - bump to next     *         
* output element (R5)                                                 *         
***********************************************************************         
                                                                                
COPYELX  DS    0H                                                               
MOVEELX  CLI   0(R5),PTRELQ        Test this is the pointer element             
         JNE   *+8                                                              
         ST    R5,APTRELN          Yes - save its address                       
         LLC   RF,1(R5)            Bump to next element                         
         AR    R5,RF                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* Set up record elements - R1=A(element table) (see ELETABD)          *         
***********************************************************************         
                                                                                
SETELS   NTR1  LABEL=*                                                          
         LR    R6,R1               Point to normal element table                
         USING ELETABD,R6                                                       
SETELS02 OC    ELETRDSP,ELETRDSP   Test initialization routine present          
         JZ    SETELS06                                                         
         LLH   R1,ELETSDSP                                                      
         LA    R1,SAVED(R1)        Point to element in SAVED                    
         LLC   RF,ELETELEN                                                      
         TM    ELETFLAG,ELETFAGO   Test always call uploader                    
         JNZ   SETELS04                                                         
         SHI   RF,1                                                             
         LARL  RB,SETELOC                                                       
         EX    RF,0(RB)                                                         
         JZ    SETELS06                                                         
SETELS04 LLH   RF,ELETRDSP         Get displacement to uploader                 
         A     RF,LP_ASVR                                                       
         GOTOR (RF)                Point to it                                  
SETELS06 AHI   R6,ELETABL          Bump to next table entry                     
         CLI   ELETABD,ELETEOTQ    Test more to do                              
         JNE   SETELS02                                                         
SETELSX  J     EXITY                                                            
                                                                                
SETELOC  OC    0(0,R1),0(R1)       Test any data given                          
         DROP  R6                                                               
                                                                                
***********************************************************************         
* Add elements in SAVED and stack to output record                    *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2          R2 points to new record on entry             
ADDELS   NTR1  LABEL=*                                                          
         LA    R5,ACTRFST          Point to first element on new record         
         LR    R6,R1               Point to element table                       
         USING ELETABD,R6                                                       
ADDELS02 OC    ELETSDSP,ELETSDSP   Test element in SAVED                        
         JZ    ADDELS04                                                         
         TM    A_ACCSW1,HILVACCQ   High Level account?                          
         JZ    *+12                                                             
         CLI   ELETELE,ABLELQ      Account balance element?                     
         JE    ADDELS04            High level account - no balance elem         
         CLI   ELETELE,APOELQ      Account peel-off element?                    
         JE    ADDELS04            High level account - no peel-off el          
         LLH   R1,ELETSDSP                                                      
         LA    R1,SAVED(R1)        Point to element in SAVED                    
         GOTOR MOVEEL                                                           
ADDELS04 AHI   R6,ELETABL          Bump to next table entry                     
         CLI   ELETABD,ELETEOTQ    Test more to do                              
         JNE   ADDELS02                                                         
         MVI   0(R5),0             Set record terminator                        
         LA    R0,1(R5)                                                         
         SR    R0,R2               Calculate record length                      
         STCM  R0,3,ACTRLEN        Set record length                            
         J     EXITR5              Exit to caller                               
         DROP  R2,R6                                                            
                                                                                
***********************************************************************         
* Build status area for account records and copy to directory record  *         
***********************************************************************         
                                                                                
SETACS   LR    RF,R1               Point to record                              
         USING ACTRECD,RF                                                       
         NI    ACTRSTAT,ACTSDELT+ACTSDRFT                                       
K        USING ACTRECD,IOKEY                                                    
         TM    A_ACCSW1,HILVACCQ   High Level account?                          
         JNZ   SETACS04                                                         
         LA    R1,ACTRFST          Locate status element                        
         SR    R0,R0                                                            
         USING ABLELD,R1                                                        
SETACS02 CLI   ABLEL,0             Test end of record                           
         JE    SETACS04                                                         
         CLI   ABLEL,ABLELQ        Test balance element                         
         JE    *+14                                                             
         IC    R0,ABLLN                                                         
         AR    R1,R0                                                            
         J     SETACS02                                                         
         OI    ACTRSTAT,ACTSABLP   Set Has Balance Element                      
                                                                                
SETACS04 LA    R1,ACTRFST          Locate status element                        
         SR    R0,R0                                                            
         USING RSTELD,R1                                                        
SETACS06 CLI   RSTEL,0             Test end of record                           
         JE    SETACSX                                                          
         CLI   RSTEL,RSTELQ        Test status element                          
         JE    *+14                                                             
         IC    R0,RSTLN                                                         
         AR    R1,R0                                                            
         J     SETACS06                                                         
                                                                                
         TM    RSTSTAT,RSTSACIC                                                 
         JZ    *+8                                                              
         OI    ACTRSTAT,ACTSCLOS   Set account closed                           
         TM    RSTSTAT,RSTSACIL                                                 
         JZ    *+8                                                              
         OI    ACTRSTAT,ACTSLOCK   Set account locked                           
         MVC   ACTRSAF1,RSTFILT1   Set account filters                          
         MVC   ACTRSAF2,RSTFILT2                                                
         MVC   ACTRSAF3,RSTFILT3                                                
         MVC   ACTRSAF4,RSTFILT4                                                
         CLI   RSTLN,RSTLN1Q       Test old status element                      
         JE    SETACSX                                                          
         TM    RSTSTAT3,RSTSLAPL                                                
         JZ    *+8                                                              
         OI    ACTRSTA,ACTSLAPL    Set account is profit and loss               
         TM    RSTSTAT3,RSTSLABS                                                
         JZ    *+8                                                              
         OI    ACTRSTA,ACTSLABS    Set account is balance sheet                 
         MVC   ACTRSAF5,RSTFILT5                                                
SETACSX  MVC   K.ACTKSTA,ACTRSTA   Set status in IOKEY                          
         BR    RE                                                               
         DROP  RF,R1                                                            
                                                                                
***********************************************************************         
* Validate and build an account record                                *         
***********************************************************************         
                                                                                
GETACT   NTR1  LABEL=*,WORK=(RC,GAWORKL)                                        
         USING GAWORKD,RC          RC=A(local storage)                          
         MVC   GAIOVAL,IOVALUES                                                 
         LM    R2,R4,0(R1)                                                      
         L     R6,12(R1)           R6=A(output flag byte)                       
         MVI   0(R6),0             Initialize flag byte                         
         MVC   A_NAMEL(A_NAMELN),0(R3)                                          
         USING LDGTABD,R4          R4=A(ledger table entry)                     
         LLC   R5,0(R1)            R5=I/O area number                           
         LR    R0,R5                                                            
         SLL   R0,12               Shift to the left                            
         STH   R0,HALF1            Save I/O area equate                         
         SLL   R5,2                                                             
         LA    R5,AIOS-L'AIOS(R5)                                               
         L     R5,0(R5)                                                         
         USING ACTRECD,R5          R5=A(record)                                 
K        USING ACTRECD,IOKEY                                                    
         MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCULA,0(R2)    Set full account code                        
         OC    K.ACTKCULA,K.ACTKCULA                                            
         JNZ   GETACT02                                                         
         GOTOR ERROR,DMCB,AE$MISIF,(L'AMISLIT,AMISLIT)                          
         J     GETACTN                                                          
                                                                                
GETACT02 TM    A_ACCSW1,HILVACCQ   High Level account?                          
         JNZ   GETACT10                                                         
         CLI   LDGTLVA,L'ACTKACT   Test single level ledger                     
         JE    GETACT10                                                         
         LA    R1,K.ACTKACT+L'ACTKACT-1                                         
         LHI   R0,L'ACTKACT                                                     
GETACT04 CLI   0(R1),C' '          Test trailing space                          
         JH    *+10                                                             
         BCTR  R1,0                                                             
         JCT   R0,GETACT04                                                      
         SR    RF,RF                                                            
         LA    R1,LDGTLVA                                                       
GETACT06 CLI   0(R1),L'ACTKACT     Get length of next higher account            
         JE    *+16                                                             
         IC    RF,0(R1)                                                         
         AHI   R1,1                                                             
         J     GETACT06                                                         
         CR    R0,RF                                                            
         JH    GETACT08                                                         
         GOTOR ERROR,DMCB,AE$WLACE,(14,1(R2))                                   
         J     GETACTN                                                          
                                                                                
GETACT08 MVC   K.ACTKEY,SPACES     Read for high level account                  
         AHI   RF,(ACTKACT-ACTKEY)-1                                            
         LARL  RB,GETAMVC1                                                      
         EX    RF,0(RB)                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOACCDIR'                                
         JE    GETACT10                                                         
         GOTOR ERROR,DMCB,AE$HLACM,(14,IOKEYSAV+1)                              
         J     GETACTN                                                          
                                                                                
GETACT10 MVC   K.ACTKEY,SPACES     Read for account record                      
         MVC   K.ACTKCULA,0(R2)                                                 
         LH    R1,HALF1                                                         
         LA    R1,IORDUPD+IOACCDIR(R1)                                          
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    GETACT14                                                         
         TM    IOERR,IOERNF        Error must be not found                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   IOKEY,IOKEYSAV      Restore saved key                            
         GOTOR INIREC,ACTRECD      Build a new account record                   
         LA    R2,ACTRECD          Point to new record                          
                                                                                
         CLI   A_NAMEL,0           Test account name is present                 
         JNE   GETACT12                                                         
         GOTOR ERROR,DMCB,AE$MISIF,(L'NMISLIT,NMISLIT)                          
         J     GETACTN                                                          
                                                                                
GETACT12 OC    C_SRFLT1-C_SRNAML(C_SRFLTL,R3),SPACES                            
I        USING RSTELD,A_RSTEL      Move in account filters                      
         MVI   I.RSTEL,RSTELQ                                                   
         MVI   I.RSTLN,L'A_RSTEL                                                
         MVC   I.RSTFILT1,C_SRFLT1-C_SRNAML(R3)                                 
         MVC   I.RSTFILT2,C_SRFLT2-C_SRNAML(R3)                                 
         MVC   I.RSTFILT3,C_SRFLT3-C_SRNAML(R3)                                 
         MVC   I.RSTFILT4,C_SRFLT4-C_SRNAML(R3)                                 
         MVC   I.RSTFILT5,C_SRFLT5-C_SRNAML(R3)                                 
         GOTOR VDATCON,DMCB,(5,0),(1,I.RSTBDATE)                                
         MVC   I.RSTTDATE,I.RSTBDATE                                            
         DROP  I                                                                
                                                                                
         GOTOR INIABL              Initialize balance element                   
         GOTOR INIAPO              Initialize peel-off element                  
         GOTOR INIRAC              Initialize Record Activity Element           
         GOTOR ADDELS,ACETAB       Add account elements                         
         GOTOR SETACS,ACTRECD      Set account status                           
         OI    0(R6),GAFADDR       Tell caller to add a new record              
         J     GETACTY                                                          
                                                                                
GETACT14 TM    K.ACTKSTAT,ACTSLOCK Test account is locked                       
         JZ    GETACT16                                                         
         GOTOR ERROR,DMCB,AE$ACTLK,(14,K.ACTKUNT)                               
         J     GETACTN                                                          
                                                                                
GETACT16 TM    K.ACTKSTAT,ACTSABLP Test account is at lowest level              
         JZ    GETACT17                                                         
         MVI   A_NAMEL,0           Clear NAMEL if there.                        
                                                                                
GETACT17 LH    R1,HALF1                                                         
         LA    R1,IOGETRUP+IOACCMST(R1)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* Do limited maintenance on change of record - only the account name  *         
* and filters are changed here                                        *         
***********************************************************************         
                                                                                
N        USING ACTRECD,GAIO                                                     
         MVC   N.ACTRECD(ACTRFST-ACTRECD),ACTRECD                               
         LA    R2,ACTRECD          Point to old record                          
         DROP  R5                  Swap registers                               
         USING ACTRECD,R2                                                       
         LA    R3,ACTRFST          Point to first element on old                
         LA    R5,N.ACTRFST        Point to first element on new                
                                                                                
GETACT18 CLI   0(R3),0             Test end of old record                       
         JE    GETACT28                                                         
         CLI   0(R3),NAMELQ        Test name element                            
         JNE   GETACT22                                                         
         CLI   A_NAMEL,0           Test account name is present                 
         JE    GETACT24                                                         
         CLC   A_NAMEL(2),0(R3)    Test same length                             
         JNE   GETACT20                                                         
         LLC   R1,1(R3)                                                         
         BCTR  R1,0                                                             
         LARL  RB,GETAMVC2                                                      
         EX    R1,0(RB)                                                         
         JE    GETACT24            No - copy old name element                   
                                                                                
GETACT20 GOTOR MOVEEL,A_NAMEL      Yes - use new NAMEL                          
         OI    0(R6),GAFNAMC       Set account name changed  flag               
         J     GETACT26                                                         
                                                                                
GETACT22 CLI   0(R3),RSTELQ        Test account status element                  
         JNE   GETACT24                                                         
         USING RSTELD,R3                                                        
I        USING RSTELD,A_RSTEL      Update old and copy to new                   
         CLI   I.RSTFILT1,0        Update account filters                       
         JE    *+10                                                             
         MVC   RSTFILT1,I.RSTFILT1                                              
         CLI   I.RSTFILT2,0                                                     
         JE    *+10                                                             
         MVC   RSTFILT2,I.RSTFILT2                                              
         CLI   I.RSTFILT3,0                                                     
         JE    *+10                                                             
         MVC   RSTFILT3,I.RSTFILT3                                              
         CLI   I.RSTFILT4,0                                                     
         JE    *+10                                                             
         MVC   RSTFILT4,I.RSTFILT4                                              
         CLI   I.RSTFILT5,0                                                     
         JE    *+10                                                             
         MVC   RSTFILT5,I.RSTFILT5                                              
         DROP  I,R3                                                             
                                                                                
GETACT24 GOTOR COPYEL              Copy from old record to new                  
                                                                                
GETACT26 LLC   R0,1(R3)            Bump to next input element                   
         AR    R3,R0                                                            
         J     GETACT18                                                         
                                                                                
GETACT28 MVI   0(R5),0             Set record terminator                        
         AHI   R5,1                Bump over terminator                         
         LA    R0,N.ACTRECD                                                     
         SR    R5,R0               Calculate record length                      
         STCM  R5,3,N.ACTRLEN      Set record length                            
         GOTOR SETACS,N.ACTRECD    Build account status area                    
                                                                                
         CLC   ACTRLEN,N.ACTRLEN   Test any change to the record                
         JNE   GETACT30                                                         
         LLH   R1,ACTRLEN                                                       
         LLH   RF,N.ACTRLEN                                                     
         LA    R0,ACTRECD                                                       
         LA    RE,N.ACTRECD                                                     
         CLCL  R0,RE                                                            
         JE    GETACTY             All the same - exit (null flag)              
         GOTOR BLDPAS,DMCB,('BPLADEL',ACTRECD),('ACRTACTL',LDGTABD)             
                                                                                
GETACT30 OI    0(R6),GAFPUTR       Tell caller to put record back               
         LA    R0,ACTRECD          Copy record back to original I/O             
         LHI   R1,IODDWQ           Don't overwrite saved DA and work            
         LA    RE,N.ACTRECD                                                     
         LLH   RF,N.ACTRLEN                                                     
         MVCL  R0,RE                                                            
                                                                                
GETACTY  MVC   IOVALUES(L'GAIOVAL),GAIOVAL                                      
         J     EXITY                                                            
                                                                                
GETACTN  MVC   IOVALUES(L'GAIOVAL),GAIOVAL                                      
         J     EXITN                                                            
         DROP  N,R4,RC                                                          
                                                                                
GETAMVC1 MVC   K.ACTKEY(0),0(R2)   Move in high level account code              
GETAMVC2 CLC   A_NAMEL(0),0(R3)    Test for change of account name              
                                                                                
GAWORKD  DSECT ,                   ** GETACT local w/s **                       
GAIOVAL  DS    XL(IOVALUEL)        Saved I/O values                             
GAIO     DS    XL(IOLENQ)          Saved record                                 
GAWORKL  EQU   *-GAWORKD           Length of local storage                      
SVRDEF   CSECT ,                                                                
                                                                                
***********************************************************************         
* Add or change subsidiary account record (note name change pointer   *         
* is added by this routine)                                           *         
***********************************************************************         
                                                                                
PUTACT   NTR1  LABEL=*                                                          
         LM    R2,R3,0(R1)         R2=A(flag byte),R3=A(ledger entry)           
         CLI   0(R2),0             Test anything to do                          
         JE    EXITY                                                            
                                                                                
         LLC   RE,0(R1)            RE=I/O area number                           
         LR    R4,RE               Save in R4 for code below                    
         SLL   RE,12               Shift to the left                            
         STH   RE,HALF1            Save I/O area equate                         
         SLL   R4,2                                                             
         LA    R4,AIOS-L'AIOS(R4)  Point to I/O area address                    
         L     R4,0(R4)            R4=A(record)                                 
                                                                                
         GOTOR SETACS,(R4)         Build account status area                    
                                                                                
         TM    0(R2),GAFADDR       Test adding a record                         
         JZ    PUTACT02                                                         
         LLH   R1,HALF1                                                         
         AHI   R1,IOADDREC+IOACCMST                                             
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    PUTACT04                                                         
         DC    H'0'                                                             
                                                                                
PUTACT02 TM    0(R2),GAFPUTR       Test changing an existing record             
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LLH   R1,HALF1                                                         
         AHI   R1,IOPUTREC+IOACCMST                                             
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    PUTACT04                                                         
         DC    H'0'                                                             
                                                                                
PUTACT04 GOTOR BLDPAS,DMCB,('BPLAADD',(R4)),('ACRTACTL',(R3))                   
                                                                                
         TM    0(R2),GAFNAMC       Test need name change pointer                
         JZ    EXITY                                                            
                                                                                
R        USING ACTRECD,R4                                                       
K        USING ANCRECD,IOKEY       Build key of name change pointer             
         XC    K.ANCKEY,K.ANCKEY                                                
         MVI   K.ANCKTYP,ANCKTYPQ                                               
         MVC   K.ANCKCULA,R.ACTKCULA                                            
         XC    K.ANCKSTA,K.ANCKSTA                                              
         MVI   K.ANCKSTAT,ANCSDELT These records are flagged deleted            
         MVC   K.ANCKDA,IODA                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOACCDIR'                             
         TM    IOERR,IOERNF        Test record not found                        
         JZ    EXITY                                                            
         MVC   IOKEY,IOKEYSAV      Yes - restore key and add it                 
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOACCDIR'                               
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R,K                                                              
         EJECT                                                                  
***********************************************************************         
* Get passive pointers for a record and add to TSAR buffer            *         
***********************************************************************         
                                                                                
BLDPAS   NTR1  LABEL=*,WORK=(RC,BPWORKL)                                        
         USING BPWORKD,RC          RC=A(local w/s)                              
TP       USING TSARD,TSARPASS      TSAR block for passives                      
         LR    R2,R1               Point to parameter list                      
         USING BPLD,R2             R2=A(parameter list)                         
         LA    R0,BPKEY                                                         
         ST    R0,TP.TSAREC        Set A(TSAR record)                           
                                                                                
         TM    PASIND,PASIINIT     Test TSAR buffer initialized                 
         JNZ   BLDPAS04                                                         
         OI    PASIND,PASIINIT                                                  
         MVI   TP.TSACTN,TSAINI    No - initialize TSAR buffer                  
         MVI   TP.TSRECI,TSRXTN+TSRWSSVR                                        
         MVI   TP.TSKEYL,BPKLQ                                                  
         LHI   R0,BPLNQ                                                         
         STCM  R0,3,TP.TSRECL                                                   
         MVC   TP.TSACOM,LP_ACOM                                                
         GOTOR VTSAR,TP.TSARD      Call TSAR to initialize                      
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   TP.TSACTN,TSAADD    Set TSAR action to 'add'                     
                                                                                
BLDPAS04 L     RF,BPLALDG          Build CPTRBLK and call PADDLE                
         MVC   LDGSTA2,LDGTSTA2-LDGTABD(RF)                                     
         MVC   LDGLVALN,LDGTLVA-LDGTABD(RF)                                     
         MVC   LDGLVBLN,LDGTLVB-LDGTABD(RF)                                     
         MVC   LDGLVCLN,LDGTLVC-LDGTABD(RF)                                     
         MVC   LDGLVDLN,LDGTLVD-LDGTABD(RF)                                     
         MVC   CPTCPY,LP_AGYB                                                   
         MVC   CPTREC,BPLRECT                                                   
         GOTOR VPADDLE,PARM,(C'B',BPLAREC),CPTRBLK,IODA,BPLIST,LP_ACOM,+        
               (X'FF',SRCHAREA)                                                 
                                                                                
         LA    R3,BPLIST           Point to first key                           
L        USING ACCRECD,R3                                                       
BLDPAS06 AHI   R3,ACCKLEN          Bump to first/next passive key               
         CLI   L.ACCKEY,FF         Test end of passive list                     
         JE    EXITY                                                            
         MVC   BPKEY,L.ACCKEY      Set key                                      
         LH    R0,PASSEQ           Bump sequence number                         
         AHI   R0,1                                                             
         STH   R0,PASSEQ                                                        
         MVC   BPSEQ,PASSEQ        Set sequence number                          
         MVC   BPSTA,L.ACCKSTA     Set status                                   
         MVC   BPDA,IODA           Set disk address                             
         MVC   BPACT,BPLACT        Set action (add or delete)                   
         GOTOR VTSAR,TP.TSARD      Add passive to TSAR buffer                   
         JE    BLDPAS06                                                         
         DC    H'0'                Passive buffer full                          
         DROP  L                                                                
         EJECT                                                                  
***********************************************************************         
* Read TSAR passive pointer buffer and add/change directory passives  *         
***********************************************************************         
                                                                                
UPDPAS   NTR1  LABEL=*,WORK=(RC,BPWORKL)                                        
                                                                                
         TM    PASIND,PASIINIT     Test any passives added to buffer            
         JZ    EXITY               No - exit                                    
                                                                                
K        USING ACCKEY,IOKEY                                                     
         XC    BPKEY(BPLNQ),BPKEY                                               
         MVI   TP.TSACTN,TSARDH    Read high first time                         
         LA    R0,BPKEY                                                         
         ST    R0,TP.TSAREC        Set A(TSAR record)                           
                                                                                
UPDPAS02 GOTOR VTSAR,TP.TSARD      Get first/next passive                       
         TM    TP.TSERRS,TSEEOF    Test end of file                             
         JNZ   EXITY               Yes - all done                               
                                                                                
         MVI   TP.TSACTN,TSANXT    (set action for next call)                   
         MVC   K.ACCKEY,BPKEY                                                   
                                                                                
         CLI   BPACT,BPADEL        Test deleting a passive                      
         JNE   UPDPAS04                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOACCDIR'                             
         TM    IOERR,FF-(IOEDEL+IOERNF)    Was there an I/O error               
         JZ    *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOERNF        Does record exist                            
         JNZ   UPDPAS02            No - ignore                                  
         TM    IOERR,IOEDEL        Is this record already deleted?              
         JNZ   UPDPAS02            Yes - ignore                                 
         OI    K.ACCKSTA,X'80'     Set deleted status                           
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOACCDIR'                             
         JE    UPDPAS02                                                         
         DC    H'0'                                                             
                                                                                
UPDPAS04 CLI   BPACT,BPAADD        Test adding a passive                        
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOACCDIR'                             
         TM    IOERR,FF-(IOEDEL+IOERNF)                                         
         JZ    *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL        Test found but deleted                       
         JNZ   UPDPAS06                                                         
         TM    IOERR,IOERNF        Test record not found                        
         JZ    UPDPAS06                                                         
         TM    BPSTA,X'80'         Test adding as deleted                       
         JNZ   UPDPAS02                                                         
         MVC   K.ACCKEY,BPKEY                                                   
         MVC   K.ACCKSTA,BPSTA                                                  
         MVC   K.ACCKDA,BPDA                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOACCDIR'                               
         JE    UPDPAS02                                                         
         DC    H'0'                                                             
                                                                                
UPDPAS06 TM    IOERR,IOEDEL        Test found but deleted                       
         JZ    *+12                                                             
         TM    BPSTA,X'80'         Test changing as deleted                     
         JNZ   UPDPAS02            Yes - ignore                                 
         CLC   K.ACCKSTA,BPSTA     Test status or...                            
         JNE   *+14                                                             
         CLC   K.ACCKDA,BPDA       ...disk address changed                      
         JE    UPDPAS02            No - don't write this one                    
         MVC   K.ACCKEY,BPKEY                                                   
         MVC   K.ACCKSTA,BPSTA                                                  
         MVC   K.ACCKDA,BPDA                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOACCDIR'                             
         JE    UPDPAS02                                                         
         DC    H'0'                                                             
         DROP  K                                                                
                                                                                
BPWORKD  DSECT ,                   ** Passive pointer buffer **                 
                                                                                
BPKEY    DS    XL(L'ACCKEY)        Record key                                   
BPSEQ    DS    XL(L'PASSEQ)        Sequence number                              
BPKLQ    EQU   *-BPKEY             Length of TSAR key                           
                                                                                
BPSTA    DS    XL(L'ACCKSTA)       Record status                                
BPDA     DS    XL(L'ACCRLNK)       Record disk address                          
                                                                                
BPACT    DS    X                   ** Passive pointer action **                 
BPADEL   EQU   1                   Delete passive                               
BPAADD   EQU   2                   Add passive                                  
BPLNQ    EQU   *-BPWORKD                                                        
                                                                                
BPLIST   DS    XL(2*ONEK)          Passive pointer list                         
BPWORKL  EQU   *-BPWORKD                                                        
                                                                                
BPLD     DSECT ,                   ** BLDPAS parameter list **                  
                                                                                
BPLACT   DS    0X                  ** Action code **                            
BPLADEL  EQU   1                   Delete passive pointer                       
BPLAADD  EQU   2                   Add passive pointer                          
BPLAREC  DS    A                   A(input record)                              
                                                                                
BPLRECT  DS    0X                  ** Record type **                            
BPLALDG  DS    A                   A(ledger table entry)                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
TRASHLIT DC    C'Trash - do not send'                                           
RECALIT  DC    C'Record action'                                                 
TOKNLIT  DC    C'Record token'                                                  
CKSMLIT  DC    C'Record checksum'                                               
CCODLIT  DC    C'Client code'                                                   
CNAMLIT  DC    C'Client name'                                                   
SRANLIT  DC    C'SR account name'                                               
SRF1LIT  DC    C'SR filter 1'                                                   
SRF2LIT  DC    C'SR filter 2'                                                   
SRF3LIT  DC    C'SR filter 3'                                                   
SRF4LIT  DC    C'SR filter 4'                                                   
SRF5LIT  DC    C'SR filter 5'                                                   
OCANLIT  DC    C'1C account name'                                               
OCF1LIT  DC    C'1C filter 1'                                                   
OCF2LIT  DC    C'1C filter 2'                                                   
OCF3LIT  DC    C'1C filter 3'                                                   
OCF4LIT  DC    C'1C filter 4'                                                   
OCF5LIT  DC    C'1C filter 5'                                                   
PCODLIT  DC    C'Product code'                                                  
PNAMLIT  DC    C'Product name'                                                  
BGRPLIT  DC    C'Billing group'                                                 
RECVLIT  DC    C'Receivable account'                                            
COSTLIT  DC    C'Costing account'                                               
GAOFLIT  DC    C'G/A office code'                                               
BTYPLIT  DC    C'Billing type'                                                  
BAMTLIT  DC    C'Bill amount'                                                   
UFRALIT  DC    C'Unit for analysis'                                             
UWRKLIT  DC    C'Unbillable work codes'                                         
BPRTLIT  DC    C'Additional billing print'                                      
BNR1LIT  DC    C'Bill narrative 1'                                              
BNR2LIT  DC    C'Bill narrative 2'                                              
BNR3LIT  DC    C'Bill narrative 3'                                              
SECYLIT  DC    C'Security number'                                               
FLT1LIT  DC    C'Filter 1'                                                      
FLT2LIT  DC    C'Filter 2'                                                      
FLT3LIT  DC    C'Filter 3'                                                      
FLT4LIT  DC    C'Filter 4'                                                      
FLT5LIT  DC    C'Filter 5'                                                      
CCTRLIT  DC    C'Cost center'                                                   
S180LIT  DC    C'RSTSTAT1 80 bit'                                               
S140LIT  DC    C'RSTSTAT1 40 bit'                                               
S120LIT  DC    C'RSTSTAT1 20 bit'                                               
S110LIT  DC    C'RSTSTAT1 10 bit'                                               
S108LIT  DC    C'RSTSTAT1 08 bit'                                               
S104LIT  DC    C'RSTSTAT1 04 bit'                                               
S102LIT  DC    C'RSTSTAT1 02 bit'                                               
S101LIT  DC    C'RSTSTAT1 01 bit'                                               
S280LIT  DC    C'RSTSTAT2 80 bit'                                               
S240LIT  DC    C'RSTSTAT2 40 bit'                                               
S220LIT  DC    C'RSTSTAT2 20 bit'                                               
S210LIT  DC    C'RSTSTAT2 10 bit'                                               
S208LIT  DC    C'RSTSTAT2 08 bit'                                               
S204LIT  DC    C'RSTSTAT2 04 bit'                                               
S202LIT  DC    C'RSTSTAT2 02 bit'                                               
S201LIT  DC    C'RSTSTAT2 01 bit'                                               
S380LIT  DC    C'RSTSTAT3 80 bit'                                               
S340LIT  DC    C'RSTSTAT3 40 bit'                                               
S320LIT  DC    C'RSTSTAT3 20 bit'                                               
S310LIT  DC    C'RSTSTAT3 10 bit'                                               
S308LIT  DC    C'RSTSTAT3 08 bit'                                               
S304LIT  DC    C'RSTSTAT3 04 bit'                                               
S302LIT  DC    C'RSTSTAT3 02 bit'                                               
S301LIT  DC    C'RSTSTAT3 01 bit'                                               
S480LIT  DC    C'RSTSTAT4 80 bit'                                               
S440LIT  DC    C'RSTSTAT4 40 bit'                                               
S420LIT  DC    C'RSTSTAT4 20 bit'                                               
S410LIT  DC    C'RSTSTAT4 10 bit'                                               
S408LIT  DC    C'RSTSTAT4 08 bit'                                               
S404LIT  DC    C'RSTSTAT4 04 bit'                                               
S402LIT  DC    C'RSTSTAT4 02 bit'                                               
S401LIT  DC    C'RSTSTAT4 01 bit'                                               
S580LIT  DC    C'RSTSTAT5 80 bit'                                               
S540LIT  DC    C'RSTSTAT5 40 bit'                                               
S520LIT  DC    C'RSTSTAT5 20 bit'                                               
S510LIT  DC    C'RSTSTAT5 10 bit'                                               
S508LIT  DC    C'RSTSTAT5 08 bit'                                               
S504LIT  DC    C'RSTSTAT5 04 bit'                                               
S502LIT  DC    C'RSTSTAT5 02 bit'                                               
S501LIT  DC    C'RSTSTAT5 01 bit'                                               
S680LIT  DC    C'RSTSTAT6 80 bit'                                               
S640LIT  DC    C'RSTSTAT6 40 bit'                                               
S620LIT  DC    C'RSTSTAT6 20 bit'                                               
S610LIT  DC    C'RSTSTAT6 10 bit'                                               
S608LIT  DC    C'RSTSTAT6 08 bit'                                               
S604LIT  DC    C'RSTSTAT6 04 bit'                                               
S602LIT  DC    C'RSTSTAT6 02 bit'                                               
S601LIT  DC    C'RSTSTAT6 01 bit'                                               
CSTRLIT  DC    C'Cost center replace'                                           
BBFDLIT  DC    C'Balance b/f date'                                              
LTDTLIT  DC    C'Last transaction date'                                         
CSGPLIT  DC    C'Costing group'                                                 
SMEDLIT  DC    C'System media letter'                                           
OFFCLIT  DC    C'Office code'                                                   
X109LIT  DC    C'1099 payment type'                                             
DFSKLIT  DC    C'Default TMS task'                                              
MAILLIT  DC    C'Vendor mail delivery'                                          
LBDTLIT  DC    C'Last bill date'                                                
LS80LIT  DC    C'RSTLSTAT 80 bit'                                               
LS40LIT  DC    C'RSTLSTAT 40 bit'                                               
LS20LIT  DC    C'RSTLSTAT 20 bit'                                               
LS10LIT  DC    C'RSTLSTAT 10 bit'                                               
LS08LIT  DC    C'RSTLSTAT 08 bit'                                               
LS04LIT  DC    C'RSTLSTAT 04 bit'                                               
LS02LIT  DC    C'RSTLSTAT 02 bit'                                               
LS01LIT  DC    C'RSTLSTAT 01 bit'                                               
ADR1LIT  DC    C'Address line 1'                                                
ADR2LIT  DC    C'Address line 2'                                                
ADR3LIT  DC    C'Address line 3'                                                
ADR4LIT  DC    C'Address line 4'                                                
ADR5LIT  DC    C'Address line 5'                                                
OAD1LIT  DC    C'Other address line 1'                                          
OAD2LIT  DC    C'Other address line 2'                                          
OAD3LIT  DC    C'Other address line 3'                                          
OAD4LIT  DC    C'Other address line 4'                                          
OAD5LIT  DC    C'Other address line 5'                                          
NBEFLIT  DC    C'Number before'                                                 
NAFTLIT  DC    C'Number after'                                                  
NTYPLIT  DC    C'Number type'                                                   
FBILLIT  DC    C'First bill number'                                             
LBILLIT  DC    C'Last bill number'                                              
RBILLIT  DC    C'Reset bill number'                                             
ONUMLIT  DC    C'Number'                                                        
OPROLIT  DC    C'Profile'                                                       
ONETLIT  DC    C'Network'                                                       
SSEQLIT  DC    C'Sequence number'                                               
POBLLIT  DC    C'Print on bills?'                                               
POESLIT  DC    C'Print on estimates?'                                           
SAMTLIT  DC    C'Seq/amount flag'                                               
OMATLIT  DC    C'Order matching comment'                                        
PBEFLIT  DC    C'Print before data'                                             
PAFTLIT  DC    C'Print after data'                                              
POFTLIT  DC    C'Prod. order footline'                                          
ORDXLIT  DC    C'Order printed desc.'                                           
SCMCLIT  DC    C'Scheme code'                                                   
FTYPLIT  DC    C'FFT type'                                                      
FSEQLIT  DC    C'FFT sequence number'                                           
FTXTLIT  DC    C'FFT text'                                                      
OMEMLIT  DC    C'Memo comment'                                                  
PPERLIT  DC    C'Person 1 activity code'                                        
PDATLIT  DC    C'Person 1 activity date'                                        
PPR2LIT  DC    C'Person 2 activity code'                                        
PPD2LIT  DC    C'Person 2 activity date'                                        
ANACLIT  DC    C'Sales analysis a/c'                                            
ANAMLIT  DC    C'Sales anaylsis a/c name'                                       
ULDGLIT  DC    C'Unit/Ledger'                                                   
OPT1LIT  DC    C'Option 1'                                                      
         EJECT                                                                  
GLOBALS  DS    0D                  ** Global literals **                        
         LTORG ,                                                                
                                                                                
F100     DC    F'100'                                                           
PZERO    DC    P'0'                                                             
                                                                                
LDGSJL   DC    C'SJ'               Production ledger                            
LDGSRL   DC    C'SR'               Debtors ledger                               
LDG1CL   DC    C'1C'               Costing client ledger                        
LDG29L   DC    C'29'               Costing analysis ledger                      
AMISLIT  DC    C'Account code'                                                  
NMISLIT  DC    C'Account name'                                                  
                                                                                
CPETAB   DS    0XL(ELETABL)        ** Client/product SAVED elements **          
         DC    AL1(NAMELQ),AL2(C_NAMEL-SAVED,0)                                 
         DC    AL1(0,0)                                                         
         DC    AL1(PPRELQ),AL2(P_PPREL-SAVED,PPRUPL-SVRDEF)                     
         DC    AL1(L'P_PPREL,ELETFAGO)                                          
         DC    AL1(RSTELQ),AL2(S_RSTEL-SAVED,RSTUPL-SVRDEF)                     
         DC    AL1(L'S_RSTEL,ELETFAGO)                                          
         DC    AL1(ADRELQ),AL2(A_ADREL-SAVED,ADRUPL-SVRDEF)                     
         DC    AL1(L'A_ADREL,0)                                                 
         DC    AL1(OADELQ),AL2(O_OADEL-SAVED,OADUPL-SVRDEF)                     
         DC    AL1(L'O_OADEL,0)                                                 
         DC    AL1(OTHELQ),AL2(O_OTHEL-SAVED,OTHUPL-SVRDEF)                     
         DC    AL1(L'O_OTHEL,ELETDNCQ)                                          
         DC    AL1(OMEELQ),AL2(O_OMEEL-SAVED,OMEUPL-SVRDEF)                     
         DC    AL1(L'O_MEMO,ELETDNCQ)                                           
         DC    AL1(PACELQ),AL2(P_PACEL-SAVED,PACUPL-SVRDEF)                     
         DC    AL1(L'P_PACEL,0)                                                 
         DC    AL1(SANELQ),AL2(A_SANEL-SAVED,SANUPL-SVRDEF)                     
         DC    AL1(L'A_SANEL,0)                                                 
         DC    AL1(PTRELQ),AL2(P_PTREL-SAVED,INIPTR-SVRDEF)                     
         DC    AL1(L'P_PTREL,0)                                                 
         DC    AL1(RACELQ),AL2(A_RACEL-SAVED,INIRAC-SVRDEF)                     
         DC    AL1(L'A_RACEL,ELETFAGO)                                          
CPETABX  DC    AL1(ELETEOTQ)                                                    
                                                                                
ACETAB   DS    0XL(ELETABL)        ** Subsidiary account elements **            
         DC    AL1(NAMELQ),AL2(A_NAMEL-SAVED,0)                                 
         DC    AL1(0,0)                                                         
         DC    AL1(RSTELQ),AL2(A_RSTEL-SAVED,RSTUPL-SVRDEF)                     
         DC    AL1(L'A_RSTEL,0)                                                 
         DC    AL1(ABLELQ),AL2(A_ABLEL-SAVED,INIABL-SVRDEF)                     
         DC    AL1(L'A_ABLEL,ELETFAGO)                                          
         DC    AL1(APOELQ),AL2(A_APOEL-SAVED,INIAPO-SVRDEF)                     
         DC    AL1(L'A_APOEL,ELETFAGO)                                          
         DC    AL1(RACELQ),AL2(A_RACEL-SAVED,INIRAC-SVRDEF)                     
         DC    AL1(L'A_RACEL,ELETFAGO)                                          
ACETABX  DC    AL1(ELETEOTQ)                                                    
                                                                                
ELETABD  DSECT ,                   ** Elements in SAVED **                      
ELETELE  DS    X                   Element code                                 
ELETEOTQ EQU   0                   End of table indicator                       
ELETSDSP DS    AL2                 Displacement to element in SAVED             
ELETRDSP DS    AL2                 Displacement to element upload               
ELETELEN DS    AL1                 Width of element in SAVED                    
                                                                                
ELETFLAG DS    X                   ** Flags **                                  
ELETFAGO EQU   X'80'               Always call initialize routine               
ELETDNCQ EQU   X'40'               Do not copy element                          
                                                                                
ELETABL  EQU   *-ELETABD           Width of table entry                         
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
SAVED    DSECT ,                   ** Saved working storage **                  
                                                                                
LDGSJ    DS    XL(LDGTABL2)        Production ledger values                     
LDGSR    DS    XL(LDGTABL2)        Debtors ledger values                        
LDG1C    DS    XL(LDGTABL2)        Costing client ledger values                 
LDG29    DS    XL(LDGTABL2)        Analysis ledger ledger                       
                                                                                
SVDIR    DS    XL(ACCKLEN)         Saved record key                             
SVRDT    DS    XL(L'RAPKDATE+L'RAPKTIME)                                        
                                                                                
       ++INCLUDE ACLDCPTRD                                                      
SRCHAREA DS    XL(SEARCHDX)        SRCHEXEC control block                       
                                                                                
TSARPASS DS    XL(TSPXTNL)         TSAR block for passive pointers              
                                                                                
LDCPTR   DS    A                   A(ACLDCPTR)                                  
LINKIO   DS    A                   A(LINKIO)                                    
APUTRTN  DS    A                   A(record put routine)                        
                                                                                
Q_VALUES DS    0X                  ** Record values & variables **              
                                                                                
APTRELO  DS    A                   A(old pointer element)                       
APTRELN  DS    A                   A(new pointer element)                       
                                                                                
A_NUM    DS    A                   A(bill number array)                         
A_SCM    DS    A                   A(standard comment array)                    
A_FFT    DS    A                   A(free form text array)                      
                                                                                
PASSEQ   DS    H                   Passive pointer sequence number              
                                                                                
PASIND   DS    X                   ** BLDPAS routine flags **                   
PASIINIT EQU   X'80'               Passive buffer initialized                   
                                                                                
TRASH1   DS    X                   One byte trash goes here                     
TRASH2   DS    XL2                 Two byte trash goes here                     
                                                                                
GAFADDR  EQU   X'80'               Record to be added                           
GAFPUTR  EQU   X'40'               Record to be changed                         
GAFNAMC  EQU   X'20'               Account name changed                         
                                                                                
FLAGSR   DS    X                   SR account flags                             
FLAG29   DS    X                   29 account flags                             
FLAG1C   DS    X                   1C account flags                             
                                                                                
SVELEFLG DS    XL(L'ELETFLAG)      Save element flag                            
                                                                                
RECFLAG  DS    X                   ** Record flag **                            
RECFUERR EQU   X'80'               Upload in error                              
RECFTOKN EQU   X'40'               Record map and token sent                    
                                                                                
C_ACTN   DS    C                   ** Record action **                          
ACTNADDQ EQU   C'A'                Add a record                                 
ACTNCHAQ EQU   C'C'                Change a record                              
ACTNDELQ EQU   C'D'                Delete a record                              
ACTNRESQ EQU   C'R'                Restore a record                             
                                                                                
C_TOKN   DS    CL8                 PC token value                               
C_CKSM   DS    XL4                 Record checksum                              
                                                                                
C_CODE   DS    CL6                 Client code                                  
P_CODE   DS    CL3                 Product code                                 
                                                                                
A_ACCSW1 DS    X                   Account switch 1                             
HILVACCQ EQU   X'80'               High Level account                           
                                                                                
A_OPT001 DS    C                   Option 1 (not used as of now)                
A_ACCKEY DS    0XL15               Account Key                                  
A_ACCMPC DS    XL1                 Company Code                                 
A_UNTLDG DS    CL2                 Unit/Ledger                                  
A_ACCODE DS    CL12                Account Code                                 
A_ACNAML DS    X                   Name element code                            
A_ACNAME DS    XL(L'NAMEREC+1)     Account Name                                 
A_ACFLT1 DS    CL(L'RSTFILT1)      Account filters                              
A_ACFLT2 DS    CL(L'RSTFILT2)                                                   
A_ACFLT3 DS    CL(L'RSTFILT3)                                                   
A_ACFLT4 DS    CL(L'RSTFILT4)                                                   
A_ACFLT5 DS    CL(L'RSTFILT5)                                                   
A_ACLENQ EQU   *-A_ACNAML                                                       
                                                                                
C_NAMEL  DS    X                   ** NAMEL for main account **                 
C_NAME   DS    XL(L'NAMEREC+1)                                                  
                                                                                
A_NAMEL  DS    X                   ** NAMEL for subsidiary accounts **          
A_NAME   DS    XL(L'NAMEREC+1)                                                  
A_NAMELN EQU   *-A_NAMEL                                                        
                                                                                
C_SRNAML DS    X                   ** NAMEL for SR account **                   
C_SRNAME DS    XL(L'NAMEREC+1)                                                  
C_SRFLT1 DS    CL(L'RSTFILT1)      SR account filters                           
C_SRFLT2 DS    CL(L'RSTFILT2)                                                   
C_SRFLT3 DS    CL(L'RSTFILT3)                                                   
C_SRFLT4 DS    CL(L'RSTFILT4)                                                   
C_SRFLT5 DS    CL(L'RSTFILT5)                                                   
C_SRFLTL EQU   *-C_SRFLT1                                                       
C_SRLENQ EQU   *-C_SRNAML                                                       
                                                                                
C_1CNAML DS    X                   ** NAMEL for 1C account **                   
C_1CNAME DS    XL(L'NAMEREC+1)                                                  
C_1CFLT1 DS    CL(L'RSTFILT1)      1C account filters                           
C_1CFLT2 DS    CL(L'RSTFILT2)                                                   
C_1CFLT3 DS    CL(L'RSTFILT3)                                                   
C_1CFLT4 DS    CL(L'RSTFILT4)                                                   
C_1CFLT5 DS    CL(L'RSTFILT5)                                                   
C_1CLENQ EQU   *-C_1CNAML                                                       
                                                                                
C_ANACOD DS    XL(ACTKEND)         29 analysis code                             
C_ANANAM DS    XL(L'NAMEREC+2)     ** NAMEL for costing analysis **             
C_ANFLT1 DS    CL(L'RSTFILT1)      29 account filters                           
C_ANFLT2 DS    CL(L'RSTFILT2)                                                   
C_ANFLT3 DS    CL(L'RSTFILT3)                                                   
C_ANFLT4 DS    CL(L'RSTFILT4)                                                   
C_ANFLT5 DS    CL(L'RSTFILT5)                                                   
C_ANLENQ EQU   *-C_ANACOD                                                       
                                                                                
N_NUMEL  DS    XL(NUMLN2Q)         ** NUMEL **                                  
         ORG   N_NUMEL+(NUMBEF-NUMELD)                                          
N_NUMBEF DS    CL(L'NUMBEF)                                                     
N_NUMAFT DS    CL(L'NUMAFT)                                                     
N_NUMTYP DS    CL(L'NUMTYPE)                                                    
         ORG   N_NUMEL+L'N_NUMEL                                                
                                                                                
A_ADREL  DS    XL(ADRLN5Q)         ** ADREL **                                  
         ORG   A_ADREL+(ADRADD1-ADRELD)                                         
A_ADDR1  DS    CL(L'ADRADD1)                                                    
A_ADDR2  DS    CL(L'ADRADD2)                                                    
A_ADDR3  DS    CL(L'ADRADD3)                                                    
A_ADDR4  DS    CL(L'ADRADD4)                                                    
A_ADDR5  DS    CL(L'ADRADD5)                                                    
         ORG   A_ADREL+L'A_ADREL                                                
                                                                                
O_OADEL  DS    XL(ADRLN5Q)         ** OADEL **                                  
         ORG   O_OADEL+(ADRADD1-ADRELD)                                         
O_OADDR1 DS    CL(L'ADRADD1)                                                    
O_OADDR2 DS    CL(L'ADRADD2)                                                    
O_OADDR3 DS    CL(L'ADRADD3)                                                    
O_OADDR4 DS    CL(L'ADRADD4)                                                    
O_OADDR5 DS    CL(L'ADRADD5)                                                    
         ORG   O_OADEL+L'O_OADEL                                                
                                                                                
P_PPREL  DS    XL(PPRLNXQ)         ** PPREL **                                  
         ORG   P_PPREL+(PPRGRUP-PPRELD)                                         
P_BILGRP DS    CL(L'PPRGRUP)                                                    
         ORG   P_PPREL+(PPRRECV-PPRELD)                                         
P_RECV   DS    XL(L'PPRRECVC)                                                   
         ORG   P_PPREL+(PPRRECVA-PPRELD)                                        
P_RECVBL DS    CL(L'PPRRECVA)                                                   
         ORG   P_PPREL+(PPRCOST-PPRELD)                                         
P_COST   DS    XL(L'PPRCOSTC)                                                   
         ORG   P_PPREL+(PPRCOSTA-PPRELD)                                        
P_COSTAC DS    CL(L'PPRCOSTA)                                                   
         ORG   P_PPREL+(PPRGAOFF-PPRELD)                                        
P_GAOFF  DS    CL(L'PPRGAOFF)                                                   
         ORG   P_PPREL+(PPRBTYPE-PPRELD)                                        
P_BILTYP DS    CL(L'PPRBTYPE)                                                   
         ORG   P_PPREL+(PPRBLAMT-PPRELD)                                        
P_BILAMT DS    CL(L'PPRBLAMT)                                                   
         ORG   P_PPREL+(PPRUFORA-PPRELD)                                        
P_UFORA  DS    CL(L'PPRUFORA)                                                   
         ORG   P_PPREL+(PPRUWRK-PPRELD)                                         
P_UWORK  DS    CL(PPRUWRKN*L'PPRUWRK)                                           
         ORG   P_PPREL+(PPRBILLP-PPRELD)                                        
P_BILPRT DS    CL(L'PPRBILLP)                                                   
         ORG   P_PPREL+(PPRNARRP-PPRELD)                                        
P_BILNR1 DS    CL(L'PPRNARRP/3)                                                 
P_BILNR2 DS    CL(L'PPRNARRP/3)                                                 
P_BILNR3 DS    CL(L'PPRNARRP/3)                                                 
         ORG   P_PPREL+L'P_PPREL                                                
                                                                                
M_PMDEL  DS    XL(PMDLN1Q)         ** PMDEL **                                  
         ORG   M_PMDEL+(PMDFBILL-PMDELD)                                        
M_FBILL  DS    CL(L'PMDFBILL)                                                   
         ORG   M_PMDEL+(PMDLBILL-PMDELD)                                        
M_LBILL  DS    CL(L'PMDLBILL)                                                   
         ORG   M_PMDEL+(PMDRBILL-PMDELD)                                        
M_RBILL  DS    CL(L'PMDRBILL)                                                   
         ORG   M_PMDEL+L'M_PMDEL                                                
                                                                                
S_RSTEL  DS    XL(RSTLN3Q)         ** RSTEL for main account **                 
         ORG   S_RSTEL+(RSTSECY-RSTELD)                                         
S_RSECY  DS    XL(L'RSTSECY)                                                    
         ORG   S_RSTEL+(RSTFILT1-RSTELD)                                        
S_RFILT1 DS    CL(L'RSTFILT1)                                                   
         ORG   S_RSTEL+(RSTFILT2-RSTELD)                                        
S_RFILT2 DS    CL(L'RSTFILT2)                                                   
         ORG   S_RSTEL+(RSTFILT3-RSTELD)                                        
S_RFILT3 DS    CL(L'RSTFILT3)                                                   
         ORG   S_RSTEL+(RSTFILT4-RSTELD)                                        
S_RFILT4 DS    CL(L'RSTFILT4)                                                   
         ORG   S_RSTEL+(RSTFILT5-RSTELD)                                        
S_RFILT5 DS    CL(L'RSTFILT5)                                                   
         ORG   S_RSTEL+(RSTSTAT1-RSTELD)                                        
S_RSTAT1 DS    XL(L'RSTSTAT1)                                                   
         ORG   S_RSTEL+(RSTSTAT2-RSTELD)                                        
S_RSTAT2 DS    XL(L'RSTSTAT2)                                                   
         ORG   S_RSTEL+(RSTSTAT3-RSTELD)                                        
S_RSTAT3 DS    XL(L'RSTSTAT3)                                                   
         ORG   S_RSTEL+(RSTSTAT4-RSTELD)                                        
S_RSTAT4 DS    XL(L'RSTSTAT4)                                                   
         ORG   S_RSTEL+(RSTSTAT5-RSTELD)                                        
S_RSTAT5 DS    XL(L'RSTSTAT5)                                                   
         ORG   S_RSTEL+(RSTSTAT6-RSTELD)                                        
S_RSTAT6 DS    XL(L'RSTSTAT6)                                                   
         ORG   S_RSTEL+(RSTCCTR-RSTELD)                                         
S_RCCTR  DS    XL(L'RSTCCTR)                                                    
         ORG   S_RSTEL+(RSTCCTRR-RSTELD)                                        
S_RCSTR  DS    XL(L'RSTCCTR)                                                    
         ORG   S_RSTEL+(RSTBDATE-RSTELD)                                        
S_RBBFD  DS    XL(L'RSTBDATE)                                                   
         ORG   S_RSTEL+(RSTTDATE-RSTELD)                                        
S_RLTDT  DS    XL(L'RSTTDATE)                                                   
         ORG   S_RSTEL+(RSTCOSTG-RSTELD)                                        
S_RCSTG  DS    XL(L'RSTCOSTG)                                                   
         ORG   S_RSTEL+(RSTSYSME-RSTELD)                                        
S_RSMDC  DS    XL(L'RSTSYSME)                                                   
         ORG   S_RSTEL+(RSTOFFC-RSTELD)                                         
S_ROFFC  DS    XL(L'RSTOFFC)                                                    
         ORG   S_RSTEL+(RSTX1099-RSTELD)                                        
S_RX1099 DS    XL(L'RSTX1099)                                                   
         ORG   S_RSTEL+(RSTDFTSK-RSTELD)                                        
S_RDFTTT DS    CL(L'RSTDFTSK)                                                   
         ORG   S_RSTEL+(RSTMAIL-RSTELD)                                         
S_RMAIL  DS    CL(L'RSTMAIL)                                                    
         ORG   S_RSTEL+(RSTLSBD-RSTELD)                                         
S_RLBILD DS    XL(L'RSTLSBD)                                                    
         ORG   S_RSTEL+(RSTLSTAT-RSTELD)                                        
S_RLSTAT DS    XL(L'RSTLSTAT)                                                   
         ORG   S_RSTEL+L'S_RSTEL                                                
                                                                                
A_RSTEL  DS    XL(RSTLN3Q)         ** RSTEL for subsidiary accounts **          
                                                                                
O_OTHEL  DS    XL(OTHLN1Q)         ** OTHEL **                                  
         ORG   O_OTHEL+(OTHNUM-OTHELD)                                          
O_NUM    DS    CL(L'OTHNUM)                                                     
         ORG   O_OTHEL+(OTHPROF-OTHELD)                                         
O_PROF   DS    CL(L'OTHPROF)                                                    
         ORG   O_OTHEL+(OTHNET-OTHELD)                                          
O_NET    DS    CL(L'OTHNET)                                                     
         ORG   O_OTHEL+L'O_OTHEL                                                
                                                                                
S_SCMEL  DS    XL256               ** SCMEL **                                  
         ORG   S_SCMEL+(SCMSEQ-SCMELD)                                          
S_SEQ    DS    XL(L'SCMSEQ)                                                     
         ORG   S_SCMEL+(SCMTYPE-SCMELD)                                         
S_TYPE   DS    XL(L'SCMTYPE)                                                    
         ORG   S_SCMEL+(SCMCODE-SCMELD)                                         
S_CDTX   DS    CL50                Comment code or text string                  
         ORG   S_SCMEL+L'S_SCMEL                                                
                                                                                
F_FFTEL  DS    XL256               ** FFTEL **                                  
         ORG   F_FFTEL+(FFTTYPE-FFTELD)                                         
F_TYPE   DS    XL(L'FFTTYPE)                                                    
         ORG   F_FFTEL+(FFTSEQ-FFTELD)                                          
F_SEQ    DS    XL(L'FFTSEQ)                                                     
         ORG   F_FFTEL+(FFTDATA-FFTELD)                                         
F_DATA   DS    CL250                                                            
         ORG   F_FFTEL+L'F_FFTEL                                                
                                                                                
O_OMEEL  DS    XL256               ** OMEEL **                                  
         ORG   O_OMEEL+(OMELN-OMEELD)                                           
O_MEMO   DS    XL(L'OMELN+L'OMEMO)                                              
         ORG   O_OMEEL+L'O_OMEEL                                                
                                                                                
P_PACEL  DS    XL(PACLNQ2)         ** PACEL **                                  
         ORG   P_PACEL+(PACPERS-PACELD)                                         
P_PERS   DS    CL(L'PACPERS)                                                    
         ORG   P_PACEL+(PACDATE-PACELD)                                         
P_ADAT   DS    XL(L'PACDATE)                                                    
         ORG   P_PACEL+(PACPERS2-PACELD)                                        
P_PER2   DS    CL(L'PACPERS2)                                                   
         ORG   P_PACEL+(PACDATE2-PACELD)                                        
P_ADAT2  DS    XL(L'PACDATE2)                                                   
         ORG   P_PACEL+L'P_PACEL                                                
                                                                                
A_SANEL  DS    XL(SANLNQ)          ** SANEL **                                  
         ORG   A_SANEL+(SANCODE+1-SANELD)                                       
A_SULA   DS    CL(L'SANCODE-1)                                                  
         ORG   A_SANEL+(SANNAME-SANELD)                                         
A_SNAME  DS    CL(L'SANNAME)                                                    
         ORG   A_SANEL+L'A_SANEL                                                
                                                                                
A_ABLEL  DS    XL(ABLLN3Q)         ** ABLEL **                                  
A_APOEL  DS    XL(APOLN1Q)         ** APOEL **                                  
A_RACEL  DS    XL(RACLNQ)          ** RACEL **                                  
                                                                                
P_PTREL  DS    XL(PTRLN1Q+L'ACCKEY)                                             
                                                                                
STACKL   DS    H                   Length of data in element stack              
                                                                                
Q_VALUEL EQU   *-Q_VALUES                                                       
                                                                                
TMPCOMM  DS    CL6                                                              
                                                                                
SAVEL    EQU   *-SAVED             Length of used saved area                    
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACLNKWRKD                                                      
PPRELD   DSECT ,                   Set length of longest PPREL                  
         ORG   PPRNARRP+L'PPRNARRP                                              
PPRLNXQ  EQU   *-PPRELD                                                         
       ++INCLUDE ACRECEQUS                                                      
       ++INCLUDE ACMSGEQUS                                                      
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE GESRCHBLKD                                                     
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACLNK11   07/29/20'                                      
         END                                                                    
