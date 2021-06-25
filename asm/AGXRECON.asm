*          DATA SET AGXRECON   AT LEVEL 012 AS OF 08/11/20                      
*PHASE AGXRECA                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE CARDS                                                                  
*INCLUDE CUREDIT                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE XSORT                                                                  
                                                                                
         TITLE 'AGXRECON - BulkAPI Reconciliation Process'                      
                                                                                
***********************************************************************         
* Who  Lvl Date    Change(s)                                 Audit              
* ---- --- ------- ----------------------------------------- ----------         
* YNGX 012 11AUG20 PREFIXED WITH NEGATIVE SIGN                                  
* YNGX 011 15JAN20 SKIP PEELED TRANSACTIONS                  DSRD-25135         
* YNGX 010 15Jan20 More fix on level 9                       DSRD-24769         
* YNGX 009 14Jan20 Fix exp trans reconciliation issue        DSRD-24769         
* MPEN 008 21Oct19 Return reconciliation tots for exp trans  DSRD-23404         
* MPEN 007 30May19 Fix for WHICHSYS= card                    ITMF-36661         
* TKLU 006 23Nov18 Remove CPYCURRS dependancy                ITMF-31367         
* TKLU 005 02Oct18 EU/UK typo fix                            DSRD-20365         
* TKLU 005 28Sep18 More MQRPT improvements                   SPEC-28142         
* TKLU 004 20Sep18 TIMEITMS/TIMIMULT addition                DSRD-20170         
* TKLU 003 19Sep18 MQ_ADV typo (informational only)          DSRD-19615         
* TKLU 002 07Sep18 QA DSN issue requires agency card change  ITMF-29112         
* TKLU 001 04Sep18 Initial Version: BulkAPI Reconciliation   DSRD-16206         
***********************************************************************         
*                                                                     *         
* This job reads all file records that create AGXTRACT facts and      *         
* summarizes them per agency and type, then sends this to MQ.         *         
*                                                                     *         
* Sample JCL: TKLU.DDS.JCL(AGXREC*)                                   *         
*                                                                     *         
* Note: As AGXTRACT doesn't use DXF/TDATEP/C filtering this isn't     *         
*       catered for in here.                                          *         
*                                                                     *         
* Possible parameter cards see CARDTAB, job specific are the MODE=T   *         
* for testing purposes and BREAK=Y to generate a client/product break *         
* down (*). AGENCY=(alphaID) allows to filter on a single agency.     *         
* *: Specific analysis code can be written using BREAK=S              *         
*                                                                     *         
***********************************************************************         
                                                                                
AGXRECON CSECT                                                                  
         PRINT NOGEN                                                            
         COPY  IEABRC                                                           
         NBASE WORKX-WORKD,**AGRC**,WORK=A(WORKC),CLEAR=YES                     
         USING WORKD,RC                                                         
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         J     MAIN                                                             
                                                                                
$$DATA   LOCTR ,                                                                
$$CODE   LOCTR ,                                                                
                                                                                
MAIN     DS    0H                                                               
                                                                                
         MVI   RETCODE,0                                                        
                                                                                
         GOTOR INITIAL             Init storage and validate cards              
                                                                                
         GOTOR OPENSYS,THISSENO    Open all files                               
                                                                                
         GOTOR RETCPYS             Retrieve applicable companies                
                                                                                
         USING AGYLSTD,R2                                                       
         LA    R2,AGYLIST                                                       
                                                                                
MAIN02   CLI   AGYLCPY,EOTQ        Loop through company table                   
         JE    MAIN04                                                           
                                                                                
         GOTOR GETRECS             Set record counts/totals                     
                                                                                
         AHI   R2,AGYLLNQ                                                       
         J     MAIN02                                                           
         DROP  R2                                                               
                                                                                
MAIN04   GOTOR PUTOUT              Put out company table                        
                                                                                
END      DS    0H                                                               
         GOTOR CLOSE               Close files                                  
                                                                                
         XBASE RC=RETCODE,RL=1                                                  
                                                                                
***********************************************************************         
* Subroutines                                                         *         
* -----------                                                         *         
***********************************************************************         
                                                                                
***********************************************************************         
* Put out data to datset and print totals, send MQ message.           *         
***********************************************************************         
                                                                                
PUTOUT   NTR1  ,                                                                
                                                                                
         MVC   WORK(3),=CL3'SE='                                                
         MVC   WORK+4(1),THISSENO                                               
         GOTO1 =V(DMDDNAME),DMCB,(X'24',=C'DDNAME'),WORK,0                      
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         L     RF,8(R1)            Get A(File Info list)                        
         MVC   CURSYS,2(RF)                                                     
         MVC   CURPRG,=CL8'AGXRECON'                                            
                                                                                
         LA    R2,DUBX                                                          
         EXTRACT (2),'S',FIELDS=TIOT                                            
         L     R2,DUBX                                                          
         MVC   DSNNAMO(4),0(R2)                                                 
                                                                                
         LA    RE,DSNNAMO          Set DSNNAME                                  
         LA    R1,L'DSNNAMO-1                                                   
         MVC   CURMQQ,GSPACES                                                   
                                                                                
         CLI   RUNMODE,RUNTSTQ     Local override for testing                   
         JE    PUTOUT02                                                         
                                                                                
         LA    RE,DSNNAMA                                                       
         LA    R1,L'DSNNAMA-1                                                   
         MVC   CURMQQ,MQ_ADV                                                    
         CLI   DSPACE,C'A'                                                      
         JE    PUTOUT02                                                         
         LA    R1,L'DSNNAMC-1                                                   
         LA    RE,DSNNAMC                                                       
         MVC   CURMQQ,MQ_CSC                                                    
         CLI   DSPACE,C'C'                                                      
         JE    PUTOUT02                                                         
         LA    R1,L'DSNNAMT-1                                                   
         LA    RE,DSNNAMT                                                       
         MVC   CURMQQ,MQ_TST                                                    
         CLI   DSPACE,C'T'                                                      
         JE    PUTOUT02                                                         
         LA    R1,L'DSNNAMQ-1                                                   
         LA    RE,DSNNAMQ                                                       
         MVC   CURMQQ,MQ_FQA                                                    
         CLI   DSPACE,C'Q'                                                      
         JNE   *+2                                                              
                                                                                
PUTOUT02 MVC   DSNNAME(0),0(RE)                                                 
         EXRL  R1,*-6                                                           
         LA    R3,DSNNAME+1(R1)                                                 
         MVI   0(R3),DELIMQ                                                     
         AHI   R3,1                                                             
                                                                                
*&&UK*&& MVC   0(L'EUQ,R3),EUQ                                                  
*&&UK*&& AHI   R3,L'EUQ                                                         
*&&US*&& MVC   0(L'NAQ,R3),NAQ                                                  
*&&US*&& AHI   R3,L'NAQ                                                         
         MVI   0(R3),DELIMQ                                                     
         AHI   R3,1                                                             
                                                                                
         MVC   0(L'CURSYS,R3),CURSYS                                            
         AHI   R3,L'CURSYS-1                                                    
         CLI   0(R3),SPACEQ                                                     
         JNH   PUTOUT04                                                         
         AHI   R3,1                                                             
                                                                                
PUTOUT04 MVI   0(R3),DELIMQ                                                     
         AHI   R3,1                                                             
         MVC   0(L'CURPRG,R3),CURPRG                                            
         AHI   R3,L'CURPRG                                                      
         MVI   0(R3),DELIMQ                                                     
         AHI   R3,1                                                             
                                                                                
         MVI   0(R3),DATEQ                                                      
         AHI   R3,1                                                             
         GOTOR VDATCON,DMCB,(5,0),(20,DUB)                                      
         MVC   0(6,R3),DUB+2       (YYMMDD)                                     
         AHI   R3,6                                                             
         MVI   0(R3),DELIMQ                                                     
         AHI   R3,1                                                             
         MVI   0(R3),TIMEQ                                                      
         AHI   R3,1                                                             
         TIME                                                                   
         LTR   R0,R0                                                            
         JZ    *+2                 BAD RETURN FROM MACRO                        
         ST    R0,FULL                                                          
                                                                                
         L     R1,FULL                                                          
         SRL   R1,28                                                            
         STC   R1,DUB+0                                                         
         OI    DUB+0,X'F0'                                                      
         L     R1,FULL                                                          
         SRL   R1,24                                                            
         STC   R1,DUB+1                                                         
         OI    DUB+1,X'F0'                                                      
         L     R1,FULL                                                          
         SRL   R1,20                                                            
         STC   R1,DUB+2                                                         
         OI    DUB+2,X'F0'                                                      
         L     R1,FULL                                                          
         SRL   R1,16                                                            
         STC   R1,DUB+3                                                         
         OI    DUB+3,X'F0'                                                      
         L     R1,FULL                                                          
         SRL   R1,12                                                            
         STC   R1,DUB+4                                                         
         OI    DUB+4,X'F0'                                                      
         L     R1,FULL                                                          
         SRL   R1,8                                                             
         STC   R1,DUB+5                                                         
         OI    DUB+5,X'F0'                                                      
         L     R1,FULL                                                          
         SRL   R1,4                                                             
         STC   R1,DUB+6                                                         
         OI    DUB+6,X'F0'                                                      
         MVC   0(6,R3),DUB                                                      
         AHI   R3,6                                                             
                                                                                
         GOTO1 VDYNALLO,DMCB,(X'80',D2NAME),(X'87',DSNSPACE),          +        
               (X'80',DSNNAME)                                                  
                                                                                
         OPEN  (DSNFILE,OUTPUT)    Open file for output                         
                                                                                
         USING AGYLSTD,R2                                                       
PUTOUT10 LA    R2,AGYLIST                                                       
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
         MVC   P(21),=CL21'Agency Reconciliation'                               
         GOTOR VPRINTER                                                         
         MVC   P(21),=CL21'---------------------'                               
         GOTOR VPRINTER                                                         
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
                                                                                
         USING CPYOUTD,R3                                                       
         USING CPY2UTD,R5                                                       
         LA    R3,ELEM                                                          
         LA    R5,ELEM2                                                         
         MVC   ELEM,GSPACES                                                     
         MVC   ELEM2,GSPACES                                                    
         MVC   CPYOREGH,H_REGION                                                
         MVC   CPYOALPH,H_ALPHA                                                 
         MVC   CPYOSYSH,H_SYSTEM                                                
         MVC   CPYOTDRH,H_TRNDR                                                 
         MVC   CPYOTCRH,H_TRNCR                                                 
         MVC   CPYOTIMH,H_TIM                                                   
         MVC   CPYOEXPH,H_EXP                                                   
         MVC   CPYOORDH,H_ORD                                                   
         MVC   CPYOESTH,H_EST                                                   
         MVC   CPY2EDRH,H_EDR                                                   
         MVC   CPY2ECRH,H_ECR                                                   
                                                                                
         MVC   P,CPYOUTD                                                        
         GOTOR VPRINTER                                                         
         MVC   P,CPY2UTD                                                        
         GOTOR VPRINTER                                                         
                                                                                
PUTOUT12 CLI   AGYLCPY,FFQ         Done?                                        
         JE    PUTOUT16                                                         
                                                                                
         MVC   ELEM,GSPACES                                                     
         MVC   ELEM2,GSPACES                                                    
         LA    R4,DSOUT                                                         
*                                                                               
         LR    R0,R4                                                            
         LHI   R1,L'DSOUT                                                       
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
*&&US*&& MVC   CPYOREG,NAQ                                                      
*&&UK*&& MVC   CPYOREG,EUQ                                                      
         MVC   0(L'CPYOREG,R4),CPYOREG                                          
         AHI   R4,L'CPYOREG                                                     
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOALP,AGYLALP                                                  
         MVC   0(L'CPYOALP,R4),CPYOALP                                          
         AHI   R4,L'CPYOALP                                                     
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOSYS,CURSYS                                                   
         MVC   0(L'CPYOSYS,R4),CPYOSYS                                          
         AHI   R4,L'CPYOSYS-1                                                   
         CLI   CPYOSYS+L'CPYOSYS-1,SPACEQ                                       
         JNH   PUTOUT14                                                         
         AHI   R4,1                                                             
                                                                                
PUTOUT14 MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,AGYLTDR),(L'CPYOTDR,CPYOTDR),2,ZERO=Y,FLOAT=-                
         CURED (P8,AGYLTDR),(L'CPYOTDR,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,AGYLTCR),(L'CPYOTCR,CPYOTCR),2,ZERO=Y,FLOAT=-                
         CURED (P8,AGYLTCR),(L'CPYOTCR,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,AGYLTIM),(L'CPYOTIM,CPYOTIM),2,ZERO=Y,FLOAT=-                
         CURED (P8,AGYLTIM),(L'CPYOTIM,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,AGYLEXP),(L'CPYOEXP,CPYOEXP),2,ZERO=Y,FLOAT=-                
         CURED (P8,AGYLEXP),(L'CPYOEXP,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,AGYLORD),(L'CPYOORD,CPYOORD),2,ZERO=Y,FLOAT=-                
         CURED (P8,AGYLORD),(L'CPYOORD,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,AGYLEST),(L'CPYOEST,CPYOEST),2,ZERO=Y,FLOAT=-                
         CURED (P8,AGYLEST),(L'CPYOEST,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,AGYLETD),(L'CPY2EDR,CPY2EDR),2,ZERO=Y,FLOAT=-                
         CURED (P8,AGYLETD),(L'CPY2EDR,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,AGYLETC),(L'CPY2ECR,CPY2ECR),2,ZERO=Y,FLOAT=-                
         CURED (P8,AGYLETC),(L'CPY2ECR,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
                                                                                
         PUT   DSNFILE,DSOUT       Move it out                                  
                                                                                
         MVC   P,CPYOUTD                                                        
         GOTOR VPRINTER                                                         
                                                                                
         MVC   P,CPY2UTD                                                        
         GOTOR VPRINTER                                                         
                                                                                
         AHI   R2,AGYLLNQ                                                       
         J     PUTOUT12                                                         
                                                                                
PUTOUT16 DS    0H                                                               
         DROP  R2,R3,R5                                                         
                                                                                
         CLOSE (DSNFILE)           Close file                                   
                                                                                
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
         MVC   P(18),=CL18'Reconciliation DSN'                                  
         GOTOR VPRINTER                                                         
         MVC   P(18),=CL18'------------------'                                  
         GOTOR VPRINTER                                                         
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
                                                                                
         MVC   P+1(8),=C'DSNFILE:'                                              
         MVC   P+10(L'DSNNAME),DSNNAME                                          
         GOTOR VPRINTER                                                         
                                                                                
         CLI   RUNMODE,RUNTSTQ                                                  
         JE    PUTOUT30                                                         
                                                                                
* MQRPT now (MEREPSX2 based, see also DSRD-19909)                               
                                                                                
PUTOUT20 MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
         MVC   P(10),=CL10'MQ process'                                          
         GOTOR VPRINTER                                                         
         MVC   P(10),=CL10'----------'                                          
         GOTOR VPRINTER                                                         
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
                                                                                
         MVI   BYTE,X'80'+X'20'    (X'01')                                      
         GOTOR VMQRPT,DMCB,MQOPEN,MYMQHDR,(BYTE,0)                              
         LA    RF,MQOPEN                                                        
         CLI   8(R1),0             All okay ?                                   
         JNE   PUTOUTER                                                         
                                                                                
         MVC   P(10),=C'MQOPEN  OK'                                             
         MVC   P+11(L'MYMQHDR),MYMQHDR                                          
         GOTOR VPRINTER                                                         
                                                                                
         MVC   MYFILE,DSNNAME                                                   
         LHI   RF,MYMSGLQ                                                       
         GOTOR VMQRPT,DMCB,MQPUT,MYRECTY,(RF)                                   
         LA    RF,MQPUT                                                         
         CLI   8(R1),0             All okay ?                                   
         JNE   PUTOUTER                                                         
                                                                                
         MVC   P(10),=C'MQPUT   OK'                                             
         MVC   P+11(L'MYRECTY),MYRECTY                                          
         MVC   P+11+L'MYRECTY+1(L'MYPREF+L'MYFILE),MYPREF                       
         GOTOR VPRINTER                                                         
                                                                                
         GOTOR VMQRPT,DMCB,MQCLOSE                                              
         LA    RF,MQCLOSE                                                       
         CLI   8(R1),0             All okay ?                                   
         JNE   PUTOUTER                                                         
                                                                                
         MVC   P(10),=C'MQCLOSE OK'                                             
         MVC   P+11(L'CURMQQ),CURMQQ                                            
         GOTOR VPRINTER                                                         
         MVI   RETCODE,1                                                        
                                                                                
         J     PUTOUT30                                                         
                                                                                
PUTOUTER MVC   P(11),=C'MQ Error on'                                            
         MVC   P+12(6),0(RF)                                                    
         GOTOR VPRINTER                                                         
         MVI   RETCODE,7                                                        
                                                                                
PUTOUT30 CLI   BREAK,YESQ                                                       
         JE    PUTOUT31                                                         
         CLI   BREAK,SPECIALQ                                                   
         JE    PUTOUT31                                                         
         CLI   BREAK,LEDGERQ                                                    
         JNE   PUTOUTX                                                          
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
         MVC   P(24),=CL24'Unit/Ledger breakdown'                               
         GOTOR VPRINTER                                                         
         MVC   P(24),=CL24'------------------------'                            
         GOTOR VPRINTER                                                         
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
         J     PUTOUT3A                                                         
                                                                                
PUTOUT31 MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
         MVC   P(24),=CL24'Client/Product breakdown'                            
         GOTOR VPRINTER                                                         
         MVC   P(24),=CL24'------------------------'                            
         GOTOR VPRINTER                                                         
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
                                                                                
PUTOUT3A OPEN  (BRKOUT,OUTPUT)     Open file for breakdown output               
                                                                                
         USING CPYOUTD,R3                                                       
         USING CPY2UTD,R5                                                       
         LA    R3,ELEM                                                          
         LA    R5,ELEM2                                                         
         MVC   ELEM,GSPACES                                                     
         LA    R4,DSOUT                                                         
*                                                                               
         LR    R0,R4                                                            
         LHI   R1,L'DSOUT                                                       
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   CPYOREGH,H_REGION                                                
         MVC   0(L'CPYOREGH,R4),CPYOREGH                                        
         AHI   R4,L'CPYOREGH       REGION EU/NA                                 
                                                                                
PUTOUT32 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT34                                                         
         JCT   R4,PUTOUT32                                                      
                                                                                
PUTOUT34 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOALPH,H_ALPHA    ALPHA                                        
         MVC   0(L'CPYOALPH,R4),CPYOALPH                                        
         AHI   R4,L'CPYOALPH                                                    
                                                                                
PUTOUT36 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT38                                                         
         JCT   R4,PUTOUT36                                                      
                                                                                
PUTOUT38 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOSYSH,H_SYSTEM   SYSTEM                                       
         MVC   0(L'CPYOSYSH,R4),CPYOSYSH                                        
         AHI   R4,L'CPYOSYSH                                                    
                                                                                
PUTOUT40 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT42                                                         
         JCT   R4,PUTOUT40                                                      
                                                                                
PUTOUT42 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOCLIH,H_CLI      CLIENT                                       
         MVC   0(L'CPYOCLIH,R4),CPYOCLIH                                        
         AHI   R4,L'CPYOCLIH                                                    
                                                                                
PUTOUT44 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT44                                                         
         JCT   R4,PUTOUT46                                                      
                                                                                
PUTOUT46 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOPROH,H_PRO      PRODUCT                                      
         MVC   0(L'CPYOPROH,R4),CPYOPROH                                        
         AHI   R4,L'CPYOPROH                                                    
                                                                                
PUTOUT48 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT50                                                         
         JCT   R4,PUTOUT48                                                      
                                                                                
PUTOUT50 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOTDRH,H_TRNDR    SJ DEBIT TOTAL                               
         MVC   0(L'CPYOTDRH,R4),CPYOTDRH                                        
         AHI   R4,L'CPYOTDRH                                                    
                                                                                
PUTOUT52 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT54                                                         
         JCT   R4,PUTOUT52                                                      
                                                                                
PUTOUT54 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOTCRH,H_TRNCR    SJ CREDIT TOTAL                              
         MVC   0(L'CPYOTCRH,R4),CPYOTCRH                                        
         AHI   R4,L'CPYOTCRH                                                    
                                                                                
PUTOUT56 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT58                                                         
         JCT   R4,PUTOUT56                                                      
                                                                                
PUTOUT58 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOTIMH,H_TIM      TIME TOTAL                                   
         MVC   0(L'CPYOTIMH,R4),CPYOTIMH                                        
         AHI   R4,L'CPYOTIMH                                                    
                                                                                
PUTOUT60 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT62                                                         
         JCT   R4,PUTOUT60                                                      
                                                                                
PUTOUT62 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOEXPH,H_EXP      EXPENSE TOTAL                                
         MVC   0(L'CPYOEXPH,R4),CPYOEXPH                                        
         AHI   R4,L'CPYOEXPH                                                    
                                                                                
PUTOUT64 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT66                                                         
         JCT   R4,PUTOUT64                                                      
                                                                                
PUTOUT66 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOORDH,H_ORD      ORDER TOTAL                                  
         MVC   0(L'CPYOORDH,R4),CPYOORDH                                        
         AHI   R4,L'CPYOORDH                                                    
                                                                                
PUTOUT68 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT69                                                         
         JCT   R4,PUTOUT68                                                      
                                                                                
PUTOUT69 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOESTH,H_EST      ESTIMATE TOTAL                               
         MVC   0(L'CPYOESTH,R4),CPYOESTH                                        
         AHI   R4,L'CPYOESTH                                                    
                                                                                
PUTOUT70 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT71                                                         
         JCT   R4,PUTOUT70                                                      
                                                                                
PUTOUT71 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPY2EDRH,H_EDR      EXP TRANSACTION DEBIT                        
         MVC   0(L'CPY2EDRH,R4),CPY2EDRH                                        
         AHI   R4,L'CPY2EDRH                                                    
                                                                                
PUTOUT72 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT73                                                         
         JCT   R4,PUTOUT72                                                      
                                                                                
PUTOUT73 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPY2ECRH,H_ECR      EXP TRANSACTION CREDIT                       
         MVC   0(L'CPY2ECRH,R4),CPY2ECRH                                        
         AHI   R4,L'CPY2ECRH                                                    
                                                                                
PUTOUT74 CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT75                                                         
         JCT   R4,PUTOUT74                                                      
                                                                                
PUTOUT75 AHI   R4,1                                                             
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
                                                                                
***      CLI   BREAK,SPECIALQ                                                   
***      JNE   PUTOUT78                                                         
***      MVC   0(14,R4),=C'Job;1RAccount;'                                      
                                                                                
PUTOUT78 PUT   BRKOUT,DSOUT                                                     
                                                                                
         MVC   P,CPYOUTD                                                        
         GOTOR VPRINTER                                                         
                                                                                
         MVC   P,CPY2UTD                                                        
         GOTOR VPRINTER                                                         
                                                                                
         USING BRKBUFD,R2                                                       
         SAM31 ,                                                                
         L     R2,ABRKBUF                                                       
         LHI   RF,1                                                             
                                                                                
PUTOUT80 CLI   BRKBCPY,EOTQ        count entris                                 
         JE    PUTOUT82                                                         
         AHI   RF,1                                                             
         AHI   R2,BRKBLNQ                                                       
         J     PUTOUT80                                                         
                                                                                
PUTOUT82 L     R2,ABRKBUF                                                       
         SAM24 .                                                                
         GOTO1 VXSORT,DMCB,X'00FFFFFF',(RF),BRKBLNQ,BRKBKLQ,0,(R2)              
         SAM31 ,                                                                
                                                                                
PUTOUT84 CLI   BRKBCPY,EOTQ                                                     
         JE    PUTOUT98                                                         
                                                                                
         MVC   TEMP,BRKBUFD                                                     
         ST    R2,SAVER2                                                        
         SAM24 .                                                                
         LA    R2,TEMP                                                          
                                                                                
         LA    R3,ELEM                                                          
         MVC   ELEM,GSPACES                                                     
         LA    R4,DSOUT                                                         
*                                                                               
         LR    R0,R4                                                            
         LHI   R1,L'DSOUT                                                       
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
*&&US*&& MVC   CPYOREG,NAQ                                                      
*&&UK*&& MVC   CPYOREG,EUQ                                                      
         MVC   0(L'CPYOREG,R4),CPYOREG                                          
         AHI   R4,L'CPYOREG                                                     
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOALP,BRKBALP                                                  
         MVC   0(L'CPYOALP,R4),CPYOALP                                          
         AHI   R4,L'CPYOALP                                                     
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOSYS,CURSYS                                                   
         MVC   0(L'CPYOSYS,R4),CPYOSYS                                          
         AHI   R4,L'CPYOSYS-1                                                   
         CLI   CPYOSYS+L'CPYOSYS-1,SPACEQ                                       
         JNH   PUTOUT86                                                         
         AHI   R4,1                                                             
                                                                                
PUTOUT86 MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOCLI,BRKBCLI                                                  
         CLC   BRKBCLI,GSPACES                                                  
         JH    PUTOUT88                                                         
         MVI   CPYOCLI+0,L_CHEVQ                                                
         MVC   CPYOCLI+1(2),BRKBUNT                                             
         MVI   CPYOCLI+3,R_CHEVQ                                                
                                                                                
PUTOUT88 MVC   0(L'CPYOCLI,R4),CPYOCLI                                          
                                                                                
PUTOUT90 AHI   R4,1                                                             
         CLI   0(R4),SPACEQ                                                     
         JH    PUTOUT90                                                         
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         MVC   CPYOPRO,BRKBPRO                                                  
         MVC   0(L'CPYOPRO,R4),CPYOPRO                                          
                                                                                
PUTOUT92 CLI   0(R4),SPACEQ                                                     
         JNH   PUTOUT94                                                         
         AHI   R4,1                                                             
         J     PUTOUT92                                                         
                                                                                
PUTOUT94 MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,BRKBTDR),(L'CPYOTDR,CPYOTDR),2,ZERO=Y,FLOAT=-                
         CURED (P8,BRKBTDR),(L'CPYOTDR,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,BRKBTCR),(L'CPYOTCR,CPYOTCR),2,ZERO=Y,FLOAT=-                
         CURED (P8,BRKBTCR),(L'CPYOTCR,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,BRKBTIM),(L'CPYOTIM,CPYOTIM),2,ZERO=Y,FLOAT=-                
         CURED (P8,BRKBTIM),(L'CPYOTIM,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,BRKBEXP),(L'CPYOEXP,CPYOEXP),2,ZERO=Y,FLOAT=-                
         CURED (P8,BRKBEXP),(L'CPYOEXP,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,BRKBORD),(L'CPYOORD,CPYOORD),2,ZERO=Y,FLOAT=-                
         CURED (P8,BRKBORD),(L'CPYOORD,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,BRKBEST),(L'CPYOEST,CPYOEST),2,ZERO=Y,FLOAT=-                
         CURED (P8,BRKBEST),(L'CPYOEST,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,BRKBEDR),(L'CPY2EDR,CPY2EDR),2,ZERO=Y,FLOAT=-                
         CURED (P8,BRKBEDR),(L'CPY2EDR,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
         CURED (P8,BRKBECR),(L'CPY2ECR,CPY2ECR),2,ZERO=Y,FLOAT=-                
         CURED (P8,BRKBECR),(L'CPY2ECR,0(R4)),2,ZERO=Y,FLOAT=-,        +        
               ALIGN=LEFT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),SEPARQ                                                     
         AHI   R4,1                                                             
                                                                                
         PUT   BRKOUT,DSOUT       Move it out                                   
                                                                                
         MVC   P,CPYOUTD                                                        
         GOTOR VPRINTER                                                         
                                                                                
         MVC   P,CPY2UTD                                                        
         GOTOR VPRINTER                                                         
                                                                                
***      CLI   BREAK,SPECIALQ                                                   
***      JNE   PUTOUT96                                                         
***      MVC   ELEM,GSPACES       Now show counts and more details              
***      MVC   CPYOCLI(L'BRKBJOB),BRKBJOB                                       
***      MVC   CPYOPRO(L'BRKBNUM),BRKBNUM                                       
***      CURED (P6,BRKBTD#),(L'CPYOTDR-4,CPYOTDR+4),0,ZERO=Y,FLOAT=-            
***      CURED (P6,BRKBTC#),(L'CPYOTCR-4,CPYOTCR+4),0,ZERO=Y,FLOAT=-            
***      CURED (P6,BRKBTI#),(L'CPYOTIM-4,CPYOTIM+4),0,ZERO=Y,FLOAT=-            
***      CURED (P6,BRKBEX#),(L'CPYOEXP-4,CPYOEXP+4),0,ZERO=Y,FLOAT=-            
***      CURED (P6,BRKBOR#),(L'CPYOORD-4,CPYOORD+4),0,ZERO=Y,FLOAT=-            
***      CURED (P6,BRKBES#),(L'CPYOEST-4,CPYOEST+4),0,ZERO=Y,FLOAT=-            
***                                                                             
***      MVC   P,CPYOUTD                                                        
***      GOTOR VPRINTER                                                         
                                                                                
PUTOUT96 SAM31 ,                                                                
         L     R2,SAVER2                                                        
         AHI   R2,BRKBLNQ                                                       
         J     PUTOUT84                                                         
         DROP  R2                                                               
                                                                                
PUTOUT98 SAM24 ,                                                                
                                                                                
         CLOSE (BRKOUT)            Close file                                   
                                                                                
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
                                                                                
***      no alternate DSNFILE CLOSE                                             
                                                                                
PUTOUTX  DS    0H                                                               
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Routine to open all relevant accounting files for current system.   *         
***********************************************************************         
                                                                                
OPENSYS  NTR1  ,                                                                
                                                                                
         L     RF,VUTL                                                          
         MVC   4(1,RF),0(R1)       Set UTL Value                                
         GOTOR VDATAMGR,DMCB,DMOPEN,ACCSYS,OPENLST,AIO3                         
                                                                                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Retrieve applicable companies in AGYLIST                            *         
***********************************************************************         
                                                                                
RETCPYS  NTR1  ,                                                                
* Build table of companies to be processed (AGYLIST)                            
                                                                                
         USING AGYLSTD,R2                                                       
         LA    R2,AGYLIST                                                       
         MVI   AGYLCPY,EOTQ                                                     
         LHI   R3,AGYLMXQ                                                       
                                                                                
         MVI   KEYSAVE,C' '                                                     
         MVC   KEYSAVE+1(L'KEYSAVE-1),KEYSAVE                                   
                                                                                
DIR      USING CPYRECD,KEY                                                      
RETCPY02 LLC   R1,KEYSAVE                                                       
         AHI   R1,1                                                             
         STC   R1,KEYSAVE                                                       
         CHI   R1,X'FE'                                                         
         JE    RETCPY20                                                         
                                                                                
         MVC   DIR.CPYKEY,KEYSAVE                                               
         GOTOR VDATAMGR,DMCB,DMREAD,ACCDIR,DIR.CPYKEY,DIR.CPYKEY                
         JNE   RETCPY02                                                         
         GOTOR VDATAMGR,DMCB,GETREC,ACCMST,DIR.CPYKDA,AIO2,DMWORK               
         JNE   *+2                                                              
                                                                                
         USING CPYRECD,R4                                                       
         L     R4,AIO2                                                          
         USING CPYELD,R5                                                        
         LA    R5,CPYRFST                                                       
                                                                                
RETCPY04 CLI   CPYEL,CPYELQ                                                     
         JE    RETCPY06                                                         
         CLI   CPYEL,0                                                          
         JE    *+2                                                              
         LLC   R0,CPYLN                                                         
         AR    R5,R0                                                            
         J     RETCPY04                                                         
                                                                                
RETCPY06 OC    FLTCPY1,FLTCPY1     Card filter?                                 
         JZ    RETCPY08                                                         
         CLC   FLTCPY1,CPYALPHA                                                 
         JE    RETCPY08                                                         
         OC    FLTCPY2,FLTCPY2                                                  
         JZ    RETCPY02                                                         
         CLC   FLTCPY2,CPYALPHA                                                 
         JE    RETCPY08                                                         
         OC    FLTCPY3,FLTCPY3                                                  
         JZ    RETCPY02                                                         
         CLC   FLTCPY3,CPYALPHA                                                 
         JNE   RETCPY02                                                         
                                                                                
RETCPY08 CLI   CPYLN,CPYSTATD-CPYELD                                            
         JL    RETCPY02                                                         
         TM    CPYSTATD,CPYSGPDX                                                
         JZ    RETCPY02                                                         
                                                                                
         MVC   AGYLCPY,CPYKCPY                                                  
         MVC   AGYLALP,CPYALPHA                                                 
         ZAP   AGYLTDR,PZERO                                                    
         ZAP   AGYLTCR,PZERO                                                    
         ZAP   AGYLTIM,PZERO                                                    
         ZAP   AGYLEXP,PZERO                                                    
         ZAP   AGYLORD,PZERO                                                    
         ZAP   AGYLEST,PZERO                                                    
         ZAP   AGYLETD,PZERO                                                    
         ZAP   AGYLETC,PZERO                                                    
         XC    AGYLCLL(3),AGYLCLL                                               
                                                                                
*        MVI   AGYLCTR,CTRYGBR     set country based on currencies              
*        CLC   CPYCURR,EUROQ       (not used so far)                            
*        JNE   RETCPY10                                                         
*        CLC   CPYCURRS,SPACES     not set?                                     
*        JH    RETCPY09                                                         
*        GOTOR GETCTRY,CPYALPHA                                                 
*        MVC   AGYLCTR,BYTE                                                     
*        J     RETCPY10                                                         
*                                                                               
*ETCPY09 MVI   AGYLCTR,CTRYGER                                                  
*        CLC   CPYCURRS,DEMQ                                                    
*        JE    RETCPY10                                                         
*        MVI   AGYLCTR,CTRYIRE                                                  
*        CLC   CPYCURRS,IEPQ                                                    
*        JNE   *+2                 ???                                          
         DROP  R5                                                               
                                                                                
RETCPY10 CLI   BREAK,YESQ                                                       
         JE    RETCPY11                                                         
         CLI   BREAK,SPECIALQ                                                   
         JNE   RETCPY18                                                         
                                                                                
DIR      USING LDGRECD,KEY                                                      
RETCPY11 MVC   DIR.LDGKEY,GSPACES                                               
         MVC   DIR.LDGKCPY,AGYLCPY                                              
         MVC   DIR.LDGKUNT(2),SJUL                                              
                                                                                
         GOTOR VDATAMGR,DMCB,DMREAD,ACCDIR,DIR.LDGKEY,DIR.LDGKEY                
         JNE   *+2                 (Company has no SJ ledger)                   
         GOTOR VDATAMGR,DMCB,GETREC,ACCMST,DIR.LDGKDA,AIO2,DMWORK               
         JNE   *+2                                                              
                                                                                
         USING ACLELD,R5                                                        
         L     RF,AIO2                                                          
         LA    R5,LDGRFST-LDGRECD(RF)                                           
                                                                                
RETCPY12 CLI   ACLEL,0                                                          
         JE    *+2                 (SJ ledger has no levels)                    
         CLI   ACLEL,ACLELQ                                                     
         JE    RETCPY14                                                         
         LLC   RE,ACLLN                                                         
         AR    R5,RE                                                            
         J     RETCPY12                                                         
                                                                                
RETCPY14 MVC   AGYLCLL,ACLELLVA                                                 
         LLC   RE,ACLELLVB                                                      
         LLC   RF,ACLELLVA                                                      
         SR    RE,RF                                                            
         STC   RE,AGYLPRL                                                       
         LLC   RE,ACLELLVC                                                      
         LLC   RF,ACLELLVB                                                      
         SR    RE,RF                                                            
         CHI   RE,L'ESTKJOB                                                     
         JNH   RETCPY16                                                         
         LHI   RE,L'ESTKJOB                                                     
                                                                                
RETCPY16 STC   RE,AGYLJOL                                                       
         DROP  DIR,R5                                                           
                                                                                
RETCPY18 AHI   R2,AGYLLNQ                                                       
         MVI   AGYLCPY,EOTQ                                                     
                                                                                
         JCT   R3,RETCPY02                                                      
                                                                                
         MVC   P(28),=CL28'Increase of AGYLMXQ required'                        
         LA    R5,522                                                           
         GOTOR VPRINTER                                                         
         ABEND (R5),DUMP                                                        
                                                                                
RETCPY20 LA    R2,AGYLIST                                                       
         CLI   AGYLCPY,EOTQ                                                     
         JE    CARDER7                                                          
         J     EXIT                                                             
         DROP  R2,R4                                                            
                                                                                
***********************************************************************         
* Set BYTE from CPYALPHA                                              *         
***********************************************************************         
                                                                                
*ETCTRY  NTR1  ,                                                                
*                                                                               
*        MVI   BYTE,CTRYIRE                                                     
*                                                                               
*        USING CT5REC,R3                                                        
*        LA    R3,KEY                                                           
*        XC    CT5KEY,CT5KEY       READ FOR ACCESS RECORD                       
*        MVI   CT5KTYP,CT5KTYPQ                                                 
*        MVC   CT5KALPH,0(R1)                                                   
*                                                                               
*        GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT5REC,AIO2                          
*        JNE   *+2                                                              
*                                                                               
*        USING CTAGDD,R5                                                        
*        L     R3,AIO2                                                          
*        LA    R5,CT5DATA                                                       
*                                                                               
*CTRY2   CLI   CTAGDEL,CTAGDELQ                                                 
*        JE    GCTRY4                                                           
*        CLI   CTAGDEL,0                                                        
*        JE    GCTRYX                                                           
*        LLC   R1,CTAGDLEN                                                      
*        AR    R5,R1                                                            
*        J     GCTRY2                                                           
*                                                                               
*CTRY4   CLI   CTAGDLEN,CTAGDL2Q                                                
*        JL    GCTRYX                                                           
*        CLI   CTAGDCTY,CTRYGER                                                 
*        JNE   GCTRYX                                                           
*        MVI   BYTE,CTRYGER                                                     
*                                                                               
*CTRYX   DS    0H                                                               
*        J     EXIT                                                             
*        DROP  R3,R5                                                            
                                                                                
***********************************************************************         
* Get records for current company                                     *         
***********************************************************************         
                                                                                
         USING AGYLSTD,R2                                                       
         USING TRNTABD,R7                                                       
GETRECS  NTR1  ,                                                                
         LA    R7,TRNTAB                                                        
*                                                                               
         USING TRNRECD,R3                                                       
GR_TRN00 LA    R3,KEY              SB/SE/SJ/SQ Transactions                     
         MVC   TRNKEY,GSPACES      ------------------------                     
         MVC   TRNKCPY,AGYLCPY                                                  
         MVC   TRNKULA(L'TRNTULG),TRNTULG                                       
         OI    TRNKACT,X'01'                                                    
         MVC   KEYSAVE,KEY                                                      
                                                                                
GR_TRN02 GOTOR VDATAMGR,DMCB,DMRDHI,ACCDIR,TRNKEY,TRNKEY                        
         J     GR_TRN06                                                         
                                                                                
GR_TRN04 LA    R3,KEY                                                           
         GOTOR VDATAMGR,DMCB,DMRSEQ,ACCDIR,TRNKEY,TRNKEY                        
                                                                                
GR_TRN06 JNE   *+2                                                              
         CLC   KEYSAVE(TRNKACT-TRNRECD),TRNKEY                                  
         JNE   GR_TRN30                                                         
                                                                                
         GOTO1 VRECTYP,DMCB,(C'I',TRNRECD)                                      
         CLI   0(R1),ACRTTRN                                                    
         JE    *+12                                                             
         CLI   0(R1),ACRTTRNA                                                   
         JNE   GR_TRN04                                                         
         MVC   BYTE,0(R1)          SAVED RECORD TYPE                            
*                                                                               
         TM    TRNKSTAT,TRNSDRFT   no drafts                                    
         JZ    GR_TRN08                                                         
*&&UK                                                                           
         CLC   TRNKUNT(2),SJUL                                                  
         JNE   GR_TRN04                                                         
         CLI   TRNKSTYP,TRNTFBAD   unless advance billing on prod               
         JNE   GR_TRN04                                                         
*&&                                                                             
*&&US*&& J     GR_TRN04                                                         
                                                                                
GR_TRN08 DS    0H                                                               
* Note: AC98 set TRNSPEEL on Archived directory only in EU                      
         TM    TRNKSTA2,TRNSPEEL   skip peeled transctions                      
         JNZ   GR_TRN04                                                         
         CLI   TRNKSTYP,TRNTORD    skip order transctions                       
         JE    GR_TRN04                                                         
*                                                                               
         CLI   BYTE,ACRTTRN                                                     
         JE    GR_TRN10                                                         
         CLI   BYTE,ACRTTRNA                                                    
         JNE   GR_TRN04                                                         
                                                                                
         GOTOR VDATAMGR,DMCB,GETREC,ACCARC,TRNKDA,AIO2,DMWORK                   
         J     GR_TRN12                                                         
                                                                                
GR_TRN10 GOTOR VDATAMGR,DMCB,GETREC,ACCMST,TRNKDA,AIO2,DMWORK                   
                                                                                
GR_TRN12 JNE   *+2                                                              
         L     R3,AIO2                                                          
                                                                                
         USING TRNELD,R4                                                        
         LA    R4,TRNRFST                                                       
         CLI   TRNEL,TRNELQ                                                     
         JNE   *+2                 (???)                                        
                                                                                
         LLC   RE,TRNTDDA                                                       
         TM    TRNSTAT,TRNSDR                                                   
         JNZ   GR_TRN14                                                         
         LLC   RE,TRNTDDC                                                       
                                                                                
GR_TRN14 AR    RE,R2               Displacement to accumulator                  
         AP    0(L'AGYLTDR,RE),TRNAMNT                                          
                                                                                
         CLI   BREAK,YESQ                                                       
         JNE   GR_TRN04                                                         
*                                                                               
         GOTOR SAVBRK,DMCB,(TRNTREQ,TRNRECD),TRNELD                             
         J     GR_TRN04                                                         
*                                                                               
GR_TRN30 AHI   R7,TRNTABL           Bump to next transaction type               
         CLI   0(R7),X'FF'                                                      
         JNE   GR_TRN00                                                         
         DROP  R3,R4                                                            
                                                                                
         USING TSWRECD,R3                                                       
GR_TIM   LA    R3,KEY              Time                                         
         XC    TSWKEY,TSWKEY       ----                                         
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,AGYLCPY                                                  
         MVC   KEYSAVE,KEY                                                      
                                                                                
GR_TIM02 GOTOR VDATAMGR,DMCB,DMRDHI,ACCDIR,TSWKEY,TSWKEY                        
         J     GR_TIM06                                                         
                                                                                
GR_TIM04 LA    R3,KEY                                                           
         GOTOR VDATAMGR,DMCB,DMRSEQ,ACCDIR,TSWKEY,TSWKEY                        
                                                                                
GR_TIM06 JNE   *+2                                                              
         CLC   KEYSAVE(TSWKPER-TSWKEY),TSWKEY                                   
         JNE   GR_EXP                                                           
                                                                                
         GOTOR VDATAMGR,DMCB,GETREC,ACCMST,TSWKDA,AIO2,DMWORK                   
         JNE   *+2                                                              
                                                                                
         USING TIMRECD,R3                                                       
         L     R3,AIO2                                                          
                                                                                
         CLC   TIMKULC,GSPACES     Skip t/s if no c/a                           
         JNH   GR_TIM04                                                         
                                                                                
         USING TIMELD,R4                                                        
         LA    R4,TIMRFST                                                       
                                                                                
GR_TIM08 CLI   TIMEL,0                                                          
         JE    GR_TIM04                                                         
         CLI   TIMEL,TIMELQ                                                     
         JNE   GR_TIM14                                                         
         CLI   TIMETYP,TIMEITMS                                                 
         JE    GR_TIM10                                                         
         CLI   TIMETYP,TIMEINP                                                  
         JNE   GR_TIM14                                                         
         CLI   TIMLN,TIMILN1Q                                                   
         JL    GR_TIM14                                                         
                                                                                
         AP    AGYLTIM,TIMHRS                                                   
         J     GR_TIM12                                                         
                                                                                
GR_TIM10 AP    AGYLTIM,TIMIMULT                                                 
                                                                                
GR_TIM12 CLI   BREAK,YESQ                                                       
         JNE   GR_TIM14                                                         
                                                                                
         GOTOR SAVBRK,DMCB,('ACRTTIM',TIMRECD),TIMELD                           
                                                                                
GR_TIM14 LLC   R1,TIMLN                                                         
         AR    R4,R1                                                            
         J     GR_TIM08                                                         
         DROP  R3,R4                                                            
                                                                                
         USING EXCRECD,R3                                                       
GR_EXP   LA    R3,KEY              Expenses                                     
         XC    EXCKEY,EXCKEY       --------                                     
         MVI   EXCKTYP,EXCKTYPQ                                                 
         MVI   EXCKSUB,EXCKSUBQ                                                 
         MVC   EXCKCPY,AGYLCPY                                                  
         MVC   KEYSAVE,KEY                                                      
                                                                                
GR_EXP02 GOTOR VDATAMGR,DMCB,DMRDHI,ACCDIR,EXCKEY,EXCKEY                        
         J     GR_EXP06                                                         
                                                                                
GR_EXP04 LA    R3,KEY                                                           
         GOTOR VDATAMGR,DMCB,DMRSEQ,ACCDIR,EXCKEY,EXCKEY                        
                                                                                
GR_EXP06 JNE   *+2                                                              
         CLC   KEYSAVE(EXCKPIDB-EXCKEY),EXCKEY                                  
         JNE   GR_ORD                                                           
                                                                                
         GOTOR VDATAMGR,DMCB,GETREC,ACCMST,EXCKDA,AIO2,DMWORK                   
         JNE   *+2                                                              
                                                                                
         L     R3,AIO2                                                          
         USING CIDELD,R4                                                        
         LA    R4,EXCRFST                                                       
                                                                                
GR_EXP08 CLI   CIDEL,0                                                          
         JE    GR_EXP04                                                         
         CLI   CIDEL,CIDELQ                                                     
         JNE   GR_EXP10                                                         
         CLI   CIDTYPE,CIDTYMQ                                                  
         JNE   GR_EXP10                                                         
         TM    CIDMSTA,CIDMSID     (skip if deleted)                            
         JNZ   GR_EXP10                                                         
                                                                                
         GOTOR GETXSJ                                                           
         JNE   GR_EXP10                                                         
                                                                                
         AP    AGYLEXP,CIDMAMT                                                  
                                                                                
         CLI   BREAK,YESQ                                                       
         JNE   GR_EXP10                                                         
                                                                                
         GOTOR SAVBRK,DMCB,('ACRTEXPC',EXCRECD),CIDELD                          
                                                                                
GR_EXP10 LLC   R1,CIDLN                                                         
         AR    R4,R1                                                            
         J     GR_EXP08                                                         
         DROP  R3,R4                                                            
                                                                                
         USING ORDRECD,R3                                                       
GR_ORD   LA    R3,KEY              Orders                                       
         XC    ORDKEY,ORDKEY       ------                                       
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,AGYLCPY                                                  
         MVC   KEYSAVE,KEY                                                      
                                                                                
GR_ORD02 GOTOR VDATAMGR,DMCB,DMRDHI,ACCDIR,ORDKEY,ORDKEY                        
         J     GR_ORD06                                                         
                                                                                
GR_ORD04 LA    R3,KEY                                                           
         GOTOR VDATAMGR,DMCB,DMRSEQ,ACCDIR,ORDKEY,ORDKEY                        
                                                                                
GR_ORD06 JNE   *+2                                                              
         CLC   KEYSAVE(ORDKORD-ORDKEY),ORDKEY                                   
         JNE   GR_EST                                                           
                                                                                
         CLC   ORDKORD,ORDCNT                                                   
         JE    GR_ORD04                                                         
         CLI   ORDKSEQ,0                                                        
         JNE   GR_ORD04                                                         
                                                                                
         GOTOR VDATAMGR,DMCB,GETREC,ACCMST,ORDKDA,AIO2,DMWORK                   
         JNE   *+2                                                              
                                                                                
         L     R3,AIO2                                                          
         USING OAMELD,R4                                                        
         LA    R4,ORDRFST                                                       
                                                                                
         GOTOR GETOSJ                                                           
         JNE   GR_ORD04                                                         
                                                                                
GR_ORD08 CLI   OAMEL,OAMELQ                                                     
         JNE   GR_ORD10                                                         
                                                                                
         AP    AGYLORD,OAMAMNT                                                  
                                                                                
         CLI   BREAK,YESQ                                                       
         JNE   GR_ORD10                                                         
                                                                                
GR_ORD09 GOTOR SAVBRK,DMCB,('ACRTORD',ORDRECD),OAMELD                           
                                                                                
GR_ORD10 CLI   OAMEL,0                                                          
         JE    GR_ORD04                                                         
         LLC   R1,OAMLN                                                         
         AR    R4,R1                                                            
         J     GR_ORD08                                                         
         DROP  R3,R4                                                            
                                                                                
         USING ESTRECD,R3                                                       
GR_EST   LA    R3,KEY              Estimates                                    
         XC    ESTKEY,ESTKEY       ---------                                    
         MVI   ESTKTYP,ESTKTYPQ                                                 
         MVI   ESTKSUB,ESTKSUBQ                                                 
         MVC   ESTKCPY,AGYLCPY                                                  
         MVC   KEYSAVE,KEY                                                      
                                                                                
GR_EST02 GOTOR VDATAMGR,DMCB,DMRDHI,ACCDIR,ESTKEY,ESTKEY                        
         J     GR_EST06                                                         
                                                                                
GR_EST04 LA    R3,KEY                                                           
         GOTOR VDATAMGR,DMCB,DMRSEQ,ACCDIR,ESTKEY,ESTKEY                        
                                                                                
GR_EST06 JNE   *+2                                                              
         CLC   KEYSAVE(ESTKCLI-ESTKEY),ESTKEY                                   
         JNE   GETRECSX                                                         
                                                                                
         CLI   ESTKPRO,ESTKBPQ     Skip ballpark estimates                      
         JE    GR_EST04                                                         
         CLI   ESTKJOB,ESTKBPQ                                                  
         JE    GR_EST04                                                         
         CLI   ESTKSEQ,ESTKSMQ                                                  
         JNE   GR_EST04                                                         
                                                                                
         GOTOR VDATAMGR,DMCB,GETREC,ACCMST,ESTKDA,AIO2,DMWORK                   
         JNE   *+2                                                              
                                                                                
         L     R3,AIO2                                                          
         USING EMDELD,R4                                                        
         LA    R4,ESTRFST                                                       
                                                                                
* AGXROUTS.AGXEST46 'Ensure good estimate' ERDEL check not donehere so          
* may cause differences.                                                        
                                                                                
GR_EST08 CLI   EMDEL,EMDELQ                                                     
         JNE   GR_EST10                                                         
                                                                                
         AP    AGYLEST,EMDAMT                                                   
                                                                                
         CLI   BREAK,YESQ                                                       
         JNE   GR_EST10                                                         
                                                                                
GR_EST09 GOTOR SAVBRK,DMCB,('ACRTESTR',ESTRECD),EMDELD                          
                                                                                
GR_EST10 CLI   EMDEL,0                                                          
         JE    GR_EST04                                                         
         LLC   R1,EMDLN                                                         
         AR    R4,R1                                                            
         J     GR_EST08                                                         
         DROP  R3,R4                                                            
                                                                                
GETRECSX DS    0H                                                               
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* SGet order's SJ account                                             *         
***********************************************************************         
                                                                                
GETOSJ   NTR1  ,                                                                
                                                                                
         USING ORDELD,R4                                                        
         MVC   CURSJA,GSPACES                                                   
                                                                                
GETOSJ02 CLI   ORDEL,ORDELQ                                                     
         JE    GETOSJ10                                                         
         CLI   ORDEL,SORELQ                                                     
         JE    GETOSJ20                                                         
         CLI   ORDEL,0                                                          
         JE    GETOSJ30                                                         
                                                                                
GETOSJ04 LLC   R0,ORDLN                                                         
         AR    R4,R0                                                            
         J     GETOSJ02                                                         
                                                                                
GETOSJ10 CLI   ORDSUPL,STLDGQ      Skip artist orders                           
         JE    GETOSJN                                                          
         CLI   ORDSUPL,SKLDGQ      Skip internal orders                         
         JE    GETOSJN                                                          
         CLI   ORDSUPL,SILDGQ                                                   
         JE    GETOSJN                                                          
         CLC   ORDACCU(2),SJUL                                                  
         JNE   GETOSJ04                                                         
         MVC   CURSJA,ORDACCA                                                   
         J     GETOSJ04                                                         
                                                                                
         USING SORELD,R4                                                        
GETOSJ20 CLI   SORSYS,SORSACC                                                   
         JNE   GETOSJ04                                                         
         CLC   SORAULA(2),SJUL                                                  
         JNE   GETOSJ04                                                         
         MVC   CURSJA,SORAACT                                                   
         J     GETOSJ04                                                         
                                                                                
GETOSJ30 DS    0H                                                               
* AGXROUTS check not implemented here so may use orders not used in             
* AGXTRACT:                                                                     
*        TM    SAVOIND,SAVOIOAQ+SAVOIORQ                                        
*        JNO   AGXORDN             Data integrity check                         
         J     GETOSJY                                                          
         DROP  R4                                                               
                                                                                
GETOSJN  DS    0H                                                               
         J     EXITN                                                            
                                                                                
GETOSJY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
***********************************************************************         
* SGet expense claim's SJ account                                     *         
***********************************************************************         
                                                                                
GETXSJ   NTR1  ,                                                                
                                                                                
         USING CIDELD,R4                                                        
COD      USING CIDELD,R5                                                        
         LR    R5,R4                                                            
         MVC   CURSJA,GSPACES                                                   
                                                                                
GETXSJ02 LLC   R1,COD.CIDLN                                                     
         AR    R5,R1                                                            
                                                                                
         CLI   COD.CIDEL,0         Die if no CIDTYCQ element                    
***      JE    *+2                 (well ... MCSTST.PUNETRA.D/913669)           
         JE    GETXSJY                                                          
         CLI   COD.CIDEL,CIDELQ                                                 
         JNE   GETXSJ02                                                         
         CLC   COD.CIDSEQ,CIDSEQ                                                
         JNE   GETXSJ02                                                         
         CLI   COD.CIDTYPE,CIDTYCQ                                              
         JNE   GETXSJ02                                                         
         CLC   COD.CIDCCPJ,GSPACES                                              
         JNH   GETXSJY                                                          
         MVC   CURSJA,COD.CIDCCPJ                                               
         J     GETXSJY                                                          
         DROP  R4,COD                                                           
                                                                                
GETXSJN  DS    0H                                                               
         J     EXITN                                                            
                                                                                
GETXSJY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Save Breakdown values                                               *         
***********************************************************************         
                                                                                
SAVBRK   NTR1  ,                                                                
                                                                                
         USING AGYLSTD,R2                                                       
                                                                                
         MVC   BYTE2,0(R1)         ACRECEQUS value                              
         XR    R4,R4                                                            
         ICM   R4,B'0111',1(R1)    A(RECORD)                                    
         L     R5,4(R1)            A(ELEMENT)                                   
                                                                                
TST      USING BRKBUFD,R6                                                       
         LA    R6,TEMP                                                          
         MVC   TST.BRKBCPY(BRKBKLQ),GSPACES                                     
         MVC   TST.BRKBCPY,AGYLCPY                                              
         MVC   TST.BRKBALP,AGYLALP                                              
                                                                                
         MVI   BYTE,BRKBTDR-BRKBUFD                                             
         CLI   BYTE2,ACRTTRN                                                    
         JE    SB_TRN                                                           
         MVI   BYTE,BRKBEDR-BRKBUFD                                             
         CLI   BYTE2,ACRTETR                                                    
         JE    SB_TRN1                                                          
         MVI   BYTE,BRKBTIM-BRKBUFD                                             
         CLI   BYTE2,ACRTTIM                                                    
         JE    SB_TIM                                                           
         MVI   BYTE,BRKBEXP-BRKBUFD                                             
         CLI   BYTE2,ACRTEXPC                                                   
         JE    SB_EXP                                                           
         MVI   BYTE,BRKBEST-BRKBUFD                                             
         CLI   BYTE2,ACRTESTR                                                   
         JE    SB_EST                                                           
         MVI   BYTE,BRKBORD-BRKBUFD                                             
         CLI   BYTE2,ACRTORD                                                    
         JNE   *+2                                                              
                                                                                
         USING ORDRECD,R4                                                       
         USING OAMELD,R5                                                        
         MVC   TST.BRKBUNT,SEUL                                                 
         CLC   CURSJA,GSPACES                                                   
         JNH   SB_ORD2                                                          
         MVC   TST.BRKBUNT,SJUL                                                 
         LLC   R1,AGYLCLL                                                       
         SHI   R1,1                                                             
         MVC   TST.BRKBCLI(0),CURSJA                                            
         EXRL  R1,*-6                                                           
         AHI   R1,1                                                             
         LA    R1,CURSJA(R1)                                                    
         LLC   RE,AGYLPRL                                                       
         SHI   RE,1                                                             
         MVC   TST.BRKBPRO(0),0(R1)                                             
         EXRL  RE,*-6                                                           
***      CLI   BREAK,SPECIALQ                                                   
***      JNE   SB_ORD2                                                          
***      LLC   R1,AGYLCLL                                                       
***      LA    R1,1(RE,R1)                                                      
***      LA    R1,CURSJA(R1)                                                    
***      LLC   RE,AGYLJOL                                                       
***      MVC   TST.BRKBJOB(0),0(R1)                                             
***      EXRL  RE,*-6                                                           
***      MVC   TST.BRKBNUM,ORDKORD                                              
                                                                                
SB_ORD2  ZAP   DUB,OAMAMNT                                                      
         J     SAVBRK10                                                         
         DROP  R4,R5                                                            
                                                                                
         USING TRNRECD,R4                                                       
         USING TRNELD,R5                                                        
SB_TRN   MVC   TST.BRKBUNT,TRNKULA                                              
         LLC   R1,AGYLCLL                                                       
         SHI   R1,1                                                             
         MVC   TST.BRKBCLI(0),TRNKACT                                           
         EXRL  R1,*-6                                                           
         AHI   R1,1                                                             
         LA    R1,TRNKACT(R1)                                                   
         LLC   RE,AGYLPRL                                                       
         SHI   RE,1                                                             
         MVC   TST.BRKBPRO(0),0(R1)                                             
         EXRL  RE,*-6                                                           
SB_TRN1  TM    TRNSTAT,TRNSDR                                                   
         JNZ   SB_TRN2                                                          
         LLC   R1,BYTE                                                          
         AHI   R1,BRKBTCR-BRKBTDR                                               
         STC   R1,BYTE                                                          
                                                                                
SB_TRN2  ZAP   DUB,TRNAMNT                                                      
         J     SAVBRK10                                                         
         DROP  R4,R5                                                            
                                                                                
         USING TIMRECD,R4                                                       
         USING TIMELD,R5                                                        
SB_TIM   CLI   TIMETYP,TIMEITMS                                                 
         JE    SB_TIM4                                                          
         MVC   TST.BRKBUNT,ONUL                                                 
         CLC   ONUL,TIMACC                                                      
         JE    SB_TIM2                                                          
         MVC   TST.BRKBUNT,SJUL                                                 
         LLC   R1,AGYLCLL                                                       
         SHI   R1,1                                                             
         MVC   TST.BRKBCLI(0),TIMACC+2                                          
         EXRL  R1,*-6                                                           
         AHI   R1,1                                                             
         LA    R1,TIMACC+2(R1)                                                  
         LLC   RE,AGYLPRL                                                       
         SHI   RE,1                                                             
         MVC   TST.BRKBPRO(0),0(R1)                                             
         EXRL  RE,*-6                                                           
***      CLI   BREAK,SPECIALQ                                                   
***      JNE   SB_TIM2                                                          
***      LLC   R1,AGYLCLL                                                       
***      LA    R1,1(RE,R1)                                                      
***      LA    R1,TIMACC+2(R1)                                                  
***      LLC   RE,AGYLJOL                                                       
***      MVC   TST.BRKBJOB(0),0(R1)                                             
***      EXRL  RE,*-6                                                           
***      GOTO1 VDATCON,DMCB,(1,TIMKPEDT),(0,TST.BRKBJOB)                        
***      MVC   TST.BRKBNUM,TIMKACT+5                                            
                                                                                
SB_TIM2  ZAP   DUB,TIMHRS                                                       
         J     SAVBRK10                                                         
                                                                                
SB_TIM4  MVC   TST.BRKBUNT,ONUL                                                 
         CLC   ONUL,TIMIULA                                                     
         JE    SB_TIM6                                                          
         MVC   TST.BRKBUNT,SJUL                                                 
         LLC   R1,AGYLCLL                                                       
         SHI   R1,1                                                             
         MVC   TST.BRKBCLI(0),TIMIACC                                           
         EXRL  R1,*-6                                                           
         AHI   R1,1                                                             
         LA    R1,TIMIACC(R1)                                                   
         LLC   RE,AGYLPRL                                                       
         SHI   RE,1                                                             
         MVC   TST.BRKBPRO(0),0(R1)                                             
         EXRL  RE,*-6                                                           
                                                                                
SB_TIM6  ZAP   DUB,TIMIMULT                                                     
         J     SAVBRK10                                                         
         DROP  R4,R5                                                            
                                                                                
         USING EXCRECD,R4                                                       
         USING CIDELD,R5                                                        
SB_EXP   MVC   TST.BRKBUNT,SEUL                                                 
         CLC   CURSJA,GSPACES                                                   
         JNH   SB_EXP2                                                          
         MVC   TST.BRKBUNT,SJUL                                                 
         LLC   R1,AGYLCLL                                                       
         SHI   R1,1                                                             
         MVC   TST.BRKBCLI(0),CURSJA                                            
         EXRL  R1,*-6                                                           
         AHI   R1,1                                                             
         LA    R1,CURSJA(R1)                                                    
         LLC   RE,AGYLPRL                                                       
         SHI   RE,1                                                             
         MVC   TST.BRKBPRO(0),0(R1)                                             
         EXRL  RE,*-6                                                           
                                                                                
SB_EXP2  ZAP   DUB,CIDMAMT                                                      
         J     SAVBRK10                                                         
         DROP  R4,R5                                                            
                                                                                
         USING ESTRECD,R4                                                       
         USING EMDELD,R5                                                        
SB_EST   MVC   TST.BRKBUNT,SJUL                                                 
         MVC   TST.BRKBCLI(L'ESTKCLI),ESTKCLI                                   
         MVC   TST.BRKBPRO(L'ESTKPRO),ESTKPRO                                   
         ZAP   DUB,EMDAMT                                                       
***      CLI   BREAK,SPECIALQ                                                   
***      JNE   SAVBRK10                                                         
***      MVC   TST.BRKBJOB(L'ESTKJOB),ESTKJOB                                   
***      MVC   TST.BRKBNUM,EMDGNO                                               
***      J     SAVBRK10                                                         
         DROP  R4,R5                                                            
                                                                                
         USING BRKBUFD,R2                                                       
SAVBRK10 SAM31 ,                                                                
         L     R2,ABRKBUF                                                       
         L     R3,=AL4(BRKBLNQ*BRKBMAX)                                         
         AR    R3,R2                                                            
                                                                                
SAVBRK12 CLI   BRKBCPY,EOTQ                                                     
         JNE   SAVBRK14                                                         
                                                                                
         MVC   BRKBCPY(BRKBKLQ),TST.BRKBCPY                                     
         ZAP   BRKBTDR,PZERO       New entry ...                                
         ZAP   BRKBTD#,PZERO                                                    
         ZAP   BRKBTCR,PZERO                                                    
         ZAP   BRKBTC#,PZERO                                                    
         ZAP   BRKBTIM,PZERO                                                    
         ZAP   BRKBTI#,PZERO                                                    
         ZAP   BRKBEXP,PZERO                                                    
         ZAP   BRKBEX#,PZERO                                                    
         ZAP   BRKBORD,PZERO                                                    
         ZAP   BRKBOR#,PZERO                                                    
         ZAP   BRKBEST,PZERO                                                    
         ZAP   BRKBES#,PZERO                                                    
         ZAP   BRKEEC#,PZERO                                                    
         ZAP   BRKBECR,PZERO                                                    
         ZAP   BRKEED#,PZERO                                                    
         ZAP   BRKBEDR,PZERO                                                    
         MVI   BRKBCPY+BRKBLNQ,EOTQ                                             
         CR    R2,R3               ... if sufficient space                      
         JL    SAVBRK16                                                         
                                                                                
         MVC   P(28),=CL28'Increase of BRKBMXQ required'                        
         LA    R5,523                                                           
         GOTOR VPRINTER                                                         
         ABEND (R5),DUMP                                                        
                                                                                
SAVBRK14 CLC   TST.BRKBCPY(BRKBKLQ),BRKBCPY                                     
         JE    SAVBRK16                                                         
         AHI   R2,BRKBLNQ                                                       
         J     SAVBRK12                                                         
                                                                                
SAVBRK16 LLC   R1,BYTE                                                          
         LA    R1,BRKBCPY(R1)                                                   
         AP    0(L'BRKBTDR,R1),DUB                                              
         AHI   R1,BRKBTD#-BRKBTDR                                               
         AP    0(L'BRKBTD#,R1),PONE                                             
                                                                                
         SAM24 ,                                                                
         J     EXIT                                                             
         DROP  R2,TST                                                           
                                                                                
***********************************************************************         
* Close files                                                         *         
***********************************************************************         
                                                                                
CLOSE    NTR1  ,                                                                
                                                                                
         GOTOR VDATAMGR,DMCB,DMCLSE,ACCSYS,OPENLST,AIO3                         
                                                                                
         CLI   BREAK,YESQ                                                       
         JE    CLOSE2                                                           
         CLI   BREAK,SPECIALQ                                                   
         JNE   CLOSEX                                                           
                                                                                
CLOSE2   SAM31 ,                                                                
         L     R0,=AL4(BRKBLNQ*BRKBMAX)                                         
         L     R1,ABRKBUF                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
         SAM24 ,                                                                
                                                                                
CLOSEX   DS    0H                                                               
         J     EXIT                                                             
                                                                                
*********************************************************************           
* General exits                                                     *           
*********************************************************************           
                                                                                
EXITL    DS    0H                  Low                                          
EXITN    LHI   R0,0                Not Equal                                    
         J     EXITCC                                                           
EXITY    LHI   R0,1                Equal                                        
         J     EXITCC                                                           
EXITH    LHI   R0,2                High                                         
                                                                                
EXITCC   CHI   R0,1                Set condition code                           
                                                                                
EXIT     XIT1  ,                                                                
                                                                                
*********************************************************************           
* Initialisation routine.                                           *           
* 1. Set up working storage.                                        *           
* 2. Process SYSIN cards.                                           *           
*********************************************************************           
                                                                                
INITIAL  NTR1  ,                                                                
                                                                                
* Set up External routines                                                      
         MVC   VCARDS,=V(CARDS)                                                 
         MVC   VDATCON,=V(DATCON)                                               
         MVC   VDATVAL,=V(DATVAL)                                               
         MVC   VDATAMGR,=V(DATAMGR)                                             
         MVC   VDYNALLO,=V(DYNALLOC)                                            
         MVC   VXSORT,=V(XSORT)                                                 
         MVC   VHELLO,=V(HELLO)                                                 
         MVC   VRECTYP,=V(ACRECTYP)                                             
         MVC   CUREDIT,=V(CUREDIT)                                              
         MVC   VHEXIN,=V(HEXIN)                                                 
         MVC   VHEXOUT,=V(HEXOUT)                                               
         MVC   VLOGIO,=V(LOGIO)                                                 
         MVC   VPRINTER,=V(PRINTER)                                             
         MVC   VPRNTBL,=V(PRNTBL)                                               
         MVC   VNUMVAL,=V(NUMVAL)                                               
         MVC   VLOADER,=V(LOADER)                                               
         MVC   VMQRPT,=V(MQRPT)                                                 
         MVC   VUTL,=V(UTL)                                                     
         MVC   ASSB,=A(SSB)                                                     
                                                                                
* Set up working storage addresses                                              
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO1                                                          
         L     R1,=A(IOAREA2-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO2                                                          
         L     R1,=A(IOAREA3-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO3                                                          
         MVC   ATRKBUF,=A(TRKBUF)                                               
         MVC   ATRLOWER,=A(TRLOWER)                                             
                                                                                
         XC    GSPACES,GSPACES     Preset GSPACES to space                      
         TR    GSPACES,=C' '                                                    
                                                                                
***********************************************************************         
* Read and validate control cards                                     *         
***********************************************************************         
                                                                                
         L     R7,AIO1             A(work area for control cards)               
         XC    TRTAB,TRTAB                                                      
         MVI   TRTAB+C'>',X'24'                                                 
         MVI   TRTAB+C'<',X'44'                                                 
         MVI   TRTAB+C'^',X'74'                                                 
         MVI   TRTAB+C'=',X'84'                                                 
                                                                                
         XC    FLTCPYS(FLTCPYL),FLTCPYS                                         
                                                                                
* Read in cards                                                                 
VALC002  DS    0H                                                               
         GOTOR VCARDS,DMCB,(R7),=C'RE00'                                        
         CLC   =C'/*',0(R7)                                                     
         JE    VALC020                                                          
         LR    R3,R7               A(start of card)                             
         CLI   0(R3),C'*'          * in col 1 is a comment                      
         JE    VALC014             ignore comment cards                         
         LA    R1,79(R1)                                                        
         MVC   P(80),0(R7)                                                      
         GOTOR VPRINTER            Print out card                               
                                                                                
* Scan CARDTAB for card                                                         
         USING CARDTABD,R4                                                      
         LA    R4,CARDTAB                                                       
                                                                                
VALC004  DS    0H                                                               
         CLI   0(R4),EOTQ                                                       
         JE    CARDER2             Error if card not found                      
         LLC   R1,CARDKXLN                                                      
         CLC   0(0,R3),CARDKEY                                                  
         EXRL  R1,*-6                                                           
         JE    VALC006                                                          
         AHI   R4,CARDTBLQ         Try next entry in CARDTAB                    
         J     VALC004                                                          
                                                                                
* Register usage:                                                               
*   R2 - branch condition (set by TRT)                                          
*   R3 - points to the delimeter                                                
*   R7 - points to start of card                                                
VALC006  DS    0H                                                               
         LA    R3,1(R1,R3)         Point to delimiter                           
         XR    RF,RF                                                            
         TM    CARDIND,CARDNUMQ    Numbers before delimeter ?                   
         JNO   VALC008             No                                           
         LA    RF,79(R7)           A(end of card)                               
         SR    RF,R3               L' to check                                  
                                                                                
VALC008  DS    0H                                                               
         TRT   0(0,R3),TRTAB       Is this a valid delimeter ?                  
         EXRL  RF,*-6                                                           
         JZ    CARDER2             No, error                                    
         ICM   RF,15,CARDVDSP                                                   
         TM    CARDIND,CARDRTNQ    Validation routine ?                         
         JNO   VALC010             No                                           
         BASR  RE,RF               Yes, call validation routine                 
         JNE   CARDER2                                                          
         J     VALC012                                                          
                                                                                
* Move value into local storage                                                 
VALC010  DS    0H                                                               
         IC    R1,CARDVXLN         Get len for move                             
         LA    RF,WORKD(RF)                                                     
         MVC   0(0,RF),1(R3)       Move to output area                          
         EXRL  R1,*-6                                                           
                                                                                
VALC012  DS    0H                                                               
         MVI   ANYCARDS,YESQ                                                    
                                                                                
VALC014  DS    0H                                                               
         J     VALC002                                                          
                                                                                
* All cards have been processed. Now validate cards in context.                 
VALC020  DS    0H                                                               
                                                                                
* Validate DSPACE                                                               
VALC022  DS    0H                                                               
         CLI   DSPACE,0                                                         
         JE    CARDER5             Must provide DSPACE=                         
         CLI   DSPACE,C'T'                                                      
         JE    VALC024                                                          
         CLI   DSPACE,C'A'                                                      
         JE    VALC024                                                          
         CLI   DSPACE,C'C'                                                      
         JE    VALC024                                                          
         CLI   DSPACE,C'Q'                                                      
         JE    VALC024                                                          
         CLI   DSPACE,C'B'                                                      
         JNE   CARDER6                                                          
                                                                                
         USING SSOOFF,RF                                                        
VALC024  L     RF,ASSB                                                          
         MVC   SSODSPAC,DSPACE     Set dataspace ID                             
         MVI   SSOSTAT2,SSOSGALO+SSOSNRCV                                       
                                                                                
         L     RF,VUTL             SET SE NO. FOR CHOSEN SYS                    
         MVI   4(RF),CONSYSQ                                                    
         L     R7,AIO2             OPEN CT FILE FIRST                           
         LARL  RF,CFILLIST                                                      
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONSYS,(RF),(R7)                            
                                                                                
         USING CTWREC,R2                                                        
         L     R2,AIO1             READ SYSTEM LIST ACC RECORD                  
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVI   CTWKREC,CTWKRSYS                                                 
         MVI   CTWKSYSN,ACCQ                                                    
         GOTOR VDATAMGR,DMCB,DMREAD,CTFILE,CTWREC,CTWREC                        
         JNE   CARDER3             NOT FOUND - INVALID                          
         USING CTLSTD,R1                                                        
         LA    R1,CTWDATA                                                       
                                                                                
VALC026  CLI   CTLSTD,0            TEST END OF RECORD - INVALID                 
         JE    CARDER3                                                          
         CLI   CTLSTEL,CTLSTELQ    TEST LIST ELEMENT                            
         JNE   VALC028                                                          
         CLI   CTLSTSYS,ACCSYSQ    TEST ACCPAK SE LIST ENTRY                    
         JNE   VALC028                                                          
         LARL  RF,SYSACC                                                        
         CLC   CTLSTNAM(3),0(RF)                                                
         JNE   VALC028                                                          
         CLC   CTLSTNAM+3(2),THISSEAL                                           
         JNE   VALC028                                                          
         MVC   THISSENO,CTLSTSE                                                 
         J     VALC030                                                          
                                                                                
VALC028  LLC   R0,CTLSTLEN         BUMP TO NEXT ELEMENT ON LIST RE              
         AR    R1,R0                                                            
         J     VALC026                                                          
         DROP  R1,R2                                                            
                                                                                
VALC030  L     R7,AIO2             OPEN CT FILE FIRST                           
         LARL  RF,CFILLIST                                                      
         GOTO1 VDATAMGR,DMCB,DMCLSE,CONSYS,(RF),(R7)                            
                                                                                
         CLI   BREAK,YESQ                                                       
         JE    VALC032                                                          
         CLI   BREAK,SPECIALQ                                                   
         JNE   VALC040                                                          
                                                                                
VALC032  SAM31 ,                                                                
         L     R0,=AL4(BRKBLNQ*BRKBMAX)                                         
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ABRKBUF                                                       
         USING BRKBUFD,R1                                                       
         MVI   BRKBCPY,EOTQ                                                     
         DROP  R1                                                               
         SAM24 ,                                                                
                                                                                
VALC040  DS    0H                                                               
                                                                                
INITIALX DS    0H                                                               
         XIT1  ,                                                                
                                                                                
         DS    0H                                                               
SYSACC   DC    CL3'ACC'                                                         
         DS    0H                                                               
CFILLIST DS    0XL9                                                             
         DC    C'NCTFILE '         CONTROL FILE (FOR USERIDS)                   
         DC    C'X'                                                             
         DS    0H                                                               
                                                                                
* Invalid control card. Print it out and abend.                                 
CARDER1  MVC   P(20),=CL20'No control cards'                                    
         LA    R2,501                                                           
         J     VALCDIE                                                          
                                                                                
CARDER2  MVC   P(20),=CL20'Invalid control card'                                
         LA    R2,502                                                           
         J     VALCDIE                                                          
                                                                                
CARDER3  MVC   P(20),=CL20'Invalid acc system'                                  
         LA    R2,503                                                           
         J     VALCDIE                                                          
                                                                                
CARDER5  MVC   P(18),=CL20'DSPACE= is missing'                                  
         LA    R2,505                                                           
         J     VALCDIE                                                          
                                                                                
CARDER6  MVC   P(20),=CL20'DSPACE= is invalid'                                  
         LA    R2,506                                                           
         J     VALCDIE                                                          
                                                                                
CARDER7  MVC   P(28),=CL28'No applicable company found'                         
         LA    R2,507                                                           
         J     VALCDIE                                                          
                                                                                
CARDER8  MVC   P(28),=CL28'Invalid MODE= value'                                 
         LA    R2,508                                                           
         J     VALCDIE                                                          
                                                                                
CARDER9  MVC   P(28),=CL28'Invalid BREAK= value'                                
         LA    R2,509                                                           
         J     VALCDIE                                                          
                                                                                
CARDER10 MVC   P(28),=CL28'Too many AGENCY= cards'                              
         LA    R2,510                                                           
         J     VALCDIE                                                          
                                                                                
VALCDIE  DS    0H                                                               
         GOTOR VPRINTER            Print out card                               
                                                                                
VALCDIE2 ABEND (R2),DUMP                                                        
                                                                                
*********************************************************************           
* MODE=A (space or T)                                               *           
* Validate RUNMODE                                                  *           
*********************************************************************           
                                                                                
MODVAL   NTR1  ,                                                                
         LA    RF,5(R7)                                                         
         CLI   0(RF),SPACEQ                                                     
         JE    MODVAL02                                                         
         CLI   0(RF),RUNTSTQ                                                    
         JNE   CARDER8                                                          
                                                                                
MODVAL02 MVC   RUNMODE,0(RF)                                                    
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* WHICHSYS=AA                                                       *           
* Validate basic format. MEDSID is called at end of validation      *           
*********************************************************************           
                                                                                
SYSVAL   NTR1  ,                                                                
         LA    R2,20(R7)           Find length of RHS                           
         LHI   RE,-1                                                            
         LA    RF,8(R7)                                                         
         CLI   0(R2),C' '                                                       
         JNE   SYSVAL02                                                         
         BRXH  R2,RE,*-8                                                        
         J     CARDERR                                                          
                                                                                
SYSVAL02 SR    R2,RF                                                            
         CHI   R2,1                                                             
         JL    SYSVAL04                                                         
         CHI   R2,2                                                             
         JH    CARDERR                                                          
                                                                                
SYSVAL04 MVC   THISSEAL,9(R7)                                                   
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* FLASH=.                                                           *           
* Validate and set FLASH= parms.                                    *           
*********************************************************************           
                                                                                
FLASH    NTR1  ,                                                                
         L     RF,ASSB                                                          
         CLI   SSOXTND-SSOOFF(RF),FFQ                                           
         JNE   *+2                                                              
         CLI   6(R7),C'A'          Test if valid flash character                
         JL    CARDERR                                                          
         CLI   6(R7),C'N'          FLASH=N means no flash copy                  
         JE    CARDOK                                                           
         OI    SSOFLAG2-SSOOFF(RF),SSO2FLSH                                     
         MVC   SSOFLSHI-SSOOFF(1,RF),6(R7)                                      
         CLI   6(R7),C'Y'          FLASH=Y is default so set to FLASH=S         
         JNE   CARDOK              to cause DSN=FLS.XXX...                      
         MVI   SSOFLSHI-SSOOFF(RF),C'S'                                         
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* BREAK=.                                                           *           
* Validate and set BREAK= parms                                     *           
*********************************************************************           
                                                                                
BRKVAL   NTR1  ,                                                                
         MVC   BREAK,6(R7)                                                      
         CLI   6(R7),YESQ                                                       
         JE    CARDOK                                                           
         CLI   6(R7),SPECIALQ                                                   
         JE    CARDOK                                                           
         CLI   6(R7),LEDGERQ                                                    
         JE    CARDOK                                                           
         J     CARDER9                                                          
                                                                                
*********************************************************************           
* DDSIO=  validation                                                *           
*********************************************************************           
                                                                                
DSIOVAL  NTR1  ,                                                                
         LA    R2,6(R7)                                                         
         L     RF,=V(DDSIO)        Set up DDSIO override                        
         MVC   0(8,RF),0(R2)                                                    
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* AGENCY=AA                                                         *           
*********************************************************************           
                                                                                
AGYVAL   NTR1  ,                                                                
         LA    R1,FLTCPY1                                                       
         OC    FLTCPY1,FLTCPY1                                                  
         JZ    AGYVAL2                                                          
         LA    R1,FLTCPY2                                                       
         OC    FLTCPY2,FLTCPY2                                                  
         JZ    AGYVAL2                                                          
         LA    R1,FLTCPY3                                                       
         OC    FLTCPY3,FLTCPY3                                                  
         JNZ   CARDER10                                                         
                                                                                
AGYVAL2  LA    RF,7(R7)                                                         
         CLC   0(2,RF),GSPACES                                                  
         JNH   CARDOK                                                           
         MVC   0(2,R1),0(RF)                                                    
         J     CARDOK                                                           
                                                                                
* Exit from card validation routines                                            
                                                                                
CARDOK   CR    RB,RB               Set r/c EQ                                   
         J     *+6                                                              
                                                                                
CARDERR  LTR   RB,RB               Set r/c NE                                   
         J     VALCARDX                                                         
                                                                                
VALCARDX XIT1  ,                                                                
                                                                                
*********************************************************************           
* Global literals and constants (addressed by RB)                   *           
*********************************************************************           
                                                                                
$$DATA   LOCTR ,                                                                
GLOBALS  DS    0D                                                               
                                                                                
         LTORG ,                                                                
ACRTETR  EQU   X'FE'               Custom record type for exp trans             
                                                                                
EFFS     DC    X'FFFFFFFF'                                                      
NULLS    DC    X'0000000000000000'                                              
DMOPEN   DC    CL8'OPEN   '                                                     
DMCLSE   DC    CL8'DMCLSE '                                                     
GETREC   DC    CL8'GETREC '                                                     
ADDREC   DC    CL8'ADDREC '                                                     
PUTRECLT DC    CL8'PUTREC '                                                     
DMREAD   DC    CL8'DMREAD '                                                     
DMRSEQ   DC    CL8'DMRSEQ '                                                     
DMRDHI   DC    CL8'DMRDHI '                                                     
DMWRT    DC    CL8'DMWRT  '                                                     
DMADD    DC    CL8'DMADD  '                                                     
CONTROL  DC    CL8'CONTROL'                                                     
MQOPEN   DC    CL6'OPEN'                                                        
MQPUT    DC    CL6'PUT'                                                         
MQCLOSE  DC    CL6'CLOSE'                                                       
CTFILE   DC    CL8'CTFILE'                                                      
CONSYS   DC    CL4'CON'                                                         
CONQ     EQU   X'0A'                                                            
CONSYSQ  EQU   CONQ                                                             
ACCQ     EQU   X'06'                                                            
ACCSYSQ  EQU   ACCQ                                                             
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCSYS   DC    CL4'ACC'                                                         
ACCDIR   DC    CL8'ACCDIR '                                                     
ACCMST   DC    CL8'ACCMST '                                                     
ACCARC   DC    CL8'ACCARC '                                                     
                                                                                
OPENLST  DS    0CL9                                                             
         DC    C'NACCDIR '         ACCOUNTING FILES TO OPEN                     
         DC    C'NACCMST '                                                      
         DC    C'NACCARC '                                                      
         DC    C'NCTFILE '                                                      
         DC    C'X'                                                             
                                                                                
PONE     DC    PL1'1'                                                           
PMONE    DC    PL1'-1'                                                          
PZERO    DC    PL1'0'                                                           
EUROQ    DC    CL3'EUR'                                                         
DEMQ     DC    CL3'DEM'                                                         
IEPQ     DC    CL3'IEP'                                                         
NAQ      DC    CL2'NA'                                                          
EUQ      DC    CL2'EU'                                                          
ORDCNT   DC    CL6'000000'                                                      
SJUL     DC    CL2'SJ'                                                          
SEUL     DC    CL2'SE'                                                          
ONUL     DC    CL2'1N'                                                          
H_REGION DC    CL6'Region'                                                      
H_ALPHA  DC    CL6'Alpha'                                                       
H_SYSTEM DC    CL6'System'                                                      
H_TRNDR  DC    CL12'Trx Debits'                                                 
H_TRNCR  DC    CL12'Trx Credits'                                                
H_TIM    DC    CL12'Time'                                                       
H_EXP    DC    CL12'Expenses'                                                   
H_ORD    DC    CL12'Orders'                                                     
H_EST    DC    CL12'Estimates'                                                  
H_CLI    DC    CL6'Client'                                                      
H_PRO    DC    CL7'Product'                                                     
H_EDR    DC    CL12'Exp trx Debs'                                               
H_ECR    DC    CL12'Exp trx Crds'                                               
DSNNAMA  DC    CL7'AURAPRD'                                                     
DSNNAMQ  DC    CL7'AURAFQA'                                                     
DSNNAMC  DC    CL7'AURACSC'                                                     
DSNNAMT  DC    CL7'AURADEV'                                                     
DSNNAMO  DC    CL9'TKLU.AURA'      LOCAL OVERRIDE FOR TESTING                   
MQ_CSC   DS    CL25'MO.AURAWH.NOTIFY.CSC1.LQ'       FLASH                       
MQ_TST   DC    CL25'MO.AURAWH.NOTIFY.DEV1.LQ'       TEST/DEV                    
MQ_FQA   DC    CL25'MO.AURAWH.NOTIFY.QA1.LQ'        QA                          
MQ_ADV   DC    CL25'MO.AURAWH.NOTIFY.PRD1.LQ'       PROD                        
GSPACES  DC    256C' '                                                          
                                                                                
* MQ MQRPT message                                                              
                                                                                
MYMQLNQ  EQU   200                                                              
MYMQMSG  DC    (MYMQLNQ)C' '       200 byte space filled message                
                                                                                
* Header                                                                        
         ORG   MYMQMSG+001                                                      
MYMQHDR  DS    0CL16               16 byte message routing label                
         DC    16C'*'                                                           
         ORG   MYMQHDR+001                                                      
MYROUTE  DC    C'AURAWH'           AURA Warehouse Q routing                     
                                                                                
* Message                                                                       
         ORG   MYMQHDR+017                                                      
MYRECTY  DC    C'RECONCILIATION'   Record type                                  
         ORG   MYMQHDR+60                                                       
MYPREF   DC    C'AURA-DSN='        Prefix                                       
         ORG   MYMQHDR+60+9                                                     
MYFILE   DC    CL(L'DSNNAME)' '    MVS DSN                                      
         ORG   ,                                                                
MYMSGLQ  EQU   *-MYRECTY           L' message                                   
                                                                                
*********************************************************************           
* Datasets                                                          *           
* MEACTV generates DDFA recovery tapes with a BLKSIZE of 8200       *           
* which is why I have defined them as that here.                    *           
*********************************************************************           
                                                                                
DSNFILE  DCB   DDNAME=DSNFIL,                                          +        
               DSORG=PS,                                               +        
               MACRF=PM,                                               +        
               RECFM=FB,                                               +        
               BLKSIZE=2560,LRECL=256                                  +        
                                                                                
D2NAME   EQU   DSNFILE+40         DDNAME is at DCB+40                           
DSNSPACE DC    AL3(1,1)           TRK,(1,1) should be enough                    
DSNNAME  DC    CL44' '                                                          
                                                                                
BRKOUT   DCB   DDNAME=BRKFIL,                                          +        
               DSORG=PS,                                               +        
               MACRF=PM,                                               +        
               RECFM=FB,                                               +        
               BLKSIZE=2560,LRECL=256                                           
                                                                                
CARDTAB  DS    0H                                                               
         DC    C'AGENCY    ',AL1(5),AL1(0),AL1(CARDRTNQ),AL4(AGYVAL)            
         DC    C'BREAK     ',AL1(4),AL1(0),AL1(CARDRTNQ),AL4(BRKVAL)            
         DC    C'DDSIO     ',AL1(4),AL1(0),AL1(CARDRTNQ),AL4(DSIOVAL)           
         DC    C'DSPACE    ',AL1(5),AL1(0),AL1(0),AL4(DSPACE-WORKD)             
         DC    C'FLASH     ',AL1(4),AL1(0),AL1(CARDRTNQ),AL4(FLASH)             
         DC    C'MODE      ',AL1(3),AL1(0),AL1(CARDRTNQ),AL4(MODVAL)            
         DC    C'WHICHSYS  ',AL1(7),AL1(0),AL1(CARDRTNQ),AL4(SYSVAL)            
         DC    AL1(EOTQ)                                                        
                                                                                
TRLOWER  DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  00-0F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  10-1F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  20-2F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  30-3F                    
         DC    XL16'406F6F6F6F6F6F6F6F6F4A4B4C4D4E4F'  40-4F                    
         DC    XL16'506F6F6F6F6F6F6F6F6F5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60616F6F6F6F6F6F6F6F6A6B6C6D6E6F'  60-6F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F797A7B7C7D7E7F'  70-7F                    
         DC    XL16'6F8182838485868788896F6F6F6F6F6F'  80-8F                    
         DC    XL16'6F9192939495969798996F6F6F6F6F6F'  90-9F                    
         DC    XL16'6FA1A2A3A4A5A6A7A8A96F6F6F6F6F6F'  A0-AF                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C96F6F6F6F6F6F'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D96F6F6F6F6F6F'  D0-D1                    
         DC    XL16'E06FE2E3E4E5E6E7E8E96F6F6F6F6F6F'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F96F6F6F6F6F6F'  F0-FF                    
                                                                                
TRNTAB   DC    C'SA',AL1(ACRTETR),AL1(AGYLETD-AGYLSTD)                          
         DC    AL1(AGYLETC-AGYLSTD)                                             
*&&US*&& DC    C'SB',AL1(ACRTETR),AL1(AGYLETD-AGYLSTD)                          
*&&US*&& DC    AL1(AGYLETC-AGYLSTD)                                             
         DC    C'SE',AL1(ACRTETR),AL1(AGYLETD-AGYLSTD)                          
         DC    AL1(AGYLETC-AGYLSTD)                                             
         DC    C'SJ',AL1(ACRTTRN),AL1(AGYLTDR-AGYLSTD)                          
         DC    AL1(AGYLTCR-AGYLSTD)                                             
*&&UK*&& DC    C'SQ',AL1(ACRTETR),AL1(AGYLETD-AGYLSTD)                          
*&&UK*&& DC    AL1(AGYLETC-AGYLSTD)                                             
         DC    X'FF'                                                            
                                                                                
$$CODE   LOCTR ,                                                                
                                                                                
*********************************************************************           
* Equates.                                                          *           
*********************************************************************           
                                                                                
FFQ      EQU   X'FF'                                                            
EOTQ     EQU   255                                                              
EORQ     EQU   0                                                                
YESQ     EQU   C'Y'                                                             
SPECIALQ EQU   C'S'                                                             
LEDGERQ  EQU   C'L'                                                             
NOQ      EQU   C'N'                                                             
STLDGQ   EQU   C'T'                                                             
SKLDGQ   EQU   C'K'                                                             
SILDGQ   EQU   C'I'                                                             
SPACEQ   EQU   C' '                                                             
L_CHEVQ  EQU   C'<'                                                             
R_CHEVQ  EQU   C'>'                                                             
DATEQ    EQU   C'D'                                                             
TIMEQ    EQU   C'T'                                                             
SLASHQ   EQU   C'/'                                                             
DELIMQ   EQU   C'.'                                                             
SEPARQ   EQU   C';'                                                             
                                                                                
CARDTABD DSECT ,                                                                
CARDKEY  DS    CL10                                                             
CARDKXLN DS    XL1             Length of key                                    
CARDVXLN DS    XL1             Length of field to move directly                 
CARDIND  DS    XL1                                                              
CARDRTNQ EQU   X'80'           Call routine to validate field                   
CARDNUMQ EQU   X'40'           Number comes before the delimeter                
CARDVDSP DS    XL4             Routine to validate field                        
CARDTBLQ EQU   *-CARDTABD                                                       
                                                                                
WORKD    DSECT                 ** GLOBAL WORKING STORAGE **                     
                                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
DUBX     DS    D                                                                
FULL     DS    F                                                                
SAVERE   DS    A                                                                
SAVER2   DS    A                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
DMCB     DS    6F                                                               
DMWORK   DS    9F                                                               
WORK     DS    XL80                                                             
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
TEMP     DS    XL256                                                            
ELEM     DS    XL256                                                            
ELEM2    DS    XL256                                                            
DSOUT    DS    XL500                                                            
TRTAB    DS    XL256                                                            
VDATAMGR DS    V                                                                
VDATCON  DS    V                                                                
VDATVAL  DS    V                                                                
VDYNALLO DS    V                                                                
VXSORT   DS    V                                                                
CUREDIT  DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VHELLO   DS    V                                                                
VCARDS   DS    V                                                                
VLOGIO   DS    V                                                                
VPRINTER DS    V                                                                
VPRNTBL  DS    V                                                                
VUTL     DS    V                                                                
VDDSIO   DS    V                                                                
VNUMVAL  DS    V                                                                
VLOADER  DS    V                                                                
VMQRPT   DS    V                                                                
VRECTYP  DS    V                                                                
ATRKBUF  DS    A                                                                
ATRLOWER DS    A                                                                
ASSB     DS    A                                                                
ABRKBUF  DS    A                                                                
                                                                                
DA       DS    F               Disk address                                     
                                                                                
CURSYS   DS    CL5                                                              
CURPRG   DS    CL8                                                              
CURMQQ   DS    CL(L'MQ_ADV)                                                     
CURSJA   DS    CL(L'ACTKACT)                                                    
RETCODE  DS    XL1                                                              
RUNMODE  DS    CL1                                                              
RUNTSTQ  EQU   C'T'                                                             
FLTCPYS  DS    0X                                                               
FLTCPY1  DS    CL2                                                              
FLTCPY2  DS    CL2                                                              
FLTCPY3  DS    CL2                                                              
FLTCPYL  EQU   *-FLTCPYS                                                        
ANYCARDS DS    CL1                                                              
TRACE    DS    C               TRACE=Y(ES)/N(O)                                 
DSPACE   DS    XL1                                                              
BREAK    DS    CL1                                                              
THISSEAL DS    CL2             Acc alpha code                                   
THISSENO DS    XL1             SE number                                        
                                                                                
PRTLINE  DS    CL80                                                             
                                                                                
KEY      DS    XL(L'ACTKEY)        KEY AREA                                     
         DS    XL(64-L'ACTKEY)                                                  
KEYSAVE  DS    XL64                KEY SAVE AREA                                
                                                                                
LASTKEY  DS    XL42                                                             
                                                                                
AIO1     DS    A                   A(I/O AREA1)                                 
IOWORK1  DS    XL96                                                             
IODA1    DS    XL4                                                              
AIO2     DS    A                   A(I/O AREA2)                                 
IOWORK2  DS    XL96                                                             
IODA2    DS    XL4                                                              
AIO3     DS    A                   A(I/O AREA3)                                 
IOWORK3  DS    XL96                                                             
IODA3    DS    F                                                                
                                                                                
         DS    0H                                                               
AGYLIST  DS    XL(AGYLMXQ*AGYLLNQ+1)                                            
AGYLMXQ  EQU   20                                                               
                                                                                
         DS    0H                                                               
IOAREA1  DS    2048X                                                            
IOAREA2  DS    2048X                                                            
IOAREA3  DS    2048X                                                            
                                                                                
WORKX    DS    0D                                                               
                                                                                
AGYLSTD  DSECT                                                                  
AGYLCPY  DS    XL1                                                              
AGYLALP  DS    CL2                                                              
AGYLCLL  DS    XL1                                                              
AGYLPRL  DS    XL1                                                              
AGYLJOL  DS    XL1                                                              
*AGYLCTR  DS    XL1                                                             
AGYLTDR  DS    PL8                                                              
AGYLTCR  DS    PL8                                                              
AGYLTIM  DS    PL8                                                              
AGYLEXP  DS    PL8                                                              
AGYLORD  DS    PL8                                                              
AGYLEST  DS    PL8                                                              
AGYLETD  DS    PL8                                                              
AGYLETC  DS    PL8                                                              
AGYLLNQ  EQU   *-AGYLSTD                                                        
                                                                                
TRNTABD  DSECT                                                                  
TRNTULG  DS    CL(L'ACTKUNT+L'ACTKLDG) unit/ledger                              
TRNTREQ  DS    XL1                     record type equate                       
TRNTDDA  DS    XL1                     displacement to deb accumulator          
TRNTDDC  DS    XL1                     displacement to cred accumulator         
TRNTABL  EQU   *-TRNTABD                                                        
                                                                                
BRKBUFD  DSECT                                                                  
BRKBCPY  DS    XL1                                                              
BRKBALP  DS    CL2                                                              
BRKBCLI  DS    CL6                                                              
BRKBPRO  DS    CL6                                                              
BRKBUNT  DS    CL2                                                              
***BRKBJOB  DS    CL7                                                           
***BRKBNUM  DS    CL6                                                           
BRKBKLQ  EQU   *-BRKBUFD                                                        
BRKBTDR  DS    PL8                                                              
BRKBTD#  DS    PL6                                                              
BRKBTCR  DS    PL8                                                              
BRKBTC#  DS    PL6                                                              
BRKBTIM  DS    PL8                                                              
BRKBTI#  DS    PL6                                                              
BRKBEXP  DS    PL8                                                              
BRKBEX#  DS    PL6                                                              
BRKBORD  DS    PL8                                                              
BRKBOR#  DS    PL6                                                              
BRKBEST  DS    PL8                                                              
BRKBES#  DS    PL6                                                              
BRKBEDR  DS    PL8                                                              
BRKEED#  DS    PL6                                                              
BRKBECR  DS    PL8                                                              
BRKEEC#  DS    PL6                                                              
BRKBLNQ  EQU   *-BRKBUFD                                                        
BRKBMAX  EQU   50000                                                            
                                                                                
CPYOUTD  DSECT                                                                  
CPYOREGH DS    0CL6                                                             
         DS    CL2                                                              
CPYOREG  DS    CL2                                                              
         DS    CL2                                                              
         DS    CL1                                                              
CPYOALPH DS    0CL5                                                             
         DS    CL2                                                              
CPYOALP  DS    CL2                                                              
         DS    CL1                                                              
         DS    CL1                                                              
CPYOSYSH DS    0CL6                                                             
         DS    CL1                                                              
CPYOSYS  DS    CL5                                                              
         DS    CL1                                                              
CPYOCLIH DS    0CL6                                                             
CPYOCLI  DS    CL6                                                              
CPYOPROH DS    0CL7                                                             
         DS    CL1                                                              
CPYOPRO  DS    CL6                                                              
         DS    CL1                                                              
CPYOTDR  DS    0CL15                                                            
         DS    CL3                                                              
CPYOTDRH DS    CL12                                                             
         DS    CL1                                                              
CPYOTCR  DS    0CL15                                                            
         DS    CL3                                                              
CPYOTCRH DS    CL12                                                             
         DS    CL1                                                              
CPYOTIM  DS    0CL15                                                            
         DS    CL3                                                              
CPYOTIMH DS    CL12                                                             
         DS    CL1                                                              
CPYOEXP  DS    0CL15                                                            
         DS    CL3                                                              
CPYOEXPH DS    CL12                                                             
         DS    CL1                                                              
CPYOORD  DS    0CL15                                                            
         DS    CL3                                                              
CPYOORDH DS    CL12                                                             
         DS    CL1                                                              
CPYOEST  DS    0CL15                                                            
         DS    CL3                                                              
CPYOESTH DS    CL12                                                             
         DS    CL1                                                              
CPYOLNQ  EQU   *-CPYOUTD                                                        
         DS    XL(L'P-CPYOLNQ)                                                  
                                                                                
CPY2UTD  DSECT                                                                  
CPY2EDR  DS    0CL15                                                            
         DS    CL3                                                              
CPY2EDRH DS    CL12                                                             
         DS    CL1                                                              
CPY2ECR  DS    0CL15                                                            
         DS    CL3                                                              
CPY2ECRH DS    CL12                                                             
         DS    CL1                                                              
CPY2LNQ  EQU   *-CPY2UTD                                                        
         DS    XL(L'P-CPY2LNQ)                                                  
                                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
                                                                                
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
                                                                                
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
                                                                                
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
                                                                                
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
                                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
                                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
                                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
                                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
                                                                                
* DDCTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
                                                                                
CTLSTD   DSECT                                                                  
         ORG   CTLSTDTA                                                         
CTLSTNAM DS    CL7                 SE NAME                                      
CTLSTSYS DS    XL1                 CALLOV SYSTEM NUMBER                         
CTLSTSE  DS    XL1                 SE NUMBER                                    
         ORG                                                                    
                                                                                
CORE     CSECT                     ** OTHER LARGE AREAS **                      
TRKBUF   DS    (64*1024)X                                                       
                                                                                
MASTC    CSECT                                                                  
         DC    10000AL1(0)                                                      
                                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
                                                                                
SSB      CSECT                                                                  
         DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSOOFF)                                             
         DC    X'FF'               OFFLINE EXTENSION IN USE                     
         ORG   SSB+(SSOSTAT2-SSOOFF)                                            
         DC    AL1(0)                                                           
         ORG                                                                    
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012AGXRECON  08/11/20'                                      
         END                                                                    
