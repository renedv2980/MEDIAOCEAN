*          DATA SET ACBLKDRIV  AT LEVEL 042 AS OF 10/15/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE ACBLKDRA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'ACC BLOCKCHAIN DRIVER'                                          
***********************************************************************         
*                                                                               
* THIS IS THE ACC DRIVER MODULE TO CALL ICETOOL.                                
* MQ FILENAME FOR THE FOLLOWING ASSETS:                                         
* AGENCYINVOICE, CASHPAID, CASHRECEIPTS, SUPPLIERINVOICE                        
* THE REST READS THE XMLOUT FILE AND MQPUTS EACH HEADER AS AN XML FILE          
*                                                                               
***********************************************************************         
ACBLKDRV CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ENTRY SSB                                                              
*                                                                               
         NBASE 0,ACBLKDRV,=V(REGSAVE),R9                                        
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         BAS   RE,READCRDS         READ PARAMETER CARDS                         
         JNE   *+2                 INVALID PARAMETER CARD SEEN                  
*                                                                               
         BAS   RE,CALLICE          GENERATE THE XML                             
         LTR   R3,R3               R3 = ICETOOL RETURN CODE                     
         BZ    GETDSN              RETURN CODE 0: WE HAVE XML                   
*                                                                               
         MVC   P(32),=C'NO RECORDS GENERATED IN XML FILE'                       
         GOTO1 =V(PRINTER)                                                      
         B     XBASE                                                            
*                                                                               
GETDSN   DS    0H                                                               
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'D',=CL8'XMLOUT'),CARD                       
         CLI   DMCB+4,0            IS XMLOUT ALLOCATED?                         
         JZ    *+2                 NO ?!?                                       
*                                                                               
         LLC   R1,DMCB+4           L'RETURNED DSN                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   XMLDSN(0),CARD      XMLDSN IS THE DSN FOR XMLOUT                 
         MVC   P(13),=C'XMLOUT DSN = '                                          
         MVC   P+13(L'XMLDSN),XMLDSN                                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*** WRITE THE MQ MESSAGE HERE, OR WRITE THE DSN TO A SEQUENTIAL FILE            
*** AND SEND THE MQ MESSAGE IN A SUBSEQUENT JOB STEP                            
*                                                                               
         CLC   =C'NULLFILE',XMLDSN    ONLY IF THERE IS A REAL FILE              
         JE    XBASE                                                            
*                                                                               
         CLC   =C'AGENCYINVOICE',ASSETID    THESE 4 MQ FILENAME                 
         JE    ABDRV100                                                         
         CLC   =C'CASHPAID',ASSETID                                             
         JE    ABDRV100                                                         
         CLC   =C'CASHRECEIPTS',ASSETID                                         
         JE    ABDRV100                                                         
         CLC   =C'SUPPLIERINVOICE',ASSETID                                      
         JNE   ABDRV300                     THE REST MQ 1 RECORD EACH           
ABDRV100 BAS   RE,MQRPQ                                                         
         J     XBASE                                                            
*                                                                               
ABDRV300 OPEN  (XMLOUT,(INPUT))    TRY TO OPEN XML FILE                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,XMLHEAD          GET THE XML HEADER                           
         BRAS  RE,PUTXML           GET RECORD, PUT OUT EACH REC AS XML          
*                                                                               
NOMORE   DS    0H                                                               
         CLOSE (XMLOUT,)           CLOSE XML FILE                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XBASE    DS    0H                                                               
         XBASE ,                                                                
         EJECT                                                                  
*                                                                               
XMLHEAD  NTR1  ,                                                                
         MVC   XML1,MYSPACES                                                    
         GET   XMLOUT,XML1                                                      
XMLHD10  BAS   RE,RDLINE                                                        
         MVC   SN1TAG,CURRLINE                                                  
         GOTO1 GENENTAG,DMCB,SN1TAG,EN1TAG                                      
*                                                                               
         MVI   NETORDER,C'N'                                                    
         MVI   PRTORDER,C'N'                                                    
         CLC   =C'<n1:netOrder',SN1TAG                                          
         BNE   XMLHD20                                                          
         MVI   NETORDER,C'Y'                                                    
         MVC   SN1TAG,MYSPACES                  as per Rajiv, xmlns             
         MVC   SN1TAG(10),=C'<netOrder>'        info messes him up              
         MVC   EN1TAG_A,MYSPACES                                                
         MVC   EN1TAG_A(11),=C'</netOrder>'                                     
*                                                                               
XMLHD20  CLC   =C'<n1:spectraPrintOrder',SN1TAG                                 
         BNE   XIT                                                              
         MVI   PRTORDER,C'Y'                                                    
         MVC   SN1TAG,MYSPACES                  as per Rajiv, xmlns             
         MVC   SN1TAG(19),=C'<spectraPrintOrder>'                               
         MVC   EN1TAG,MYSPACES                                                  
         MVC   EN1TAG(23),=C'</n1:spectraPrintOrder>'                           
         MVC   EN1TAG_A,MYSPACES                                                
         MVC   EN1TAG_A(20),=C'</spectraPrintOrder>'                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
GENENTAG NTR1                                                                   
         L     RE,0(R1)            START TAG                                    
         L     RF,4(R1)            END TAG                                      
         LA    R1,L'CURRLINE                                                    
*                                                                               
         MVC   0(255,RF),MYSPACES                                               
*                                                                               
GENTAG10 DS    0H                                                               
         CLI   0(RE),C' '          SPACE                                        
         BE    GENTAG30                                                         
         CLI   0(RE),C'>'          OR >, TAG IS COMPLETE                        
         BE    GENTAG30                                                         
         MVC   0(1,RF),0(RE)                                                    
         AHI   RE,1                                                             
*                                                                               
         CLI   0(RF),C'<'          START OF A TAG                               
         BNE   GENTAG20                                                         
         MVI   1(RF),C'/'          DEFINE ENDING TAG                            
         AHI   RF,1                                                             
GENTAG20 AHI   RF,1                                                             
         BCT   R1,GENTAG10                                                      
*                                                                               
GENTAG30 MVI   0(RF),C'>'          WRAP UP THE ENDING TAG                       
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
* MAIN LOOPING OF THE ORIGINAL XML FILE                                         
*======================================================================         
PUTXML   NTR1  ,                                                                
PXML010  BAS   RE,RDLINE                                                        
         CLI   NETORDER,C'Y'                                                    
         BNE   PXML030                                                          
         CLC   =C'<packkey>',CURRLINE                                           
         BNE   PXML030                                                          
         CLC   LASTPKEY,MYSPACES   FIRST PACKAGE KEY?                           
         BNE   PXML020                                                          
         MVC   LASTPKEY,CURRLINE                                                
         MVI   NEWXML,C'Y'                                                      
         B     PXML010                                                          
*                                                                               
PXML020  CLC   LASTPKEY,CURRLINE   PACKAGE KEY THE SAME?                        
         BE    PXML010             YES, JUST KEEP GOING                         
         MVI   NEWXML,C'Y'         NO, HAVE TO SEND THIS XML TO MQ              
         MVC   LASTPKEY,CURRLINE                                                
         BAS   RE,FINMQ                                                         
         B     PXML900                                                          
*                                                                               
PXML030  CLC   CURRLINE,EN1TAG     END OF XML?                                  
         BE    PXML9990            YES, GET OUTTA HERE                          
         CLI   RECTAG,C'Y'                                                      
         BE    PXML100                                                          
         MVI   RECTAG,C'Y'                                                      
         MVC   SRECTAG,CURRLINE                                                 
         GOTO1 GENENTAG,DMCB,SRECTAG,NRECTAG                                    
*                                                                               
PXML100  CLC   CURRLINE,SRECTAG    START OF THIS FILE                           
         BNE   PXML200                                                          
         BAS   RE,BEGXML           BEGIN THE XML FILE                           
*                                                                               
PXML200  CLI   NETORDER,C'Y'                                                    
         BE    PXML210                                                          
         CLI   PRTORDER,C'Y'                                                    
         BNE   PXML800                                                          
PXML210  CLC   CURRLINE,SRECTAG    SKIP SENDING <Header>                        
         BE    PXML900                                                          
         CLC   CURRLINE,NRECTAG    SKIP SENDING </Header>                       
         BE    PXML900                                                          
*                                                                               
         CLI   PRTORDER,C'Y'       PRINT ORDER CAN SKIP BELOW                   
         BE    PXML800                                                          
*                                                                               
         CLI   INPRDCST,C'Y'                                                    
         BE    PXML300                                                          
         CLI   INGRTBLK,C'Y'                                                    
         BE    PXML300                                                          
         CLC   =C'<product>',CURRLINE                                           
         BE    PXML230                                                          
         CLC   =C'<cost>',CURRLINE                                              
         BE    PXML230                                                          
         CLC   =C'<demo>',CURRLINE                                              
         BE    PXML230                                                          
         CLC   =C'<packageGuaranteeFactor>',CURRLINE                            
         BE    PXML240                                                          
         CLC   =C'<demoGuaranteeFactor>',CURRLINE                               
         BE    PXML240                                                          
         CLC   =C'<packageCPM>',CURRLINE                                        
         BE    PXML240                                                          
         B     PXML800                                                          
PXML230  MVI   INPRDCST,C'Y'       IN PRODUCT, COST, OR DEMO BLOCK              
         B     PXML250                                                          
PXML240  MVI   INGRTBLK,C'Y'                                                    
PXML250  MVC   SPRCSTAG,CURRLINE                                                
         GOTO1 GENENTAG,DMCB,SPRCSTAG,NPRCSTAG                                  
         B     PXML010             LOOP BACK FOR MORE                           
*                                                                               
PXML300  CLI   PCPRINT,C'N'        OK TO SKIP THE WHOLE BLOCK                   
         BE    PXML800             YES                                          
         CLC   =C'<productCode>',CURRLINE                                       
         BE    PXML310                                                          
         CLC   =C'<type>',CURRLINE                                              
         BE    PXML310                                                          
         CLC   =C'<category>',CURRLINE                                          
         BE    PXML310                                                          
         CLC   =C'<demoValue>',CURRLINE                                         
         BNE   PXML303                                                          
         MVI   INGRTBLK,C'Y'                                                    
PXML303  CLI   INGRTBLK,C'Y'                                                    
         BNE   PXML800                                                          
         CLC   =C'<value>',CURRLINE                                             
         BNE   PXML800                                                          
PXML305  MVI   PCPRINT,C'Y'                                                     
PXML310  CLC   =C'<productCode>@@@',CURRLINE                                    
         BE    PXML330                                                          
         CLC   =C'<type>@@@',CURRLINE                                           
         BE    PXML330                                                          
         CLC   =C'<category>@@@',CURRLINE                                       
         BE    PXML330                                                          
         CLI   INGRTBLK,C'Y'                                                    
         BNE   PXML350                                                          
         CLC   =C'<value>0</value>',CURRLINE                                    
         B     PXML800                                                          
*        BNE   PXML350                                                          
PXML330  MVI   PCPRINT,C'N'        DON'T PRINT THIS PROD/COST BLOCK             
*                                                                               
PXML350  GOTO1 LINXML,DMCB,SPRCSTAG                                             
*                                                                               
PXML800  GOTO1 LINXML,DMCB,CURRLINE                                             
         CLI   INPRDCST,C'Y'                                                    
         BE    PXML850                                                          
         CLI   INGRTBLK,C'Y'                                                    
         BNE   PXML900                                                          
PXML850  CLC   CURRLINE,NPRCSTAG   END TAG FOR PROD/COST                        
         BNE   PXML900                                                          
         MVI   INPRDCST,C'N'                                                    
         MVI   INGRTBLK,C'N'                                                    
         MVI   PCPRINT,C'Y'                                                     
*                                                                               
PXML900  CLC   CURRLINE,NRECTAG    DONE WITH THIS FILE                          
         BNE   PXML010             NO, LOOP BACK                                
         BAS   RE,FINXML           FINISH UP THE XML FILE                       
         CLI   PRTORDER,C'Y'                                                    
         BNE   PXML010                                                          
         BAS   RE,FINMQ            FINISH UP THE MQ FILE                        
*                                                                               
         B     PXML010             AND LOOP BACK FOR MORE                       
*=========================================================                      
PXML9990 DS    0H                                                               
         CLI   NETORDER,C'Y'       NETORDER HAS TO PUSH OUT LAST XML            
         BNE   XIT                                                              
         MVI   NEWXML,C'Y'                                                      
         BAS   RE,FINMQ            FINISH UP THE MQ FILE                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
RDLINE   NTR1  ,                                                                
RDLIN010 MVC   TAPEIO,MYSPACES                                                  
         GET   XMLOUT,TAPEIO                                                    
         CLC   TAPEIO,MYSPACES     SKIP BLANK LINES                             
         BNH   RDLIN010                                                         
*                                                                               
         LA    R1,L'CURRLINE                                                    
         LA    RE,TAPEIO                                                        
         MVC   CURRLINE,MYSPACES                                                
*        CLI   DEBUG,C'Y'                                                       
*        BE    RDLIN200                                                         
*                                                                               
RDLIN100 CLI   0(RE),C' '                                                       
         BH    RDLIN200                                                         
         AHI   RE,R1                                                            
         BCT   R1,RDLIN100                                                      
*                                                                               
RDLIN200 AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CURRLINE(0),0(RE)                                                
*                                                                               
         CLC   =C'<status>@@@</status>',CURRLINE      SKIP FOR NOW              
         JE    RDLIN010                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
* BEGIN THE XML FILE BEFORE OUTPUTTING RECORDS                                  
*======================================================================         
BEGXML   NTR1  ,                                                                
         CLI   NETORDER,C'Y'                                                    
         JNE   BEGXML05                                                         
         CLI   NEWXML,C'Y'                                                      
         JNE   BEGXML10                                                         
BEGXML05 CLI   DEBUG,C'Y'                                                       
         JE    BEGXML10                                                         
         GOTO1 =V(MQRPT),DMCB,(0,=C'OPEN'),(0,MQFILID),(X'E0',0),0              
*                                                                               
         CLI   DMCB+8,0            MQ OPEN FAILS ?                              
         JE    BEGXML10            NO, CONTINUE                                 
*                                  YES ,                                        
         MVC   AUTOREAS,OPENFAIL   SEND EMAIL NOTIFICATION AND DUMP             
         J     MQERREND                                                         
*                                                                               
BEGXML10 DS    0H                                                               
         CLI   NETORDER,C'Y'                                                    
         JNE   BEGXML20                                                         
         CLI   NEWXML,C'N'                                                      
         BE    BEGXML50                                                         
         MVI   NEWXML,C'N'                                                      
*                                                                               
BEGXML20 MVC   MQASST,ASSETID                                                   
         MVC   MQMESS3,MYSPACES                                                 
         MVC   MQMESS3(MQM1LNQ),MQMESS1                                         
         GOTO1 LINXML,DMCB,MQMESS3                                              
         GOTO1 LINXML,DMCB,XML1                                                 
         CLI   NETORDER,C'Y'                                                    
         JNE   BEGXML50                                                         
         GOTO1 LINXML,DMCB,NCOLSTAG                                             
BEGXML50 GOTO1 LINXML,DMCB,SN1TAG                                               
*                                                                               
         B     XIT                                                              
*======================================================================         
* FINISH THE XML FILE                                                           
*======================================================================         
FINXML   NTR1  ,                                                                
         LA    RF,EN1TAG_A                                                      
FINXML3  GOTO1 LINXML,DMCB,(RF)                                                 
         J     XIT                                                              
*                                                                               
FINMQ    NTR1                                                                   
         CLI   NEWXML,C'Y'                                                      
         BNE   FINMQX                                                           
         CLI   NETORDER,C'Y'                                                    
         JNE   FINMQ3                                                           
         GOTO1 LINXML,DMCB,NCOLETAG    netOrderColleciton                       
*                                                                               
FINMQ3   CLI   DEBUG,C'Y'                                                       
         BE    FINMQX                                                           
         GOTO1 =V(MQRPT),DMCB,(0,=C'CLOSE'),0,0,0                               
         CLI   DMCB+8,0            IF MQ PUT FAILS                              
         BE    FINMQX                                                           
         MVC   AUTOREAS,CLOSFAIL                                                
         J     MQERREND                                                         
*                                                                               
FINMQX   B     XIT                                                              
*======================================================================         
* SEND A LINE TO XML FILE VIA MQ                                                
*======================================================================         
LINXML   NTR1  ,                                                                
         CLI   NETORDER,C'Y'       NET ORDER AND                                
         BNE   LINXML3                                                          
         CLI   INPRDCST,C'Y'       IN PRODUCT / COST BLOCK OR                   
         BE    LINXML2                                                          
         CLI   INGRTBLK,C'Y'       IN GUARANTEE BLOCK                           
         BNE   LINXML3                                                          
LINXML2  CLI   PCPRINT,C'N'        DON'T PRINT?                                 
         BE    LINXMLX             YES, LEAVE                                   
*                                                                               
LINXML3  DS    0H                                                               
         L     RE,0(R1)            ADDRESS OF LINE TO SEND                      
         LR    R7,RE                                                            
         CLI   DEBUG,C'Y'          NO NEED TO SHRINK IN DEBUG                   
         BE    LINXML9                                                          
         LA    RF,L'CURRLINE       MAX LENGTH OF LINE                           
         AHI   RE,L'CURRLINE-1     POINT TO LAST CHAR OF LINE                   
*                                                                               
LINXML5  CLI   0(RE),C' '          FIND LEN OF LINE W/O TRAILING SPACES         
         BH    LINXML8                                                          
         AHI   RE,-1                                                            
         AHI   RF,-1                                                            
         B     LINXML5                                                          
*                                                                               
LINXML8  CLI   DEBUG,C'Y'                                                       
         BE    LINXML9                                                          
         GOTO1 =V(MQRPT),DMCB,(0,=C'PUT'),(R7),(RF),0                           
         CLI   DMCB+8,0            MQ PUT FAILS ?                               
         JE    LINXMLX             NO, CONTINUE                                 
*                                                                               
         MVC   AUTOREAS,PUTFAIL    SEND EMAIL NOTIFICATION                      
*                                                                               
MQERREND DS    0H                  COMMENT OUT BELOW IN CASE ABEND              
*QERREND GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',(=AL1(AUTOLENG),AUTONOTE)             
         DC    H'00'                                                            
*                                                                               
LINXML9  PUT   OUTFIL,(R7)                                                      
LINXMLX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
READCRDS NTR1  ,                                                                
*                                                                               
         MVC   TITLE(19),=C'ACC/BLKCHAIN DRIVER'                                
         MVC   P(16),=C'PARAMETER CARDS:'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
NEXTCARD DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE10'                                     
         CLC   =C'/*',CARD                                                      
         BE    RDCARD90            NO MORE CARDS                                
*                                                                               
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)         PRINT THE PARAMETER CARD                     
*                                                                               
         CLI   CARD,C'*'           COMMENT?                                     
         BE    NEXTCARD            YES                                          
*                                                                               
         CLC   =C'DDSIO=',CARD                                                  
         BNE   NXTCRD05                                                         
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6      DDSIO= OVERRIDE                              
         B     NEXTCARD                                                         
*                                                                               
NXTCRD05 CLC   =C'DSPACE=',CARD                                                 
         BNE   NXTCRD08                                                         
         LARL  RF,SSB                                                           
         MVC   SSODSPAC-SSOOFF(,RF),CARD+7   DSPACE= OVERRIDE                   
         B     NEXTCARD                                                         
*                                                                               
NXTCRD08 CLC   =C'MQFILID=',CARD   MQFILID SETUP                                
         BNE   NXTCRD09                                                         
         MVC   MQFILID,CARD+8                                                   
         CLC   =C'DEBUG',MQFILID                                                
         BE    NXTCRD8B                                                         
         CLC   =C'GORDONHO',MQFILID                                             
         BNE   NEXTCARD                                                         
NXTCRD8B MVI   DEBUG,C'Y'          DEBUG MODE                                   
         OPEN  (OUTFIL,OUTPUT)                                                  
         B     NEXTCARD                                                         
*                                                                               
NXTCRD09 CLC   =C'ASSET=',CARD     ASSET SETUP                                  
         BNE   NXTCRD10                                                         
         MVC   ASSETID,CARD+6                                                   
         B     NEXTCARD                                                         
                                                                                
NXTCRD10 B     NO                  INVALID PARAMETER CARD SEEN                  
*                                                                               
RDCARD90 DS    0H                                                               
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     YES                 ALL CARDS ARE VALID                          
*                                                                               
         ANSR  ,                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
* CALL ICETOOL TO GENERATE THE XML                                              
*                                                                               
CALLICE  NTR1  ,                                                                
*                                                                               
         MVC   P(42),=C'ABOUT TO CALL ICETOOL VIA TOOLIN INTERFACE'             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SR    R1,R1               USE TOOLIN INTERFACE                         
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LR    R3,RF                                                            
         CHI   R3,4                R3 = HIGHEST DFSORT RETURN CODE              
         JH    *+2                 RETURN CODE > 4: SHOULDN'T HAPPEN            
*                                                                               
         MVC   P(30),=C'SUCCESSFUL RETURN FROM ICETOOL'                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*********************************************************************           
* WRITE OUT MQ HEADER TO THE MQ FILE                                *           
*********************************************************************           
MQRPQ    NTR1  ,                                                                
         L     RC,0(R1)            RESET RC                                     
*                                  CONTINUE                                     
         CLI   DEBUG,C'Y'                                                       
         BE    MQRPQ00                                                          
         GOTO1 =V(MQRPT),DMCB,(0,=C'OPEN'),(0,MQFILID),(X'E0',0),0              
*                                                                               
         CLI   DMCB+8,0            MQ OPEN FAILS ?                              
         JE    MQRPQ00             NO, CONTINUE                                 
*                                  YES ,                                        
         MVC   AUTOREAS,OPENFAIL   SEND EMAIL NOTIFICATION AND DUMP             
         J     MQERREND                                                         
*                                                                               
MQRPQ00  DS    0H                                                               
                                                                                
         CLC   ASSETID,SPACES                                                   
         BNE   *+10                                                             
         MVC   ASSETID,=CL15'AGENCYINVOICE'                                     
                                                                                
         MVC   MQASST,ASSETID                                                   
         LA    RF,MQM1LNQ                                                       
         LA    RE,MQMESS1+MQM1LNQ-1                                             
MQRPQ01  CLI   0(RE),C' '                                                       
         BH    MQRPQ02                                                          
         AHI   RE,-1                                                            
         AHI   RF,-1                                                            
         B     MQRPQ01                                                          
*                                                                               
MQRPQ02  DS    0H                                                               
         CLI   DEBUG,C'Y'                                                       
         BE    MQPRQ023                                                         
         GOTO1 =V(MQRPT),DMCB,(0,=C'PUT'),MQMESS1,(RF),0                        
         CLI   DMCB+8,0            MQ PUT FAILS ?                               
         JNE   MQRPQFL             NO, CONTINUE                                 
         J     MQPRQ027                                                         
*                                                                               
MQPRQ023 PUT   OUTFIL,MQMESS1                                                   
*                                  YES,                                         
MQPRQ027 MVC   MQMDSN(L'MQMDSN),SPACES   SET DSN NAME                           
         MVC   MQMDSN(L'XMLDSN),XMLDSN                                          
*                                                                               
         LA    RF,MQM2LNQ                                                       
         LA    RE,MQMESS2+MQM2LNQ-1                                             
MQRPQ03  CLI   0(RE),C' '                                                       
         BH    MQRPQ05                                                          
         AHI   RE,-1                                                            
         AHI   RF,-1                                                            
         B     MQRPQ03                                                          
*                                                                               
MQRPQ05  DS    0H                                                               
         CLI   DEBUG,C'Y'                                                       
         BE    MQRPQ07                                                          
         GOTO1 =V(MQRPT),DMCB,(0,=C'PUT'),MQMESS2,(RF),0                        
         CLI   DMCB+8,0            MQ PUT FAILS ?                               
         JNE   MQRPQFL             NO, CONTINUE                                 
         J     MQRPQ09                                                          
*                                                                               
MQRPQ07  PUT   OUTFIL,MQMESS2                                                   
*                                                                               
MQRPQ09  DS    0H                                                               
         CLI   DEBUG,C'Y'                                                       
         BE    MQRPQX                                                           
         GOTO1 =V(MQRPT),DMCB,(0,=C'CLOSE'),0,0,0                               
         CLI   DMCB+8,0            IF MQ PUT FAILS                              
         JE    MQRPQX                                                           
         MVC   AUTOREAS,CLOSFAIL                                                
*                                                                               
MQRPQFL  MVC   AUTOREAS,PUTFAIL    SEND EMAIL NOTIFICATION                      
         J     MQERREND                                                         
*MQERREND GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',(=AL1(AUTOLENG),AUTONOTE)            
         DC    H'00'                                                            
*                                                                               
MQRPQX   XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
         ENTRY XMLOUT                                                           
XMLOUT   DCB   DDNAME=XMLOUT,DSORG=PS,MACRF=GM,RECFM=FB,EODAD=NOMORE            
*UTFIL   DCB   DDNAME=OUTFIL,DSORG=PS,MACRF=PM,RECFM=FB,LRECL=330               
OUTFIL   DCB   DDNAME=OUTFIL,DSORG=PS,MACRF=PM,                        X        
               RECFM=FB,LRECL=255,BUFNO=2,BLKSIZE=5100                          
*                                                                               
*********************************************************************           
* MQ CONSTANTS                                                      *           
*********************************************************************           
MQFILID  DC    CL16'DSEXTRACT*******'                                           
ASSETID  DC    CL15' '                                                          
AUTONOTE DC    C'AUTONOTE*JSHA,GHOA'                                            
AUTOREAS DS    CL15                                                             
AUTOLENG EQU   *-AUTONOTE                                                       
OPENFAIL DC    CL(L'AUTOREAS)'MQ OPEN FAILED'                                   
PUTFAIL  DC    CL(L'AUTOREAS)'MQ PUT ERROR'                                     
CLOSFAIL DC    CL(L'AUTOREAS)'MQ CLOSE FAILED'                                  
SETUFAIL DC    CL(L'AUTOREAS)'NOT SETUP TO MQ'                                  
*********************************************************************           
* MQ TABLES                                                         *           
*********************************************************************           
MQMESS1  DC    X'0D25'                                                          
         DC    C'ASSET='                                                        
MQASST   DC    CL15' '                                                          
MQM1LNQ  EQU   *-MQMESS1                                                        
*                                                                               
MQMESS2  DC    C'FILENAME='                                                     
MQMDSN   DC    CL40' '             DATASET NAME                                 
MQM2LNQ  EQU   *-MQMESS2                                                        
*                                                                               
MQMESS3  DS    CL255                                                            
MQM3LNQ  EQU   *-MQMESS2                                                        
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
VMQRPT   DS    A                   A(MQ FILE INTERFACE)                         
CARD     DS    CL80                                                             
XMLDSN   DC    CL44' '             DSN OF XMLOUT                                
RECTAG   DC    C'N'                RECTAG DEFINED?                              
NETORDER DS    C                   NET ORDER FILE                               
PRTORDER DS    C                   PRT ORDER FILE                               
INPRDCST DC    C'N'                IN PRODUCT OR COST BLOCK                     
INGRTBLK DC    C'N'                IN GUARANTEE BLOCK                           
PCPRINT  DC    C'Y'                PRODUCT OR COST, PRINT?                      
DEBUG    DC    C'N'                DEBUG MODE                                   
NEWXML   DC    C'Y'                                                             
*                                                                               
MYSPACES DC    CL255' '                                                         
TAPEIO   DS    CL255                                                            
CURRLINE DS    CL255               CURRENT LINE                                 
XML1     DC    CL255'<?xml version="1.0" encoding="UTF-8" ?>'                   
SN1TAG   DC    CL255' '            N1 TAG                                       
EN1TAG   DC    CL255' '                                                         
EN1TAG_A DC    CL255' '            AMENDED NETORDER END TAG                     
SRECTAG  DC    CL255' '            RECORD TAG                                   
NRECTAG  DC    CL255' '                                                         
SPRCSTAG DC    CL255' '            START PRODUCT OR COST TAG                    
NPRCSTAG DC    CL255' '            END PRODUCT OR COST TAG                      
NCOLSTAG DC    CL255'<netOrdersCollection>'                                     
NCOLETAG DC    CL255'</netOrdersCollection>'                                    
*                                                                               
LASTPKEY DC    CL128' '            LAST PACKKEY                                 
*                                                                               
         DS    0D                                                               
         DC    CL16'******SSB*******'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOMTIND            SYSTEM DATAMGR FLAGS                         
         DC    AL1(SSOWRTN)        WRITE=NO (DON'T OPEN FOR UPDATE)             
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* ++INCLUDE DDDPRINT                                                            
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*DDMASTC                                                                        
         PRINT OFF                                                              
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
         SPACE 2                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042ACBLKDRIV 10/15/20'                                      
         END                                                                    
