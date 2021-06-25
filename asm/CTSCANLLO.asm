*          DATA SET CTSCANLLO  AT LEVEL 007 AS OF 06/18/18                      
*PHASE CTLLOA                                                                   
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE DLFLD                                                                  
*                                                                               
         TITLE 'CTLLO - SCAN / REPORT ON PERSON LAST LOGON ACTIVITY'            
*                                                                               
         PRINT NOGEN                                                            
CTLLO    CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE WORKX-WORKD,**CLLO**,=V(REGSAVE),R9                              
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         L     RA,=A(COMMON)                                                    
         USING COMMON,RA                                                        
Q        USING PQPLD,QLINE                                                      
*                                                                               
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO1                                                          
         SHI   R1,8                                                             
         MVC   0(8,R1),=C'**IOA1**'                                             
*                                                                               
         L     R1,=A(IOAREA2-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO2                                                          
         SHI   R1,8                                                             
         MVC   0(8,R1),=C'**IOA2**'                                             
*                                                                               
         L     R1,=A(BUFF)                                                      
         ST    R1,ABUFF                                                         
*                                                                               
         USING PLINED,PLINE                                                     
*                                                                               
         BRAS  RE,INIT             READ CARDS ECT                               
         JNE   *+2                                                              
         BRAS  RE,MAIN             MAIN LOOP                                    
         BRAS  RE,DONE             CLOSE ALL                                    
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
                                                                                
***********************************************************************         
* READ DATA LINES                                                               
***********************************************************************         
MAIN     NTR1                                                                   
*                                                                               
MAIN010  BRAS  RE,GETACCR          GET ACCESS RECORD                            
         BNE   MAINX                                                            
         BRAS  RE,GETPER           GET PERSON RECORD                            
         BNE   MAINX                                                            
         B     MAIN010                                                          
*                                                                               
MAINX    NI    SINDI,X'FF'-SIOTOP  STOP OUTPUT TO DATA SET                      
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
* INITIALISE                                                                    
***********************************************************************         
INIT     NTR1                                                                   
*                                                                               
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
*                                                                               
         ZAP   LINE,PKZERO                                                      
         ZAP   PAGE,PKONE                                                       
*                                                                               
         ZAP   GTOTCNT,PKZERO      PERSON RECORDS                               
         ZAP   GUNICNT,PKZERO      UNIQUE PERSONS                               
         ZAP   GTERCNT,PKZERO      TERMINATED PERSONS                           
         ZAP   GEXPCNT,PKZERO      ACTIVE PERSONS WITH EXPIRED PASSWORD         
*                                                                               
         XC    CUAGY,CUAGY                                                      
         MVI   SINDI,0             STATUS INDICATOR                             
*                                                                               
         LA    R3,CARD                                                          
INIT010  GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
         CLC   =C'DDSIO=',0(R3)                                                 
         BNE   INIT020                                                          
         L     RF,=V(DDSIO)        OVERRIDE DDSIO NAME                          
         MVC   0(8,RF),6(R3)                                                    
         B     INIT010                                                          
*                                                                               
INIT020  CLC   =C'DSPACE=',0(R3)                                                
         BNE   INIT030                                                          
         L     RF,=A(SSB)          SET DSPACE ID IN SSB                         
         MVC   SSODSPAC-SSOOFF(1,RF),7(R3)                                      
         B     INIT010                                                          
*                                                                               
INIT030  GOTO1 VDATCON,DMCB,(5,0),(2,TODAY2)                                    
         GOTO1 VDATCON,DMCB,(2,TODAY2),(0,TODAY0)                               
         GOTO1 VDATCON,DMCB,(2,TODAY2),(3,TODAY3)                               
*                                                                               
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(35),=CL35' * SECURITY ACTIVE PERSON REPORT * '             
         MVC   TITLE+35(10),=CL10'   RUN ON '                                   
         GOTO1 VDATCON,DMCB,(2,TODAY2),(5,TITLE+45)                             
         LA    R1,TITLE            PRINT PARAMETER CARDS TITLE                  
         BRAS  RE,PRINTT                                                        
         LA    R3,CARD                                                          
         B     INIT102                                                          
*                                                                               
INIT100  GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
INIT102  CLC   =C'/*',0(R3)                                                     
         BE    INIT200                                                          
         CLC   =C'XX',0(R3)                                                     
         BE    INIT200                                                          
         MVC   PLINE+1(80),0(R3)                                                
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         CLC   =C'WRITE=',0(R3)                                                 
         BNE   INIT120                                                          
         CLC   6(3,R3),=C'YES'                                                  
         BNE   *+8                                                              
         MVI   RCWRITE,C'Y'        WRITE RECORD                                 
         B     INIT100                                                          
*                                                                               
INIT120  CLC   =C'AGENCY=',0(R3)                                                
         BNE   INIT130                                                          
         LA    R1,RAGYLST                                                       
INIT122  CLI   0(R1),0                                                          
         JE    *+2                 TOO MANY AGENCIES, INCREASE AGYLST           
         CLC   0(L'RAGYLST,R1),SPACES                                           
         BNH   *+12                                                             
         LA    R1,L'RAGYLST(,R1)                                                
         B     INIT122                                                          
         MVC   0(L'RAGYLST,R1),7(R3)                                            
         B     INIT100                                                          
*                                                                               
INIT130  CLC   =C'DOWN=',0(R3)                                                  
         BNE   INIT140                                                          
         MVC   RDOWN,5(R3)                                                      
         B     INIT100                                                          
*                                                                               
INIT140  CLC   =C'TEST=',0(R3)                                                  
         BNE   INIT150                                                          
         MVC   RTEST,5(R3)                                                      
         CLI   RTEST,C'Y'                                                       
         BNE   INIT100                                                          
         L     RF,PSAAOLD-PSA(,0)       GET CURRENT/HOME ASCB                   
         L     RF,(ASCBASXB-ASCB)(,RF)  GET ASXB ADDRESS                        
         L     RF,(ASXBSENV-ASXB)(,RF)  GET ACEE ADDRESS                        
         CLC   =C'ACEE',0(RF)           VALID ACEE?                             
         JNE   *+2                      YES: EXTRACT RACF USERID                
         XR    R1,R1                                                            
         ICM   R1,1,(ACEEUSRL-ACEE)(RF) Length of user id                       
         JZ    *+2                      NO                                      
         CHI   R1,4                     Only support length of 4                
         JNE   *+2                                                              
         MVC   DSNAME(16),=CL16'XXXX.LLOGON.TEST'                               
         MVC   DSNAME(4),(ACEEUSRI-ACEE)(RF)   Length of user id                
         B     INIT100                                                          
*                                                                               
INIT150  CLC   =C'ACTIVE=',0(R3)                                                
         BNE   INIT160                                                          
         CLC   =C'YEAR',7(R3)                                                   
         BE    INIT151                                                          
         CLC   =C'MONTH',7(R3)                                                  
         BE    INIT152                                                          
         CLC   =C'QUARTER',7(R3)                                                
         BE    INIT153                                                          
         CLC   =C'BYDATEMONTH',7(R3)                                            
         BE    INIT154                                                          
         GOTO1 VDATCON,DMCB,(0,7(R3)),(2,RASTART)                               
         GOTO1 VDATCON,DMCB,(0,14(R3)),(2,RAEND)                                
         B     INIT100                                                          
*                                                                               
INIT151  MVI   RACTIV,C'Y'               YEAR REQUEST                           
         MVC   RASTART,TODAY2                                                   
         MVC   RAEND,TODAY2                                                     
         MVC   DUB(3),TODAY3                                                    
         CLC   TODAY3+1(2),=X'0101'                                             
         BH    INIT151A                                                         
         GOTO1 VDATCON,DMCB,(X'30',TODAY0),(2,RAEND),(5,0)                      
         GOTO1 VDATCON,DMCB,(X'30',TODAY0),(3,DUB),(4,0)                        
INIT151A MVI   DUB+1,X'01'                                                      
         GOTO1 VDATCON,DMCB,(3,DUB),(2,RASTART)                                 
         B     INIT100                                                          
*                                                                               
INIT152  MVI   RACTIV,C'M'               MONTH REQUEST                          
         CLI   TODAY3+2,1                IS IT THE FIRST OF THE MONTH?          
         BH    INIT152A                                                         
         GOTO1 VDATCON,DMCB,(X'30',TODAY0),(2,RASTART),(4,0)                    
         GOTO1 VDATCON,DMCB,(X'30',TODAY0),(2,RAEND),(5,0)                      
         B     INIT100                                                          
INIT152A GOTO1 VDATCON,DMCB,(X'30',TODAY0),(2,RASTART),(0,0)                    
         GOTO1 VDATCON,DMCB,(X'30',TODAY0),(2,RAEND),(1,0)                      
         B     INIT100                                                          
*                                                                               
INIT153  MVI   RACTIV,C'Q'               QUARTERLY REQUEST                      
         MVC   RAEND,TODAY2                                                     
         MVC   DUB(3),TODAY3                                                    
         CLI   TODAY3+2,X'01'                                                   
         BNE   INIT153B                                                         
         GOTO1 VDATCON,DMCB,(X'30',TODAY0),(2,RAEND),(5,0)                      
         GOTO1 VDATCON,DMCB,(X'30',TODAY0),(3,DUB),(5,0)                        
INIT153B CLI   DUB+1,X'03'                                                      
         BH    *+12                                                             
         MVI   DUB+1,X'01'                                                      
         B     INIT53C                                                          
         CLI   DUB+1,X'06'                                                      
         BH    *+12                                                             
         MVI   DUB+1,X'04'                                                      
         B     INIT53C                                                          
         CLI   DUB+1,X'09'                                                      
         BH    *+12                                                             
         MVI   DUB+1,X'07'                                                      
         B     INIT53C                                                          
         MVI   DUB+1,X'10'                                                      
INIT53C  MVI   DUB+2,1                                                          
         GOTO1 VDATCON,DMCB,(3,DUB),(2,RASTART)                                 
         B     INIT100                                                          
*                                                                               
INIT154  MVI   RACTIV,C'B'               FULL MONTH, BY DATE                    
         MVC   RAEND,TODAY2                                                     
         GOTO1 VADDAY,DMCB,(C'M',TODAY0),WORK,-1                                
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,1                                 
         GOTO1 VDATCON,DMCB,(0,WORK+6),(2,RASTART)                              
         B     INIT100                                                          
*                                                                               
INIT160  CLC   =C'TAPE=',0(R3)                                                  
         BNE   INIT170                                                          
         MVC   RTAPE,5(R3)                                                      
         B     INIT100                                                          
*                                                                               
INIT170  CLC   =C'LOGON=',0(R3)                                                 
         BNE   INIT180                                                          
         MVC   RLOGON,6(R3)                                                     
         B     INIT100                                                          
*                                                                               
INIT180  CLC   =C'DETAIL=',0(R3)                                                
         BNE   INIT184                                                          
         MVC   RDETAIL,7(R3)                                                    
         B     INIT100                                                          
*                                                                               
INIT184  CLC   =C'STATE=',0(R3)                                                 
         BNE   INIT185                                                          
         MVC   RSTATE,6(R3)                                                     
         B     INIT100                                                          
*                                                                               
INIT185  CLC   =C'USA=',0(R3)                                                   
         BNE   INIT186                                                          
         MVC   RUSA,4(R3)                                                       
         B     INIT100                                                          
*                                                                               
INIT186  CLC   =C'LIMIT=',0(R3)                                                 
         BNE   INIT199                                                          
         MVC   RLIMIT,6(R3)                                                     
         B     INIT100                                                          
*                                                                               
INIT199  MVC   PLINE(80),=CL80'**ERROR** INVALID INPUT CARD'                    
         BRAS  RE,PRINTL                                                        
         B     EXITNE                                                           
*                                                                               
INIT200  GOTO1 VDATAMGR,DMCB,=C'DMOPEN  ',=C'CONTROL ',FLIST,AIO1               
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         CLI   RTAPE,C'Y'              ALL TAPES WANTED                         
         BE    INIT202                                                          
         CLI   RTAPE,C'T'              ONLY TEMP DATA SET WANTED                
         BE    INIT204                                                          
         B     INIT210                 OTHERWISE NOTHING WANTED                 
*                                                                               
INIT202  BRAS  RE,DYNIT                DYNAMICALLY ALLOCATE                     
         OPEN  (TAPEOUT,OUTPUT),TYPE=J DATE/TIME STAMPED OUTPUT TAPE            
INIT204  OPEN  (TEMPOUT,OUTPUT)        TEMPORARY TAPE FOR EMAIL                 
         OI    SINDI,SIOTOP            START OUTPUT TO DATA SET                 
*                                                                               
INIT210  CLI   RDOWN,C'N'                                                       
         BE    INIT220                                                          
*                                                                               
         MVC   PLINE(COLDEFQ),COLDEF                                            
         CLI   RDOWN,C'C'                                                       
         BNE   *+10                                                             
         MVC   PLINE(COLDCSVQ),COLDCSV                                          
*                                                                               
         BRAS  RE,PRINTL                                                        
*                                                                               
         GOTOR DWNL,DWNINIT        INITIALIZE THE DOWNLOAD                      
*                                                                               
INIT220  DS    0H                                                               
*                                                                               
INITEQX  B     EXITEQ                                                           
                                                                                
***********************************************************************         
* FINISHED PROCESSING                                                           
***********************************************************************         
DONE     NTR1                                                                   
*                                                                               
         CLI   RDOWN,C'N'                                                       
         BE    DONE10                                                           
         GOTOR DWNL,DWNEOR                                                      
*                                                                               
DONE10   CLOSE SYSPRINT            CLOSE PRINT                                  
         GOTO1 VDATAMGR,DMCB,=C'DMCLSE ',=C'CONTROL ',FLIST,AIO1                
*                                                                               
         CLI   RTAPE,C'Y'                                                       
         BE    DONE20                                                           
         CLI   RTAPE,C'T'                                                       
         BE    DONE25                                                           
         B     EXITEQ                                                           
*                                                                               
DONE20   CLOSE TAPEOUT                                                          
DONE25   CLOSE TEMPOUT                                                          
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
* GET ACCESS RECORD AND ACCESS DETAIL ELEMENT                                   
***********************************************************************         
GETACCR  NTR1                                                                   
*                                                                               
GAC000   XC    ACDELEM,ACDELEM                                                  
         XC    AGDELEM,AGDELEM                                                  
         NI    SINDI,X'FF'-SIDAFN                                               
*                                                                               
         XC    KEY,KEY             FIND ID REC                                  
         LA    R2,KEY                                                           
         USING CT5REC,R2                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,CUAGY                                                       
         AHI   R1,1                                                             
         STCM  R1,3,CT5KALPH                                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,KEY,AIO1                             
         CLI   8(R1),0                                                          
         BNE   GACNOAC             ACCESS RECORD? AGENCY DOES NOT EXIST         
         L     R2,AIO1                                                          
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   GACNOAC                                                          
         MVC   CUAGY,CT5KALPH                                                   
         MVC   CUSAGY,CUAGY                                                     
*                                                                               
         CLC   RAGYLST,SPACES                                                   
         BE    GAC009                                                           
         LA    R1,RAGYLST                                                       
GAC008   CLI   0(R1),0                                                          
         BE    GAC000                                                           
         CLC   CUAGY,0(R1)                                                      
         BE    GAC009                                                           
         LA    R1,L'RAGYLST(,R1)                                                
         B     GAC008                                                           
*                                                                               
GAC009   LA    R4,CT5DATA                                                       
GAC010   CLI   0(R4),0             END OF RECORD                                
         BE    GAC100                                                           
         CLI   0(R4),CTDSCELQ      PRINCIPAL CONTROL ID       (X'02')           
         BE    GAC020                                                           
         CLI   0(R4),CTAGDELQ      AGENCY GROUP DETAILS       (X'B4')           
         BE    GAC040                                                           
         CLI   0(R4),CTSEAELQ      SECURITY AGENCY ALPHA      (X'B8')           
         BE    GAC045                                                           
         CLI   0(R4),CTAADELQ      AGENCY ACCESS DETAILS      (X'B9')           
         BE    GAC050                                                           
GAC015   LLC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     GAC010                                                           
*                                                                               
         USING CTDSCD,R4                                                        
GAC020   MVC   CUPCID,CTDSC        PRINCIPAL CONTROL ID                         
         B     GAC015                                                           
*                                                                               
         USING CTAGDD,R4                                                        
GAC040   MVC   AGDELEM,CTAGDD      SAVE AGENCY GROUP DETAILS ELEMENT            
         B     GAC015                                                           
*                                                                               
         USING CTSEAD,R4                                                        
GAC045   MVC   CUSAGY,CTSEAAID     SAVE SECURITY AGENCY AGENCY                  
         B     GAC015                                                           
*                                                                               
         USING CTAADD,R4                                                        
GAC050   MVC   ACDELEM,CTAADD      SAVE ACCESS DETAIL ELEMENT                   
         B     GAC015                                                           
*                                                                               
GAC100   XC    KEY,KEY             FIND PRINCIPAL ID RECORD                     
         LA    R2,KEY                                                           
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CUPCID                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,KEY,AIO1                             
         CLI   8(R1),0                                                          
         BNE   GACNOUS                                                          
         L     R2,AIO1                                                          
*                                                                               
         LA    R4,CTIDATA                                                       
GAC110   CLI   0(R4),0             END OF RECORD                                
         BE    GACEX                                                            
         CLI   0(R4),CTDSCELQ      COMPANY ID CODE (X'02)                       
         BE    GAC120                                                           
         CLI   0(R4),CTORGELQ      ORIGIN DETAIL ELEMENT (X'36')                
         BE    GAC130                                                           
GAC115   LLC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     GAC110                                                           
*                                                                               
         USING CTDSCD,R4                                                        
GAC120   MVC   CUPCOD,CTDSC        PRINCIPAL COMPANY ID CODE                    
         B     GAC115                                                           
*                                                                               
         USING CTORGD,R4                                                        
GAC130   MVC   CUPCNM,CTORGNAM     PRINCIPAL COMPANY ID NAME                    
         B     GAC115                                                           
*                                                                               
GACEX    CLI   RLIMIT,C'N'                                                      
         BE    GACEX5                                                           
*                                                                               
         CLC   CUAGY,CUSAGY       IS THIS A SECURITY ALPHA ID                   
         BNE   GAC000             NO: SKIP IT                                   
*                                                                               
         LA    R4,ACDELEM         ACCESS DETAIL ELEMENT                         
         USING CTAADD,R4                                                        
         CLI   CTAACSTN,9         INTERNAL AGENCY?                              
         BE    GAC000             YES: SKIP IT                                  
**NOP    CLI   CTAACSTN,10        INACTIVE AGENCY?                              
*        BE    GAC000             YES: SKIP IT                                  
         LA    R4,AGDELEM         AGENCY GROUP ELEMENT                          
         USING CTAGDD,R4                                                        
         TM    CTAGOPTS,CTAGTST+CTAGTNG+CTAGUAT  TEST/TRAINING/UAT?             
         BNZ   GAC000                             YES: SKIP IT                  
*                                                                               
GACEX5   CLI   RUSA,C'N'          USA ONLY                                      
         BE    GACEX10                                                          
         LA    R4,AGDELEM                                                       
         USING CTAGDD,R4                                                        
         CLI   CTAGDLEN,CTAGDL2Q                                                
         BNE   GACEX10                                                          
         CLI   CTAGDCTY,CTRYDFL                                                 
         BE    GACEX10                                                          
         CLI   CTAGDCTY,CTRYUSA                                                 
         BE    GACEX10                                                          
         B     GAC000                                                           
         DROP  R4                                                               
*                                                                               
GACEX10  CLI   RDOWN,C'N'                                                       
         BNE   EXITEQ                                                           
         CLI   RDETAIL,C'O'                                                     
         BE    EXITEQ                                                           
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   PLINED(PLINEQ),DASHES                                            
         BRAS  RE,PRINTL                                                        
         MVC   PLINED(PLINEQ),SPACES                                            
         MVC   PLAGY,CUAGY                                                      
         MVC   PLAGY+L'PLAGY+1(L'CUPCOD),CUPCOD                                 
         MVC   PLAGY+L'PLAGY+L'CUPCOD+1(L'CUPCNM),CUPCNM                        
         BRAS  RE,PRINTL                                                        
         MVC   PLINED(PLINEQ),DASHES                                            
         BRAS  RE,PRINTL                                                        
         B     EXITEQ                                                           
*                                                                               
GACNOUS  CLI   RDOWN,C'N'                                                       
         BNE   GAC000                                                           
         BRAS  RE,PRINTL                                                        
         MVC   PLMESS,MNOCID       PRINCIPAL COMPANY ID NOT FOUND               
         BRAS  RE,PRINTL                                                        
         B     GAC000                                                           
*                                                                               
GACNOAC  CLI   RDOWN,C'N'                                                       
         BNE   EXITNE                                                           
         BRAS  RE,PRINTL                                                        
         MVC   PLMESS,MNOACC       NO ACCESS RECORDS FOUND                      
         BRAS  RE,PRINTL                                                        
GACNEX   B     EXITNE                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
* GET PERSON RECORD                                                             
***********************************************************************         
GETPER   NTR1                                                                   
*                                                                               
         XC    CUPID,CUPID         CLEAR PID                                    
         ZAP   TOTCNT,PKZERO       PERSON RECORDS                               
         ZAP   UNICNT,PKZERO       UNIQUE PERSONS                               
         ZAP   TERCNT,PKZERO       TERMINATED PERSONS                           
         ZAP   EXPCNT,PKZERO       ACTIVE PERSONS WITH EXPIRED PASSWORD         
         ZAP   ACTCNT,PKZERO                                                    
         ZAP   ADRCNT,PKZERO                                                    
         ZAP   RNGCNT,PKZERO                                                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING SAPEREC,R2                                                       
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,CUAGY                                                    
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,KEY,AIO2                             
         B     GP011                                                            
GP010    GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,KEY,AIO2                             
GP011    CLI   8(R1),0                                                          
         BNE   GP200                                                            
         L     R2,AIO2                                                          
         CLI   SAPETYP,SAPETYPQ    PERSON RECORD (X'C6')                        
         BNE   GP200                                                            
         CLI   SAPESUB,SAPESUBQ                  (X'04')                        
         BNE   GP200                                                            
         CLC   SAPEAGY,CUAGY                                                    
         BNE   GP200                                                            
*                                                                               
         AP    TOTCNT,PKONE        TOTAL PID COUNT                              
         CLC   CUPID,SAPEPID                                                    
         BE    GP010                                                            
         AP    UNICNT,PKONE        UNIQUE PID COUNT                             
*                                                                               
         MVC   CUPID,SAPEPID       PERSON ID                                    
         MVC   CUDEF,SAPEDEF       EFFECTIVE DATE (ONES COMPLEMENT)             
         XC    CUDEF,=X'FFFF'      EFFECTIVE DATE                               
*                                                                               
         MVC   CUFNM,SPACES        FIRST NAME                                   
         MVC   CUMNM,SPACES        MIDDLE NAME                                  
         MVC   CULNM,SPACES        LAST NAME                                    
         MVC   CUCITY,SPACES       CITY                                         
         MVC   CUSTATE,SPACES      STATE                                        
         MVC   CUZIP,SPACES        ZIP                                          
         MVC   CUCTRY,SPACES       COUNTRY                                      
         MVC   CUEMAIL,SPACES      EMAIL                                        
*                                                                               
         XC    CULLO,CULLO         LAST LOGON DATE                              
         XC    CUHIRE,CUHIRE       HIRE DATE                                    
         XC    CUTERM,CUTERM       TERMINATION DATE                             
*                                                                               
         LA    R4,SAPEDATA                                                      
GP020    CLI   0(R4),0             END OF RECORD                                
         BE    GP150                                                            
         CLI   0(R4),SALLOELQ      LAST LOG ON ELEMENT      (X'04')             
         BE    GP030                                                            
         CLI   0(R4),SANAMELQ      PERSON NAME ELEMENT      (X'C5')             
         BE    GP040                                                            
         CLI   0(R4),SAPERELQ      PERSONNEL DETAIL ELEMENT (X'C6')             
         BE    GP050                                                            
         CLI   0(R4),SAADRELQ      PERSON ADDRESS ELEMENT   (X'C7')             
         BE    GP060                                                            
         CLI   0(R4),SAPEEELQ      PERSON EMAIL ADDRESS     (X'E5')             
         BE    GP070                                                            
GP025    LLC   R0,1(R4)            NEXT                                         
         AR    R4,R0                                                            
         B     GP020                                                            
*----------------------------------                                             
* EMPLOYEE LAST LOG ON ELEMENT                                                  
*----------------------------------                                             
         USING SALLOD,R4                                                        
GP030    OC    SALLODT,SALLODT                                                  
         BZ    GP025                                                            
         GOTO1 VDATCON,DMCB,(3,SALLODT),(2,CULLO)                               
         B     GP025                                                            
*----------------------------------                                             
* EMPLOYEE NAME ELEMENT                                                         
*----------------------------------                                             
         USING SANAMD,R4                                                        
GP040    LA    RF,SANAMES                                                       
         TM    SANAMIND,SANAMIFN   FIRST NAME PRESENT  (X'80')                  
         BZ    GP042                                                            
         LLC   R1,0(RF)                                                         
         AHI   R1,-1                                                            
         BM    GP025                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CUFNM(0),1(RF)                                                   
         LA    RF,2(R1,RF)                                                      
*                                                                               
GP042    TM    SANAMIND,SANAMIMN   MIDDLE NAME PRESENT (X'40')                  
         BZ    GP044                                                            
         LLC   R1,0(RF)                                                         
         AHI   R1,-1                                                            
         BM    GP025                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CUMNM(0),1(RF)                                                   
         LA    RF,2(R1,RF)                                                      
*                                                                               
GP044    TM    SANAMIND,SANAMILN   LAST NAME PRESENT   (X'20')                  
         BZ    GP025                                                            
         LLC   R1,0(RF)                                                         
         AHI   R1,-1                                                            
         BM    GP025                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CULNM(0),1(RF)                                                   
         LA    RF,2(R1,RF)                                                      
         B     GP025                                                            
*----------------------------------                                             
* EMPLOYEE DETAIL ELEMENT                                                       
*----------------------------------                                             
         USING SAPERD,R4                                                        
GP050    MVC   CUHIRE,SAPERDHI                                                  
         MVC   CUTERM,SAPERDTE                                                  
         OC    SAPERDTE,SAPERDTE   TERMINATED?                                  
         BZ    GP025                                                            
         AP    TERCNT,PKONE                                                     
         B     GP025                                                            
*----------------------------------                                             
* EMPLOYEE LOCATION ELEMENT                                                     
*----------------------------------                                             
         USING SAADRD,R4                                                        
GP060    CLI   SAADRTYP,SAADCITQ   CITY        (X'04')                          
         BE    GP062                                                            
         CLI   SAADRTYP,SAADCODQ   STATE / ZIP (X'05')                          
         BE    GP063                                                            
         CLI   SAADRTYP,SAADCTRQ   COUNTRY     (X'06')                          
         BE    GP066                                                            
         CLI   SAADRTYP,SAADSTEQ   STATE       (X'07')                          
         BE    GP067                                                            
         CLI   SAADRTYP,SAADZIPQ   ZIP         (X'08')                          
         BE    GP068                                                            
         B     GP025                                                            
*                                                                               
GP062    LLC   R1,SAADRDLN         CITY                                         
         AHI   R1,-1                                                            
         BM    GP025                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CUCITY(0),SAADRDAT                                               
         B     GP025                                                            
*                                                                               
GP063    MVC   CUSTATE,SAADRDAT    STATE                                        
*                                                                               
         LA    R1,SAADRDAT+2                                                    
         LLC   RF,SAADRDLN         ZIP                                          
         AHI   RF,-2                                                            
         BM    GP025                                                            
GP064    CLI   0(R1),C'0'                                                       
         BL    *+12                                                             
         CLI   0(R1),C'9'                                                       
         BNH   GP065                                                            
         AHI   R1,1                                                             
         CHI   RF,5                                                             
         BNH   GP025                                                            
         AHI   RF,-1                                                            
         B     GP064                                                            
GP065    MVC   CUZIP(5),0(R1)                                                   
         B     GP025                                                            
*                                                                               
GP066    MVC   CUCTRY,SAADRDAT     COUNTRY                                      
         B     GP025                                                            
*                                                                               
GP067    MVC   CUSTATE,SAADRDAT    STATE                                        
         B     GP025                                                            
*                                                                               
GP068    LLC   R1,SAADRDLN         ZIP                                          
         AHI   R1,-1                                                            
         BM    GP025                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CUZIP(0),SAADRDAT                                                
         B     GP025                                                            
*----------------------------------                                             
* EMPLOYEE EMAIL ADDRESS ELEMENT                                                
*----------------------------------                                             
         USING SAPEED,R4                                                        
GP070    LLC   R1,SAPEELN          EMAIL ADDRESS                                
         AHI   R1,-SAPEELNQ-1                                                   
         BM    GP025                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CUEMAIL(0),SAPEEID                                               
         B     GP025                                                            
*----------------------------------                                             
* FILTER AND PRINT RECORD                                                       
*----------------------------------                                             
GP150    CLI   RSTATE,C'*'                                                      
         BE    GP160                                                            
         CLC   RSTATE,CUSTATE                                                   
         BNE   GP010                                                            
*                                                                               
GP160    CLI   RDOWN,C'N'          DOWN-LOAD FORMAT?                            
         BE    *+12                . NO                                         
         BRAS  RE,PDOWN            . YES                                        
         B     GP010                                                            
         BRAS  RE,PINFO            STANDARD REPORT                              
         B     GP010                                                            
*                                                                               
GP200    CLI   RDOWN,C'N'                                                       
         BNE   GPOKX                                                            
         CLI   RDETAIL,C'O'                                                     
         BE    GPOKX                                                            
*                                                                               
         CP    TOTCNT,PKZERO                                                    
         BNE   GP210                                                            
         MVC   PLAGY,CUAGY                                                      
         MVC   PLCOUNT,DASHES                                                   
         MVC   PLMESS(40),=CL40'NO PERSONS FOUND FOR AGENCY ALPHA'              
         BRAS  RE,PRINTL                                                        
         B     GPOKX                                                            
*                                                                               
GP210    MVC   PLAGY,CUAGY                                                      
         MVC   PLMESS(40),=CL40'TOTAL PERSON RECORDS'                           
         CP    TOTCNT,PKZERO                                                    
         BE    *+8                                                              
         MVI   PLMESS-2,C'*'                                                    
         EDIT  TOTCNT,PLCOUNT,COMMAS=YES,ZERO=NOBLANK                           
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   PLAGY,CUAGY                                                      
         MVC   PLMESS(40),=CL40'UNIQUE PERSONS'                                 
         CP    UNICNT,PKZERO                                                    
         BE    *+8                                                              
         MVI   PLMESS-2,C'*'                                                    
         EDIT  UNICNT,PLCOUNT,COMMAS=YES,ZERO=NOBLANK                           
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   PLAGY,CUAGY                                                      
         MVC   PLMESS(40),=CL40'PERSONS TERMINATED'                             
         CP    TERCNT,PKZERO                                                    
         BE    *+8                                                              
         MVI   PLMESS-2,C'*'                                                    
         EDIT  TERCNT,PLCOUNT,COMMAS=YES,ZERO=NOBLANK                           
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   PLAGY,CUAGY                                                      
         MVC   PLMESS(40),=CL40'ACTIVE PERSONS WITHIN RANGE'                    
         OC    RASTART,RASTART                                                  
         BNZ   *+14                                                             
         CLC   RAEND,=X'FFFF'                                                   
         BE    GP230                                                            
         GOTO1 VDATCON,DMCB,(2,RASTART),(5,PLMESS+28)                           
         MVI   PLMESS+36,C'-'                                                   
         GOTO1 VDATCON,DMCB,(2,RAEND),(5,PLMESS+37)                             
GP230    CLI   RLOGON,C'Y'                                                      
         BNE   *+10                                                             
         MVC   PLMESS+15(12),=C'LOGGED ON   '                                   
         CP    RNGCNT,PKZERO                                                    
         BE    *+8                                                              
         MVI   PLMESS-2,C'*'                                                    
         EDIT  RNGCNT,PLCOUNT,COMMAS=YES,ZERO=NOBLANK                           
         BRAS  RE,PRINTL                                                        
*                                                                               
         AP    GTOTCNT,TOTCNT                                                   
         AP    GUNICNT,UNICNT                                                   
         AP    GTERCNT,TERCNT                                                   
         AP    GEXPCNT,EXPCNT                                                   
*                                                                               
GPOKX    B     EXITEQ                                                           
         DROP  R2,R4                                                            
                                                                                
***********************************************************************         
* PRINT PERSON INFORMATION IN DOWN-LOAD FORM                                    
***********************************************************************         
PDOWN    NTR1                                                                   
*                                                                               
         MVC   PLINED(PLINEQ),SPACES                                            
*                                                                               
         OC    CUHIRE,CUHIRE                                                    
         BZ    *+14                                                             
         CLC   CUHIRE,RAEND                                                     
         BH    PDEQX                                                            
*                                                                               
         OC    CUTERM,CUTERM                                                    
         BZ    *+14                                                             
         CLC   CUTERM,RASTART                                                   
         BL    PDEQX                                                            
*                                                                               
         CLI   RLOGON,C'N'                                                      
         BE    *+14                                                             
         CLC   CULLO,RASTART                                                    
         BL    PDEQX                                                            
*                                                                               
         MVC   DWNFLD(L'CUAGY),CUAGY                                            
         GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         MVC   DWNFLD(L'CUPID),CUPID                                            
         GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         MVC   DWNFLD(L'CUFNM),CUFNM                                            
         GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         MVC   DWNFLD(L'CUMNM),CUMNM                                            
         GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         MVC   DWNFLD(L'CULNM),CULNM                                            
         GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         MVC   DWNFLD(L'CUCITY),CUCITY                                          
         GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         MVC   DWNFLD(L'CUSTATE),CUSTATE                                        
         GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         MVC   DWNFLD(L'CUZIP),CUZIP                                            
         GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         MVC   DWNFLD(L'CUCTRY),CUCTRY                                          
         GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         OC    CUHIRE,CUHIRE       HIRE DATE                                    
         BZ    PD005                                                            
         CLI   RDOWN,C'C'          CSV FORMAT?                                  
         BNE   PD004               NO                                           
         GOTO1 VDATCON,DMCB,(2,CUHIRE),(23,DWNFLD)                              
         B     PD005                                                            
PD004    GOTO1 VDATCON,DMCB,(2,CUHIRE),(21,DWNFLD)                              
PD005    GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         OC    CULLO,CULLO         LAST LOGON DATE                              
         BZ    PD010                                                            
         CLI   RDOWN,C'C'          CSV FORMAT?                                  
         BNE   PD009               NO                                           
         GOTO1 VDATCON,DMCB,(2,CULLO),(23,DWNFLD)                               
         B     PD010                                                            
PD009    GOTO1 VDATCON,DMCB,(2,CULLO),(21,DWNFLD)                               
PD010    GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         OC    CUTERM,CUTERM       TERMINATION DATE                             
         BZ    PD015                                                            
         CLI   RDOWN,C'C'          CSV FORMAT?                                  
         BNE   PD014               NO                                           
         GOTO1 VDATCON,DMCB,(2,CUTERM),(23,DWNFLD)                              
         B     PD015                                                            
PD014    GOTO1 VDATCON,DMCB,(2,CUTERM),(21,DWNFLD)                              
PD015    GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         MVC   DWNFLD(L'CUEMAIL),CUEMAIL                                        
         GOTOR DWNL,DWNTEXT                                                     
*                                                                               
         GOTOR DWNL,DWNEOL                                                      
*                                                                               
PDEQX    B     EXITEQ                                                           
                                                                                
***********************************************************************         
* PRINT PERSON INFORMATION                                                      
***********************************************************************         
PINFO    NTR1                                                                   
*                                                                               
         MVC   PLINED(PLINEQ),SPACES                                            
         CLI   RDETAIL,C'O'                                                     
         BE    PI010                                                            
*                                                                               
         TM    SINDI,SIDAFN                                                     
         BO    PI010                                                            
         OI    SINDI,SIDAFN                                                     
         MVC   PLINE,H1                                                         
         BRAS  RE,PRINTL                                                        
         MVC   PLINE(PLINEQ),DASHES                                             
         BRAS  RE,PRINTL                                                        
         MVC   PLINED(PLINEQ),SPACES                                            
*                                                                               
PI010    MVC   PLSAGY,CUAGY                                                     
         MVC   PLPID,CUPID                                                      
*                                                                               
         MVC   PLLNAME,CULNM                                                    
         MVC   PLFNAME,CUFNM                                                    
         MVC   PLMNAME,CUMNM                                                    
*                                                                               
         MVC   PLEMAIL,CUEMAIL                                                  
*                                                                               
         MVC   PLCSZ,SPACES                                                     
         LA    RE,PLCSZ                                                         
         MVC   0(L'CUCITY,RE),CUCITY                                            
         LA    RF,PLCSZ+L'PLCSZ-1                                               
PI011    CLI   0(RF),C' '                                                       
         BH    PI012                                                            
         AHI   RF,-1                                                            
         CR    RE,RF                                                            
         BE    PI020                                                            
         B     PI011                                                            
PI012    LA    RF,1(,RF)                                                        
         MVI   0(RF),C','                                                       
         MVC   2(L'CUSTATE,RF),CUSTATE                                          
         LA    RF,6(,RF)                                                        
         MVC   0(L'CUZIP,RF),CUZIP                                              
*                                                                               
PI020    MVC   PLHIRE,SPACES                                                    
         OC    CUHIRE,CUHIRE                                                    
         BZ    PI060                                                            
         GOTO1 VDATCON,DMCB,(2,CUHIRE),(5,PLHIRE)                               
         CLC   CUHIRE,RAEND                                                     
         BH    PIEQX                                                            
*                                                                               
PI060    MVC   PLLLO,SPACES                                                     
         OC    CULLO,CULLO                                                      
         BZ    PI070                                                            
         GOTO1 VDATCON,DMCB,(2,CULLO),(5,PLLLO)                                 
*                                                                               
PI070    MVC   PLTERM,SPACES                                                    
         OC    CUTERM,CUTERM                                                    
         BZ    PI080                                                            
         GOTO1 VDATCON,DMCB,(2,CUTERM),(5,PLTERM)                               
*                                                                               
PI080    DS    0H                                                               
         OC    CUHIRE,CUHIRE                                                    
         BZ    PI090                                                            
         CLC   CUHIRE,RAEND                                                     
         BH    PIEQX                                                            
*                                                                               
PI090    OC    CUTERM,CUTERM                                                    
         BZ    PI095                                                            
         CLC   CUTERM,RASTART                                                   
         BL    PIEQX                                                            
*                                                                               
PI095    CLI   RLOGON,C'N'                                                      
         BE    PI098                                                            
         CLC   CULLO,RASTART                                                    
         BL    PIEQX                                                            
*                                                                               
PI098    AP    RNGCNT,=PL1'1'                                                   
*                                                                               
PI100    CLI   RDETAIL,C'N'                                                     
         BE    *+8                                                              
         BRAS  RE,PRINTL                                                        
*                                                                               
PIEQX    MVC   PLINE(PLINEQ),SPACES                                             
         B     EXITEQ                                                           
                                                                                
*********************************************************************           
* DOWNLOAD MODULE                                                               
*********************************************************************           
DWNL     NTR1  BASE=*,LABEL=*                                                   
         STC   R1,DWNMODE                                                       
*                                                                               
         LA    R5,DWNBLOCK                                                      
         USING DLCBD,R5                                                         
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWNL20                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWNL50                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWNL50                                                           
         DC    H'0'                                                             
*----------------------------------                                             
* INITIALIZATION                                                                
*----------------------------------                                             
DWNL10   TM    SINDI,SIDWIN        HAS IT ALREADY BEEN INITIALIZED?             
         BO    DWNLX               YES - EXIT                                   
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,PLINE            PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'PLINE)                                             
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
*                                                                               
         CLI   RDOWN,C'C'          CSV FORMAT?                                  
         BNE   DWNL12              NO                                           
         MVI   DLCXDELC,C','       DELIMITER                                    
         MVI   DLCXEOTC,0          TEXT DELIMITER                               
         MVI   DLCXEOTA,0          ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,0          SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,0          END-OF-REPORT                                
*                                                                               
DWNL12   GOTO1 VDLFLD,(R5)                                                      
         BRAS  RE,PRINTL                                                        
         OI    SINDI,SIDWIN        TURN ON INITIALIZED BYTE                     
         B     DWNLX               EXIT                                         
*----------------------------------                                             
* DOWNLOAD A RECORD - TEXT                                                      
*----------------------------------                                             
DWNL20   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE IS TEXT                                 
         OI    DLCBFLG1,DLCBFXFL   USE EXTENDED FIELD (160 CHARACTERS)          
         MVC   DLCBFLX(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*----------------------------------                                             
* END OF LINE/END OF RECORD                                                     
*----------------------------------                                             
DWNL50   GOTO1 VDLFLD,(R5)         DOWN-LOAD FIELD                              
*                                                                               
DWNLX    MVC   DLCBFLD,SPACES      CLEAR DOWN-LOAD FIELDS                       
         MVC   DLCBFLX,SPACES                                                   
         MVC   DWNFLD,SPACES                                                    
         B     EXITEQ                                                           
         DROP  R5                                                               
*----------------------------------                                             
* DOWNLOAD HOOK                                                                 
*----------------------------------                                             
DWNHOOK  ST    RE,HOOKRE                                                        
         BRAS  RE,PRINTL                                                        
         L     RE,HOOKRE                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Dynamically Allocate Report Dump Tape                                         
***********************************************************************         
DYNIT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   DSNDSPC,SSB+SSODSPAC-SSOOFF          DATA SPACE                  
*                                                                               
         MVC   DSNDACT,RACTIV      ACTIVE TIME PERIOD                           
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(X'20',DSNDATE)   DATE                        
*                                                                               
         EDIT  (TIME,NOW),DUBDUB   TIME                                         
         MVC   DSNHOUR,DUBDUB                                                   
         MVC   DSNMINS,DUBDUB+3                                                 
         MVC   DSNSECS,DUBDUB+6                                                 
*                                                                               
         LA    R1,ARBLK            DYNAMIC ALLOCATION PARAMETER BLOCK           
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BNZ   DIT010              ERROR CREATING DATA SET                      
*                                                                               
         RDJFCB TAPEOUT                                                         
         LTR   RF,RF                                                            
         BNZ   DIT010                                                           
*                                                                               
         LA    R3,JFCB                                                          
         USING INFMJFCB,R3                                                      
         MVC   JFCBXPDT,JFCBCRDT   COPY CREATION DATE TO EXPIRATION             
*                                                                               
         CLI   SSB+SSODSPAC-SSOOFF,C'T'                                         
         BE    DIT005                                                           
         LLC   R1,JFCBXPDT                                                      
         AHI   R1,1                                                             
         STC   R1,JFCBXPDT         EXPIRES 1 YEAR AFTER CREATION                
         B     EXIT                                                             
*                                                                               
DIT005   XR    R1,R1               FOR TEST IT'S 5 DAYS                         
         ICM   R1,B'0011',JFCBXPDT+1                                            
         AHI   R1,5                                                             
         CHI   R1,365                                                           
         BH    EXIT                END OF THE YEAR EVERYTHINGS GOES             
         STCM  R1,B'0011',JFCBXPDT+1                                            
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
DIT010   MVC   CARD,SPACES                                                      
         MVC   CARD(2),=AL2(78)                                                 
         MVC   CARD+2(13),=C'<<< ERROR >>>'                                     
         MVC   CARD+20(22),=C'DYNALLOC ERROR CODE = '                           
         GOTO1 VHEXOUT,DMCB,RBLK+4,CARD+42,2,=C'TOG'                            
         MVC   CARD+46(15),=C',  INFO CODE = '                                  
         GOTO1 VHEXOUT,DMCB,RBLK+6,CARD+61,2,=C'TOG'                            
*                                                                               
         LA    R3,CARD                                                          
         WTO   TEXT=(R3)                                                        
         DC    H'0'                COULD NOT ALLOCATE TAPE                      
                                                                                
***********************************************************************         
* COMMON VALUES (COVERED BY RA)                                                 
***********************************************************************         
COMMON   DS    0D                                                               
                                                                                
***********************************************************************         
* PRINT ROUTINES                                                                
***********************************************************************         
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,PKZERO         RESET LINECOUNT                              
         AP    PAGE,PKONE          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,PKONE          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,PKTHREE        RESET LINECOUNT                              
         AP    PAGE,PKONE          BUMP PAGECOUNT                               
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
*                                                                               
         TM    SINDI,SIOTOP        DID WE OPEN AN OUTPUT DATA SET?              
         BZ    PRINTL9             NO:                                          
         XC    PLINEL(4),PLINEL                                                 
         MVI   PLINEL+1,PLINEQ                                                  
*                                                                               
         CLC   PLINE,SPACES                                                     
         BNH   PRINTL9                                                          
         LA    RE,PLINE+L'PLINE-1                                               
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         AHI   RE,-1                                                            
         B     *-12                                                             
         LA    RF,PLINE                                                         
         SR    RE,RF                                                            
         STC   RE,PLINEL+1                                                      
*                                                                               
         CLI   RTAPE,C'Y'              ALL TAPES WANTED                         
         BE    PRINTL3                                                          
         CLI   RTAPE,C'T'              ONLY TEMP DATA SET WANTED                
         BE    PRINTL4                                                          
         B     PRINTL9                 OTHERWISE NOTHING WANTED                 
*                                                                               
PRINTL3  PUT   TAPEOUT,PLINEL                                                   
PRINTL4  PUT   TEMPOUT,PLINE                                                    
PRINTL9  MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITNE   MVI   EXITCC,2                                                         
         J     *+8                                                              
EXITEQ   MVI   EXITCC,1                                                         
         CLI   EXITCC,1                                                         
EXIT     XIT1                                                                   
                                                                                
***********************************************************************         
* DYNALLOC REQUEST BLOCK                                                        
***********************************************************************         
         DS    0F                                                               
ARBLK    DC    X'80',AL3(RBLK)     R1 POINTS TO THIS BEFORE DYNALLOC            
RBLK     DC    X'1401000000000000',A(ATXT),X'0000000000000000'                  
*                                                                               
ATXT     DC    X'00',AL3(TXTDD)                                                 
         DC    X'00',AL3(TXTDSN)                                                
         DC    X'00',AL3(TXTUNIT)                                               
         DC    X'00',AL3(TXTSDISP)                                              
         DC    X'00',AL3(TXTNDISP)                                              
         DC    X'00',AL3(TXTCDISP)                                              
         DC    X'00',AL3(TXTCYL)                                                
         DC    X'00',AL3(TXTPRIME)                                              
         DC    X'00',AL3(TXTSECND)                                              
         DC    X'80',AL3(TXTCLOSE)                                              
*                                                                               
TXTDD    DC    AL2(DALDDNAM),X'00010007',CL7'TAPEOUT'  DDNAME=TAPEOUT           
TXTDSN   DC    AL2(DALDSNAM),X'0001002C',CL44' '       DSN=............         
TXTUNIT  DC    AL2(DALUNIT),X'00010005',CL5'SYSDA'     UNIT=SYSDA               
TXTSDISP DC    AL2(DALSTATS),X'00010001',X'04'         DISP=(NEW,     )         
TXTNDISP DC    AL2(DALNDISP),X'00010001',X'02'              ( ,CATLG, )         
TXTCDISP DC    AL2(DALCDISP),X'00010001',X'02'              (   ,CATLG)         
TXTCYL   DC    AL2(DALCYL),X'0000'                     CYL=                     
TXTPRIME DC    AL2(DALPRIME),X'00010003',X'000001'         (1,                  
TXTSECND DC    AL2(DALSECND),X'00010003',X'00000A'            10)               
TXTCLOSE DC    AL2(DALCLOSE),X'0000'                   FREE=CLOSE               
*                                                                               
         ORG   TXTDSN+6            BACK UP AND DEFINE DATA SET NAME             
DSNAME   DS    0CL44                                                            
         DC    C'CONTAPE.'         CONTAPE.                                     
         DC    C'CNLLOGON.'        CNLLOGON.                                    
DSNDSPC  DC    C'X'                DATA SPACE                                   
         DC    C'.'                .                                            
DSNDACT  DC    C'D'                D=DATE,M=MONTH,Q=QUARTER,Y=YEAR              
DSNDATE  DC    C'YYMMDD'           YEAR/MONTH/DAY                               
         DC    C'.T'               .T = TIME                                    
DSNHOUR  DC    C'HH'               HOURS                                        
DSNMINS  DC    C'MM'               MINUTES                                      
DSNSECS  DC    C'SS'               SECONDS                                      
         ORG                                                                    
                                                                                
***********************************************************************         
* CONSTANTS, LITERALS, AND PARAMETERS                                           
***********************************************************************         
VDATAMGR DC    V(DATAMGR)                                                       
VCARDS   DC    V(CARDS)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VHELLO   DC    V(HELLO)                                                         
VDATCON  DC    V(DATCON)                                                        
VADDAY   DC    V(ADDAY)                                                         
VDLFLD   DC    V(DLFLD)                                                         
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMPRINT  DC    CL8'DMPRINT'                                                     
BUFFER   DC    CL8'BUFFER'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
SPACES   DC    166C' '                                                          
DASHES   DC    166C'-'                                                          
PKZERO   DC    PL1'0'                                                           
PKONE    DC    PL1'1'                                                           
PKTWO    DC    PL1'2'                                                           
PKTHREE  DC    PL1'3'                                                           
MAXLINE  DC    P'60'                                                            
*                                                                               
RCWRITE  DC    C'N'                                                             
RDOWN    DC    C'N'                                                             
RTEST    DC    C'N'                                                             
RLOGON   DC    C'N'                                                             
RTAPE    DC    C'N'                                                             
RDETAIL  DC    C'Y'                                                             
RLIMIT   DC    C'N'                                                             
RUSA     DC    C'N'                                                             
RSTATE   DC    C'**'                                                            
RAGYLST  DC    64CL2'  '                                                        
         DC    X'00'                                                            
RASTART  DC    XL2'0000'                                                        
RAEND    DC    XL2'FFFF'                                                        
RACTIV   DC    C'D'                                                             
RDSN     DC    CL44' '                                                          
*                                                                               
DDSUSER  DC    AL2(17)                   SJR                                    
PRTQID   DC    CL7'PRTQU'                                                       
*                                                                               
         DC    0D                                                               
         DC    C'**FLST**'                                                      
FLIST    DS    0CL8                                                             
FCTFILE  DC    C'NCTFILE '                                                      
         DC    C'X       '                                                      
*                                                                               
COLDEF   DC    C'<SECURITY AGENCY> '                                            
         DC    C'<PERSON ID> '                                                  
         DC    C'<FIRST NAME> '                                                 
         DC    C'<MIDDLE NAME> '                                                
         DC    C'<LAST NAME> '                                                  
         DC    C'<CITY> '                                                       
         DC    C'<STATE> '                                                      
         DC    C'<ZIP> '                                                        
         DC    C'<COUNTRY> '                                                    
         DC    C'<HIRE DATE> '                                                  
         DC    C'<LAST LOG ON> '                                                
         DC    C'<TERM DATE> '                                                  
         DC    C'<EMAIL>'                                                       
COLDEFQ  EQU   *-COLDEF                                                         
*                                                                               
COLDCSV  DC    C'<SECURITY AGENCY>,'                                            
         DC    C'<PERSON ID>,'                                                  
         DC    C'<FIRST NAME>,'                                                 
         DC    C'<MIDDLE NAME>,'                                                
         DC    C'<LAST NAME>,'                                                  
         DC    C'<CITY>,'                                                       
         DC    C'<STATE>,'                                                      
         DC    C'<ZIP>,'                                                        
         DC    C'<COUNTRY>,'                                                    
         DC    C'<HIRE DATE>,'                                                  
         DC    C'<LAST LOG ON>,'                                                
         DC    C'<TERM DATE>,'                                                  
         DC    C'<EMAIL>'                                                       
COLDCSVQ EQU   *-COLDCSV                                                        
*                                                                               
MERROR   DC    CL10'**ERROR***'                                                 
MNOACC   DC    CL40'NO MORE ACCESS RECORDS FOUND           '                    
MNOCID   DC    CL40'PRINCIPAL COMPANY ID NOT FOUND         '                    
*                                                                               
H1       DC    C'  AID PID       LAST NAME           FIRST NAME  MI'            
         DC    C'  HIRED      LOGON      TERM''D   ADDRESS         '            
         DC    C'                      EMAIL                       '            
         DC    C'                                                  '            
*                                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
*        DCBS & ADCONS                                                          
***********************************************************************         
JFCB     DS    44F                                                              
JFCBPTR  DC    X'87'                                                            
         DC    AL3(JFCB)                                                        
*                                                                               
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
TEMPOUT  DCB   DDNAME=TEMPOUT,DSORG=PS,MACRF=PM,RECFM=FB,              +        
               LRECL=160,BLKSIZE=8000                                           
*                                                                               
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=PM,RECFM=VB,              +        
               LRECL=4004,BLKSIZE=8200,EXLST=JFCBPTR                            
*                                                                               
         DC    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    F'0',X'0A',XL3'00',XL252'00'                                     
*                                                                               
         DC    0D                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
SSB      DC    X'0000'                                                          
         DC    X'FF'                                                            
         DC    AL1(SSOSNRCV)                                                    
         DC    1024X'00'                                                        
*                                                                               
       ++INCLUDE FATABSDEQU                                                     
*                                                                               
         DS    2D                                                               
         DC    C'*SOBSOB*'                                                      
CXREC    DS    0C                                                               
BUFF     DC    14336X'00'                                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
WORKD    DSECT                                                                  
*                                                                               
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
HOOKRE   DS    A                                                                
*                                                                               
AIO1     DS    A                   A(IO AREA 1)                                 
AIO2     DS    A                   A(IO AREA 2)                                 
ACIREC   DS    A                   A(PRTQ CI)                                   
ABUFF    DS    A                   A(PQ BUFFER)                                 
*                                                                               
DUB      DS    D                                                                
DUBDUB   DS    2D                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
EXITCC   DS    X                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
WORK     DS    CL64                                                             
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
*                                                                               
TOTCNT   DS    PL14                TOTAL NUMBER OF PERSON RECORDS               
UNICNT   DS    PL14                UNIQUE PERSONS                               
TERCNT   DS    PL14                PERSONS TERMINATED                           
EXPCNT   DS    PL14                                                             
ADRCNT   DS    PL14                PERSONS W/ ADDRESS FIELD ENTERED             
ACTCNT   DS    PL14                ACTIVE PERSONS                               
RNGCNT   DS    PL14                ACTIVE PERSONS                               
*                                                                               
GTOTCNT  DS    PL14                                                             
GUNICNT  DS    PL14                                                             
GTERCNT  DS    PL14                                                             
GEXPCNT  DS    PL14                                                             
*                                                                               
PLINEL   DS    XL2                 PRINT LINE LENGTH                            
         DS    XL2                                                              
PLINE    DS    CL166               PRINT LINE                                   
TITLE    DS    CL166                                                            
*                                                                               
TODAY0   DS    XL6                 TODAY                                        
TODAY2   DS    XL2                 TODAY                                        
TODAY3   DS    XL3                 TODAY                                        
*                                                                               
CARD     DS    CL80                                                             
KEY      DS    CL40                                                             
         DS    CL20                                                             
*                                                                               
QH       DS    XL4                                                              
QLINE    DS    CL166               PRINT QUEUE LINE                             
NDX      DS    CL56                PRINT QUEUE INDEX                            
*                                                                               
CUAGY    DS    CL2                 AGENCY                                       
CUSAGY   DS    CL2                 SECURITY AGENCY                              
CUPCID   DS    XL2                 PRINCIPAL CONTROL ID NUMBER                  
CUPCOD   DS    CL8                 PRINCIPAL ID CODE                            
CUPCNM   DS    CL33                PRINCIPAL ID NAME                            
CUPID    DS    CL8                 PERSON ID                                    
CUDEF    DS    XL2                 PERSON ID RECORD EFFECTIVE DATE              
CUFNM    DS    CL20                FIRST NAME                                   
CUMNM    DS    CL20                MIDDLE NAME                                  
CULNM    DS    CL60                LAST NAME                                    
CUNAME   DS    CL100               FULL NAME                                    
CUCITY   DS    CL20                CITY                                         
CUSTATE  DS    CL2                 STATE                                        
CUZIP    DS    CL10                ZIP                                          
CUCTRY   DS    CL3                 COUNTRY                                      
CUEMAIL  DS    CL80                EMAIL                                        
CULLO    DS    XL2                 LAST LOGGED ON (YMD)                         
CUHIRE   DS    XL2                 HIRE DATE (BINARY)                           
CUTERM   DS    XL2                 TERMINATION DATE (BINARY)                    
*                                                                               
SINDI    DS    XL1                 STATUS INDICATOR                             
SIDWIN   EQU   X'80'               . DOWNLOAD INITIALIZED                       
SIDAFN   EQU   X'40'               . DATA FOUND                                 
SIOTOP   EQU   X'20'               . OUTPUT TAPE OPEN                           
*                                                                               
ACDELEM  DS    XL(CTAADLNQ)        ACCESS DETAILS ELEMENT                       
AGDELEM  DS    XL(CTAGDL2Q)        AGENCY GROUP DETAILS ELEMENT                 
ELEM     DS    XL256               GENERAL USE ELEMENT                          
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                   . DOWN-LOAD INITIALIZATION                   
DWNEOL   EQU   2                   . MARK END OF LINE                           
DWNEOR   EQU   3                   . MARK END OF REPORT                         
DWNTEXT  EQU   4                   . DOWN-LOAD TEXT                             
*                                                                               
DWNFLD   DS    CL80                DOWNLOAD FIELD                               
DWNBLOCK DS    CL250               DOWNLOAD BLOCK                               
*                                                                               
         DS    CL8                                                              
IOAREA1  DS    2048C               IO AREA 1                                    
         DS    CL8                                                              
IOAREA2  DS    2048C               IO AREA 1                                    
*                                                                               
SPARE    DS    1024X                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRINT LINE DSECT                                                              
***********************************************************************         
PLINED   DSECT                                                                  
PLINES   DS    CL2                                                              
PLHEAD   DS    0CL10                                                            
         DS    CL31                                                             
PLMESS   DS    CL40                                                             
         DS    CL40                                                             
*                                                                               
         ORG   PLHEAD                                                           
PLAGY    DS    CL2                                                              
         DS    CL2                                                              
PLCOUNT  DS    CL17                                                             
         DS    CL2                                                              
         DS    CL40                                                             
*                                                                               
         ORG   PLHEAD                                                           
PLSAGY   DS    CL2                                                              
         DS    CL2                                                              
PLPID    DS    CL8                                                              
         DS    CL2                                                              
PLLNAME  DS    CL19                                                             
         DS    CL1                                                              
PLFNAME  DS    CL12                                                             
         DS    CL1                                                              
PLMNAME  DS    CL1                                                              
         DS    CL2                                                              
PLHIRE   DS    CL10                                                             
         DS    CL1                                                              
PLLLO    DS    CL10                                                             
         DS    CL1                                                              
PLTERM   DS    CL10                                                             
         DS    CL1                                                              
PLCSZ    DS    CL35                                                             
         DS    CL1                                                              
PLEMAIL  DS    CL50                                                             
         ORG                                                                    
*                                                                               
PLINEQ   EQU   *-PLINED                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* DCBD                                                                          
***********************************************************************         
         DCBD    DSORG=QS,DEVD=DA                                               
                                                                                
***********************************************************************         
* IBM DSECTS                                                                    
***********************************************************************         
         IHASDWA GR32=YES                                                       
*                                                                               
* PSA -  Prefixed Save Area                                                     
         IHAPSA                                                                 
* ACEE - Accessor Environment Element                                           
         IHAACEE                                                                
* ASCB - Address Space Control Block                                            
         IHAASCB                                                                
* ASXB - Address Space Control Block Extension                                  
         IHAASXB                                                                
*                                                                               
         IEFZB4D0                                                               
         IEFZB4D2                                                               
         IEFJFCBN                                                               
*                                                                               
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
*                                                                               
***********************************************************************         
* INCLUDED DSECTS                                                               
***********************************************************************         
* DDPERVALD                                                                     
* CTGENFILE                                                                     
* SEACSFILE                                                                     
* DDDLCB                                                                        
* DMSPACED                                                                      
* FASSBOFF                                                                      
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DMSPACED                                                       
       ++INCLUDE DDCTRYEQUS                                                     
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CTSCANLLO 06/18/18'                                      
         END                                                                    
