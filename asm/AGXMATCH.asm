*          DATA SET AGXMATCH   AT LEVEL 008 AS OF 07/06/20                      
*PHASE AGXMATA                                                                  
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
                                                                                
         TITLE 'AGXMATCH - BulkAPI Mainframe Data Matching'                     
                                                                                
***********************************************************************         
* Who  Lvl Date    Change(s)                                 Audit              
* ---- --- ------- ----------------------------------------- ----------         
* TKLU 006 08Feb19 Adjust to new Acc Level layout            DSRD-21185         
* TKLU 005 23Jan19 For analysis purposes add totals          DSRD-21047         
* TKLU 004 28Sep18 Remove MQRPT as not used                  SPEC-28142         
* TKLU 003 20Sep18 TIMEITMS/TIMIMULT addition                DSRD-20170         
* TKLU 002 19Sep18 EDITR negative amounts and time ENTDNUM   DSRD-20170         
* TKLU 001 17Sep18 Initial Version                           DSRD-20170         
***********************************************************************         
*                                                                     *         
* This job reads an AGXTRACT extract file and accummulates data per   *         
* entity, then reads the entity and checks the AGXRECON value. Any    *         
* difference will be printed.                                         *         
*                                                                     *         
* Sample JCL: TKLU.DDS.JCL(AGXMAT*)                                   *         
*                                                                     *         
* Possible parameter cards see CARDTAB.                               *         
*                                                                     *         
***********************************************************************         
                                                                                
AGXMATCH CSECT                                                                  
         PRINT NOGEN                                                            
         COPY  IEABRC                                                           
         NBASE WORKX-WORKD,**AGXM**,WORK=A(WORKC),CLEAR=YES                     
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
                                                                                
         GOTOR OPNEXF              Open EXFILE                                  
                                                                                
         XC    ENTITY,ENTITY                                                    
         XC    LASTENTY,LASTENTY                                                
         ZAP   LOWAMNT,PZERO                                                    
         ZAP   HIGAMNT,PZERO                                                    
         MVI   PRTSTAT1,NOQ                                                     
                                                                                
EXLOOP   DS    0H                  Loop through EXFILE                          
                                                                                
         LAY   R3,CUR_REC                                                       
         GET   EXFILE,(R3)                                                      
                                                                                
         GOTOR SETENT              Set entity                                   
         JL    EXLOOP                                                           
                                                                                
         OC    LASTENTY,LASTENTY   First time?                                  
         JZ    EXLOOPS                                                          
                                                                                
         CLC   ENTITY,LASTENTY     Change in entity                             
         JE    ADDVAL                                                           
                                                                                
         AP    COUNTALL,PONE                                                    
                                                                                
         GOTOR GETFIL              Get AGXRECON value to HIGAMNT                
                                                                                
         CP    LOWAMNT,HIGAMNT     Do values match?                             
         JE    EXLOOPS                                                          
         GOTOR PRNTERR             Print error                                  
         AP    COUNTERR,PONE                                                    
                                                                                
EXLOOPS  ZAP   LOWAMNT,PZERO                                                    
         ZAP   HIGAMNT,PZERO                                                    
         MVC   LASTENTY,ENTITY                                                  
                                                                                
ADDVAL   GOTOR EXFVAL              add EXFILE value to LOWAMNT                  
                                                                                
         J     EXLOOP                                                           
                                                                                
EOEXF    DS    0H                                                               
                                                                                
         AP    COUNTALL,PONE                                                    
                                                                                
         GOTOR GETFIL              Get AGXRECON value to HIGAMNT                
                                                                                
         CP    LOWAMNT,HIGAMNT     Do values match?                             
         JE    PRNTOT                                                           
         GOTOR PRNTERR             Print error                                  
         AP    COUNTERR,PONE                                                    
                                                                                
PRNTOT   GOTOR PRNTIT              Printo totals                                
                                                                                
END      DS    0H                                                               
                                                                                
         GOTOR CLOEXF              Close EXFILE                                 
                                                                                
         GOTOR CLOSE               Close files                                  
                                                                                
         CP    COUNTERR,PZERO                                                   
         JE    ENDIT                                                            
         MVI   RETCODE,8                                                        
         J     ENDIT4                                                           
                                                                                
ENDIT    CP    COUNTIOE,PZERO                                                   
         JE    ENDIT2                                                           
         MVI   RETCODE,7                                                        
         J     ENDIT4                                                           
                                                                                
ENDIT2   CP    COUNTEXP,PZERO                                                   
         JE    ENDIT4                                                           
         MVI   RETCODE,6                                                        
                                                                                
ENDIT4   XBASE RC=RETCODE,RL=1                                                  
                                                                                
***********************************************************************         
* Subroutines                                                         *         
* -----------                                                         *         
***********************************************************************         
                                                                                
***********************************************************************         
* Open EXFILE for this type                                           *         
***********************************************************************         
                                                                                
OPNEXF   NTR1  ,                                                                
                                                                                
         OPEN  (EXFILE,INPUT)                                                   
                                                                                
         ZAP   COUNTALL,PZERO                                                   
         ZAP   COUNTERR,PZERO                                                   
         ZAP   COUNTIOE,PZERO                                                   
         ZAP   COUNTEXP,PZERO                                                   
                                                                                
         ZAP   TOTESTHL,PZERO                                                   
         ZAP   TOTESTLL,PZERO                                                   
         ZAP   TOTEXPHL,PZERO                                                   
         ZAP   TOTEXPLL,PZERO                                                   
         ZAP   TOTORDHL,PZERO                                                   
         ZAP   TOTORDLL,PZERO                                                   
         ZAP   TOTTIMHL,PZERO                                                   
         ZAP   TOTTIMLL,PZERO                                                   
         ZAP   TOTTRNHL,PZERO                                                   
         ZAP   TOTTRNLL,PZERO                                                   
                                                                                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Set entity of CUR_REC                                               *         
***********************************************************************         
                                                                                
SETENT   NTR1  ,                                                                
                                                                                
         XC    ENTITY,ENTITY                                                    
                                                                                
         LA    R4,RECIDS                                                        
         LAY   RF,CUR_RTY                                                       
                                                                                
SETENT02 CLI   0(R4),FFQ                                                        
         JE    EXITL               Not interested in                            
         CLC   0(L'CUR_RTY,RF),0(R4)                                            
         JE    SETENT04            Process it                                   
         AHI   R4,L'RECIDS                                                      
         J     SETENT02                                                         
                                                                                
         USING ENTITYD,R3                                                       
SETENT04 LA    R3,ENTITY           Set entity key                               
         MVC   ENTDTYP,0(RF)                                                    
                                                                                
         LAY   R2,CUR_RTY          Scan through record = count fields           
         XR    R0,R0                                                            
         LLC   RE,5(R4)            position of key                              
         LLC   RF,6(R4)            length of key * not used                     
         MVC   POSAMNT,7(R4)       position of amount                           
                                                                                
SETENT06 AHI   R2,1                                                             
         CLI   0(R2),SEPARQ        New data field?                              
         JNE   SETENT06                                                         
         AHI   R0,1                                                             
         CHI   R0,AGYQ                                                          
         JNE   SETENT08                                                         
         AHI   R2,1                                                             
         MVC   ENTDAGY,0(R2)                                                    
         J     SETENT06                                                         
                                                                                
SETENT08 CR    R0,RE                                                            
         JNE   SETENT06                                                         
         AHI   R2,1                                                             
         LR    R1,R2                                                            
         XR    R5,R5                                                            
         AHI   RF,1                                                             
                                                                                
SETENT10 CLI   0(R1),SEPARQ                                                     
         JE    SETENT12                                                         
         AHI   R5,1                                                             
         AHI   R1,1                                                             
         JCT   RF,SETENT10                                                      
         J     *+2                 bug in logic or table                        
                                                                                
SETENT12 DS    0H                                                               
         SHI   R5,1                                                             
         MVC   ENTDNUM(0),0(R2)                                                 
         EX    R5,*-6                                                           
         LHI   RF,L'ENTDNUM-1-1                                                 
         SR    RF,R5                                                            
         LA    R5,ENTDNUM+1(R5)                                                 
         CHI   RF,0                                                             
         JL    SETENT14                                                         
         MVC   0(0,R5),GSPACES                                                  
         EX    RF,*-6                                                           
                                                                                
SETENT14 CLC   ENTDTYP,AGXTIMFQ                                                 
         JNE   SETENT40                                                         
                                                                                
* format is squashed by AGXROUTS, e.g. 0003_2015-10-25_2015-10-25               
                                                                                
         MVC   WORK,GSPACES        get actual string                            
         MVC   WORK(L'ENTDNUM),ENTDNUM                                          
         MVC   ENTDNUM,SPACES                                                   
         MVI   WORK+L'ENTDNUM,SEPARQ                                            
         LA    R1,WORK                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         XR    R0,R0                                                            
                                                                                
SETENT16 CLI   0(R1),SEPARQ                                                     
         JE    SETENT24                                                         
         CLI   0(R1),UNDERSQ                                                    
         JNE   SETENT22                                                         
         LTR   RE,RE                                                            
         JNZ   SETENT18                                                         
         LR    RE,R1               set A(first _)                               
         J     SETENT22                                                         
                                                                                
SETENT18 LTR   RF,RF                                                            
         JNZ   SETENT20                                                         
         LR    RF,R1               set A(second _)                              
         J     SETENT22                                                         
                                                                                
SETENT20 LTR   R0,R0                                                            
         JNZ   *+2                 (use more registers, check data)             
         LR    R0,R1               set A(third _)                               
                                                                                
SETENT22 AHI   R1,1                                                             
         J     SETENT16                                                         
                                                                                
SETENT24 MVC   0(L'ENTDNUM,R1),GSPACES   clear down remainder                   
                                                                                
         LTR   RE,RE               must have at least two _s                    
         JZ    *+2                                                              
         LTR   RF,RF                                                            
         JZ    *+2                                                              
         LTR   R0,R0               if third move them up                        
         JZ    SETENT26                                                         
                                                                                
         LR    RE,RF                                                            
         LR    RF,R0                                                            
         XR    R0,R0                                                            
                                                                                
SETENT26 LA    R1,WORK             change address to offsets                    
         SR    RE,R1                                                            
         SR    RF,R1                                                            
                                                                                
         LR    R1,RE               now split up in relevant chunks              
         SHI   R1,1                                                             
         MVC   WORK+50(0),WORK     (WORK+50=Person code)                        
         EX    R1,*-6                                                           
                                                                                
         LA    RE,WORK+1(RE)                                                    
         MVC   WORK+62(10),0(RE)   (WORK+62=End date -)                         
                                                                                
         LA    RF,WORK+1(RF)                                                    
         MVC   WORK+74(10),0(RF)   (WORK+74=Loc. end date -)                    
                                                                                
         LA    R1,WORK+62          remove -s from dates                         
         LHI   RE,10                                                            
         LA    RF,WORK+100                                                      
                                                                                
SETENT28 CLI   0(R1),MINUSQ                                                     
         JE    SETENT30                                                         
         MVC   0(1,RF),0(R1)                                                    
         AHI   RF,1                                                             
                                                                                
SETENT30 AHI   R1,1                                                             
         JCT   RE,SETENT28                                                      
                                                                                
         LA    R1,WORK+74          remove -s from dates                         
         LHI   RE,10                                                            
         LA    RF,WORK+110                                                      
                                                                                
SETENT32 CLI   0(R1),MINUSQ                                                     
         JE    SETENT34                                                         
         MVC   0(1,RF),0(R1)                                                    
         AHI   RF,1                                                             
                                                                                
SETENT34 AHI   R1,1                                                             
         JCT   RE,SETENT32                                                      
                                                                                
         MVC   ENTDNUM+0(8),WORK+50                                             
         GOTO1 VDATCON,DMCB,(9,WORK+100),(1,DUB)                                
         XR    R1,R1                                                            
         ICM   R1,B'0111',DUB                                                   
         LNR   R1,R1                                                            
         STCM  R1,B'0111',ENTDNUM+10                                            
                                                                                
* ignore location end date                                                      
                                                                                
SETENT40 DS    0H                                                               
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
AGYQ     EQU   4                                                                
                                                                                
***********************************************************************         
* Print any single error                                              *         
***********************************************************************         
                                                                                
PRNTERR  NTR1  ,                                                                
                                                                                
         GOTOR INIPRT                                                           
                                                                                
         MVC   P(9),=CL9'Error in:'                                             
         MVC   P+12(L'LASTENTY),LASTENTY                                        
         CLC   AGXTIMFQ,LASTENTY+ENTDTYP-ENTITYD                                
         JNE   PRNTERR2                                                         
         MVC   P+12(L'LASTENTY),SPACES                                          
         MVC   P+12(18),LASTENTY                                                
         XR    R1,R1                                                            
         ICM   R1,B'0111',LASTENTY+ENTDNUM-ENTITYD+10                           
         LNR   R1,R1                                                            
         STCM  R1,B'0111',DUB                                                   
         GOTO1 VDATCON,DMCB,(1,DUB),(21,P+32)                                   
                                                                                
PRNTERR2 GOTOR VPRINTER                                                         
         EDITR (P8,HIGAMNT),(15,P+10),2,MINUS=YES                               
         MVC   P+27(11),=C'HIGH -> LOW'                                         
         EDITR (P8,LOWAMNT),(15,P+40),2,MINUS=YES                               
         GOTOR VPRINTER                                                         
                                                                                
         LA    RE,TOTESTHL                                                      
         CLC   LASTENTY(5),AGXESTFQ                                             
         JE    PRNTERR4                                                         
         LA    RE,TOTEXPHL                                                      
         CLC   LASTENTY(5),AGXEXPFQ                                             
         JE    PRNTERR4                                                         
         LA    RE,TOTTRNHL                                                      
         CLC   LASTENTY(5),AGXTRNFQ                                             
         JE    PRNTERR4                                                         
         LA    RE,TOTORDHL                                                      
         CLC   LASTENTY(5),AGXORDFQ                                             
         JE    PRNTERR4                                                         
         LA    RE,TOTTIMHL                                                      
         CLC   LASTENTY(5),AGXTIMFQ                                             
         JNE   *+2                                                              
                                                                                
PRNTERR4 AP    0(L'TOTTIMHL,RE),HIGAMNT                                         
         AP    L'TOTTIMHL(L'TOTTIMLL,RE),LOWAMNT                                
                                                                                
PRNTERRX DS    0H                                                               
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Initialiase printing details                                        *         
***********************************************************************         
                                                                                
INIPRT   ST    RE,SAVERE                                                        
                                                                                
         CLI   PRTSTAT1,YESQ                                                    
         BER   RE                                                               
         MVI   PRTSTAT1,YESQ                                                    
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
         MVC   P(26),=CL26'List of errornous entities'                          
         GOTOR VPRINTER                                                         
         MVC   P(26),=CL26'--------------------------'                          
         GOTOR VPRINTER                                                         
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
                                                                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* Print totals                                                        *         
***********************************************************************         
                                                                                
PRNTIT   NTR1  ,                                                                
                                                                                
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
         MVC   P(27),=CL27'Agency Data Matching Totals'                         
         GOTOR VPRINTER                                                         
         MVC   P(27),=CL27'---------------------------'                         
         GOTOR VPRINTER                                                         
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
                                                                                
         CP    TOTESTHL,TOTESTLL                                                
         JE    PRNTIT1                                                          
         MVC   P+00(16),=CL16'Estimate High:'                                   
         CURED (P8,TOTESTHL),(15,P+16),2,ZERO=YES,MINUS=YES                     
         MVC   P+32(6),=CL6'Low:'                                               
         CURED (P8,TOTESTLL),(15,P+38),2,ZERO=YES,MINUS=YES                     
         ZAP   DUB,TOTESTHL                                                     
         SP    DUB,TOTESTLL                                                     
         MVC   P+54(11),=CL11'Difference:'                                      
         CURED (P8,DUB),(15,P+65),2,ZERO=YES,MINUS=YES                          
         GOTOR VPRINTER                                                         
                                                                                
PRNTIT1  CP    TOTEXPHL,TOTEXPLL                                                
         JE    PRNTIT2                                                          
         MVC   P+00(16),=CL16'Expenses High:'                                   
         CURED (P8,TOTEXPHL),(15,P+16),2,ZERO=YES,MINUS=YES                     
         MVC   P+32(6),=CL6'Low:'                                               
         CURED (P8,TOTEXPLL),(15,P+38),2,ZERO=YES,MINUS=YES                     
         ZAP   DUB,TOTEXPHL                                                     
         SP    DUB,TOTEXPLL                                                     
         MVC   P+54(11),=CL11'Difference:'                                      
         CURED (P8,DUB),(15,P+65),2,ZERO=YES,MINUS=YES                          
         GOTOR VPRINTER                                                         
                                                                                
PRNTIT2  CP    TOTORDHL,TOTORDLL                                                
         JE    PRNTIT3                                                          
         MVC   P+00(16),=CL16'Orders High:'                                     
         CURED (P8,TOTORDHL),(15,P+16),2,ZERO=YES,MINUS=YES                     
         MVC   P+32(6),=CL6'Low:'                                               
         CURED (P8,TOTORDLL),(15,P+38),2,ZERO=YES,MINUS=YES                     
         ZAP   DUB,TOTORDHL                                                     
         SP    DUB,TOTORDLL                                                     
         MVC   P+54(11),=CL11'Difference:'                                      
         CURED (P8,DUB),(15,P+65),2,ZERO=YES,MINUS=YES                          
         GOTOR VPRINTER                                                         
                                                                                
PRNTIT3  CP    TOTTIMHL,TOTTIMLL                                                
         JE    PRNTIT4                                                          
         MVC   P+00(16),=CL16'Time High:'                                       
         CURED (P8,TOTTIMHL),(15,P+16),2,ZERO=YES,MINUS=YES                     
         MVC   P+32(6),=CL6'Low:'                                               
         CURED (P8,TOTTIMLL),(15,P+38),2,ZERO=YES,MINUS=YES                     
         ZAP   DUB,TOTTIMHL                                                     
         SP    DUB,TOTTIMLL                                                     
         MVC   P+54(11),=CL11'Difference:'                                      
         CURED (P8,DUB),(15,P+65),2,ZERO=YES,MINUS=YES                          
         GOTOR VPRINTER                                                         
                                                                                
PRNTIT4  CP    TOTTRNHL,TOTTRNLL                                                
         JE    PRNTIT5                                                          
         MVC   P+00(16),=CL16'Transactn High:'                                  
         CURED (P8,TOTTRNHL),(15,P+16),2,ZERO=YES,MINUS=YES                     
         MVC   P+32(6),=CL6'Low:'           '                                   
         CURED (P8,TOTTRNLL),(15,P+38),2,ZERO=YES,MINUS=YES                     
         ZAP   DUB,TOTTRNHL                                                     
         SP    DUB,TOTTRNLL                                                     
         MVC   P+54(11),=CL11'Difference:'                                      
         CURED (P8,DUB),(15,P+65),2,ZERO=YES,MINUS=YES                          
         GOTOR VPRINTER                                                         
                                                                                
PRNTIT5  MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
         MVC   P(18),=CL18'Total entities:'                                     
         EDITR (P8,COUNTALL),(15,P+20),0                                        
         GOTOR VPRINTER                                                         
         MVC   P(18),=CL18'Total errors:'                                       
         EDITR (P8,COUNTERR),(15,P+20),0                                        
         GOTOR VPRINTER                                                         
         MVC   P(18),=CL18'Total I/O errors:'                                   
         EDITR (P8,COUNTIOE),(15,P+20),0                                        
         GOTOR VPRINTER                                                         
         MVC   P(18),=CL18'Total EXC errors:'                                   
         EDITR (P8,COUNTEXP),(15,P+20),0                                        
         GOTOR VPRINTER                                                         
         MVC   P,GSPACES                                                        
         GOTOR VPRINTER                                                         
                                                                                
PRNTITX  DS    0H                                                               
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Routine to open all relevant accounting files for current system    *         
***********************************************************************         
                                                                                
OPENSYS  NTR1  ,                                                                
                                                                                
         L     RF,VUTL                                                          
         MVC   4(1,RF),0(R1)       Set UTL Value                                
         GOTOR VDATAMGR,DMCB,DMOPEN,ACCSYS,OPENLST,AIO3                         
                                                                                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Get EXFILE values for record                                        *         
***********************************************************************         
                                                                                
EXFVAL   NTR1  ,                                                                
                                                                                
         LAY   R2,CUR_RTY          Scan through record = count fields           
         XR    R0,R0                                                            
         LLC   RE,POSAMNT                                                       
         MVC   VALFLD,CNULLS                                                    
         MVC   TMPFLD,GSPACES                                                   
                                                                                
EXFVAL02 AHI   R2,1                                                             
         CLI   0(R2),SEPARQ        New data field?                              
         JNE   EXFVAL02                                                         
                                                                                
         AHI   R0,1                                                             
         CR    R0,RE                                                            
         JL    EXFVAL02                                                         
         JH    *+2                                                              
         AHI   R2,1                                                             
         LR    R1,R2                                                            
         XR    R5,R5                                                            
         LHI   RF,16+1                                                          
                                                                                
EXFVAL04 CLI   0(R1),SEPARQ                                                     
         JE    EXFVAL06                                                         
         AHI   R5,1                                                             
         AHI   R1,1                                                             
         JCT   RF,EXFVAL04                                                      
         J     *+2                 bug in logic                                 
                                                                                
EXFVAL06 LA    R4,TMPFLD                                                        
         XR    R3,R3                                                            
         ZAP   SIGN,PONE                                                        
                                                                                
EXFVAL08 CLI   0(R2),SPACEQ                                                     
         JE    EXFVAL14                                                         
         CLI   0(R2),MINUSQ                                                     
         JNE   EXFVAL10                                                         
         ZAP   SIGN,PMONE                                                       
         J     EXFVAL12                                                         
                                                                                
EXFVAL10 CLI   0(R2),CNULLQ                                                     
         JL    EXFVAL12                                                         
         MVC   0(1,R4),0(R2)                                                    
         AHI   R4,1                                                             
         AHI   R3,1                                                             
                                                                                
EXFVAL12 AHI   R2,1                                                             
         JCT   R5,EXFVAL08                                                      
                                                                                
EXFVAL14 CHI   R3,16                                                            
         JE    EXFVAL16                                                         
         LHI   RF,L'VALFLD                                                      
         SR    RF,R3                                                            
         SHI   R3,1                                                             
         LA    RF,VALFLD(RF)                                                    
         EXRL  R3,EXFVALEX                                                      
         J     EXFVAL18                                                         
                                                                                
EXFVAL16 MVC   VALFLD,TMPFLD                                                    
                                                                                
EXFVAL18 PACK  DUB,VALFLD                                                       
         MP    DUB,SIGN                                                         
                                                                                
         AP    LOWAMNT,DUB                                                      
                                                                                
         J     EXIT                                                             
                                                                                
EXFVALEX MVC   0(0,RF),TMPFLD                                                   
                                                                                
***********************************************************************         
* Get record values from file                                         *         
***********************************************************************         
                                                                                
GETFIL   NTR1  ,                                                                
                                                                                
         USING ENTITYD,R2                                                       
         LA    R2,LASTENTY                                                      
         ZAP   HIGAMNT,PZERO                                                    
                                                                                
         CLC   ENTDTYP,AGXTRNFQ                                                 
         JE    GF_TRN                                                           
         CLC   ENTDTYP,AGXORDFQ                                                 
         JE    GF_ORD                                                           
         CLC   ENTDTYP,AGXESTFQ                                                 
         JE    GF_EST                                                           
         CLC   ENTDTYP,AGXEXPFQ                                                 
         JE    GF_EXP                                                           
         CLC   ENTDTYP,AGXTIMFQ                                                 
         JE    GF_TIM                                                           
         J     *+2                 bug in logic                                 
                                                                                
         USING TRSPASD,R3                                                       
GF_TRN   LA    R3,KEY              Transactions                                 
         MVC   TRSPKEY,TRSPKEY     ------------                                 
         MVI   TRSPTYP,TRSPTYPQ                                                 
         MVI   TRSPSUB,TRSPSUBQ                                                 
         MVC   TRSPCPY,CPYCODE                                                  
         GOTO1 VHEXIN,DMCB,ENTDNUM,FULL,8                                       
         L     RE,FULL                                                          
         LNR   RE,RE                                                            
         STCM  RE,B'1111',TRSPSER                                               
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTOR VDATAMGR,DMCB,DMREAD,ACCDIR,TRSPKEY,TRSPKEY                      
         JE    GF_TRN02                                                         
                                                                                
         GOTOR INIPRT                                                           
                                                                                
         MVC   P,GSPACES                                                        
         MVC   P(23),=CL23'ERROR: TRSPAS NOT FOUND'                             
         MVC   P+30(30),ENTITYD                                                 
         GOTOR VPRINTER                                                         
         AP    COUNTIOE,PONE                                                    
         J     GETFILX                                                          
                                                                                
GF_TRN02 TM    TRSPSTA+TRNKSTAT-TRNKSTA,TRNSARCH                                
         JZ    GF_TRN04                                                         
         GOTOR VDATAMGR,DMCB,GETREC,ACCARC,TRSPDA,AIO2,DMWORK                   
         J     GF_TRN06                                                         
                                                                                
GF_TRN04 GOTOR VDATAMGR,DMCB,GETREC,ACCMST,TRSPDA,AIO2,DMWORK                   
                                                                                
GF_TRN06 JNE   *+2                                                              
                                                                                
         USING TRNRECD,R3                                                       
         L     R3,AIO2                                                          
                                                                                
         USING TRNELD,R4                                                        
         LA    R4,TRNRFST                                                       
         CLI   TRNEL,TRNELQ                                                     
         JNE   *+2                 (???)                                        
                                                                                
*        ZAP   DUB,TRNAMNT                                                      
*        TM    TRNSTAT,TRNSDR                                                   
*        JNZ   GF_TRN08                                                         
*        MP    DUB,PMONE                                                        
*                                                                               
*F_TRN08 ZAP   HIGAMNT,DUB                                                      
         ZAP   HIGAMNT,TRNAMNT                                                  
         J     GETFILX                                                          
         DROP  R3,R4                                                            
                                                                                
         USING TSWRECD,R3                                                       
GF_TIM   LA    R3,KEY              Time                                         
         XC    TSWKEY,TSWKEY       ----                                         
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CPYCODE                                                  
         MVI   BYTE,NOQ                                                         
                                                                                
         MVC   TSWKPER,ENTDNUM+0                                                
         MVC   TSWKEND,ENTDNUM+10                                               
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTOR VDATAMGR,DMCB,DMRDHI,ACCDIR,TSWKEY,TSWKEY                        
         J     GF_TIM04                                                         
                                                                                
GF_TIM02 LA    R3,KEY                                                           
         GOTOR VDATAMGR,DMCB,DMRSEQ,ACCDIR,TSWKEY,TSWKEY                        
                                                                                
GF_TIM04 JNE   GF_TIM06                                                         
         CLC   KEYSAVE(TSWKODS-TSWKEY),TSWKEY                                   
         JE    GF_TIM08                                                         
         CLI   BYTE,YESQ                                                        
         JE    GETFILX                                                          
                                                                                
GF_TIM06 GOTOR INIPRT                                                           
                                                                                
         MVC   P,GSPACES                                                        
         MVC   P(23),=CL23'ERROR: TSWKEY NOT FOUND'                             
***      MVC   P+30(30),ENTITYD                                                 
         MVC   P+30(18),ENTITYD                                                 
         XR    R1,R1                                                            
         ICM   R1,B'0111',ENTDNUM+10                                            
         LNR   R1,R1                                                            
         STCM  R1,B'0111',DUB                                                   
         GOTO1 VDATCON,DMCB,(1,DUB),(21,P+50)                                   
         GOTOR VPRINTER                                                         
         AP    COUNTIOE,PONE                                                    
         J     GETFILX                                                          
                                                                                
GF_TIM08 GOTOR VDATAMGR,DMCB,GETREC,ACCMST,TSWKDA,AIO2,DMWORK                   
         JNE   *+2                                                              
                                                                                
         USING TIMRECD,R3                                                       
         L     R3,AIO2                                                          
                                                                                
         CLC   TIMKULC,GSPACES     Skip t/s if no c/a                           
         JNH   GF_TIM02                                                         
                                                                                
         USING TIMELD,R4                                                        
         LA    R4,TIMRFST                                                       
                                                                                
GF_TIM10 CLI   TIMEL,0                                                          
         JE    GF_TIM02                                                         
         CLI   TIMEL,TIMELQ                                                     
         JNE   GF_TIM14                                                         
         CLI   TIMETYP,TIMEINP                                                  
         JNE   GF_TIM12                                                         
         CLI   TIMLN,TIMILN1Q                                                   
         JL    GF_TIM14                                                         
                                                                                
         AP    HIGAMNT,TIMHRS                                                   
         MVI   BYTE,YESQ                                                        
         J     GF_TIM14                                                         
                                                                                
GF_TIM12 CLI   TIMETYP,TIMEITMS                                                 
         JNE   GF_TIM14                                                         
         AP    HIGAMNT,TIMIMULT                                                 
         MVI   BYTE,YESQ                                                        
                                                                                
GF_TIM14 LLC   R1,TIMLN                                                         
         AR    R4,R1                                                            
         J     GF_TIM10                                                         
         DROP  R3,R4                                                            
                                                                                
         USING EXNPASD,R3                                                       
GF_EXP   LA    R3,KEY              Expenses                                     
         XC    EXNPAS,EXNPAS       --------                                     
         MVI   EXNPTYP,EXNPTYPQ                                                 
         MVI   EXNPSUB,EXNPSUBQ                                                 
         MVC   EXNPCPY,CPYCODE                                                  
         MVC   EXNPTYPE,ENTDNUM+7                                               
         MVC   EXNPNUM,ENTDNUM+0                                                
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTOR VDATAMGR,DMCB,DMREAD,ACCDIR,EXNPAS,EXNPAS                        
         JE    GF_EXP02                                                         
                                                                                
         GOTOR INIPRT                                                           
                                                                                
         MVC   P,GSPACES                                                        
         MVC   P(23),=CL23'ERROR: EXNPAS NOT FOUND'                             
         MVC   P+30(30),ENTITYD                                                 
         GOTOR VPRINTER                                                         
         AP    COUNTIOE,PONE                                                    
         J     GETFILX                                                          
                                                                                
GF_EXP02 GOTOR VDATAMGR,DMCB,GETREC,ACCMST,EXNPDA,AIO2,DMWORK                   
         JNE   *+2                                                              
                                                                                
         USING EXCRECD,R3                                                       
         L     R3,AIO2                                                          
         USING CLDELD,R4                                                        
         LA    R4,EXCRFST                                                       
                                                                                
GF_EXP04 CLI   CLDEL,0                                                          
         JE    *+2                                                              
         CLI   CLDEL,CLDELQ                                                     
         JNE   GF_EXP06                                                         
         CLI   CLDTYPE,CLDTHDRQ                                                 
         JE    GF_EXP08                                                         
                                                                                
GF_EXP06 LLC   R1,CLDLN                                                         
         AR    R4,R1                                                            
         J     GF_EXP04                                                         
                                                                                
GF_EXP08 ZAP   HIGAMNT,CLDTAMT     (differs to AGXRECON -ª )                    
         GOTOR CHKEXP                                    |                      
         JE    GETFILX                                   V                      
         AP    COUNTEXP,PONE       separate check for CLDTAMT                   
                                                                                
         GOTOR INIPRT                                                           
                                                                                
         MVC   P,GSPACES                                                        
         MVC   P(26),=CL26'ERROR: CLDTAMT NEQ CIDMAMT'                          
         MVC   P+30(30),ENTITYD                                                 
         GOTOR VPRINTER                                                         
         J     GETFILX                                                          
         DROP  R3,R4                                                            
                                                                                
         USING ORDRECD,R3                                                       
GF_ORD   LA    R3,KEY              Orders                                       
         XC    ORDKEY,ORDKEY       ------                                       
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CPYCODE                                                  
         MVC   ORDKORD,ENTDNUM                                                  
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTOR VDATAMGR,DMCB,DMREAD,ACCDIR,ORDKEY,ORDKEY                        
         JE    GF_ORD02                                                         
                                                                                
         GOTOR INIPRT                                                           
                                                                                
         MVC   P,GSPACES                                                        
         MVC   P(23),=CL23'ERROR: ORDKEY NOT FOUND'                             
         MVC   P+30(30),ENTITYD                                                 
         GOTOR VPRINTER                                                         
         AP    COUNTIOE,PONE                                                    
         J     GETFILX                                                          
                                                                                
GF_ORD02 GOTOR VDATAMGR,DMCB,GETREC,ACCMST,ORDKDA,AIO2,DMWORK                   
         JNE   *+2                                                              
                                                                                
         L     R3,AIO2                                                          
         USING OAMELD,R4                                                        
         LA    R4,ORDRFST                                                       
                                                                                
GF_ORD04 CLI   OAMEL,OAMELQ                                                     
         JNE   GF_ORD06                                                         
                                                                                
         AP    HIGAMNT,OAMAMNT                                                  
                                                                                
GF_ORD06 CLI   OAMEL,0                                                          
         JE    GETFILX                                                          
         LLC   R1,OAMLN                                                         
         AR    R4,R1                                                            
         J     GF_ORD04                                                         
         DROP  R3,R4                                                            
                                                                                
         USING EGNPASD,R3                                                       
GF_EST   LA    R3,KEY              Estimates                                    
         XC    EGNPAS,EGNPAS       ---------                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,CPYCODE                                                  
         MVC   EGNPNUM,ENTDNUM                                                  
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTOR VDATAMGR,DMCB,DMRDHI,ACCDIR,EGNPAS,EGNPAS                        
         JNE   GF_EST02                                                         
         CLC   KEYSAVE(EGNPCLI-EGNPASD),EGNPASD                                 
         JE    GF_EST04                                                         
                                                                                
GF_EST02 GOTOR INIPRT                                                           
                                                                                
         MVC   P,GSPACES                                                        
         MVC   P(23),=CL23'ERROR: EGNPAS NOT FOUND'                             
         MVC   P+30(30),ENTITYD                                                 
         GOTOR VPRINTER                                                         
         AP    COUNTIOE,PONE                                                    
         J     GETFILX                                                          
                                                                                
GF_EST04 GOTOR VDATAMGR,DMCB,GETREC,ACCMST,EGNPDA,AIO2,DMWORK                   
         JNE   *+2                                                              
                                                                                
         USING ESTRECD,R3                                                       
         L     R3,AIO2                                                          
         USING EMDELD,R4                                                        
         LA    R4,ESTRFST                                                       
                                                                                
GF_EST06 CLI   EMDEL,EMDELQ                                                     
         JNE   GF_EST08                                                         
                                                                                
         AP    HIGAMNT,EMDAMT                                                   
         J     GETFILX                                                          
                                                                                
GF_EST08 CLI   EMDEL,0                                                          
         JE    GETFILX                                                          
         LLC   R1,EMDLN                                                         
         AR    R4,R1                                                            
         J     GF_EST06                                                         
         DROP  R3,R4                                                            
                                                                                
GETFILX  DS    0H                                                               
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Check Expense claim CIDMAMT value                                   *         
***********************************************************************         
                                                                                
CHKEXP   NTR1  ,                                                                
                                                                                
         USING EXCRECD,R3                                                       
         L     R3,AIO2                                                          
         MVC   KEYSAVE,EXCKEY                                                   
                                                                                
         ZAP   DUB,PZERO           CIDMAMT accumulator                          
                                                                                
         MVC   KEY,EXCKEY                                                       
         GOTOR VDATAMGR,DMCB,DMREAD,ACCDIR,KEY,KEY                              
         JNE   *+2                                                              
                                                                                
         J     CHKEXP10                                                         
                                                                                
CHKEXP02 LA    R3,KEY                                                           
         GOTOR VDATAMGR,DMCB,DMRSEQ,ACCDIR,EXCKEY,EXCKEY                        
         JNE   *+2                                                              
                                                                                
         CLC   KEYSAVE(EXCKSEQ-EXCRECD),EXCKEY                                  
         JNE   CHKEXP20                                                         
                                                                                
         GOTOR VDATAMGR,DMCB,GETREC,ACCMST,EXCKDA,AIO2,DMWORK                   
         JNE   *+2                                                              
                                                                                
         L     R3,AIO2                                                          
                                                                                
         USING CIDELD,R4                                                        
CHKEXP10 LA    R4,EXCRFST                                                       
                                                                                
CHKEXP12 CLI   CIDEL,0                                                          
         JE    CHKEXP02                                                         
         CLI   CIDEL,CIDELQ                                                     
         JNE   CHKEXP14                                                         
         CLI   CIDTYPE,CIDTYMQ                                                  
         JNE   CHKEXP14                                                         
         TM    CIDMSTA,CIDMSID     (skip if deleted)                            
         JNZ   CHKEXP14                                                         
                                                                                
         AP    DUB,CIDMAMT                                                      
                                                                                
CHKEXP14 LLC   R1,CIDLN                                                         
         AR    R4,R1                                                            
         J     CHKEXP12                                                         
         DROP  R3,R4                                                            
                                                                                
CHKEXP20 CP    HIGAMNT,DUB                                                      
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Close files                                                         *         
***********************************************************************         
                                                                                
CLOSE    NTR1  ,                                                                
                                                                                
         GOTOR VDATAMGR,DMCB,DMCLSE,ACCSYS,OPENLST,AIO3                         
                                                                                
CLOSEX   DS    0H                                                               
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Close EXFILE for this type                                          *         
***********************************************************************         
                                                                                
CLOEXF   NTR1  ,                                                                
                                                                                
         CLOSE (EXFILE)                                                         
                                                                                
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
         MVC   VHELLO,=V(HELLO)                                                 
         MVC   CUREDIT,=V(CUREDIT)                                              
         MVC   VHEXIN,=V(HEXIN)                                                 
         MVC   VHEXOUT,=V(HEXOUT)                                               
         MVC   VLOGIO,=V(LOGIO)                                                 
         MVC   VPRINTER,=V(PRINTER)                                             
         MVC   VPRNTBL,=V(PRNTBL)                                               
         MVC   VNUMVAL,=V(NUMVAL)                                               
         MVC   VLOADER,=V(LOADER)                                               
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
                                                                                
VALC040  CLI   CPYCODE,X'41'                                                    
         JL    CARDER7                                                          
         CLI   CPYCODE,X'FD'                                                    
         JH    CARDER7                                                          
                                                                                
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
                                                                                
CARDER7  MVC   P(20),=CL20'Missing/bad COMPANY='                                
         LA    R2,507                                                           
         J     VALCDIE                                                          
                                                                                
VALCDIE  DS    0H                                                               
         GOTOR VPRINTER            Print out card                               
                                                                                
VALCDIE2 ABEND (R2),DUMP                                                        
                                                                                
*********************************************************************           
* WHICHSYS=AA                                                       *           
* Validate basic format. MEDSID is called at end of validation      *           
*********************************************************************           
                                                                                
SYSVAL   NTR1  ,                                                                
         LA    R2,79(R7)           Find length of RHS                           
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
* COMPANY=AN                                                        *           
*********************************************************************           
                                                                                
CPYVAL   NTR1  ,                                                                
                                                                                
         GOTO1 VHEXIN,DMCB,8(R7),CPYCODE,2                                      
                                                                                
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
* DDSIO=  validation                                                *           
*********************************************************************           
                                                                                
DSIOVAL  NTR1  ,                                                                
         LA    R2,6(R7)                                                         
         L     RF,=V(DDSIO)        Set up DDSIO override                        
         MVC   0(8,RF),0(R2)                                                    
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
                                                                                
EFFS     DC    X'FFFFFFFF'                                                      
NULLS    DC    X'0000000000000000'                                              
DMOPEN   DC    CL8'OPEN   '                                                     
DMCLSE   DC    CL8'DMCLSE '                                                     
GETREC   DC    CL8'GETREC '                                                     
DMREAD   DC    CL8'DMREAD '                                                     
DMRSEQ   DC    CL8'DMRSEQ '                                                     
DMRDHI   DC    CL8'DMRDHI '                                                     
CONTROL  DC    CL8'CONTROL'                                                     
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
CNULLS   DC    16C'0'                                                           
GSPACES  DC    256C' '                                                          
                                                                                
*********************************************************************           
* Datasets and Cards                                                *           
*********************************************************************           
                                                                                
RECIDS   DS    0CL8                AGXRECID required values                     
AGXTRNFQ DC    CL5'05311',AL1(11)  TransactionFacts TRNFKEY/TRNFNAM             
         DC    AL1(08),AL1(30)                                                  
AGXORDFQ DC    CL5'05313',AL1(09)  OrderFacts ORDFORD/ORDFAMT                   
         DC    AL1(06),AL1(26)                                                  
AGXESTFQ DC    CL5'05315',AL1(08)  EstimateFacts ESTFEID/ESTFAMT                
         DC    AL1(06),AL1(25)                                                  
AGXEXPFQ DC    CL5'05317',AL1(25)  ExpenseClaimFacts EXPFCLN/EXPFAMT            
         DC    AL1(08),AL1(27)                                                  
AGXTIMFQ DC    CL5'05319',AL1(15)  TimeSheetFacts TIMFTSRN/TIMFHONI             
         DC    AL1(30),AL1(26)                                                  
         DC    X'FF'                                                            
                                                                                
EXFILE   DCB   DDNAME=EXFILE,                                          +        
               DSORG=PS,                                               +        
               MACRF=(GM),                                             +        
               RECFM=VB,                                               +        
               LRECL=8192,                                             +        
               EODAD=EOEXF                                                      
                                                                                
CARDTAB  DS    0H                                                               
         DC    C'COMPANY   ',AL1(6),AL1(0),AL1(CARDRTNQ),AL4(CPYVAL)            
         DC    C'DDSIO     ',AL1(4),AL1(0),AL1(CARDRTNQ),AL4(DSIOVAL)           
         DC    C'DSPACE    ',AL1(5),AL1(0),AL1(0),AL4(DSPACE-WORKD)             
         DC    C'FLASH     ',AL1(4),AL1(0),AL1(CARDRTNQ),AL4(FLASH)             
*&&US*&& DC    C'SYSTEM    ',AL1(7),AL1(0),AL1(CARDRTNQ),AL4(SYSVAL)            
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
                                                                                
$$CODE   LOCTR ,                                                                
                                                                                
*********************************************************************           
* Equates.                                                          *           
*********************************************************************           
                                                                                
FFQ      EQU   X'FF'                                                            
EOTQ     EQU   255                                                              
EORQ     EQU   0                                                                
CNULLQ   EQU   C'0'                                                             
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
MINUSQ   EQU   C'-'                                                             
SPACEQ   EQU   C' '                                                             
SLASHQ   EQU   C'/'                                                             
DELIMQ   EQU   C'.'                                                             
SEPARQ   EQU   C';'                                                             
UNDERSQ  EQU   C'_'                                                             
                                                                                
CARDTABD DSECT ,                                                                
CARDKEY  DS    CL10                                                             
CARDKXLN DS    XL1             Length of key                                    
CARDVXLN DS    XL1             Length of field to move directly                 
CARDIND  DS    XL1                                                              
CARDRTNQ EQU   X'80'           Call routine to validate field                   
CARDNUMQ EQU   X'40'           Number comes before the delimeter                
CARDVDSP DS    XL4             Routine to validate field                        
CARDTBLQ EQU   *-CARDTABD                                                       
                                                                                
ENTITYD  DSECT ,                                                                
ENTDTYP  DS    CL5                                                              
ENTDAGY  DS    CL5                                                              
ENTDNUM  DS    CL30                                                             
                                                                                
WORKD    DSECT                 ** GLOBAL WORKING STORAGE **                     
                                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
SAVERE   DS    A                                                                
SAVER2   DS    A                                                                
HALF     DS    H                                                                
DMCB     DS    6F                                                               
DMWORK   DS    9F                                                               
COUNTALL DS    PL8                                                              
COUNTERR DS    PL8                                                              
COUNTIOE DS    PL8                                                              
COUNTEXP DS    PL8                                                              
TOTESTHL DS    PL8                                                              
TOTESTLL DS    PL8                                                              
TOTEXPHL DS    PL8                                                              
TOTEXPLL DS    PL8                                                              
TOTORDHL DS    PL8                                                              
TOTORDLL DS    PL8                                                              
TOTTIMHL DS    PL8                                                              
TOTTIMLL DS    PL8                                                              
TOTTRNHL DS    PL8                                                              
TOTTRNLL DS    PL8                                                              
ENTITY   DS    CL40                L'ENTITYD                                    
LASTENTY DS    CL40                L'ENTITYD                                    
LOWAMNT  DS    PL8                                                              
HIGAMNT  DS    PL8                                                              
WORK     DS    XL120                                                            
BYTE     DS    X                                                                
SIGN     DS    PL1                                                              
POSAMNT  DS    X                                                                
VALFLD   DS    CL16                                                             
TMPFLD   DS    CL16                                                             
TRTAB    DS    XL256                                                            
VDATAMGR DS    V                                                                
VDATCON  DS    V                                                                
VDATVAL  DS    V                                                                
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
ATRKBUF  DS    A                                                                
ATRLOWER DS    A                                                                
ASSB     DS    A                                                                
                                                                                
DA       DS    F               Disk address                                     
                                                                                
CURSYS   DS    CL5                                                              
RETCODE  DS    XL1                                                              
ANYCARDS DS    CL1                                                              
TRACE    DS    C               TRACE=Y(ES)/N(O)                                 
DSPACE   DS    XL1                                                              
CPYCODE  DS    XL1                                                              
THISSEAL DS    CL2             Acc alpha code                                   
THISSENO DS    XL1             SE number                                        
                                                                                
PRTSTAT1 DS    CL1                                                              
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
IOAREA1  DS    2048X                                                            
IOAREA2  DS    2048X                                                            
IOAREA3  DS    2048X                                                            
                                                                                
         DS    0H                                                               
CUR_REC  DS    (8192+4)X                                                        
         ORG   CUR_REC                                                          
CUR_RLN  DS    XL2                                                              
         DS    XL2                                                              
CUR_RTY  DS    CL5                                                              
         ORG                                                                    
                                                                                
WORKX    DS    0D                                                               
                                                                                
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
**PAN#1  DC    CL21'008AGXMATCH  07/06/20'                                      
         END                                                                    
