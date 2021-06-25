*          DATA SET CTCONPRO   AT LEVEL 039 AS OF 03/20/17                      
*PHASE CONPROA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE QSORT                                                                  
         TITLE 'CTCONPRO - CONVERT PROFILE ELEMENT TO OFFICE LIST'              
         PRINT NOGEN                                                            
CTCPR    CSECT                                                                  
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE WORKX-WORKD,**CPRO**,=V(REGSAVE),RA,R9                           
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
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
         USING PLINED,PLINE                                                     
*                                                                               
         BAS   RE,INIT             READ CARDS ECT                               
         BAS   RE,MAIN             MAIN LOOP                                    
         BAS   RE,DONE             CLOSE ALL                                    
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
*                                                                               
EXITEQ   CR    RB,RB                                                            
         B     EXIT                                                             
EXITNE   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
                                                                                
***********************************************************************         
* READ PROFILE RECORDS, CHANGE ELEMENTS, AND WRITE THEM BACK                    
***********************************************************************         
MAIN     NTR1                                                                   
*                                                                               
MAIN010  BAS   RE,GETPRO           GET PROFILE RECORD                           
         BNE   MAIN020                                                          
         BAS   RE,PROPRO           PROCESS PROFILE OFFICE LIST                  
*                                                                               
MAIN020  BAS   RE,PRTREP           PRINT REPORT                                 
*                                                                               
MAINX    B     EXITEQ                                                           
                                                                                
***********************************************************************         
* INITIALIZE                                                                    
***********************************************************************         
INIT     NTR1                                                                   
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
*                                                                               
         MVI   PGI,0                                                            
         MVI   DMBITS,0                                                         
         XC    CUAGY,CUAGY                                                      
         ZAP   COUNT,=P'0'         COUNT FOR RECORDS CHANGED                    
         ZAP   COUNT2,=P'0'        COUNT FOR ERRORS                             
*                                                                               
         LA    R3,CARD                                                          
INIT010  GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
         CLC   =C'DDSIO=',0(R3)                                                 
         BNE   INIT014                                                          
         L     RF,=V(DDSIO)        OVERRIDE DDSIO NAME                          
         MVC   0(8,RF),6(R3)                                                    
         B     INIT010                                                          
*                                                                               
INIT014  CLC   =C'DSPACE=',0(R3)                                                
         BNE   INIT018                                                          
         L     RF,=A(SSB)          SET DSPACE ID IN SSB                         
         MVC   SSODSPAC-SSOOFF(1,RF),7(R3)                                      
         B     INIT010                                                          
*                                                                               
INIT018  GOTO1 VDATCON,DMCB,(5,0),(1,TODAY)                                     
         GOTO1 (RF),(R1),(5,0),(2,TODAYC)                                       
         GOTO1 (RF),(R1),(5,0),(0,TODAY0)                                       
*                                                                               
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(30),=CL30'PROFILE ELEMENT CONVERSION'                      
         LA    R1,TITLE            PRINT PARAMETER CARDS TITLE                  
         BAS   RE,PRINTT                                                        
         LA    R3,CARD                                                          
         B     INIT032                                                          
*                                                                               
INIT030  GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
INIT032  CLC   =C'/*',0(R3)                                                     
         BE    INIT100                                                          
         CLC   =C'XX',0(R3)                                                     
         BE    INIT100                                                          
         MVC   PLINE+1(80),0(R3)                                                
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         CLC   =C'WRITE=',0(R3)                                                 
         BNE   INIT040                                                          
         CLC   6(3,R3),=C'YES'                                                  
         BNE   *+8                                                              
         MVI   RCWRITE,C'Y'        WRITE RECORD                                 
         B     INIT030                                                          
*                                                                               
INIT040  CLC   =C'AGENCY=',0(R3)                                                
         BNE   INIT050                                                          
         CLI   9(R3),C' '          AGENCY=(TWO CHARACTER ALPHA ID)              
         JH    *+2                 BAD INPUT                                    
         CLI   CUAGY,C' '          DO WE HAVE ONE AGENCY ALREADY                
         BH    *+10                YES: ADD IT TO THE TABLE                     
         MVC   CUAGY,7(R3)         NO: SAVE IT AS THE CURRENT AGENCY            
         LA    R1,AGYTAB                                                        
INIT042  CLI   0(R1),X'FF'                                                      
         JE    *+2                 NO ROOM IN TABLE FOR ANOTHER AGENCY          
         CLI   0(R1),C' '          LOOK FOR EMPTY SLOT                          
         BNH   INIT044                                                          
         LA    R1,2(,R1)           BUMP TO NEXT SLOT IN TABLE                   
         B     INIT042                                                          
INIT044  MVC   0(2,R1),7(R3)       SAVE AGENCY IN OPEN SLOT                     
         B     INIT030                                                          
*                                                                               
INIT050  CLC   =C'MODE=',0(R3)                                                  
         BNE   INIT060                                                          
         CLI   5(R3),C'R'                                                       
         BNE   INIT060                                                          
         MVI   RMODE,C'R'                                                       
         B     INIT030                                                          
*                                                                               
INIT060  CLC   =C'SYSTEM=',0(R3)                                                
         BNE   INIT080                                                          
         MVC   RSYS,7(R3)                                                       
         B     INIT030                                                          
*                                                                               
INIT080  CLC   =C'LIST=',0(R3)                                                  
         BNE   INIT090                                                          
         MVC   RLIST,5(R3)                                                      
         B     INIT030                                                          
*                                                                               
INIT090  B     INIT030                                                          
*                                                                               
INIT100  CLI   RCWRITE,C'Y'        WRITE=Y?                                     
         BNE   *+12                . NO                                         
         MVI   FCTFILE,C'U'        OPEN CTFILE FOR UPDATE                       
         MVI   DMBITS,X'80'        READ FOR UPDATE                              
         OI    DMBITS,X'08'        READ FOR DELETED RECORDS                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMOPEN  ',=C'CONTROL ',FLIST,AIO1               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RCWRITE,C'Y'        ENQUEUE CONTROL IF WRITING                   
         BNE   INIT110                                                          
         GOTO1 VDATAMGR,DMCB,=C'ENQCTL ',(C'E',=C'CTRL')                        
         TM    8(R1),X'04'                                                      
         BO    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         BAS   RE,PRINTL                 HEADLINES                              
         MVC   PLINE(L'MWRITE),MWRITE                                           
         BAS   RE,PRINTL                                                        
         MVC   PLINE,DASHES                                                     
         BAS   RE,PRINTL                                                        
         BAS   RE,PRINTL                                                        
         MVC   PLINE(L'H1),H1                                                   
         BAS   RE,PRINTL                                                        
         MVC   PLINE(L'H2),H2                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
INIT110  B     EXITEQ                                                           
                                                                                
***********************************************************************         
* FINISHED, CLOSE                                                               
***********************************************************************         
DONE     NTR1                                                                   
         CLOSE SYSPRINT            CLOSE PRINT                                  
*                                                                               
         CLI   RCWRITE,C'Y'        ENQUEUE CONTROL IF WRITING                   
         BNE   DONE010                                                          
         GOTO1 VDATAMGR,DMCB,=C'ENQCTL ',(C'D',=C'CTRL')                        
DONE010  GOTO1 VDATAMGR,DMCB,=C'DMCLSE ',=C'CONTROL ',FLIST,AIO1                
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
* GET PROFILE RECORD(S)                                                         
***********************************************************************         
GETPRO   NTR1                                                                   
*                                                                               
         MVC   CUSYS,RSYS                                                       
         MVC   CUOFFL,RLIST                                                     
*                                                                               
         XC    PROFOF,PROFOF       INITIALIZE THE PROFILE VALUES                
         XC    PROF,PROF           "                                            
         XC    PROFA,PROFA         "                                            
         XC    PROFB,PROFB         "                                            
         XC    PROFC,PROFC         "                                            
         XC    PROFD,PROFD         "                                            
*                                                                               
         XC    KEY,KEY             FIND ID REC                                  
         LA    R2,KEY                                                           
         USING CTUREC,R2                                                        
         MVI   CTUKTYP,CTUKTYPQ    C'U'                                         
         MVC   CTUKSYS,CUSYS                                                    
         MVI   CTUKPROG+1,C'$'                                                  
         MVC   CTUKPROG+2(1),CUOFFL                                             
         MVC   CTUKAGY,CUAGY                                                    
*                                                                               
GPR010   GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),CTFILE,KEY,AIO2                    
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         TM    8(R1),X'02'         RECORD MARKED FOR DELETION                   
         BZ    GPR200              NO: OFFICE LIST NOT FOUND                    
*                                                                               
         GOTOR DUMPREC,DMCB,(C'R',AIO2)                                         
*                                                                               
         L     R2,AIO2                                                          
         LA    R4,CTUDATA                                                       
GPR020   CLI   0(R4),0             END OF RECORD                                
         BE    GPR200                                                           
         CLI   0(R4),CTPVELQ       PROFILE VALUE ELEMENT     X'72'              
         BE    GPR040                                                           
         CLI   0(R4),CTOFELQ       MEDIA OFFICE LIST ELEMENT X'75'              
         BE    GPR030                                                           
GPR025   LLC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     GPR020                                                           
*                                                                               
         USING CTOFD,R4                                                         
GPR030   CLI   RMODE,C'C'          CONVERT MODE?                                
         BE    GPRNX               NO NEED, ALREADY NEW STYLE                   
         LLC   RF,CTOFLEN                                                       
         AHI   RF,-((CTOFOFFS-CTOFEL)+1)                                        
         BM    GPR100                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PROFOF(0),CTOFOFFS  SAVE OFFICE VALUES                           
         B     GPR100                                                           
*                                                                               
         USING CTPVD,R4                                                         
GPR040   CLI   RMODE,C'R'          REVERSE MODE?                                
         BE    GPRNX               NO NEED, ALREADY OLD STYLE                   
         MVC   PROF,CTPVALUE       SAVE PROFILE OFFICE VALUES                   
*                                                                               
         MVI   CUPASS,C'A'                                                      
         LA    R6,PROFA                                                         
*                                                                               
GPR050   XC    KEY,KEY             FIND ID REC                                  
         LA    R2,KEY                                                           
         USING CTUREC,R2                                                        
         MVI   CTUKTYP,CTUKTYPQ    C'U'                                         
         MVC   CTUKSYS,CUSYS                                                    
         MVI   CTUKPROG,C'$'                                                    
         MVC   CTUKPROG+1(1),CUOFFL                                             
         MVC   CTUKPROG+2(1),CUPASS                                             
         MVC   CTUKAGY,CUAGY                                                    
*                                                                               
GPR060   GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),CTFILE,KEY,AIO2                    
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         TM    8(R1),X'02'         RECORD MARKED FOR DELETION                   
         BZ    GPR090              NO: THIS IS NO GOOD                          
*                                                                               
         L     R2,AIO2                                                          
         LA    R4,CTUDATA                                                       
GPR070   CLI   0(R4),0             END OF RECORD                                
         BE    GPR090                                                           
         CLI   0(R4),CTPVELQ       PROFILE VALUE ELEMENT     X'72'              
         BE    GPR080                                                           
GPR075   LLC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     GPR070                                                           
*                                                                               
GPR080   MVC   0(L'PROF,R6),CTPVALUE                                            
*                                                                               
GPR090   LLC   RF,CUPASS           BUMP TO NEXT SUB RECORD                      
         AHI   RF,1                                                             
         STC   RF,CUPASS                                                        
         CLI   CUPASS,C'D'         ONLY A,B,C,D SUB RECORDS                     
         BH    GPR100                                                           
         LA    R6,L'PROF(,R6)      BUMP TO NEXT ELEMENT FOR PASS                
         B     GPR050                                                           
         DROP  R4                                                               
*                                                                               
GPR100   DS    0H                                                               
*                                                                               
GPR200   DS    0H                                                               
*                                                                               
GPREX    B     EXITEQ                                                           
GPRNX    B     EXITNE                                                           
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* CHANGE PROFILE RECORD(S)                                                      
***********************************************************************         
PROPRO   NTR1                                                                   
*                                                                               
         XC    OFFCOUNT,OFFCOUNT                                                
         XC    ELEM,ELEM                                                        
*                                                                               
         CLI   RMODE,C'R'          REVERSE CONVERSION?                          
         BE    PPR500              YES: DIFFERENT PROCESS                       
*                                                                               
         OC    PROFOF,PROFOF       IS THIS RECORD IN NEW FORMAT?                
         BZ    PPR010              NO: THEN NEEDS CONVERSION                    
*                                                                               
         MVC   PLINE(50),=CL50'ELEMENTS ALREADY IN NEW FORMAT'                  
         BRAS  RE,PRINTL                                                        
*                                                                               
         B     PPR900                                                           
                                                                                
*--------------------------------------------------------------                 
* CONVERT OFFICE LIST PROFILE TO NEW MEDIA OFFICE LIST ELEMENT                  
*--------------------------------------------------------------                 
PPR010   LA    R4,PROF             R4=A(OLD STYLE OFFICE LIST)                  
         LHI   R6,L'PROF*MAXPROFS                                               
*                                                                               
PPR020   LA    R5,PROFOF           R5=A(NEW STYLE OFFICE LIST)                  
PPR022   CLI   0(R5),0             EMPTY SLOT FOR OFFICE                        
         BE    PPR030              YES: ADD OFFICE                              
         CLC   0(1,R5),0(R4)       DID WE ALREADY ADD THIS OFFICE?              
         BE    PPR040              YES: MOVE ON TO NEXT OFFICE                  
         LA    R5,1(,R5)           NO: KEEP LOOKING FOR SLOT TO ADD             
         B     PPR022                                                           
*                                                                               
PPR030   CLI   0(R4),0             IS AN OFFICE DEFINED                         
         BE    PPR040              NO: SKIP                                     
         MVC   0(1,R5),0(R4)       ADD TO NEW STYLE OFFICE LIST                 
         LLC   RF,OFFCOUNT                                                      
         AHI   RF,1                                                             
         STC   RF,OFFCOUNT         COUNT OFFICES IN THE LIST                    
*                                                                               
PPR040   LA    R4,1(,R4)           BUMP TO NEXT IN OLD STYLE LIST               
         BCT   R6,PPR020                                                        
*                                                                               
         CLI   OFFCOUNT,0          ANY OFFICES IN THE LIST                      
         BE    PPR900              NO                                           
*                                                                               
         XC    KEY,KEY             FIND ID REC                                  
         LA    R2,KEY                                                           
         USING CTUREC,R2                                                        
         MVI   CTUKTYP,CTUKTYPQ    C'U'                                         
         MVC   CTUKSYS,CUSYS                                                    
         MVI   CTUKPROG+1,C'$'                                                  
         MVC   CTUKPROG+2(1),CUOFFL                                             
         MVC   CTUKAGY,CUAGY                                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),CTFILE,KEY,AIO2                    
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         TM    8(R1),X'02'         RECORD MARKED FOR DELETION                   
         JZ    *+2                 NO: THIS IS NO GOOD                          
*                                                                               
         L     R2,AIO2                                                          
         NI    CTUSTAT,X'FF'-X'80'                                              
         GOTO1 VHELLO,DMCB,(C'D',CTFILE),('CTPVELQ',(R2)),0,0                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTOFD,R3                                                         
         MVI   CTOFEL,CTOFELQ                                                   
         MVI   CTOFLEN,CTOFOFFS-CTOFEL                                          
         CLI   OFFCOUNT,0          ANY OFFICES IN THE LIST                      
         BE    PPR100              NO                                           
         LLC   RF,OFFCOUNT                                                      
         AHI   RF,CTOFOFFS-CTOFEL                                               
         STC   RF,CTOFLEN                                                       
         AHI   RF,-((CTOFOFFS-CTOFEL)-1)                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTOFOFFS(0),PROFOF                                               
*                                                                               
PPR100   GOTO1 VHELLO,DMCB,(C'P',CTFILE),((R2)),ELEM,0                          
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
*                                                                               
         GOTOR DUMPREC,DMCB,(C'W',AIO2)                                         
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PPR900                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMWRT,CTFILE,KEY,AIO2                              
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         B     PPR900                                                           
                                                                                
*----------------------------------------------------------------               
* CONVERT OFFICE LIST ELEMENT BACK TO OLD STYLE PROFILE ELEMENTS                
*----------------------------------------------------------------               
PPR500   DS    0H                                                               
                                                                                
         LA    R2,PROFOF           COUNT OFFICES IN LIST                        
PPR522   CLI   0(R2),0             EMPTY SLOT FOR OFFICE                        
         BE    PPR530              YES: ADD OFFICE                              
         LLC   RF,OFFCOUNT                                                      
         AHI   RF,1                                                             
         STC   RF,OFFCOUNT         COUNT OFFICES IN THE LIST                    
         LA    R2,1(,R2)           NO: KEEP LOOKING FOR SLOT TO ADD             
         B     PPR522                                                           
*                                                                               
PPR530   LLC   R5,OFFCOUNT         OFFICES IN THE LIST                          
         CHI   R5,L'PROF*MAXPROFS                                               
         JH    *+2                 OFFICE OVERFLOW, TRUNCATION NEEDED           
                                                                                
         LA    R2,PROFOF                                                        
         LA    R3,PROF                                                          
         LA    R5,MAXPROFS                                                      
*                                                                               
PPR540   LA    R4,L'CTPVALUE                                                    
*                                                                               
PPR542   MVC   0(1,R3),0(R2)                                                    
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   0(R3),C'0'                                                       
*                                                                               
         LA    R2,1(,R2)                                                        
         LA    R3,1(,R3)                                                        
         BCT   R4,PPR542                                                        
*                                                                               
         CLI   0(R2),0                                                          
         BE    PPR550                                                           
         BCT   R5,PPR540                                                        
         DROP  R3                                                               
*                                                                               
PPR550   XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CTUREC,R2                                                        
         MVI   CTUKTYP,CTUKTYPQ    C'U'                                         
         MVC   CTUKSYS,CUSYS                                                    
         MVI   CTUKPROG+1,C'$'                                                  
         MVC   CTUKPROG+2(1),CUOFFL                                             
         MVC   CTUKAGY,CUAGY                                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),CTFILE,KEY,AIO2                    
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         TM    8(R1),X'02'         RECORD MARKED FOR DELETION                   
         JZ    *+2                 NO: THIS IS NO GOOD                          
*                                                                               
         L     R2,AIO2                                                          
         NI    CTUSTAT,X'FF'-X'80'                                              
         GOTO1 VHELLO,DMCB,(C'D',CTFILE),('CTOFELQ',(R2)),0,0                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTPVD,R3                                                         
         MVI   CTPVEL,CTPVELQ                                                   
         MVI   CTPVLEN,(CTPVALUE-CTPVD)+L'CTPVALUE                              
         MVC   CTPVALUE,PROF                                                    
*                                  ADD THE ELEMENT                              
         GOTO1 VHELLO,DMCB,(C'P',CTFILE),((R2)),ELEM,0                          
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
*                                                                               
         GOTOR DUMPREC,DMCB,(C'W',AIO2)                                         
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PPR590                                                           
         GOTO1 VDATAMGR,DMCB,DMWRT,CTFILE,KEY,AIO2                              
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
PPR590   MVI   CUPASS,C'A'         SUB RECORDS START WITH PASS C'A'             
         LA    R6,PROFA            FIRST OFFICE IN SUB RECORD                   
*                                                                               
PPR600   CLI   0(R6),0             ANY OFFICES LEFT?                            
         BE    PPR900              NO: EXIT HERE                                
*                                                                               
         XC    KEY,KEY             FIND ID REC                                  
         LA    R2,KEY                                                           
         USING CTUREC,R2                                                        
         MVI   CTUKTYP,CTUKTYPQ    C'U'                                         
         MVC   CTUKSYS,CUSYS                                                    
         MVI   CTUKPROG,C'$'                                                    
         MVC   CTUKPROG+1(1),CUOFFL                                             
         MVC   CTUKPROG+2(1),CUPASS                                             
         MVC   CTUKAGY,CUAGY                                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),CTFILE,KEY,AIO2                    
         CLI   8(R1),0                                                          
         BE    PPR610                                                           
         TM    8(R1),X'02'         RECORD MARKED FOR DELETION                   
         JO    PPR610              NO: RECORD NOT FOUND                         
*                                                                               
         L     R2,AIO2                                                          
         OI    PGI,PGIADD                                                       
         XC    0(256,R2),0(R2)                                                  
         MVC   CTUKEY,KEY                                                       
*                                                                               
PPR610   GOTOR DUMPREC,DMCB,(C'R',AIO2)                                         
*                                                                               
         L     R2,AIO2                                                          
         NI    CTUSTAT,X'FF'-X'80'                                              
         GOTO1 VHELLO,DMCB,(C'D',CTFILE),('CTOFELQ',(R2)),0,0                   
         GOTO1 VHELLO,DMCB,(C'D',CTFILE),('CTPVELQ',(R2)),0,0                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTPVD,R3                                                         
         MVI   CTPVEL,CTPVELQ                                                   
         MVI   CTPVLEN,(CTPVALUE-CTPVD)+L'CTPVALUE                              
         MVC   CTPVALUE,0(R6)                                                   
*                                  ADD THE ELEMENT                              
         GOTO1 VHELLO,DMCB,(C'P',CTFILE),((R2)),ELEM,0                          
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
*                                                                               
         GOTOR DUMPREC,DMCB,(C'W',AIO2)                                         
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PPR660                                                           
         TM    PGI,PGIADD                                                       
         BZ    PPR650                                                           
         GOTO1 VDATAMGR,DMCB,DMADD,CTFILE,KEY,AIO2                              
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         B     PPR660                                                           
PPR650   GOTO1 VDATAMGR,DMCB,DMWRT,CTFILE,KEY,AIO2                              
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
PPR660   NI    PGI,X'FF'-PGIADD                                                 
*                                                                               
         LA    R6,L'PROF(,R6)                                                   
         LLC   RF,CUPASS                                                        
         AHI   RF,1                                                             
         STC   RF,CUPASS                                                        
         CLI   CUPASS,C'D'                                                      
         BH    PPR900                                                           
         B     PPR600                                                           
         DROP  R3                                                               
*                                                                               
PPR900   DS    0H                                                               
*                                                                               
PPREQX   B     EXITEQ                                                           
PPRNEX   B     EXITNE                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PRTREP   NTR1                                                                   
*                                                                               
*        BAS   RE,PRINTL                 HEADLINES                              
*        MVC   PLINE(L'MREPORT),MREPORT                                         
*        BAS   RE,PRINTL                                                        
*        MVC   PLINE,DASHES                                                     
*        BAS   RE,PRINTL                                                        
*        BAS   RE,PRINTL                                                        
*        MVC   PLINE(L'H1),H1                                                   
*        BAS   RE,PRINTL                                                        
*        MVC   PLINE(L'H2),H2                                                   
*        BAS   RE,PRINTL                                                        
*                                                                               
PREX     B     EXITEQ                                                           
         B     EXITNE                                                           
                                                                                
***********************************************************************         
* DUMP RECORDS                                                                  
***********************************************************************         
DUMPREC  NTR1                                                                   
*                                                                               
         L     R2,0(R1)                                                         
         LA    R2,0(R2)                                                         
         MVC   PLINE(4),=C'READ'                                                
         CLI   0(R1),C'R'                                                       
         BE    DR004                                                            
         MVC   PLINE(5),=C'WRITE'                                               
         CLI   RCWRITE,C'Y'                                                     
         BE    DR004                                                            
         MVC   PLINE+6(31),=C'** WRITE=NO, NO ACTUAL WRITE **'                  
DR004    BAS   RE,PRINTL                                                        
*                                                                               
         LHI   R5,CTUDATA-CTUREC                                                
         GOTO1 VHEXOUT,DMCB,(R2),PLINE,(R5)                                     
         BAS   RE,PRINTL                                                        
*                                                                               
         LA    R2,CTUDATA-CTUREC(,R2)                                           
DR010    CLI   0(R2),0                                                          
         BE    DR020                                                            
         LLC   R4,1(R2)                                                         
*                                                                               
DR012    LR    R5,R4                                                            
         CHI   R5,L'PLINE/2                                                     
         BNH   *+8                                                              
         LHI   R5,L'PLINE/2                                                     
         GOTO1 VHEXOUT,DMCB,(R2),PLINE,(R5)                                     
         BAS   RE,PRINTL                                                        
DR014    AR    R2,R4                                                            
         B     DR010                                                            
*                                                                               
DR020    MVC   PLINE(2),=C'00'                                                  
         BAS   RE,PRINTL                                                        
         BAS   RE,PRINTL                                                        
*                                                                               
DREX     B     EXITEQ                                                           
         B     EXITNE                                                           
                                                                                
***********************************************************************         
* PRINT ROUTINES                                                                
***********************************************************************         
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
                                                                                
***********************************************************************         
* CONSTANTS & LTORG                                                             
***********************************************************************         
VDATAMGR DC    V(DATAMGR)                                                       
VCARDS   DC    V(CARDS)                                                         
VPERVAL  DC    V(PERVAL)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VHELLO   DC    V(HELLO)                                                         
VDATCON  DC    V(DATCON)                                                        
VQSORT   DC    V(QSORT)                                                         
*                                                                               
DMREAD   DC    CL8'DMREAD '                                                     
DMRDHI   DC    CL8'DMRDHI '                                                     
DMRSEQ   DC    CL8'DMRSEQ '                                                     
DMWRT    DC    CL8'DMWRT  '                                                     
DMADD    DC    CL8'DMADD  '                                                     
CTFILE   DC    CL8'CTFILE '                                                     
SPACES   DC    166C' '                                                          
DASHES   DC    166C'-'                                                          
DOTS     DC    CL16'................'                                           
MAXLINE  DC    P'60'                                                            
*                                                                               
CTENQU   DC    C'N'                                                             
*                                                                               
RCWRITE  DC    C'N'                                                             
RMODE    DC    C'C'                                                             
RSYS     DC    X'00'                                                            
RLIST    DC    X'00'                                                            
*                                                                               
         DC    0D                                                               
         DC    C'**FLST**'                                                      
FLIST    DS    0CL8                                                             
FCTFILE  DC    C'NCTFILE '                                                      
         DC    C'X       '                                                      
*                                                                               
MERROR   DC    CL10'**ERROR***'                                                 
MRECNT   DC    CL40'RECORDS CHANGED TOTAL                  '                    
MERCNT   DC    CL40'RECORDS IN ERROR                       '                    
MDRAFT   DC    CL40'**WRITE=NO, NO RECORDS ACTUALLY CHANGED'                    
MWRITE   DC    CL40'RECORDS IN NUMBER ORDER - READ/WRITE   '                    
MREPORT  DC    CL40'                                       '                    
*                                                                               
H1       DC    C'           AGY   PID         PIDN      NOTE'                   
H2       DC    C'           ---   --------    ----      -------------'          
*                                                                               
         LTORG                                                                  
                                                                                
AGYTAB   DC    20CL2' '            LIST OF AGENCIES TO PROCESS                  
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* DCBS & ADCONS                                                                 
***********************************************************************         
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
         DC    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    F'0',X'0A',XL3'00',XL252'00'                                     
         DC    0D                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
SSB      DC    X'0000'                                                          
         DC    X'FF'                                                            
         DC    AL1(SSOSNRCV)                                                    
         DC    1024X'00'                                                        
*                                                                               
       ++INCLUDE FATABSDEQU                                                     
*                                                                               
         DS    16D                                                              
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
*                                                                               
AIO1     DS    A                   A(IO AREA 1)                                 
AIO2     DS    A                   A(IO AREA 2)                                 
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
DMBITS   DS    X                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
WORK     DS    CL64                                                             
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
COUNT    DS    PL8                                                              
COUNT2   DS    PL8                                                              
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
*                                                                               
ELEM     DS    XL256                                                            
*                                                                               
TODAY    DS    CL3                 YYMMDD PWOS                                  
TODAYC   DS    H                   TODAY COMP                                   
TODAY0   DS    CL6                 TODAY YYMMDD                                 
DATE0    DS    CL6                 DATE YYMMDD                                  
*                                                                               
PGI      DS    X                   PROGRAM INDICATOR                            
PGINEXT  EQU   X'80'               . GET NEXT PROFILE                           
PGIADD   EQU   X'40'               . ADD PROFILE                                
PGIFRST  EQU   X'01'               . FIRST PROFILE READ                         
*                                                                               
CARD     DS    CL80                                                             
KEY      DS    CL40                                                             
         DS    CL40                                                             
*                                                                               
CUAGY    DS    CL2                 AGENCY                                       
CUSYS    DS    CL1                 SYSTEM                                       
CUOFFL   DS    CL1                 OFFICE LIST                                  
CUPASS   DS    CL1                 PROFILE RECORD PASS                          
OFFCOUNT DS    X                                                                
*                                                                               
PROFOF   DS    XL256               MEDIA OFFICE LIST ELEMENT                    
PROF     DS    XL16                PROFILES FOR OFFICE LIST                     
PROFA    DS    XL16                PROFILES FOR PAGE 2 OF OFFICE LIST           
PROFB    DS    XL16                PROFILES FOR PAGE 3 OF OFFICE LIST           
PROFC    DS    XL16                PROFILES FOR PAGE 4 OF OFFICE LIST           
PROFD    DS    XL16                PROFILES FOR PAGE 5 OF OFFICE LIST           
MAXPROFS EQU   5                                                                
*                                                                               
         DS    CL8                                                              
IOAREA1  DS    2048C               IO AREA 1                                    
         DS    CL8                                                              
IOAREA2  DS    2048C               IO AREA 1                                    
*                                                                               
SPARE    DS    1024X                                                            
WORKX    EQU   *                                                                
                                                                                
***********************************************************************         
* PRINT LINE DSECT                                                              
***********************************************************************         
PLINED   DSECT                                                                  
PLHEAD   DS    CL10                                                             
         DS    CL1                                                              
PLAGY    DS    CL2                                                              
         DS    CL4                                                              
PLPID    DS    CL8                                                              
         DS    CL4                                                              
PLPIDN   DS    CL4                                                              
         DS    CL6                                                              
PLMESS   DS    CL40                                                             
         DS    CL40                                                             
                                                                                
***********************************************************************         
* OTHER                                                                         
***********************************************************************         
         DCBD    DSORG=QS,DEVD=DA                                               
*                                                                               
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
*                                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
*                                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
*                                                                               
SSBOFFD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039CTCONPRO  03/20/17'                                      
         END                                                                    
