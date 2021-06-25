*          DATA SET NENAV02A   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NENAV02    AT LEVEL 043 AS OF 05/12/00                      
*          DATA SET NENAV01    AT LEVEL 049 AS OF 03/23/00                      
*PHASE T31802B                                                                  
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31802   TITLE 'NENAV02 - STEWARD/MATCHMAKER - RESEARCH OVERLAY'                
T31802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV02**,R9,RR=R2                                              
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         ST    R2,OVRELO                                                        
* ROUTE THE ELEMENTS TO THE RIGHT ROUTINES                                      
         CLC   SVRCVEL,=X'0070'                                                 
         BE    ROUT020                                                          
         CLC   SVRCVEL,=X'0072'                                                 
         BE    ROUT120                                                          
         CLC   SVRCVEL,=X'0074'                                                 
         BE    ROUT220                                                          
         CLC   SVRCVEL,=X'0076'                                                 
         BE    ROUT320                                                          
         SPACE 3                                                                
*                                                                               
*  TIME PERIOD TREND REPORT                                                     
*                                                                               
ROUT020  BAS   RE,GETTMPTR                                                      
         B     EXIT                                                             
*                                                                               
*  TIME PROGRAM PERIOD TREND                                                    
*                                                                               
ROUT120  BAS   RE,GETPRPTR                                                      
         B     EXIT                                                             
*                                                                               
*  PROGRAM RANKER                                                               
*                                                                               
ROUT220  BAS   RE,GETPROGR                                                      
         B     EXIT                                                             
*                                                                               
*  SHARE/HUT ANALYSIS                                                           
*                                                                               
ROUT320  BAS   RE,GETSHRHT                                                      
         B     EXIT                                                             
         SPACE 3                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  ROUTINE BUILDS DBLOCK CALLS DEMAND PASSES DATA TO THE                        
*  PC THAT USES IT TO CREATE A TIME PERIOD TREND REPORT                         
*                                                                               
GETTMPTR NTR1                                                                   
         GOTO1 VDATCON,DMCB,(2,SVSDATE),(0,BOOKEDAT)                            
         BAS   RE,GETMONDY                                                      
         GOTO1 VDATCON,DMCB,(0,BOOKEDAT),(2,BOOKDAT)                            
         MVC   SVSDATE,BOOKDAT                                                  
GTTPT100 BAS   RE,BLDBLOCK          CREATE DBLOCK                               
         GOTO1 VDEMAND,DMCB,BLOCK,FILL                                          
*  GET THE NEXT WEEK                                                            
         LA    R0,7                                                             
         GOTO1 VADDAY,DMCB,BOOKEDAT,BOOKEDAT,(R0)      BUMP A WEEK              
         GOTO1 VDATCON,DMCB,(0,BOOKEDAT),(2,BOOKDAT)   MAKE COMPRESSED          
*  CHECK IF WEEK FITS IN DATE FILTER                                            
         CLC   BOOKDAT,SVEDATE      PASS END DATE                               
         BH    GTTPTEX                                                          
         B     GTTPT100                                                         
*                                                                               
GTTPTEX  B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*  ROUTINE BUILDS DBLOCK CALLS DEMAND PASSES DATA TO THE                        
*  PC THAT USES IT TO CREATE A PROGRAM PERIOD TREND REPORT                      
*                                                                               
GETPRPTR NTR1                                                                   
         GOTO1 VDATCON,DMCB,(2,SVSDATE),(0,BOOKEDAT)                            
         BAS   RE,GETMONDY                                                      
         GOTO1 VDATCON,DMCB,(0,BOOKEDAT),(2,BOOKDAT)                            
         MVC   SVSDATE,BOOKDAT                                                  
GTPRT100 BAS   RE,BLDBLOCK          CREATE DBLOCK                               
         GOTO1 VDEMAND,DMCB,BLOCK,FILL                                          
*  GET THE NEXT WEEK                                                            
         LA    R0,7                                                             
         GOTO1 VADDAY,DMCB,BOOKEDAT,BOOKEDAT,(R0)      BUMP A WEEK              
         GOTO1 VDATCON,DMCB,(0,BOOKEDAT),(2,BOOKDAT)   MAKE COMPRESSED          
*  CHECK IF WEEK FITS IN DATE FILTER                                            
         CLC   BOOKDAT,SVEDATE      PASS END DATE                               
         BH    GTPRTEX                                                          
         B     GTPRT100                                                         
*                                                                               
GTPRTEX  B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*  ROUTINE BUILDS DBLOCK CALLS DEMAND PASSES DATA TO THE                        
*  PC THAT USES IT TO CREATE A PROGRAM RANKER REPORT                            
*                                                                               
GETPROGR NTR1                                                                   
         GOTO1 VDATCON,DMCB,(2,SVSDATE),(0,BOOKEDAT)                            
         BAS   RE,GETMONDY                                                      
         GOTO1 VDATCON,DMCB,(0,BOOKEDAT),(2,BOOKDAT)                            
         MVC   SVSDATE,BOOKDAT                                                  
GTPGR100 BAS   RE,BLDBLOCK          CREATE DBLOCK                               
         GOTO1 VDEMAND,DMCB,BLOCK,FILL                                          
*  GET THE NEXT WEEK                                                            
         LA    R0,7                                                             
         GOTO1 VADDAY,DMCB,BOOKEDAT,BOOKEDAT,(R0)      BUMP A WEEK              
         GOTO1 VDATCON,DMCB,(0,BOOKEDAT),(2,BOOKDAT)   MAKE COMPRESSED          
*  CHECK IF WEEK FITS IN DATE FILTER                                            
         CLC   BOOKDAT,SVEDATE      PASS END DATE                               
         BH    GTPGREX                                                          
         B     GTPGR100                                                         
*                                                                               
GTPGREX  B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*  ROUTINE BUILDS DBLOCK CALLS DEMAND PASSES DATA TO THE                        
*  PC THAT USES IT TO CREATE A SAHRE/HUT REPORT                                 
*                                                                               
GETSHRHT NTR1                                                                   
         XC    SVDEMOS,SVDEMOS                                                  
         GOTO1 VDATCON,DMCB,(2,SVSDATE),(0,BOOKEDAT)                            
         BAS   RE,GETMONDY                                                      
         GOTO1 VDATCON,DMCB,(0,BOOKEDAT),(2,BOOKDAT)                            
         MVC   SVSDATE,BOOKDAT                                                  
GTSHT100 MVC   SVDEMOS(4),=XL4'00E201FF'                                        
         BAS   RE,BLDBLOCK          CREATE DBLOCK                               
         GOTO1 VDEMAND,DMCB,BLOCK,FILL   GET SHARE VALUE                        
*                                                                               
         BAS   RE,BLDBLOCK          CREATE DBLOCK                               
         MVC   SVDEMOS(4),=XL4'00D901FF'                                        
         LA    RE,BLOCK                                                         
         USING DBLOCK,RE                                                        
         MVC   DBSELSTA(5),=CL5'HUT T'                                          
         DROP  RE                                                               
         GOTO1 VDEMAND,DMCB,BLOCK,FILL   GET HUT VALUE                          
*  GET THE NEXT WEEK                                                            
         LA    R0,7                                                             
         GOTO1 VADDAY,DMCB,BOOKEDAT,BOOKEDAT,(R0)      BUMP A WEEK              
         GOTO1 VDATCON,DMCB,(0,BOOKEDAT),(2,BOOKDAT)   MAKE COMPRESSED          
*  CHECK IF WEEK FITS IN DATE FILTER                                            
         CLC   BOOKDAT,SVEDATE      PASS END DATE                               
         BH    GTSHTEX                                                          
         B     GTSHT100                                                         
*                                                                               
GTSHTEX  B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*  ROUTINE FINDS FIRST MONDAY FROM INPUT DATE                                   
*                                                                               
GETMONDY NTR1                                                                   
         GOTO1 VGETDAY,DMCB,BOOKEDAT,WORK                                       
         ZIC   R0,DMCB             (END OF QUARTER DAY IN R0)                   
         LA    R1,1                (DEFAULT IS MONDAY)                          
         SR    R1,R0                                                            
         BZ    GETMONEX                                                         
         BM    *+8                                                              
         SH    R1,=H'7'                                                         
*                                                                               
         ST    R1,DMCB+8                                                        
         GOTO1 VADDAY,DMCB,BOOKEDAT,WORK                                        
         MVC   BOOKEDAT(6),WORK                                                 
GETMONEX B     EXIT                                                             
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
*  ROUTINE BUILDS THE DBLOCK FOR THE VARIOUS RESEARCH REQUEST                   
*                                                                               
BLDBLOCK NTR1                                                                   
*                                                                               
         XC    BLOCK,BLOCK           DEMOS EXIST ...                            
         LA    R6,BLOCK                                                         
         USING DBLOCK,R6             SET UP CALL TO DEMOCON                     
         MVC   DBAREC,AIO3                                                      
         MVC   DBSELAGY,QAGY                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELSTA(4),SVNET                                                
         MVC   DBSELSTA+4(1),SVSOURC                                            
         CLI   DBSELSTA+4,C'N'                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
*                                                                               
         CLC   SVRCVEL,=X'0070'     CHECK TIME PERIOD TREND REPORT              
         BE    BLDDB050                                                         
         CLC   SVRCVEL,=X'0074'     CHECK PROGRAM RANKER REPORT                 
         BE    BLDDB050                                                         
         CLC   SVRCVEL,=X'0076'     CHECK SHARE/HUT REPORT                      
         BNE   BLDDB100                                                         
BLDDB050 MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBSELMED,C'N'                                                    
         CLI   SVSOURC,C'H'                                                     
         BNE   *+8                                                              
         MVI   DBSELMED,C'W'                                                    
         MVC   DBSELTIM,SVTIME                                                  
         MVC   DBSELDAY,SVDAYDEM                                                
*                                                                               
BLDDB100 CLC   SVRCVEL,=X'0072'     CHECK PROGRAM PERIOD TREND REPORT           
         BNE   BLDDB400                                                         
         MVI   DBFUNCT,DBGETNTI                                                 
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBSELMED,C'N'                                                    
         CLI   SVSOURC,C'H'                                                     
         BNE   *+8                                                              
         MVI   DBSELMED,C'W'                                                    
         MVC   DBSELPRG,SVNTI                                                   
*                                                                               
BLDDB400 GOTO1 VNETWEEK,DMCB,BOOKEDAT,VGETDAY,VADDAY                            
         MVC   DBSELBK+1(1),DMCB       SAVE WEEK                                
         MVC   DBSELBK(1),DMCB+4       SAVE YEAR                                
*                                                                               
*        GOTO1 VDATCON,DMCB,(2,BOOKDAT),(3,DUB)                                 
*        MVC   DBSELBK(1),DUB        SAVE YEAR                                  
         B     EXIT                                                             
         EJECT                                                                  
*              PROCESS A RECORD                                                 
*                                                                               
FILL     NTR1                                                                   
***************                                                                 
         CLC   SVRCVEL,=X'0070'      TIME PERIOD TREND                          
         BNE   FILL20                                                           
         LHI   R1,X'71'                                                         
         BAS   RE,SENDH                                                         
         BAS   RE,GETWEEK                                                       
         BAS   RE,GETPROG                                                       
         BAS   RE,GETDEMS                                                       
         B     FILLEX                                                           
*                                                                               
FILL20   CLC   SVRCVEL,=X'0072'      PROGRAM PERIOD TREND                       
         BNE   FILL30                                                           
         LHI   R1,X'73'                                                         
         BAS   RE,SENDH                                                         
         BAS   RE,GETWEEK                                                       
         BAS   RE,GETDAY                                                        
         BAS   RE,GETTIMES                                                      
         BAS   RE,GETDEMS                                                       
*                                                                               
FILL30   CLC   SVRCVEL,=X'0074'      PROGRAM RANKER                             
         BNE   FILL40                                                           
         LHI   R1,X'75'                                                         
         BAS   RE,SENDH                                                         
         BAS   RE,GETNET                                                        
         BAS   RE,GETDAY                                                        
         BAS   RE,GETTIMES                                                      
         BAS   RE,GETPROG                                                       
         BAS   RE,GETDEMS                                                       
*                                                                               
FILL40   CLC   SVRCVEL,=X'0076'      SHARE HUT                                  
         BNE   FILLEX                                                           
         LHI   R1,X'77'                                                         
         BAS   RE,SENDH                                                         
         BAS   RE,GETWEEK                                                       
         BAS   RE,GETTIMES                                                      
         BAS   RE,GETDEMS                                                       
*                                                                               
FILLEX   B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*  COMMON DEMO EXTRACT ROUTINES CALLED FROM DEMAND HOOK                         
*                                                                               
         SPACE 3                                                                
*  PASS NET TO FALINK                                                           
GETNET   NTR1                                                                   
         LA    R4,SVNET             PASS THE NETWORK                            
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
*  PASS WEEK TO FALINK                                                          
GETWEEK  NTR1                                                                   
         LA    R4,BOOKDAT           PASS THE WEEK                               
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
*  PASS PROGRAM TO FALINK                                                       
GETPROG  NTR1                                                                   
         GOTO1 VDEFINE,DMCB,=C'PROGRAM',BLOCK,WORK                              
         OC    WORK(16),SPACES                                                  
         LA    R4,WORK                                                          
         LHI   R1,X'05'                                                         
         BAS   RE,SENDD             PASS THE PROGRAM NAME                       
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
*  PASS DAYS TO FALINK                                                          
GETDAY   NTR1                                                                   
         MVC   WORK(10),SPACES                                                  
         GOTO1 VDEFINE,DMCB,=C'DAY',BLOCK,WORK                                  
         LA    R4,WORK+2                                                        
         LHI   R1,X'03'                                                         
         BAS   RE,SENDD             PASS THE DAY                                
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
*  PASS TIMES TO FALINK                                                         
GETTIMES NTR1                                                                   
         XC    WORK,WORK                                                        
         GOTO1 VDEFINE,DMCB,=C'TIME',BLOCK,WORK                                 
         GOTO1 VUNTIME,DMCB,WORK+2,WORK+20                                      
         LA    R4,WORK+20                                                       
         LHI   R1,X'04'                                                         
         BAS   RE,SENDD             PASS THE TIME                               
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
*  PASS DEMOS TO FALINK                                                         
GETDEMS  NTR1                                                                   
         LA    R2,SVDEMOS                                                       
         CLI   DBSELSTA+4,C'C'                                                  
         BNE   GTDM100                                                          
         LA    RE,DBEXTRA          FORCE 2 CHAR RATINGS                         
         USING DBXNTID,RE                                                       
         ST    RE,DBEXTEND                                                      
         MVI   DBXNCR2,C'Y'                                                     
         MVC   DBXNID,=C'NETW'                                                  
GTDM100  GOTO1 VDEMOUT,DMCB,(C'L',(R2)),BLOCK,WORK2                             
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,SVDEMOS                                                       
         LA    R4,WORK2                                                         
* IF SHARE/HUT REPORT AND WE ARE LOOKING UP HUTS GO TO STDM150                  
         CLC   SVDEMOS(3),=X'00D901'                                            
         BNE   GTDM140                                                          
         CLC   SVRCVEL,=X'0076'                                                 
         BE    GTDM150                                                          
*                                                                               
*  PASS THE DEMO VALUES OUT                                                     
*                                                                               
GTDM140  CLI   0(R3),X'FF'                                                      
         BE    GTDMEX                                                           
         LHI   R1,X'06'                                                         
         BAS   RE,SENDD                                                         
         LA    R3,3(R3)                                                         
         LA    R4,4(R4)                                                         
         B     GTDM140                                                          
*                                                                               
*  PASS THE HUT VALUE OUT                                                       
*                                                                               
GTDM150  CLI   0(R3),X'FF'                                                      
         BE    GTDMEX                                                           
         LHI   R1,X'07'                                                         
         BAS   RE,SENDD                                                         
         LA    R3,3(R3)                                                         
         LA    R4,4(R4)                                                         
         B     GTDM150                                                          
*                                                                               
GTDMEX   B     EXIT                                                             
                                                                                
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
         EJECT                                                                  
SPTFILE  DC    CL8'SPTFILE'                                                     
UNTFILE  DC    CL8'UNTFILE'                                                     
         LTORG                                                                  
*                                                                               
       ++INCLUDE DENADCATS                                                      
         EJECT                                                                  
       ++INCLUDE NENAVWRKA                                                      
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
BOOKDAT  DS    CL2                  CURRENT LOOKUP DATE                         
BOOKEDAT DS    CL6                  CURRENT LOOKUP DATE EBCDIC                  
DBEXTRA  DS    CL128                                                            
*                                                                               
         ORG                                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENREAS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NENAV02A  05/01/02'                                      
         END                                                                    
