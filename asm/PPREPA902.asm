*          DATA SET PPREPA902  AT LEVEL 011 AS OF 07/17/18                      
*PHASE PPA902A                                                                  
*INCLUDE PPBVAL                                                                 
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*  SMUR  SPEC-17729  NEW MEDIA D FOR DIGITAL AUDIO (18.3)                       
*                                                                               
*  BPLA  06/15  CHANGES FOE NEW MEDIA B,V,W ALLOW FOR PLANNED COST              
*                                                                               
*  BPLA  03/14  CHANGES FOR MEDIA L - ALLOW FOR PLANNED COST                    
*                                                                               
*  BPLA  04/09  PROCESS BOTH INSERTION AND BILLABLE MONTH ORDERED $             
*               QOPT4 = B (FOR BOTH)                                            
*                                                                               
*  BPLA  09/08  OPTION (QOPT4) TO USE INSERTION DATE BUCKETS                    
*               OPTION (QOPT5) TO REPORT PLANNED COST (MEDIA I ONLY)            
*                                                                               
*  BPLA  08/07  SET GROSS/NET INDICATOR IN FILE HEADER                          
*                                                                               
*  BPLA  02/06  IGNORE BILLS WITH ZERO $                                        
*               ALLOWS FOR BILLS WITH MOS OUTSIDE OF NORMAL RANGE               
*                                                                               
*===================================================================            
* QOPT1 =T INCLUDE TEST ESTIMATES                                               
* QOPT2 =G GROSS                                                                
*        N NET                                                                  
*        1 GROSS-CD                                                             
*        2 NET-CD                                                               
* QOPT3  P PRODUCT TOTALS (NO ESTIMATE DETAILS)                                 
*        C CLIENT TOTALS (NO PRODUCT TOTALS)                                    
*        M MEDIA TOTALS (NO CLIENT TOTALS)                                      
* QOPT4  I USE INSERTION DATE BUCKETS                                           
* QOPT4  B PROCESS BOTH INSERTION DATE AND BILLABLE DATE BUCKETS                
*                                                                               
* QOPT5  P REPORT PLANNED COST (FOR MEDIA I)                                    
*                                                                               
*===================================================================            
         TITLE 'PPA902 - CREATE AGENCY SUMMARY DATA FOR ACCENT'                 
PPA902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPA902                                                         
         L     RC,=A(PPA9WORK)                                                  
         USING PPA9WORK,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'                                                    
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
         L     R8,PPFILEC                                                       
         USING PPFILED,R8                                                       
*                                                                               
         CLI   MODE,FESTCLI                                                     
         BNE   DS1                                                              
         BRAS  RE,CLTF                                                          
         J     EXIT                                                             
*                                                                               
DS1      CLI   MODE,LESTCLI                                                     
         BNE   DS2                                                              
         BRAS  RE,CLTL                                                          
         J     EXIT                                                             
*                                                                               
DS2      CLI   MODE,FESTPRO                                                     
         BNE   DS3                                                              
         BRAS  RE,PRDF                                                          
         J     EXIT                                                             
*                                                                               
DS3      CLI   MODE,LESTPRO                                                     
         BNE   DS4                                                              
         BRAS  RE,PRDL                                                          
         J     EXIT                                                             
*                                                                               
DS4      CLI   MODE,PROCEST                                                     
         BNE   DS6                                                              
         BRAS  RE,PROCESS                                                       
         J     EXIT                                                             
*                                                                               
DS6      CLI   MODE,REQFRST                                                     
         BNE   DS7                                                              
         BRAS  RE,REQF                                                          
         J     EXIT                                                             
*                                                                               
DS7      CLI   MODE,REQLAST                                                     
         BNE   *+12                                                             
         BRAS  RE,REQL                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         J     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*=============================================================                  
* OPEN A SEQUENTIAL DISK OUTPUT FILE                                            
*=============================================================                  
         SPACE 1                                                                
RUNF     DS    0H                                                               
         RELOC RELO                                                             
*                                                                               
         L     R0,=V(PPBVAL)                                                    
         A     R0,RELO                                                          
         ST    R0,VPPBVAL                                                       
*                                                                               
*        GET A(OFFICER)                                                         
*                                                                               
         XC    DMCB(12),DMCB                                                    
*                                                                               
         MVC   DUB,SPACES          GET OFFICER                                  
         MVC   DUB(6),=C'T00A38'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         MVC   VOFFICER,4(R1)      SAVE ADDRESS                                 
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ZAP   RUNORD,=P'0'                                                     
         ZAP   RUNPD,=P'0'                                                      
         ZAP   RUNBL,=P'0'                                                      
         ZAP   RUNCMB,=P'0'       CURRENT MONTH BILLED                          
         ZAP   RUNPDT,=P'0'       NET PAID TODAY                                
         ZAP   RUNRECS,=P'0'                                                    
*                                                                               
         MVC   TODAY1+0(2),RCDATE+6      YR                                     
         MVC   TODAY1+2(2),RCDATE+0      MO                                     
         MVC   TODAY1+4(2),RCDATE+3      DA                                     
*                                                                               
         GOTO1 DATCON,DMCB,TODAY1,(3,BTODAY)                                    
         J     EXIT                                                             
         EJECT                                                                  
*=================================================================              
* AT RUNLAST, CLOSE FILE                                                        
*=================================================================              
                                                                                
RUNL     CLOSE FILEOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P1+1(2),PCLTKAGY                                                 
         MVC   P1+6(3),=C'***'                                                  
         LA    R1,RUNORD                                                        
         BAS   RE,MYEDIT                                                        
         MVC   P1+24(18),WORK                                                   
         LA    R1,RUNPD                                                         
         BAS   RE,MYEDIT                                                        
         MVC   P1+45(18),WORK                                                   
         LA    R1,RUNBL                                                         
         BAS   RE,MYEDIT                                                        
         MVC   P1+66(18),WORK                                                   
         LA    R1,RUNCMB                                                        
         BAS   RE,MYEDIT                                                        
         MVC   P1+87(18),WORK                                                   
         LA    R1,RUNPDT                                                        
         BAS   RE,MYEDIT                                                        
         MVC   P1+108(18),WORK                                                  
         MVC   P1+42(4),=C'****'                                                
         MVC   P1+63(4),=C'****'                                                
         MVC   P1+84(4),=C'****'                                                
         MVC   P1+105(4),=C'****'                                               
         MVC   P1+126(4),=C'****'                                               
*                                                                               
         CLI   SVQOPT3,C'P'                                                     
         BNE   RUNL2                                                            
         MVI   P1+45,C' '      ONLY THREE *                                     
         MVI   P1+66,C' '      ONLY THREE *                                     
         MVI   P1+87,C' '      ONLY THREE *                                     
         MVI   P1+108,C' '      ONLY THREE *                                    
         MVI   P1+129,C' '      ONLY THREE *                                    
         B     RUNL5                                                            
*                                                                               
RUNL2    CLI   SVQOPT3,C'C'                                                     
         BNE   RUNL4                                                            
         MVC   P1+44(2),SPACES    TWO STARS                                     
         MVC   P1+65(2),SPACES    TWO STARS                                     
         MVC   P1+86(2),SPACES    TWO STARS                                     
         MVC   P1+107(2),SPACES    TWO STARS                                    
         MVC   P1+128(2),SPACES   TWO STARS                                     
         B     RUNL5                                                            
*                                                                               
RUNL4    CLI   SVQOPT3,C'M'                                                     
         BNE   RUNL5                                                            
         MVC   P1+43(3),SPACES    ONE STAR                                      
         MVC   P1+64(3),SPACES    ONE STAR                                      
         MVC   P1+85(3),SPACES    ONE STAR                                      
         MVC   P1+106(3),SPACES    ONE STAR                                     
         MVC   P1+127(3),SPACES   ONE STAR                                      
*                                                                               
RUNL5    DS    0H                                                               
         MVC   HEAD4+45(L'MYHEAD),MYHEAD                                        
         MVC   HEAD5(L'MYHEAD5),MYHEAD5                                         
         MVC   HEAD6(L'MYHEAD6),MYHEAD6                                         
         MVC   HEAD7(L'MYHEAD7),MYHEAD7                                         
         GOTO1 REPORT                                                           
         MVC   P1+2(13),=C'TOTAL RECORDS'                                       
         EDIT  RUNRECS,(11,P1+20),0,COMMAS=YES                                  
         MVC   HEAD4+45(L'MYHEAD),MYHEAD                                        
         MVC   HEAD5(L'MYHEAD5),MYHEAD5                                         
         MVC   HEAD6(L'MYHEAD6),MYHEAD6                                         
         MVC   HEAD7(L'MYHEAD7),MYHEAD7                                         
         GOTO1 REPORT                                                           
*                                                                               
         J     EXIT                                                             
*                                                                               
MYEDIT   DS    0H                                                               
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         ZAP   DUB,0(8,R1)                                                      
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK,C'-'                                                        
         BR    RE               RETURN                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* REQUEST FIRST PROCESSING                                            *         
*===============================================================                
                                                                                
REQF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   REQSW,C'N'          REQUEST ACTIVITY SWITCH                      
         MVI   FORCEHED,C'Y'                                                    
*        MVC   PAGE,=H'1'          DON'T RESET FOR EACH REQUEST                 
*                                  AS IT'S USUALLY RUN ACROSS MEDIA             
         MVC   SVQOPT3,QOPT3       SAVE FOR RUNLAST                             
         MVC   SVQOPT4,QOPT4       SAVE ORIGINAL QOPT4                          
         MVC   SVRUNTYP,QRECORD    SAVE RUN TYPE                                
*                                                                               
         MVC   FILEDATE,TODAY1     CREATE HEADER FOR OUTPUT FILE                
         MVC   FILEQSTR,QSTART                                                  
         MVC   FILEQEND,QEND                                                    
*                                                                               
         MVI   FILEDOLS,C'G'       ASSUME GROSS                                 
         CLI   QOPT2,C' '          IF OPTION 2 NOT ENTERED                      
         BE    *+10                                                             
         MVC   FILEDOLS,QOPT2                                                   
*                                                                               
         MVI   FILEMTYP,C' '       DEFAULT TO BILLABLE MTHS                     
         CLI   QOPT4,C' '          IF OPTION 4 NOT ENTERED                      
         BE    *+10                                                             
         MVC   FILEMTYP,QOPT4      I= INSERTION MTHS                            
*                                  B= BOTH INSERTION AND BILLABLE               
*                                  BLANK = BILLABLE ONLY                        
*                                                                               
         CLI   SVQOPT4,C'B'        AM I TO DO BOTH INS. AND BILLABLE?           
         BNE   *+8                                                              
         MVI   QOPT4,C' '          FIRST DO BILLABLE                            
*                                                                               
         MVI   FILEDTYP,C' '       DEFAULT TO NORMAL $                          
         CLI   QOPT5,C' '          IF OPTION 5 NOT ENTERED                      
         BE    *+10                                                             
         MVC   FILEDTYP,QOPT5      P=PLANNED COST                               
*                                                                               
         LA    R0,PBUYREC                                                       
         LHI   R1,OUTRECLN         FIXED OUTPUT LENGTH                          
         LA    RE,FILEHDR                                                       
         LHI   RF,FILEHDRX-FILEHDR                                              
         MVCL  R0,RE                                                            
         LA    R0,PBUYREC                                                       
         PUT   FILEOUT,(0)                                                      
*                                                                               
         ZAP   REQORD,=P'0'                                                     
         ZAP   REQPD,=P'0'                                                      
         ZAP   REQBL,=P'0'                                                      
         ZAP   REQCMB,=P'0'       CURRENT MONTH BILLED                          
         ZAP   REQPDT,=P'0'       NET PAID TODAY                                
*                                                                               
         MVC   MYHEAD,SPACES                                                    
         MVC   MYHEAD5,SPACES                                                   
         MVC   MYHEAD6,SPACES                                                   
         MVC   MYHEAD7,SPACES                                                   
*                                                                               
         MVC   MYHEAD(3),=C'NET'                                                
         CLI   QOPT2,C'N'                                                       
         BE    REQF5                                                            
         MVC   MYHEAD(5),=C'GROSS'                                              
         CLI   QOPT2,C'G'                                                       
         BE    REQF5                                                            
         CLI   QOPT2,C' '   DEFAULT                                             
         BE    REQF5                                                            
         MVC   MYHEAD(6),=C'NET-CD'                                             
         CLI   QOPT2,C'2'                                                       
         BE    REQF5                                                            
         MVC   MYHEAD(8),=C'GROSS-CD'                                           
         CLI   QOPT2,C'1'                                                       
         BE    REQF5                                                            
         DC    H'0'       BAD QOPT2                                             
REQF5    DS    0H                                                               
         LA    R1,MYHEAD5                                                       
         CLI   QOPT1,C'T'  INCLUDE TEST ESTIMATES                               
         BNE   REQF7                                                            
         MVC   0(23,R1),=C'TEST ESTIMATES INCLUDED'                             
         LA    R1,L'MYHEAD5(R1)                                                 
REQF7    CLI   QOPT4,C'I'  USING INSERTION MONTH BUCKETS?                       
         BNE   REQF9                                                            
         MVC   0(31,R1),=C'ORDERED/PAID BY INSERTION MONTH'                     
         LA    R1,L'MYHEAD5(R1)                                                 
REQF9    CLI   QOPT5,C'P'   PLANNED COST REPORT?                                
         BNE   REQFX                                                            
         MVC   0(22,R1),=C'REPORTING PLANNED COST'                              
REQFX    J     EXIT                                                             
         EJECT                                                                  
***********************************                                             
*        LAST ESTIMATE FOR REQUEST  (MEDIA)                                     
***********************************                                             
REQL     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   QOPT3,SVQOPT3       MUST RESTORE QOPT3 AND 4                     
         MVC   QOPT4,SVQOPT4       THEY MAY HAVE BEEN ALTERED                   
*                                  AND NOT RESTORED                             
         CLI   REQSW,C'Y'                                                       
         BNE   REQLX                                                            
         MVC   P1+1(2),PCLTKAGY                                                 
         MVC   P1+7(1),PCLTKMED                                                 
         MVC   P1+11(3),=C'***'                                                 
*                                                                               
         LA    R1,REQORD                                                        
         BAS   RE,MYQEDIT                                                       
         MVC   P1+24(18),WORK                                                   
         LA    R1,REQPD                                                         
         BAS   RE,MYQEDIT                                                       
         MVC   P1+45(18),WORK                                                   
         LA    R1,REQBL                                                         
         BAS   RE,MYQEDIT                                                       
         MVC   P1+66(18),WORK                                                   
         LA    R1,REQCMB                                                        
         BAS   RE,MYQEDIT                                                       
         MVC   P1+87(18),WORK                                                   
         LA    R1,REQPDT                                                        
         BAS   RE,MYQEDIT                                                       
         MVC   P1+108(18),WORK                                                  
*                                                                               
         MVC   P1+42(3),=C'***'                                                 
         MVC   P1+63(3),=C'***'                                                 
         MVC   P1+84(3),=C'***'                                                 
         MVC   P1+105(3),=C'***'                                                
         MVC   P1+126(3),=C'***'                                                
*                                                                               
         CLI   QOPT3,C'P'                                                       
         BNE   REQL2                                                            
         MVI   P1+44,C' '      ONLY TWO *                                       
         MVI   P1+65,C' '      ONLY TWO *                                       
         MVI   P1+86,C' '      ONLY TWO *                                       
         MVI   P1+107,C' '      ONLY TWO *                                      
         MVI   P1+128,C' '      ONLY TWO *                                      
         B     REQL5                                                            
*                                                                               
REQL2    CLI   QOPT3,C'C'                                                       
         BNE   REQL4                                                            
         MVC   P1+43(2),SPACES    ONE STAR                                      
         MVC   P1+64(2),SPACES    ONE STAR                                      
         MVC   P1+85(2),SPACES    ONE STAR                                      
         MVC   P1+106(2),SPACES    ONE STAR                                     
         MVC   P1+127(2),SPACES   ONE STAR                                      
         B     REQL5                                                            
*                                                                               
REQL4    CLI   QOPT3,C'M'                                                       
         BNE   REQL5                                                            
         MVC   P1+42(3),SPACES    NO STARS                                      
         MVC   P1+63(3),SPACES    NO STARS                                      
         MVC   P1+84(3),SPACES    NO STARS                                      
         MVC   P1+105(3),SPACES    NO STARS                                     
         MVC   P1+126(3),SPACES   NO STARS                                      
*                                                                               
REQL5    MVC   HEAD4+45(L'MYHEAD),MYHEAD                                        
         MVC   HEAD5(L'MYHEAD5),MYHEAD5                                         
         MVC   HEAD6(L'MYHEAD6),MYHEAD6                                         
         MVC   HEAD7(L'MYHEAD7),MYHEAD7                                         
         GOTO1 REPORT                                                           
         AP    RUNORD,REQORD    ROLL TO RUN (AGENCY) TOTALS                     
         AP    RUNPD,REQPD                                                      
         AP    RUNBL,REQBL                                                      
         AP    RUNCMB,REQCMB                                                    
         AP    RUNPDT,REQPDT                                                    
*                                                                               
REQLX    J     EXIT                                                             
*                                                                               
MYQEDIT  DS    0H                                                               
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         ZAP   DUB,0(8,R1)                                                      
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK,C'-'                                                        
         BR    RE               RETURN                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************                                              
*        FIRST ESTIMATE FOR CLIENT                                              
**********************************                                              
CLTF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    B2BPROF,B2BPROF                                                  
         XC    WORK,WORK           CLEAR WORK AREA                              
         MVI   WORK,C'P'           SET SYSTEM                                   
         NI    WORK,X'FF'-X'40'    FORCE LOWERCASE TO INDICATE                  
*                                     PROF ID IS 3 LONG                         
         MVC   WORK+1(3),=C'B2B'   B2B PROFILE FOR PLANNED COSTS                
         MVC   WORK+4(3),PCLTKEY   AGENCY/MEDIA                                 
         MVC   WORK+7(3),PCLTKCLT  CLIENT                                       
*                                                                               
         CLI   PCLTOFF,C' '        SKIP IF NO CLIENT OFFICE                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF  CLIENT OFFICE                                
*                                                                               
         GOTO1 GETPROF,DMCB,(X'C0',WORK),B2BPROF,DATAMGR,0,0,0                  
*                                                                               
         MVC   SAVCOFF,SPACES                                                   
         MVC   SAVCOFF(1),PCLTOFF    SAVE OFFICE FOR HEADLINES                  
*                                                                               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'P'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,QAGENCY                                              
         MVC   OFFD.OFCPMED,QMEDIA                                              
         MVC   OFFD.OFCOFC,PCLTOFF                                              
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',WORK),(0,VCOMFACS)                           
         CLI   0(R1),0                                                          
         BNE   CLTF10                                                           
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
*                                                                               
CLTF10   DS    0H                                                               
         CLC   SAVCOFF,=X'0000'   IF STILL ZEROS, MAKE SPACES                   
         BNE   *+10                                                             
         OC    SAVCOFF,SPACES    NEEDED FOR CLIENTS WITHOUT AN OFFICE           
*                                                                               
         MVC   THISAGY,PCLTKAGY                                                 
         MVC   THISMED,PCLTKMED                                                 
         MVC   THISAGMD,PCLTKMED                                                
         MVC   THISCLT,PCLTKCLT                                                 
*                                                                               
         MVC   THISMDNM,SPACES                                                  
         MVC   THISMDNM(L'PAGYMED),PAGYMED                                      
*                                                                               
         MVC   THISMDOF(2),SAVCOFF                                              
         MVC   THISACOF,PCLTAOFC                                                
         XC    THISCACC,THISCACC    NO LIMIT ACCESS FOR PRINT                   
         MVC   THISCLNM,SPACES                                                  
         MVC   THISCLNM(L'PCLTNAME),PCLTNAME                                    
         OC    THISCLNM,SPACES                                                  
         ZAP   CLTORD,=P'0'                                                     
         ZAP   CLTPD,=P'0'                                                      
         ZAP   CLTBL,=P'0'                                                      
         ZAP   CLTCMB,=P'0'       CURRENT MONTH BILLED                          
         ZAP   CLTPDT,=P'0'       NET PAID TODAY                                
         MVI   CLTSW,C'N'                                                       
*                                                                               
CLTFX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************                                             
*        LAST ESTIMATE FOR CLIENT                                               
***********************************                                             
CLTL     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   CLTSW,C'Y'         CLIENT ACTIVITY?                              
         BNE   CLTLX                                                            
         CLI   QOPT3,C'M'     MEDIA TOTALS                                      
         BE    CLTL5          SKIP CLIENT TOTALS                                
*                                                                               
         MVC   P1+1(2),PCLTKAGY                                                 
         MVC   P1+7(1),PCLTKMED                                                 
         MVC   P1+11(3),PCLTKCLT                                                
         MVC   P1+16(3),=C'***'                                                 
         EDIT  CLTORD,(16,P1+26),2,COMMAS=YES,FLOAT=-                           
         EDIT  CLTPD,(16,P1+47),2,COMMAS=YES,FLOAT=-                            
         EDIT  CLTBL,(16,P1+68),2,COMMAS=YES,FLOAT=-                            
         EDIT  CLTCMB,(16,P1+89),2,COMMAS=YES,FLOAT=-                           
         EDIT  CLTPDT,(16,P1+110),2,COMMAS=YES,FLOAT=-                          
         MVC   P1+42(2),=C'**'                                                  
         MVC   P1+63(2),=C'**'                                                  
         MVC   P1+84(2),=C'**'                                                  
         MVC   P1+105(2),=C'**'                                                 
         MVC   P1+126(2),=C'**'                                                 
*                                                                               
         CLI   QOPT3,C'P'                                                       
         BNE   CLTL3                                                            
         MVI   P1+43,C' '      ONLY ONE *                                       
         MVI   P1+64,C' '      ONLY ONE *                                       
         MVI   P1+85,C' '      ONLY ONE *                                       
         MVI   P1+106,C' '      ONLY ONE *                                      
         MVI   P1+127,C' '      ONLY ONE *                                      
         B     CLTL4                                                            
*                                                                               
CLTL3    CLI   QOPT3,C'C'                                                       
         BNE   CLTL4                                                            
         MVC   P1+42(2),SPACES    NO STARS                                      
         MVC   P1+63(2),SPACES    NO STARS                                      
         MVC   P1+84(2),SPACES    NO STARS                                      
         MVC   P1+105(2),SPACES    NO STARS                                     
         MVC   P1+126(2),SPACES    NO STARS                                     
*                                                                               
CLTL4    MVC   HEAD4+45(L'MYHEAD),MYHEAD                                        
         MVC   HEAD5(L'MYHEAD5),MYHEAD5                                         
         MVC   HEAD6(L'MYHEAD6),MYHEAD6                                         
         MVC   HEAD7(L'MYHEAD7),MYHEAD7                                         
         GOTO1 REPORT                                                           
*                                                                               
CLTL5    DS    0H                                                               
*                                                                               
         AP    REQORD,CLTORD   ROLL TO REQUEST (MEDIA) TOTALS                   
         AP    REQPD,CLTPD                                                      
         AP    REQBL,CLTBL                                                      
         AP    REQCMB,CLTCMB                                                    
         AP    REQPDT,CLTPDT                                                    
*                                                                               
CLTLX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************                                             
*        FIRST ESTIMATE FOR PRODUCT                                             
***********************************                                             
PRDF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVPRDEFD,SVPRDEFD   CLEAR PLANNED COST EFFECTIVE DATE            
*                                                                               
         LA    RE,PPRDELEM         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
PRDF16LP DS    0H                  FIND PLANNED COST EFFECTIVE DATE             
*                                                                               
         USING PPRDBPCE,RE         ESTABLISH EFFECTIVE DATE ELM                 
*                                                                               
         CLI   PPBPCELC,0          DONE AT END OF RECORD                        
         BE    PRDF16DN                                                         
*                                                                               
         CLI   PPBPCELC,PPBPCECQ   SKIP IF NOT EFF DTE ELM                      
         BNE   PRDF16CN                                                         
*                                                                               
         B     PRDF16FD            ELEMENT FOUND                                
*                                                                               
PRDF16CN DS    0H                                                               
         ZIC   RF,PPBPCELN         GET ELEMENT LENGTH                           
         LA    RE,PPBPCELC(RF)     BUMP TO NEXT ELEMENT                         
         B     PRDF16LP                                                         
*                                                                               
PRDF16FD DS    0H                                                               
*                                                                               
         MVC   SVPRDEFD,PPBPCEFF   SAVE PLANNED COST BILLING                    
*                                                                               
         DROP  RE                                                               
*                                                                               
PRDF16DN DS    0H                                                               
         MVC   THISPRD,PPRDKPRD                                                 
         MVC   THISPRNM,SPACES                                                  
         MVC   THISPRNM(L'PPRDNAME),PPRDNAME                                    
         OC    THISPRNM,SPACES                                                  
         ZAP   PRDORD,=P'0'                                                     
         ZAP   PRDPD,=P'0'                                                      
         ZAP   PRDBL,=P'0'                                                      
         ZAP   PRDCMB,=P'0'       CURRENT MONTH BILLED                          
         ZAP   PRDPDT,=P'0'       NET PAID TODAY                                
         MVI   PRDSW,C'N'                                                       
PRDFX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************                                             
PRDL     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                                                               
         CLI   PRDSW,C'Y'         ANY ACTIVITY?                                 
         BNE   PRDLX                                                            
         MVI   CLTSW,C'Y'         CLIENT ACTIVITY                               
         MVI   REQSW,C'Y'         REQUEST ACTIVITY                              
         CLI   QOPT3,C'C'         CLIENT + HIGHER TOTALS                        
         BE    PRDL5              SKIP PRODUCT                                  
         CLI   QOPT3,C'M'         MEDIA + HIGHER TOTALS                         
         BE    PRDL5              SKIP PRODUCT                                  
*                                                                               
         MVC   P1+1(2),PPRDKAGY                                                 
         MVC   P1+7(1),PPRDKMED                                                 
         MVC   P1+11(3),PPRDKCLT                                                
         MVC   P1+16(3),PPRDKPRD                                                
         MVC   P1+21(3),=C'***'                                                 
         EDIT  PRDORD,(16,P1+26),2,COMMAS=YES,FLOAT=-                           
         MVI   P1+42,C'*'                                                       
         EDIT  PRDPD,(16,P1+47),2,COMMAS=YES,FLOAT=-                            
         MVI   P1+63,C'*'                                                       
         EDIT  PRDBL,(16,P1+68),2,COMMAS=YES,FLOAT=-                            
         MVI   P1+84,C'*'                                                       
         EDIT  PRDCMB,(16,P1+89),2,COMMAS=YES,FLOAT=-                           
         MVI   P1+105,C'*'                                                      
         EDIT  PRDPDT,(16,P1+110),2,COMMAS=YES,FLOAT=-                          
         MVC   HEAD4+45(L'MYHEAD),MYHEAD                                        
         MVI   P1+126,C'*'                                                      
*                                                                               
         CLI   QOPT3,C'P'             NO ESTIMATE DETAIL?                       
         BNE   PRDL3                                                            
         MVI   P1+42,C' '       NO STARS                                        
         MVI   P1+63,C' '       NO STARS                                        
         MVI   P1+84,C' '       NO STARS                                        
         MVI   P1+105,C' '       NO STARS                                       
         MVI   P1+126,C' '       NO STARS                                       
PRDL3    DS    0H                                                               
         MVC   HEAD4+45(L'MYHEAD),MYHEAD                                        
         MVC   HEAD5(L'MYHEAD5),MYHEAD5                                         
         MVC   HEAD6(L'MYHEAD6),MYHEAD6                                         
         MVC   HEAD7(L'MYHEAD7),MYHEAD7                                         
         GOTO1 REPORT                                                           
*                                                                               
PRDL5    DS    0H                                                               
         AP    CLTORD,PRDORD          ROLL TO CLIENT TOTALS                     
         AP    CLTPD,PRDPD                                                      
         AP    CLTBL,PRDBL                                                      
         AP    CLTCMB,PRDCMB                                                    
         AP    CLTPDT,PRDPDT                                                    
*                                                                               
PRDLX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* PROCESS ESTIMATE HEADER - BUILD LIST OF DATES                                 
*==============================================================                 
                                                                                
PROCESS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        NEED TO PROCESS ALL ESTIMATES                                          
*                                                                               
******   CLC   QEND,PESTST         REQ END BEFORE EST START                     
******   JL    EXIT                                                             
******   CLC   QSTART,PESTEND      REQ START AFTER EST END                      
******   JH    EXIT                                                             
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    PROCE2                                                           
         CLC   PESTKPRD,QPRODUCT     MUST BE RIGHT PRODUCT                      
         JNE   EXIT                                                             
*                                                                               
PROCE2   DS    0H                                                               
         CLI   QOPT1,C'T'           INCLUDING TEST ESTIMATES?                   
         BE    PROCE4                                                           
         TM    PESTTEST,X'80'       OTHERWISE SKIP TEST ESTS                    
         JO    EXIT                                                             
*                                                                               
PROCE4   MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISEST,DUB+6(2)                                                 
*                                                                               
         ZAP   ESTORD,=P'0'                                                     
         ZAP   ESTPD,=P'0'                                                      
         ZAP   ESTBL,=P'0'                                                      
         ZAP   ESTCMB,=P'0'       CURRENT MONTH BILLED                          
         ZAP   ESTPDT,=P'0'       NET PAID TODAY                                
*                                                                               
PROCE5   MVC   THISTYPE,PESTRTYP   SET ESTIMATE RATE TYPE                       
         CLI   THISTYPE,C' '                                                    
         BH    *+8                                                              
         MVI   THISTYPE,C'A'                                                    
         MVC   THISESNM,SPACES                                                  
         MVC   THISESNM(L'PESTNAME),PESTNAME                                    
         OC    THISESNM,SPACES                                                  
*                                                                               
         XC    ESTPCED,ESTPCED CLEAR ESTIMATE'S PLANNED COST EFF DATE           
         XC    ESTPCAD,ESTPCAD CLEAR ESTIMATE'S ACTUALIZATION DATE              
*                                                                               
         CLI   B2BPROF+12,C'Y'     PLANNED COST CLIENT?                         
         BNE   PROCE50                                                          
         LA    R1,PESTELEM         POINT TO FIRST ELEMENT IN ESTREC             
         SR    RE,RE                                                            
*                                                                               
PROCETLP DS    0H                                                               
*                                                                               
         CLI   0(R1),0             DONE AT END OF RECORD                        
         BE    PROCETDN                                                         
*                                                                               
         USING PESTBPCE,R1         ESTABLISH PLANNED COST EFF DATE ELM          
*                                                                               
         CLI   PEBPCELC,PEBPCECQ   SKIP IF NOT EFF DATE ELM                     
         BNE   PROCET10                                                         
*                                                                               
         MVC   ESTPCED,PEBPCEFF    SAVE PC BILLING EFFECTIVE DATE               
*                                                                               
         B     PROCETCN                                                         
*                                                                               
PROCET10 DS    0H                                                               
*                                                                               
*        FIND ACTUALIZATION DATE                                                
*                                                                               
         USING PESTACTD,R1         ESTABLISH PC ACTUALIZATION DATE ELM          
*                                                                               
         CLI   PEACTELC,PEACTECQ   SKIP IF NOT ACT DATE ELM                     
         BNE   PROCET20                                                         
*                                                                               
         MVC   ESTPCAD,PEACTDAT    SAVE ACTUALIZATION DATE                      
*                                                                               
         B     PROCETCN                                                         
*                                                                               
PROCET20 DS    0H                                                               
*                                                                               
         DROP  R1                                                               
*                                                                               
PROCETCN DS    0H                                                               
         ZIC   RE,1(R1)            GET ELEMENT LENGTH                           
         LA    R1,0(RE,R1)         BUMP TO NEXT ELEMENT                         
         B     PROCETLP                                                         
*                                                                               
PROCETDN DS    0H                                                               
*                                                                               
         OC    ESTPCED,ESTPCED     IF NO EST OVERRIDE OF EFF DTE                
         BNZ   *+10                                                             
         MVC   ESTPCED,SVPRDEFD       USE PRODUCT'S OVERRIDE                    
*                                                                               
         OC    ESTPCED,ESTPCED     IF NO OVERRIDE OF EFF DTE                    
         BNZ   PROCETSX                                                         
*                                                                               
         MVC   ESTPCED,B2BPROF+13     USE CLIENT'S FROM B2B PROFILE             
*                                                                               
         OC    ESTPCED,ESTPCED        SKIP IF NONE                              
         BZ    PROCETSX                                                         
*                                                                               
         CLI   ESTPCED,80             SET CENTURY                               
         BNL   PROCETSX                                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,ESTPCED                                                       
         AHI   RE,100                                                           
         STC   RE,ESTPCED             21ST CENTURY                              
*                                                                               
PROCETSX DS    0H                                                               
                                                                                
PROCE50  DS    0H                                                               
* ESTIMATE IS IN REQUEST PERIOD                                                 
                                                                                
                                                                                
*==================================================================             
* POST ORDERED AND PAID DOLLARS TO MONTH BUCKETS AND CLEAR BILLED               
*==================================================================             
*                                                                               
*        READ ESTIMATE BUCKET RECORD                                            
*                                                                               
RDBKT    MVC   PPGKEY,KEY          SAVE PPG'S KEY                               
*                                                                               
*        CLEAR RECORD AREAS                                                     
*                                                                               
         ZAP   THISCMB,=P'0'       CLEAR CURR MONTH BILLED                      
         ZAP   THISNPT,=P'0'       PAID TODAY                                   
*                                                                               
         MVI   ESTSW,C'N'          ESTIMATE ACTIVITY SWITCH                     
         MVI   NOPAYSW,C'N'                                                     
         XC    THISYM01(MAXMOS*4),THISYM01 MAXMOS X 4 MONTHS                    
*                                                                               
         LA    R5,THISIO01        ORDERED  - INSERTION MONTHS                   
         LHI   R6,MAXMOS                                                        
         ZAP   0(8,R5),=P'0'                                                    
         AHI   R5,L'THISIO01                                                    
         BCT   R6,*-10                                                          
*                                                                               
         LA    R5,THISOR01        ORDERED  - BILLABLE MONTHS                    
         LHI   R6,MAXMOS                                                        
         ZAP   0(8,R5),=P'0'                                                    
         AHI   R5,L'THISOR01                                                    
         BCT   R6,*-10                                                          
*                                                                               
         LA    R5,THISPD01        PAID                                          
         LHI   R6,MAXMOS                                                        
         ZAP   0(8,R5),=P'0'                                                    
         AHI   R5,L'THISPD01                                                    
         BCT   R6,*-10                                                          
*                                                                               
         LA    R5,THISBL01        BILLED                                        
         LHI   R6,MAXMOS                                                        
         ZAP   0(8,R5),=P'0'                                                    
         AHI   R5,L'THISBL01                                                    
         BCT   R6,*-10                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(12),PESTREC                                                  
         MVI   KEY+3,X'09'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     BUCKET RECORD NOT FOUND                      
         JNE   RDBILLS             STILL TRY FOR BILLS                          
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R6,PBUYREC                                                       
         USING PBKRECD,R6                                                       
         LA    R2,PBKREC+33                                                     
         MVI   ELCODE,X'21'        TODAY'S BUCKETS                              
         CLI   0(R2),X'21'         FIRST ELEMENT?                               
         BE    RDBK2B                                                           
RDBK2    BAS   RE,NEXTEL                                                        
         BNE   RDBK2X              END OF BUCKETS                               
         USING BKELEM,R2                                                        
*                                                                               
RDBK2B   CLI   QOPT1,C'T'          INCLUDING TEST?                              
         BE    RDBK2T                                                           
         TM    BKIND,X'01'         TEST ESTIMATE?                               
         BO    RDBK2X              SKIP                                         
RDBK2T   DS    0H                                                               
         CLC   BKDATE,BTODAY      MUST MATCH TODAY                              
         BNE   RDBK2X                                                           
*                                                                               
         AP    THISNPT,BKPNET      PAID TODAY - NET                             
         AP    ESTPDT,BKPNET      PAID TODAY - NET                              
         B     RDBK2X              KEEP LOOKING                                 
*                                                                               
         DROP  R2                                                               
*                                                                               
RDBK2X   DS    0H                                                               
         LA    R6,PBUYREC                                                       
         USING PBKRECD,R6                                                       
*                                                                               
         MVI   PASS,1              FIRST PASS - GETS PLANNED COST               
*                                  ORDERED $ IF DOING PLANNED COST              
*                                  THEN SECOND PASS WILL GET PAID $             
*                                  FOR NON-PLANNED COST REPORTS                 
*                                  FIRST PASS WILL GET BOTH ORDERED             
*                                  AND PAID - NO SECOND PASS NEEDED             
RDBK2XX  LA    R2,PBKREC+33                                                     
*                                                                               
         MVI   ELCODE,X'22'        BILLABLE MONTH BUCKETS                       
*                                                                               
         CLI   QOPT4,C'I'          REPORTING INSERTION MONTHS?                  
         BNE   *+8                                                              
         MVI   ELCODE,X'23'                                                     
*                                                                               
         CLI   PASS,2              LEAVE AT 'NORMAL' SETTING                    
         BE    RDBK3                                                            
*                                                                               
         CLI   QMEDIA,C'I'         SEE IF INTERACTIVE                           
         BE    RDBK2Z                                                           
         CLI   QMEDIA,C'B'         SEE IF MOBILE                                
         BE    RDBK2Z                                                           
         CLI   QMEDIA,C'D'         SEE IF DIGITAL AUDIO                         
         BE    RDBK2Z                                                           
         CLI   QMEDIA,C'V'         NATIONAL VIDEO                               
         BE    RDBK2Z                                                           
         CLI   QMEDIA,C'W'         LOCAL VIDEO                                  
         BE    RDBK2Z                                                           
         CLI   QMEDIA,C'L'         SEE IF SOCIAL                                
         BNE   RDBK3                                                            
RDBK2Z   CLI   QOPT5,C'P'          REPORTING PLANNED COST?                      
         BNE   RDBK3                                                            
         NI    ELCODE,X'0F'                                                     
         OI    ELCODE,X'40'          SWITCH TO PLANNED COST CODES               
*                                                                               
RDBK3    CLC   0(1,R2),ELCODE        CHECK FIRST ELEMENT                        
         BE    RDBK50                                                           
RDBK5    BAS   RE,NEXTEL                                                        
         JNE   RDBK60              END OF ELEMENT BUCKETS                       
         USING BKELEM,R2                                                        
*                                                                               
RDBK50   CLI   QOPT1,C'T'          INCLUDING TEST?                              
         BE    RDBK5A                                                           
*                                                                               
         TM    BKIND,X'01'         TEST ESTIMATE?                               
         BO    RDBK5               SKIP                                         
*                                                                               
RDBK5A   DS    0H                                                               
*                                                                               
                                                                                
*                                                                               
RDBK5A5  LA    R3,BKOGRS                                                        
         LA    R4,6                                                             
         TM    ELCODE,X'40'   PROCESSING PLANNED COST ELEMENT?                  
         BZ    *+8                                                              
         LA    R4,3           THEY DON'T HAVE PAID BUCKETS                      
RDBK5B   CP    0(6,R3),=P'0'                                                    
         BNE   RDBK5C                                                           
         LA    R3,6(R3)                                                         
         BCT   R4,RDBK5B                                                        
         B     RDBK5        SKIP IF ALL BUCKETS ARE ZERO                        
*                                                                               
RDBK5C   DS    0H                                                               
         MVI   PCSW,C'N'           SET PLANNED COST SWITCH OFF                  
         CLI   QMEDIA,C'I'         FIRST CHECK MEDIA I INTERNET                 
         BE    RDBK5CP                                                          
         CLI   QMEDIA,C'B'         MOBILE                                       
         BE    RDBK5CP                                                          
         CLI   QMEDIA,C'D'         DIGITAL AUDIO                                
         BE    RDBK5CP                                                          
         CLI   QMEDIA,C'V'         NAT. VIDEO                                   
         BE    RDBK5CP                                                          
         CLI   QMEDIA,C'W'         LOC. VIDEO                                   
         BE    RDBK5CP                                                          
         CLI   QMEDIA,C'L'         OR MEDIA L - SOCIAL                          
         BNE   RDBK5C4                                                          
RDBK5CP  CLI   QOPT5,C'P'          REPORTING PLANNED COST?                      
         BNE   RDBK5C4                                                          
*                                                                               
         TM    ELCODE,X'40'     LOOKING AT PLANNED COST ELEMENTS?               
         BNO   RDBK5C3                                                          
         OC    ESTPCED,ESTPCED  CHECK FOR PLANNED COST EFFECTIVE DATE           
         BZ    RDBK5            SKIP IF NONE                                    
         CLC   BKYM,ESTPCED     IS BUCKET'S MTH BEFORE EFFECTIVE DATE?          
         BL    RDBK5            SKIP IF BEFORE                                  
         OC    ESTPCAD,ESTPCAD  IS ACTUALIZATION DATE PRESENT?                  
         BZ    RDBK5C1          NO - PROCESS THESE PLANNED $                    
         CLC   BKYM,ESTPCAD     IS MTH ACTUALIZED?                              
         BNH   RDBK5            IF SO SKIP THIS BUCKET                          
*                                                                               
RDBK5C1  MVI   PCSW,C'Y'        SET TO PROCESS PLANNED COST                     
         B     RDBK5C4                                                          
*                                                                               
RDBK5C3  DS    0H             HERE IF PROCESSING A NORMAL BUCKET                
*                                                                               
         OC    ESTPCED,ESTPCED  CHECK FOR PLANNED COST EFFECTIVE DATE           
         BZ    RDBK5C4          IF NONE THEN SKIP                               
         CLC   BKYM,ESTPCED     IS BUCKET'S MTH BEFORE EFFECTIVE DATE?          
         BL    RDBK5C4          IF BEFORE THEN SKIP                             
         OC    ESTPCAD,ESTPCAD  IS ACTUALIZATION DATE PRESENT?                  
         BZ    RDBK5            NO - SKIP  THIS BUCKET                          
         CLC   BKYM,ESTPCAD     BUCKET'S MTH AFTER ACTUALIZATION DATE?          
         BNH   RDBK5C4          IF NOT, PROCESS THESE $ (AND PAYS)              
*                                                                               
RDBK5C3A MVI   PCSW,C'P'        SET TO PROCESS JUST PAYMENTS                    
         B     RDBK5C4                                                          
*                                                                               
***                                                                             
***      FOR PLANNED COST PROCESSING, CHECK BUCKET'S MONTH                      
***      AGAINST PLANNED COST'S EFFECTIVE DATE                                  
***      IF BEFORE - PROCESS ORDERED $ FROM 'NORMAL' BUCKET                     
***      IF EQUAL OR AFTER, CHECK ACTUALIZATION DATE                            
***      IF BUCKET'S MONTH IS EQUAL TO OR AFTER ACTUALIZATION DATE              
***      AGAIN PROCESS $ FROM 'NORMAL' BUCKET                                   
***      IF BEFORE PROCESS $ FROM PLANNED COST BUCKET                           
***                                                                             
RDBK5C4  DS    0H                                                               
         TM    ELCODE,X'40'     PROCESSING PC ELEMENT?                          
         BZ    RDBK5C5                                                          
         CLI   PCSW,C'Y'       SHOULD I PROCESS IT?                             
         BE    RDBK5C5         YES                                              
         B     RDBK5    IF NOT, MEANS THAT I CAN SKIP THIS ELEMENT              
*                       I SHOULD GET THEN ORDERED $ FROM 'NORMAL' ELEM          
RDBK5C5  MVC   WORK(2),BKYM                                                     
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK+6)                                  
*                                                                               
         LA    R3,THISYM01                                                      
         LA    R4,THISOR01                                                      
         LA    R5,THISPD01                                                      
*                                                                               
         CLI   SVQOPT4,C'B'       AM I DOING BOTH INS. AND BILLABLE?            
         BNE   RDBK10                                                           
         CLI   QOPT4,C'I'         INSERTION MTH PASS?                           
         BNE   RDBK10                                                           
         LA    R4,THISIO01        RESET R4 TO INS. MTH ACCUMS                   
*                                                                               
RDBK10   OC    0(4,R3),0(R3)      DATE PRESENT?                                 
         BZ    RDBK15                                                           
         CLC   0(4,R3),WORK+6     ENTRY MAY ALREADY BE THERE                    
         BE    RDBK15A                                                          
         LA    R3,4(R3)                                                         
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         B     RDBK10                                                           
*                                                                               
RDBK15   DS    0H                                                               
         MVC   0(4,R3),WORK+6        YYMM                                       
*                                                                               
RDBK15A  ZAP   MYDUB1,=P'0'                                                     
         ZAP   MYDUB2,=P'0'                                                     
         CLI   QOPT2,C'G'                                                       
         BNE   RDBK15N                                                          
RDBK15G  DS    0H                                                               
         PRINT  GEN                                                             
         JIF   PASS,EQ,2,AND,PCSW,EQ,C'P',RDBK15H,JUMP=N                        
*              ELSE GET ORDERED $ NOW                                           
         ZAP   0(8,R4),BKOGRS        ORDERED - GROSS                            
         ZAP   MYDUB1,0(8,R4)                                                   
*                                                                               
RDBK15H  TM    ELCODE,X'40'    PLANNED COST BUCKETS DON'T HAVE PAID             
         BNZ   RDBK15X                                                          
         CLI   NOPAYSW,C'Y'                                                     
         BE    RDBK15X                                                          
         ZAP   0(8,R5),BKPGRS        PAID - GROSS                               
         ZAP   MYDUB2,0(8,R5)                                                   
         B     RDBK15X                                                          
*                                                                               
RDBK15N  DS    0H                                                               
         CLI   QOPT2,C'N'                                                       
         BNE   RDBK151                                                          
         JIF   PASS,EQ,2,AND,PCSW,EQ,C'P',RDBK15O,JUMP=N                        
*              ELSE GET ORDERED $ NOW                                           
         ZAP   0(8,R4),BKONET        ORDERED - NET                              
         ZAP   MYDUB1,0(8,R4)                                                   
RDBK15O  TM    ELCODE,X'40'    PLANNED COST BUCKETS DON'T HAVE PAID             
         BNZ   RDBK15X                                                          
         CLI   NOPAYSW,C'Y'                                                     
         BE    RDBK15X                                                          
         ZAP   0(8,R5),BKPNET        PAID - NET                                 
         ZAP   MYDUB2,0(8,R5)                                                   
         B     RDBK15X                                                          
*                                                                               
RDBK151  DS    0H                                                               
         CLI   QOPT2,C'1'            GROSS-CD                                   
         BNE   RDBK152                                                          
         JIF   PASS,EQ,2,AND,PCSW,EQ,C'P',RDBK151A,JUMP=N                       
*              ELSE GET ORDERED $ NOW                                           
         ZAP   0(8,R4),BKOGRS        ORDERED - GROSS-CD                         
         SP    0(8,R4),BKOCD                                                    
         ZAP   MYDUB1,0(8,R4)                                                   
RDBK151A TM    ELCODE,X'40'    PLANNED COST BUCKETS DON'T HAVE PAID             
         BNZ   RDBK15X                                                          
         CLI   NOPAYSW,C'Y'                                                     
         BE    RDBK15X                                                          
         ZAP   0(8,R5),BKPGRS        PAID - GROSS-CD                            
         SP    0(8,R5),BKPCD                                                    
         ZAP   MYDUB2,0(8,R5)                                                   
         B     RDBK15X                                                          
*                                                                               
RDBK152  DS    0H                                                               
         CLI   QOPT2,C'2'            NET-CD                                     
         BNE   RDBK15G               (DEFALUT BACK TO GROSS)                    
         JIF   PASS,EQ,2,AND,PCSW,EQ,C'P',RDBK152A,JUMP=N                       
*              ELSE GET ORDERED $ NOW                                           
         ZAP   0(8,R4),BKONET        ORDERED - NET-CD                           
         SP    0(8,R4),BKOCD                                                    
         ZAP   MYDUB1,0(8,R4)                                                   
RDBK152A TM    ELCODE,X'40'    PLANNED COST BUCKETS DON'T HAVE PAID             
         BNZ   RDBK15X                                                          
         CLI   NOPAYSW,C'Y'                                                     
         BE    RDBK15X                                                          
         ZAP   0(8,R5),BKPNET        PAID - NET-CD                              
         SP    0(8,R5),BKPCD                                                    
         ZAP   MYDUB2,0(8,R5)                                                   
         B     RDBK15X                                                          
*                                                                               
         PRINT NOGEN                                                            
*                                                                               
RDBK15X  DS    0H                                                               
*                                                                               
         CLI   SVQOPT4,C'B'       AM I DOING BOTH INS. AND BILLABLE?            
         BNE   RDBK17                                                           
         CLI   QOPT4,C'I'         INSERTION MTH PASS?                           
         BNE   RDBK17             IF SO, DON'T ADD TO THESE TOTALS              
         B     RDBK18                                                           
*                                                                               
RDBK17   AP    ESTORD,MYDUB1                                                    
         AP    ESTPD,MYDUB2                                                     
*                                                                               
RDBK18   CP    0(8,R4),=P'0'                                                    
         BNE   RDBK20                                                           
         CP    0(8,R5),=P'0'                                                    
         BNE   RDBK20                                                           
         B     RDBK5                 GO DO NEXT BUCKET                          
*                                                                               
RDBK20   MVI   ESTSW,C'Y'         ACTIVITY                                      
         B     RDBK5                 GO DO NEXT ELEMENT                         
*                                                                               
RDBK60   CLI   PASS,2          SECOND RUN THRU ELEMENTS?                        
         BE    RDBK70          THEN DONE                                        
         TM    ELCODE,X'40'    WAS FIRST PASS LOOKING FOR PLANNED COST?         
         BZ    RDBK70          IF NOT,THEN ALSO DONE                            
         MVI   PASS,2          NOW DO SECOND PASS FOR PAID                      
*                              FROM 'NORMAL' ELEMENTS PLUS ORDERED              
*                              IF MONTH WAS ACTUALIZED                          
*                           FIRST PASS WILL HAVE GOTTEN ORDERED IF              
*                           WAS NOT ACTUALIZED                                  
         B     RDBK2XX                                                          
*                                                                               
RDBK70   DS    0H                                                               
         CLI   SVQOPT4,C'B'      AM I TO DO BOTH INS. AND BILLABLE $?           
         BNE   RDBILLS                                                          
         CLI   QOPT4,C'I'       DID I JUST DO INS.?                             
         BE    RDBILLS          YES - THEN GO READ BILLS                        
         MVI   QOPT4,C'I'       SET TO DO INSERTION MONTHS                      
         MVI   NOPAYSW,C'Y'     SET TO SKIP PAYMENTS                            
*                               THEY ARE REPORTED BY BILLABLE MONTHS            
         B     RDBK2X           GO GET ORDERED $ BY INSERTION MONTH             
         EJECT                                                                  
*                                                                               
RDBILLS  DS    0H               READ BILLS FOR THIS ESTIMATE                    
         MVC   QOPT4,SVQOPT4    RESTORE TO ORIGINAL VALUE FOR NEXT EST          
         XC    KEY,KEY                                                          
         MVC   KEY(12),PESTKEY                                                  
         MVI   KEY+3,X'08'                                                      
         GOTO1 HIGH                                                             
         B     RDBL10                                                           
RDBL5    GOTO1 SEQ                                                              
*                                                                               
RDBL10   CLC   KEY(12),KEYSAVE      RIGHT ESTIMATE?                             
         BNE   RDBLX                                                            
         LA    R0,PBILLREC                                                      
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         TM    PBILCMSW,X'20'      SKIP AOR BILLS                               
         BO    RDBL5                                                            
         CLI   PBRETAIL,X'41'      SKIP RETAIL CORP SUMMARY                     
         BE    RDBL5                                                            
*******                                                                         
*******  USE PPBVAL TO EXTRACT "EFFECTIVE" VALUES                               
*******                                                                         
         GOTO1 VPPBVAL,DMCB,(C'B',PBILLREC),PPBVALD                             
*                                                                               
         MVC   PBILLGRS,PPBVEBG     SET NEW VALUES INTO PBILLREC                
         MVC   PBILLBIL,PPBVEBB                                                 
         MVC   PBILLNET,PPBVEBN                                                 
         MVC   MYBILLCD,PPBVEBC   "EFFECTIVE" CD                                
*                                                                               
         CP    PBILLGRS,=P'0'                                                   
         BNE   RDBL12                                                           
         CP    PBILLBIL,=P'0'                                                   
         BNE   RDBL12                                                           
         CP    PBILLNET,=P'0'                                                   
         BNE   RDBL12                                                           
         CP    MYBILLCD,=P'0'                                                   
         BNE   RDBL12                                                           
         B     RDBL5          SKIP BILLS WITH NO $                              
*                             SHOULD CATCH BILLS WITH A MOS                     
*                             OUT OF "NORMAL" 21 MONTH RANGE                    
*                                                                               
RDBL12   DS    0H                                                               
         MVC   WORK(2),PBILKMOS                                                 
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK+6)                                  
*                                                                               
         LA    R3,THISYM01                                                      
         LA    R4,THISBL01                                                      
RDBL15   OC    0(4,R3),0(R3)      DATE PRESENT?                                 
         BZ    RDBL20                                                           
         CLC   0(4,R3),WORK+6     DOES IT MATCH MY BILL?                        
         BE    RDBL25                                                           
         LA    R3,4(R3)                                                         
         LA    R4,8(R4)                                                         
         B     RDBL15                                                           
*                                                                               
RDBL20   DS    0H                                                               
         MVC   0(4,R3),WORK+6        YYMM                                       
*                                                                               
RDBL25   DS    0H                                                               
         CLI   QOPT2,C'G'                                                       
         BNE   RDBL25N                                                          
RDBL25G  AP    0(8,R4),PBILLGRS                                                 
         ZAP   DUB,PBILLGRS                                                     
         B     RDBL25X                                                          
*                                                                               
RDBL25N  CLI   QOPT2,C'N'                                                       
         BNE   RDBL251                                                          
         ZAP   DUB,PBILLNET        (NET-CD)                                     
         AP    DUB,MYBILLCD        READD CD                                     
         AP    0(8,R4),DUB                                                      
         B     RDBL25X                                                          
*                                                                               
RDBL251  CLI   QOPT2,C'1'                                                       
         BNE   RDBL252                                                          
         AP    0(8,R4),PBILLBIL    (GROSS-CD)                                   
         ZAP   DUB,PBILLBIL                                                     
         B     RDBL25X                                                          
*                                                                               
RDBL252  CLI   QOPT2,C'2'                                                       
         BNE   RDBL25G      DEFAULT BACK TO GROSS                               
         AP    0(8,R4),PBILLNET     (NET-CD)                                    
         ZAP   DUB,PBILLNET                                                     
         B     RDBL25X                                                          
*                                                                               
RDBL25X  AP    ESTBL,DUB                                                        
*                                                                               
         CLC   PBILKBMN,BTODAY     IN CURRENT MONTH?                            
         BNE   RDBL30                                                           
         AP    THISCMB,DUB                                                      
         AP    ESTCMB,DUB                                                       
*                                                                               
RDBL30   MVI   ESTSW,C'Y'           SET ACTIVITY SWITCH                         
         B     RDBL5                KEEP LOOKING                                
                                                                                
RDBLX    MVC   KEY,PPGKEY           RESTORE PPG'S KEY                           
         GOTO1 HIGH                                                             
*                                                                               
*===============================================================                
* NOW CREATE OUTPUT IF ESTIMATE ACTIVE                                          
*===============================================================                
                                                                                
         CLI   ESTSW,C'Y'          TEST ESTIMATE ACTIVE                         
         BNE   RDBLXX              NO-SKIP                                      
         MVI   PRDSW,C'Y'          PRODUCT ACTIVITY                             
         CLI   QOPT3,C' '    ANY VALUE WILL SKIP ESTIMATE DETAIL                
         BNE   RDBLX5                                                           
*                                                                               
         MVC   P1+1(2),PESTKAGY                                                 
         MVC   P1+7(1),PESTKMED                                                 
         MVC   P1+11(3),PESTKCLT                                                
         MVC   P1+16(3),PESTKPRD                                                
         EDIT  (B2,PESTKEST),(3,P1+21),0                                        
         EDIT  ESTORD,(16,P1+26),2,COMMAS=YES,FLOAT=-                           
         EDIT  ESTPD,(16,P1+47),2,COMMAS=YES,FLOAT=-                            
         EDIT  ESTBL,(16,P1+68),2,COMMAS=YES,FLOAT=-                            
         EDIT  ESTCMB,(16,P1+89),2,COMMAS=YES,FLOAT=-                           
         EDIT  ESTPDT,(16,P1+110),2,COMMAS=YES,FLOAT=-                          
         MVC   HEAD4+45(L'MYHEAD),MYHEAD                                        
         MVC   HEAD5(L'MYHEAD5),MYHEAD5                                         
         MVC   HEAD6(L'MYHEAD6),MYHEAD6                                         
         MVC   HEAD7(L'MYHEAD7),MYHEAD7                                         
         GOTO1 REPORT                                                           
*                                                                               
RDBLX5   AP    PRDORD,ESTORD       ROLL TO PRODUCT TOTALS                       
         AP    PRDPD,ESTPD                                                      
         AP    PRDBL,ESTBL                                                      
         AP    PRDCMB,ESTCMB                                                    
         AP    PRDPDT,ESTPDT                                                    
*                                                                               
         LA    R4,DRECTAB                                                       
         BRAS  RE,OUTPUT           PUT DATA FOR THIS ESTIMATE                   
*                                                                               
RDBLXX   J     EXIT                                                             
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         LTR   R0,R0      NO LENGTH                                             
         BZ    NEXTEL2                                                          
         AR    R2,R0                                                            
         CLI   0(R2),0     END OF REC                                           
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H          ELEMENT NOT FOUND                                    
         LTR   R2,R2                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* CREATE OUTPUT RECORDS                                                         
*=============================================================                  
         SPACE 1                                                                
OUTPUT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,PBUYREC          BUILD OUTPUT RECORD IN BUYREC                
         LR    R0,R6                                                            
         LHI   R1,OUTRECLN         FIXED OUTPUT LENGTH                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
OUTPUT2  L     RE,0(R4)            GET DATA ADDR                                
         SR    RF,RF                                                            
         IC    RF,4(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(RE)                                                    
*                                                                               
         LA    R6,1(R6,RF)         NEXT OUTPUT POSITION                         
         AHI   R4,8                NEXT INPUT FIELD                             
         CLI   0(R4),X'FF'                                                      
         BNE   OUTPUT2                                                          
*                                                                               
*        COUNT MONTHS                                                           
*        AND SORT IN ASCENDING ORDER                                            
*                                                                               
         LA    R1,THISYM01                                                      
         SR    R3,R3                                                            
         LA    R2,MAXMOS FOR BCT                                                
OUTP5    CLI   0(R1),0            DATA DATA MEANS LAST ENTRY                    
         BE    OUTP7                                                            
         LA    R3,1(R3)                                                         
         LA    R1,4(R1)                                                         
         BCT   R2,OUTP5                                                         
*                                                                               
OUTP7    LA    R2,PBUYREC+133    21+16+96 =133                                  
*                                R2 TO FIRST MONTH                              
         PRINT GEN                                                              
         GOTO1 XSORT,DMCB,(0,0(R2)),(R3),36,4,0                                 
         PRINT NOGEN                                                            
         LA    R0,PBUYREC                                                       
         PUT   FILEOUT,(0)                                                      
         AP    RUNRECS,=P'1'                                                    
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
PPA9WORK DS    0D                                                               
         DC    CL8'PPA9WORK'                                                    
         DS    0D                                                               
BLOCK    DS    XL256                                                            
*                                                                               
SVRUNTYP DS    CL2                                                              
*                                                                               
B2BPROF  DS    CL16                                                             
SVPRDEFD DS    XL2        PLANNED COST EFFECTIVE DATE FROM PRODUCT              
*                                                                               
ESTPCED  DS    XL2        CONTROLS FOR PLANNED COST - EFFECTIVE DATE            
ESTPCAD  DS    XL2        ACTUALIZATION DATE                                    
*                                                                               
SAVCOFF  DS    CL2        2 CHARACTER OFFICE (RETURNED BY OFFICER)              
*                                                                               
RELO     DS    A                                                                
VPPBVAL  DS    A                                                                
VOFFICER DS    A                                                                
MYBILLCD DS    PL6                                                              
SVQOPT3  DS    CL1                                                              
SVQOPT4  DS    CL1                                                              
PASS     DS    XL1                                                              
NOPAYSW  DS    CL1        Y= SKIP ADDING PAID $ FROM BUCKET                     
*                                                                               
PCSW     DS    CL1        PLANNED COST SWITCH                                   
MYDUB1   DS    PL8                                                              
MYDUB2   DS    PL8                                                              
*                         P= REPORT JUST PAYMENTS FOR THIS ELEMENT              
*                         Y= REPORT PLANNED COST FOR THIS ELEMENT               
MYHEAD   DS    CL10                                                             
MYHEAD5  DS    CL35                                                             
MYHEAD6  DS    CL35                                                             
MYHEAD7  DS    CL35                                                             
*                         ACCUMULATORS                                          
ESTORD   DS    PL8        ESTIMATE                                              
ESTPD    DS    PL8                                                              
ESTBL    DS    PL8                                                              
ESTCMB   DS    PL8                                                              
ESTPDT   DS    PL8                                                              
*                                                                               
PRDORD   DS    PL8        PRODUCT                                               
PRDPD    DS    PL8                                                              
PRDBL    DS    PL8                                                              
PRDCMB   DS    PL8                                                              
PRDPDT   DS    PL8                                                              
*                                                                               
CLTORD   DS    PL8        CLIENT                                                
CLTPD    DS    PL8                                                              
CLTBL    DS    PL8                                                              
CLTCMB   DS    PL8                                                              
CLTPDT   DS    PL8                                                              
*                                                                               
REQORD   DS    PL8        REQUEST  (MEDIA)                                      
REQPD    DS    PL8                                                              
REQBL    DS    PL8                                                              
REQCMB   DS    PL8                                                              
REQPDT   DS    PL8                                                              
*                                                                               
*                                                                               
RUNORD   DS    PL8        RUN (AGENCY)                                          
RUNPD    DS    PL8                                                              
RUNBL    DS    PL8                                                              
RUNCMB   DS    PL8                                                              
RUNPDT   DS    PL8                                                              
RUNRECS  DS    PL8        RECORD COUNT                                          
*                                                                               
ESTSW    DS    CL1                                                              
PRDSW    DS    CL1                                                              
CLTSW    DS    CL1                                                              
REQSW    DS    CL1                                                              
TODAY1   DS    CL6                                                              
BTODAY   DS    XL3                                                              
*                                                                               
MAXMOS   EQU   21         MAXIMUM MONTHS FOR ESTIMATE DATA                      
*                                                                               
PPGKEY   DS    CL64                SAVED PPG'S KEY                              
       ++INCLUDE PPBVALD                                                        
*                                                                               
FILEHDR  DS    0D                                                               
FILEID   DC    CL6'PA9HDR'                                                      
FILESYS  DC    C'PRNT'                                                          
FILEDATE DC    C'YYMMDD'           FILE CREATION DATE                           
FILEQSTR DC    C'YYMMDD'           REQUEST START DATE                           
FILEQEND DC    C'YYMMDD'           REQUEST END DATE                             
FILEDOLS DC    C'G'                FROM QOPT2 (DEFAULT IS GROSS)                
*                                  G=GROSS,N=NET,1=GROSS-CD,2=NET-CD            
FILEMTYP DC    C' '                I=INSERTION MTH FOR ORDERED AND PAID         
*                                  BILLING STILL BY BILLABLE MTH                
FILEDTYP DC    C' '                P=PLANNED COST $ (PRINT ONLY)                
*                                  MEDIA I (FOR NOW)                            
FILEHDRX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'**FLD***'                                                    
FLD      DS    CL32                                                             
**OLD*                                                                          
**OLD*          OLD FORMAT WITH ONLY ONE TYPE OF ORDERED DATA                   
**OLD*                                                                          
**LEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=FB,BLKSIZE=19200,                  
**OLD          MACRF=PM,LRECL=768                                               
**OLD                                                                           
**OLD          THISYM01-THISREC = 134 BYTES                                     
**OLD**        21 X 28 = 588  (21 MONTHS 28 BYTES EACH)                         
**OLD**        134+588 + 46 SPARE = 768                                         
**OLD                                                                           
**OLD          25 X 768 = 19200                                                 
**OLD                                                                           
**TRECLN EQU   768                                                              
*                                                                               
*          NEW FORMAT WITH INSERTIION AND BILLABLE MTH ORDERED DATA             
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=FB,BLKSIZE=18000,         X        
               MACRF=PM,LRECL=900                                               
                                                                                
*              THISYM01-THISREC = 134 BYTES                                     
*              21 X 36 = 756  (21 MONTHS 36 BYTES EACH)                         
*              134+756 + 10 SPARE = 900                                         
*                                                                               
*              20 X 900 = 18000                                                 
*                                                                               
OUTRECLN EQU   900                                                              
*                                                                               
         EJECT                                                                  
* ENTRIES ARE                                                                   
* AL4(DATA)                                                                     
* AL1(L'DATA)                                                                   
* CL1'TYPE'                                                                     
* C'  '              IF NOT X'00' EOR IF FIELD NOT > THIS VALUE                 
* X'01'              CONVERT THE FIELD TO DECIMAL BEFORE WRITE                  
* OR IF TYPE=B OR P  LAST BYTE IS NUMBER OF DECIMAL PLACES                      
         SPACE 1                                                                
         DS    0D                                                               
RECTAB   DS    0XL8                                                             
         DC    CL8'DRECTAB*'                                                    
DRECTAB  DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISAGMD),AL1(L'THISAGMD),C'T',2X'00'                        
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISPRD),AL1(L'THISPRD),C'T',2X'00'                          
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISTYPE),AL1(L'THISTYPE),C'T',2X'00'                        
         DC    AL4(THISMDOF),AL1(L'THISMDOF),C'T',2X'00'                        
         DC    AL4(THISACOF),AL1(L'THISACOF),C'T',2X'00'                        
         DC    AL4(THISCACC),AL1(L'THISCACC),C'T',2X'00'                        
         DC    AL4(THISCMB),AL1(L'THISCMB),C'P',X'00',X'02'                     
         DC    AL4(THISNPT),AL1(L'THISNPT),C'P',X'00',X'02'                     
         DC    AL4(THISMDNM),AL1(L'THISMDNM),C'T',2X'00'                        
         DC    AL4(THISCLNM),AL1(L'THISCLNM),C'T',2X'00'                        
         DC    AL4(THISPRNM),AL1(L'THISPRNM),C'T',2X'00'                        
         DC    AL4(THISESNM),AL1(L'THISESNM),C'T',2X'00'                        
*                                                                               
         DC    AL4(THISYM01),AL1(L'THISYM01),C'T',2X'00'                        
         DC    AL4(THISOR01),AL1(L'THISOR01),C'P',X'00',X'02'                   
         DC    AL4(THISPD01),AL1(L'THISPD01),C'P',X'00',X'02'                   
         DC    AL4(THISBL01),AL1(L'THISBL01),C'P',X'00',X'02'                   
         DC    AL4(THISIO01),AL1(L'THISIO01),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM02),AL1(L'THISYM02),C'T',2X'00'                        
         DC    AL4(THISOR02),AL1(L'THISOR02),C'P',X'00',X'02'                   
         DC    AL4(THISPD02),AL1(L'THISPD02),C'P',X'00',X'02'                   
         DC    AL4(THISBL02),AL1(L'THISBL02),C'P',X'00',X'02'                   
         DC    AL4(THISIO02),AL1(L'THISIO02),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM03),AL1(L'THISYM03),C'T',2X'00'                        
         DC    AL4(THISOR03),AL1(L'THISOR03),C'P',X'00',X'02'                   
         DC    AL4(THISPD03),AL1(L'THISPD03),C'P',X'00',X'02'                   
         DC    AL4(THISBL03),AL1(L'THISBL03),C'P',X'00',X'02'                   
         DC    AL4(THISIO03),AL1(L'THISIO03),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM04),AL1(L'THISYM04),C'T',2X'00'                        
         DC    AL4(THISOR04),AL1(L'THISOR04),C'P',X'00',X'02'                   
         DC    AL4(THISPD04),AL1(L'THISPD04),C'P',X'00',X'02'                   
         DC    AL4(THISBL04),AL1(L'THISBL04),C'P',X'00',X'02'                   
         DC    AL4(THISIO04),AL1(L'THISIO04),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM05),AL1(L'THISYM05),C'T',2X'00'                        
         DC    AL4(THISOR05),AL1(L'THISOR05),C'P',X'00',X'02'                   
         DC    AL4(THISPD05),AL1(L'THISPD05),C'P',X'00',X'02'                   
         DC    AL4(THISBL05),AL1(L'THISBL05),C'P',X'00',X'02'                   
         DC    AL4(THISIO05),AL1(L'THISIO05),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM06),AL1(L'THISYM06),C'T',2X'00'                        
         DC    AL4(THISOR06),AL1(L'THISOR06),C'P',X'00',X'02'                   
         DC    AL4(THISPD06),AL1(L'THISPD06),C'P',X'00',X'02'                   
         DC    AL4(THISBL06),AL1(L'THISBL06),C'P',X'00',X'02'                   
         DC    AL4(THISIO06),AL1(L'THISIO06),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM07),AL1(L'THISYM07),C'T',2X'00'                        
         DC    AL4(THISOR07),AL1(L'THISOR07),C'P',X'00',X'02'                   
         DC    AL4(THISPD07),AL1(L'THISPD07),C'P',X'00',X'02'                   
         DC    AL4(THISBL07),AL1(L'THISBL07),C'P',X'00',X'02'                   
         DC    AL4(THISIO07),AL1(L'THISIO07),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM08),AL1(L'THISYM08),C'T',2X'00'                        
         DC    AL4(THISOR08),AL1(L'THISOR08),C'P',X'00',X'02'                   
         DC    AL4(THISPD08),AL1(L'THISPD08),C'P',X'00',X'02'                   
         DC    AL4(THISBL08),AL1(L'THISBL08),C'P',X'00',X'02'                   
         DC    AL4(THISIO08),AL1(L'THISIO08),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM09),AL1(L'THISYM09),C'T',2X'00'                        
         DC    AL4(THISOR09),AL1(L'THISOR09),C'P',X'00',X'02'                   
         DC    AL4(THISPD09),AL1(L'THISPD09),C'P',X'00',X'02'                   
         DC    AL4(THISBL09),AL1(L'THISBL09),C'P',X'00',X'02'                   
         DC    AL4(THISIO09),AL1(L'THISIO09),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM10),AL1(L'THISYM10),C'T',2X'00'                        
         DC    AL4(THISOR10),AL1(L'THISOR10),C'P',X'00',X'02'                   
         DC    AL4(THISPD10),AL1(L'THISPD10),C'P',X'00',X'02'                   
         DC    AL4(THISBL10),AL1(L'THISBL10),C'P',X'00',X'02'                   
         DC    AL4(THISIO10),AL1(L'THISIO10),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM11),AL1(L'THISYM11),C'T',2X'00'                        
         DC    AL4(THISOR11),AL1(L'THISOR11),C'P',X'00',X'02'                   
         DC    AL4(THISPD11),AL1(L'THISPD11),C'P',X'00',X'02'                   
         DC    AL4(THISBL11),AL1(L'THISBL11),C'P',X'00',X'02'                   
         DC    AL4(THISIO11),AL1(L'THISIO11),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM12),AL1(L'THISYM12),C'T',2X'00'                        
         DC    AL4(THISOR12),AL1(L'THISOR12),C'P',X'00',X'02'                   
         DC    AL4(THISPD12),AL1(L'THISPD12),C'P',X'00',X'02'                   
         DC    AL4(THISBL12),AL1(L'THISBL12),C'P',X'00',X'02'                   
         DC    AL4(THISIO12),AL1(L'THISIO12),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM13),AL1(L'THISYM13),C'T',2X'00'                        
         DC    AL4(THISOR13),AL1(L'THISOR13),C'P',X'00',X'02'                   
         DC    AL4(THISPD13),AL1(L'THISPD13),C'P',X'00',X'02'                   
         DC    AL4(THISBL13),AL1(L'THISBL13),C'P',X'00',X'02'                   
         DC    AL4(THISIO13),AL1(L'THISIO13),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM14),AL1(L'THISYM14),C'T',2X'00'                        
         DC    AL4(THISOR14),AL1(L'THISOR14),C'P',X'00',X'02'                   
         DC    AL4(THISPD14),AL1(L'THISPD14),C'P',X'00',X'02'                   
         DC    AL4(THISBL14),AL1(L'THISBL14),C'P',X'00',X'02'                   
         DC    AL4(THISIO14),AL1(L'THISIO14),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM15),AL1(L'THISYM15),C'T',2X'00'                        
         DC    AL4(THISOR15),AL1(L'THISOR15),C'P',X'00',X'02'                   
         DC    AL4(THISPD15),AL1(L'THISPD15),C'P',X'00',X'02'                   
         DC    AL4(THISBL15),AL1(L'THISBL15),C'P',X'00',X'02'                   
         DC    AL4(THISIO15),AL1(L'THISIO15),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM16),AL1(L'THISYM16),C'T',2X'00'                        
         DC    AL4(THISOR16),AL1(L'THISOR16),C'P',X'00',X'02'                   
         DC    AL4(THISPD16),AL1(L'THISPD16),C'P',X'00',X'02'                   
         DC    AL4(THISBL16),AL1(L'THISBL16),C'P',X'00',X'02'                   
         DC    AL4(THISIO16),AL1(L'THISIO16),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM17),AL1(L'THISYM17),C'T',2X'00'                        
         DC    AL4(THISOR17),AL1(L'THISOR17),C'P',X'00',X'02'                   
         DC    AL4(THISPD17),AL1(L'THISPD17),C'P',X'00',X'02'                   
         DC    AL4(THISBL17),AL1(L'THISBL17),C'P',X'00',X'02'                   
         DC    AL4(THISIO17),AL1(L'THISIO17),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM18),AL1(L'THISYM18),C'T',2X'00'                        
         DC    AL4(THISOR18),AL1(L'THISOR18),C'P',X'00',X'02'                   
         DC    AL4(THISPD18),AL1(L'THISPD18),C'P',X'00',X'02'                   
         DC    AL4(THISBL18),AL1(L'THISBL18),C'P',X'00',X'02'                   
         DC    AL4(THISIO18),AL1(L'THISIO18),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM19),AL1(L'THISYM19),C'T',2X'00'                        
         DC    AL4(THISOR19),AL1(L'THISOR19),C'P',X'00',X'02'                   
         DC    AL4(THISPD19),AL1(L'THISPD19),C'P',X'00',X'02'                   
         DC    AL4(THISBL19),AL1(L'THISBL19),C'P',X'00',X'02'                   
         DC    AL4(THISIO19),AL1(L'THISIO19),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM20),AL1(L'THISYM20),C'T',2X'00'                        
         DC    AL4(THISOR20),AL1(L'THISOR20),C'P',X'00',X'02'                   
         DC    AL4(THISPD20),AL1(L'THISPD20),C'P',X'00',X'02'                   
         DC    AL4(THISBL20),AL1(L'THISBL20),C'P',X'00',X'02'                   
         DC    AL4(THISIO20),AL1(L'THISIO20),C'P',X'00',X'02'                   
*                                                                               
         DC    AL4(THISYM21),AL1(L'THISYM21),C'T',2X'00'                        
         DC    AL4(THISOR21),AL1(L'THISOR21),C'P',X'00',X'02'                   
         DC    AL4(THISPD21),AL1(L'THISPD21),C'P',X'00',X'02'                   
         DC    AL4(THISBL21),AL1(L'THISBL21),C'P',X'00',X'02'                   
         DC    AL4(THISIO21),AL1(L'THISIO21),C'P',X'00',X'02'                   
         DC    X'FF'               END OF TABLE                                 
*                                                                               
DUMMYNAM EQU   *                                                                
THISBLNK DC   C' '                                                              
*                                                                               
THISREC  DS    0D                                                               
*                                                                               
THISAGY  DS    CL2                                                              
THISMED  DS    CL1                                                              
THISAGMD DS    CL1               BLANK FOR PRINT                                
THISCLT  DS    CL3                                                              
THISCACC DS    CL3               BINARY ZEROS FOR PRINT                         
THISPRD  DS    CL3                                                              
THISEST  DS    CL3                                                              
*                                                                               
THISTYPE DS    CL1                                                              
THISMDOF DS    CL2                                                              
THISACOF DS    CL2                                                              
         DS    CL6                                                              
*                                                                               
         DS    0D                                                               
THISCMB  DS    PL8                 CURRENT MONTH BILLED                         
THISNPT  DS    PL8                 NET PAID TODAY                               
*                                                                               
THISMDNM DS    CL24                                                             
THISCLNM DS    CL24                                                             
THISPRNM DS    CL24                                                             
THISESNM DS    CL24                                                             
*                                                                               
THISYM01 DS    CL4                                                              
THISYM02 DS    CL4                                                              
THISYM03 DS    CL4                                                              
THISYM04 DS    CL4                                                              
THISYM05 DS    CL4                                                              
THISYM06 DS    CL4                                                              
THISYM07 DS    CL4                                                              
THISYM08 DS    CL4                                                              
THISYM09 DS    CL4                                                              
THISYM10 DS    CL4                                                              
THISYM11 DS    CL4                                                              
THISYM12 DS    CL4                                                              
THISYM13 DS    CL4                                                              
THISYM14 DS    CL4                                                              
THISYM15 DS    CL4                                                              
THISYM16 DS    CL4                                                              
THISYM17 DS    CL4                                                              
THISYM18 DS    CL4                                                              
THISYM19 DS    CL4                                                              
THISYM20 DS    CL4                                                              
THISYM21 DS    CL4                                                              
         DS    CL4                 ROOM FOR X'FF' TERMINATOR                    
         DS    XL6                                                              
*                                                                               
         DS    0D                                                               
THISIO01 DS    PL8                 ORDERED $ BY INSERTION MONTH                 
THISIO02 DS    PL8                                                              
THISIO03 DS    PL8                                                              
THISIO04 DS    PL8                                                              
THISIO05 DS    PL8                                                              
THISIO06 DS    PL8                                                              
THISIO07 DS    PL8                                                              
THISIO08 DS    PL8                                                              
THISIO09 DS    PL8                                                              
THISIO10 DS    PL8                                                              
THISIO11 DS    PL8                                                              
THISIO12 DS    PL8                                                              
THISIO13 DS    PL8                                                              
THISIO14 DS    PL8                                                              
THISIO15 DS    PL8                                                              
THISIO16 DS    PL8                                                              
THISIO17 DS    PL8                                                              
THISIO18 DS    PL8                                                              
THISIO19 DS    PL8                                                              
THISIO20 DS    PL8                                                              
THISIO21 DS    PL8                                                              
*                                                                               
         DS    0D                                                               
THISOR01 DS    PL8               ORDERED $ BY BILLABLE MONTH                    
THISOR02 DS    PL8                                                              
THISOR03 DS    PL8                                                              
THISOR04 DS    PL8                                                              
THISOR05 DS    PL8                                                              
THISOR06 DS    PL8                                                              
THISOR07 DS    PL8                                                              
THISOR08 DS    PL8                                                              
THISOR09 DS    PL8                                                              
THISOR10 DS    PL8                                                              
THISOR11 DS    PL8                                                              
THISOR12 DS    PL8                                                              
THISOR13 DS    PL8                                                              
THISOR14 DS    PL8                                                              
THISOR15 DS    PL8                                                              
THISOR16 DS    PL8                                                              
THISOR17 DS    PL8                                                              
THISOR18 DS    PL8                                                              
THISOR19 DS    PL8                                                              
THISOR20 DS    PL8                                                              
THISOR21 DS    PL8                                                              
*                                                                               
         DS    0D                                                               
THISPD01 DS    PL8                 PAID $ BY BILLABLE MONTH                     
THISPD02 DS    PL8                                                              
THISPD03 DS    PL8                                                              
THISPD04 DS    PL8                                                              
THISPD05 DS    PL8                                                              
THISPD06 DS    PL8                                                              
THISPD07 DS    PL8                                                              
THISPD08 DS    PL8                                                              
THISPD09 DS    PL8                                                              
THISPD10 DS    PL8                                                              
THISPD11 DS    PL8                                                              
THISPD12 DS    PL8                                                              
THISPD13 DS    PL8                                                              
THISPD14 DS    PL8                                                              
THISPD15 DS    PL8                                                              
THISPD16 DS    PL8                                                              
THISPD17 DS    PL8                                                              
THISPD18 DS    PL8                                                              
THISPD19 DS    PL8                                                              
THISPD20 DS    PL8                                                              
THISPD21 DS    PL8                                                              
*                                                                               
         DS    0D                                                               
THISBL01 DS    PL8                   BILLED $ BY BILLABLE MONTH                 
THISBL02 DS    PL8                   (FROM BILLING HEADER RECORDS)              
THISBL03 DS    PL8                                                              
THISBL04 DS    PL8                                                              
THISBL05 DS    PL8                                                              
THISBL06 DS    PL8                                                              
THISBL07 DS    PL8                                                              
THISBL08 DS    PL8                                                              
THISBL09 DS    PL8                                                              
THISBL10 DS    PL8                                                              
THISBL11 DS    PL8                                                              
THISBL12 DS    PL8                                                              
THISBL13 DS    PL8                                                              
THISBL14 DS    PL8                                                              
THISBL15 DS    PL8                                                              
THISBL16 DS    PL8                                                              
THISBL17 DS    PL8                                                              
THISBL18 DS    PL8                                                              
THISBL19 DS    PL8                                                              
THISBL20 DS    PL8                                                              
THISBL21 DS    PL8                                                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
*                                                                               
PMED     DS    CL1                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PMOS     DS    CL6                                                              
         DS    CL1                                                              
PORD     DS    CL10                                                             
         DS    CL1                                                              
PPAID    DS    CL10                                                             
         DS    CL1                                                              
PBILLED  DS    CL10                                                             
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
*                                                                               
       ++INCLUDE PPREPWORK                                                      
         PRINT ON                                                               
PPWORKD  DSECT                                                                  
         ORG   QPROG+52            USED TO BE COL 28 (QPROG+27)                 
QINVDATE DS    CL6                                                              
QDUEDAYS DS    CL2                                                              
         ORG   QPROG+64            COL 65-68                                    
QMANCD   DS    CL4                 4 BYTES BINARY                               
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK2                                                     
         PRINT ON                                                               
         ORG   QAREA2+26           COL 27 CARD 2                                
QMANUAL  DS    CL9                 G OR P FOLLOWED BY 4 BINARY                  
         ORG   QAREA2+39           CARD 2 COL 40                                
QPRIOR   DS    0CL3                                                             
QPRIORMO DS    CL2                 NUMBER OF PRIOR MONTHS - REQ                 
QPRIORMD DS    CL1                 C'T' (TOGETHER) SUPPORTED NOW                
         ORG                                                                    
PBKRECD  DSECT                                                                  
       ++INCLUDE PBKREC                                                         
       ++INCLUDE DDBKELEM                                                       
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
         EJECT                                                                  
       ++INCLUDE PPMODEQU                                                       
         EJECT                                                                  
       ++INCLUDE DDLOGOD                                                        
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE PBLPSTEL                                                       
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011PPREPA902 07/17/18'                                      
         END                                                                    
