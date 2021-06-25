*          DATA SET SPTY02     AT LEVEL 065 AS OF 04/18/07                      
*PHASE SPTY02A,*                                                                
         TITLE 'SPTY02 CREATE NEW TRAFFIC STATION RECS-INIT'                    
*                                                                               
***********************************************************************         
*                                                                     *         
* BG LEV  62 AUG20/98 BYPASS CABLE STATIONS                           *         
*                     PROVIDED BY AMS                                 *         
* SM LEV  65 APR05/07 FIX F1 ELEM, RECORD LENGTH & REMOVE HARD CODE   *         
*                                                                     *         
***********************************************************************         
SPTY02   START 0                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPTY02**                                                       
SRCEAGY  EQU   X'C0'               SJR                                          
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING SPTY02,RB,R7                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    SPTRA10                                                          
EXIT     XIT1                                                                   
SPTRA10  DS    0H                                                               
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         STM   R7,RB,SPTYRR                                                     
         OPEN  (TRASTP,(OUTPUT))                                                
         L     R6,=A(WORKO)                                                     
         GOTO1 DATCON,DMCB,(5,0),(3,DATE3)                                      
         TITLE 'SPTY02 CREATE NEW TRAFFIC STATION RECS-SPOT FILE OPEN'          
         L     R2,=A(WORKI)                                                     
         ST    R2,AREC                                                          
         MVC   COMMAND,DMRDHI                                                   
         MVC   FILE,=C'TRFDIR'                                                  
         USING STARECD,R2          OLD REC FORMAT                               
         XC    STADDKEY,STADDKEY                                                
         MVC   STAKID,=X'0A28'       FILE CODE                                  
         MVI   STAKAM,SRCEAGY       START THIS AGENCY                           
         MVC   KEY,STADDKEY                                                     
         MVC   KEY1,STADDKEY                                                    
         MVC   KEYSAVE,STADDKEY                                                 
         SPACE 1                                                                
* DO FIRST READ                                                                 
         SPACE 1                                                                
         GOTO1 DATAMGR,DMCB,                                           C        
               DMRDHI,             PASS RECORD LENGTH, COMMAND         C        
               =C'TRFDIR',         FILE NAME                           C        
               KEY,                KEY ADDRESS                         C        
               KEY                 WORK AREA ADDRESS                            
*                                                                               
         XC    WORKI,WORKI                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,                                           C        
               GETREC,             PASS RECORD LENGTH, COMMAND         C        
               =C'TRFFILE',        FILE NAME                           C        
               KEY+14,             KEY ADDRESS                         C        
               (R2),               WORK AREA ADDRESS                   C        
               DMWORK                                                           
         TITLE 'SPTY02 CREATE NEW TRAFFIC STATION RECS-SPOT'                    
* CONVERT SPOT FILE STATION RECORDS LOOP                                        
         SPACE 1                                                                
SPTRA40  DS    0H                                                               
         TM    DMCB+8,X'80'        IS THIS END OF FILE?                         
         BO    SPTRA60                                                          
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   STAKSTA,C'0'        BYPASS CABLE                                 
         BNL   SPTRA58                                                          
         SPACE                                                                  
         CLI   STAKSTA,X'00'       BYPASS NULL                                  
         BE    SPTRA58                                                          
         SPACE                                                                  
         AP    RECRDCTR,=P'1'                                                   
         CLC   STAKID,=X'0A28'     IS THIS RECORD ID CODE                       
         BH    SPTRA60             NO, TREAT AS EOF                             
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         MVI   WORK,0                                                           
         MVZ   WORK(1),STAKAM                                                   
         CLI   WORK,SRCEAGY     THIS SOURCE TEST AGENCY?                        
         BL    SPTRA58             NO, GET NEXT REC                             
         BH    SPTRA60             NO, TREAT AS EOF                             
         AP    INCTR,=P'1'                                                      
         MVI   WORK,0                                                           
         MVN   WORK(1),STAKAM                                                   
         CLI   WORK,2             IS IT RADIO                                   
         BNE   SPTRA42             NO, CK IF TV                                 
         AP    MEDRCTR,=P'1'                                                    
         B     SPTRA46                                                          
SPTRA42 CLI    WORK,1             IS IT TV                                      
         BNE   SPTRA44             NO, UNKNOWN                                  
         AP    MEDTCTR,=P'1'                                                    
         B     SPTRA46                                                          
SPTRA44  AP    MEDUCTR,=P'1'                                                    
         B     SPTRA58             BYPASS CONVERTING UNKNOWN                    
         SPACE                                                                  
* PRINT OUT INPUT RECORD IN HEX                                                 
         SPACE 1                                                                
SPTRA46  CLC    QOPT2(4),=CL4'TEST' TEST RUN?                                   
         BNE   SPTRA54             NO                                           
         MVI   WORK,0                                                           
         MVN   WORK(1),STAKAM                                                   
         CLI   WORK,1             IS IT TV                                      
         BE    SPTRA50             YES                                          
         CP    MEDRCTR,MAX         *****AT END OF TESTING RADIO                 
         BL    SPTRA52             *****TESTING                                 
         CP    MEDTCTR,MAX         *****AT END OF TESTING TV                    
         BL    SPTRA58             *****TESTING NO                              
         B     SPTRA60                                                          
SPTRA50  CP    MEDTCTR,MAX         *****AT END OF TESTING TV                    
         BL    SPTRA52             *****TESTING                                 
         CP    MEDRCTR,MAX         *****AT END OF TESTING RADIO                 
         BL    SPTRA58             *****TESTING                                 
         B     SPTRA60             *****YES                                     
SPTRA52  MVC   P(5),=CL5'INPUT'                                                 
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,STADDKEY,P,13,0,0                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,STADDKEY+13,P,11,0,0                                 
         GOTO1 HEXOUT,DMCB,STADTAEL,P+30,2,0,0                                  
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,STALINE1,P+4,20,0,0                                  
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,STALINE2,P+4,24,0,0                                  
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,STALINE3,P+4,24,0,0                                  
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,STALINE4,P+4,24,0,0                                  
         GOTO1 HEXOUT,DMCB,STATWX,P+12,20,0,0                                   
         GOTO1 REPORT                                                           
         SPACE                                                                  
SPTRA54  AP    CNVCTR,=P'1'        ADD TO RECS CONVERTED COUNT                  
         MVI   WORK,0                                                           
         MVN   WORK(1),STAKAM                                                   
         CLI   WORK,1            TV                                             
         BE    SPTRA54T                                                         
         CLI   WORK,2            RADIO                                          
         BE    SPTRA56T                                                         
         DC    H'0'                                                             
SPTRA54T AP    CNVTCTR,=P'1'                                                    
         B     *+10                                                             
SPTRA56T AP    CNVRCTR,=P'1'                                                    
         NI    STAKAM,X'0F'                                                     
         OI    STAKAM,NEWAGY       AGY                                          
         MVC   STAAGYA,NEWAGYA     DS CL2 AGENCY ALPHA                          
         BAS   RE,BLDACT           BUILD ACTIVITY ELEMENT                       
*                                  DS XL3 DELETE DATE-AS ZEROS                  
         AP    OTCTR,=P'1'                                                      
         LM    RE,RF,=A(WORKI,WORKO)                                            
         MVC   0(256,RF),0(RE)                                                  
         CLC   QOPT2(4),=CL4'TEST' TEST RUN?                                    
         BNE   SPTRA57             NO                                           
         GOTO1 PTNEWREC            PRINT CONVERTED RECORD IN HEX                
* PUT CONVERTED RECORD OUT TO TAPE                                              
         SPACE 1                                                                
SPTRA57  SR    R1,R1                                                            
         ICM   R1,3,13(R2)                                                      
         AHI   R1,4                                                             
         STCM  R1,3,VARFLD                                                      
*****    MVC   VARFLD(2),13(R2)    SET LENGTH                                   
         PUT   TRASTP,VARFLD                                                    
         SPACE 1                                                                
* GET NEXT NEW TRAFFIC SPOT FILE STATION RECORD                                 
         SPACE 1                                                                
SPTRA58 GOTO1 DATAMGR,DMCB,                                            C        
               DMRSEQ,             READ SEQUENTIAL                     C        
               =C'TRFDIR',         ADDR OF FILE                        C        
               KEY,                KEY ADDRESS                         C        
               KEY                 WORK AREA ADDRESS                            
*                                                                               
         XC    WORKI,WORKI                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,                                           C        
               GETREC,             PASS RECORD LENGTH, COMMAND         C        
               =C'TRFFILE',        FILE NAME                           C        
               KEY+14,             KEY ADDRESS                         C        
               (R2),               WORK AREA ADDRESS                   C        
               DMWORK                                                           
         B     SPTRA40                                                          
         EJECT                                                                  
* FORMAT TOTALS & CLOSE OUTPUT TAPE                                             
         SPACE 1                                                                
SPTRA60  DS    0H                                                               
         CLOSE (TRASTP)                                                         
         MVC   P(8),=CL8'AGENCY ='                                              
         MVC   P+8(2),AGY                                                       
         GOTO1 REPORT                                                           
         MVC   P(36),FILCONV                                                    
         GOTO1 REPORT                                                           
         MVC   P(17),=CL17'TOTAL RECS READ ='                                   
         EDIT  RECRDCTR,(7,P+24),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P(18),=CL18'AGENCY RECS READ ='                                  
         EDIT  INCTR,(7,P+24),COMMAS=YES                                        
         GOTO1 REPORT                                                           
         MVC   P(20),=CL20'MEDIA R RECS READ ='                                 
         EDIT  MEDRCTR,(7,P+24),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P(20),=CL20'MEDIA T RECS READ ='                                 
         EDIT  MEDTCTR,(7,P+24),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P(21),=CL21'MEDIA UNK RECS READ ='                               
         EDIT  MEDUCTR,(7,P+24),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P(19),=CL19'TV RECS CONVERTED ='                                 
         EDIT  CNVTCTR,(7,P+24),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P(22),=CL22'RADIO RECS CONVERTED ='                              
         EDIT  CNVRCTR,(7,P+24),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P(16),=CL16'RECS CONVERTED ='                                    
         EDIT  CNVCTR,(7,P+24),COMMAS=YES                                       
         GOTO1 REPORT                                                           
         MVC   P(14),=CL14'ZIP ERR RECS ='                                      
         EDIT  ERRCTR,(7,P+24),COMMAS=YES                                       
         GOTO1 REPORT                                                           
         MVC   P(22),=CL22'RECS WRITTEN TO TAPE ='                              
         EDIT  OTCTR,(7,P+24),COMMAS=YES                                        
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ             FORCE END OF REQUEST                         
         DC    H'0'                SHOULD NEVER COME BACK HERE                  
         SPACE 3                                                                
* BUILD ACTIVITY ELEMENT                                                        
         SPACE                                                                  
BLDACT   NTR1                                                                   
         LA    R6,STADTAEL                                                      
         DROP  R2                                                               
BLDACT10 CLI   0(R6),X'F1'                                                      
         BE    BLDACT20                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   BLDACT10                                                         
         DC    H'0'                                                             
         USING ACTVD,R6                                                         
BLDACT20 DS    0H                                                               
         MVC   ACTVADDT,DATE3      DS XL3 CREATION DATE                         
         MVC   ACTVADID,=CL2'X2'   ID FROM CONVERSION                           
* LEAVE REST OF ELEMENT - BINARY ZEROS                                          
*******  XC    STAADDFL(13),STAADDFL   ***** OLD DSECT DO NOT USE ***           
*******  XC    ACTVADFL(ACTVLENQ-(ACTVADFL-ACTVEL)),ACTVADFL                    
         ZIC   RF,ACTVLEN          ELEMENT LEN                                  
         LA    RE,ACTVADFL-ACTVEL                                               
         SR    RF,RE                                                            
*NOP*    BCTR  RF,0                MINUS 1 FOR XC                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    ACTVADFL(0),ACTVADFL                                             
         B     EXIT                                                             
         DROP  R6                                                               
         USING STARECD,R2                                                       
         EJECT                                                                  
* PRINT OUTPUT RECORD IN HEX                                                    
         SPACE 1                                                                
PTNEWREC NTR1                                                                   
         MVC   P(6),=CL6'OUTPUT'                                                
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,WORKO,P,13,0,0 KEY                                   
         GOTO1 HEXOUT,DMCB,WORKO+13,P+30,11,0,0 CONTROL                         
         MVC   P+53(3),=CL3'KEY'                                                
         MVC   P+60(13),WORKO                                                   
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,WORKO+24,P+90,2,0,0  ELEM CODE/ELEM LEN              
         MVC   P+96(13),=CL13'ELEM CODE/LEN'                                    
         GOTO1 HEXOUT,DMCB,WORKO+26,P,24,0,0 ADDR 1                             
         MVC   P+52(6),=CL6'LINE 1'                                             
         MVC   P+60(24),WORKO+26                                                
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,WORKO+50,P,24,0,0 ADDR 2                             
         MVC   P+52(6),=CL6'LINE 2'                                             
         MVC   P+60(24),WORKO+50                                                
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,WORKO+74,P,24,0,0 ADDR 3                             
         MVC   P+52(6),=CL6'LINE 3'                                             
         MVC   P+60(24),WORKO+74                                                
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,WORKO+98,P,24,0,0 ADDR 4                             
         MVC   P+52(6),=CL6'LINE 4'                                             
         MVC   P+60(24),WORKO+98                                                
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,WORKO+122,P,21,0,0 TWIX                              
         MVC   P+60(21),WORKO+122                                               
         MVC   P+52(4),=CL4'TWIX'                                               
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,WORKO+143,P,20,0,0 ACTIVITY ELEM                     
         MVC   P+52(8),=CL8'ACT ELEM'                                           
         MVC   P+80(24),WORKO+143                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         DS    0D                                                               
         USING *,RF                                                             
         DROP  R7,R9,RA,RB                                                      
HDHK     NTR1                                                                   
         LM    R7,RB,SPTYRR                                                     
         USING SPWORKD,RA,R9                                                    
         USING SPTY02,RB,R7                                                     
         B     HDHK10                                                           
SPTYRR   DC    5F'0'                                                            
HDHK10   MVC   H1+52(2),NEWAGYA                                                 
         B     EXIT                                                             
NEWAGY   EQU   X'C0'                                                            
NEWAGYA  DC    C'BJ'                                                            
MAX      DC    P'50'              *****TESTING                                  
RECRDCTR DC    PL5'0'              TOTAL RECS READ                              
INCTR    DC    PL5'0'              TOT RECS READ-THIS AGENCY                    
MEDRCTR  DC    PL5'0'              MEDIA - R RECS READ                          
MEDTCTR  DC    PL5'0'              MEDIA - T RECS READ                          
MEDUCTR  DC    PL5'0'              MEDIA - UNKNOWN RECS READ                    
CNVTCTR  DC    PL5'0'              TV RECS CONVERTED                            
CNVRCTR  DC    PL5'0'              RADIO RECS CONVERTED                         
CNVCTR   DC    PL5'0'              ALL RECS CONVERTED                           
MTRCTR   DC    PL5'0'              MASTER STATION RECS RD                       
OTCTR    DC    PL5'0'              RECS WRITTEN                                 
ERRCTR   DC    PL5'0'              ERRORS FOUND-RECS WRITTEN ANYWAY             
DATE3    DC    XL3'00'             TODAYS DATE BINARY                           
FILCONV  DC    CL36' '             FILE CONVERSION TITLE                        
         EJECT                                                                  
         LTORG                                                                  
* OUTPUT TAPE - TO BE USED AS LOAD TAPE TO SPOT FILE                            
TRASTP   DCB   BLKSIZE=8000,                                           C        
               BUFNO=2,                                                C        
               DDNAME=TRASTP,                                          C        
               DEVD=DA,                                                C        
               DSORG=PS,                                               C        
               LRECL=208,                                              C        
               MACRF=(PM),                                             C        
               RECFM=VB                                                         
VARFLD   DS    0F                                                               
         DC    H'0'                                                             
         DC    H'0'                                                             
WORKO    DS    CL256                                                            
WORKI    DS    CL256                                                            
STAKEYSV DS    CL17                                                             
         EJECT                                                                  
* NEW TRAFFIC SYSTEM STATION NAME AND ADDRESS RECORD LAYOUT                     
         SPACE 1                                                                
       ++INCLUDE SPTRSTA                                                        
STAEND   EQU   *                                                                
         EJECT                                                                  
* NEW DDGENCON SYSTEM ACTIVITY ELEMENT                                          
         SPACE 1                                                                
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
SPTY02   CSECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065SPTY02    04/18/07'                                      
         END                                                                    
