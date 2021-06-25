*          DATA SET DEDEM0B    AT LEVEL 154 AS OF 11/17/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T21B0BB                                                                  
T21B0B   TITLE 'DEDEM0B - INIT DOWNLOAD                '                        
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* FEB07/02 116 BPOO - CHANGE DMA PREVIEW BOOKTYPE DESCRIPTION AND     *         
*                     ADD MISC EXLCUSIONS                             *         
* MAR07/01 114 BPOO - CHANGE OPTION 38 TO READ FROM MATIRX GRID       *         
*                     WHEN PROFILE IS NOT SET...CURRENTLY ALWAYS C'R'           
* NOV07/00 112 BPOO - BBM WEEKLY SUPPORT                              *         
* NOV02/00 111 BPOO - break up dem00 tables into dem81 phase          *         
* OCT24/00 109 BPOO - SAME AS DEM0BB BUT LINK IN NEW DEMATRIX1  WITH  *         
*                   - SOME FIL/SRC TAKEN OUT                                    
* OCT17/00 108 BPOO - SUPPORT MMU UPGRADE                             *         
* SEP22/00 107 BPOO - falink stuff...change screen one line lower     *         
* Jul24/00 106 GLEE - Support for "VAR" and "AVGn" day codes          *         
*                                                                     *         
* Mar08/00 105 GLEE - Relinked for bigger buy record I/O area         *         
*                                                                     *         
* Feb03/00 101 GLEE - Support for start/end times for each source/file*         
*                                                                     *         
* Jan24/00 100 GLEE - Look up methodology profile                     *         
*                                                                     *         
* Sep13/99 099 GLEE - Set CC upon exiting DEMPROC mode                *         
*                                                                     *         
* Aug05/99 098 GLEE - Disable  SVI  option for non-SPOT sessions      *         
*                                                                     *         
* Jun15/99 097 GLEE - Suppress PAV sources for non-REP sessions       *         
*                                                                     *         
* Feb11/99 092 GLEE - Pass book formats to DEM32                      *         
*                                                                     *         
* AUG02/98   001 BPOO - CONTROL GRID DOWNLOAD                         *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
DEM0B    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DEMB**,RA,R5,RR=RE                                           
         USING DEMWRKD,R9          R9=A(GLOBAL W/S)                             
         USING DEMTWAD,R8          R8=A(TWA)                                    
         L     R7,AAPWORK                                                       
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
         ST    RE,RELO0B                                                        
*                                  HANDLE CONTROLLER MODE SETTINGS              
         CLI   APMODE,FORMHEAD     READ & POST RECORDS TO BUFFER                
         BE    DEMHEAD                                                          
         CLI   APMODE,PROCESS      READ & POST RECORDS TO BUFFER                
         BE    DEMPROC                                                          
         CLI   APMODE,FORMLINE     FORMAT A PRINT A BUFFER RECORD               
         BE    DEMLINE                                                          
*                                                                               
EXITH    DS    0H                  EXIT W/ CC HIGH                              
         LA    R0,1                                                             
         J     EXITCR                                                           
                                                                                
EXITL    DS    0H                  EXIT W/ CC LOW                               
         LHI   R0,-1                                                            
         J     EXITCR                                                           
                                                                                
EXITE    DS    0H                  EXIT W/ CC EQUAL                             
         SR    R0,R0                                                            
         J     EXITCR                                                           
                                                                                
EXITCR   DS    0H                                                               
         AR    R0,R9                                                            
         CR    R0,R9               SET CC                                       
         J     EXIT                 AND EXIT                                    
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
* FORMAT HEADLINES & INITIALIZE                                                 
*  nothing in demhead right now                                                 
DEMHEAD  DS    0H                                                               
DEMHD10  DS    0H                  THIS PART FOR STEREO SESSION ONLY            
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         CLI   TSKEYL,0            SEE IF WE NEED TO SET LENGTHS AGAI           
         BNE   DEMHD20              NOPE, THEY WERE SET BEFORE                  
                                                                                
         MVI   TSKEYL,TDRKEYL      SET KEY LENGTH                               
                                                                                
         LA    R0,TDRRECL+2                                                     
         STH   R0,TSRECL           SET MAX LENGTH OF RECORD                     
*                                                                               
DEMHD20  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* READ DEMO FILES & POST TO BINSRCH BUFFER                                      
*                                                                               
DEMPROC  DS    0H                                                               
         BAS   RE,GETPROFL         GET USER PROFILES                            
                                                                                
*                                                                               
         DS    0H                                                               
*****    LA    R5,DBLOCK1          INITIALIZE DBLOCK FOR MARKET READS           
*                                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R1,R1                                                            
         ICM   R1,7,TSAREC+1                                                    
         MVC   0(2,R1),TSRECL      MOVE IN LENGTH OF RECORD                     
         LA    R1,2(R1)                                                         
                                                                                
         USING TSDEMRCD,R1                                                      
         MVC   TDRBOOK,=X'0260'     MOVE IN DUMMY BOOK                          
         MVI   TDRTYPE,TDRTBKQ      RECORD TYPE,                                
         MVI   TDRDUMMY,0           AND A DUMMY VALUE                           
         DROP  R1                                                               
                                                                                
         GOTO1 APOST                                                            
                                                                                
*                                                                               
DEMPRCX  DS    0H                                                               
*&&DO                                                                           
         B     EXIT                                                             
*&&                                                                             
         B     EXITE                                                            
         EJECT                                                                  
         EJECT                                                                  
* FORMAT PRINT LINES                                                            
*----------------------------------------------------------------------         
DEMLINE  DS    0H                                                               
         CLC   =C'PINIT',DUMACT                                                 
         BE    PDEMLINE             PROPOSER DEMLINE                            
*                                                                               
         LA    R6,0                 COLUMN DISPLACENMENT                        
*                                                                               
DMLP     DS    0H                  START OF LOOP                                
         LA    RF,FILES+3           CHECK TO SUPPRESS FILES                     
         ZIC   R3,FILES+2                                                       
         LR    RE,R3                                                            
         MR    R2,R6                                                            
         AR    RF,R3                 RF-->FILE CODE                             
*                                                                               
         CLC   =C'RTV',0(RF)         SKIP ARBITRON TV                           
         BE    DMLPNEXT                                                         
***      CLC   =C'OTP',0(RF)         SKIP overnights FOR NOW                    
***      BE    DMLPNEXT                                                         
***      CLC   =C'CTP',0(RF)         SKIP COUNTY COVERAGE FOR NOW               
***      BE    DMLPNEXT     *** NO COUNTY COVERAGE FOR NOW **                   
*                                                                               
*        BNE   *+14                                                             
*        CLC   D32PCVER,=AL4(XTRCVER3) MAKE SURE CORRECT VERSION FOR            
*        BL    DMLPNEXT                COUNTY COVERAGE                          
DMLP001  CLC   =C'CTP',0(RF)           ONLY NEW VERSION OF DEM32                
         BNE   *+14                    GETS COUNTY COVERAGE                     
         CLC   D32PCVER,=AL4(XTRCVER9)                                          
         BL    DMLPNEXT                                                         
*                                                                               
         CLC   =C'OPA',0(RF)                                                    
         BE    *+10                                                             
         CLC   =C'PAV',0(RF)         ARE WE LOOKING AT PAV FILE?                
         BNE   DMLP005G                   NOPE                                  
         L     RF,ASYSNTRY                                                      
         CLI   (SYSOVSYS-SYSTABD)(RF),8   YEP, SYSTEM MUST BE REP               
         BNE   DMLPNEXT                    OTHERWISE, SKIP THIS SRC/FIL         
DMLP005G EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   FALEMPC,=Y(FMHCTGRID)                                            
         SR    R1,R1                                                            
         GOTO1 ADM32SET                                                         
*                                                                               
         MVC   FALDMPC,=Y(FMDOPT1) SOURCE DESCRIPTION                           
         SR    R3,R3                                                            
         LA    RF,SOURCES+3          FIRST SRC DESCRIPTION ENTRY                
         ZIC   R3,SOURCES+2                                                     
         MR    R2,R6                                                            
         AR    RF,R3                 ADD COLUMN NUMBER                          
         ZIC   RE,SOURCES+2                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RF)                                                    
         MVC   SVSOURCE,0(RF)                                                   
*&&DO                                                                           
         CLC   =C'FUS',0(RF)         ONLY DDS GETS FUSION FOR NOW               
         BNE   *+12                                                             
         CLI   DDS,C'Y'                                                         
         BNE   DMLPNEXT                                                         
*&&                                                                             
         LA    RE,1(RE)                                                         
         LA    R0,WORK                                                          
         LR    R1,RE                                                            
         BAS   RE,D32DLGD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDOPT2) SOURCE DESCRIPTION                           
         LA    RF,SRCDIS+3           FIRST SRC DESCRIPTION ENTRY                
         SR    R3,R3                                                            
         ZIC   R3,SRCDIS+2                                                      
         MR    R2,R6                                                            
         AR    RF,R3                 ADD COLUMN NUMBER                          
         ZIC   RE,SRCDIS+2                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    R0,WORK                                                          
         LR    R1,RE                                                            
         BAS   RE,D32DLGD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDOPT3)     FILE DESCRIPTION                         
         LA    RF,FILES+3              FIRST FIL DESCRIPTION ENTRY              
         SR    R3,R3                                                            
         ZIC   R3,FILES+2                                                       
         MR    R2,R6                                                            
         AR    RF,R3                   ADD COLUMN NUMBER                        
         ZIC   RE,FILES+2                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RF)                                                    
         MVC   SVFILECD,WORK                                                    
         LA    RE,1(RE)                                                         
         LA    R0,WORK                                                          
         LR    R1,RE                                                            
         BAS   RE,D32DLGD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDOPT4)     FILE DESCRIPTION                         
         LA    RF,FILEDISC+3           FIRST FIL DESCRIPTION ENTRY              
         SR    R3,R3                                                            
         ZIC   R3,FILEDISC+2                                                    
         MR    R2,R6                                                            
         AR    RF,R3                 ADD COLUMN NUMBER                          
         ZIC   RE,FILEDISC+2                                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    R0,WORK                                                          
         LR    R1,RE                                                            
         BAS   RE,D32DLGD                                                       
*                                                                               
*============ DO ALL THE AVAILABLE OPTIONS                                      
*                                                                               
****     LA    R2,M1ACTDIS                                                      
         L     R2,=A(M1ACTDIS)                                                  
         A     R2,RELO0B                                                        
ACTLP    MVC   FALDMPC,0(R2)                                                    
         XC    WORK,WORK                                                        
         LA    RE,ENTDISP(R2)                                                   
         AR    RE,R6                 GET N'TH COLUMN ENTRY                      
         MVC   WORK(1),0(RE)                                                    
*                                                                               
         DS    0H                                                               
         CLC   FALDMPC,=AL2(FMDOPT38)    IF WORKING ON DMA OPTION,              
         BNE   ACTLP038X                                                        
         MVC   WORK(1),PRECPROF                                                 
*                                                                               
         OC    PRECPROF,PRECPROF                                                
         BNZ   *+8                                                              
         MVI   WORK,C'R'                                                        
ACTLP038X EQU  *                                                                
*                                                                               
         DS    0H                                                               
         CLC   FALDMPC,=AL2(FMDOPT39)    IF WORKING ON SVI OPTION,              
         BNE   ACTLP039X                                                        
         L     RF,ASYSNTRY                                                      
         CLI   (SYSOVSYS-SYSTABD)(RF),2   YEP, SYSTEM MUST BE  SPOT             
         BE    *+8                                                              
         MVI   WORK,D                      OTHERWISE, DISABLE IT                
ACTLP039X EQU  *                                                                
*                                                                               
* MMU UPGRADE                                                                   
         CLC   FALDMPC,=AL2(FMDMMU)                                             
         BNE   ACTLP54                   MMU STUFF                              
         CLC   D32PCVER,=AL4(XTRCVER4)   MAKE SURE CORRECT VERSION FOR          
         BL    ACTLP60                   MMU STUFF                              
         B     ACTLP59                                                          
ACTLP54  DS    0H                                                               
         CLC   FALDMPC,=AL2(FMDWMBK)     MULTIWEEK OPTION                       
         BE    ACTLP57                                                          
         CLC   FALDMPC,=AL2(FMDDEC0)     0 DECIMAL                              
         BE    ACTLP57                                                          
         CLC   FALDMPC,=AL2(FMDDEC1)     1 DECIMAL                              
         BE    ACTLP57                                                          
         CLC   FALDMPC,=AL2(FMDDEC2)     2 DECIMAL                              
         BNE   ACTLP59                                                          
ACTLP57  CLC   D32PCVER,=AL4(XTRCVER8)                                          
         BL    ACTLP60                                                          
                                                                                
ACTLP59  LA    R0,WORK                                                          
         LA    R1,1                                                             
         BAS   RE,D32DLGD                                                       
ACTLP60  DS    0H                                                               
                                                                                
         LA    R2,TENTRYL(R2)    BUMP TO NEXT ROW                               
         L     RE,=A(NOMORE-DEM0B)                                              
         A     RE,=A(DEM0B)                                                     
         A     RE,RELO0B                                                        
         CR    R2,RE                                                            
         BL    ACTLP                 ANYMORE ENTRIES?                           
*                                                                               
*======= GET DEMO MODIFIER AND DESCRIPTIONS                                     
*                                                                               
         LA    R4,ADEMOTAB                                                      
         SR    R3,R3                                                            
         LA    R3,ADEMOTL           LENGTH OF ADDR TABLE ENTRY                  
         MR    R2,R6                MULTIPLY WITH COLUMN NUMBER                 
         AR    R4,R3                ADD IT TO START OF ADDR TABLE               
         L     R3,0(R4)             GO TO DEMO MOD TABLE                        
         OR    R3,R3                IF NO TABLES POINTED TO                     
         BZ    DMMLPX               THEN NOTHING FOR THIS FIL\SRC               
         A     R3,RELO0B                                                        
*****    LA    R4,DEMOMODN          REUSE R4 - NUMBER OF TOTAL ENTRIES          
* pass down the delimiter between the  modifiers                                
         MVC   WORK(1),0(R3)                                                    
         LA    R0,WORK                                                          
         LA    R1,1                                                             
         MVC   FALDMPC,=Y(FMDOPT89)                                             
         BAS   RE,D32DLGD                                                       
         LA    R3,1(R3)                                                         
*                                                                               
DMMLP    XC    WORK,WORK                                                        
* FOR OTP - OVERNIGHTS TP IF SIGNED ON AS REP - DONT SHOW IMPRESSIONS           
* AND TSA DEMOTYPES                                                             
         CLC   =C'OTP',SVFILECD                                                 
         BNE   DMMLP10                                                          
         CLC   =C'OPA',SVFILECD       OPA DONT SHOW IMPRESSIONS AND TSA         
         BE    *+12                                                             
         CLI   SIGNON,C'S'                                                      
         BE    DMMLP10                                                          
         CLI   0(R3),C' '                                                       
         BE    DMMLP20                                                          
         CLI   0(R3),C'T'                                                       
         BE    DMMLP20                                                          
         CLI   0(R3),C'Q'                                                       
         BE    DMMLP20                                                          
*                                                                               
DMMLP10  DS    0H                                                               
*                                                                               
         MVC   WORK(DEMOMODL),0(R3)                                             
         LA    R0,WORK                                                          
         LA    R1,DEMOMODL                                                      
         MVC   FALDMPC,=Y(FMDDMMOD)                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(DEMODISL),DEMODISP(R3)  DESCRIPTION                         
         LA    R0,WORK                                                          
         LA    R1,DEMODISL                                                      
         MVC   FALDMPC,=Y(FMDDMDIS)                                             
         BAS   RE,D32DLGD                                                       
DMMLP20  LA    R3,DEMOTABL(R3)                                                  
         CLC   =X'0000',0(R3)                                                   
         BNE   DMMLP                                                            
DMMLPX   DS    0H                                                               
*                                                                               
*======= GET BOOK TYPE CODE AND DESCRIPTIONS                                    
*                                                                               
******GETBKTP  LA    R4,ABOOKTAB                                                
GETBKTP  L     R4,=A(ABOOKTAB)                                                  
         A     R4,RELO0B                                                        
         SR    R3,R3                                                            
         LA    R3,ABOOKTL           LENGTH OF ADDR TABLE ENTRY                  
         MR    R2,R6                MULTIPLY WITH COLUMN NUMBER                 
         AR    R4,R3                ADD IT TO START OF ADDR TABLE               
         L     R3,0(R4)             GO TO BOOK TYPE TABLE                       
         OR    R3,R3                IF NO TABLES POINTED TO                     
         BZ    BKTLPX               THEN NOTHING FOR THIS FIL\SRC               
         A     R3,RELO0B                                                        
                                                                                
BKTLP    XC    WORK,WORK                                                        
* CHECK FOR COUNTY COVERAGE .  WE DONT SEND THE NORMAL BOOKTYPE                 
* TABLE.  WE HAVE TO SEND THE STATE CODES                                       
*                                                                               
         CLC   =C'CTP',SVFILECD                                                 
         BE    BKCOUNTY                                                         
*                                                                               
         MVC   WORK(BKTPCDEL),0(R3)                                             
         LA    R0,WORK                                                          
         LA    R1,BKTPCDEL                                                      
         MVC   FALDMPC,=Y(FMDBKTPC)                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(BKTPDISL),BKTPDISP(R3)  DESCRIPTION                         
         LA    R0,WORK                                                          
         LA    R1,BKTPDISL                                                      
         LA    R3,BKTPTABL(R3)                                                  
         MVC   FALDMPC,=Y(FMDBKTPD)                                             
         BAS   RE,D32DLGD                                                       
*        BCT   R4,BKTLP                                                         
         CLC   =X'0000',0(R3)                                                   
         BNE   BKTLP                                                            
         B     BKTLPX                                                           
BKCOUNTY DS    0H                                                               
****     LA    R3,STACODE                 2 BYTE STATE CODE                     
         L     R3,=A(STACODE)                                                   
         A     R3,RELO0B                                                        
BKTCLP   XC    WORK,WORK                                                        
         MVC   WORK(2),1(R3)                                                    
         LA    R0,WORK                                                          
         LA    R1,2                                                             
         MVC   FALDMPC,=Y(FMDBKTPC)                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(25),3(R3)        DESCRIPTION                                
         LA    R0,WORK                                                          
         LA    R1,25                                                            
         AHI   R3,28                                                            
         MVC   FALDMPC,=Y(FMDBKTPD)                                             
         BAS   RE,D32DLGD                                                       
         CLC   =X'FFFF',0(R3)                                                   
         BNE   BKTCLP                                                           
BKTLPX   DS    0H                                                               
*                                                                               
*                                                                               
*======= GET DAY TIME AVG CODE AND DESCRIPTIONS                                 
*                                                                               
****     LA    R4,ADAYTMTB2                                                     
         L     R4,=A(ADAYTMTB2)                                                 
         A     R4,RELO0B                                                        
         CLC   D32PCVER,=AL4(XTRCVER4)                                          
         BNL   *+12                                                             
****     LA    R4,ADAYTMTB                                                      
         L     R4,=A(ADAYTMTB)                                                  
         A     R4,RELO0B                                                        
                                                                                
         SR    R3,R3                                                            
         LA    R3,ADAYTML           LENGTH OF ADDR TABLE ENTRY                  
         MR    R2,R6                MULTIPLY WITH COLUMN NUMBER                 
         AR    R4,R3                ADD IT TO START OF ADDR TABLE               
         L     R3,0(R4)             GO TO DAY TIME AVG TABLE                    
         OR    R3,R3                IF NO TABLES POINTED TO                     
         BZ    DAYTMLPX             THEN NO DEMOS FOR THIS FIL\SRC              
         A     R3,RELO0B                                                        
DAYTMLP  XC    WORK,WORK                                                        
         MVC   WORK(DYTMCDEL),0(R3)                                             
         LA    R0,WORK                                                          
         LA    R1,DYTMCDEL                                                      
         MVC   FALDMPC,=Y(FMDDYTMC)                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(DYTMDISL),DYTMDISP(R3)  DESCRIPTION                         
         LA    R0,WORK                                                          
         LA    R1,DYTMDISL                                                      
         LA    R3,DAYTMTBL(R3)                                                  
         MVC   FALDMPC,=Y(FMDDYTMD)                                             
         BAS   RE,D32DLGD                                                       
*        BCT   R4,BKTLP                                                         
         CLC   =X'0000',0(R3)   END OF TABLE IS NULLS                           
         BNE   DAYTMLP                                                          
DAYTMLPX DS    0H                                                               
*                                                                               
*======= GET PROGRAM TYPES AND DESCIPTIONS                                      
*                                                                               
****     LA    R4,APRGTPTB                                                      
         L     R4,=A(APRGTPTB)                                                  
         A     R4,RELO0B                                                        
         SR    R3,R3                                                            
         LA    R3,APRGTPL           LENGTH OF ADDR TABLE ENTRY                  
         MR    R2,R6                MULTIPLY WITH COLUMN NUMBER                 
         AR    R4,R3                ADD IT TO START OF ADDR TABLE               
         L     R3,0(R4)             GO TO DEMO MOD TABLE                        
         OR    R3,R3                IF NO TABLES POINTED TO                     
         BZ    PRGTPLPX             THEN NO DEMOS FOR THIS FIL\SRC              
         A     R3,RELO0B                                                        
         XC    WORK,WORK                                                        
         MVC   WORK(2),0(R3)                                                    
         MVC   FALDMPC,=Y(FMDPRGNM)  PASS NUM OF PRG TP CODE                    
         LA    R0,WORK                                                          
         LA    R1,2                                                             
         BAS   RE,D32DLGD                                                       
         LA    R3,2(R3)              GO TO FIRST ENTRY OF TABLE                 
*                                                                               
PRGTPLP  XC    WORK,WORK                                                        
         MVC   WORK(PRGTPCDL),0(R3)                                             
         LA    R0,WORK                                                          
         LA    R1,PRGTPCDL                                                      
         MVC   FALDMPC,=Y(FMDPRGTC)                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(PRGDESPL),PRGDDISP(R3)  DESCRIPTION                         
         LA    R0,WORK                                                          
         LA    R1,PRGDESPL                                                      
         LA    R3,PRGTPTBL(R3)                                                  
         MVC   FALDMPC,=Y(FMDPRGTD)                                             
         BAS   RE,D32DLGD                                                       
*        BCT   R4,BKTLP                                                         
         CLC   =X'0000',0(R3)   END OF TABLE IS NULLS                           
         BNE   PRGTPLP                                                          
PRGTPLPX DS    0H                                                               
         EJECT                                                                  
*                                                                               
*** START/END TIMES FOR RESPECTIVE SOURCE/FILE ***                              
*                                                                               
         DS    0H                                                               
         LR    RF,R6                                                            
         MHI   RF,L'SETIMES                                                     
*****    LA    RF,SETIMES(RF)                                                   
         L     RE,=A(SETIMES)                                                   
         A     RE,RELO0B                                                        
         AR    RF,RE                                                            
                                                                                
*                                                                               
         DS    0H                  EARLIEST START TIME                          
         LA    R0,0(RF)                                                         
         LHI   R1,2                                                             
         MVC   FALDMPC,=Y(FMDSTIM)                                              
         BAS   RE,D32DLGD                                                       
*                                                                               
         DS    0H                  LATEST   END   TIME                          
         LA    R0,2(RF)                                                         
         LHI   R1,2                                                             
         MVC   FALDMPC,=Y(FMDETIM)                                              
         BAS   RE,D32DLGD                                                       
*                                                                               
         DS    0H                  TIME INCREMENTS                              
         LA    R0,4(RF)                                                         
         LHI   R1,1                                                             
         MVC   FALDMPC,=Y(FMDTINC)                                              
         BAS   RE,D32DLGD                                                       
         EJECT                                                                  
*                                                                               
*  affiliation table stuff                                                      
*======= GET DEMO MODIFIER AND DESCRIPTIONS                                     
         CLC   D32PCVER,=AL4(XTRCVER5)   MAKE SURE CORRECT VERSION FOR          
         BL    AFFLPX                                                           
*                                                                               
         LA    R4,AAFFLTAB                                                      
         SR    R3,R3                                                            
         LA    R3,AAFFLTL           LENGTH OF ADDR TABLE ENTRY                  
         MR    R2,R6                MULTIPLY WITH COLUMN NUMBER                 
         AR    R4,R3                ADD IT TO START OF ADDR TABLE               
         L     R3,0(R4)             GO TO DEMO MOD TABLE                        
         OR    R3,R3                IF NO TABLES POINTED TO                     
         BZ    AFFLPX               THEN NOTHING FOR THIS FIL\SRC               
         A     R3,RELO0B                                                        
*                                                                               
AFFLP    XC    WORK,WORK                                                        
         MVC   WORK(AFFLCODL),0(R3)                                             
         LA    R0,WORK                                                          
         LA    R1,AFFLCODL                                                      
         MVC   FALDMPC,=Y(FMDAFFILC)        AFFIL CODE                          
         BAS   RE,D32DLGD                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(AFFLNAML),AFFLCODL(R3)  AFFIL  DESCRIPTION                  
         LA    R0,WORK                                                          
         LA    R1,AFFLNAML                                                      
         LA    R3,AFFLTABL(R3)                                                  
         MVC   FALDMPC,=Y(FMDAFFILN)                                            
         BAS   RE,D32DLGD                                                       
         CLC   =X'0000',0(R3)                                                   
         BNE   AFFLP                                                            
AFFLPX   DS    0H                                                               
*                                                                               
* ------- LOOP BACK TO NEXT COLUMN INFO                                         
DMLPNEXT DS    0H                                                               
         AHI   R6,1                                                             
         CHI   R6,SRCFILN           COMPARE AGAINST MAX SRC FILE COMBO          
         BE    DMLN100                                                          
         B     DMLP                                                             
         EJECT                                                                  
DMLN100  DS    0H                                                               
         MVC   FALEMPC,=Y(FMHBFMT)    FORMAT TYPES FOR BOOKS                    
         SR    R1,R1                                                            
         GOTO1 ADM32SET                                                         
                                                                                
*                                                                               
         DS    0H                     TYPE 1 = MMMYY                            
         MVC   FALDMPC,=Y(FMDBFMT001)                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK,=C'MMMYY'                                                   
         LA    R0,WORK                                                          
         LA    R1,5                                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         DS    0H                     TYPE 2 = MMMDD/YY                         
         MVC   FALDMPC,=Y(FMDBFMT002)                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK,=C'MMMDD/YY'                                                
         LA    R0,WORK                                                          
         LA    R1,8                                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
* comScore SWEEP DATES                                                          
*                                                                               
CSSWPLOP MVC   FALEMPC,=Y(FMCSSWP)    FORMAT TYPES COMSCORE SWEEP DATES         
         SR    R1,R1                                                            
         GOTO1 ADM32SET                                                         
*                                                                               
         L     RF,AFAC                                                          
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),DMCB,SWEEPTBL                                               
         ICM   R3,15,0(R1)                                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            LENGTH OF EACH ENTRY                         
         STC   RF,NTRYLEN          HAS TO FIT IN 1 BYTE                         
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(3,FULL)                                      
         ZIC   RF,FULL             SAVE OFF YEAR                                
         SHI   RF,4                DATA FOR 2012 AND FORWARD                    
         STC   RF,BYTE                                                          
         J     CSSWP20                                                          
*                                                                               
CSSWP10  LLC   RF,NTRYLEN          BUMP TO NEXT ENTRY                           
         AR    R3,RF                                                            
         USING SWPTABD,R3                                                       
*                                                                               
CSSWP20  CLC   SWPTTYPE,=C'TTN'    TEST END OF TABLE                            
         JNE   CSSWPLPX                                                         
         CLC   BYTE,SWPTYR         WITHIN PAST 3 YEARS?                         
         JH    CSSWP10                                                          
*                                                                               
         MVC   FALDMPC,=Y(FMCSFILE)                                             
         MVC   WORK(3),SWPTTYPE    TYPE                                         
         LA    R0,WORK                                                          
         LA    R1,1                                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMCSMEDIA)                                            
         LA    R0,WORK+1           MEDIA                                        
         LA    R1,1                                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMCSSOURCE)                                           
         LA    R0,WORK+2           SOURCE                                       
         LA    R1,1                                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMCSMONTH)                                            
         LA    R0,SWPTMO           MONTH                                        
         LA    R1,1                                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         GOTOR VDATCON,DMCB,(10,SWPTSDTE),(11,WORK)     START DATE              
         MVC   FALDMPC,=Y(FMCSSTART)                                            
         LA    R0,WORK             START DATE                                   
         LA    R1,8                                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         GOTOR VDATCON,DMCB,(10,SWPTSDTE),(0,WORK)                              
                                                                                
         CLI   SWPTNUM,X'01'       1 WEEK?                                      
         JNE   CSSWP22                                                          
         GOTOR VADDAY,DMCB,WORK,WORK+8,F'6'                                     
CSSWP22  CLI   SWPTNUM,X'02'       2 WEEK?                                      
         JNE   CSSWP24                                                          
         GOTOR VADDAY,DMCB,WORK,WORK+8,F'13'                                    
CSSWP24  CLI   SWPTNUM,X'03'       3 WEEK?                                      
         JNE   CSSWP26                                                          
         GOTOR VADDAY,DMCB,WORK,WORK+8,F'20'                                    
CSSWP26  CLI   SWPTNUM,X'04'       4 WEEK?                                      
         JNE   CSSWP28                                                          
         GOTOR VADDAY,DMCB,WORK,WORK+8,F'27'                                    
                                                                                
CSSWP28  MVC   DUB(6),WORK+8       DEFAULT TO END DATE YEAR                     
         CLC   =C'12',SWPTSDTE+5   START DATE IN DECEMBER?                      
         JNE   CSSWP30                                                          
         GOTOR VDATCON,DMCB,(10,SWPTSDTE),(0,WORK)                              
         GOTOR VADDAY,DMCB,WORK,DUB,F'15'                                       
                                                                                
CSSWP30  GOTOR VDATCON,DMCB,(0,DUB),(23,DUB2)                                   
         MVC   FALDMPC,=Y(FMCSYEAR)                                             
         LA    R0,DUB2             YEAR                                         
         LA    R1,4                                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         GOTOR VDATCON,DMCB,(0,WORK+8),(11,DUB)                                 
         MVC   FALDMPC,=Y(FMCSEND)                                              
         LA    R0,DUB              END DATE                                     
         LA    R1,8                                                             
         BAS   RE,D32DLGD                                                       
         B     CSSWP10                                                          
*                                                                               
CSSWPLPX DS    0H                                                               
*                                                                               
* comScore MARKET NUMBERS                                                       
*                                                                               
CSMKTLOP MVC   FALEMPC,=Y(FMCSMKT)    FORMAT TYPES COMSCORE SWEEP DATES         
         SR    R1,R1                                                            
         GOTO1 ADM32SET                                                         
*                                                                               
         LA    R6,WORK                                                          
         USING CTDMREC,R6                                                       
         XC    WORK,WORK                                                        
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIOAREA                 
         B     CSMKT10                                                          
*                                                                               
CSMKTSEQ GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'CTFILE',WORK,AIOAREA                 
*                                                                               
CSMKT10  L     R6,AIOAREA                                                       
         CLI   CTDMKTYP,CTDMKTEQ                                                
         JNE   CSMKTLPX                                                         
         CLI   CTDMKTY2,CTDMKT2E                                                
         JNE   CSMKTLPX                                                         
*                                                                               
         CLI   CTDMKSRC,C'C'       COMSCORE?                                    
         JNE   CSMKTSEQ                                                         
         CLI   CTDMKBKT,X'FF'      DEFAULT?                                     
         JNE   CSMKTSEQ                                                         
*                                                                               
         MVC   FALDMPC,=Y(FMCSMMEDIA)                                           
         LA    R0,CTDMKMED         MEDIA                                        
         LA    R1,1                                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMCSMSOURCE)                                          
         LA    R0,CTDMKSRC         SOURCE                                       
         LA    R1,1                                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMCSMALPHA)                                           
         LA    R0,CTDMKMKT         ALPHA MARKET                                 
         LA    R1,3                                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMCSMMKTNUM)                                          
         LA    R0,CTDMKNUM         MARKET NUMBER                                
         LA    R1,2                                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         B     CSMKTSEQ                                                         
         DROP  R6                                                               
*                                                                               
CSMKTLPX DS    0H                                                               
*                                                                               
* Source Access Info (Token record)                                             
*                                                                               
         BRAS  RE,TOKEN                                                         
*                                                                               
DEMLINX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
******************* PROPOSAL STUFF ONLY ******************                      
PDEMLINE DS    0H                                                               
         LA    R6,0                 COLUMN DISPLACENMENT                        
*                                                                               
PDMLP    MVC   FALEMPC,=Y(FMHPROPS)                                             
         SR    R1,R1                                                            
         GOTO1 ADM32SET                                                         
*                                                                               
         MVC   FALDMPC,=Y(FMDDSSRC)  1 BYTE SOURCE CODE                         
         SR    R3,R3                                                            
         LA    RF,PSRCCODE+1          FIRST SRC  ENTRY                          
         ZIC   R3,PSRCCODE                                                      
         MR    R2,R6                                                            
         AR    RF,R3                 ADD COLUMN NUMBER                          
         ZIC   RE,PSRCCODE                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    R0,WORK                                                          
         LR    R1,RE                                                            
         BAS   RE,D32DLGD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDDSRC)   SOURCE                                     
         SR    R3,R3                                                            
         LA    RF,PSOURCES+3          FIRST SRC  ENTRY                          
         ZIC   R3,PSOURCES+2                                                    
         MR    R2,R6                                                            
         AR    RF,R3                 ADD COLUMN NUMBER                          
         ZIC   RE,PSOURCES+2                                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    R0,WORK                                                          
         LR    R1,RE                                                            
         BAS   RE,D32DLGD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDDSRCD) SOURCE DESCRIPTION                          
         LA    RF,PSRCDIS            FIRST SRC DESCRIPTION ENTRY                
         SR    R3,R3                                                            
         LA    R3,PSRCDISL           LENGTH OF DESCRIPTION ENTRY                
         MR    R2,R6                                                            
         AR    RF,R3                 ADD COLUMN NUMBER                          
         LA    RE,PSRCDISL                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    R0,WORK                                                          
         LR    R1,RE                                                            
         BAS   RE,D32DLGD                                                       
*                                                                               
*======= GET BOOK TYPE CODE AND DESCRIPTIONS                                    
*                                                                               
PGETBKTP LA    R4,APBOOKTB                                                      
         SR    R3,R3                                                            
         LA    R3,APBOOKTL          LENGTH OF ADDR TABLE ENTRY                  
         MR    R2,R6                MULTIPLY WITH COLUMN NUMBER                 
         AR    R4,R3                ADD IT TO START OF ADDR TABLE               
         L     R3,0(R4)             GO TO BOOK TYPE TABLE                       
         OR    R3,R3                IF NO TABLES POINTED TO                     
         BZ    PBKTLPX              THEN NOTHING FOR THIS FIL\SRC               
         A     R3,RELO0B                                                        
*                                                                               
PBKTLP   XC    WORK,WORK                                                        
         MVC   WORK(BKTPCDEL),0(R3)                                             
         LA    R0,WORK                                                          
         LA    R1,BKTPCDEL                                                      
         MVC   FALDMPC,=Y(FMDDBKTY)                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(BKTPDISL),BKTPDISP(R3)  DESCRIPTION                         
         LA    R0,WORK                                                          
         LA    R1,BKTPDISL                                                      
         LA    R3,BKTPTABL(R3)                                                  
         MVC   FALDMPC,=Y(FMDDBKTD)                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         CLC   =X'0000',0(R3)                                                   
         BNE   PBKTLP                                                           
PBKTLPX  DS    0H                                                               
*                                                                               
*======= GET DEMO CODES  FOR PROPOSER                                           
*                                                                               
         LA    R4,ADEMOSTB                                                      
         SR    R3,R3                                                            
         LA    R3,ADEMOSTL          LENGTH OF ADDR TABLE ENTRY                  
         MR    R2,R6                MULTIPLY WITH COLUMN NUMBER                 
         AR    R4,R3                ADD IT TO START OF ADDR TABLE               
         L     R3,0(R4)             GO TO DEMO MOD TABLE                        
         OR    R3,R3                IF NO TABLES POINTED TO                     
         BZ    PDMMLPX              THEN NOTHING FOR THIS FIL\SRC               
         A     R3,RELO0B                                                        
*                                                                               
PDMMLP   CLC   =X'0004',0(R3)       IF CATEGORY STUFF, JUST SKIP FOR            
         BNE   *+8                  NOW                                         
         LA    R3,DMCATLEN(R3)                                                  
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(DEMOSLQ),0(R3)  DESCRIPTION                                 
         LA    R0,WORK                                                          
         LA    R1,DEMOSLQ                                                       
         LA    R3,DEMOSTL(R3)                                                   
         MVC   FALDMPC,=Y(FMDDEMON)                                             
         BAS   RE,D32DLGD                                                       
         CLC   =X'0000',0(R3)                                                   
         BNE   PDMMLP                                                           
PDMMLPX  DS    0H                                                               
*                                                                               
* ------- LOOP BACK TO NEXT COLUMN INFO                                         
         LA    R6,1(R6)                                                         
         CHI   R6,PSRCNUM          COMPARE AGAINST MAX NUMBER OF SRC            
         BE    PDEMLINX                                                         
         B     PDMLP                                                            
*                                                                               
PDEMLINX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
**                                                                              
*                                                                               
D32DLGD  NTR1                                                                   
         ST    R0,ADLDATA                                                       
         ST    R1,LDLDATA                                                       
         MVI   ADMODE,ADMADD                                                    
         GOTO1 ADM32ADD                                                         
         XIT1                                                                   
         EJECT                                                                  
**********************************************************                      
ADEMOSTL  EQU   4     LENGTH OF EACH TABLE ENTRY                                
*                                                                               
ADEMOSTB DC    AL4(DEMOSTB1)                                                    
         DC    AL4(DEMOSTB1)                                                    
         DC    AL4(DEMOSTB1)                                                    
         DC    AL2(0)                                                           
**********************************************************                      
         EJECT                                                                  
***********************************************************************         
*============================ GET PROFILE ============================*         
                                                                                
GETPROFL NTR1                                                                   
         XC    MYPROFLS(MYPROFLL),MYPROFLS                                      
                                                                                
*                                                                               
         DS    0H                                                               
         L     RF,ASYSNTRY                                                      
         CLI   (SYSOVSYS-SYSTABD)(RF),2      SPOT                               
         BE    GPRFSP1W00                                                       
         CLI   (SYSOVSYS-SYSTABD)(RF),8      REP                                
         BE    GPRFRRMP00                                                       
         DC    H'0'                                                             
                                                                                
*                                                                               
** SPOT 1W PROFILE **                                                           
*                                                                               
GPRFSP1W00 DS  0H                                                               
         XC    WORK,WORK           SET UP 1W PROFILE KEY                        
         MVI   SIGNON,C'S'                                                      
         MVC   WORK+0(4),=C'S01W'                                               
         MVC   WORK+4(2),AGYALPH                                                
         MVI   WORK+6,C'T'                                                      
         MVC   WORK+7(3),OPTCLI                                                 
         GOTO1 VGETPROF,DMCB,WORK,PROF1W,VDATAMGR                               
                                                                                
*                                                                               
*** SET THE DIFFERENT PROFILES ***                                              
*                                                                               
         DS    0H                  DMA/METHODOLOGY/PRECISION                    
         MVC   PRECPROF,PROF1W+5   GET PRECISION FROM (SPOT) 1W PROFILE         
                                                                                
*                                                                               
GPRFSP1WX EQU  *                                                                
         B     GETPRFLX                                                         
                                                                                
*                                                                               
** REP RMP PROFILE **                                                           
*                                                                               
GPRFRRMP00 DS  0H                                                               
         MVI   SIGNON,C'R'                                                      
         XC    WORK,WORK            BUILD KEY OF REP RECD IN WORK               
         LA    R6,WORK                                                          
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,X01                                                     
         MVC   RREPKREP,AGYALPH                                                 
         DROP  R6                                                               
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'REPDIR',WORK,WORK                    
         CLI   DMCB+08,X'10'                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 (RF),(R1),=C'GETREC',=C'REPFILE',WORK+28,AIOAREA,       +        
               MYDMWORK                                                         
         CLI   DMCB+08,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                  LOOK FOR PROFILE IN RECORD                   
         L     R3,AIOAREA           R3-->REP RECORD                             
         MVI   MYELCODE,X'04'                                                   
         LA    R0,RREPELEM-RREPREC                                              
         STH   R0,MYDATDSP                                                      
         BRAS  RE,GETEL                                                         
         BNE   GPRFRRMPX                                                        
*                                                                               
         ZIC   R0,(RREPPGM#-RREPPGMP)(R3)  R0 = # OF PROGRAM PROFILES           
         LA    R6,(RREPPGM1-RREPPGMP)(R3)  R6-->PROGRAM PROFILES LIST           
                                                                                
GPRFRRMP40 DS  0H                                                               
         CLI   0(R6),RREPQRMP       LOOK FOR RMP PROGRAM PROFILE                
         BE    *+16                                                             
         LA    R6,RREPPGML(R6)                                                  
         BCT   R0,GPRFRRMP40                                                    
         B     GPRFRRMPX                                                        
*                                                                               
         MVC   PROFRRMP,2(R6)       MOVE PROFILE INTO STORAGE AREA              
                                                                                
*                                                                               
*** SET THE DIFFERENT PROFILES ***                                              
*                                                                               
         DS    0H                  DMA/METHODOLOGY/PRECISION                    
         MVI   PRECPROF,C'R'        SET PRECISION                               
         TM    PROFRRMP+RMPIMPSB,RMPIMPSA                                       
         BZ    *+8                                                              
         MVI   PRECPROF,C'I'         ACCORDING TO THE REP RMP PROFILE           
                                                                                
*                                                                               
GPRFRRMPX EQU  *                                                                
         B     GETPRFLX                                                         
                                                                                
*                                                                               
GETPRFLX DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ GETEL MACRO ============================*         
                                                                                
         GETEL R3,MYDATDSP,MYELCODE                                             
                                                                                
***********************************************************************         
         EJECT                                                                  
*                                                                               
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
         EJECT                                                                  
       ++INCLUDE DEMATRIX1                                                      
       ++INCLUDE DESTACODE                                                      
       ++INCLUDE DEDEMLIST                                                      
*                                                                               
TOKEN    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
*                                                                               
         LA    R6,WORK                                                          
         USING TOKKEY,R6                                                        
         XC    WORK,WORK                                                        
         MVI   TOKKMIN,TOKKMINQ      C'K'                                       
         MVI   TOKKTYP,TOKKRTRK      X'01' - RENTRAK RECORD                     
         MVC   TOKKAAGY,AGYALPH      AGENCY ALPHA CODE                          
         MVC   TOKKSAGY,FATAGYSC     SECURITY AGENCY CODE                       
         MVI   TOKKSYS,X'02'         SPOT SYSTEM                                
         DROP  R1                                                               
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',WORK,WORK2                   
         CLC   WORK(L'TOKKEY),WORK2                                             
         BNE   TOKENX                                                           
*                                                                               
         GOTO1 (RF),(R1),=C'GETREC',=C'GENFIL ',WORK2+36,AIOAREA,      +        
               MYDMWORK                                                         
         CLI   DMCB+08,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIOAREA                                                       
         AHI   R3,TOKFIRST             R4=A(1ST ELEMENT)                        
TOKEN10  CLI   0(R3),0                 ANY ELEMENTS?                            
         JE    TOKEN30                                                          
         CLI   0(R3),RTAUTELQ          X'0A' - RENTRAK AUTHOR ELEM?             
         JE    TOKEN20                                                          
         LLC   R0,1(R3)                CHECK THE NEXT ELEMENT                   
         AR    R3,R0                                                            
         J     TOKEN10                                                          
*                                                                               
         USING RTAUTHD,R3                                                       
TOKEN20  CLC   RTAUTID,SPACES          LICENSE ID BETTER BE > SPACES            
         JNH   TOKEN30                                                          
         LA    RF,WORK                                                          
         USING COMACCSD,RF                                                      
         MVC   RNTKRTGS,=C'COM'        comScore                                 
         MVI   RNTKPIP1,C'|'                                                    
         MVC   RNTKLICI,RTAUTID        LICENSE ID                               
         MVI   RNTKPIP2,C'|'                                                    
         MVC   RNTKSECI,RTAUTSEC       SECURITY ID                              
         DROP  R3,RF                                                            
*                                                                               
         MVC   FALEMPC,=Y(FMTOK)       TOKEN                                    
         SR    R1,R1                                                            
         GOTO1 ADM32SET                                                         
*                                                                               
         MVC   FALDMPC,=Y(FMCSMMEDIA)                                           
         LA    R0,WORK             MEDIA                                        
         LA    R1,RNTKLENQ                                                      
         BAS   RE,D32DLGD2                                                      
*                                                                               
TOKEN30  GOTO1 VGETFACT,DMCB,(X'80',0),F#SSBD                                   
         L     R1,0(R1)                                                         
         MVC   BYTE,F@SSYSFL-F@SSBD(R1)    SYSTEM                               
*                                                                               
         LA    R6,WORK                                                          
         USING SDRRECD,R6                                                       
         XC    WORK,WORK                                                        
         MVI   SDRKMIN,SDRKMINQ      X'5D'                                      
         MVC   SDRKFFL,BYTE                                                     
         MVI   SDRKSYS,X'02'       SPOT                                         
         MVC   SDRKAPP,=X'0005'    DEM32 (FROM FAXPEQUS)                        
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   WORK2(L'WORK),WORK                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',WORK,WORK                    
         B     TOKEN45                                                          
*                                                                               
TOKEN40  GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',WORK,WORK                    
TOKEN45  CLC   WORK(L'SDRKEY),WORK2                                             
         BNE   TOKENX                                                           
*                                                                               
         GOTO1 (RF),(R1),=C'GETREC',=C'GENFIL ',WORK+36,AIOAREA,       +        
               MYDMWORK                                                         
         CLI   DMCB+08,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         USING SDRRECD,R3                                                       
         L     R3,AIOAREA                                                       
         LA    R3,SDRRFRST                                                      
TOKEN50  CLI   0(R3),0                                                          
         BE    TOKENX                                                           
         USING SDELD,R3                                                         
*                                                                               
         CLI   SDEEL,X'02'                                                      
         JNE   TOKEN55                                                          
         MVC   FALDMPC,=Y(FMTOKSDATA)                                           
         LA    R0,SDEDATA              RATING SOURCE CONNECTION DATA            
         ZIC   R1,SDELEN                                                        
         SHI   R1,SDELEN1Q                                                      
         BAS   RE,D32DLGD2                                                      
         J     TOKEN70                                                          
*                                                                               
TOKEN55  CLI   SDEEL,X'03'                                                      
         JNE   TOKEN60                                                          
         MVC   FALDMPC,=Y(FMTOKMENDP)                                           
         LA    R0,SDEDATA          RATING SOURCE METHOD ENDPOINTS               
         ZIC   R1,SDELEN                                                        
         SHI   R1,SDELEN1Q                                                      
         BAS   RE,D32DLGD2                                                      
         J     TOKEN70                                                          
*                                                                               
TOKEN60  CLI   SDEEL,X'04'                                                      
         JNE   TOKEN65                                                          
         MVC   FALDMPC,=Y(FMTOKMASHK)                                           
         LA    R0,SDEDATA          MASHERY KEY FOR RENTRAK API                  
         ZIC   R1,SDELEN                                                        
         SHI   R1,SDELEN1Q                                                      
         BAS   RE,D32DLGD2                                                      
*                                                                               
TOKEN65  CLI   SDEEL,X'05'                                                      
         JNE   TOKEN70                                                          
         MVC   FALDMPC,=Y(FMTOKNODAYS)                                          
         LA    R0,SDEDATA          NUMBER OF DAYS                               
         ZIC   R1,SDELEN                                                        
         SHI   R1,SDELEN1Q                                                      
         BAS   RE,D32DLGD2                                                      
*                                                                               
TOKEN70  ZIC   RF,SDELEN                                                        
         AR    R3,RF                                                            
         B     TOKEN50                                                          
*                                                                               
TOKENX   J     EXIT                                                             
*                                                                               
D32DLGD2 NTR1                                                                   
         ST    R0,ADLDATA                                                       
         ST    R1,LDLDATA                                                       
         MVI   ADMODE,ADMADD                                                    
         GOTO1 ADM32ADD                                                         
         J     EXIT                                                             
*                                                                               
COMACCSD DSECT                                                                  
RNTKRTGS DS    CL3                     RATING SOURCE                            
RNTKPIP1 DS    C                       C'|'                                     
RNTKLICI DS    CL(L'RTAUTID)           LICENSE ID                               
RNTKPIP2 DS    C                       C'|'                                     
RNTKSECI DS    CL(L'RTAUTSEC)          SECURITY ID                              
RNTKLENQ EQU   *-RNTKRTGS                                                       
*                                                                               
* DEDEMWRK                                                                      
       ++INCLUDE DEDEMWRK                                                       
         ORG   APSAVE                                                           
         SPACE 1                                                                
* SAVE STORAGE FOR OVERLAY                                                      
*                                                                               
         ORG                                                                    
         EJECT                                                                  
* DSECT TO COVER TEMPORARY WORKING STORAGE                                      
*                                                                               
DEMTMPD  DSECT                                                                  
MYDMWORK DS    12D                                                              
SAVERE   DS    A                   SAVED RE VALUE IN DEMHOOK                    
MYDATDSP DS    H                                                                
SRCDISL  DS    X                                                                
RELO0B   DS    F                                                                
NTRYLEN  DS    XL1                                                              
*                                                                               
SVFILECD DS    CL5                                                              
SVSOURCE DS    CL3                                                              
MYELCODE DS    XL1                                                              
SIGNON   DS    C                   C'R'/S'   REP OR SPOT                        
*                                                                               
MYPROFLS DS    0X                  STORAGE FOR PROFILES USED BY PHASE           
PRECPROF DS     CL1                 PRECISION SET IN PROFILE                    
MYPROFLL EQU   *-MYPROFLS                                                       
         SPACE 1                                                                
*                                                                               
* DSECT TO COVER TSAR DEMO RECORD                                               
*                                                                               
TSDEMRCD DSECT                                                                  
TDRKEY   DS    0X                  KEY OF TSAR DEMO RECORD                      
TDRTYPE  DS    XL1                  RECORD TYPE                                 
TDRTMSGQ EQU   X'10'                 (DUMMY) MESSAGE RECORD                     
TDRTBKQ  EQU   X'20'                 BOOK RECORD                                
TDRBOOK  DS    XL2                  BOOK VALUE                                  
TDRKEYL  EQU   *-TSDEMRCD          KEY LENGTH                                   
TDRDUMMY DS    XL1                  DUMMY DATA FIELD                            
TDRRECL  EQU   *-TSDEMRCD          REC LENGTH                                   
         EJECT                                                                  
*                                                                               
* DSECT TO COVER SWEEP TABLE ENTRY                                              
*                                                                               
SWPTABD  DSECT                                                                  
SWPTTYPE DS    CL3                 TTN                                          
SWPTYR   DS    XL1                 BINARY YEAR                                  
SWPTMO   DS    XL1                 BINARY MONTH                                 
SWPTNUM  DS    XL1                 # WEEK SWEEP                                 
SWPTSDTE DS    0CL10               START DATE                                   
SWPTYEAR DS    CL4                 YEAR                                         
         DS    CL1                                                              
SWPTMON  DS    CL2                 MONTH                                        
         DS    CL1                                                              
SWPTDAY  DS    CL2                 DAY                                          
         DS    CL1                                                              
***********************************************************************         
*============================= REP DSECTS ============================*         
                                                                                
*------------------------------ REGENREP -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE REGENREPA                                                      
         PRINT ON                                                               
                                                                                
                                                                                
*----------------------------- RERMPPROF -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE RERMPPROF                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE GEGENTOK                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE GEGENSDR                                                       
         PRINT ON                                                               
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'154DEDEM0B   11/17/20'                                      
         END                                                                    
