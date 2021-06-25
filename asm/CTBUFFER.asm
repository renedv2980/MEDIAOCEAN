*          DATA SET CTBUFFER   AT LEVEL 004 AS OF 03/21/14                      
*PHASE CTBUFFA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE BINSR31                                                                
*INCLUDE ARREDIT                                                                
*INCLUDE DDWTO                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'CTBUFFER - BUFFER CTFILE RECORDS IN ADDRESS SPACE'              
         PRINT NOGEN                                                            
CTBUFF   CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE WORKL,CTBUFFER,=A(WORKAREA),RA,R9                                
         USING WORKD,RC                                                         
         USING LIBUFFD,LIBUFFR                                                  
         USING DMSPACED,DSPHD                                                   
*                                                                               
         ST    RD,SAVERD                                                        
         BRAS  RE,INIT             INITIALISE                                   
         BRAS  RE,SETOPS           SET UP COMMS                                 
*                                                                               
MAIN02   XC    DUB,DUB             LOCK TABLE IN TABS DATASPACE                 
         MVC   DUB(4),=AL4(DTCTB)                                               
         MVI   DUB,X'80'           SET LONG ALLOCATE                            
         GOTO1 VLOCKSPC,DUB                                                     
*                                                                               
         L     RF,4(R1)            SAVE DSPACE HEADER                           
         MVC   DSPHD,0(RF)                                                      
         NC    DSPTFRST,=XL4'3FFFFFFF'                                          
*                                                                               
         LAM   AR4,AR4,ALET        SET HEADER INFO IN DATASPACE                 
         ICM   R4,15,DSPTFRST                                                   
         SAC   512                                                              
         USING CTBUFFD,R4                                                       
         XC    CTBUFFID,CTBUFFID   FLAG NOTHING BUILT                           
         XC    CTBUFFCP,CTBUFFCP   CLEAR CPU TIME                               
         BRAS  RE,ARSOFF                                                        
         DROP  R4                                                               
*                                                                               
         BRAS  RE,FREEMAIN         FREE STORAGE IF ANY ALLOCATED                
         BRAS  RE,SIZES            CALCULATE SIZES                              
         BRAS  RE,GETMAIN          GET XA STORAGE REQUIRED                      
         BRAS  RE,BLDTABS          BUILD TABLES IN XA AS REQUIRED               
*                                                                               
         XC    DUB,DUB             FREE TABLE IN TABS DATASPACE                 
         MVC   DUB(4),=AL4(DTCTB)                                               
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB                                                     
*                                                                               
MAIN04   BRAS  RE,WAIT             SLEEPY TIME                                  
*                                                                               
         TM    JOBSTEP,JOBSTOP     TEST FORCE STOP POSTED                       
         BO    MAIN06                                                           
         TM    JOBSTEP,JOBEOJ      TEST FORCE EOJ POSTED                        
         BO    MAIN06                                                           
         TM    JOBSTEP,JOBINIT     TEST FORCE REBUILD POSTED                    
         BNO   MAIN04                                                           
*                                                                               
         BRAS  RE,RPTONIO          OUTPUT COUNTS OF I/O                         
         B     MAIN02                                                           
*                                                                               
MAIN06   BRAS  RE,ARSOFF                                                        
         LAM   AR4,AR4,ALET        SET HEADER INFO IN DATASPACE                 
         ICM   R4,15,DSPTFRST                                                   
         SAC   512                                                              
         USING CTBUFFD,R4                                                       
         XC    CTBUFFID,CTBUFFID                                                
         XC    CTBUFFCP,CTBUFFCP   CLEAR CPU TIME                               
         XC    CTBUFFSV,CTBUFFSV                                                
         BRAS  RE,ARSOFF                                                        
         B     XBASE                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1                                                                   
         LHI   R0,-4               MAKE NON-SWAPPABLE                           
         SVC   247                                                              
*                                                                               
         EXTRACT RESULTS,'S',FIELDS=(TIOT,COMM,ASID)                            
         ALESERV EXTRACTH,STOKEN=ASTOKEN                                        
         BRAS  RE,PRINTI           INIT PRINTING                                
*                                                                               
         MVC   PLINE,SPACES                                                     
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LHI   R1,1                BEGINNING INPUT CARD VALIDATION              
         BRAS  RE,DOMSG                                                         
*                                                                               
         LA    R2,CARD                                                          
INIT02   GOTO1 VCARDS,DMCB,(R2),=C'RE00'                                        
         CLC   0(2,R2),=C'/*'                                                   
         BE    INIT04                                                           
*                                                                               
         MVC   PLINE+15(L'CARD),CARD                                            
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R2               VALIDATE KEYWORD=VALUE                       
         BRAS  RE,CARDVAL                                                       
         BE    INIT02                                                           
*                                                                               
         LHI   R1,3                CARD VALIDATION ERROR                        
         BRAS  RE,DOMSG                                                         
         B     XBASE                                                            
*                                                                               
INIT04   LHI   R1,2                COMPLETED INPUT CARD VALIDATION              
         BRAS  RE,DOMSG                                                         
         BRAS  RE,GETSPC           GET ADDRESS OF TABS DSPACE                   
*                                                                               
         LHI   R1,8                BEGAN OPENING FILES                          
         BRAS  RE,DOMSG                                                         
         GOTO1 VDMGR,DMCB,DMOPEN,DMSYS,DMFLIST                                  
*                                                                               
         GOTO1 VDMGR,DMCB,DTFAD,CTFILE,0,0                                      
         L     RF,DMCB+12                                                       
         ST    RF,ACTDTF           SAVE A(CTFILE DTF)                           
*                                                                               
         LHI   R1,9                ENDED OPENING FILES                          
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREEMAIN ANY ALREADY HELD STORAGE                                   *         
***********************************************************************         
FREEMAIN NTR1                                                                   
         ICM   R0,15,GETLEN                                                     
         BNZ   FREE02                                                           
         LHI   R1,10               FREEMAIN NOT REQUIRED                        
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
*                                                                               
FREE02   LHI   R1,31               ABOUT TO FREEMAIN                            
         BRAS  RE,DOMSG                                                         
*                                                                               
         ICM   R1,15,GETADR                                                     
         FREEMAIN RC,A=(1),LV=(0)  FREE STORAGE                                 
         LTR   RF,RF                                                            
         BNZ   FREE04                                                           
         LHI   R1,11               FREEMAIN PERFORMED                           
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
*                                                                               
FREE04   LHI   R1,12               FREEMAIN ERROR                               
         BRAS  RE,DOMSG                                                         
         ABEND 912,DUMP                                                         
         EJECT                                                                  
***********************************************************************         
* READ RECORDS AND CALCULATE SIZE REQUIREMENTS FOR EACH               *         
***********************************************************************         
SIZES    NTR1                                                                   
         LHI   R1,13               BEGAN CALCULATING SIZE REQUIREMENTS          
         BRAS  RE,DOMSG                                                         
*                                                                               
         XC    TOTCNT,TOTCNT                                                    
         XC    TOTTOT,TOTTOT                                                    
         XC    TOTGET,TOTGET                                                    
         XC    TOTMOD,TOTMOD                                                    
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+15(05),=CL05'Type '                                        
         MVC   PLINE+21(12),=CL12'Record Count'                                 
         MVC   PLINE+34(12),=CL12'  Byte Count'                                 
         MVC   PLINE+47(12),=CL12'Modify Count'                                 
         MVC   PLINE+60(12),=CL12'  Table size'                                 
         BRAS  RE,PRINTL                                                        
*                                                                               
         LA    R2,RECTAB                                                        
         USING RECTABD,R2                                                       
SIZE02   CLI   RECID,0             END OF RECTAB                                
         BE    SIZE12                                                           
         XC    RECCNT,RECCNT       FOR REBUILD                                  
         XC    RECTOT,RECTOT                                                    
         XC    RECMOD,RECMOD                                                    
         XC    RECGET,RECGET                                                    
*                                                                               
         XC    KEY,KEY             SET KEY FOR THIS RECORD TYPE                 
         MVC   KEY(1),RECID                                                     
*                                                                               
SIZE04   XC    P1(32),P1                                                        
         MVC   P1,ISREAD                                                        
         LA    R3,IO               R3 = IO AREA ADDRESS                         
         ST    R3,P2                                                            
         MVC   P4,ACTDTF           CONTROL FILE DTF                             
         LA    RF,KEY                                                           
         ST    RF,P5               SET A(KEYARG)                                
         GOTO1 VISDDS,P1                                                        
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   RECID,0(R3)         ENSURE AT LEAST 1 OF TYPE FOUND              
         BE    SIZE08                                                           
         DC    H'0'                                                             
*                                                                               
SIZE06   MVC   P1,ISRDSEQ          NEXT IN FILE                                 
         GOTO1 VISDDS,P1                                                        
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   RECID,0(R3)         CHECK IF END OF TYPE                         
         BNE   SIZE10                                                           
*                                                                               
SIZE08   XR    R1,R1               GIVE APPROXIMATION OF BUFFER USED            
         ICM   R1,3,CTLEN-CTDSECT(R3)                                           
*                                                                               
         LR    RE,R1                                                            
         A     RE,RECTOT                                                        
         ST    RE,RECTOT           SET REAL TOTAL OF BYTES                      
*                                                                               
         LR    RE,R1                                                            
         SH    RE,RECKDSP          ADJUST FOR LENGTH OF KEY                     
         AHI   RE,8                                                             
         A     RE,RECMOD                                                        
         ST    RE,RECMOD           SET MODIFIED TOTAL                           
*                                                                               
         LR    RE,R1                                                            
         SH    RE,RECKDSP                                                       
         AHI   RE,8                GET EXTRA FOR EXPANSION                      
         CLI   RECID,C'I'          ID RECORDS NEED A BIT MORE                   
         BNE   *+8                                                              
         AHI   RE,40                                                            
         LR    RF,RE                                                            
         SRL   RF,2                RF = RE/4 (25% EXTRA)                        
         AR    RE,RF                                                            
         A     RE,RECGET                                                        
         ST    RE,RECGET           SET MODIFIED TOTAL                           
*                                                                               
         L     RF,RECCNT                                                        
         AHI   RF,1                                                             
         ST    RF,RECCNT                                                        
         B     SIZE06                                                           
*                                                                               
SIZE10   MVC   PLINE+15(04),=CL04'. XX'                                         
         GOTO1 VHEXOUT,DMCB,RECID,PLINE+17,1,(24,0)                             
         CLI   RECID,C'A'                                                       
         BL    *+10                                                             
         MVC   PLINE+15(1),RECID                                                
*                                                                               
         L     RF,RECGET                                                        
         AHI   RF,4095                                                          
         SRL   RF,12                                                            
         SLL   RF,12                                                            
         ST    RF,RECGET           ROUND TO NEAREST 4K                          
*                                                                               
         EDIT  (B4,RECCNT),(12,PLINE+21),0,COMMAS=YES,ZERO=NOBLANK              
         EDIT  (B4,RECTOT),(12,PLINE+34),0,COMMAS=YES,ZERO=NOBLANK              
         EDIT  (B4,RECMOD),(12,PLINE+47),0,COMMAS=YES,ZERO=NOBLANK              
         EDIT  (B4,RECGET),(12,PLINE+60),0,COMMAS=YES,ZERO=NOBLANK              
         BRAS  RE,PRINTL                                                        
*                                                                               
         L     RF,RECCNT           ADD THIS LINE TO THE TOTALS LINE             
         A     RF,TOTCNT                                                        
         ST    RF,TOTCNT                                                        
         L     RF,RECTOT                                                        
         A     RF,TOTTOT                                                        
         ST    RF,TOTTOT                                                        
         L     RF,RECMOD                                                        
         A     RF,TOTMOD                                                        
         ST    RF,TOTMOD                                                        
         L     RF,RECGET                                                        
         A     RF,TOTGET                                                        
         ST    RF,TOTGET                                                        
         AHI   R2,RECTABL          NEXT IN RECTAB                               
         B     SIZE02                                                           
         DROP  R2                                                               
*                                                                               
SIZE12   MVC   PLINE+15(05),=CL05'TOTAL'                                        
         EDIT  (B4,TOTCNT),(12,PLINE+21),0,COMMAS=YES,ZERO=NOBLANK              
         EDIT  (B4,TOTTOT),(12,PLINE+34),0,COMMAS=YES,ZERO=NOBLANK              
         EDIT  (B4,TOTMOD),(12,PLINE+47),0,COMMAS=YES,ZERO=NOBLANK              
         EDIT  (B4,TOTGET),(12,PLINE+60),0,COMMAS=YES,ZERO=NOBLANK              
         BRAS  RE,PRINTL                                                        
*                                                                               
         BRAS  RE,PRINTL                                                        
         LHI   R1,14               ENDED CALCULATING SIZE REQUIREMENTS          
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* REPORT ON I/O SAVINGS FROM DATASPACE                                *         
***********************************************************************         
RPTONIO  NTR1                                                                   
         LHI   R1,29               BEGAN REPORTING ON I/O SAVINGS               
         BRAS  RE,DOMSG                                                         
         XC    HITSON,HITSON                                                    
         XC    HITSOFF,HITSOFF                                                  
         XC    HITSTOT,HITSTOT                                                  
*                                                                               
         SAM31                                                                  
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,ALET        SET HEADER INFO IN DATASPACE                 
         ICM   R2,15,DSPTFRST                                                   
         ICM   R3,15,DSPTEND                                                    
         AHI   R3,1                                                             
         SR    R3,R2                                                            
         LA    RE,IO                                                            
         LHI   RF,MAXIOL                                                        
         SAC   512                                                              
         MVCL  RE,R2               COPY STORAGE LOCALLY                         
         BRAS  RE,ARSOFF                                                        
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+15(L'H1LINE),H1LINE                                        
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+15(L'H2LINE),H2LINE                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         LA    R4,IO                                                            
         USING CTBUFFD,R4                                                       
         LA    R4,CTBUFFP                                                       
         USING CTBUFFP,R4                                                       
ROI02    CLI   CTBUFFP,X'FF'       TEST END OF TABLE                            
         BNE   ROI03                                                            
         MVC   MISSON,CTBUFFCT     SAVE COUNT OF ONLINE MISSES                  
         MVC   MISSOFF,CTBUFFOL    SAVE COUNT OF OFFLINE MISSES                 
         B     ROI04                                                            
*                                                                               
ROI03    LR    R0,R4               GUARD AGAINST OVERFLOW                       
         LA    R1,IO                                                            
         SR    R0,R1                                                            
         CHI   R0,MAXIOL                                                        
         BH    ROI04                                                            
*                                                                               
         MVC   BYTE,CTBUFFP                                                     
         MVC   PLINE+15(04),=CL04'. XX'                                         
         GOTO1 VHEXOUT,DMCB,BYTE,PLINE+17,1,(24,0)                              
         CLI   BYTE,C'A'                                                        
         BL    *+10                                                             
         MVC   PLINE+15(1),BYTE                                                 
         LG    GR0,CTBUFFCT                                                     
         CVDG  GR0,BIGPACK                                                      
         EDIT  (P8,BIGPACK+8),(12,PLINE+21),0,COMMAS=YES,ZERO=NOBLANK           
         LG    GR0,CTBUFFOL                                                     
         CVDG  GR0,BIGPACK                                                      
         EDIT  (P8,BIGPACK+8),(12,PLINE+34),0,COMMAS=YES,ZERO=NOBLANK           
         LG    GR0,CTBUFFCT                                                     
         AG    GR0,CTBUFFOL                                                     
         CVDG  GR0,BIGPACK                                                      
         EDIT  (P8,BIGPACK+8),(12,PLINE+47),0,COMMAS=YES,ZERO=NOBLANK           
         BRAS  RE,PRINTL                                                        
*                                                                               
         LG    GR0,HITSON          BUMP ONLINE TOTAL                            
         AG    GR0,CTBUFFCT                                                     
         STG   GR0,HITSON                                                       
         LG    GR1,HITSOFF         BUMP OFFLINE TOTAL                           
         AG    GR1,CTBUFFOL                                                     
         STG   GR1,HITSOFF                                                      
         AGR   GR0,GR1             BUMP GRAND TOTAL                             
         STG   GR0,HITSTOT                                                      
         AHI   R4,CTBUFFPL                                                      
         B     ROI02                                                            
*                                                                               
ROI04    LA    R4,IO                                                            
         USING CTBUFFD,R4                                                       
         MVC   PLINE+15(L'H3LINE),H3LINE                                        
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+15(5),=CL5'Total'                                          
         LG    GR0,HITSON                                                       
         CVDG  GR0,BIGPACK                                                      
         EDIT  (P8,BIGPACK+8),(12,PLINE+21),0,COMMAS=YES,ZERO=NOBLANK           
         LG    GR0,HITSOFF                                                      
         CVDG  GR0,BIGPACK                                                      
         EDIT  (P8,BIGPACK+8),(12,PLINE+34),0,COMMAS=YES,ZERO=NOBLANK           
         LG    GR0,HITSTOT                                                      
         CVDG  GR0,BIGPACK                                                      
         EDIT  (P8,BIGPACK+8),(12,PLINE+47),0,COMMAS=YES,ZERO=NOBLANK           
*                                                                               
ROI06    LA    R4,IO               OUTPUT CPU TIME USED IN DMDMGR               
         USING CTBUFFD,R4                                                       
         MVC   PLINE+60(9),=CL9'CPU Mili='                                      
         LG    GR1,CTBUFFCP                                                     
         DSG   GR0,=FD'1000'       CONVERT MICROSECS TO MILLISECS               
         CVDG  GR1,BIGPACK                                                      
         EDIT  (P8,BIGPACK+8),(12,PLINE+69),0,ALIGN=LEFT                        
         BRAS  RE,PRINTL                                                        
*                                                                               
ROI08    MVC   PLINE+15(6),=CL6'Misses'                                         
         LG    GR0,MISSON                                                       
         CVDG  GR0,BIGPACK                                                      
         EDIT  (P8,BIGPACK+8),(12,PLINE+21),0,COMMAS=YES,ZERO=NOBLANK           
         LG    GR0,MISSOFF                                                      
         CVDG  GR0,BIGPACK                                                      
         EDIT  (P8,BIGPACK+8),(12,PLINE+34),0,COMMAS=YES,ZERO=NOBLANK           
         LG    GR0,MISSON                                                       
         AG    GR0,MISSOFF                                                      
         STG   GR0,MISSTOT                                                      
         CVDG  GR0,BIGPACK                                                      
         EDIT  (P8,BIGPACK+8),(12,PLINE+47),0,COMMAS=YES,ZERO=NOBLANK           
*                                                                               
ROI09    MVC   PLINE+60(9),=CL9'Hit+Miss='                                      
         LG    GR0,HITSTOT                                                      
         AG    GR0,MISSTOT                                                      
         CVDG  GR0,BIGPACK                                                      
         EDIT  (P8,BIGPACK+8),(12,PLINE+69),0,COMMAS=YES,ALIGN=LEFT             
         BRAS  RE,PRINTL                                                        
*                                                                               
ROI10    BRAS  RE,PRINTL                                                        
         LHI   R1,30               ENDED REPORTING ON I/O SAVINGS               
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         DROP  R4                                                               
*                                                                               
H1LINE   DC    CL50'Rec   --------------I/O Counts--------------'               
H2LINE   DC    CL50'Type        Online      Offline        Total'               
H3LINE   DC    CL50'--------------------------------------------'               
         EJECT                                                                  
***********************************************************************         
* DO GETMAIN FOR STORAGE REQUIREMENTS                                 *         
***********************************************************************         
GETMAIN  NTR1                                                                   
         LHI   R1,15               BEGAN GETMAIN CALCULATIONS                   
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     R0,TOTGET                                                        
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BNZ   GETM02                                                           
*                                                                               
         ST    R0,GETLEN           SAVE STORAGE OBTAINED                        
         ST    R1,GETADR                                                        
*                                                                               
         LHI   R1,16               ENDED GETMAIN CALCULATIONS                   
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
*                                                                               
GETM02   LHI   R1,17               FAILURE ON GETMAIN                           
         BRAS  RE,DOMSG                                                         
         ABEND 912,DUMP                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD TABLES IN XA STORAGE AS REQUIRED                              *         
***********************************************************************         
BLDTABS  NTR1                                                                   
         LHI   R1,18                                                            
         BRAS  RE,DOMSG                                                         
*                                                                               
         LA    R2,RECTAB                                                        
         USING RECTABD,R2                                                       
         L     R3,GETADR                                                        
*                                                                               
BLDT02   CLI   RECID,0             TEST OF TABLE                                
         BE    BLDT04                                                           
         ST    R3,FULL             SAVE A(TABLE FOR ROUTINE)                    
         ICM   RF,15,RECRTN                                                     
         BASR  RE,RF               BUILD TABLE                                  
         A     R3,RECGET           GO PAST START OF THIS TABLE                  
         AHI   R2,RECTABL                                                       
         B     BLDT02                                                           
*                                                                               
BLDT04   BRAS  RE,ARSOFF           NOW FILL IN DETAILS IN DSPACE                
         XR    R0,R0                                                            
         BRAS  RE,TIMER            GET CURRENT DATE AND TIME                    
*                                                                               
         SAM31                                                                  
         LAM   AR4,AR4,ALET        SET HEADER INFO IN DATASPACE                 
         ICM   R4,15,DSPTFRST                                                   
         SAC   512                                                              
         USING CTBUFFD,R4                                                       
         MVC   CTBUFFID,=CL08'*CTBUFF*'                                         
         XC    CTBUFFCP,CTBUFFCP                                                
         MVC   CTBUFFST,=CL08'Stoken= '                                         
         MVC   CTBUFFLT,=CL12'Last action='                                     
         MVC   CTBUFFDT,=CL05'Date='                                            
         MVC   CTBUFFTM,=CL05'Time='                                            
         MVC   CTBUFFIT,=CL08'Hit Cnt='                                         
*                                                                               
         MVC   CTBUFFLV,=CL04'Init'                                             
         TM    JOBSTEP,JOBINIT     TEST REBUILD                                 
         BZ    *+10                                                             
         MVC   CTBUFFLV,=CL04'Rbld'                                             
         MVC   CTBUFFSV,ASTOKEN                                                 
         MVC   CTBUFFDV,DATE                                                    
         MVC   CTBUFFTV,TIME                                                    
         XC    CTBUFFIV,CTBUFFIV                                                
*                                                                               
         LA    R4,CTBUFFP                                                       
         USING CTBUFFP,R4                                                       
         LA    R2,RECTAB                                                        
BLDT06   CLI   RECID,0             TEST END OF LIST                             
         BE    BLDT08                                                           
         XC    CTBUFFP,CTBUFFP                                                  
         MVC   CTBUFFP(1),RECID    USE RECORD ID AS EYECATCHER                  
         MVC   CTBUFFPI+1(L'CTBUFFPI-1),CTBUFFPI                                
         MVC   CTBUFFPP,RECPRM                                                  
         AHI   R4,CTBUFFPL                                                      
         AHI   R2,RECTABL                                                       
         B     BLDT06                                                           
         DROP  R2                                                               
*                                                                               
BLDT08   XC    CTBUFFP,CTBUFFP     SET LAST TABLE ENTRY                         
         MVI   CTBUFFP,X'FF'                                                    
         MVC   CTBUFFPI+1(L'CTBUFFPI-1),CTBUFFPI                                
         BRAS  RE,ARSOFF                                                        
         SAM24                                                                  
         DROP  R4                                                               
*                                                                               
         LHI   R1,19                                                            
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD BUFFER OF SINGLE KEY CTFILE RECORDS IN BUFFER                 *         
* NTRY:  R2    A(RECTAB ENTRY)                                        *         
*        FULL  A(START OF BUILD AREA)                                 *         
***********************************************************************         
         USING RECTABD,R2                                                       
BLDSGL   NTR1                      SINGLE KEY BUILD ROUTINE                     
         XR    R0,R0                                                            
         BAS   RE,TIMER                                                         
         MVC   TIME1(8),TIMEN      SAVE TIME/DATE OF INIT                       
*                                                                               
         XC    KEY,KEY             SET KEY FOR THIS RECORD TYPE                 
         MVC   KEY(1),RECID                                                     
*                                                                               
         XC    LIBUFFR,LIBUFFR                                                  
         MVC   LIABUFF,FULL        SET A(BUFFER)                                
         MVC   LILBUFF,RECGET      SET L'BUFFER                                 
         MVI   LIACTN,LIAINI       INITIALISE                                   
         MVI   LIFLAG1,LIF1VAR+LIF1BIG                                          
         MVC   LIKEYL,RECKLEN                                                   
         MVC   LIRECLMX,=AL2(CTMAXLEN)                                          
         GOTO1 VARREDIT,DMCB,LIBUFFD                                            
         CLI   LIRTN,LIROK                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    P1(32),P1           FIRST ONE IS A READ                          
         MVC   P1,ISREAD                                                        
         LA    R3,IO               SET IO AREA ADDRESS                          
         ST    R3,P2                                                            
         MVC   P4,ACTDTF           SET CONTROL FILE                             
         LA    RE,KEY                                                           
         ST    RE,P5               SET A(KEYARG)                                
         B     BSGL04                                                           
*                                                                               
BSGL02   MVC   P1,ISRDSEQ          SUBSEQUENT ARE SEQUENTIAL                    
*                                                                               
BSGL04   GOTO1 VISDDS,P1           CALL ISDDS                                   
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   RECID,0(R3)                                                      
         BNE   BSGL06              END OF RECORDS OF THIS TYPE                  
*                                                                               
         MVI   LIACTN,LIAADD       ADD RECORD TO BUFFER                         
         OI    LIFLAG1,LIF1INS                                                  
         LR    RF,R3                                                            
         AH    RF,RECKDSP                                                       
         ST    RF,LIAREC           SET A(RECORD)                                
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,CTLEN-CTDSECT(R3)                                           
         SH    RF,RECKDSP                                                       
         STCM  RF,3,LIRECL         SET L'RECORD                                 
*                                                                               
         GOTO1 VARREDIT,DMCB,LIBUFFD                                            
         CLI   LIRTN,0                                                          
         BE    BSGL02                                                           
         DC    H'0'                                                             
*                                                                               
BSGL06   MVI   LIACTN,LIAFIX       FIX ARRAY (TURN TO LINKED LIST)              
         GOTO1 VARREDIT,DMCB,LIBUFFD                                            
*                                                                               
         MVC   RECPRM,LIBUFFD      SAVE PARAMETER LIST LOCALLY                  
         MVI   LIACTN,LIAREP       REPORT ON SIZES                              
         GOTO1 VARREDIT,DMCB,LIBUFFD                                            
         L     RF,LIAREC                                                        
         MVC   SZNOW,LIBCURL-LIBBUFFD(RF)                                       
*                                                                               
         XR    R0,R0                                                            
         BAS   RE,TIMER                                                         
         MVC   TIME2(8),TIMEN      SAVE TIME/DATE TABLE REBUILD                 
         BRAS  RE,DOTIME           OUTPUT TIMING INFORMATION                    
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* TIMING AND SIZE MESSAGE OUTPUT                                      *         
* NTRY:  TABLE-NAME OF TABLE OR ZEROS (USE UPRMSG)                    *         
*        TIME1-CLEAR TIME                                             *         
*        TIME2-BUILT TIME                                             *         
*        SZNOW-CURRENT TABLE SIZE                                     *         
*        DSPHD-DATASPACE INFORMATION                                  *         
***********************************************************************         
         USING RECTABD,R2                                                       
DOTIME   NTR1                                                                   
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE+0(11),TIME    TIME                                         
         MVC   PLINE+12(2),DATE+8  DAY                                          
         MVC   DMSG1(L'RECID),RECID                                             
         MVC   PLINE+15(L'DMSG1),DMSG1                                          
         BRAS  RE,PRINTL                                                        
*                                                                               
         L     R0,TIME1            START TIME                                   
         BRAS  RE,TIMER                                                         
         MVC   DMSGT2,TIME                                                      
*                                                                               
         L     R0,TIME2            FINISH TIME                                  
         BRAS  RE,TIMER                                                         
         MVC   DMSGT3,TIME                                                      
*                                                                               
         EDIT  (B4,RECGET),(12,DMSGMAX),0,COMMAS=YES,ZERO=NOBLANK               
         EDIT  (B4,SZNOW),(12,DMSGNOW),0,COMMAS=YES,ZERO=NOBLANK                
*                                                                               
         MVC   DMSGPCT,=CL3'???'   PERCENT FREE                                 
         ICM   R0,15,RECGET                                                     
         ICM   RE,15,RECGET        = (GOT-NOW)/GOT*100                          
         S     RE,SZNOW                                                         
         SRDL  RE,32                                                            
         M     RE,=F'100'                                                       
         DR    RE,R0                                                            
         CVD   RF,DUB              DO NOT ROUND IF REMAINDER                    
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  DMSGPCT,DUB                                                      
*                                                                               
         MVC   PLINE+15(DMSG2L),DMSG2                                           
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+15(DMSG3L),DMSG3                                           
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+15(DMSG4L),DMSG4                                           
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
*                                                                               
DMSG1    DC    C'? Record Build Timing and Size Information'                    
DMSG1L   EQU   *-DMSG1                                                          
*                                                                               
DMSG2    DC    C'Build began at '                                               
DMSGT2   DC    CL11' '                                                          
         DC    C'  Allocated size '                                             
DMSGMAX  DC    CL12' '                                                          
         DC    C' bytes  '                                                      
DMSG2L   EQU   *-DMSG2                                                          
*                                                                               
DMSG3    DC    C'Build ended at '                                               
DMSGT3   DC    CL11' '                                                          
         DC    C'  Amount in use  '                                             
DMSGNOW  DC    CL12' '                                                          
         DC    C' bytes'                                                        
DMSG3L   EQU   *-DMSG3                                                          
*                                                                               
DMSG4    DC    C'Percent free   '                                               
DMSGPCT  DC    CL3' '                                                           
         DC    C'%'                                                             
DMSG4L   EQU   *-DMSG4                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT TIME FROM MVS TIME MACRO                                     *         
* NTRY:  R0=0  CURRENT TIME (FROM MVS TIME MACRO) AND DATE            *         
*        R0=NZ USE TIME IN HHMMSSXX IN R0                             *         
* EXIT:  TIME  HOLDS HH:MM:SS:XX WHERE XX IS 1/100 SECS               *         
***********************************************************************         
TIMER    NTR1                                                                   
         XR    R1,R1                                                            
         LTR   R0,R0               TEST IF TIME PASSED IN R0                    
         BNZ   TIME02                                                           
         TIME  DEC                                                              
TIME01   STM   R0,R1,TIMEN         SAVE TIME/DATE NOW                           
*                                                                               
TIME02   STC   R0,TIME+10                                                       
         OI    TIME+10,X'F0'                                                    
         SRL   R0,4                                                             
         STC   R0,TIME+9           XX PORTION                                   
         OI    TIME+9,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+7                                                        
         OI    TIME+7,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+6           SS PORTION                                   
         OI    TIME+6,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+4                                                        
         OI    TIME+4,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+3           MM PORTION                                   
         OI    TIME+3,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+1                                                        
         OI    TIME+1,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+0           HH PORTION                                   
         OI    TIME+0,X'F0'                                                     
*                                                                               
         MVI   TIME+2,C':'         SET COLONS FOR HH:MM:SS:XX                   
         MVI   TIME+5,C':'                                                      
         MVI   TIME+8,C':'                                                      
*                                                                               
TIME03   LTR   R1,R1               TEST CALL FOR TIME/DATE NOW                  
         BZ    EXIT                                                             
         CLC   DATEN,DATEL         TEST IF DATE HAS CHANGED                     
         BE    TIME05                                                           
TIME04   GOTO1 DATCON,DMCB,(6,DATEN),(23,DATE),0                                
         MVI   DATE+10,C' '                                                     
*                                                                               
TIME05   MVC   TIMEL(8),TIMEN      SAVE LAST TIME/DATE                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATASPACE ROUTINES                                                  *         
***********************************************************************         
GETSPC   NTR1                                                                   
         LHI   R1,4                                                             
         BRAS  RE,DOMSG                                                         
*                                                                               
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    GETSPC2                                                          
*                                                                               
         LHI   R1,6                                                             
         BRAS  RE,DOMSG                                                         
         ABEND 912,DUMP                                                         
*                                                                               
GETSPC2  MVC   DMOFFS,WORK+20      EXTRACT VALUES                               
         MVC   ALET,WORK+24                                                     
         MVC   DMTOKN,WORK+28                                                   
         OC    ALET,ALET                                                        
         BNZ   GETSPC4                                                          
*                                                                               
         LHI   R1,7                                                             
         BRAS  RE,DOMSG                                                         
         ABEND 912,DUMP                                                         
*                                                                               
GETSPC4  MVC   SSB+SSODSPAC-SSOOFF(1),DSPACE+3  SET IT IN SSB ALSO              
         MVC   SSB+SSOTBLET-SSOOFF(4),ALET                                      
         LHI   R1,5                                                             
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET WAIT UNTIL POSTED EITHER BY OPERATOR OR FACPAK                  *         
***********************************************************************         
WAIT     NTR1                                                                   
         BRAS  RE,ARSOFF                                                        
         SAM24                                                                  
         XC    JOBSTEP,JOBSTEP                                                  
         L     R1,AOPERECB         OPERATOR ECB COMES LAST                      
         WAIT  ECB=(R1)                                                         
         BRAS  RE,OPSCOMS                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
SETOPS   NTR1                                                                   
         LHI   R1,20               OUTPUT INITIALISING OPERATOR MSG             
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     RF,RXCOM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         QEDIT ORIGIN=(R3),BLOCK=(R2) RELEASE THE CIB                           
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1   ACCEPT MODIFY COMMANDS                    
*                                                                               
         LHI   R1,21                                                            
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* PROCESS OPERATOR COMMUNICATIONS                                     *         
***********************************************************************         
OPSCOMS  NTR1                                                                   
         LHI   R1,22                                                            
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     RF,RXCOM            SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         USING CIBNEXT,R2                                                       
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   OPSC02                                                           
         MVI   JOBSTEP,JOBSTOP     SET EOJ FLAG                                 
         LHI   R1,24               STOP JOB MESSAGE                             
         BRAS  RE,DOMSG                                                         
         B     OPSC14                                                           
*                                                                               
OPSC02   CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    OPSC04                                                           
         LHI   R1,25               BAD COMMAND (UNKNOWN VERB)                   
         BRAS  RE,DOMSG                                                         
         B     OPSC14                                                           
*                                                                               
OPSC04   XR    R1,R1               GET DATA LEN IN R1                           
         ICM   R1,3,CIBDATLN                                                    
         CHI   R1,8                                                             
         BNH   OPSC06                                                           
         LHI   R1,25               BAD COMMAND (TOO LONG)                       
         BRAS  RE,DOMSG                                                         
         B     OPSC14                                                           
*                                                                               
OPSC06   CHI   R1,3                                                             
         BNE   OPSC08                                                           
         CLC   CIBDATA(3),=C'EOJ'                                               
         BNE   OPSC08                                                           
         MVI   JOBSTEP,JOBEOJ      SET EOJ FLAG                                 
         LHI   R1,26               EOJ MESSAGE                                  
         BRAS  RE,DOMSG                                                         
         B     OPSC14                                                           
*                                                                               
OPSC08   CHI   R1,4                                                             
         BNE   OPSC10                                                           
         CLC   CIBDATA(4),=C'STOP'                                              
         BNE   OPSC10                                                           
         MVI   JOBSTEP,JOBEOJ      SET EOJ FLAG                                 
         LHI   R1,26               EOJ MESSAGE                                  
         BRAS  RE,DOMSG                                                         
         B     OPSC14                                                           
*                                                                               
OPSC10   CHI   R1,7                                                             
         BNE   OPSC12                                                           
         CLC   CIBDATA(7),=C'REBUILD'                                           
         BNE   OPSC12                                                           
         MVI   JOBSTEP,JOBINIT                                                  
         LHI   R1,27               REBUILD MESSAGE                              
         BRAS  RE,DOMSG                                                         
         B     OPSC14                                                           
*                                                                               
OPSC12   LHI   R1,28               INVALID MESSAGE                              
         BRAS  RE,DOMSG                                                         
         MVC   PLINE(22),=C'**Input message was - '                             
         XR    R1,R1                                                            
         ICM   R1,3,CIBDATLN                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLINE+22(0),CIBDATA                                              
         BRAS  RE,PRINTL                                                        
         MVI   JOBSTEP,JOBBAD                                                   
*                                                                               
         MVC   PLINE(32),=C'INVALID KEYWORD               //'                   
         XR    R1,R1                                                            
         ICM   R1,3,CIBDATLN                                                    
         CHI   R1,13                                                            
         BNH   *+8                                                              
         LHI   R1,13                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLINE+16(0),CIBDATA                                              
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
*                                                                               
OPSC14   L     RF,RXCOM            CLEAR OPERATOR COMMAND                       
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         BZ    OPSC16                                                           
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)                                           
         DROP  RF                                                               
*                                                                               
OPSC16   LHI   R1,23                                                            
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE INPUT CARDS                                     *         
***********************************************************************         
CARDVAL  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1               R2=A(CARD START)                             
         LA    R1,79(R2)                                                        
         ST    R1,CARDEND          SAVE A(LAST CHAR)                            
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
         GOTO1 VSCANNER,DMCB,(C'C',(R2)),(1,SCNBLK)                             
         CLI   4(R1),0                                                          
         BNE   *+12                                                             
         LHI   R1,33               INVALID LINE                                 
         B     CERR                                                             
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         LA    R3,CARDTAB                                                       
         USING CARDTABD,R3                                                      
         XR    RF,RF                                                            
*                                                                               
CARDV02  CLI   CNAME,CARDEOT       TEST END OF TABLE                            
         BNE   *+12                                                             
         LHI   R1,39               INVALID KEYWORD                              
         B     CERR                                                             
*                                                                               
         ICM   RF,1,CXLEN                                                       
         EX    RF,*+8                                                           
         BE    CARDV06                                                          
         CLC   SC1STFLD(0),CNAME                                                
CARDV04  LA    R3,CARDTABL(R3)                                                  
         B     CARDV02                                                          
*                                                                               
CARDV06  CLI   CTYPE,CTNUM         TEST NUMERIC INPUT                           
         BNE   CARDV08                                                          
         TM    SC2NDVAL,SCNUMQ                                                  
         BO    *+12                                                             
         LHI   R1,34                                                            
         B     CERR                                                             
*                                                                               
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BNL   *+12                                                             
         LHI   R1,35               TOO LITTLE                                   
         B     CERR                                                             
*                                                                               
         CLC   SC2NDNUM,CMAX                                                    
         BNH   *+12                                                             
         LHI   R1,36               TOO BIG                                      
         B     CERR                                                             
*                                                                               
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         B     EXITOK                                                           
*                                                                               
CARDV08  CLI   CTYPE,CTCHR         TEST CHARACTER INPUT                         
         BNE   CARDV10                                                          
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         LHI   R1,32               INVALID/MISSING                              
         B     CERR                                                             
*                                                                               
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BNL   *+12                                                             
         LHI   R1,37               TOO SHORT                                    
         B     CERR                                                             
*                                                                               
         C     RF,CMAX                                                          
         BNH   *+12                                                             
         LHI   R1,38               TOO LONG                                     
         B     CERR                                                             
*                                                                               
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SC2NDFLD                                                 
         B     EXITOK                                                           
*                                                                               
CARDV10  DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CERR     BRAS  RE,DOMSG                                                         
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINTER                                                  *         
***********************************************************************         
PRINTI   NTR1                                                                   
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         XR    R0,R0                                                            
         BRAS  RE,TIMER            GET CURRENT DATE AND TIME                    
         MVC   PTITLE+20(10),DATE                                               
         PUT   SYSPRINT,PTITLE     PRINT TITLES                                 
         PUT   SYSPRINT,PTITLEU                                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT LINE                                                          *         
***********************************************************************         
PRINTL   NTR1                                                                   
         PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         B     EXIT                EXIT                                         
         EJECT                                                                  
***********************************************************************         
* CLOSE PRINTER                                                       *         
***********************************************************************         
PRINTX   NTR1                                                                   
         CLOSE SYSPRINT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT INFORMATION MESSAGE                                          *         
* NTRY:  R1=0  ZERO MESSAGE IS ALREADY ON PRINT LINE                  *         
*        R1=NZ INDEX TO MESSAGE                                       *         
***********************************************************************         
DOMSG    NTR1                                                                   
         LTR   R1,R1                                                            
         BZ    DOMSG02                                                          
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE+0(11),TIME    TIME                                         
         MVC   PLINE+12(2),DATE+8  DAY                                          
*                                                                               
         BCTR  R1,0                                                             
         MHI   R1,L'MESSTAB                                                     
         A     R1,AMESSTAB                                                      
         MVC   PLINE+15(L'MESSTAB),0(R1)                                        
*                                                                               
DOMSG02  BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EXITS AND OTHER USEFUL ROUTINES                                     *         
***********************************************************************         
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
XBASE    BRAS  RE,PRINTX           CLOSE PRINTING AND EXIT TO MVS               
         L     RD,SAVERD                                                        
         XBASE                                                                  
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         DS    0L                                                               
         DC    CL16'CARDTAB*CARDTAB*'                                           
*                                                                               
CARDTAB  DC    CL8'RUN     ',F'001',F'0000010'                                  
         DC    AL1(02,CTCHR,L'RUN,0),AL4(RUN)                                   
         DC    CL8'DSPACE  ',F'001',F'0000012'                                  
         DC    AL1(05,CTCHR,L'DSPACE,0),AL4(DSPACE)                             
         DC    CL8'DDSIO   ',F'001',F'0000012'                                  
         DC    AL1(04,CTCHR,8,0),V(DDSIO)                                       
CARDTABX DC    AL1(CARDEOT)                                                     
         DS    0L                                                               
         DC    CL16'CARDTABXCARDTABX'                                           
         EJECT                                                                  
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    AL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CLEN     DS    AL1                 OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    AL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
CTBUFF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
EOT      EQU   X'FF'                                                            
CTMAXLEN EQU   1024                                                             
CTMAXID  EQU   997                                                              
ARZERO   DC    16F'0'                                                           
*                                                                               
ISREAD   DC    F'1'                                                             
ISRDSEQ  DC    F'2'                                                             
*                                                                               
VHEXOUT  DC    V(HEXOUT)                                                        
VSCANNER DC    V(SCANNER)                                                       
VISDDS   DC    V(ISDDS)                                                         
VCARDS   DC    V(CARDS)                                                         
VARREDIT DC    V(ARREDIT)                                                       
VLOCKSPC DC    V(LOCKSPC)                                                       
VDMGR    DC    V(DATAMGR)                                                       
VHELLO   DC    V(HELLO)                                                         
DATCON   DC    V(DATCON)                                                        
*                                                                               
TIMEL    DC    X'00000000'         LAST TIME PWOS=HHMMSSHH                      
DATEL    DC    X'00000000'         LAST DATE PWOS=YYYYMMDD                      
AMESSTAB DC    A(MESSTAB)                                                       
*                                                                               
DMOPEN   DC    CL8'DMOPEN'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DTFAD    DC    CL8'DTFAD '                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
*                                                                               
DMSYS    DC    CL8'CONTROL '                                                    
DMFLIST  DC    C'NCTFILE X'                                                     
*                                                                               
PTITLE   DC    CL132'CTBUFFER Stats Date=YYYY-MM-DD'                            
PTITLEU  DC    CL132'-------------- ---------------'                            
*                                                                               
         DS    0D                                                               
         DC    CL8'EXTRACTR'                                                    
RESULTS  DS    0F                  RESULTS FROM EXTRACT MACRO                   
RXTIOT   DC    F'0'                TIOT                                         
RXCOM    DC    F'0'                COMMS BLOCK                                  
RXASID   DC    F'0'                ASID                                         
         DC    F'0'                N/D                                          
*                                                                               
         DC    CL8'JOBSTEP='                                                    
JOBSTEP  DC    X'00'               OPERATOR COMMAND FLAGS                       
JOBEOJ   EQU   X'80'               TERMINATE                                    
JOBINIT  EQU   X'40'               REBUILD                                      
JOBSTOP  EQU   X'20'               FORCE CANCEL                                 
JOBBAD   EQU   X'10'               BAD INPUT                                    
         DC    XL7'00'                                                          
*                                                                               
         DC    CL8'GETMAIN*'                                                    
GETADR   DC    F'0'                                                             
GETLEN   DC    F'0'                                                             
*                                                                               
         DC    CL8'STOKEN**'                                                    
ASTOKEN  DC    D'0'                                                             
*                                                                               
         DC    CL8'OPERECB*'                                                    
AOPERECB DC    A(0)                                                             
         DC    A(0)                                                             
*                                                                               
RUN      DC    CL4'DSP'                                                         
TEST     DC    C'N'                                                             
DSPACE   DC    CL12' '                                                          
*                                                                               
* FATABSDEQU                                                                    
       ++INCLUDE FATABSDEQU                                                     
*                                                                               
SPACES   DC    132C' '                                                          
         EJECT                                                                  
***********************************************************************         
* DCBS                                                                *         
***********************************************************************         
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FB,LRECL=132             
         EJECT                                                                  
***********************************************************************         
* RECORD TABLE                                                        *         
***********************************************************************         
RECTAB   DS    0D                                                               
*                                                                               
         DC    X'01',XL3'000000',A(BLDSGL)   ?-PROGRAM SAVE                     
         DC    AL2(CT01AGID-CT01KEY,CT01LEN-CT01AGID)                           
         DC    A(0,0,0,0),XL64'00'                                              
*                                                                               
         DC    X'99',XL3'000000',A(BLDSGL)   ?-RADIO                            
         DC    AL2(L'CTKID,L'CTKREST)                                           
         DC    A(0,0,0,0),XL64'00'                                              
*                                                                               
         DC    X'9B',XL3'000000',A(BLDSGL)   ?-AGENCY/USERID# PASSIVE           
         DC    AL2(L'CTKID,L'CTKREST)                                           
         DC    A(0,0,0,0),XL64'00'                                              
*                                                                               
         DC    X'C6',XL3'000000',A(BLDSGL)   F-SECURITY                         
         DC    AL2(L'CTKID,L'CTKREST)                                           
         DC    A(0,0,0,0),XL64'00'                                              
*                                                                               
         DC    X'C9',XL3'000000',A(BLDSGL)   I-USER ID                          
         DC    AL2(CTIKID-CTIREC,CTILEN-CTIKID)                                 
         DC    A(0,0,0,0),XL64'00'                                              
*                                                                               
         DC    X'D7',XL3'000000',A(BLDSGL)   P-PROGRAM PROFILE                  
         DC    AL2(CTPKSYS-CTPREC,CTPLEN-CTPKSYS)                               
         DC    A(0,0,0,0),XL64'00'                                              
*                                                                               
         DC    X'E3',XL3'000000',A(BLDSGL)   T-TERMINAL                         
         DC    AL2(CTTKTID-CTTREC-1,CTTLEN-CTTKTID+1)                           
         DC    A(0,0,0,0),XL64'00'                                              
*                                                                               
         DC    X'E4',XL3'000000',A(BLDSGL)   U-USER PROFILE                     
         DC    AL2(CTUKSYS-CTUREC,CTULEN-CTUKSYS)                               
         DC    A(0,0,0,0),XL64'00'                                              
*                                                                               
         DC    X'E6',XL3'000000',A(BLDSGL)   W-SYSYTEM LIST                     
         DC    AL2(CTWKAGY-CTWREC,CTWLEN-CTWKAGY)                               
         DC    A(0,0,0,0),XL64'00'                                              
*                                                                               
         DC    X'F0',XL3'000000',A(BLDSGL)   0-AUTH                             
         DC    AL2(L'CTKID,L'CTKREST)                                           
         DC    A(0,0,0,0),XL64'00'                                              
*                                                                               
         DC    X'F3',XL3'000000',A(BLDSGL)   3-HELP TEXT                        
         DC    AL2(L'CTKID,L'CTKREST)                                           
         DC    A(0,0,0,0),XL64'00'                                              
*                                                                               
         DC    X'F5',XL3'000000',A(BLDSGL)   5-ACCESS                           
         DC    AL2(CT5KALPH-CT5REC,CT5LEN-CT5KALPH)                             
         DC    A(0,0,0,0),XL64'00'                                              
*                                                                               
         DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
* UTL AND SSB FOR DDSIO                                               *         
***********************************************************************         
         DS    0D                                                               
         DC    CL16'UTL*UTL*UTL*UTL*'                                           
UTL      DC    F'0'                                                             
         DC    X'0A'                                                            
         DC    XL251'00'                                                        
*                                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      DC    H'0'                                                             
         DC    X'FF'                                                            
         DC    X'02'               SUPPRESS RECOVERY                            
         DC    1020X'00'                                                        
         EJECT                                                                  
***********************************************************************         
* MESSAGES                                                            *         
***********************************************************************         
MESSTAB  DS    0CL60                                                            
  DC CL60'Began processing input parameters from cards                '         
  DC CL60'Ended processing input parameters from cards                '         
  DC CL60'Card validation error - Application terminating             '         
  DC CL60'Attempting Dataspace binds                                  '         
  DC CL60'Completed  Dataspace binds                                  '         
  DC CL60'Unable to bind to TABS dataspace - Application terminating  '         
  DC CL60'No ALET for TABS dataspace- Application terminating         '         
  DC CL60'Began opening files                                         '         
  DC CL60'Ended opening files                                         '         
  DC CL60'FREEMAIN not required                                       '         
  DC CL60'FREEMAIN performed as requested                             '         
  DC CL60'FREEMAIN error - Application terminating - check return code'         
  DC CL60'Began calculating size requirements                         '         
  DC CL60'Ended calculating size requirements                         '         
  DC CL60'Began GETMAIN calculations                                  '         
  DC CL60'Ended GETMAIN calculations                                  '         
  DC CL60'GETMAIN error - Application terminating - check return code '         
  DC CL60'Began building tables in XA storage                         '         
  DC CL60'Ended building tables in XA storage                         '         
  DC CL60'Began setting operator communications                       '         
  DC CL60'Ended setting operator communications                       '         
  DC CL60'Operator input detected - beginning processing              '         
  DC CL60'Operator input processing completed                         '         
  DC CL60'Operator input was "STOP JOB"                               '         
  DC CL60'Operator input was an invalid command                       '         
  DC CL60'Operator input was "EOJ JOB"                                '         
  DC CL60'Operator input was "REBUILD" - command accepted             '         
  DC CL60'Operator input was not valid - command ignoredd             '         
  DC CL60'Began reporting on I/O savings before rebuild               '         
  DC CL60'Ended reporting on I/O savings before rebuild               '         
  DC CL60'About to attempt FREEMAIN on storage                        '         
  DC CL60'*** ERROR *** Invalid/Missing value input                   '         
  DC CL60'*** ERROR *** Invalid Line Format                           '         
  DC CL60'*** ERROR *** Value input is not a valid number             '         
  DC CL60'*** ERROR *** Numeric value input is too small              '         
  DC CL60'*** ERROR *** Numeric value input is too big                '         
  DC CL60'*** ERROR *** Length of input string is too short           '         
  DC CL60'*** ERROR *** Length of input string is too long            '         
  DC CL60'*** ERROR *** Invalid keyword                               '         
*40                                                                             
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DC                                                  *         
***********************************************************************         
         DS    0L                                                               
         DC    CL16'WORKAREAWORKAREA'                                           
WORKAREA DC    200000X'00'                                                      
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
XDUB     DS    D                                                                
DSPHD    DS    XL64                                                             
LIBUFFR  DS    XL(LIBUFFL)                                                      
*                                                                               
SAVERD   DS    A                                                                
CARDRD   DS    A                                                                
CARDEND  DS    A                                                                
*                                                                               
FULL     DS    F                                                                
TOTCNT   DS    F                                                                
TOTTOT   DS    F                                                                
TOTMOD   DS    F                                                                
TOTGET   DS    F                                                                
*                                                                               
ACTDTF   DS    A                                                                
*                                                                               
TIMEN    DS    F                   TIME NOW PWOS=HHMMSSHH                       
DATEN    DS    F                   DATE NOW PWOS=YYYYMMDD                       
TIME1    DS    F                                                                
DATE1    DS    F                                                                
TIME2    DS    F                                                                
DATE2    DS    F                                                                
*                                                                               
HITSON   DS    D                                                                
HITSOFF  DS    D                                                                
HITSTOT  DS    D                                                                
MISSON   DS    D                                                                
MISSOFF  DS    D                                                                
MISSTOT  DS    D                                                                
BIGPACK  DS    PL16                                                             
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
P7       DS    F                                                                
P8       DS    F                                                                
*                                                                               
DMOFFS   DS    A                   DATASPACE OFFSET                             
ALET     DS    A                   ALET                                         
DMTOKN   DS    CL8                 TOKEN                                        
*                                                                               
SZNOW    DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
TIME     DS    CL11                HH:MM:SS:XX                                  
DATE     DS    CL11                YYYY-MM-DD                                   
*                                                                               
         DS    0D                                                               
WORK     DS    CL64                                                             
CARD     DS    CL80                                                             
*                                                                               
PLINE    DS    CL132                                                            
*                                                                               
IDKLIST  DS    XL(2+L'CTIKID)                                                   
KEY      DS    XL25                                                             
*                                                                               
SCNBLK   DS    3CL(SCBLKLQ)                                                     
*                                                                               
IO       DS    2048C                                                            
IO2      DS    2048C                                                            
MAXIOL   EQU   *-IO                                                             
WORKL    EQU   *-WORKD                                                          
*                                                                               
RECTABD  DSECT                                                                  
RECID    DS    C                   RECORD TYPE                                  
RECSPR   DS    XL3                 N/D                                          
RECRTN   DS    A                   A(BUILD ROUTINE)                             
RECKDSP  DS    H                   DISP TO START OF KEY                         
RECKLEN  DS    H                   LENGTH OF KEY                                
RECCNT   DS    A                   COUNT                                        
RECTOT   DS    A                   BYTE COUNT                                   
RECMOD   DS    A                   MODIFIED BYTE COUNT                          
RECGET   DS    A                   GETMAIN BYTE COUNT                           
RECPRM   DS    XL48                LOCAL PARAMETER LIST                         
         DS    XL16                                                             
RECTABL  EQU   *-RECTABD                                                        
*                                                                               
CTDSECT  DSECT                     GENERIC CTFILE KEY                           
CTKEY    DS    0XL25                                                            
CTKID    DS    X                                                                
CTKREST  DS    XL24                                                             
CTLEN    DS    XL2                                                              
CTSTAT   DS    XL1                                                              
CTREC    EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* SCANNER CLONE FOR INPUT CARD VALIDATION                             *         
***********************************************************************         
SCANNER  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SWORKL,**SCAN**                                                  
         USING SWORKD,RC                                                        
         LR    R9,R1               R9=A(PARAMETER LIST)                         
         LM    R2,R3,0(R9)         R2=A(DATA STRING) R3=A(BLOCK)                
         MVC   MAXLINES,4(R9)                                                   
         XC    SDISP,SDISP                                                      
         XR    R4,R4                                                            
         IC    R4,5(R2)            L'DATA IF SCREEN FIELD                       
         LA    R2,8(R2)                                                         
         MVC   LROW,=H'42'         PRESET DEFAULT LENGTHS                       
         MVC   LRIGHT,=H'20'                                                    
         MVC   LBOTH,=H'30'                                                     
         CLI   0(R9),C'C'                                                       
         BE    SCAN1                                                            
*                                                                               
SCAN1    AHI   R2,-8                                                            
         LA    R4,80                                                            
         CLC   0(80,R2),SSPACES                                                 
         BE    ERROR2                                                           
         LA    R5,79(R2)                                                        
*                                                                               
SCAN2    CLI   0(R5),C' '                                                       
         BNE   SCAN4                                                            
         BCTR  R5,0                                                             
         BCT   R4,SCAN2                                                         
*                                                                               
SCAN4    LA    R5,0(R2,R4)         L'DATA IN R4                                 
         MVC   BORROW,0(R5)        SAVE THE NEXT CHARACTER                      
         MVC   0(1,R5),COMMA       AND POP IN A COMMA TO SIMPLIFY               
         SR    R6,R6               R6=NUMBER OF LINES USED                      
                                                                                
***********************************************************************         
*HANDLE LINES OF DATA                                                 *         
***********************************************************************         
SCAN6    XC    0(12,R3),0(R3)      PRESET A LINE                                
         LH    RF,LBOTH                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),SSPACES                                                 
         MVC   2(2,R3),=X'E0E0'                                                 
         BRAS  RE,GETL                                                          
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   2(R3),0                                                          
         CLI   1(R3),0                                                          
         BNE   *+8                                                              
         MVI   3(R3),0                                                          
         CLC   0(1,R3),LBOTH+1                                                  
         BH    ERROR                                                            
         CLC   1(1,R3),LRIGHT+1                                                 
         BH    ERROR                                                            
         CLI   1(R3),0                                                          
         BE    SCAN8                                                            
         CLI   0(R3),10                                                         
         BH    ERROR                                                            
*                                                                               
SCAN8    SR    R7,R7                                                            
         IC    R7,0(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN18                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),0(R2)                                                   
         TM    2(R3),X'80'                                                      
         BZ    SCAN10                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN10                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,5(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    SCAN10                                                           
         ST    R8,4(R3)                                                         
*                                                                               
SCAN10   LA    R2,2(R2,R7)                                                      
         IC    R7,1(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN20                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R3),0(R2)                                                   
         TM    3(R3),X'80'                                                      
         BZ    SCAN12                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN12                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,9(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING SDISPS.              
         BO    SCAN12                                                           
         ST    R8,8(R3)                                                         
*                                                                               
SCAN12   LA    R2,2(R2,R7)                                                      
         B     SCAN20                                                           
*                                                                               
VARPAK   PACK  SDUB,0(0,R2)                                                     
*                                                                               
SCAN18   LA    R2,1(R2)                                                         
         CLI   1(R3),0                                                          
         BNE   ERROR                                                            
*                                                                               
SCAN20   LA    R6,1(R6)            BUMP N'LINES                                 
         AH    R3,LROW             BUMP TO NEXT LINE IN BLOCK                   
         CR    R2,R5               ARE WE NOW PAST LAST 'COMMA'                 
         BH    OK                                                               
         IC    R7,MAXLINES                                                      
         LTR   R7,R7                                                            
         BZ    SCAN6                                                            
         CR    R6,R7               HAVE WE REACHED MAX N'LINES                  
         BNE   SCAN6                                                            
*                                                                               
OK       MVC   0(1,R5),BORROW      RETURN THE BYTE                              
         STC   R6,4(R9)            SET NUMBER OF LINES USED                     
         B     XIT                                                              
*                                                                               
ERROR    MVI   4(R9),0                                                          
         MVC   0(1,R5),BORROW                                                   
         MVC   2(2,R3),=X'FFFF'                                                 
         B     XIT                                                              
*                                                                               
ERROR2   MVI   4(R9),0                                                          
         MVC   2(2,R3),=X'FFFF'                                                 
*                                                                               
XIT      XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND GET LENGTHS                                            *         
***********************************************************************         
GETL     NTR1                                                                   
         LR    R4,R3                                                            
         SR    R5,R5                                                            
         TM    4(R9),X'80'                                                      
         BZ    GETL2                                                            
         MVC   4(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
*                                                                               
GETL2    CLC   0(1,R2),COMMA       TEST FIELD SEPERATOR                         
         BE    GETL12                                                           
         CLC   0(1,R2),EQUAL                                                    
         BE    GETL14                                                           
*                                                                               
GETL3    LA    R5,1(R5)                                                         
         CLI   0(R2),C'9'                                                       
         BNH   *+8                                                              
         MVI   2(R4),0             (ALL INVALID)                                
         CLI   0(R2),C'0'                                                       
         BL    GETL4                                                            
         NI    2(R4),X'BF'         (INVALID ALPHA)                              
         B     GETL10                                                           
*                                                                               
GETL4    NI    2(R4),X'7F'         (INVALID NUM)                                
         CLI   0(R2),C'Z'                                                       
         BNH   GETL6                                                            
         MVI   2(R4),0             Z-0 = ALL INVALID                            
         B     GETL10                                                           
*                                                                               
GETL6    CLI   0(R2),C'A'          LESS THAN A = ALL INVALID                    
         BNL   GETL8                                                            
         MVI   2(R4),0                                                          
         B     GETL10                                                           
*                                                                               
GETL8    CLI   0(R2),C'F'          OK FOR ALPHA                                 
         BNH   GETL10                                                           
         NI    2(R4),X'DF'         G-Z = INVALID HEX                            
*                                                                               
GETL10   LA    R2,1(R2)                                                         
         B     GETL2                                                            
*                                                                               
GETL12   STC   R5,0(R4)            COMMA FOUND                                  
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         B     XIT                                                              
*                                                                               
GETL14   CR    R4,R3               EQUAL FOUND - IS THIS THE FIRST ONE?         
         BNE   GETL3               TREAT AS NORMAL CHARACTER IF NOT             
         STC   R5,0(R4)            NOW STORE L1                                 
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         TM    4(R9),X'80'                                                      
         BZ    GETL16                                                           
         MVC   8(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
*                                                                               
GETL16   LA    R4,1(R4)            POINT TO FIELD2 DATA                         
         SR    R5,R5               CLEAR L2                                     
         LA    R2,1(R2)            POINT PAST EQUAL SIGN                        
         B     GETL2                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
COMMA    DC    C','                                                             
EQUAL    DC    C'='                                                             
SSPACES  DC    CL80' '                                                          
         EJECT                                                                  
***********************************************************************         
* SCANNER WORKING STORAGE DSECT                                       *         
***********************************************************************         
SWORKD   DSECT                                                                  
SDUB     DS    D                                                                
SWORK    DS    CL32                                                             
LASTSTOP DS    F                                                                
BORROW   DS    CL1                                                              
MAXLINES DS    CL1                                                              
LROW     DS    H                                                                
LRIGHT   DS    H                                                                
LBOTH    DS    H                                                                
SDISP    DS    H                                                                
SWORKL   EQU   *-SWORKD                                                         
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         EJECT                                                                  
***********************************************************************         
* DATASPACE BUFFER DSECT                                              *         
***********************************************************************         
* CTBUFFERD                                                                     
       ++INCLUDE CTBUFFERD                                                      
                                                                                
* DDSCANBLKD                                                                    
       ++INCLUDE DDSCANBLKD                                                     
                                                                                
* FATABSD                                                                       
       ++INCLUDE FATABSD                                                        
                                                                                
* FASSBOFF                                                                      
       ++INCLUDE FASSBOFF                                                       
                                                                                
* DMDSHDR                                                                       
       ++INCLUDE DMDSHDR                                                        
                                                                                
* DMSPACED                                                                      
       ++INCLUDE DMSPACED                                                       
                                                                                
* DDARREDITD                                                                    
       ++INCLUDE DDARREDITD                                                     
                                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CTBUFFER  03/21/14'                                      
         END                                                                    
