*          DATA SET CTLFM21A   AT LEVEL 140 AS OF 05/01/02                      
*PHASE TA0221A,+0                                                               
*INCLUDE SCINKEY                                                                
         TITLE 'CTLFM21 - CONTROL FILE MAINT - MPA/NTI PROGRAM CODES'           
CTLFM21  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 4000,**LFM21**,RA                                                
         USING WORKD,RC            RC=A(W/S)                                    
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(TEMP W/S)                               
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTDPREC,R4          R4=A(RECORD)                                 
*                                                                               
         L     R1,APARM                                                         
         L     RE,12(R1)                                                        
         USING COMFACSD,RE                                                      
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VXSORT,CXSORT                                                    
         MVC   VCLLOV,CCALLOV                                                   
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VADDAY,CADDAY                                                    
*                                                                               
         GOTO1 VCLLOV,DMCB,0,X'D9000A17',0,0     NETWEEK                        
         MVC   VNETWEEK,DMCB       SAVE NETWEEK CORE-RES ADDRESS                
         MVI   FIRSTTME,C'Y'       FIRST TIME AROUND                            
*                                                                               
         CLI   ACTN,CHANGE                                                      
         BE    KEYVAL                                                           
         CLI   ACTN,DISPLAY                                                     
         BE    KEYVAL                                                           
         MVI   FERN,11             INVALID ACTION                               
         B     EXIT                                                             
         DROP  RE                                                               
*                                                                               
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
KEYVAL   GOTO1 AFVAL,DEMSRCEH                                                   
         BZ    EIIF                                                             
         CLI   DEMSRCE,C'A'        VALID SOURCES ARE ARBITRON                   
         BE    KEYVAL02                                                         
         CLI   DEMSRCE,C'N'        AND NIELSON                                  
         BNE   EIIF                                                             
KEYVAL02 XC    CTDPKEY,CTDPKEY                                                  
         MVI   CTDPKTYP,CTDPKTEQ                                                
         MVI   CTDPKTY2,CTDPKT2E                                                
         MVI   CTDPKFIL,C'N'                                                    
         MVC   CTDPKSRC,DEMSRCE                                                 
         MVC   CTDPKBOK,=X'FFFF'                                                
*                                                                               
         LA    RE,TABLE1                                                        
         A     RE,=F'9999'                                                      
         ST    RE,ATABLE2                                                       
         MVC   DATADISP,=H'23'                                                  
*                                                                               
         GOTO1 AFVAL,DEMOPTSH                                                   
         BZ    KEYVAL03                                                         
         CLI   DEMOPTS,C'M'                                                     
         BE    KEYVAL05                                                         
         CLI   DEMOPTS,C'N'                                                     
         BE    KEYVAL05                                                         
         CLI   DEMOPTS,C'A'                                                     
         BE    KEYVAL05                                                         
         B     EIIF                                                             
KEYVAL03 MVI   DEMOPTS,C'A'                                                     
         OI    DEMOPTSH+6,X'80'                                                 
KEYVAL05 CLI   ACTN,CHANGE         CHECK THE FIELDS IF CHANGE                   
         BNE   KEYVAL08                                                         
         LA    R1,16                                                            
         LA    R5,DEMMPPCH                                                      
         LA    R6,DEMLAST                                                       
KEYVAL06 CR    R5,R6                                                            
         BH    KEYVAL08                                                         
         TM    4(R5),X'80'                                                      
         BNO   KEYVAL07                                                         
         TM    4(R5),X'08'                                                      
         BO    KEYVAL07                                                         
         ST    R5,FADR                                                          
         B     EIIF                                                             
KEYVAL07 LA    R5,NEXTLINE(R5)                                                  
         BCT   R1,KEYVAL06                                                      
*                                  VALIDATE BOOK CODE                           
KEYVAL08 GOTO1 AFVAL,DEMBOOKH                                                   
         BZ    EIIF                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'MP'       FOR MPA FILE                                 
         MVC   KEY+2(1),DEMSRCE    MPA SOURCE                                   
         XC    PRGNUM,PRGNUM                                                    
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,BOOKVAL          GET THE MPA BOOK                             
         BNZ   EIIF                                                             
*                                                                               
*        B     KEYVAL20                                                         
*                                                                               
         MVI   SYSNUM,2            SPOT SYSTEM #                                
         BAS   RE,SWTCHSYS                                                      
         XCEF  TABLE1,9999         CLEAR THE TABLE                              
*                                                                               
         LA    R5,TABLE1                                                        
         USING TABLED,R5                                                        
         SR    R6,R6               COUNTER                                      
*                                                                               
KEYVAL10 DS    0H                                                               
         MVC   PRGNUM(2),KEYSAVE+3                                              
         XC    KEY+7(5),KEY+7                                                   
         MVC   KEY+5(2),BOOKREAL                                                
         SR    R1,R1                                                            
         ICM   R1,3,PRGNUM                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,PRGNUM                                                      
*                                                                               
         MVC   KEY+3(2),PRGNUM                                                  
KEYVAL11 BAS   RE,SYSHIGH                                                       
         CLC   =C'MP',KEY          MAKE SURE THE KEY IS GOOD                    
         BNE   KEYVAL15                                                         
         CLC   DEMSRCE(1),KEY+2    MAKE SURE THE KEY IS GOOD                    
         BNE   KEYVAL15                                                         
         CLC   BOOKREAL,KEY+5      CHECK THE BOOK TOO                           
         BL    KEYVAL10            OUR BOOK IS LOWER, NEXT PROGRAM              
         BE    KEYVAL14                                                         
         MVC   KEY+5(2),BOOKREAL                                                
         XC    KEY+7(5),KEY+7                                                   
*        SR    R1,R1                                                            
*        ICM   R1,3,KEY+5          GET NEXT BOOK                                
*        LA    R1,1(R1)                                                         
*        STCM  R1,3,KEY+5                                                       
         B     KEYVAL11                                                         
KEYVAL14 LA    R6,1(R6)            RECORD IS OK, GET IT                         
         MVC   PRGNUM,KEY+3                                                     
         MVC   WKAREA(24),KEY                                                   
         BAS   RE,SYSGETRC                                                      
*                                                                               
         BAS   RE,GETNTI21         GET INFO FROM RECORD TO TABLE                
         CH    R6,=H'400'                                                       
         BH    KEYVAL15                                                         
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         LA    R5,TABLELEN(R5)                                                  
         MVC   KEYSAVE+3(2),PRGNUM                                              
         B     KEYVAL10                                                         
*                                                                               
KEYVAL15 MVI   0(R5),X'FF'         END OF TABLE                                 
         LTR   R6,R6                                                            
         BZ    EIIO                                                             
         GOTO1 VXSORT,DMCB,TABLE1,(R6),27,25,0                                  
*                                                                               
KEYVAL20 DS    0H                                                               
         MVI   SYSNUM,3            DON'T NEED TO SWITCH                         
*                                  SPOT CAN ACCESS PAVDIR AND PAVFIL            
         XC    KEY,KEY                                                          
         MVC   KEY(3),=C'QNN'                                                   
         MVC   KEY+3(2),=X'5B01'   TST SYSTEM'S ONLY BOOK                       
         B     KEYVAL21            CHANGE THIS TO KEYVAL22 FOR TST              
*-------------------------------------------------------------------            
* QUICK FIX FOR NTI BOOK FROM MPA BOOK.                                         
* DON'T USE THIS UNTIL LIVE BECAUSE TEST DOESN'T HAVE ALL BOOKS                 
*-------------------------------------------------------------------            
MONTHEND DC    AL1(31,29,31,30,31,30,31,31,30,31,30,31)                         
NETCNV   DC    CL6' '                                                           
NETCNV2  DC    CL6' '                                                           
STBOOK   DS    0XL2                                                             
STYEAR   DC    X'00'                                                            
STWEEK   DC    X'00'                                                            
ENDWEEK  DC    X'00'                                                            
KEYVAL21 DS    0H                                                               
         MVC   NETCNV,BOOKDATE     GRAB 6 BYTE DATE YYMMDD                      
         MVI   NETCNV+5,C'1'       MAKE IT FIRST OF THE MONTH                   
         MVC   NETCNV2,NETCNV                                                   
         PRINT GEN                                                              
         GOTO1 VADDAY,DMCB,NETCNV2,NETCNV,-45                                   
         PRINT NOGEN                                                            
         GOTO1 VNETWEEK,DMCB,NETCNV,VGETDAY,VADDAY                              
         MVC   KEY+3(1),DMCB+4     YEAR #                                       
         MVC   KEY+4(1),DMCB       WEEK #                                       
         MVC   STBOOK,KEY+3                                                     
*                                                                               
         ZIC   R1,BOOKREAL+1                                                    
         BCTR  R1,0                                                             
         LA    RE,MONTHEND                                                      
         AR    RE,R1                                                            
         ZIC   RF,0(RE)                                                         
         EDIT  (RF),(2,NETCNV+4)                                                
         GOTO1 VNETWEEK,DMCB,NETCNV,VGETDAY,VADDAY                              
         MVC   ENDWEEK,DMCB                                                     
*                                                                               
KEYVAL22 L     RE,ATABLE2                                                       
         XCEF  (RE),9999             CLEAR THE TABLE                            
*                                                                               
         L     R5,ATABLE2                                                       
         USING TABLED,R5                                                        
         SR    R6,R6               COUNTER                                      
KEYVAL23 MVC   KEY+4(1),STWEEK     MOVE WEEK # INTO KEY                         
         LA    R7,NETWORKS                                                      
KEYVAL25 CLI   0(R7),X'FF'         FINISHED THE NETWORK                         
         BE    KEYVAL50                                                         
         MVC   KEY+5(5),0(R7)      GET NETWORK                                  
*                                                                               
KEYVAL30 DS    0H                                                               
         BAS   RE,SYSHIGH                                                       
         CLC   =C'QNN',KEY         MAKE SURE THE KEY IS GOOD                    
         BNE   KEYVAL40                                                         
         CLC   STBOOK,KEY+3                                                     
         BNE   KEYVAL40                                                         
         CLC   KEY+5(5),0(R7)      CHECK NETWORK TOO                            
         BNE   KEYVAL40                                                         
         MVC   WKAREA(24),KEY                                                   
         BAS   RE,SYSGETRC                                                      
         OC    WKAREA+18(2),WKAREA+18                                           
         BZ    KEYVAL35                                                         
         BAS   RE,GETNTI21                                                      
         CLI   FIRSTTME,C'Y'                                                    
         BE    KEYVAL34                                                         
         BAS   RE,PRGSRCH                                                       
         BNE   KEYVAL35                                                         
KEYVAL34 LA    R6,1(R6)                                                         
         LA    R5,TABLELEN(R5)                                                  
* SEQUENTIAL PART                                                               
KEYVAL35 SR    R1,R1                                                            
         ICM   R1,3,WKAREA+18                                                   
         LA    R1,1(R1)                                                         
         STCM  R1,3,WKAREA+18                                                   
         BAS   RE,SYSGETRC                                                      
         BNZ   KEYVAL40                                                         
         CLC   =C'QNN',KEY         MAKE SURE THE KEY IS GOOD                    
         BNE   KEYVAL40                                                         
         CLC   KEY+5(5),0(R7)      CHECK NETWORK TOO                            
         BNE   KEYVAL40                                                         
         OC    WKAREA+18(2),WKAREA+18                                           
         BZ    KEYVAL35                                                         
         BAS   RE,GETNTI21                                                      
         CLI   FIRSTTME,C'Y'                                                    
         BE    KEYVAL36                                                         
         BAS   RE,PRGSRCH                                                       
         BNE   KEYVAL35                                                         
KEYVAL36 LA    R6,1(R6)                                                         
*                                                                               
         LA    R5,TABLELEN(R5)                                                  
         B     KEYVAL35                                                         
*                                                                               
KEYVAL40 TM    DMCB+8,X'80'        POSSIBLE EOF ERROR FROM DATA MANAGER         
         B     KEYVAL49            OTHER ERRORS GET NEXT NETWORK                
*        ZIC   R1,KEY+17  -------- DUPLICATES AFTER THIS                        
*        LA    R1,1(R1)                                                         
*        STC   R1,KEY+17                                                        
*        BAS   RE,SYSHIGH                                                       
*        B     KEYVAL30                                                         
*                                                                               
KEYVAL49 LA    R7,5(R7)                                                         
         MVC   KEY,KEYSAVE                                                      
         B     KEYVAL25                                                         
*                                                                               
KEYVAL50 DS    0H                                                               
         CLC   STWEEK,ENDWEEK      ARE WE DONE?                                 
         BNL   KEYVAL55                                                         
         MVI   FIRSTTME,C'N'                                                    
         L     R7,ATABLE2                                                       
         LTR   R6,R6                                                            
         BZ    KEYVAL52                                                         
         GOTO1 VXSORT,DMCB,(R7),(R6),27,25,0                                    
KEYVAL52 ZIC   RF,STWEEK                                                        
         LA    RF,1(RF)                                                         
         STC   RF,STWEEK                                                        
         B     KEYVAL23                                                         
*                                                                               
KEYVAL55 MVI   0(R5),X'FF'         END OF TABLE                                 
         L     R7,ATABLE2                                                       
         LTR   R6,R6                                                            
         BZ    EIIO                                                             
         GOTO1 VXSORT,DMCB,(R7),(R6),27,25,0                                    
         MVI   SYSNUM,10           SWITCH BACK TO CONTROL                       
         BAS   RE,SWTCHSYS                                                      
*                                                                               
KEYVAL60 CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         CLI   ACTN,DISPLAY                                                     
         BE    DISPREC                                                          
         MVI   FERN,13                                                          
         B     EXIT                                                             
         EJECT                                                                  
PRGSRCH  NTR1                                                                   
         LTR   R6,R6               NONE FIRST TIME                              
         BZ    EQXIT               WE COULD USE IT                              
         LR    R7,R5               R7 POINTS TO CURRENT PROGRAM                 
         L     R5,ATABLE2                                                       
PRGSRCH5 CLC   TBLPRGNM,0(R7)                                                   
         BE    NEQXIT              FOUND IT, DON'T USE IT                       
         LA    R5,TABLELEN(R5)                                                  
         BCT   R6,PRGSRCH5                                                      
         B     EQXIT                                                            
         EJECT                                                                  
GETNTI21 NTR1                                                                   
         CLI   SYSNUM,2                                                         
         BNE   GNTI1                                                            
         MVC   TBLPRGCD,KEY+3                                                   
         B     GNTI2                                                            
GNTI1    MVC   TBLPRGCD,WKAREA+18      SAVE THE CODE                            
GNTI2    LA    R6,WKAREA                                                        
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL            GET PROGRAM NAME ELEMENT                     
*                                                                               
         MVC   TBLPRGNM,=CL25' '                                                
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'            MINUS FIRST 2 BYTES, 1 FOR EX                
         EX    R1,*+8                                                           
         B     EQXIT                                                            
         MVC   TBLPRGNM(0),2(R6)                                                
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         DROP  R5                                                               
         EJECT                                                                  
*              VALIDATE BOOK                                                    
*                                                                               
BOOKVAL  NTR1                                                                   
         GOTO1 VDATVAL,DMCB,(X'02',DEMBOOK),BOOKDATE                            
         OC    DMCB,DMCB                                                        
         BZ    NEQXIT                                                           
********REFORMAT THE DATE                                                       
*     OLD CODE DISABLED                                                         
*        PACK  DUB(8),BOOKDATE(2)         YEAR                                  
*        CVB   R2,DUB                                                           
*        STC   R2,KEY+5                                                         
*        PACK  DUB(8),BOOKDATE+2(2)       MONTH                                 
*        CVB   R2,DUB                                                           
*        STC   R2,KEY+6                                                         
***   NEW CODE                                                                  
         GOTO1 VDATCON,DMCB,(0,BOOKDATE),(3,DUB)                                
         MVC   KEY+5(2),DUB                                                     
         MVC   BOOKREAL,KEY+5                                                   
         B     EQXIT                                                            
*                                                                               
BOOKDATE DC    XL6'00'                                                          
BOOKREAL DC    XL2'00'                                                          
         EJECT                                                                  
*-------------------------------------------------------------------            
* SWITCH SYSTEM ROUTINE                                                         
*    NEEDS SYSNUM, SYSTEM NUMBER   SPOT=02, NET=03, CONTROL=0A, ...             
*-------------------------------------------------------------------            
SWTCHSYS NTR1                                                                   
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SYSNUM      MOVE SYSTEM #                                
         GOTO1 VSWITCH,DMCB                                                     
*                                                                               
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EQXIT                                                            
         EJECT                                                                  
*---------------------------------------------------------------                
* DATA MANAGER CALLS TO USE IN SPOT SYSTEM                                      
*---------------------------------------------------------------                
SYSHIGH  MVC   COMMAND,=C'DMRDHI  '                                             
         B     SYSDIR                                                           
SYSSEQDR MVC   COMMAND,=C'DMRSEQ  '                                             
SYSDIR   NTR1                                                                   
         CLI   SYSNUM,2                                                         
         BNE   SYSDIR3                                                          
         LA    R5,=C'DEMDIR  '                                                  
         B     SYSDIR10                                                         
SYSDIR3  LA    R5,=C'NTIDIR  '                                                  
SYSDIR10 DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMR,DMCB,COMMAND,(R5),KEYSAVE,KEY,0                          
         MVC   COMMAND,=C'DMREAD  '                                             
         GOTO1 VDATAMR,DMCB,COMMAND,(R5),KEYSAVE,KEY,0                          
         B     EQXIT                                                            
*                                                                               
SYSGETRC MVC   COMMAND,=C'DMRDHI  '                                             
         B     SYSFILE                                                          
SYSSEQFL MVC   COMMAND,=C'DMRSEQ  '                                             
         DC    H'0'                                                             
SYSFILE  NTR1                                                                   
         CLI   SYSNUM,2                                                         
         BNE   SYSFIL3                                                          
         LA    R5,=C'DEMFIL  '                                                  
         B     SYSFIL10                                                         
SYSFIL3  LA    R5,=C'NTIFIL  '                                                  
SYSFIL10 LA    R2,KEY+19           DISK ADDRESS                                 
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMR,DMCB,COMMAND,(R5),(R2),WKAREA,0                          
         OC    DMCB+8(1),DMCB+8                                                 
         BNZ   NEQXIT                                                           
         B     EQXIT                                                            
         SPACE 3                                                                
         EJECT                                                                  
*              DISPLAY RECORD                                                   
*                                                                               
DISPREC  DS    0H                                                               
         LA    R7,TABLE1           NTI PROGRAM TABLE                            
         USING TABLED,R7                                                        
         L     R5,ATABLE2          MPA PROGRAM TABLE                            
         USING TABLED2,R5                                                       
*                                                                               
         CLI   DEMSRCHH+5,0        IS THERE A SEARCH STRING?                    
         BE    DISPRC10            NO, START FROM A                             
DISPRC01 CLI   TBLPRGNM,X'FF'      END OF TABLE?                                
         BNE   DISPRC03                                                         
         LA    R7,TABLE1           YES, START FROM BEGINNING                    
         B     NEXTTABL                                                         
DISPRC03 CLC   TBLPRGNM,DEMSRCH    NAME MATCHES SEARCH STRING?                  
         BNL   NEXTTABL                                                         
         LA    R7,TABLELEN(R7)                                                  
         B     DISPRC01                                                         
NEXTTABL CLI   TB2PRGNM,X'FF'      SAME FOR MPA TABLE                           
         BNE   DISPRC05                                                         
         L     R5,ATABLE2                                                       
         B     DISPRC10                                                         
DISPRC05 CLC   TB2PRGNM,DEMSRCH                                                 
         BNL   DISPRC10                                                         
         LA    R5,TABL2LEN(R5)                                                  
         B     NEXTTABL                                                         
*                                                                               
DISPRC10 BAS   RE,DISPROUT         SHOW THE TABLE CONTENTS                      
*                                                                               
         OC    DEML1PN,DEML1PN     MAKE LAST PROGRAM SEARCH STRING              
         BZ    DISPRC13                                                         
         MVC   DEMSRCH,DEML1PN                                                  
         B     DISPRC15                                                         
DISPRC13 MVC   DEMSRCH,DEML2PN                                                  
DISPRC15 OI    DEMSRCHH+6,X'81'                                                 
         MVI   NACTN,OKDEL+OKCHA                                                
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         B     EXIT                                                             
         SPACE 3                                                                
DISPROUT NTR1                                                                   
         MVC   KEYSAVE,CTDPKEY                                                  
         LA    R6,16                                                            
         LA    R1,DEMMPPNH                                                      
         LA    RF,DEMMPPCH                                                      
DISPRT10 CLI   DEMOPTS,C'M'                                                     
         BE    DISPRT13                                                         
         CLC   TBLPRGNM,TB2PRGNM                                                
         BL    DISPRT20                                                         
DISPRT13 MVC   CTDPKEY(25),KEYSAVE                                              
         SR    RE,RE                                                            
         ICM   RE,3,TB2PRGCD                                                    
         STCM  RE,7,CTDPKPCD                                                    
         MVC   KEY,CTDPKEY                                                      
         LR    R8,R1                                                            
         LR    R0,RF                                                            
         GOTO1 AREAD                                                            
         LR    RF,R0                                                            
         LR    R1,R8                                                            
         CLC   KEY,CTDPKEY                                                      
         BNE   DISPRT20                                                         
         CLI   DEMOPTS,C'N'                                                     
         BNE   DISPRT15                                                         
         LA    R5,TABL2LEN(R5)                                                  
         LA    R7,TABLELEN(R7)                                                  
         B     DISPRT10                                                         
*                                                                               
DISPRT15 BAS   RE,SRCHMPA                                                       
         BNE   DISPRT20                                                         
         B     DISPRT60                                                         
*                                                                               
DISPRT20 XC    8(25,R1),8(R1)                                                   
         XC    8(5,RF),8(RF)                                                    
         XC    NEXTONE+8(25,R1),NEXTONE+8(R1)                                   
         XC    NEXTONE+8(5,RF),NEXTONE+8(RF)                                    
         MVI   54(R1),C' '                                                      
         CLI   TB2PRGNM,X'FF'                                                   
         BE    DISPRT70                                                         
         CLC   TBLPRGNM,TB2PRGNM                                                
         BE    DISPRT50                                                         
         BH    DISPRT30                                                         
DISPRT25 CLI   DEMOPTS,C'M'                                                     
         BE    DISPRT29                                                         
         MVC   8(25,R1),TBLPRGNM                                                
         SR    R8,R8                                                            
         ICM   R8,3,TBLPRGCD                                                    
         CVD   R8,DUB                                                           
         UNPK  8(5,RF),DUB                                                      
         OI    12(RF),X'F0'                                                     
         LA    R7,TABLELEN(R7)                                                  
         B     DISPRT40                                                         
*                                                                               
DISPRT29 LA    R7,TABLELEN(R7)                                                  
         B     DISPRT10                                                         
*                                                                               
DISPRT30 CLI   DEMOPTS,C'M'                                                     
         BE    DISPRT39                                                         
         MVC   NEXTONE+8(25,R1),TB2PRGNM                                        
         SR    R8,R8                                                            
         ICM   R8,3,TB2PRGCD                                                    
         CVD   R8,DUB                                                           
         UNPK  NEXTONE+8(5,RF),DUB                                              
         OI    NEXTONE+12(RF),X'F0'                                             
         LA    R5,TABL2LEN(R5)                                                  
         B     DISPRT40                                                         
*                                                                               
DISPRT39 LA    R5,TABL2LEN(R5)                                                  
         B     DISPRT10                                                         
*                                                                               
DISPRT40 OI    6(R1),X'80'         TRANSMIT                                     
         OI    6(RF),X'80'         TRANSMIT                                     
         OI    NEXTONE+6(R1),X'80' TRANSMIT                                     
         OI    NEXTONE+6(RF),X'80' TRANSMIT                                     
         OI    52(R1),X'80'                                                     
         LA    R1,NEXTLINE(R1)                                                  
         LA    RF,NEXTLINE(RF)                                                  
         BCT   R6,DISPRT10                                                      
         B     XIT                                                              
*                                                                               
DISPRT45 LA    R5,TABL2LEN(R5)                                                  
         LA    R7,TABLELEN(R7)                                                  
         B     DISPRT10                                                         
*                                                                               
DISPRT50 CLI   DEMOPTS,C'N'                                                     
         BE    DISPRT45                                                         
*                                                                               
         MVC   8(25,R1),TBLPRGNM                                                
         MVC   NEXTONE+8(25,R1),TB2PRGNM                                        
*                                                                               
         SR    R8,R8                                                            
         ICM   R8,3,TBLPRGCD                                                    
         CVD   R8,DUB                                                           
         UNPK  8(5,RF),DUB                                                      
         OI    12(RF),X'F0'                                                     
*                                                                               
         SR    R8,R8                                                            
         ICM   R8,3,TB2PRGCD                                                    
         CVD   R8,DUB                                                           
         UNPK  NEXTONE+8(5,RF),DUB                                              
         OI    NEXTONE+12(RF),X'F0'                                             
*                                                                               
DISPRT60 OI    6(R1),X'80'         TRANSMIT                                     
         OI    6(RF),X'80'         TRANSMIT                                     
         OI    NEXTONE+6(R1),X'80' TRANSMIT                                     
         OI    NEXTONE+6(RF),X'80' TRANSMIT                                     
         OI    52(R1),X'80'                                                     
*                                                                               
         LA    R1,NEXTLINE(R1)                                                  
         LA    RF,NEXTLINE(RF)                                                  
         CLC   TBLPRGNM,TB2PRGNM                                                
         BNE   *+8                                                              
         LA    R7,TABLELEN(R7)                                                  
         LA    R5,TABL2LEN(R5)                                                  
         BCT   R6,DISPRT10                                                      
         B     XIT                                                              
*                                                                               
DISPRT70 DS    0H                                                               
         XC    8(25,R1),8(R1)                                                   
         XC    NEXTONE+8(25,R1),NEXTONE+8(R1)                                   
         XC    8(5,RF),8(RF)                                                    
         XC    NEXTONE+8(5,RF),NEXTONE+8(RF)                                    
         MVI   54(R1),C' '                                                      
         OI    6(R1),X'80'         TRANSMIT                                     
         OI    6(RF),X'80'         TRANSMIT                                     
         OI    NEXTONE+6(R1),X'80'                                              
         OI    NEXTONE+6(RF),X'80'                                              
         OI    52(R1),X'80'                                                     
*                                                                               
         LA    R1,NEXTLINE(R1)                                                  
         LA    RF,NEXTLINE(RF)                                                  
         BCT   R6,DISPRT70                                                      
         B     XIT                                                              
         SPACE 3                                                                
SRCHMPA  NTR1                                                                   
         SR    RE,RE                                                            
         ICM   RE,7,CTDPKEY+37     GET MPA PROGRAM #                            
         LA    R7,TABLE1                                                        
SRCMPA10 CLI   TBLPRGNM,X'FF'                                                   
         BE    NEQXIT                                                           
         SR    R6,R6                                                            
         ICM   R6,3,TBLPRGCD                                                    
         CR    RE,R6                                                            
         BE    SRCMPA40                                                         
         LA    R7,TABLELEN(R7)                                                  
         B     SRCMPA10                                                         
*                                                                               
SRCMPA40 XC    8(25,R1),8(R1)                                                   
         XC    NEXTONE+8(25,R1),NEXTONE+8(R1)                                   
         XC    8(5,RF),8(RF)                                                    
         XC    NEXTONE+8(5,RF),NEXTONE+8(RF)                                    
         MVI   54(R1),C' '        CLEAR =                                       
*                                                                               
         MVC   8(25,R1),TBLPRGNM                                                
         MVC   NEXTONE+8(25,R1),TB2PRGNM                                        
*                                                                               
         SR    R8,R8                                                            
         ICM   R8,3,TBLPRGCD                                                    
         CVD   R8,DUB                                                           
         UNPK  8(5,RF),DUB                                                      
         OI    12(RF),X'F0'                                                     
*                                                                               
         SR    R8,R8                                                            
         ICM   R8,3,TB2PRGCD                                                    
         CVD   R8,DUB                                                           
         UNPK  NEXTONE+8(5,RF),DUB                                              
         OI    NEXTONE+12(RF),X'F0'                                             
*                                                                               
         MVI   54(R1),C'='                                                      
         OI    52(R1),X'80'                                                     
         B     EQXIT                                                            
         EJECT                                                                  
DATAVAL  DS    0H                                                               
         LA    R7,TABLE1                                                        
         USING TABLED,R7                                                        
         L     R5,ATABLE2                                                       
         USING TABLED2,R5                                                       
         XC    SAVEADR1,SAVEADR1                                                
         XC    SAVEADR2,SAVEADR2                                                
*                                                                               
         LA    R3,DEMMPPNH                                                      
         LA    R1,DEMNTPNH                                                      
         LA    RF,DEMMPPCH                                                      
         BAS   RE,DATSRCH                                                       
         ST    R7,SAVEADR1                                                      
         ST    R5,SAVEADR2                                                      
*                                                                               
DATVAL00 TM    4(RF),X'80'         INPUT THIS TIME?                             
         BNO   DATBUMP                                                          
         CLI   7(R1),0             SEE IF THERE WAS ANY PROGS LISTED            
         BZ    DATBUMP                                                          
         MVI   ACTN,ADD            SHOULD ADD THIS, CHANGE IF EXIST             
* CONVERT CODE TO BINARY                                                        
DATVAL07 BAS   RE,CODECNVT                                                      
         BE    DATVAL09                                                         
         ST    RF,FADR             BAD CODE, SAY SO                             
         B     EIIF                                                             
DATVAL09 BAS   RE,DATSRCH                                                       
DATVAL10 BAS   RE,ADDEM                                                         
         BZ    DATBUMP             NO ERROR, NEXT LINE                          
         MVI   FERN,0              ERROR, REPORT IT                             
         B     EXIT                                                             
DATBUMP  LA    R1,NEXTLINE(R1)     BUMP TO NEXT LINE IN TWA                     
         LA    R6,DEMLAST                                                       
         CR    R1,R6               IS THAT ALL?                                 
         BNL   DATVALXT            THAT'S ALL FOLKS!                            
         LA    RF,NEXTLINE(RF)                                                  
         LA    R3,NEXTLINE(R3)                                                  
         B     DATVAL00                                                         
*                                                                               
DATVALXT OC    SAVEADR1,SAVEADR1                                                
         BZ    DATXIT2                                                          
         L     R7,SAVEADR1                                                      
         L     R5,SAVEADR2                                                      
         BAS   RE,DISPROUT                                                      
DATXIT2  MVI   NACTN,OKDEL+OKCHA                                                
         MVI   ACTN,CHANGE                                                      
         MVI   FERN,X'FF'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* SEARCH FOR THE MATCHING ENTRIES IN THE TABLES                                 
*                                                                               
DATSRCH  NTR1                                                                   
         CLI   7(R1),0             IS NTI PROGRAM BLANK?                        
         BNZ   DATSRC01            NO, USE NTI PROGRAM NAME                     
         LR    R1,R3               YES, USE MPA'S PROGRAM NAME                  
DATSRC01 CLI   TBLPRGNM,X'FF'                                                   
         BNE   DATSRC03                                                         
         LA    R7,TABLE1                                                        
         B     DATSRC04                                                         
DATSRC03 ZIC   R6,7(R1)                                                         
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   TBLPRGNM(0),8(R1)                                                
         BNL   DATSRC04                                                         
         LA    R7,TABLELEN(R7)                                                  
         B     DATSRC01                                                         
*                                                                               
DATSRC04 CLI   TB2PRGNM,X'FF'                                                   
         BNE   DATSRC05                                                         
         L     R5,ATABLE2                                                       
         B     DATSRCXT                                                         
DATSRC05 ZIC   R6,7(R1)                                                         
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   TB2PRGNM(0),8(R1)                                                
         BNL   DATSRCXT                                                         
         LA    R5,TABL2LEN(R5)                                                  
         B     DATSRC04                                                         
DATSRCXT XIT1  REGS=(R5,R7)                                                     
*                                                                               
CODECNVT NTR1                                                                   
         TM    4(RF),X'08'         HAS TO BE A VALID NUMERIC                    
         BNO   NEQXIT              NO, DON'T CHANGE A THING                     
         ZIC   R1,5(RF)            LENGTH OF EBCIDIC CODE                       
         LTR   R1,R1               IF LENGTH IS ZERO                            
         BZ    NEQXIT              DON'T CHANGE A THING                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,RF)                                                      
         CVB   R6,DUB                                                           
         ST    R6,SAVECODE                                                      
         B     EQXIT                                                            
         SPACE 3                                                                
ADDEM    NTR1                                                                   
         SR    RE,RE                                                            
         ICM   RE,3,TB2PRGCD                                                    
         STCM  RE,7,CTDPKPCD                                                    
*                                                                               
         MVC   KEY,CTDPKEY                                                      
*                                                                               
         GOTO1 AREAD                                                            
         BZ    NEQXIT                                                           
*                                                                               
         CLI   ACTN,ADD                                                         
         BNE   ADDEM05                                                          
         CLC   KEY,CTDPKEY         MAKE SURE WE GOT THE RIGHT ONE               
         BNE   ADDEM10                                                          
         NI    CTDPSTAT,X'7F'      NOT DELETED                                  
         MVI   ACTN,CHANGE         WE HAVE TO CHANGE                            
         B     ADDEM10                                                          
*                                                                               
ADDEM05  CLC   KEY,CTDPKEY         MAKE SURE WE GOT THE RIGHT ONE               
         BNE   NEQXIT                                                           
*                                                                               
ADDEM10  MVI   TEMP,0                                                           
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    XIT                                                              
*                                                                               
         XC    TEMP,TEMP                                                        
         MVI   TEMP,2                                                           
         MVI   TEMP+1,9                                                         
         MVI   TEMP+2,C'T'                                                      
         MVI   TEMP+3,C'N'                                                      
         L     R6,SAVECODE                                                      
         STCM  R6,7,TEMP+4         R6 WAS USED IN CODECNVT                      
         MVC   TEMP+7(2),=X'FFFF'                                               
         GOTO1 APUTEL                                                           
         BZ    XIT                                                              
*                                                                               
         OC    SAVECODE,SAVECODE                                                
         BNZ   ADDEM20                                                          
         OI    CTDPSTAT,X'80'      MARK FOR DELETION                            
*                                                                               
ADDEM20  L     RF,AADD                                                          
         CLI   ACTN,ADD                                                         
         BE    *+8                                                              
         L     RF,AWRITE                                                        
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNZ   NEQXIT                                                           
         B     EQXIT                                                            
         DROP  R7                                                               
         DROP  R5                                                               
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
NEQXIT   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
EQXIT    CR    R1,R1                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
SYSNUM   DC    X'0A'               SYSTEM NUMBER                                
PRGNUM   DC    XL2'0000'           PROGRAM NUMBER                               
COMMAND  DC    CL8' '                                                           
DATADISP DC    H'23'                                                            
ELCODE   DC    X'00'                                                            
SAVEADR1 DC    F'0'                                                             
SAVEADR2 DC    F'0'                                                             
SAVECODE DC    F'0'                                                             
OPT      DC    C'A'                ALL DEFAULT                                  
NETWORKS DC    C'ABC T'                                                         
         DC    C'CBS T'                                                         
         DC    C'FOX T'                                                         
         DC    C'NBC T',X'FF'                                                   
         SPACE 3                                                                
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
WKAREA   DC    1000X'00'                                                        
*                                  TABLE OF VALID FILES                         
FILETAB  DS    0CL16                                                            
         DC    C'TPT     ',C'T',C'TCPRW',2X'00'                                 
         DC    C'PAV     ',C'P',C'TN',5X'00'                                    
         DC    C'ESTIMATE',C'E',C'V',6X'00'                                     
         DC    C'INV     ',C'I',C'U',6X'00'                                     
         DC    C'DPT     ',C'T',C'D',6X'00'                                     
         DC    X'00'                                                            
*                                  TABLE OF VALID SUB-FILES                     
MEDTAB   DS    0CL9                                                             
         DC    C'USTV    ',C'T'                                                 
         DC    C'CANTV   ',C'C'                                                 
         DC    C'RADIO   ',C'R'                                                 
         DC    C'WEEKLY  ',C'W'                                                 
         DC    C'DPT     ',C'D'                                                 
         DC    C'NETWORK ',C'N'                                                 
         DC    C'MPA     ',C'P'                                                 
         DC    C'VPH     ',C'V'                                                 
         DC    C'UPGRADE ',C'U'                                                 
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
WORKD    DSECT                                                                  
MEDVALS  DS    CL7                                                              
NLINES   DS    X                                                                
SCANTBL  DS    20CL32                                                           
VXSORT   DS    V                                                                
VSWITCH  DS    V                                                                
VCLLOV   DS    V                                                                
VGETDAY  DS    V                                                                
VADDAY   DS    V                                                                
VNETWEEK DS    V                                                                
FIRSTTME DS    C                                                                
ATABLE2  DS    A                                                                
TABLE1   DS    9999C                                                            
TABLE2   DS    9999C                                                            
         SPACE 3                                                                
TABLED   DSECT                                                                  
TBLPRGNM DS    CL25                PROGRAM NAME                                 
TBLPRGCD DS    XL2                 PROGRAM CODE                                 
TABLELEN EQU   *-TABLED                                                         
         SPACE 3                                                                
TABLED2  DSECT                                                                  
TB2PRGNM DS    CL25                PROGRAM NAME                                 
TB2PRGCD DS    XL2                 PROGRAM CODE                                 
TABL2LEN EQU   *-TABLED2                                                        
         SPACE 3                                                                
* DEDBLOCK                                                                      
       ++INCLUDE DEDBLOCK                                                       
         SPACE 1                                                                
* CTLFMACTNS                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTLFMACTNS                                                     
         PRINT ON                                                               
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMDED                                                                      
       ++INCLUDE CTLFMDED                                                       
NEXTONE  EQU   DEMNTPNH-DEMMPPNH                                                
NEXTLINE EQU   DEMNXLNH-DEMMPPNH                                                
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'140CTLFM21A  05/01/02'                                      
         END                                                                    
