*          DATA SET NESYSDRIVT AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NESYSDRIVE AT LEVEL 151 AS OF 10/01/87                      
*PHASE T00A41A,+0                                                               
*INCLUDE NETACC                                                                 
         TITLE 'T00A41 - DRIVER FOR NETWORK SPOOL'                              
NETDRIVE CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 NDWORKX-NDWORK,**NEDV**,RR=R2                                    
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING NETDRIVE+4096,R7                                                 
         LR    R8,RC                                                            
         USING NDWORK,R8                                                        
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         MVI   NDSPACES,C' '                                                    
         MVC   NDSPACES+1(L'NDSPACES-1),NDSPACES                                
         SPACE 1                                                                
*                                  TEST HOOK VALUE TO SEE WHERE WE ARE          
         CLI   GLHOOK,GLRESOLV                                                  
         BNE   ND2                                                              
         BAS   RE,SYSINIT                                                       
         B     XIT                                                              
         SPACE 1                                                                
ND2      CLI   GLHOOK,GLROUT                                                    
         BNE   XIT                                                              
         CLI   GLMODE,GLINPUT                                                   
         BNE   ND6                                                              
         BAS   RE,SYSINP                                                        
         B     XIT                                                              
ND6      CLI   GLMODE,GLOUTPUT                                                  
         BNE   ND10                                                             
         BAS   RE,SYSOUTP                                                       
         B     XIT                                                              
ND10     DC    H'0'                                                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         EJECT                                                                  
*              SYSTEM INITIALIZATION                                            
         SPACE 3                                                                
SYSINIT  NTR1                                                                   
         LA    R1,ROUTLIST                                                      
         LA    R2,GLLABEL                                                       
         SPACE 1                                                                
SYSINIT2 CLC   0(8,R1),0(R2)                                                    
         BE    SYSINIT4                                                         
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         B     SYSINIT2                                                         
         SPACE 1                                                                
SYSINIT4 MVC   GLAROUT,8(R1)                                                    
         B     XIT                                                              
         SPACE 1                                                                
ROUTLIST DS    0F                                                               
*                                  INPUT ROUTINES                               
         SPACE 1                                                                
         DC    C'NIACCGEN',A(NIACCGEN)                                          
         DC    C'NIACTTIM',A(NIACTTIM)                                          
         DC    C'NIACTWHY',A(NIACTWHY)                                          
         DC    C'NIAGY   ',A(NIAGY)                                             
         DC    C'NIAFFTIM',A(NIAFFTIM)                                          
         DC    C'NIASSIGN',A(NIASSIGN)                                          
         DC    C'NIBILDT ',A(NIBILDT)                                           
         DC    C'NIBILPRD',A(NIBILPRD)                                          
         DC    C'NICALTIM',A(NICALTIM)                                          
         DC    C'NICLICOD',A(NICLICOD)                                          
         DC    C'NICOMMLA',A(NICOMMLA)                                          
         DC    C'NICOMMLS',A(NICOMMLS)                                          
         DC    C'NICOST  ',A(NICOST)                                            
         DC    C'NIDATE  ',A(NIDATE)                                            
         DC    C'NIDATNUM',A(NIDATNUM)                                          
         DC    C'NIDAY   ',A(NIDAY)                                             
         DC    C'NIDAYP  ',A(NIDAYP)                                            
         DC    C'NIEST   ',A(NIEST)                                             
         DC    C'NIFEED  ',A(NIFEED)                                            
         DC    C'NIFEEDMG',A(NIFEEDMG)                                          
         DC    C'NIGRP   ',A(NIGRP)                                             
         DC    C'NIHUTAVE',A(NIHUTAVE)                                          
         DC    C'NIHUTPCT',A(NIHUTPCT)                                          
         DC    C'NIIMP   ',A(NIIMP)                                             
         DC    C'NIIMPACT',A(NIIMPACT)                                          
         DC    C'NILEN   ',A(NILEN)                                             
         DC    C'NIMARKET',A(NIMARKET)                                          
         DC    C'NIMGBDAT',A(NIMGBDAT)                                          
         DC    C'NIMGBPCD',A(NIMGBPCD)                                          
         DC    C'NIMGBPNM',A(NIMGBPNM)                                          
         DC    C'NIMGFPCD',A(NIMGFPCD)                                          
         DC    C'NIMGFPNM',A(NIMGFPNM)                                          
         DC    C'NIMGFDAT',A(NIMGFDAT)                                          
         DC    C'NINET   ',A(NINET)                                             
         DC    C'NINTI   ',A(NINTI)                                             
         DC    C'NINSI   ',A(NINSI)                                             
         DC    C'NIPACK  ',A(NIPACK)                                            
         DC    C'NIPACKST',A(NIPACKST)                                          
         DC    C'NIPAYDT ',A(NIPAYDT)                                           
         DC    C'NIPCODE ',A(NIPROGCD)                                          
         DC    C'NIPER1  ',A(NIPER1)                                            
         DC    C'NIPRDCD ',A(NIPRDCD)                                           
         DC    C'NIPRDCOD',A(NIPRDCOD)                                          
         DC    C'NIPRFILT',A(NIPRFILT)                                          
         DC    C'NIPROGNM',A(NIPROGNM)                                          
         DC    C'NIRESULT',A(NIRESULT)                                          
         DC    C'NISREP  ',A(NISREP)                                            
         DC    C'NISCAP1 ',A(NISCAP1)                                           
         DC    C'NISCAP2 ',A(NISCAP2)                                           
         DC    C'NISUNIQ ',A(NISUNIQ)                                           
         DC    C'NIUNCODE',A(NIUNCODE)                                          
         DC    C'NIUNIT  ',A(NIUNIT)                                            
         DC    C'NIUNITST',A(NIUNITST)                                          
         DC    C'NIUNIV  ',A(NIUNIV)                                            
         DC    C'NIPRDGRP',A(NIPRDGRP)                                          
         DC    C'NIMGCMNT',A(NIMGCMNT)                                          
         SPACE 1                                                                
*                                  HEADER ROUTINES                              
         SPACE 1                                                                
         DC    C'DATEOUT ',A(NHDATOUT)                                          
         DC    C'CLIPAGE ',A(CLIPAGE)                                           
         DC    C'DPTPAGE ',A(DPTPAGE)                                           
         DC    C'ESTPAGE ',A(ESTPAGE)                                           
         DC    C'NETPAGE ',A(NETPAGE)                                           
         DC    C'PACKPAGE',A(PACKPAGE)                                          
         DC    C'PRODPAGE',A(PRODPAGE)                                          
         DC    C'PGRPAGE ',A(PGRPAGE)                                           
         DC    C'DEMOUT  ',A(NHDEM)                                             
         DC    C'NHGRP   ',A(NHGRP)                                             
         DC    C'NHIMP   ',A(NHIMP)                                             
         DC    C'NIGRPOUT',A(GRPOUT)                                            
         SPACE 1                                                                
*                                  OUTPUT ROUTINES                              
         SPACE 1                                                                
         DC    C'NODAYP  ',A(NODAYP)                                            
         DC    C'NODATNUM',A(NODATNUM)                                          
         DC    C'NOCLINAM',A(NOCLINAM)                                          
         DC    C'NOESTNAM',A(NOESTNAM)                                          
         DC    C'NOPAKNAM',A(NOPAKNAM)                                          
         DC    C'NOPRDBTH',A(NOPRDBTH)                                          
         DC    C'NOPRDNAM',A(NOPRDNAM)                                          
         DC    C'NOPRDCDE',A(NOPRDCDE)                                          
         DC    C'NOSDATE ',A(NOSDATE)                                           
         DC    C'NOMGCMNT',A(NOMGCMNT)                                          
         SPACE 1                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              CONTROL OF INPUT/OUTPUT                                          
         SPACE 3                                                                
SYSINP   NTR1                      INPUT ROUTINES                               
         L     RF,GLAROUT                                                       
         L     R3,GLAIFLD                                                       
         BR    RF                                                               
SYSOUTP  NTR1                      OUTPUT ROUTINES                              
         L     RF,GLAROUT                                                       
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         BR    RF                                                               
         SPACE 1                                                                
         EJECT                                                                  
*              SIMPLE ROUTINES                                                  
         SPACE 3                                                                
*              INPUT               (R3)=AREA TO MOVE DATA TO                    
         SPACE 1                                                                
NIAGY    MVC   0(L'NBALPHA,R3),NBALPHA         AGENCY ALPHA                     
         B     XIT                                                              
         SPACE 1                                                                
NICLICOD MVC   0(L'NBCLICOD,R3),NBCLICOD       CLIENT ALPHA                     
         B     XIT                                                              
         SPACE 1                                                                
NIPRFILT MVC   0(L'NBPRFILT,R3),NBPRFILT       PROGRAM FILTER                   
         B     XIT                                                              
         SPACE 1                                                                
NIFEEDMG MVC   0(L'NBFEEDMG,R3),NBFEEDMG       FEED MARKET GROUP                
         B     XIT                                                              
         SPACE 1                                                                
NIHUTAVE MVC   0(L'NBHUTAVE,R3),NBHUTAVE       HUT AVERAGING USED               
         B     XIT                                                              
         SPACE 1                                                                
NIMGFPCD MVC   0(L'NBMGFPCD,R3),NBMGFPCD       M/G FOR PROG CODE                
         B     XIT                                                              
         SPACE 1                                                                
NIMGBPCD MVC   0(L'NBMGBPCD,R3),NBMGBPCD       M/G BY PROG CODE                 
         B     XIT                                                              
         SPACE 1                                                                
NIMGFPNM MVC   0(L'NBMGFPNM,R3),NBMGFPNM       M/G FOR PROG NAME                
         B     XIT                                                              
         SPACE 1                                                                
NIMGBPNM MVC   0(L'NBMGBPNM,R3),NBMGBPNM       M/G BY PROG NAME                 
         B     XIT                                                              
         SPACE 1                                                                
NIRESULT MVC   0(L'NBRESULT,R3),NBRESULT       RESULT CODE FOR DEMOS            
         B     XIT                                                              
         SPACE 1                                                                
NIDAYP   MVC   0(L'NBACTDP,R3),NBACTDP         DAYPART (1-CHAR)                 
         B     XIT                                                              
         SPACE 1                                                                
NIEST    MVC   0(L'NBACTEST,R3),NBACTEST       ESTIMATE NUM                     
         B     XIT                                                              
         SPACE 1                                                                
NIPRDN   MVC   0(L'NBPRD,R3),NBPRD             PRODUCT NUM                      
         B     XIT                                                              
         SPACE 1                                                                
NIPRDN2  MVC   0(L'NBPRD2,R3),NBPRD2           SECOND PROD NUMBER               
         B     XIT                                                              
         SPACE 1                                                                
NIPACK   MVC   0(L'NBPACK,R3),NBPACK           PACKAGE NUMBER                   
         B     XIT                                                              
         SPACE 1                                                                
NINTI    MVC   0(L'NBNTI,R3),NBNTI             NTI NUMBER                       
         B     XIT                                                              
         SPACE 1                                                                
NINSI    MVC   0(L'NBNSI,R3),NBNSI             NSI NUMBER                       
         B     XIT                                                              
         SPACE 1                                                                
NIUNCODE MVC   0(L'NBUNCODE,R3),NBUNCODE       UNIVERSE CODE                    
         B     XIT                                                              
         SPACE 1                                                                
NIMARKET MVC   0(L'NBMARKET,R3),NBMARKET       MARKET NUM                       
         B     XIT                                                              
         SPACE 1                                                                
NISREP   MVC   0(L'NBSREP,R3),NBSREP           SPECIAL REP                      
         B     XIT                                                              
         SPACE 1                                                                
NIHUTPCT MVC   0(L'NBHUTPCT,R3),NBHUTPCT       HUT %                            
         B     XIT                                                              
         SPACE 1                                                                
NIFEED   MVC   0(L'NBFEED,R3),NBFEED           FEED %                           
         B     XIT                                                              
         SPACE 1                                                                
NIUNIV   MVC   0(L'NBUNIV,R3),NBUNIV           UNIV %                           
         B     XIT                                                              
         SPACE 1                                                                
NIIMPACT MVC   0(L'NBIMPACT,R3),NBIMPACT       IMPACT %                         
         B     XIT                                                              
         SPACE 1                                                                
NIUNITST MVC   0(L'NBUNITST,R3),NBUNITST       UNIT STATUS                      
         B     XIT                                                              
         SPACE 1                                                                
NIPACKST MVC   0(L'NBPACKST,R3),NBPACKST       PACKAGE STATUS                   
         B     XIT                                                              
         SPACE 1                                                                
NIACTWHY MVC   0(L'NBACTWHY,R3),NBACTWHY       REASON FOR LAST ACTIVITY         
         B     XIT                                                              
         SPACE 1                                                                
NIDATE   MVC   0(L'NBACTDAT,R3),NBACTDAT       DATE                             
         B     XIT                                                              
         SPACE 1                                                                
NIDATNUM MVC   0(L'NBACTDAT,R3),NBACTDAT       DATE                             
         MVC   2(L'NBACTSUB,R3),NBACTSUB       LINE NUMBER                      
         B     XIT                                                              
         SPACE 1                                                                
NIMGFDAT MVC   0(L'NBMGFDAT,R3),NBMGFDAT       M/G FOR DATE                     
         B     XIT                                                              
         SPACE 1                                                                
NIMGBDAT MVC   0(L'NBMGBDAT,R3),NBMGBDAT       M/G BY DATE                      
         B     XIT                                                              
         SPACE 1                                                                
NIACTTIM MVC   0(L'NBTIME,R3),NBTIME           START-END TIME                   
         B     XIT                                                              
         SPACE 1                                                                
NIAFFTIM MVC   0(L'NBAFFTIM,R3),NBAFFTIM       AFFIDAVIT TIME                   
         B     XIT                                                              
         SPACE 1                                                                
NINET    MVC   0(L'NBACTNET,R3),NBACTNET       NETWORK                          
         B     XIT                                                              
         SPACE 1                                                                
NIPROGNM MVC   0(L'NBPROGNM,R3),NBPROGNM       PROGRAM NAME                     
         OC    0(16,R3),=16XL1'40'                X'00' TO SPACE                
         B     XIT                                                              
         SPACE 1                                                                
NIPROGCD MVC   0(L'NBACTPRG,R3),NBACTPRG       PROGRAM CODE                     
         B     XIT                                                              
         SPACE 1                                                                
NIACTUAL MVC   0(L'NBACTUAL,R3),NBACTUAL       ACTUAL COST                      
         B     XIT                                                              
         SPACE 1                                                                
NIASSIGN MVC   0(L'NBASSIGN,R3),NBASSIGN       ASSIGNED COST                    
         B     XIT                                                              
         SPACE 1                                                                
NIINTEG  MVC   0(L'NBINTEG,R3),NBINTEG         INTEGRATION COST                 
         B     XIT                                                              
         SPACE 1                                                                
NIDAYNAM MVC   0(L'NBDAYNAM,R3),NBDAYNAM       DAY  (3 CHAR)                    
         B     XIT                                                              
         EJECT                                                                  
*              INTERNAL ROUTINES                                                
         SPACE 3                                                                
NISCAP1  MVI   0(R3),1             RETURN 1 - USED TO ID RECAP POSITION         
         B     XIT                  IN FIRST RECORD                             
         SPACE 1                                                                
NISCAP2  MVI   0(R3),2             RETURN 2 - USED TO ID RECAP POSITION         
         B     XIT                  IN RECAP RECORD                             
         SPACE 1                                                                
NISUNIQ  ICM   R1,15,NRWUNIQ        RETURN A UNIQUE NUMBER SO EACH UNIT         
         LA    R1,1(R1)               PRINTS SEPERATELY                         
         STCM  R1,15,NRWUNIQ                                                    
         ST    R1,0(R3)                                                         
         B     XIT                                                              
         EJECT                                                                  
*              FAIRLY SIMPLE ROUTINES                                           
         SPACE 3                                                                
*                                  NO ARGUMENTS - MINOR EXPANSION               
         SPACE 1                                                                
NILEN    ZIC   R1,NBLEN            LENGTH                                       
         ST    R1,0(R3)            1-BYTE SAVED AS FULLWORD                     
         B     XIT                                                              
         SPACE 1                                                                
*                                  AFFID OR START-END TIME                      
NICALTIM XC    0(4,R3),0(R3)       INITIALIZE 4 BYTES TO 0                      
         OC    NBAFFTIM,NBAFFTIM   IF AFFID TIME                                
         BZ    CALTM2                                                           
         MVC   0(2,R3),NBAFFTIM    USE FIRST 2 BYTES, LEAVE REST 0              
         B     CALTMXIT                                                         
         SPACE 1                                                                
CALTM2   MVC   0(4,R3),NBTIME      ELSE USE 4-BYTE START-END TIME               
         SPACE 1                                                                
CALTMXIT B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*                                  CONVERT DAY MASK INTO DAY NUMBER             
NIDAY    LA    R1,DAYCONVT         DAY MASK TABLE                               
NIDY100  CLC   NBDAY,0(R1)                                                      
         BE    NIDY200                                                          
         LA    R1,2(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   NIDY100                                                          
NIDY200  MVC   0(1,R3),1(R1)       SET UP DAY NUMBER                            
         B     XIT                                                              
         SPACE 1                                                                
*                                  4-BYTE PERIOD FOR UNIT DATE                  
*                                  APERLST SET TO PERIOD LIST                   
NIPER1   L     R5,APERLST1                                                      
         LTR   R5,R5                                                            
         BNZ   NIPE2                                                            
         DC    H'0'                 LIST MUST EXIST                             
NIPE2    CLC   NBACTDAT,0(R5)                                                   
         BNL   NIPE4                                                            
         DC    H'0'                 DATE NOT IN LIST                            
NIPE4    OC    0(4,R5),0(R5)        CHECK FOR EOL                               
         BNZ   NIPE6                                                            
         DC    H'0'                                                             
NIPE6    CLC   NBACTDAT,2(R5)                                                   
         BNH   XITNIP                                                           
         LA    R5,4(R5)             NEXT IN LIST                                
         B     NIPE2                                                            
         SPACE 1                                                                
XITNIP   MVC   0(4,R3),0(R5)        RETURN START-END DATES                      
         B     XIT                                                              
         EJECT                                                                  
*              7 BYTE PRODUCT (AAA/BBB)                                         
         SPACE 3                                                                
*              INPUT               NBACLI=A(CLIENT HEADER)                      
         SPACE 1                                                                
NIPRDCD  L     R6,NBACLI           A(CLIENT RECORD) FROM NETBLOCK               
         USING CLTHDR,R6                                                        
         LTR   R6,R6                                                            
         BNZ   PCD2                                                             
         DC    H'0'                BOMB IF NO CLI REC                           
         SPACE 1                                                                
PCD2     LA    R2,CLIST                                                         
         LA    R5,220                                                           
         CLI   NBPRD,X'FE'         IF PROD CODE IS PROBLEM                      
         BE    PCD2A                                                            
         CLI   NBPRD,0             IF PROD CODE IS ZERO, RETURN 'UNA'           
         BNE   PCD4                                                             
PCD2A    MVC   0(3,R3),=C'UNA'                                                  
         B     PCDXIT                                                           
         SPACE 1                                                                
PCD4     CLI   0(R2),X'FF'                                                      
         BE    PCDXIT                                                           
         SPACE 1                                                                
PCDLOP   CLC   NBPRD(1),3(R2)                                                   
         BE    PCDGOT                                                           
         LA    R2,4(R2)                                                         
         BCT   R5,PCDLOP                                                        
         SPACE 1                                                                
PCDGOT   MVC   0(3,R3),0(R2)       SECOND PRODUCT                               
         CLI   NBPRD2,0                                                         
         BE    PCDXIT                                                           
         LA    R2,CLIST                                                         
         LA    R5,220                                                           
         CLI   0(R2),X'FF'                                                      
         BE    PCDXIT                                                           
         SPACE 1                                                                
PCDLOP2  CLC   NBPRD2(1),3(R2)                                                  
         BE    PCDGOT2                                                          
         LA    R2,4(R2)                                                         
         BCT   R5,PCDLOP2                                                       
         SPACE 1                                                                
PCDGOT2  MVI   3(R3),C'/'                                                       
         MVC   4(3,R3),0(R2)                                                    
         SPACE 1                                                                
PCDXIT   B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              3 BYTE PRODUCT CODE                                              
         SPACE 3                                                                
*              INPUT               NBACLI=A(CLIENT HEADER)                      
         SPACE 1                                                                
NIPRDCOD MVC   0(3,R3),=C'999'     MAKE SURE UNALLOCATED COMES LAST             
         MVC   BYTE,NBSPLPRN       ASSUME SPLIT PROD NUMBER                     
         TM    NBSPLOPT,X'80'                                                   
         BO    *+10                                                             
         MVC   BYTE,NBPRD          UNLESS SPLIT NOT ON                          
         CLI   BYTE,0                                                           
         BE    XIT                                                              
         CLI   BYTE,X'FF'                                                       
         BE    XIT                                                              
         L     R6,NBACLI           A(CLIENT RECORD) FROM NETBLOCK               
         USING CLTHDR,R6                                                        
         LTR   R6,R6                                                            
         BNZ   PR32                                                             
         DC    H'0'                BOMB IF NO CLI REC                           
         SPACE 1                                                                
PR32     LA    R2,CLIST                                                         
         LA    R5,220                                                           
         SPACE 1                                                                
PR3LOP   CLC   BYTE,3(R2)                                                       
         BE    PR3GOT                                                           
         LA    R2,4(R2)                                                         
         BCT   R5,PR3LOP                                                        
         DC    H'0'                WHY NOT                                      
         SPACE 1                                                                
PR3GOT   MVC   0(3,R3),0(R2)       SECOND PRODUCT                               
         SPACE 1                                                                
PR3XIT   B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              IMPRESSIONS, RATINGS, VPHS                                       
         SPACE 3                                                                
*              ARGUMENTS           1 RELATIVE DEMO NUMBER                       
*                                  2 A(CTUAL) OR E(STIMATED)                    
         SPACE 1                                                                
NIIMP    BAS   RE,DEMADJ           POINTS R4 TO PROPER DEMO GROUP               
         MVC   0(4,R3),4(R4)       MOVE IMPRESSION                              
         B     XIT                                                              
         SPACE 1                                                                
*                                  RETURNS RATING AS 4 BYTE FIELD               
NIGRP    BAS   RE,DEMADJ           POINTS R4 TO PROPER DEMO GROUP               
         SR    R2,R2                                                            
         ICM   R2,3,2(R4)          TAKE 2 BYTES                                 
         ST    R2,0(R3)            AND STORE AS 4                               
         B     XIT                                                              
         SPACE 1                                                                
*                                  RETURNS VPH AS 4 BYTE FIELD                  
NIVPH    BAS   RE,DEMADJ           POINTS R4 TO PROPER DEMO GROUP               
         SR    R2,R2                                                            
         ICM   R2,3,0(R4)          TAKE 2 BYTES                                 
         ST    R2,0(R3)            AND STORE AS 4                               
         B     XIT                                                              
         EJECT                                                                  
*              UNITS                                                            
         SPACE 3                                                                
*                                  RETURNS A 1 IN A 4 BYTE FIELD                
*                                     IF UNIT IS TO BE COUNTED                  
*                                                                               
*              ARGUMENTS           1 (IGNORED)                                  
*                                  2 A(CTUAL) OR E(STIMATED)                    
*                                    IF BLANK 1 IS AUTOMATIC                    
         SPACE 1                                                                
NIUNIT   SR    R2,R2                                                            
         CLI   GLARGS+1,C'E'                                                    
         BE    UN2A                                                             
         CLI   GLARGS+1,C'A'                                                    
         BE    UN2B                                                             
         LA    R2,1                ELSE USE 1                                   
         B     UN2D                                                             
         SPACE 1                                                                
UN2A     LH    R2,NBESTUN          ESTIMATED                                    
         B     UN2D                                                             
         SPACE 1                                                                
UN2B     LH    R2,NBACTUN          ACTUAL                                       
         B     UN2D                                                             
         SPACE 1                                                                
UN2D     ST    R2,0(R3)                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO RETURN 3 BYTE PRD GROUP CODE                          
         SPACE 3                                                                
*              NDPRGBUF CONTAINS 2 BYTE POSITIONAL PRD GRP CODES                
*              NBSELPGR HAS THE 1 BYTE SCHEME CODE (V,W,X)                      
*              NBSPLPRN HAS POSITIONAL PRD NUMBER                               
*                                                                               
NIPRDGRP DS    0H                                                               
         LA    R2,NDPRGBUF         POSITIONAL 2X220 PRD GRP CODES               
         ZIC   R1,NBSPLPRN         GET POSITIONAL PRD NUMBER                    
         BCTR  R1,0                                                             
         CH    R1,=H'0'                                                         
         BNL   *+6                                                              
         DC    H'0'                                                             
         SLA   R1,1                DOUBLE NUMBER                                
         AR    R2,R1               POINT TO REQUIRED PRD GRP CODE               
         MVC   1(2,R3),0(R2)       PASS 2 BYTE CODE                             
         MVC   0(1,R3),NBSELPGR    PASS SCHEME CODE                             
         B     XIT                                                              
         SPACE 3                                                                
GRPOUT   DS    0H         A FUDGE FOR NIPRDGRP                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO RETURN COMMERCIAL SCHEDULED                           
NICOMMLS L     R4,NBAIO            COMMERCIAL SCHEDULED                         
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUCMLEL,R4                                                       
         LA    RE,NUCML1                                                        
         CLC   NBSPLPRN,NBPRD                                                   
         BE    *+8                                                              
         LA    RE,NUCML2                                                        
         MVC   0(8,R3),0(RE)                                                    
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO RETURN COMMERCIAL AIRED                               
NICOMMLA L     R4,NBAIO            COMMERCIAL THAT AIRED                        
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUPCELD,R4                                                       
         CLC   NBSPLPRN,NUPCPRD                                                 
         BE    *+12                                                             
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         MVC   1(8,R3),NUPCCML                                                  
*                                                                               
         L     R4,NBAIO            COMMERCIAL SCHEDULED                         
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   CA100                                                            
         USING NUCMLEL,R4                                                       
         LA    RE,NUCML1                                                        
         CLC   NBSPLPRN,NBPRD                                                   
         BE    *+8                                                              
         LA    RE,NUCML2                                                        
         CLC   1(8,R3),0(RE)                                                    
         BE    XIT                                                              
CA100    MVI   0(R3),C'*'                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
******************************************************                          
*              ROUTINE TO SET UP MADE GOOD BY/FOR DATA                          
*                                                                               
NIMGCMNT DS    0H                                                               
         TM    NBUNITST,X'02'      IS IT MISSED                                 
         BNO   MGC10                                                            
         MVI   0(R3),X'02'                                                      
         MVC   1(NBMGBLEN,R3),NBMGBPCD                                          
         B     MGCX                                                             
*                                                                               
MGC10    TM    NBUNITST,X'01'      IS IT MAKE-GOOD                              
         BNO   MGCX                                                             
         MVI   0(R3),X'01'                                                      
         MVC   1(NBMGFLEN,R3),NBMGFPCD                                          
MGCX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADDRESS CORRECT 8-BYTE DEMOS                          
         SPACE 3                                                                
*              INPUT (GLARGS)      1 RELATIVE DEMO NO (0=HOMES)                 
*                                  2 E OR A                                     
*              OUTPUT              (R4)=A(8-BYTE DEMOS)                         
*              LOCALS              BYTE SET TO E OR A                           
         SPACE 1                                                                
DEMADJ   NTR1                                                                   
         MVC   BYTE,GLARGS+1       MOVE ARG2 TO BYTE                            
         CLI   BYTE,C'E'           IF NOT E OR A                                
         BE    DMAJGETD                                                         
         CLI   BYTE,C'A'                                                        
         BE    DMAJGETD                                                         
         CLI   NBESTOPT,0            IF ESTOPT SET                              
         BE    DMAJ4                                                            
         CLI   NBACTOPT,0              IF ACTOPT SET                            
         BE    DMAJ2                                                            
         DC    H'0'                        BOMB                                 
         SPACE 1                       ELSE                                     
DMAJ2    MVI   BYTE,C'E'                 USE EST DEMOS                          
         B     DMAJGETD                                                         
         SPACE 1                     ELSEIF ACTOPT SET                          
DMAJ4    CLI   NBACTOPT,0                                                       
         BE    DMAJ6                                                            
         MVI   BYTE,C'A'                USE ACT DEMOS                           
         B     DMAJGETD                                                         
*                                    ELSE  (NEITHER SET)                        
DMAJ6    DC    H'0'                    BOMB. NO DEMOS REQUESTED                 
         SPACE 1                                                                
DMAJGETD CLI   GLARGS,0         IF HOMES                                        
         BNE   OTHERTG                                                          
         SR    R2,R2                                                            
         LA    R4,NBESTHOM                                                      
         CLI   BYTE,C'A'                                                        
         BNE   DADJXIT                                                          
         LA    R4,NBACTHOM                                                      
         B     DADJXIT                                                          
         SPACE 1                                                                
OTHERTG  L     R5,NBADEM                                                        
         LTR   R5,R5               DEMO BLOCK MUST EXIST                        
         BNZ   DADJB                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
         USING NDDEMBLK,R5                                                      
DADJB    LA    R4,NDESTDEM                                                      
         CLI   BYTE,C'A'         ESTIMATED DEMOS                                
         BNE   DADGC                                                            
         LA    R4,NDACTDEM                                                      
         SPACE 1                                                                
DADGC    ZIC   R2,GLARGS         DEMO NUMBER                                    
         BCTR  R2,0                CALCULATE OFFSET                             
         SLL   R2,3                                                             
         LA    R4,0(R4,R2)         POINTS TO PROPER DEMO                        
         SPACE 1                                                                
DADJXIT  XIT1  REGS=(R4)                                                        
         DROP  R5                                                               
         EJECT                                                                  
*              HEADING ROUTINES                                                 
         SPACE 3                                                                
NHIMP    B     XIT                 ???                                          
NHGRP    B     XIT                 ???                                          
         EJECT                                                                  
*              ROUTINES TO OUTPUT DATES TO HEADINGS                             
         SPACE 3                                                                
*              GLARGS              1 PERIOD NUMBER                              
*              GLARGS+1            (OPTION) =C'N'                               
*              INPUT               APERLST1=A(WEEK/MONTH LIST)                  
*              OUTPUT              MMMDD-MMMDD/ (OPTION) MMMDD                  
         SPACE 1                                                                
NHDATOUT ZIC   R4,GLARGS           PICK UP PERIOD NUMBER                        
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         BCTR  R4,0                                                             
         SLL   R4,2                (X 4)                                        
         A     R4,APERLST1                                                      
         GOTO1 DATCON,DMCB,(2,0(R4)),(4,0(R5))                                  
         CLI   GLARGS+1,C'N'        OPTION (MMMDD)                              
         BE    XIT                                                              
         MVI   5(R5),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,2(R4)),(4,6(R5))                                  
         B     XIT                                                              
         EJECT                                                                  
*              7-BYTE DEMO EXPRESSION                                           
         SPACE 3                                                                
*              INPUTS (GLARGS)     1 (R6) RELATIVE DEMO NUMBER                  
*                                  2      TYPE (0=TAKE FROM DEMLIST)            
*              LOCALS              (R5) A(NET DEMO BLOCK)                       
*              NOTE                DBLOCK NEEDED FOR DEMOCON                    
         SPACE 1                                                                
NHDEM    L     R5,NBADEM                                                        
         LTR   R5,R5                                                            
         BNZ   NHD2                                                             
         DC    H'0'                                                             
         USING NDDEMBLK,R5                                                      
         SPACE 1                                                                
NHD2     ZIC   R6,GLARGS           DEMO NUMBER                                  
         LTR   R6,R6               IF R6=0 THEN HOMES                           
         BNZ   NHD5                                                             
         MVI   FAKEDEMO,0          SET UP A FAKE HOMES REQUEST                  
         MVC   FAKEDEMO+1(1),GLARGS+1                                           
         MVI   FAKEDEMO+3,0                                                     
         B     NHD6                                                             
         SPACE 1                                                                
NHD5     BCTR  R6,0                CALCULATE OFFSET                             
         MH    R6,=H'3'              EACH DEMO IS 3-BYTES                       
         LA    R6,NDDEMOS(R6)      R6 PTS TO PROPER DEMO                        
         MVC   FAKEDEMO(3),0(R6)                                                
         CLI   GLARGS+1,0           IF NO ARG THEN USE WHATS IN NDDEMOS         
         BE    NHD6                                                             
         MVC   FAKEDEMO+1(1),GLARGS+1   ELSE SET IT TO THE ARG                  
         SPACE 1                                                                
NHD6     CLI   FAKEDEMO+1,X'21'    IF A USER DEMO                               
         BE    NHDCUSER                                                         
         CLI   FAKEDEMO+1,63       IF A WEIGHTED DEMO                           
         BE    NHDCWGHT                                                         
         B     NHDOCON                                                          
         SPACE 1                                                                
NHDCWGHT MVC   0(7,R3),NDWGTNAM    USE WEIGHTED NAME                            
         B     NHHDEND                                                          
         SPACE 1                                                                
NHDCUSER ZIC   R1,FAKEDEMO+2       GET USER DEMO NUMBER                         
         BCTR  R1,0                DECREMENT TO CALC OFFSET                     
         MH    R1,=H'7'            USER NAMES ARE 7 BYTES                       
         LA    R1,NDUSRNMS(R1)     ADRESS OF USER DEMO                          
         MVC   0(7,R3),0(R1)                                                    
         B     NHHDEND                                                          
         SPACE 1                                                                
NHDOCON  XC    DBLOCK,DBLOCK       SET UP DBLOCK                                
         MVC   DBFILE,=C'NTI'                                                   
         MVC   DBCOMFCS,NBACOM                                                  
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         LA    R2,7                                                             
         SPACE 1                                                                
NHDOC2   GOTO1 NBCALLOV,DMCB,0,X'D9000AE0'     A(DEMOCON)                       
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(0,FAKEDEMO),((R2),WORK),(C'S',DBLOCK)                 
         MVC   0(7,R3),WORK                                                     
         SPACE 1                                                                
NHHDEND  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              CLIENT/PRODUCT/ESTIMATE/NETWORK/PACKAGE/DAYPART                  
*              ROUTINES TO GET NAMES AND SET UP FOR HEADLINES                   
         SPACE 3                                                                
*              INPUT               R2 = A(SPECIFIC CODE)                        
         SPACE 1                                                                
NOCLINAM BAS   RE,GETCLI           CLIENT NAME                                  
         MVC   0(20,R3),NDCLINAM                                                
         B     XIT                                                              
         SPACE 1                                                                
CLIPAGE  BAS   RE,GETCLI           CLIENT PAGE                                  
         B     XIT                                                              
         SPACE 1                                                                
NOPRDBTH BAS   RE,GETPRD           PRODUCT CODE AND NAME                        
         MVC   0(3,R3),NDPRDKEY                                                 
         MVC   4(20,R3),NDPRDNAM                                                
         B     XIT                                                              
         SPACE 1                                                                
NOPRDNAM BAS   RE,GETPRD           PRODUCT NAME                                 
         MVC   0(20,R3),NDPRDNAM                                                
         B     XIT                                                              
         SPACE 1                                                                
NOPRDCDE BAS   RE,GETPRD           PRODUCT CODE                                 
         MVC   0(3,R3),NDPRDKEY                                                 
         B     XIT                                                              
         SPACE 1                                                                
PRODPAGE BAS   RE,GETPRD           PRODUCT PAGE                                 
         B     XIT                                                              
         SPACE 1                                                                
NOESTNAM BAS   RE,GETEST           ESTIMATE NAME                                
         MVC   0(24,R3),NDESTNAM                                                
         B     XIT                                                              
         SPACE 1                                                                
ESTPAGE  BAS   RE,GETEST           ESTIMATE PAGE                                
         B     XIT                                                              
         SPACE 1                                                                
NETPAGE  BAS   RE,GETNET           NETWORK PAGE                                 
         B     XIT                                                              
         SPACE 1                                                                
DPTPAGE  BAS   RE,GETDPT           DAYPART PAGE                                 
         B     XIT                                                              
         SPACE 1                                                                
NOPAKNAM BAS   RE,GETPAK           PACKAGE NAME                                 
         MVC   0(36,R3),NDPAKNAM                                                
         B     XIT                                                              
         SPACE 1                                                                
PACKPAGE BAS   RE,GETPAK           PACKAGE PAGE                                 
         B     XIT                                                              
         SPACE 1                                                                
NODATNUM GOTO1 DATCON,DMCB,(2,0(R2)),(4,(R3))                                   
         CLI   2(R2),0                                                          
         BE    XIT                                                              
         CLI   2(R2),1                                                          
         BE    XIT                                                              
*                                                                               
         ZIC   R0,2(R2)                                                         
         MVI   5(R3),C'-'                                                       
         EDIT  (R0),(3,6(R3)),ALIGN=LEFT                                        
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              DAYPART EXPANSION                                                
         SPACE 3                                                                
*              INPUT               (R2)=A(1 BYTE INPUT FIELD)                   
*                                  (R3)=A(8 BYTE OUTPUT AREA)                   
         SPACE 1                                                                
NODAYP   EQU   *                                                                
         EXPDP WORK,0(R2)        EXPAND IT INTO WORK                            
         MVC   0(8,R3),WORK         AND MOVE IT TO OUT AREA                     
         SPACE 1                                                                
NODPXIT  B     XIT                                                              
         SPACE 1                                                                
*              DATE OUTPUT                                                      
         SPACE 3                                                                
*                                  RETURNS MMMDD OR SPACES FOR 0                
*                                          *VAR* FOR X'FFFF'                    
*              INPUT               (R2)=A(2 BYTE COMPRESSED DATE)               
*                                  (R3)=A(OUTPUT AREA)                          
         SPACE 1                                                                
NOSDATE  EQU   *                                                                
         MVC   0(5,R3),NDSPACES     DEFAULT TO SPACES                           
         OC    0(2,R2),0(R2)        IF ZERO, XIT                                
         BZ    NOSDXIT                                                          
         CLC   0(2,R2),=XL2'FFFF'   MOVE IN *VAR* IF FFS                        
         BNE   NOSD2                                                            
         MVC   0(5,R3),=C'*VAR*'                                                
         B     NOSDXIT                                                          
         SPACE 1                                                                
NOSD2    GOTO1 NBDATCON,DMCB,(2,0(R2)),(7,0(R3))                                
         SPACE 1                                                                
NOSDXIT  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILL IN CLIENT DETAILS                                
         SPACE 3                                                                
*              INPUT               R2 = A(3-BYTE CLIENT CODE)                   
         SPACE 1                                                                
GETCLI   NTR1                                                                   
         CLC   NDCLIABR,0(R2)      HAVE WE DONE THIS ONE BEFORE                 
         BE    XIT                 YES - SO WE'RE THRU                          
         MVC   NDCLIABR,0(R2)      3-BYTE CODE                                  
         XC    NDPRGKEY,NDPRGKEY   ENSURE REREAD OF LOWER RECORDS               
         XC    NDPRDKEY,NDPRDKEY                                                
         MVI   NDESTKEY,0                                                       
         MVI   NDPAKKEY,0                                                       
*                                  2-BYTE CODE                                  
         GOTO1 NBCLPACK,DMCB,NDCLIABR,NDCLIKEY    2-BYTE CODE                   
         USING CLTHDR,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   CKEYAM,NDAGYKEY     READ CLIENT RECORD                           
         MVC   CKEYCLT,NDCLIKEY                                                 
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         MVC   NDCLINAM,CNAME      GET OTHER STUFF                              
         SPACE 1                                                                
         XC    FILENAME,FILENAME   RESET                                        
         NETGO NVSETUNT,DMCB                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FILL IN PRODUCT GROUP DETAILS                         
         SPACE 3                                                                
*              INPUT               R2 = A(3-BYTE PRODUCT GROUP CODE)            
         SPACE 1                                                                
PGRPAGE  DS    0H                                                               
         CLC   NDPRGKEY,0(R2)      HAVE WE DONE THIS ONE BEFORE                 
         BE    XIT                 YES - SO WE'RE THRU                          
         MVC   NDPRGKEY,0(R2)      3-BYTE CODE                                  
*                                  NEED FAIR BIT HERE                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRGKEY,R4                                                        
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,NDAGYKEY                                                
         MVC   PRGKCLT,NDCLIKEY                                                 
         MVC   PRGKID,0(R2)              GET PROG GRP DEF REC                   
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,NBAIO                                                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PRGEL01,R4                                                       
         MVI   NDPRGNLV,1                                                       
         MVC   NDPRGBK1,PRGBK1                                                  
         CLI   PRGBK2,0                                                         
         BE    PGC5                                                             
         MVI   NDPRGNLV,2                                                       
         MVC   NDPRGBK2,PRGBK2                                                  
PGC5     MVC   LEVELN(1),PRGBK1LN           SAVE DIGIT LENGTH OF LEVELS         
         MVC   LEVELN+1(1),PRGBK2LN                                             
*                                                                               
         LA    R4,KEY                                                           
         USING PRGKEY,R4                                                        
         MVC   PRGKID(3),0(R2)               GET SPECIFIC PROG GRP REC          
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         XC    FULL,FULL                                                        
         MVC   NDPRGAB1(1),PRGKID                                               
         MVC   FULL+1(2),PRGKGRP                                                
         MVI   FULL+3,X'0C'          ADD X'0C' SINCE PRGKGRP IS PWOS            
         UNPK  WORK(5),FULL+1(3)                                                
         LA    R5,NDPRGAB1+1                                                    
         ZIC   R1,LEVELN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK      ** NOTE DO NOT TOUCH WORK NEEDED BELOW         
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PRGEL10,R4                                                       
         MVC   NDPRGNM1,PRGNAM1                                                 
         ZIC   R1,LEVELN           IS THERE A SECOND LEVEL                      
         ZIC   R5,LEVELN+1                                                      
         LTR   R5,R5                                                            
         BZ    PGCX                                                             
         AR    R1,R5                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NDPRGAB2+1(0),WORK      SET CODE(** NOTE WORK HAS CODE)          
         MVC   NDPRGAB2(1),NDPRGAB1    SET SCHEME LETTER                        
         MVC   NDPRGNM2,PRGNAM2                                                 
*                                                                               
PGCX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILL IN PRODUCT DETAILS                               
         SPACE 3                                                                
*              INPUT               R2 = A(3-BYTE PRODUCT CODE)                  
         SPACE 1                                                                
GETPRD   NTR1                                                                   
         CLC   0(3,R2),=C'999'     SPECIAL FOR UNALLOCATED                      
         BNE   GETPRD2                                                          
         MVC   NDPRDKEY,=C'UNA'                                                 
         MVC   NDPRDNAM,=CL20'UNALLOCATED'                                      
         MVI   NDPRDCOD,255                                                     
         B     XIT                                                              
         SPACE 1                                                                
GETPRD2  CLC   NDPRDKEY,0(R2)      HAVE WE DONE THIS ONE BEFORE                 
         BE    XIT                 YES - SO WE'RE THRU                          
         MVC   NDPRDKEY,0(R2)      3-BYTE CODE                                  
         MVI   NDESTKEY,0          ENSURE REREAD OF LOWER RECORDS               
         MVI   NDPAKKEY,0                                                       
         USING PRDHDR,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   PKEYAM,NDAGYKEY     READ PRODUCT RECORD                          
         MVC   PKEYCLT,NDCLIKEY                                                 
         MVC   PKEYPRD,NDPRDKEY                                                 
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         MVC   NDPRDNAM,NDSPACES                                                
         MVI   NDPRDCOD,255                                                     
         CLC   KEYSAVE(13),KEY                                                  
         BNE   GETPRD4                                                          
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         MVC   NDPRDNAM,PNAME      GET OTHER STUFF                              
         MVC   NDPRDCOD,PCODE+1                                                 
         SPACE 1                                                                
GETPRD4  XC    FILENAME,FILENAME   RESET                                        
         NETGO NVSETUNT,DMCB                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FILL IN ESTIMATE DETAILS                              
         SPACE 3                                                                
*              INPUT               R2 = A(1-BYTE ESTIMATE NUMBER)               
         SPACE 1                                                                
GETEST   NTR1                                                                   
         CLC   NDESTKEY,0(R2)      HAVE WE DONE THIS ONE BEFORE                 
         BE    XIT                 YES - SO WE'RE THRU                          
         MVC   NDESTKEY,0(R2)      1-BYTE NUMBER                                
         MVI   NDPAKKEY,0          ENSURE PACKAGE REREAD                        
         EDIT  (1,NDESTKEY),(3,NDESTABR)                                        
         USING ESTHDR,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   EKEYAM,NDAGYKEY     READ ESTIMATE RECORD                         
         MVC   EKEYCLT,NDCLIKEY                                                 
         MVC   EKEYPRD,NDPRDKEY                                                 
         OC    EKEYPRD,EKEYPRD                                                  
         BNZ   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         CLC   EKEYPRD,=C'UNA'                                                  
         BNZ   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,NDESTKEY                                                 
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
GTEST2   MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         MVC   NDESTNAM,NDSPACES                                                
         CLC   KEYSAVE(13),KEY                                                  
         BE    GTEST5                                                           
         CLC   EKEYPRD,=C'POL'     IF NO MATCH/ TRY FOR POL                     
         BE    GTESTX                                                           
         XC    KEY,KEY                                                          
         MVC   KEY,KEYSAVE                                                      
         MVC   EKEYPRD,=C'POL'                                                  
         B     GTEST2                                                           
GTEST5   MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         MVC   NDESTNAM,EDESC      RETURN ESTIMATE NAME                         
         SPACE 1                                                                
GTESTX   XC    FILENAME,FILENAME   RESET                                        
         NETGO NVSETUNT,DMCB                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FILL IN NETWORK DETAILS                               
         SPACE 3                                                                
*              INPUT               R2 = A(4-BYTE NETWORK CODE)                  
         SPACE 1                                                                
GETNET   NTR1                                                                   
         CLC   NDNETKEY,0(R2)      HAVE WE DONE THIS ONE BEFORE                 
         BE    XIT                 YES - SO WE'RE THRU                          
         MVC   NDNETKEY,0(R2)      SAVE CODE                                    
         XC    NDNETNAM,NDNETNAM                                                
         MVC   NDNETNAM(4),0(R2)   MAY GET 'MARKET' NAME ONE DAY                
         MVI   NDNETMED,0                                                       
         XC    NDNETMAB,NDNETMAB                                                
         L     R2,NBANBUFF         HAS A BUFFER BEEN PROVIDED                   
         LTR   R2,R2                                                            
         BZ    XIT                                                              
         USING NBUFFD,R2                                                        
         LA    R0,50                                                            
         SPACE 1                                                                
GETNET2  CLI   NBUFFNET,0          LOOK UP NETWORK BUFFER FOR MEDIA             
         BE    XIT                                                              
         CLC   NBUFFNET,NDNETKEY                                                
         BE    GETNET4                                                          
         LA    R2,L'NBUFFREC(R2)                                                
         BCT   R0,GETNET2                                                       
         B     XIT                                                              
         SPACE 1                                                                
GETNET4  MVC   NDNETMED,NBUFFMED   FOUND - PASS BACK MEDIA                      
         MVC   NDNETMAB,=C'OTHER'          EXPAND                               
         CLI   NDNETMED,C'N'                                                    
         BNE   *+10                                                             
         MVC   NDNETMAB,=C'NET  '                                               
         CLI   NDNETMED,C'C'                                                    
         BNE   *+10                                                             
         MVC   NDNETMAB,=C'CABLE'                                               
         CLI   NDNETMED,C'S'                                                    
         BNE   *+10                                                             
         MVC   NDNETMAB,=C'SYND.'                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILL IN DAYPART DETAILS                               
         SPACE 3                                                                
*              INPUT               R2 = A(1-BYTE DAYPART CODE)                  
         SPACE 1                                                                
GETDPT   NTR1                                                                   
         CLC   NDDPTKEY,0(R2)      HAVE WE DONE THIS ONE BEFORE                 
         BE    XIT                 YES - SO WE'RE THRU                          
         MVC   NDDPTKEY,0(R2)      SAVE CODE                                    
         EXPDP NDDPTNAM,0(R2)                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILL IN PACKAGE DETAILS                               
         SPACE 3                                                                
*              INPUT               R2 = A(1-BYTE PACKAGE NUMBER)                
         SPACE 1                                                                
GETPAK   NTR1                                                                   
         CLC   NDPAKKEY,0(R2)      HAVE WE DONE THIS ONE BEFORE                 
         BE    XIT                 YES - SO WE'RE THRU                          
         MVC   NDPAKKEY,0(R2)      1-BYTE NUMBER                                
         EDIT  (1,NDPAKKEY),(3,NDPAKABR)                                        
         USING NPRECD,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   NPKTYPE,X'02'       READ PACKAGE RECORD                          
         MVC   NPKAM,NDAGYKEY                                                   
         MVC   NPKCLT,NDCLIKEY                                                  
         MVC   NPKNET,NDNETKEY                                                  
         MVC   NPKEST,NDESTKEY                                                  
         MVC   NPKPACK,NDPAKKEY                                                 
         MVC   FILENAME,=C'UNTDIR  '     READ UNIT FILE                         
         GOTO1 HIGH                                                             
         MVC   NDPAKNAM,NDSPACES                                                
         CLC   KEYSAVE(20),KEY                                                  
         BNE   XIT                                                              
         MVC   FILENAME,=C'UNTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         MVC   NDPAKNAM(16),NBPAKNAM     GET OTHER STUFF                        
         EXPDP NDPAKNAM+17,NPAKDP        LOOK UP DAYPART                        
         MVI   NDPAKNAM+26,C'$'                                                 
         LA    R5,NDPAKNAM+27                                                   
         EDIT  (4,NPAKCOST),(8,(R5)),ALIGN=LEFT,FLOAT=$                         
         GOTO1 SQUASHER,DMCB,NDPAKNAM,36                                        
         SPACE 1                                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ACCGEN - GENERAL COST EXTRACT                                    
         SPACE 3                                                                
*        1ST BYTE OF GLARGS HAS ARGUMENT FOR NETACC                             
*            IF BYTE IS GREATER THAN 200, GO TO NDSFTAGR TO GET ARG             
*                                                                               
         SPACE 1                                                                
NIACCGEN DS    0H                                                               
         ZIC   R1,GLARGS                                                        
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CH    R1,=H'201'         CHK IF SOFT ARGS (GRTR THAN 200)              
         BL    ACG2                                                             
         SH    R1,=H'201'         SUBTRACT 201 FOR DISP INTO TABLE              
         LA    R2,NDSFTARG        GET START OF SOFT ARG TABLE                   
         AR    R2,R1              ADD DISP TO POINT TO ARG                      
         ZIC   R1,0(R2)           ZIC ARG INTO R1                               
ACG2     STC   R1,BYTE                                                          
         ZIC   R1,NDADJPCT         GET DECIMAL ADJUSTMENT                       
         STC   R1,DCMLADJ                                                       
         LA    R2,NETBLOCK                                                      
         L     R4,NDADJPCT         GET ANY PCT ADJUSTMENT                       
         PRINT GEN                                                              
         GOTO1 =V(NETACC),DMCB,(BYTE,(R3)),(R2),(DCMLADJ,(R4))                  
         PRINT NOGEN                                                            
         B     XIT                                                              
         EJECT                                                                  
***************************************                                         
*NICOST         RETURNS THE SPECIFIC COST REQUESTED                             
*    (1-BYTE FLAG =0 IF NONE, 1 IF ZERO + 8 BYTE PACKED COST)                   
*       INPUTS: ARGS:                                                           
*                1ST WORD - COST MASK OF COSTS TO BE ADDED                      
*                2ND WORD- COST MASK OF COSTS TO BE SUBTRACTED                  
*       LOCALS:  TOTCOST - CURRENT SUM                                          
*                R2 - ZERO IF NO COSTS, 1 IF ANY FOUND                          
*                ARGTOADD - SPECIAL FULLWORD MASK. MUST ALSO BE A               
*                           MATCH IF ANY SPECIAL MASK BITS ARE SET.             
*                           CURRENTLY UNUSED                                    
*                BYTE - 0 IF ZERO, 1 IF TRUE ZERO                               
*                                                                               
NICOST EQU     *                                                                
         XC    ARGTOADD,ARGTOADD   CLEAR SPECIAL ARG  (UNUSED)                  
         XC    TOTCOST,TOTCOST      START WITH ZERO                             
         SR    R2,R2               INITIALIZE                                   
         SPACE 1                                                                
         MVI   BYTE,0              PRESET BYTE                                  
         SPACE 1                                                                
PU1      CLI   NBUSER+8,C'Y'       IF FLAG SET TO USE ASSIGNED                  
         BNE   PU3                                                              
         OC    NBASSIGN,NBASSIGN   AND IF ASSIGNED COST NON-ZERO                
         BNZ   PU4                                                              
         TM    NBUNITST,X'08'      OR ASSIGNED COST TRULY ZERO                  
         BZ    PU3                                                              
         MVI   BYTE,1                                                           
PU3      TM    NBUNITST,X'20'                                                   
         BZ    PU4                                                              
         MVI   BYTE,1                                                           
PU4      EQU   *                                                                
PU5      L     R0,NBACTUAL         GET NET DOLLARS                              
         BAS   RE,FINDNET                                                       
         ST    R0,NETACT                                                        
         SPACE 1                                                                
         L     R0,NBASSIGN                                                      
         BAS   RE,FINDNET                                                       
         ST    R0,NETASS                                                        
         SPACE 1                                                                
         L     R0,NBCALCOS                                                      
         BAS   RE,FINDNET                                                       
         ST    R0,NETCASS                                                       
         SPACE 1                                                                
         L     R0,NBINTEG                                                       
         BAS   RE,FINDNET                                                       
         ST    R0,NETINT                                                        
         SPACE 1                                                                
         GOTO1 ADDEM,DMCB,NBCALCOS,=AL4(CASSMSK)                                
         GOTO1 ADDEM,DMCB,NETCASS,=AL4(NTCSSMSK)                                
         SPACE 1                                                                
         MVI   BYTE,0                                                           
         TM    NBUNITST,X'20'                                                   
         BZ    PU6                                                              
         MVI   BYTE,1                                                           
PU6      GOTO1 ADDEM,DMCB,NBACTUAL,=AL4(ACTMSK)                                 
         GOTO1 ADDEM,DMCB,NETACT,=AL4(NTACTMSK)                                 
         SPACE 1                                                                
         MVI   BYTE,0                                                           
         TM    NBUNITST,X'08'                                                   
         BZ    PU8                                                              
PU8      GOTO1 ADDEM,DMCB,NBASSIGN,=AL4(ASSMSK)                                 
         GOTO1 ADDEM,DMCB,NETASS,=AL4(NTASSMSK)                                 
         SPACE 1                                                                
         MVI   BYTE,0                                                           
         GOTO1 ADDEM,DMCB,NBINTEG,=AL4(INTMSK)                                  
         GOTO1 ADDEM,DMCB,NETINT,=AL4(NTINTMSK)                                 
         SPACE 1                                                                
         GOTO1 ADDEM,DMCB,NBFEED,=AL4(FEEDMSK)                                  
         SPACE 1                                                                
         L     R6,AACCBLK                                                       
         USING NACCBLKD,R6                                                      
         LTR   R6,R6                                                            
         BNZ   BILLGRS                                                          
         DC    H'0'                                                             
         SPACE 1                                                                
BILLGRS  MVC   BYTE,NACBTFLG                                                    
         GOTO1 ADDEM,DMCB,NACBTGRS,=AL4(BTGMSK)                                 
         GOTO1 ADDEM,DMCB,NACBTNET,=AL4(BTNMSK)                                 
*                                  INTEG                                        
BILLINT  MVC   BYTE,NACBIFLG                                                    
         GOTO1 ADDEM,DMCB,NACBIGRS,=AL4(BIGMSK)                                 
         GOTO1 ADDEM,DMCB,NACBINET,=AL4(BINMSK)                                 
         SPACE 1                                                                
         MVC   BYTE,NACPTFLG                                                    
         GOTO1 ADDEM,DMCB,NACPTGRS,=AL4(PTGMSK)                                 
         GOTO1 ADDEM,DMCB,NACPTNET,=AL4(PTNMSK)                                 
         SPACE 1                                                                
*                                    INTEG                                      
PAYINT   MVC   BYTE,NACPIFLG                                                    
         GOTO1 ADDEM,DMCB,NACPIGRS,=AL4(PIGMSK)                                 
         GOTO1 ADDEM,DMCB,NACPINET,=AL4(PINMSK)                                 
         SPACE 1                                                                
         L     R5,TOTCOST          PUT COST IN R5 FOR MULTIPLICATION            
         OC    NACPCT,NACPCT       IF PCT IS 0 OR 100, DON'T RECALC             
         BZ    DOCO2                                                            
         CLC   NACPCT,=F'10000'                                                 
         BE    DOCO2                                                            
         SPACE 1                                                                
         M     R4,NACPCT           MULT BY PCTG                                 
         A     R5,=F'5000'         ROUNDING FACTOR                              
         D     R4,=F'10000'        RESULT IN R5                                 
         SPACE 1                                                                
DOCO2    CVD   R5,1(R3)            PACKED COST                                  
         MVI   0(R3),0             FLAG                                         
         LTR   R2,R2                                                            
         BZ    DOCEND                                                           
         MVI   0(R3),X'20'         BIT FOR ZERO=YES                             
DOCEND   B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************                                                                
*********************                                                           
FINDNET  SRDA  R0,32               PREPARE MULTIPLICAND                         
         M     R0,=F'8500'                                                      
         SLDA  R0,1                DOUBLE FOR ROUNDING                          
         D     R0,=F'10000'                                                     
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    R0,R1               REPLACE COST WITH NET                        
         BR    RE                                                               
****************************************************************                
         EJECT                                                                  
*********************************************                                   
* ADDEM - ADD FIELD TO TOTCOST                                                  
*   INPUTS - TOTCOST -  CURRENT TOTAL                                           
*       ARG 1ST WORD - BITMASK OF COSTS TO ADD                                  
*       ARG 2ND WORD - BITMASK OF COSTS TO SUBTRACT                             
*            BYTE - SET TO 1 IF TRUE ZERO                                       
*            ARGTOADD - SET FOR SPECIAL COSTS                                   
*            ARG1 - A(FULLWORD TO ADD)                                          
*            ARG2 - A(BIT MASK OF THIS FULLWORD)                                
*   OUTPUTS - TOTCOST - NEW TOTAL                                               
*             R2 - NONZERO IF FULLWORD NOT ZERO OR BYTE SET                     
*********************************                                               
ADDEM    NTR1                                                                   
         L     R5,0(R1)                                                         
         L     R5,0(R5)            COST                                         
         L     R4,4(R1)                                                         
         L     R4,0(R4)            BITMASK                                      
         SPACE 1                                                                
         ST    R4,FULL             PUT MASK FOR THIS COST IN FULL               
         NC    FULL(4),GLARGS      IF MASK MATCH                                
         BZ    CKSUB                                                            
         MVC   FULL(4),GLARGS                                                   
         NC    FULL,=AL4(SPECMSK)   IF SPECIAL COST                             
         BZ    AML2                                                             
         OC    ARGTOADD,ARGTOADD    IF SPECIAL ARG GIVEN                        
         BZ    AML2                                                             
         NC    FULL,ARGTOADD       THEN MUST ALSO MATCH SPECIAL ARG             
         BZ    CKSUB                                                            
         SPACE 1                                                                
AML2     L     R1,TOTCOST                                                       
         AR    R1,R5                                                            
         ST    R1,TOTCOST          SO ADD IT                                    
         CLI   BYTE,1              IF BYTE SET                                  
         BNE   AML3                                                             
         ZIC   R2,1                   SET R3                                    
         SPACE 1                                                                
AML3     LTR   R5,R5               IF ZERO GO ON                                
         BZ    CKSUB                                                            
         ZIC   R2,1                A NUMBER, SO SET R3                          
         SPACE 1                                                                
CKSUB    ST    R4,FULL             NOW DO FOR SUBTRACTION                       
         NC    FULL(4),GLARGS+4    IF MASK MATCH                                
         BZ    XITAD                                                            
         MVC   FULL(4),GLARGS+4                                                 
         NC    FULL,=AL4(SPECMSK)   IF SPECIAL COST                             
         BZ    AML4                                                             
         OC    ARGTOADD,ARGTOADD    IF SPECIAL ARG GIVEN                        
         BZ    AML4                                                             
         NC    FULL,ARGTOADD       THEN MUST ALSO MATCH SPECIAL ARG             
         BZ    XITAD                                                            
         SPACE 1                                                                
AML4     L     R1,TOTCOST                                                       
         SR    R1,R5                                                            
         ST    R1,TOTCOST          SO SUBTRACT IT                               
         SPACE 1                                                                
         CLI   BYTE,1              IF BYTE SET                                  
         BNE   AD2                                                              
         ZIC   R2,1                   SET R3                                    
         SPACE 1                                                                
AD2      LTR   R5,R5               IF ZERO EXIT                                 
         BZ    XITAD                                                            
         ZIC   R2,1                A NUMBER, SO SET R3                          
         SPACE 1                                                                
XITAD    XIT1  REGS=(R2)                                                        
***************************************                                         
*                                                                               
         EJECT                                                                  
*              BILLED PRODUCT CODE                                              
         SPACE 3                                                                
NIBILPRD L     R6,NBACLI           A(CLIENT RECORD) FROM NETBLOCK               
         USING CLTHDR,R6                                                        
         LTR   R6,R6                                                            
         BNZ   BPD2                                                             
         DC    H'0'                BOMB IF NO CLI REC                           
         SPACE 1                                                                
BPD2     LA    R2,CLIST                                                         
         DROP  R6                                                               
         L     R6,AACCBLK                                                       
         USING NACCBLKD,R6                                                      
         LTR   R6,R6                                                            
         BNZ   BPD3                CK IF AN ACCTG BLOCK SET-UP                  
         DC    H'0'                                                             
         SPACE 1                                                                
BPD3     LA    R5,220                                                           
         CLI   NACBTPRD,0          IF TIME PRD IS ZERO, TRY INT                 
         BNE   BPD6                                                             
         CLI   NACBIPRD,0          IF INT PRD IS ZERO, RETURN BLANK             
         BNE   BPD8                                                             
         MVC   0(3,R3),=C'   '                                                  
         B     BPDXIT                                                           
BPD6     MVC   BYTE,NACBTPRD        USE TIME PROD                               
         CLI   NACBIPRD,0           MAKE SURE MATCHES INT PROD                  
         BE    BPDLOP                                                           
         CLC   NACBIPRD,NACBTPRD                                                
         BE    BPDLOP                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
BPD8     MVC   BYTE,NACBIPRD        USE INT PROD                                
         SPACE 1                                                                
BPDLOP   CLI   0(R2),X'FF'                                                      
         BE    BPDXIT                                                           
         CLC   BYTE(1),3(R2)                                                    
         BE    BPDGOT                                                           
         LA    R2,4(R2)                                                         
         BCT   R5,BPDLOP                                                        
BPDGOT   MVC   0(3,R3),0(R2)                                                    
         SPACE 1                                                                
BPDXIT   B     XIT                                                              
         DROP  R6                                                               
*                                  ALPHA SUB-LINE                               
         SPACE 3                                                                
*                                  RETURN LINE NUMBER PRECEDED BY -             
         SPACE 1                                                                
NISUB    MVC   0(4,R3),NDSPACES    DEFAULT TO SPACES                            
         CLI   NBACTSUB,1          IF SUBLINE NE 1                              
         BE    NISBXIT                                                          
         ZIC   R2,NBACTSUB          THEN PUT IT IN PRINTABLE FORMAT             
         EDIT  (R2),(3,1(R3)),ALIGN=LEFT                                        
         SPACE 1                                                                
NISBXIT  B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR MAKE-GOODS                                          
         SPACE 3                                                                
*                                  MAKE-GOOD FOR LINE NUMBER                    
*                                  RETURNS -NNN IF LINE PRESENT                 
NIMGFSUB MVC   0(4,R3),NDSPACES    DEFAULT TO SPACES                            
         CLI   NBMGFSUB,1          IF SUBLINE NE 1                              
         BE    NIFSBXIT                                                         
         ZIC   R2,NBMGFSUB         THEN PUT IT IN PRINTABLE FORMAT              
         EDIT  (R2),(3,1(R3)),ALIGN=LEFT                                        
         SPACE 1                                                                
NIFSBXIT B     XIT                                                              
         SPACE 1                                                                
*                                  MAKE-GOOD BY LINE NUMBER                     
         SPACE 1                                                                
NIMGBSUB MVC   0(4,R3),NDSPACES    DEFAULT TO SPACES                            
         CLI   NBMGBSUB,1          IF SUBLINE NE 1                              
         BE    NIBSBXIT                                                         
         ZIC   R2,NBMGBSUB         THEN PUT IT IN PRINTABLE FORMAT              
         EDIT  (R2),(3,1(R3)),ALIGN=LEFT                                        
         SPACE 1                                                                
NIBSBXIT B     XIT                                                              
         SPACE 2                                                                
********************************************                                    
*   ROUTINE TO PUT OUT MAKE GOOD BY/FOR DATA                                    
*                           R2=A(OUT DATA)                                      
NOMGCMNT DS    0H                                                               
         CLI   0(R2),X'01'         IS IT MAKE-GOOD                              
         BNE   OMG3                                                             
         MVC   0(14,R3),=C'MAKE-GOOD FOR '                                      
         B     OMG5                                                             
OMG3     CLI   0(R2),X'02'         IS IT MISSED                                 
         BNE   OMGX                                                             
         MVC   0(14,R3),=C'MADE GOOD BY'                                        
OMG5     LA    R3,14(R3)                                                        
         LA    R2,1(R2)                                                         
         MVC   0(6,R3),0(R2)       PROGRAM CODE                                 
         LA    R3,7(R3)                                                         
         LA    R2,6(R2)                                                         
         MVC   0(16,R3),0(R2)      PROGRAM NAME                                 
         LA    R3,17(R3)                                                        
         LA    R2,16(R2)                           DATE-SUBLINE                 
         GOTO1 DATCON,DMCB,(2,0(R2)),(8,0(R3))                                  
         ZIC   R4,2(R2)                                                         
         LTR   R4,R4                                                            
         BZ    OMG9                                                             
         MVI   8(R3),C'-'                                                       
         EDIT  (R4),(3,9(R3)),ALIGN=LEFT                                        
         L     R3,GLAOFLD                                                       
OMG9     GOTO1 SQUASHER,DMCB,(R3),50                                            
*                                                                               
OMGX     B     XIT                                                              
         EJECT                                                                  
*              BILLED AND PAID DATES                                            
         SPACE 3                                                                
NIBILDT  L     R6,AACCBLK          BILLED DATE                                  
         USING NACCBLKD,R6                                                      
         LTR   R6,R6                                                            
         BNZ   BDT1                CK IF AN ACCTG BLOCK SET-UP                  
         DC    H'0'                                                             
         SPACE 1                                                                
BDT1     OC    NACBTDAT,NACBTDAT                                                
         BZ    BDT2                                                             
         MVC   0(2,R3),NACBTDAT    TIME DATE                                    
         OC    NACBIDAT,NACBIDAT   INT DATE SHOULD BE EMPTY OR                  
         BZ    XITBDT              SAME AS TIME                                 
         CLC   NACBIDAT,NACBTDAT                                                
         BE    XITBDT                                                           
         MVC   0(2,R3),=XL2'FFFF' IF DIFFERENT RETURN FFS                       
         B     XITBDT                                                           
         SPACE 1                                                                
BDT2     MVC   0(2,R3),NACBIDAT   INTEG DATE                                    
         SPACE 1                                                                
XITBDT   B     XIT                                                              
         DROP  R6                                                               
         SPACE 1                                                                
NIPAYDT  L     R6,AACCBLK         PAID DATE                                     
         USING NACCBLKD,R6                                                      
         LTR   R6,R6                                                            
         BNZ   PDT1                                                             
         SPACE 1                                                                
PDT1     OC    NACPTDAT,NACPTDAT                                                
         BZ    PDT2                                                             
         MVC   0(2,R3),NACPTDAT   TIME DATE                                     
         OC    NACPIDAT,NACPIDAT  INT DATE SHOULD BE EMPTY OR                   
         BZ    XITPDT             SAME AS TIME                                  
         CLC   NACPIDAT,NACPTDAT                                                
         BE    XITPDT                                                           
         MVC   0(2,R3),=XL2'FFFF' IF DIFFERENT RETURN FFS                       
         B     XITPDT                                                           
         SPACE 1                                                                
PDT2     MVC   0(2,R3),NACPIDAT   INTEG DATE                                    
         SPACE 1                                                                
XITPDT   B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*        GETEL                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                  DAY CONVERSSION TABLE                        
DAYCONVT EQU   *                                                                
         DC    X'7C',X'0'          M-F                                          
         DC    X'40',X'1'          MON                                          
         DC    X'20',X'2'          TUE                                          
         DC    X'10',X'3'          WED                                          
         DC    X'08',X'4'          THU                                          
         DC    X'04',X'5'          FRI                                          
         DC    X'02',X'6'          SAT                                          
         DC    X'01',X'7'          SUN                                          
         DC    X'7F',X'8'          M-S                                          
         DC    X'00',X'9'          OTHER                                        
         SPACE 3                                                                
*              LTORG                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              WORKING STORAGE FOR MODULE                                       
         SPACE 3                                                                
NDWORK   DSECT                                                                  
TOTCOST  DS    F                                                                
ARGTOADD DS    F                                                                
NETACT   DS    F                                                                
NETASS   DS    F                                                                
NETCASS  DS    F                                                                
NETINT   DS    F                                                                
NDSPACES DS    CL198                                                            
         SPACE 1                                                                
FAKEDEMO DS    CL3                 HOLDS A 3-BYTE DEMO CODE                     
DCMLADJ  DS    CL1                                                              
LEVELN   DS    CL2                                                              
         SPACE 1                                                                
*              DEDBLOCK                                                         
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
NDWORKX  EQU   *                   END OF W/S                                   
         SPACE 1                                                                
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         SPACE 1                                                                
*              NEGENINCLS          INCLUDES HERE                                
*              SPGENCLT                                                         
*              SPGENPRD                                                         
*              SPGENEST                                                         
*              NEGENPACK                                                        
*              NEACCTBLK                                                        
*              NEGENCOSTS                                                       
*              NEGENNBUFF                                                       
*              NEGENUNIT                                                        
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEACCTBLK                                                      
       ++INCLUDE NEGENCOSTS                                                     
       ++INCLUDE NEGENNBUFF                                                     
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NESYSDRIVT05/01/02'                                      
         END                                                                    
