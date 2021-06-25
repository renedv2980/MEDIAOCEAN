*          DATA SET SPRANSID   AT LEVEL 010 AS OF 05/01/02                      
*PHASE T00A47A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        SPRANSID - I/O CONTROL MODULE FOR SIR DETAIL RECORDS  *         
*                                                                     *         
*  CALLED FROM  SPOT RESEARCH, ET. AL.                                *         
*                                                                     *         
*  INPUTS       SPRANSIDD (CONTROL BLOCK)                             *         
*                                                                     *         
*  OUTPUTS      UPDATED RANSID BLOCK                                  *         
*               SIR DETAIL RECORD IN USER IO AREA                     *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- SIRRECD                                         *         
*               R5 -- WORK                                            *         
*               R6 -- GETEL                                           *         
*               R7 -- WORK                                            *         
*               R8 -- WORKING STORAGE (2ND 4K)                        *         
*               R9 -- RANSIDD BLOCK                                   *         
*               RA -- 2ND PROGRAM BASE                                *         
*               RB -- 1ST PROGRAM BASE                                *         
*               RC -- WORKING STORAGE                                 *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
***********************************************************************         
         TITLE 'RANSID - I/O CONTROL MODULE FOR SIR DETAIL RECORDS'             
RANSID   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MODDX-MODD,**RSID**,RA                                           
         USING MODD,RC,R8                                                       
         LR    R8,RC                                                            
         AHI   R8,4096                                                          
*                                                                               
         ST    RD,SAVERD           FOR ERREX                                    
         MVC   USERRD,4(RD)        SAVE CALLING RD                              
*                                                                               
         L     R9,0(R1)            USER PASSES A(SIRBLOCK)                      
         USING RANSIDD,R9                                                       
*                                                                               
         L     R1,SRACOM                                                        
         USING COMFACSD,R1                                                      
         MVC   ADATAMGR,CDATAMGR   A(DATAMGR)                                   
         MVC   ADATCON,CDATCON     A(DATCON)                                    
         MVC   AGETPROF,CGETPROF   A(GETPROF)                                   
         MVC   AHELLO,CHELLO       A(HELLO)                                     
         MVC   ASWITCH,CSWITCH     A(SWITCH)                                    
         L     RF,CCALLOV                                                       
         DROP  R1                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QSTAPACK                                                  
         GOTO1 (RF),DMCB           CALLOV RETURNS A(STAPACK)                    
         MVC   VSTAPACK,DMCB                                                    
*                                                                               
         LA    R4,KEY              R4 ALWAYS POINTS TO KEY                      
         USING SIRKEY,R4                                                        
*                                                                               
         CLI   SIINIT,0            TEST WE'VE BEEN HERE BEFORE                  
         BE    INIT                NO                                           
*                                                                               
GETNEXT  OC    SISQNUMS,SISQNUMS   TEST MUST READ NSID RECORD                   
         BZ    READNSID            YES                                          
         B     READDET                                                          
         EJECT                                                                  
* INITIALIZATION ROUTINES                                                       
*                                                                               
INIT     LA    R1,SRBLKX-SRACTDIR                                               
         XCEF  SRACTDIR,(R1)       CLEAR RESERVED STORAGE AREA                  
         MVI   SRMODE,SRNORECS                                                  
         MVI   SIINIT,1                                                         
*                                                                               
         CLC   SRSELSCH,=C'ALL'    TEST SCHEME 'ALL'                            
         BE    INIT5               YES - SCHEME CODE REMAINS ZEROES             
         OC    SRSELSCH,=C'   '                                                 
         GOTO1 SRACLPAC,DMCB,SRSELSCH,SRACTSCH                                  
         CLI   DMCB,0                                                           
         BE    *+12                                                             
         MVI   SRERROR,SRNOSCH                                                  
         B     ERREX                                                            
*                                                                               
INIT5    XC    KEY,KEY             BUILD THE SCHEME KEY                         
         MVI   SIRKTYPE,SIRKTYPQ                                                
         MVI   SRACTTYP,SIRKTYPQ                                                
         MVC   SIRKAM,SRSELAM                                                   
         MVC   SRACTAM,SRSELAM                                                  
         MVC   SIRKCODE,SRACTSCH                                                
         GOTO1 ADATAMGR,DMCB,DMREAD,SPTDIR,KEY,KEY,0                            
         TM    DMCB+8,X'EF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    DMCB+8,X'10'        TEST SCHEME KEY FOUND                        
         BZ    *+12                YES                                          
         MVI   SRERROR,SRNOSCH                                                  
         B     ERREX                                                            
*                                                                               
         GOTO1 ADATAMGR,DMCB,GETREC,SPTFIL,KEY+14,IO,DMWORK                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SVUSECMP,C'N'       ASSUME WE SHOULDN'T USE COMPETITION          
         MVI   SVUSEREP,C'N'       ASSUME NOT READING REP MARKETS               
         XC    WORK,WORK           READ SID PROFILE                             
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),SRSELAGY                                              
         MVC   WORK+22(1),SRSELMED                                              
         CLC   SRSELSCH,=C'ALL'                                                 
         BE    *+10                                                             
         MVC   WORK+23(3),SRSELSCH                                              
         GOTO1 AGETPROF,DMCB,WORK+16,WORK,ADATAMGR                              
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    *+16                NO                                           
         MVC   SVUSECMP,WORK       SAVE PROFILE VALUES                          
         MVC   SVUSEREP,WORK+1                                                  
*                                                                               
         CLI   SVUSEREP,C'Y'       DO WE NEED TO GET REP SENUM?                 
         BNE   INIT8                                                            
         OC    SRAMASTC,SRAMASTC                                                
         BZ    INIT8               NO (WE'RE ONLINE, OR NOT A REP)              
*                                                                               
         XC    WORK,WORK           READ ACCESS RECORD FROM CTFILE               
         MVI   WORK,C'5'                                                        
         MVC   WORK+23(2),SRSELAGY AGENCY ALPHA (FOR REP)                       
         GOTO1 ADATAMGR,DMCB,DMREAD,CTFILE,WORK,IO2                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                NO ACCESS RECORD FOR THIS REP                
*                                                                               
         LA    R1,IO2+28           FIND SYSTEM AUTHORIZATION ELEMENT            
INIT6    CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   *+22                NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BNE   *+14                YES                                          
         MVC   SVREPSE,3(R1)       SAVE SENUM                                   
         B     INIT8                                                            
*                                                                               
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   INIT6               NO                                           
         DC    H'0'                CAN'T FIND REP SYSTEM ELEMENT                
*                                                                               
INIT8    LA    R6,IO                                                            
         MVI   ELCODE,ERSCODEQ     DEFAULT RATINGS SOURCE ELEMENT               
         USING ERSELEM,R6                                                       
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SRBKSRC,ERSSRC                                                   
         DROP  R6                                                               
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,EDCCODEQ     DAYPART ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 = NO. OF DAYPARTS                         
         LA    R2,ALPHANUM                                                      
         LA    R3,SVDPTS                                                        
         LA    R6,2(R6)                                                         
*                                                                               
INIT10   CLC   0(1,R2),0(R6)       LOOK FOR DAYPART CODE IN TABLE               
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         LA    R3,7(R3)                                                         
         B     INIT10                                                           
*                                                                               
         MVC   0(7,R3),1(R6)       SAVE DAYPART NAME                            
         LA    R6,8(R6)                                                         
         LA    R2,1(R2)                                                         
         LA    R3,7(R3)                                                         
         BCT   R1,INIT10                                                        
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,EPCCODEQ     PROGTYPE ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   INIT20              NOT THERE                                    
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 = NO. OF PRGTYPES                         
         LA    R2,ALPHANUM                                                      
         LA    R3,SVPGTYPS                                                      
         LA    R6,2(R6)                                                         
*                                                                               
INIT15   CLC   0(1,R2),0(R6)       LOOK FOR PRGTYPE CODE IN TABLE               
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         LA    R3,7(R3)                                                         
         B     INIT15                                                           
*                                                                               
         MVC   0(7,R3),1(R6)       SAVE PRGTYPE NAME                            
         LA    R6,8(R6)                                                         
         LA    R2,1(R2)                                                         
         LA    R3,7(R3)                                                         
         BCT   R1,INIT15                                                        
*                                                                               
INIT20   CLI   SRSELYR,0           TEST A YEAR WAS GIVEN                        
         BNE   INIT30              YES                                          
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,EDYCODEQ     DEFAULT YEAR ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EDYELEM,R6                                                       
         MVC   SRSELYR,EDYYEAR                                                  
         DROP  R6                                                               
*                                                                               
INIT30   LA    R6,IO                                                            
         MVI   ELCODE,EPNCODEQ     PERIOD NAMES ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'5'            R1 = NO. OF PERIOD NAMES                     
         LA    R6,2(R6)                                                         
*                                                                               
         OC    SRSELPER,=C'    '   PAD PERIOD NAME WITH BLANKS                  
INIT40   CLC   SRSELPER,1(R6)      TEST MATCH ON PERIOD NAME                    
         BE    INIT50              GOT IT                                       
         LA    R6,5(R6)                                                         
         BCT   R1,INIT40                                                        
         MVI   SRERROR,SRNOPER     PERIOD NAME IS INVALID                       
         B     ERREX                                                            
*                                                                               
INIT50   MVC   SRPERNUM,0(R6)      SAVE PERIOD NUMBER                           
         MVC   SRACTPER,SRPERNUM                                                
         OI    SRACTPER,SIRKBUYQ                                                
         MVC   SIRKYEAR,SRSELYR    BUILD PERIOD KEY                             
         XI    SIRKYEAR,X'FF'      YEAR IS IN ONE'S COMPLEMENT                  
         GOTO1 ADATAMGR,DMCB,DMREAD,SPTDIR,KEY,KEY,0                            
         TM    DMCB+8,X'EF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    DMCB+8,X'10'        TEST PERIOD KEY FOUND                        
         BZ    *+12                YES                                          
         MVI   SRERROR,SRNOPER                                                  
         B     ERREX                                                            
*                                                                               
         GOTO1 ADATAMGR,DMCB,GETREC,SPTFIL,KEY+14,IO,DMWORK                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,EPDCODEQ     PERIOD DEFINITION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EPDELEM,R6                                                       
INIT60   CLC   SRPERNUM,EPDNUM     LOOK FOR MATCH ON PERIOD NUMBER              
         BE    INIT70                                                           
         BAS   RE,NEXTEL                                                        
         BE    INIT60                                                           
         MVI   SRERROR,SRNOPER     PERIOD IS NOT DEFINED                        
         B     ERREX                                                            
*                                                                               
INIT70   MVC   SRSTART,EPDSTART    SAVE PERIOD START DATE                       
         MVC   SREND,EPDEND        SAVE PERIOD END DATE                         
         MVC   SVPRUPFL,EPDUPFIL   SAVE PERIOD UPGRADE DATA                     
         MVC   SVPRUPGR,EPDUPGRD                                                
         MVC   SVPRUPFB(2),EPDUPFBK                                             
         MVC   SVPRUPIN,EPDUPINP                                                
         CLI   EPDLEN,EPDLENXQ     NEW ELEMENT FORMAT?                          
         BL    *+10                                                             
         MVC   SVPRUPFB+2(6),EPDUPMBK                                           
         DROP  R6                                                               
*                                                                               
         OC    SRSELMKS,SRSELMKS   TEST ANY MARKET/STATION GIVEN                
         BZ    READNSID            NO                                           
*                                                                               
         OC    SRSELSTA,SRSELSTA   TEST STATION GIVEN                           
         BZ    INIT90              NO, BUT MARKET WAS GIVEN                     
*                                                                               
         MVC   SRACTSTA,SRSELSTA                                                
         OC    SRSELMKT,SRSELMKT   TEST MARKET GIVEN                            
         BNZ   INIT90              YES                                          
*                                                                               
         CLI   SVUSEREP,C'Y'       TEST READ REP MARKETS                        
         BE    INIT80              YES                                          
*                                                                               
         LA    R3,IO                                                            
         USING STARECD,R3                                                       
         MVI   STAKTYPE,C'S'       BUILD STATION KEY                            
         MVC   STAKMED,SRSELMED                                                 
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         XC    0(32,R1),0(R1)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,SRSELAGY                                                 
         MVC   STAPMED,SRSELMED                                                 
         MVC   STAPCTRY,SRSELCTY                                                
         MVC   STAPACOM,SRACOM                                                  
         MVC   STAPMKST,SRSELMKS                                                
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   STAKCALL,STAPQSTA                                                
         DROP  R1                                                               
         CLI   STAKCALL+4,C' '                                                  
         BH    *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,SRSELAGY                                                 
         MVC   STAKCLT,=C'000'                                                  
         MVC   STAKFILL,=C'00000'                                               
*                                                                               
         GOTO1 ADATAMGR,DMCB,DMREAD,STATION,STAKEY,IO,0                         
         TM    DMCB+8,X'EF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    DMCB+8,X'10'        TEST STATION RECORD FOUND                    
         BZ    *+12                                                             
         MVI   SRERROR,SRNOSTA                                                  
         B     ERREX                                                            
*                                                                               
         PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STCM  R1,3,SRSELMKT                                                    
         B     INIT90                                                           
         DROP  R3                                                               
*                                                                               
INIT80   ICM   R3,15,SRAMASTC      A(MASTC)                                     
         BZ    INIT82              NOT GIVEN -- WE'RE ONLINE                    
         L     R3,MCUTL-MASTD(R3)  A(UTL)                                       
         MVC   SVUTLSYS,4(R3)      SAVE SPOT SENUM                              
         MVC   4(1,R3),SVREPSE     PUT REP SENUM IN UTL                         
         B     INIT85                                                           
*                                                                               
INIT82   GOTO1 ASWITCH,DMCB,=C'REP',0                                           
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
INIT85   LA    R3,WORK             READ PASSIVE POINTER                         
         USING RMKTRECD,R3                                                      
         XC    WORK,WORK                                                        
         MVI   RMKTSTYP,X'8B'                                                   
         MVC   RMKTSREP,SRSELAGY                                                
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         XC    0(32,R1),0(R1)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,SRSELAGY                                                 
         MVC   STAPMED,SRSELMED                                                 
         MVC   STAPCTRY,SRSELCTY                                                
         MVC   STAPACOM,SRACOM                                                  
         MVC   STAPMKST,SRSELMKS                                                
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   RMKTSSTA,STAPQSTA                                                
         DROP  R1                                                               
         MVI   RMKTSSTA+4,C' '                                                  
         MVC   WORK+32(32),WORK                                                 
*                                                                               
         GOTO1 ADATAMGR,DMCB,DMRDHI,REPDIR,WORK,WORK,0                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   WORK(23),WORK+32    TEST KEY FOUND                               
         BE    *+12                YES                                          
         MVI   SRERROR,SRNOSTA                                                  
         B     ERREX                                                            
*                                                                               
         PACK  DUB,RMKTSMKT                                                     
         CVB   R1,DUB                                                           
         STCM  R1,3,SRSELMKT                                                    
         DROP  R3                                                               
*                                                                               
         ICM   R3,15,SRAMASTC      A(MASTC)                                     
         BZ    INIT88              NOT GIVEN -- WE'RE ONLINE                    
         L     R3,MCUTL-MASTD(R3)  A(UTL)                                       
         MVC   4(1,R3),SVUTLSYS    SWITCH BACK TO SPOT SYSTEM                   
         B     INIT90                                                           
*                                                                               
INIT88   GOTO1 ASWITCH,DMCB,=C'SPOT',0                                          
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    INIT90              YES                                          
         DC    H'0'                                                             
*                                                                               
INIT90   MVC   SRACTMKT,SRSELMKT                                                
         EJECT                                                                  
READNSID XC    KEY,KEY             BUILD INITIAL KEY                            
         MVC   SIRKEY,SRACTKEY                                                  
*                                                                               
         GOTO1 ADATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY,0                            
         CLI   DMCB+8,0                                                         
         BE    RNSID10                                                          
         DC    H'0'                                                             
*                                                                               
RNSID5   GOTO1 ADATAMGR,DMCB,DMRSEQ,SPTDIR,KEY,KEY,0                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RNSID10  CLC   SIRKEY(4),SRACTKEY  TEST SAME TYPE/A-M/SCHEME                    
         BNE   RNSID20             NO                                           
*                                                                               
         OC    SRSELMKT,SRSELMKT   TEST MARKET GIVEN                            
         BZ    RNSID30             NO                                           
         CLC   SIRKMKT,SRSELMKT    TEST MARKET MATCH                            
         BNE   RNSID20             NO                                           
*                                                                               
         OC    SRSELSTA,SRSELSTA   TEST STATION GIVEN                           
         BZ    RNSID30             NO                                           
         CLC   SIRKSTA,SRSELSTA    TEST STATION MATCH                           
         BE    RNSID30             YES                                          
*                                                                               
RNSID20  MVI   SIINIT,0            START OVER NEXT TIME                         
         CLI   SRMODE,SRNORECS     GIVE USER APPROPRIATE MODE                   
         BE    XIT                                                              
         MVI   SRMODE,SRNOMORE                                                  
         B     XIT                                                              
*                                                                               
RNSID30  OC    SIRKMS,SIRKMS       TEST THERE'S A MKT AND STA                   
         BZ    RNSID5                                                           
         CLI   SIRKDPT,0           TEST THERE'S A DAYPART                       
         BE    RNSID5                                                           
         CLI   SIRKSEQ,0           TEST THERE'S NO SEQUENCE NO.                 
         BNE   RNSID5              RIGHT - IT'S A NSID RECORD                   
*                                                                               
         CLC   SRACTPER,SIRKMON    TEST SAME PERIOD                             
         BNE   RNSID5                                                           
*                                                                               
         MVC   BYTE,SRSELYR                                                     
         XI    BYTE,X'FF'                                                       
         CLC   BYTE,SIRKYEAR       TEST SAME YEAR                               
         BNE   RNSID5              NO                                           
*                                                                               
         OC    SRSELDPT,SRSELDPT   MUST WE FILTER ON DAYPART                    
         BZ    RNSID50             NO                                           
*                                                                               
         LA    R3,SRSELDPT-1       MULTIPLE DAYPART FILTERS                     
         LA    R0,8                MAXIMUM OF 8                                 
*                                                                               
RNSID40  LA    R3,1(R3)                                                         
         CLC   SIRKDPT,0(R3)       TEST MATCH ON DAYPART                        
         BE    RNSID50             YES                                          
         CLI   0(R3),C' '          TEST ANY MORE DAYPARTS IN LIST               
         BNH   RNSID5              NO                                           
         BCT   R0,RNSID40                                                       
*                                                                               
* OF COURSE OGILVY NEEDS MORE THAN THE ORIGINAL 8 *                             
*                                                                               
         OC    SRSELDP2,SRSELDP2   TEST ANY EXTENDED DAYPART FILTERS            
         BZ    RNSID5              NO - IGNORE THE RECORD                       
*                                                                               
         LA    R3,SRSELDP2-1       EXTENDED DAYPART FILTERS                     
         LA    R0,8                MAXIMUM OF 8                                 
*                                                                               
RNSID45  LA    R3,1(R3)                                                         
         CLC   SIRKDPT,0(R3)       TEST MATCH ON DAYPART                        
         BE    RNSID50             YES                                          
         CLI   0(R3),C' '          TEST ANY MORE DAYPARTS IN LIST               
         BNH   RNSID5              NO                                           
         BCT   R0,RNSID45                                                       
         B     RNSID5                                                           
*                                                                               
RNSID50  GOTO1 ADATAMGR,DMCB,GETREC,SPTFIL,KEY+14,IO,DMWORK                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    SRACTFLG,X'FF'-SRNOINVN                                          
         LA    R6,IO                                                            
         MVI   ELCODE,EDPCODEQ     LOOK FOR DAY/TIME/PROGTYPE ELEMENTS          
         BAS   RE,GETEL                                                         
         BE    RNSID55             RECORD CONTAINS INVENTORY                    
         TM    SRSELFLG,SRNOINV    CALLER WANTS EMPTY NSID RECORDS?             
         BZ    RNSID5                                                           
         OI    SRACTFLG,SRNOINVN                                                
         B     RNSID105            YES                                          
*                                                                               
RNSID55  LA    R2,SISQNUMS         TABLE OF SEQUENCE NOS.                       
         USING EDPELEM,R6                                                       
*                                                                               
RNSID60  CLI   SRSELDAY,0          TEST DAY FILTER GIVEN                        
         BE    *+14                NO                                           
         CLC   SRSELDAY,EDPDAY     TEST MATCH ON DAY                            
         BNE   RNSID100                                                         
*                                                                               
         OC    SRSELTIM,SRSELTIM   TEST TIME FILTER GIVEN                       
         BZ    RNSID70             NO                                           
         CLC   EDPSTRT,SRSELTIM    TEST START TIME                              
         BL    RNSID100            TOO EARLY                                    
         MVC   HALF,EDPENDT                                                     
         OC    HALF,HALF           TEST ANY END TIME IN ELEMENT                 
         BNZ   *+10                YES                                          
         MVC   HALF,EDPSTRT        NO - USE START TIME                          
         CLC   HALF,SRSELTIM+2                                                  
         BH    RNSID100            TOO LATE                                     
*                                                                               
RNSID70  OC    SRSELPRG,SRSELPRG   MUST WE FILTER ON PROGTYPE                   
         BZ    RNSID90             NO                                           
*                                                                               
         LA    R3,SRSELPRG-1       MULTIPLE PROGTYPE FILTERS                    
         LA    R0,8                MAXIMUM OF 8                                 
*                                                                               
RNSID80  LA    R3,1(R3)                                                         
         CLC   EDPPROG,0(R3)       TEST MATCH ON PROGTYPE                       
         BE    RNSID90             YES                                          
         CLI   0(R3),C' '          TEST ANY MORE PROGTYPES IN LIST              
         BNH   RNSID100            NO                                           
         BCT   R0,RNSID80                                                       
         B     RNSID100                                                         
*                                                                               
RNSID90  MVC   0(1,R2),EDPSEQ      PUT SEQUENCE NO. IN TABLE                    
         LA    R2,1(R2)                                                         
*                                                                               
RNSID100 BAS   RE,NEXTEL           NEXT DAY/TIME ELEMENT                        
         BE    RNSID60                                                          
         DROP  R6                                                               
*                                                                               
         OC    SISQNUMS,SISQNUMS   TEST ANY ELEMENTS TO USE                     
         BZ    RNSID5              NO                                           
*                                                                               
RNSID105 LA    R3,SVDPTS           DAYPART NAME TABLE                           
         LA    R2,ALPHANUM                                                      
*                                                                               
RNSID110 CLC   SIRKDPT,0(R2)       LOOK FOR DAYPART CODE IN TABLE               
         BE    *+16                GOT IT                                       
         LA    R2,1(R2)                                                         
         LA    R3,7(R3)                                                         
         B     RNSID110            TRY NEXT LETTER                              
         MVC   SRDPTNM,0(R3)       ACTUAL DAYPART NAME                          
*                                                                               
         MVC   SRACTKEY,SIRKEY     SAVE THE NSID KEY                            
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         XC    0(32,R1),0(R1)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,SRSELAGY                                                 
         MVC   STAPMED,SRSELMED                                                 
         MVC   STAPCTRY,SRSELCTY                                                
         MVC   STAPACOM,SRACOM                                                  
         MVC   STAPMKST,SRACTMKT                                                
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SRERMNO,STAPQMKT                                                 
         MVC   SRERSTAN,STAPQSTA                                                
         DROP  R1                                                               
*                                                                               
         TM    SRSELFLG,SRNSID     TEST RETURN NSID RECS, NOT DETAILS           
         BZ    READDET             NO                                           
*                                                                               
         LA    R6,IO               A(NSID RECORD)                               
         SR    R7,R7                                                            
         ICM   R7,3,IO+13          L'RECORD                                     
         LR    R1,R7                                                            
         L     R0,SRASIR           A(USER I/O AREA)                             
         MVCL  R0,R6               RETURN NSID RECORD TO USER                   
*                                                                               
         XC    SISQNUMS,SISQNUMS   SO THAT NEXT TIME. . .                       
         MVI   SRACTSEQ,X'FF'      . . . WE'LL GET THE NEXT NSID RECORD         
         MVI   SRMODE,SRONEREC     A RECORD WAS FOUND                           
*                                                                               
         OC    SRAHOOK,SRAHOOK     TEST USER HOOK GIVEN                         
         BZ    *+8                                                              
         BAS   RE,GOHOOK                                                        
         B     XIT                                                              
         EJECT                                                                  
READDET  LA    R2,SISQNUMS         SEQUENCE NOS.                                
*                                                                               
RDET10   CLI   0(R2),0             LOOK FOR NEXT NO. IN LIST                    
         BNE   *+12                                                             
         LA    R2,1(R2)                                                         
         B     RDET10                                                           
*                                                                               
         MVC   SRACTSEQ,0(R2)      PUT SEQUENCE NO. IN KEY                      
         MVI   0(R2),0             CLEAR ENTRY IN TABLE                         
         XC    KEY,KEY                                                          
         MVC   SIRKEY,SRACTKEY     THE DETAIL KEY                               
*                                                                               
         GOTO1 ADATAMGR,DMCB,DMREAD,SPTDIR,KEY,KEY,0                            
         TM    DMCB+8,X'EF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    DMCB+8,X'10'        TEST DETAIL KEY FOUND                        
         BO    GETNEXT             **** NO -- BAD NSID RECORD ****              
*                                                                               
         MVC   SRACTDIR,KEY                                                     
         GOTO1 ADATAMGR,DMCB,GETREC,SPTFIL,SRACTDA,SRASIR,DMWORK                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    SRSELFLG,SRNODETS   SHOULD WE RETURN ANY DETAIL DATA?            
         BO    RDET200             NO                                           
*                                                                               
         L     R6,SRASIR                                                        
         MVI   ELCODE,EDPCODEQ     DAY/TIME/PROGTYPE ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EDPELEM,R6                                                       
         MVC   SRACTDAY,EDPDAY     DAY CODE                                     
         XC    WORK,WORK                                                        
         GOTO1 SRADYUNP,DMCB,SRACTDAY,WORK                                      
         XC    SRERDAY,SRERDAY                                                  
         MVC   SRERDAY,WORK                                                     
*                                                                               
         MVC   SRACTTIM,EDPTIME    TIME (START AND END)                         
         XC    WORK,WORK                                                        
         GOTO1 SRAUNTIM,DMCB,SRACTTIM,WORK                                      
         XC    SRERTIM,SRERTIM                                                  
         MVC   SRERTIM,WORK                                                     
*                                                                               
         MVC   SRACTPRG,EDPPROG    ACTUAL PROGRAM TYPE CODE                     
         DROP  R6                                                               
*                                                                               
         XC    SRPRGNM,SRPRGNM                                                  
         CLI   SRACTPRG,0          TEST ANY PROGRAM TYPE                        
         BE    RDET30              NO                                           
         LA    R3,SVPGTYPS         PROGRAM TYPE NAME TABLE                      
         LA    R2,ALPHANUM                                                      
*                                                                               
RDET20   CLC   SRACTPRG,0(R2)      LOOK FOR PROGRAM TYPE IN TABLE               
         BE    *+16                GOT IT                                       
         LA    R2,1(R2)                                                         
         LA    R3,7(R3)                                                         
         B     RDET20              TRY NEXT LETTER                              
         MVC   SRPRGNM,0(R3)       ACTUAL PROGRAM TYPE NAME                     
*                                                                               
RDET30   XC    SRACTCST,SRACTCST                                                
         MVI   SRACTSLN,0                                                       
         MVC   SRACTEF1,SRSTART    FIRST EFF. DATE IS PERIOD START DATE         
         L     R6,SRASIR                                                        
         MVI   ELCODE,EECCODEQ     COST ELEMENT                                 
         BAS   RE,GETEL                                                         
         BNE   RDET40              NO COST ELEMENTS                             
*                                                                               
         USING EECELEM,R6                                                       
         CLI   SRSELSLN,0          TEST A SPOT LENGTH FILTER GIVEN              
         BE    RDET35              NO - USE 30'S (THESE ARE ALWAYS 1ST)         
*                                                                               
         CLC   EECSLN,SRSELSLN     TEST MATCH ON SPOT LENGTH                    
         BE    RDET35              GOT IT                                       
         BAS   RE,NEXTEL           NO - TRY NEXT ELEMENT                        
         BE    *-14                                                             
*                                                                               
         L     R6,SRASIR           BACK TO BEGINNING                            
         BAS   RE,GETEL            FIND 30'S AGAIN                              
*                                                                               
RDET35   MVC   SRACTCS1(25),EECCOST1   SAVE COSTS                               
         MVC   SRACTSLN,EECSLN     SAVE SPOT LENGTH                             
         DROP  R6                                                               
*                                                                               
RDET40   XC    SROVELEM,SROVELEM                                                
         CLI   SVUSECMP,C'Y'       TEST COMPETITION USER                        
         BNE   RDET45              NO                                           
         GOTO1 AHELLO,DMCB,(C'D',SPTFIL),(EDOCODEQ,SRASIR),0                    
         CLI   DMCB+12,0           TEST ERROR                                   
         BE    RDET50              NO - ALL DEMO ELEMS ARE NOW TOSSED           
         DC    H'0'                                                             
*                                                                               
RDET45   L     R6,SRASIR                                                        
         MVI   ELCODE,EDOCODEQ     DEMO OVERRIDE ELEMENTS                       
         BAS   RE,GETEL                                                         
         BNE   RDET50                                                           
         ST    R6,SROVELEM         A(FIRST OVERRIDE ELEMENT)                    
*                                                                               
RDET50   XC    SRUPDETS,SRUPDETS                                                
         L     R6,SRASIR                                                        
         MVI   ELCODE,EUPCODEQ     UPGRADE ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   RDET55              NOT THERE - MUST USE DEFAULT                 
*                                                                               
         USING EUPELEM,R6                                                       
         MVI   SRUPWHER,C'D'       UPGRADE IS FROM DETAIL RECORD                
         MVC   SRUPFILE,EUPUPFIL                                                
         MVC   SRUPEXP,EUPUPGRD                                                 
         MVC   SRUPBOOK,EUPUPFBK                                                
         MVC   SRUPDATA,EUPUPINP                                                
         CLI   EUPLEN,EUPLENXQ     NEW ELEMENT FORMAT?                          
         BL    *+10                                                             
         MVC   SRUPEXBK(6),EUPUPMBK                                             
         DROP  R6                                                               
*                                                                               
         L     R6,SRASIR                                                        
         MVI   ELCODE,EOVCODEQ     OVERRIDE ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   RDET80                                                           
*                                                                               
         USING EOVELEM,R6                                                       
         MVC   SRUPSTA,EOVUPSTA    STATION OVERRIDE                             
         CLI   EOVLEN,EOVLENXQ     NEW ELEMENT FORMAT?                          
         BL    *+10                                                             
         MVC   SRUPSTAX,EOVUPSTX                                                
         MVC   SRUPTIM,EOVUPTIM    TIME OVERRIDE                                
         MVC   SRUPDAY,EOVUPDAY    DAY OVERRIDE                                 
         B     RDET80                                                           
         DROP  R6                                                               
*                                                                               
RDET55   XC    KEY,KEY                                                          
         MVC   SIRKTYPE,SRACTTYP   BUILD STATION DEFAULT UPGRADE KEY            
         MVC   SIRKAM,SRACTAM                                                   
         MVC   SIRKCODE,SRACTSCH                                                
         MVC   SIRKMS,SRACTMKS                                                  
         MVC   SIRKMON,SRACTPER                                                 
         MVC   KEYSAVE(13),SIRKEY                                               
         GOTO1 ADATAMGR,DMCB,DMREAD,SPTDIR,KEY,KEY,0                            
         TM    DMCB+8,X'EF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    DMCB+8,X'10'        TEST KEY FOUND                               
         BZ    RDET60              YES                                          
*                                                                               
         XC    KEY,KEY             TRY MARKET DEFAULT UPGRADE                   
         MVC   SIRKEY,KEYSAVE                                                   
         XC    SIRKSTA,SIRKSTA                                                  
         GOTO1 ADATAMGR,DMCB,DMREAD,SPTDIR,KEY,KEY,0                            
         TM    DMCB+8,X'EF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    DMCB+8,X'10'        TEST WE HAVE IT                              
         BZ    RDET60              YES                                          
*                                                                               
         MVI   SRUPWHER,C'O'       UPGRADE IS AN OVERRIDE                       
         MVC   SRUPFILE,SVPRUPFL   PERIOD UPGRADE DATA                          
         MVC   SRUPEXP,SVPRUPGR                                                 
         MVC   SRUPBOOK,SVPRUPFB                                                
         MVC   SRUPEXBK(6),SVPRUPFB+2   EXTRA BOOKS FOR CANADA                  
         MVC   SRUPDATA,SVPRUPIN                                                
         B     RDET80                                                           
*                                                                               
RDET60   GOTO1 ADATAMGR,DMCB,GETREC,SPTFIL,KEY+14,IO,DMWORK                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,EUPCODEQ     UPGRADE ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EUPELEM,R6                                                       
         MVI   SRUPWHER,C'O'       UPGRADE IS AN OVERRIDE                       
         MVC   SRUPFILE,EUPUPFIL   UPGRADE DETAILS                              
         MVC   SRUPEXP,EUPUPGRD                                                 
         MVC   SRUPDATA,EUPUPINP                                                
*                                                                               
         CLI   SRUPEXP,3           TEST HUT OR PUT                              
         BNE   RDET70              NO, NEITHER ONE                              
         CLI   SRUPEXP+1,C'P'      TEST PUT                                     
         BNE   RDET70              NO, IT'S A HUT                               
*                                                                               
         ZIC   R0,SRSELYR          CURRENT YEAR                                 
         SR    R1,R1                                                            
         ICM   R1,8,SRUPEXP+2      RELATIVE PUT BOOK YEAR                       
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,SRUPEXP+2                                                     
*                                                                               
         MVC   SRUPDATA(4),=C'PUT/'                                             
         GOTO1 ADATCON,DMCB,(3,SRUPEXP+2),(6,SRUPDATA+4)                        
         MVC   SRUPDATA+7(2),SRUPDATA+8                                         
         MVI   SRUPDATA+9,C' '     FORMAT PUT BOOK                              
*                                                                               
RDET70   OC    EUPUPFBK,EUPUPFBK   TEST ANY SHARE BOOK                          
         BZ    RDET80                                                           
         MVC   SRUPBOOK,EUPUPFBK                                                
         ZIC   R0,SRSELYR          CURRENT YEAR                                 
         SR    R1,R1                                                            
         ICM   R1,8,SRUPBOOK       RELATIVE SHARE BOOK YEAR                     
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,SRUPBOOK                                                      
         DROP  R6                                                               
*                                                                               
RDET80   XC    SRACTCOM,SRACTCOM                                                
         L     R6,SRASIR                                                        
         MVI   ELCODE,ECOCODEQ     COMMENTS ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   RDET90                                                           
*                                                                               
         USING ECOELEM,R6                                                       
         ZIC   R1,ECOLEN                                                        
         SH    R1,=H'3'            INCLUDE BCTR AND OVERHEAD                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRACTCOM(0),ECOMMENT                                             
         DROP  R6                                                               
*                                                                               
RDET90   XC    SRACTPRO,SRACTPRO                                                
         L     R6,SRASIR                                                        
         MVI   ELCODE,EPRCODEQ     PROGRAMMING ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   *+14                                                             
*                                                                               
         USING EPRELEM,R6                                                       
         MVC   SRACTPRO,EPRTEXT    BEGIN WITH PROGRAMMING FROM DETAIL           
         OI    SRACTFLG,SRPROOVR   ASSUME IT'S AN OVERRIDE                      
         DROP  R6                                                               
*                                                                               
         CLC   SRACTMKT,SILSTMKT   TEST ALREADY HAVE MARKET DATA                
         BE    RDET100             YES                                          
*                                                                               
         MVC   SILSTMKT,SRACTMKT                                                
         CLI   SVUSEREP,C'Y'       TEST READ REP MARKET RECORDS                 
         BE    RDET95              YES                                          
*                                                                               
         LA    R3,IO               BUILD MARKET KEY                             
         USING MKTRECD,R3                                                       
         XC    MKTKEY,MKTKEY                                                    
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,SRSELMED                                                 
         MVC   MKTKMKT,SRERMNO                                                  
         MVC   MKTKAGY,SRSELAGY                                                 
         MVC   MKTKFILL,=C'000000000'                                           
*                                                                               
         GOTO1 ADATAMGR,DMCB,DMREAD,STATION,MKTKEY,IO,0                         
         TM    DMCB+8,X'EF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    DMCB+8,X'10'        TEST MARKET RECORD FOUND                     
         BZ    *+12                                                             
         MVI   SRERROR,SRNOMKT                                                  
         B     ERREX                                                            
*                                                                               
         MVC   SRERRNK,MKTRANK     MARKET RANK                                  
         MVC   SRERMNM,MKTNAME     MARKET NAME                                  
         B     RDET100                                                          
         DROP  R3                                                               
*                                                                               
RDET95   ICM   R3,15,SRAMASTC      A(MASTC)                                     
         BZ    RDET96              NOT GIVEN -- WE'RE ONLINE                    
         L     R3,MCUTL-MASTD(R3)  A(UTL)                                       
         MVC   SVUTLSYS,4(R3)      SAVE SPOT SENUM                              
         MVC   4(1,R3),SVREPSE     PUT REP SENUM IN UTL                         
         B     RDET97                                                           
*                                                                               
RDET96   GOTO1 ASWITCH,DMCB,=C'REP',0                                           
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
RDET97   LA    R3,WORK             READ MARKET KEY                              
         USING RMKTRECD,R3                                                      
         XC    WORK,WORK                                                        
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,SRSELAGY                                                
         MVC   RMKTKMKT,SRERMNO                                                 
*                                                                               
         GOTO1 ADATAMGR,DMCB,DMREAD,REPDIR,WORK,WORK,0                          
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         MVI   SRERROR,SRNOMKT                                                  
         B     ERREX                                                            
*                                                                               
         GOTO1 ADATAMGR,DMCB,GETREC,REPFIL,WORK+28,IO,DMWORK                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,IO                                                            
         MVC   SRERMNM(L'RMKTNAME),RMKTNAME                                     
         DROP  R3                                                               
*                                                                               
         ICM   R3,15,SRAMASTC      A(MASTC)                                     
         BZ    RDET98              NOT GIVEN -- WE'RE ONLINE                    
         L     R3,MCUTL-MASTD(R3)  A(UTL)                                       
         MVC   4(1,R3),SVUTLSYS    SWITCH BACK TO SPOT SYSTEM                   
         B     RDET110                                                          
*                                                                               
RDET98   GOTO1 ASWITCH,DMCB,=C'SPOT',0                                          
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    RDET110             YES                                          
         DC    H'0'                                                             
*                                                                               
RDET100  CLI   SVUSEREP,C'Y'       TEST READ REP MARKET RECORDS                 
         BE    RDET110             YES - DON'T READ SPOT STATION RECORD         
*                                                                               
         CLC   SRACTSTA,SILSTSTA   TEST ALREADY HAVE STATION DATA               
         BE    RDET110             YES                                          
*                                                                               
         MVC   SILSTSTA,SRACTSTA                                                
         LA    R3,IO                                                            
         USING STARECD,R3                                                       
         MVI   STAKTYPE,C'S'       BUILD STATION KEY                            
         MVC   STAKMED,SRSELMED                                                 
         MVC   STAKCALL,SRERSTAN                                                
         CLI   STAKCALL+4,C' '                                                  
         BH    *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,SRSELAGY                                                 
         MVC   STAKCLT,=C'000'                                                  
         MVC   STAKFILL,=C'00000'                                               
*                                                                               
         GOTO1 ADATAMGR,DMCB,DMREAD,STATION,STAKEY,IO,0                         
         TM    DMCB+8,X'EF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    DMCB+8,X'10'        TEST STATION RECORD FOUND                    
         BZ    *+12                                                             
         MVI   SRERROR,SRNOSTA                                                  
         B     ERREX                                                            
*                                                                               
         CLI   SRERSTAN,X'F0'      IS THIS A CABLE HEADEND?                     
         BNL   RDET110             YES                                          
         MVC   SRERAFF,SNETWRK     PICK UP NETWORK AFFILIATION                  
         DROP  R3                                                               
*                                                                               
RDET110  MVI   SRCMPSRC,0                                                       
         CLI   SVUSECMP,C'Y'       TEST COMPETITION USER                        
         BNE   RDET200             NO                                           
*                                                                               
         CLI   SRERSTAN,X'F0'      LOCAL CABLE?                                 
         BNL   RDET200             YES -- DON'T BOTHER WITH COMPETITION         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY              BUILD COMPETITION KEY                        
         USING COMPRECD,R6                                                      
*                                                                               
         MVI   CMPKTYP,CMPKTYPQ    RECORD TYPE                                  
         MVI   CMPKFIL,CMPKFILQ                                                 
         MVC   CMPKAM,SRACTAM      A/M                                          
         ZIC   R1,SRACTYR                                                       
         X     R1,=X'000000FF'     MAKE YEAR POSITIVE                           
         CVD   R1,DUB                                                           
         MVC   CMPKYM,DUB+7        ONE'S DIGIT OF YEAR                          
         MVN   CMPKYM,SRACTPER     PERIOD NUMBER                                
         MVC   CMPKCODE,SRACTSCH   SCHEME                                       
         MVC   CMPKMKT,SRACTMKT    MARKET                                       
         MVC   CMPKDAY,SRACTDAY    DAY CODE                                     
         MVC   CMPKSTIM,SRACTTIM   START TIME                                   
         MVC   CMPKETIM,SRACTTIM+2 END TIME                                     
         DROP  R6                                                               
*                                                                               
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 ADATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY,0                            
         CLC   KEY(13),KEYSAVE     TEST COMPETITION KEY IS THERE                
         BNE   RDET200             NO                                           
*                                                                               
         GOTO1 ADATAMGR,DMCB,GETREC,SPTFIL,KEY+14,IO2,DMWORK                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO2              BEGINNING OF COMP RECORD                     
         MVI   ELCODE,CMECODEQ     LOOK FOR EXTRA INFORMATION ELEMENT           
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING CMEEL,R6                                                         
         MVC   SRCMPSRC,CMESRCE    RATING SOURCE USED IN COMPETITION            
         DROP  R6                                                               
*                                                                               
         LA    R6,IO2              BEGINNING OF COMP RECORD                     
         MVI   ELCODE,CMSCODEQ     LOOK FOR STATION ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,1                LOOK THROUGH UP TO 20 STATIONS               
         LA    R6,2(R6)            BUMP TO FIRST STATION                        
         MVC   WORK(5),SRERSTAN                                                 
         CLI   WORK+4,C' '                                                      
         BH    *+8                                                              
         MVI   WORK+4,C'T'                                                      
*                                                                               
RDET120  CLC   WORK(5),0(R6)       TEST MATCH ON STATION                        
         BE    RDET130             YES                                          
         LA    R6,5(R6)            BUMP TO NEXT STATION                         
         LA    R3,1(R3)            R3 = STATION INDEX NUMBER                    
         CH    R3,=H'21'           TEST AGAINST MAXIMUM                         
         BNE   RDET120             NOT THERE YET                                
         B     RDET200             STATION IS NOT IN ELEMENT                    
*                                                                               
RDET130  STC   R3,STAINDEX         HANG ON TO STATION INDEX                     
         LA    R6,IO2              BEGINNING OF COMP RECORD                     
         MVI   ELCODE,CMPCODEQ     LOOK FOR PROGRAMMING ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMPEL,R6                                                         
RDET140  MVC   BYTE,CMPSEQ         STATION INDEX                                
         NI    BYTE,X'7F'          TURN OFF OVERRIDE BIT IF IT'S ON             
         CLC   STAINDEX,BYTE       TEST PROGRAM FOR THIS STATION                
         BE    *+14                YES                                          
         BAS   RE,NEXTEL           NO - TRY NEXT                                
         BE    RDET140                                                          
         DC    H'0'                NO PROGRAM FOR THIS STATION                  
*                                                                               
         CLC   CMPPROG,=17X'FF'    TEST ELEMENT WAS NEVER INITIALIZED           
         BE    RDET145             RIGHT - DON'T USE THIS                       
         MVC   SRACTPRO,CMPPROG    RETURN PROGRAMMING FROM COMPETITION          
         OI    SRACTFLG,SRPROOVR   ASSUME IT'S AN OVERRIDE                      
         TM    CMPSEQ,X'80'        TEST PROGRAMMING IS AN OVERRIDE              
         BO    RDET145             YES                                          
         NI    SRACTFLG,X'FF'-SRPROOVR                                          
         DROP  R6                                                               
*                                                                               
RDET145  LA    R6,IO2              BEGINNING OF COMP RECORD                     
         MVI   ELCODE,CMDCODEQ     LOOK FOR DEMO ELEMENTS                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R2,STAINDEX                                                      
         BCTR  R2,0                                                             
         MH    R2,=H'3'                                                         
         LA    R2,4(R2)            R2 = DISP TO STATION INDEX                   
*                                                                               
         LA    R3,WORK             BUILD A SIR DEMO ELEMENT IN WORK             
         USING EDOELEM,R3                                                       
         MVI   EDOLEN,EDOLENEQ     LENGTH                                       
*                                                                               
RDET150  MVC   EDODEMO,2(R6)       DEMO TYPE                                    
         ST    R6,FULL             SAVE ELEMENT POINTER                         
         AR    R6,R2               A(STATION INDEX)                             
         MVC   BYTE,0(R6)          STATION INDEX                                
         NI    BYTE,X'7F'          TURN OFF OVERRIDE BIT IF IT'S ON             
         CLC   STAINDEX,BYTE       TEST MATCH ON STATION                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   =X'FFFF',1(R6)      TEST ANY VALUE GIVEN                         
         BE    RDET160             NO                                           
         MVI   EDOCODE,EDOCODEQ    ASSUME DEMO IS AN OVERRIDE                   
         TM    0(R6),X'80'         TEST VALUE IS AN OVERRIDE                    
         BO    *+8                 YES                                          
         MVI   EDOCODE,EDVCODEQ    NO, DEMO WAS CALCULATED                      
         MVC   EDOVALUE,1(R6)      SAVE VALUE - ADD ELEMENT TO DETAIL           
*                                                                               
         GOTO1 AHELLO,DMCB,(C'P',SPTFIL),SRASIR,WORK                            
         CLI   DMCB+12,0           TEST ERROR                                   
         BE    *+6                 NO                                           
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
RDET160  L     R6,FULL             RESTORE ELEMENT POINTER                      
         BAS   RE,NEXTEL           LOOK FOR MORE DEMO ELEMENTS                  
         BE    RDET150             PROCESS THEM                                 
*                                                                               
         L     R6,SRASIR           BEGINNING OF DETAIL RECORD                   
         MVI   ELCODE,EDOCODEQ     LOOK FOR DEMO OVERRIDE ELEMENTS              
         BAS   RE,GETEL                                                         
         BNE   *+12                NONE WERE ADDED                              
         ST    R6,SROVELEM         A(FIRST FAKED-OUT DEMO OVERRIDE)             
         B     RDET200                                                          
*                                                                               
         L     R6,SRASIR           BEGINNING OF DETAIL RECORD                   
         MVI   ELCODE,EDVCODEQ     LOOK FOR CALCULATED DEMOS                    
         BAS   RE,GETEL                                                         
         BNE   RDET200                                                          
         ST    R6,SROVELEM         A(FIRST FAKED-OUT CALCULATED DEMO)           
*                                                                               
RDET200  MVI   SRMODE,SRONEREC     A RECORD WAS FOUND                           
         OC    SRAHOOK,SRAHOOK     TEST USER HOOK GIVEN                         
         BZ    RDETX               NO                                           
         BAS   RE,GOHOOK                                                        
         B     RDETX                                                            
*                                                                               
GOHOOK   NTR1                      SAVE MY REGISTERS                            
         L     RF,SRAHOOK          A(USER HOOK)                                 
         L     RE,USERRD           USER'S RD                                    
         LM    R0,RC,20(RE)        RESTORE USER'S R0-RC                         
         BASR  RE,RF               GOTO USER HOOK                               
         XIT1                                                                   
*                                                                               
RDETX    B     XIT                                                              
         EJECT                                                                  
ERREX    L     RD,SAVERD           BACK ALL THE WAY OUT                         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
DMRDHI   DC    C'DMRDHI  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMREAD   DC    C'DMREAD  '                                                      
GETREC   DC    C'GETREC  '                                                      
STATION  DC    C'STATION '                                                      
SPTDIR   DC    C'SPTDIR  '                                                      
SPTFIL   DC    C'SPTFIL  '                                                      
REPDIR   DC    C'REPDIR  '                                                      
REPFIL   DC    C'REPFIL  '                                                      
CTFILE   DC    C'CTFILE  '                                                      
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* DSECT FOR RANSID                                                              
*                                                                               
MODD     DSECT                                                                  
DUB      DS    D                                                                
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
FULL     DS    F                                                                
SAVERD   DS    A                                                                
USERRD   DS    A                                                                
ADATAMGR DS    A                   A(DATAMGR)                                   
ADATCON  DS    A                   A(DATCON)                                    
AGETPROF DS    A                   A(GETPROF)                                   
AHELLO   DS    A                   A(HELLO)                                     
ASWITCH  DS    A                   A(SWITCH)                                    
VSTAPACK DS    A                   A(STAPACK)                                   
HALF     DS    H                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
STAINDEX DS    X                   STATION NUMBER IN COMPETITION RECS           
SVUTLSYS DS    X                                                                
WORK     DS    XL64                                                             
STAWORK  DS    XL32                                                             
KEY      DS    XL32                                                             
KEYSAVE  DS    XL18                                                             
IO       DS    CL4000              GENERAL PURPOSE IO AREA                      
IO2      DS    CL4000              FOR COMPETITION RECORDS                      
MODDX    EQU   *                                                                
         EJECT                                                                  
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         EJECT                                                                  
COMPRECD DSECT                                                                  
       ++INCLUDE SPGENCOMP                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDREPMASTD                                                     
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
RMKTRECD DSECT                                                                  
       ++INCLUDE REGENMKT                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
RANSIDD  DSECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
         EJECT                                                                  
         ORG   SRRESERV                                                         
SIINIT   DS    X                   RANSID INITIALIZATION SWITCH                 
SILSTMKT DS    XL2                 LAST MARKET READ                             
SILSTSTA DS    XL3                 LAST STATION READ                            
SISQNUMS DS    XL255               SEQUENCE NUMBERS                             
*                                                                               
SVDPTS   DS    CL252               DAYPART NAMES (A..Z,0..9)                    
SVPGTYPS DS    CL182               PRGTYPE NAMES (A..Z)                         
SVPRUPFL DS    C                   PERIOD UPGRADE FILE (T OR P)                 
SVPRUPGR DS    XL8                 PERIOD UPGRADE EXPRESSION                    
SVPRUPFB DS    XL8                 PERIOD OVERRIDE FROM BOOK(S)                 
SVPRUPIN DS    CL16                PERIOD UPGRADE INPUT DATA                    
*                                                                               
SVUSECMP DS    C                   'Y' IF COMPETITION/TREND USER                
SVUSEREP DS    C                   'Y' IF USE REP MARKETS                       
SVREPSE  DS    X                   SENUM OF REP SYSTEM FOR THIS REP             
         DS    XL(L'SRRESERV-(*-SRRESERV))                                      
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPRANSID  05/01/02'                                      
         END                                                                    
