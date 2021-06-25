*          DATA SET GERLP02    AT LEVEL 010 AS OF 06/22/07                      
*PHASE TF2D02A                                                                  
                                                                                
***********************************************************************         
* MODULE HANDLES THE FOLLOWING FUNCTIONS:-                            *         
*                                                                     *         
* REQUEST/LIST    USES X'FB' SCREEN - RLI PREFIXES - CODE=REQLST      *         
* REQUEST/DISPLAY SCREEN BUILT DYNAMICALLY         - CODE=REQDIS      *         
* REQUEST/DELETE  USES LIST LINE DSECT (LSTD)      - CODE=REQDEL      *         
* REQUEST/MOVE    USES LIST LINE DSECT (LSTD)      - CODE=REQMOV      *         
* REQUEST/COPY    USES LIST LINE DSECT (LSTD)      - CODE=REQCPY(MOV) *         
* REQUEST/GLOBAL  USES X'FA' SCREEN - RGL PREFIXES - CODE=REQGLO      *         
*                                                                     *         
***********************************************************************         
                                                                                
RLP02    TITLE '- REQUEST RECORD HANDLING'                                      
RLP02    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 50,**RLP2**,RR=RE,CLEAR=YES                                      
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA & SAVE STORAGE)                     
         USING LSTTABD,CSLSTCUR                                                 
         L     R3,ARFPIOB                                                       
         USING RFPD,R3             R3=A(RFP INTERFACE BLOCK)                    
         USING RQHD,RFPVREQH                                                    
         BASR  R7,0                                                             
         AHI   R7,GLOBALS-*                                                     
         USING GLOBALS,R7                                                       
         ST    RE,OVRELO                                                        
         ST    RB,OVBASE1                                                       
                                                                                
         L     R1,AMIXNTRY                                                      
         SRL   RF,32-8                                                          
         LTR   RF,RF                                                            
         BNZ   REQNTRY                                                          
         EJECT                                                                  
***********************************************************************         
* REGULAR ENTRY POINT FOR RECORD/ACTION                               *         
***********************************************************************         
                                                                                
         ICM   RF,1,MIXROUT-MIXTABD(R1)                                         
         CHI   RF,REQROUTM-1                                                    
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     REQROUTS(RF)                                                     
                                                                                
REQROUTS DS    0XL4                                                             
                                                                                
         J     REQLST              REQUEST/LIST                                 
         J     REQDIS              REQUEST/DISPLAY                              
         J     REQDEL              REQUEST/CHANGE                               
         J     REQMOV              REQUEST/MOVE     (TO GROUP)                  
         J     REQCPY              REQUEST/COPY     (TO GROUP)                  
         J     REQGLO              REQUEST/GLOBAL   (CHANGE)                    
                                                                                
REQROUTM EQU   (*-REQROUTS)/L'REQROUTS                                          
                                                                                
***********************************************************************         
* RETURN TO POINT AFTER NTRSES                                        *         
***********************************************************************         
                                                                                
REQNTRY  CHI   RF,REQNTRYM                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     REQNTRYS-L'REQNTRYS(RF)                                          
                                                                                
REQNTRYS DS    0XL4                                                             
                                                                                
         J     RLCHARET            RETURN FROM REQUEST/CHANGE & COPY            
         J     RLDELRET            RETURN FROM REQUEST/DELETE                   
         J     RLDISRET            RETURN FROM REQUEST/DISPLAY                  
         J     RLGLORET            RETURN FROM REQUEST/GLOBAL                   
         J     RLCALRET            RETURN FROM GROUP/CALENDAR                   
         J     RLBLDRET            RETURN FROM GROUP/BUILD                      
                                                                                
REQNTRYM EQU   (*-REQNTRYS)/L'REQNTRYS                                          
                                                                                
XITSESN  OI    TWARLI1,TWARLIRN    SET REFRESH LIST NOW & RETURN                
                                                                                
XITSES   GOTOR AXITSES                                                          
         J     EXIT                                                             
                                                                                
EXITL    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITH    DS    0H                                                               
EXITN    LHI   RE,2                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY A REQUEST IN FULL                                *         
***********************************************************************         
                                                                                
         USING RDWORKD,RC                                                       
REQDIS   BASE  ,                                                                
         MVI   TWASCRN,0           SET NO SCREEN LOADED                         
         MVC   CUUSER,REQLUSER                                                  
         MVC   CUAALF,REQLAGY                                                   
         MVC   CUSYSL,REQLSYST                                                  
         GOTOR AVALGRP,REQLGRP                                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     XITSESN                                                          
                                                                                
         MVC   RFPFRQID,REQLRQID                                                
         MVC   RFPFSORT,REQLSORT                                                
         MVC   RFPFSEQN,REQLSEQN                                                
         OI    RFPFFLAG,RFPF1STR                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPRETRQ                                                 
         GOTOR VRFPIO,PCPARM,RFPD                                               
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     XITSESN                                                          
                                                                                
         CLI   RFPRET,RFPEOF       TEST END OF REQUESTS                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     XITSESN                                                          
                                                                                
         CLC   RLLGNUMR,RFPVNREQ   TEST NUMBER OF REQUESTS CHANGED              
         JE    *+14                                                             
         MVC   RLLGNUMR,RFPVNREQ   YES - SET CURRENT COUNT                      
         J     XITSESN             AND REFRESH REQUEST LIST                     
                                                                                
         LA    R1,RLPOLY1H                                                      
         USING FVIHDR,R1           R1=A(DISPLAY POINT IN TWA)                   
         ST    R1,RDANEXT                                                       
                                                                                
         XC    0(L'FVIHDR+DSCLINL,R1),0(R1)                                     
         LHI   R0,L'FVIHDR+DSCLINL                                              
         STC   R0,FVTLEN                                                        
         MVI   FVATRB,FVAPROT                                                   
         LHI   R0,(03*80)+1                                                     
         STCM  R0,3,FVABSA                                                      
         OI    FVOIND,FVOXMT                                                    
         LA    R5,FVIFLD           R5 POINTS TO DISPLAY LINE 1                  
         LA    R0,FVIFLD+DSCLINL                                                
         ST    R0,RDANEXT                                                       
         TM    RFPVSTAT,RFPVSPOF+RFPVRUNR                                       
         BNZ   REQDIS02                                                         
         LA    R4,FVIFLD           R4 POINTS TO DISPLAY LINE 1                  
                                                                                
         AHI   R1,L'FVIHDR+DSCLINL                                              
         XC    0(L'FVIHDR+DSCLINL,R1),0(R1)                                     
         LHI   R0,L'FVIHDR+DSCLINL                                              
         STC   R0,FVTLEN                                                        
         MVI   FVATRB,FVAPROT                                                   
         LHI   R0,(04*80)+1                                                     
         STCM  R0,3,FVABSA                                                      
         OI    FVOIND,FVOXMT                                                    
         LA    R5,FVIFLD           R5 POINTS TO DISPLAY LINE 2                  
                                                                                
         LA    R0,FVIFLD+DSCLINL                                                
         ST    R0,RDANEXT                                                       
         DROP  R1                                                               
                                                                                
         MVC   0(L'PCMCPY,R4),PCMCPY                                            
         AHI   R4,L'PCMCPY-1                                                    
         CLI   0(R4),SPACE                                                      
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   1(L'PCEQUAL,R4),PCEQUAL                                          
         AHI   R4,1+L'PCEQUAL                                                   
         MVC   0(L'REQLAGY,R4),REQLAGY                                          
         AHI   R4,L'REQLAGY                                                     
         MVC   0(L'PCCOMMA,R4),PCCOMMA                                          
         AHI   R4,L'PCCOMMA                                                     
                                                                                
         MVC   0(L'PCMUSRID,R4),PCMUSRID                                        
         AHI   R4,L'PCMUSRID-1                                                  
         CLI   0(R4),SPACE                                                      
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   1(L'PCEQUAL,R4),PCEQUAL                                          
         AHI   R4,1+L'PCEQUAL                                                   
         GOTOR AGETUID,REQLUSER                                                 
         MVC   0(L'GIDCODE,R4),PCWORK+(GIDCODE-GIDTABD)                         
         AHI   R4,L'GIDCODE-1                                                   
         CLI   0(R4),SPACE                                                      
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   1(L'PCCOMMA,R4),PCCOMMA                                          
         AHI   R4,L'PCCOMMA+1                                                   
                                                                                
         MVC   0(L'PCMSYSTM,R4),PCMSYSTM                                        
         AHI   R4,L'PCMSYSTM-1                                                  
         CLI   0(R4),SPACE                                                      
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   1(L'PCEQUAL,R4),PCEQUAL                                          
         AHI   R4,1+L'PCEQUAL                                                   
         GOTOR AGETSYS,REQLSYST                                                 
         MVC   0(L'SYSLNAME,R4),PCWORK                                          
         AHI   R4,L'SYSLNAME-1                                                  
         CLI   0(R4),SPACE                                                      
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   1(L'PCCOMMA,R4),PCCOMMA                                          
         AHI   R4,L'PCCOMMA+1                                                   
                                                                                
         MVC   0(L'PCMGROUP,R4),PCMGROUP                                        
         AHI   R4,L'PCMGROUP-1                                                  
         CLI   0(R4),SPACE                                                      
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   1(L'PCEQUAL,R4),PCEQUAL                                          
         AHI   R4,1+L'PCEQUAL                                                   
         MVC   0(L'REQLGRP,R4),REQLGRP                                          
                                                                                
REQDIS02 OC    REQLOTYP,REQLOTYP                                                
         BZ    REQDIS04                                                         
         MVC   0(L'PCMOTYP,R5),PCMOTYP                                          
         AHI   R5,L'PCMOTYP-1                                                   
         CLI   0(R5),SPACE                                                      
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(L'PCEQUAL,R5),PCEQUAL                                          
         AHI   R5,1+L'PCEQUAL                                                   
         MVC   0(L'REQLOTYP,R5),REQLOTYP                                        
         AHI   R5,L'REQLOTYP-1                                                  
         CLI   0(R5),SPACE                                                      
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(L'PCCOMMA,R5),PCCOMMA                                          
         AHI   R5,L'PCCOMMA+1                                                   
                                                                                
REQDIS04 OC    REQLDEST,REQLDEST                                                
         BZ    REQDIS08                                                         
         MVC   0(L'PCMDSTID,R5),PCMDSTID                                        
         AHI   R5,L'PCMDSTID-1                                                  
         CLI   0(R5),SPACE                                                      
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(L'PCEQUAL,R5),PCEQUAL                                          
         AHI   R5,1+L'PCEQUAL                                                   
         OC    REQLFXID,REQLFXID   TEST FAX ID PRESENT                          
         BZ    REQDIS06                                                         
         MVC   0(L'PCUFAXP,R5),PCUFAXP                                          
         AHI   R5,L'PCUFAXP                                                     
         MVC   0(L'REQLFXID,R5),REQLFXID                                        
         AHI   R5,L'REQLFXID-1                                                  
         CLI   0(R5),SPACE                                                      
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(L'PCCOMMA,R5),PCCOMMA                                          
         AHI   R5,L'PCCOMMA+1                                                   
         B     REQDIS08                                                         
                                                                                
REQDIS06 GOTOR AGETUID,REQLDEST                                                 
         MVC   0(L'GIDCODE,R5),PCWORK+(GIDCODE-GIDTABD)                         
         AHI   R5,L'GIDCODE-1                                                   
         CLI   0(R5),SPACE                                                      
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(L'PCCOMMA,R5),PCCOMMA                                          
         AHI   R5,L'PCCOMMA+1                                                   
                                                                                
REQDIS08 CLC   REQLRDSC,PCSPACES                                                
         BNH   REQDIS10                                                         
         MVC   0(L'PCMDESC,R5),PCMDESC                                          
         AHI   R5,L'PCMDESC-1                                                   
         CLI   0(R5),SPACE                                                      
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(L'PCEQUAL,R5),PCEQUAL                                          
         AHI   R5,1+L'PCEQUAL                                                   
         MVC   0(L'REQLRDSC,R5),REQLRDSC                                        
         AHI   R5,L'REQLRDSC-1                                                  
         CLI   0(R5),SPACE                                                      
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(L'PCCOMMA,R5),PCCOMMA                                          
         AHI   R5,L'PCCOMMA+1                                                   
                                                                                
REQDIS10 MVC   0(L'PCMRUNQ,R5),PCMRUNQ                                          
         AHI   R5,L'PCMRUNQ-1                                                   
         CLI   0(R5),SPACE                                                      
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(L'PCEQUAL,R5),PCEQUAL                                          
         AHI   R5,1+L'PCEQUAL                                                   
         LA    RE,PCMYES                                                        
         CLI   RFPVREQD,REQLRSRD                                                
         BNE   *+8                                                              
         LA    RE,PCMNO                                                         
         MVC   0(L'PCMYES,R5),0(RE)                                             
                                                                                
REQDIS12 LHI   R0,X'80'                                                         
         TM    RFPVSTAT,RFPVSPOF   TEST SPOOF REQUEST                           
         BZ    *+12                                                             
         LHI   R0,X'C0'                                                         
         B     REQDIS14                                                         
         TM    RFPVSTAT,RFPVRUNR   TEST RUNNER REQUEST                          
         BZ    *+12                                                             
         LHI   R0,X'90'                                                         
         B     REQDIS14                                                         
                                                                                
         CLI   RLODATE,0           TEST DATES=REAL SPECIFIED                    
         BE    REQDIS14                                                         
         GOTOR AGETNXT             TEST NEXT RUN DATE FOR GROUP SET             
         BNE   REQDIS14                                                         
         GOTOR VDATCON,PCPARM,(6,PCWORK),(0,PCDUB)                              
         GOTOR VREQRFP,PCPARM,(X'20',RFPVREQH),(RFPFSYS,ACOM),         *        
               (CULANG,0),(2,0),(X'FF',ARFPBUFF),PCDUB                          
                                                                                
REQDIS14 GOTOR VREQRFP,PCPARM,((R0),RFPVREQH),(RFPFSYS,ACOM),          *        
               (CULANG,RDANEXT),ATWA,(X'FF',ARFPBUFF)                           
                                                                                
         LA    R1,RLPOLY1H                                                      
         USING FVIHDR,R1           LOCATE END OF TWA                            
         LA    RF,TWALAST                                                       
         SR    RE,RE                                                            
REQDIS16 ICM   RE,1,FVTLEN         LOOK FOR END OF TWA OR LINE 24               
         BZ    REQDIS18                                                         
         LHI   R0,(23*80)+1                                                     
         CLM   R0,3,FVABSA                                                      
         BNH   REQDIS18                                                         
         BXLE  R1,RE,REQDIS16                                                   
                                                                                
REQDIS18 XC    0(L'FVIHDR+PFKLINL+L'FVIXHDR,R1),0(R1)                           
         LHI   R0,L'FVIHDR+PFKLINL+L'FVIXHDR                                    
         STC   R0,FVTLEN                                                        
         MVI   FVATRB,FVAPROT+FVAHIGH+FVAXTND                                   
         LHI   R0,(23*80)+1                                                     
         STCM  R0,3,FVABSA                                                      
         OI    FVOIND,FVOXMT                                                    
         AHI   R1,L'FVIHDR+PFKLINL                                              
         USING FVIXHDR,R1                                                       
         MVI   FVIXNU,255                                                       
         DROP  R1                                                               
         AHI   R1,L'FVIHDR                                                      
         MVI   0(R1),0                                                          
         MVI   1(R1),1                                                          
         MVI   2(R1),1                                                          
                                                                                
         MVC   FVMSGNO,=AL2(GI$RDENR)                                           
         MVI   FVOMTYP,GTMINF                                                   
         J     EXIT                                                             
         DROP  RB,RC                                                            
                                                                                
DSCLINL  EQU   78                  WIDTH OF DESCRIPTION LINE                    
PFKLINL  EQU   78                  WIDTH OF PFKEY LINE                          
                                                                                
RDWORKD  DSECT                     ** REQUEST/DISPLAY LOCAL W/S **              
RDANEXT  DS    A                   A(NEXT OUTPUT TWA FIELD)                     
RLP02    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A REQUEST                                         *         
***********************************************************************         
                                                                                
REQDEL   BASE  ,                                                                
         MVC   CUUSER,REQLUSER                                                  
         MVC   CUAALF,REQLAGY                                                   
         MVC   CUSYSL,REQLSYST                                                  
         LA    R1,REQLGRP                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     XITSES                                                           
                                                                                
         MVC   RFPFRQID,REQLRQID                                                
         MVC   RFPFSORT,REQLSORT                                                
         MVC   RFPFSEQN,REQLSEQN                                                
         OI    RFPFFLAG,RFPF1STR                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPRETRQ                                                 
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     XITSES                                                           
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
                                                                                
         CLI   RFPRET,RFPEOF       TEST END OF REQUESTS                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     XITSES                                                           
                                                                                
         MVI   RFPMODE,RFPDELRQ                                                 
         GOTOR (RF)                                                             
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     XITSES                                                           
         OI    TWARLI1,TWARLIRL    SET REFRESH LIST PENDING                     
         SR    R1,R1                                                            
         ICM   R1,1,RLLGNUMR                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         STC   R1,RLLGNUMR                                                      
         J     XITSES                                                           
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COPY OR MOVE REQUEST TO ANOTHER GROUP                    *         
* NTRY - OVWORK1 CONTAINS 'COPY TO' OR 'MOVE TO' GROUP CODE           *         
***********************************************************************         
                                                                                
         USING RMWORKD,RC                                                       
REQCPY   MVI   RMFLAG,RMFCOPY      SET 'COPY TO' ACTION                         
         J     REQMOV00                                                         
                                                                                
REQMOV   MVI   RMFLAG,RMFMOVE      SET 'MOVE TO' ACTION                         
                                                                                
REQMOV00 BASE  ,                                                                
         ICM   R6,15,OVADDR1       POINT TO CURRENT TWA LINE                    
         USING LSTD,R6                                                          
         LA    R0,LSTACTH                                                       
         ST    R0,FVADDR                                                        
                                                                                
         CLC   REQLGRP,OVWORK1     TEST MOVING/COPYING TO SAME GROUP            
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$CCRSG)                                           
         B     REQMOVX                                                          
                                                                                
         MVC   CUUSER,REQLUSER     VALIDATE THE 'MOVE/COPY TO' GROUP            
         MVC   CUAALF,REQLAGY                                                   
         MVC   CUSYSL,REQLSYST                                                  
         OI    RFPFFLAG,RFPFSYMS                                                
         GOTOR AVALGRP,OVWORK1                                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     REQMOVX                                                          
                                                                                
         CLC   RFPVNXTR,ASJDAT     TEST NEXT RUN DATE BEFORE TODAY              
         BL    REQMOV02                                                         
         OC    RFPVNXTR,RFPVNXTR   TEST 'MOVE/COPY TO' NOT SUBMITTED            
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$CARSG)                                           
         B     REQMOVX                                                          
                                                                                
REQMOV02 LA    R1,REQLGRP                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFPIO TO VALIDATE GROUP                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     REQMOVX                                                          
                                                                                
         MVC   RFPFRQID,REQLRQID   SET REQUEST KEY VALUES                       
         MVC   RFPFSORT,REQLSORT                                                
         MVC   RFPFSEQN,REQLSEQN                                                
         OI    RFPFFLAG,RFPF1STR                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPRETRQ                                                 
         GOTOR VRFPIO,PCPARM,RFPD  CALL RFPIO TO GET REQUEST                    
         NI    RFPFFLAG,FF-(RFPF1STR+RFPSPOOF)                                  
         CLI   RFPERROR,RFPNOERR   TEST FOR ERRORS                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     REQMOVX                                                          
         CLI   RFPRET,RFPEOF       TEST END OF REQUESTS                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     REQMOVX                                                          
                                                                                
         OI    RFPFFLAG,RFPFSYMS                                                
         LA    R1,OVWORK1                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             SWITCH BACK TO 'MOVE/COPY TO' GROUP          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     REQMOVX                                                          
                                                                                
         GOTOR AFVAL,LSTREQRH      VALIDATE REQUESTOR NAME                      
         SR    RE,RE                                                            
         ICM   RE,1,RFPVRQSN                                                    
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         LHI   RF,L'RFPVREQC                                                    
         MR    RE,RE                                                            
         LA    RF,RFPVREQC-1(RF)                                                
         IC    RE,RFPVRQSC                                                      
         AR    RF,RE                                                            
         LHI   RE,L'REQLREQR-1                                                  
         TM    RFPVSTAT,RFPVSPOF                                                
         BZ    *+8                                                              
         LHI   RE,3-1                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),FVIFLD      SET REQUESTOR NAME                           
                                                                                
         XC    RQHOUT,RQHOUT       VALIDATE OUTPUT TYPE                         
         GOTOR AVALOUT,LSTOTYPH                                                 
         BL    REQMOV04                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IOUT)                                            
         B     REQMOVX                                                          
         MVC   RQHOUT,FVIFLD                                                    
                                                                                
REQMOV04 LA    R1,LSTDESTH         VALIDATE DESTINATION ID                      
         TM    RFPVSTAT,RFPVSPOF                                                
         BZ    *+8                                                              
         ICM   R1,8,=AL1(RFPXSYMS) PASS X'80' IN HOB OF R1 FOR SPOOF            
         GOTOR AVALDST,(R1)                                                     
         BNE   REQMOVX                                                          
         MVC   RQHDEST,PCWORK                                                   
         MVC   RFPVFXID,PCWORK+L'RQHDEST                                        
                                                                                
         GOTOR AFVAL,LSTDSCH       VALIDATE DESCRIPTION                         
         MVC   RFPVRDSC,FVIFLD                                                  
                                                                                
         GOTOR AVALRUN,LSTSTATH    VALIDATE RUN OPTION                          
         BH    REQMOVX                                                          
         MVC   RFPVREQD,PCWORK                                                  
                                                                                
         LA    R0,LSTACTH          POINT CURSOR BACK TO ACTION FIELD            
         ST    R0,FVADDR                                                        
                                                                                
         TM    RFPVSTAT,RFPVSPOF   COPY SPOOF STATUS                            
         BZ    *+8                                                              
         OI    RFPFFLAG,RFPSPOOF                                                
         MVC   RFPFRQID,RFPVRQID                                                
         MVC   RFPFNUMR,RFPVNUMC                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPADDRQ    ADD THE REQUEST                              
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR   TEST FOR ERRORS                              
         BE    REQMOV06                                                         
         CLI   RFPERROR,RFPNOROO   CAN WE FIT THE REQUEST IN THE GROUP?         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$NMRIG)                                           
         B     REQMOVX                                                          
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     REQMOVX                                                          
                                                                                
REQMOV06 MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    RMFLAG,RMFMOVE      TEST MOVING REQUEST                          
         BZ    REQMOVX                                                          
         NI    RFPFFLAG,FF-RFPFSYMS                                             
         LA    R1,REQLGRP                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     REQMOVX                                                          
                                                                                
         MVC   RFPFRQID,REQLRQID                                                
         MVC   RFPFSORT,REQLSORT                                                
         MVC   RFPFSEQN,REQLSEQN                                                
         OI    RFPFFLAG,RFPF1STR                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPRETRQ                                                 
         GOTOR VRFPIO,PCPARM,RFPD                                               
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
         CLI   RFPERROR,RFPNOERR   TEST FOR ERRORS                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     REQMOVX                                                          
         CLI   RFPRET,RFPEOF       TEST END OF REQUESTS                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     REQMOVX                                                          
         MVI   RFPMODE,RFPDELRQ                                                 
         GOTOR (RF)                                                             
         CLI   RFPERROR,RFPNOERR   TEST FOR ERRORS                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     REQMOVX                                                          
         OI    TWARLI1,TWARLIRL    SET REFRESH LIST PENDING                     
         SR    R1,R1                                                            
         ICM   R1,1,RLLGNUMR                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         STC   R1,RLLGNUMR                                                      
                                                                                
REQMOVX  MVC   OVADDR2,FVADDR      RETURN A(FIELD IN ERROR)                     
         GOTOR AXITSES                                                          
         DROP  R6,RB                                                            
                                                                                
RMWORKD  DSECT                     ** REQUEST MOVE/COPY LOCAL W/S **            
RMFLAG   DS    X                   REQUEST MOVE/COPY FLAG                       
RMFCOPY  EQU   X'80'               ON=COPYING REQUEST                           
RMFMOVE  EQU   X'40'               ON=MOVING REQUEST                            
RLP02    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO APPLY GLOBAL CHANGES TO REQUESTS                         *         
***********************************************************************         
                                                                                
         USING RGWORKD,RC                                                       
REQGLO   BASE  ,                                                                
         TM    CSINDSL1,CSIUSELC   TEST NESTED (LIST) CALL                      
         BNZ   REQGLO04                                                         
                                                                                
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   REQGLO02                                                         
         GOTOR AOVRSCR,PCPARM,('REQGLOSQ',RLPOLY1H)                             
         LA    R0,RGLGRPH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EKEY)                                            
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         J     EXIT                                                             
                                                                                
REQGLO02 GOTOR AFVAL,RGLGRPH       VALIDATE GROUP CODE                          
         JNE   EXIT                                                             
         MVC   CUUSER,TWAUSRID                                                  
         MVC   CUAALF,TWAAGY                                                    
         MVC   CUSYSL,ASSYSL                                                    
         GOTOR AVALGRP,FVIFLD      CALL RFPIO TO VALIDATE GROUP                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXIT                                                             
                                                                                
         CLC   RGLGRPC,FVIFLD      TEST SAME GROUP AS LAST TIME                 
         BE    REQGLO08                                                         
         MVC   RGLGRPC,FVIFLD      SAVE GROUP CODE                              
         B     REQGLO06                                                         
                                                                                
GROUP    USING LSTTABD,PCLSTLV1                                                 
REQGLO04 OC    GROUP.GRPLGRP,GROUP.GRPLGRP                                      
         JZ    XITSES                                                           
         MVC   RGLGRPC,GROUP.GRPLGRP                                            
         MVC   CUUSER,GROUP.GRPLUSER                                            
         MVC   CUAALF,GROUP.GRPLAGY                                             
         MVC   CUSYSL,GROUP.GRPLSYST                                            
         GOTOR AOVRSCR,PCPARM,('REQGLOSQ',RLPOLY1H)                             
         ORG   *-2                                                              
         TM    CSINDSL1,CSIRDSPC   TEST RECORD DISPLAYED FOR CHANGE             
         BNZ   *+6                                                              
         GOTOR (RF)                                                             
         MVC   RGLGRP,RGLGRPC                                                   
         OI    RGLGRPH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R0,RGLGRPH                                                       
         ST    R0,FVADDR                                                        
         LA    R1,RGLGRPC                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXIT                                                             
                                                                                
         TM    CSINDSL1,CSIRDSPC   TEST RECORD DISPLAYED FOR CHANGE             
         BNZ   REQGLO08                                                         
                                                                                
REQGLO06 MVC   RGLGRP,RFPVGRP                                                   
         OI    RGLGRPH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   RGLDSC,RFPVDESC                                                  
         OI    RGLDSCH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R0,RGLRQRFH                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EDATA)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIRDSPC   SET RECORD DISPLAYED FOR CHANGE              
         J     EXIT                                                             
                                                                                
REQGLO08 GOTOR AFVAL,RGLSRCHH      VALIDATE SEARCH STRING                       
         BNE   REQGLO10                                                         
         OI    RGSRCHI,RGSRCHIF    SET SEARCH STRING INPUT                      
         MVC   RGSRCHL,FVXLEN                                                   
         MVC   RGSRCHV,FVIFLD                                                   
                                                                                
         CLC   PCEQUAL,RGSRCHV     TEST STARTS WITH AN EQUALS SIGN              
         BNE   REQGLO10                                                         
         OI    RGSRCHI,RGSRCHIK    SET STRING SEARCH                            
         MVC   RGSRCHV,FVIFLD+L'PCEQUAL                                         
         IC    RF,FVXLEN                                                        
         BCTR  RF,0                                                             
         STC   RF,RGSRCHL                                                       
                                                                                
REQGLO10 GOTOR AFVAL,RGLRQRFH      FROM REQUESTOR                               
         BNE   REQGLO12                                                         
         OI    RGREQRI,RGREQRIF    SET 'FROM' REQUESTOR INPUT                   
         MVC   RGREQRF,FVIFLD                                                   
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RF,FVIFLD(RF)                                                    
         CLC   PCWILD,0(RF)        TEST WILD CARD FEATURE REQUESTED             
         BNE   REQGLO12                                                         
         OI    RGREQRI,RGREQRIW    SET WILD CARD USED                           
         ICM   RF,1,FVXLEN                                                      
         BNZ   *+12                                                             
         OI    RGREQRI,RGREQRIA    SET 'CHANGE ALL'                             
         B     REQGLO12                                                         
         BCTR  RF,0                                                             
         STC   RF,RGREQRFL         SET 'FROM' COMPARE LENGTH                    
                                                                                
REQGLO12 TM    RGREQRI,RGREQRIA    TEST 'CHANGE ALL' INPUT                      
         BZ    *+8                                                              
         MVI   FVMINL,1            YES - 'TO' REQUESTOR MUST BE INPUT           
         GOTOR AFVAL,RGLRQRTH      TO REQUESTOR                                 
         JH    EXIT                                                             
         BL    REQGLO14                                                         
         MVC   RGREQRT,FVIFLD                                                   
         OI    RGREQRI,RGREQRIT    SET 'CHANGE TO' INPUT                        
                                                                                
REQGLO14 GOTOR AFVAL,RGLOTYFH      FROM OUTPUT TYPE                             
         BL    REQGLO16                                                         
         OI    RGOTYPI,RGOTYPIF    SET 'FROM' OUTPUT TYPE INPUT                 
         CLC   PCWILD,FVIFLD       TEST 'ALL' REQUEST                           
         BNE   *+12                                                             
         OI    RGOTYPI,RGOTYPIA    YES - SET 'CHANGE ALL'                       
         B     REQGLO16                                                         
                                                                                
         GOTOR AVALOUT,FVIHDR      VALIDATE 'FROM' OUTPUT TYPE                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IOUT)                                            
         J     EXIT                                                             
         MVC   RGOTYPF,FVIFLD                                                   
                                                                                
REQGLO16 TM    RGOTYPI,RGOTYPIA    TEST 'CHANGE ALL' INPUT                      
         BZ    *+8                                                              
         MVI   FVMINL,1            YES - 'TO' OUTPUT TYPE MUST BE INPUT         
         GOTOR AFVAL,RGLOTYTH      TO OUTPUT TYPE                               
         JH    EXIT                                                             
         BL    REQGLO18                                                         
         GOTOR AVALOUT,FVIHDR      VALIDATE 'TO' OUTPUT TYPE                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IOUT)                                            
         J     EXIT                                                             
         OI    RGOTYPI,RGOTYPIT    SET 'TO' OUTPUT TYPE INPUT                   
         MVC   RGOTYPT,FVIFLD                                                   
                                                                                
REQGLO18 GOTOR AFVAL,RGLDSTFH      FROM DESTINATION ID                          
         BL    REQGLO20                                                         
         OI    RGDESTI,RGDESTIF                                                 
         CLC   PCWILD,FVIFLD       TEST 'ALL' REQUEST                           
         BNE   *+12                                                             
         OI    RGDESTI,RGDESTIA    YES - SET 'CHANGE ALL'                       
         B     REQGLO20                                                         
                                                                                
         GOTOR AVALDST,FVIHDR      VALIDATE 'FROM' DESTINATION ID               
         JNE   EXIT                                                             
         MVC   RGDESTF,PCWORK                                                   
                                                                                
REQGLO20 TM    RGDESTI,RGDESTIA    TEST 'FROM' DEST-ID INPUT                    
         BZ    *+8                                                              
         MVI   FVMINL,1            YES - 'TO' DEST-ID MUST BE INPUT             
         GOTOR AFVAL,RGLDSTTH      TO DESTINATION ID                            
         JH    EXIT                                                             
         BL    REQGLO22                                                         
         GOTOR AVALDST,FVIHDR      VALIDATE 'TO' OUTPUT TYPE                    
         JNE   EXIT                                                             
         OI    RGDESTI,RGDESTIT                                                 
         MVC   RGDESTT,PCWORK                                                   
                                                                                
REQGLO22 GOTOR AFVAL,RGLRUNFH      FROM RUN STATUS                              
         BL    REQGLO24                                                         
         OI    RGRSTAI,RGRSTAIF    SET 'FROM' RUN STATUS INPUT                  
         CLC   PCWILD,FVIFLD       TEST 'ALL' REQUEST                           
         BNE   *+12                                                             
         OI    RGRSTAI,RGRSTAIA    YES - SET 'CHANGE ALL'                       
         B     REQGLO24                                                         
                                                                                
         GOTOR AVALRUN,FVIHDR      VALIDATE 'FROM' RUN STATUS                   
         JNE   EXIT                                                             
         MVC   RGRSTAF,PCWORK                                                   
                                                                                
REQGLO24 TM    RGRSTAI,RGRSTAIA    TEST 'CHANGE ALL' INPUT                      
         BZ    *+8                                                              
         MVI   FVMINL,1            YES - 'TO' OUTPUT TYPE MUST BE INPUT         
         GOTOR AFVAL,RGLRUNTH      TO RUN STATUS                                
         JH    EXIT                                                             
         BL    REQGLO26                                                         
         GOTOR AVALRUN,FVIHDR      VALIDATE 'TO' RUN STATUS                     
         JNE   EXIT                                                             
         OI    RGRSTAI,RGRSTAIT    SET 'TO' RUN STATUS INPUT                    
         MVC   RGRSTAT,PCWORK                                                   
                                                                                
REQGLO26 TM    RGREQRI,RGREQRIT    TEST ANY 'TO' VALUES GIVEN                   
         BNZ   REQGLO28                                                         
         TM    RGOTYPI,RGOTYPIT                                                 
         BNZ   REQGLO28                                                         
         TM    RGDESTI,RGDESTIT                                                 
         BNZ   REQGLO28                                                         
         TM    RGRSTAI,RGRSTAIT                                                 
         BNZ   REQGLO28                                                         
         LA    R0,RGLRQRTH         NO CHANGES WERE INPUT                        
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EDATA)                                           
         MVI   FVOMTYP,GTMINF                                                   
         J     EXIT                                                             
                                                                                
REQGLO28 XC    RFPFRQID,RFPFRQID   CLEAR REQUEST KEY FIELDS FIRST TIME          
         XC    RFPFSORT,RFPFSORT                                                
         XC    RFPFSEQN,RFPFSEQN                                                
         OI    RFPFFLAG,RFPF1STR                                                
                                                                                
REQGLO30 MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPRETRQ    GET FIRST/NEXT REQUEST                       
         GOTOR VRFPIO,PCPARM,RFPD                                               
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXIT                                                             
                                                                                
         CLI   RFPRET,RFPEOF       TEST END OF REQUESTS                         
         BE    REQGLO56                                                         
         IC    R1,RGREQN           BUMP N'REQUESTS IN GROUP                     
         AHI   R1,1                                                             
         STC   R1,RGREQN                                                        
                                                                                
         MVI   RGINDS,0            TURN OFF CHANGED THIS TIME BIT               
                                                                                
         TM    RGSRCHI,RGSRCHIF    TEST STRING SEARCH ACTIVE                    
         BZ    REQGLO38                                                         
         TM    RGSRCHI,RGSRCHIK    TEST STRING SEARCH                           
         BNZ   REQGLO32                                                         
         SR    R0,R0                                                            
         TM    RFPVSTAT,RFPVSPOF   TEST SPOOF REQUEST                           
         BZ    *+8                                                              
         LHI   R0,X'40'                                                         
         GOTOR VREQRFP,PCPARM,((R0),RFPVREQH),(RFPFSYS,ACOM),          *        
               (CULANG,0),0,(X'FF',ARFPBUFF)                                    
         L     R1,8(R1)                                                         
         LHI   R0,512                                                           
         B     REQGLO34                                                         
                                                                                
REQGLO32 SR    R0,R0               REGULAR SEARCH ACTIVE                        
         IC    R0,RFPVNUMC                                                      
         AHI   R0,-1               SUBTRACT ONE FOR HEADER                      
         MHI   R0,L'RQHCARD                                                     
         LA    R1,RQHCARD                                                       
                                                                                
REQGLO34 SR    RF,RF               SEARCH FOR STRING                            
         IC    RF,RGSRCHL                                                       
         SR    R0,RF                                                            
REQGLO36 EX    RF,*+8                                                           
         BE    REQGLO38                                                         
         CLC   RGSRCHV(0),0(R1)    MATCH SEARCH ARGUMENT TO INPUT               
         AHI   R1,1                                                             
         BCT   R0,REQGLO36         DO FOR LENGTH OF STRING                      
         B     REQGLO30                                                         
                                                                                
REQGLO38 TM    RGREQRI,RGREQRIF+RGREQRIT                                        
         BZ    REQGLO42                                                         
                                                                                
         SR    RE,RE                                                            
         ICM   RE,1,RFPVRQSN                                                    
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         LHI   RF,L'RFPVREQC                                                    
         MR    RE,RE                                                            
         LA    RF,RFPVREQC-1(RF)                                                
         IC    RE,RFPVRQSC                                                      
         AR    RF,RE               RF=A(REQUESTOR FIELD)                        
                                                                                
         LHI   RE,L'REQLREQR-1                                                  
         TM    RFPVSTAT,RFPVSPOF                                                
         BZ    *+8                                                              
         LHI   RE,3-1                                                           
         TM    RGREQRI,RGREQRIW    TEST WILD CARD FEATURE (XXX*)                
         BZ    *+8                                                              
         IC    RE,RGREQRFL         YES - MATCH ON FIRST N CHARACTERS            
         TM    RGREQRI,RGREQRIA    TEST 'CHANGE ALL'                            
         BNZ   REQGLO40                                                         
         TM    RGREQRI,RGREQRIF    TEST 'FROM' VALUE INPUT                      
         BZ    REQGLO40                                                         
                                                                                
         EX    RE,*+8              MATCH 'FROM' REQUESTOR                       
         BE    REQGLO40                                                         
         CLC   RGREQRF(0),0(RF)                                                 
         TM    RGREQRI,RGREQRIT    TEST 'TO' VALUE GIVEN                        
         BZ    REQGLO30            NO - IT IS A FILTER                          
         B     REQGLO42            YES - DON'T CHANGE THIS ONE                  
                                                                                
REQGLO40 TM    RGREQRI,RGREQRIT    TEST 'TO' VALUE GIVEN                        
         BZ    REQGLO42                                                         
         LHI   RE,L'REQLREQR-1     MOVE IN NEW REQUESTOR VALUE                  
         TM    RFPVSTAT,RFPVSPOF                                                
         BZ    *+8                                                              
         LHI   RE,3-1                                                           
         EX    RE,*+8                                                           
         BE    REQGLO42                                                         
         CLC   0(0,RF),RGREQRT     TEST 'CHANGE TO' VALUE IS CURRENT            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),RGREQRT     MOVE IN NEW REQUESTOR                        
         OI    RGINDS,RGICHNG      SET REQUEST CHANGED                          
                                                                                
REQGLO42 OC    RFPVFXID,RFPVFXID   TEST FAX ID IS PRESENT                       
         BNZ   REQGLO50                                                         
                                                                                
         TM    RGOTYPI,RGOTYPIF    TEST OUTPUT TYPE CHANGE                      
         BZ    REQGLO44                                                         
         TM    RGOTYPI,RGOTYPIA    TEST 'CHANGE ALL'                            
         BNZ   REQGLO44                                                         
         CLC   RQHOUT,RGOTYPF      MATCH 'FROM' OUTPUT TYPE                     
         BE    REQGLO44                                                         
         TM    RGOTYPI,RGOTYPIT    TEST 'TO' OUTPUT TYPE GIVEN                  
         BZ    REQGLO30                                                         
         B     REQGLO46                                                         
                                                                                
REQGLO44 TM    RGOTYPI,RGOTYPIT    TEST 'TO' OUTPUT TYPE GIVEN                  
         BZ    REQGLO46                                                         
         CLC   RQHOUT,RGOTYPT      TEST 'CHANGE TO' VALUE IS CURRENT            
         BE    REQGLO46                                                         
         MVC   RQHOUT,RGOTYPT      MOVE IN NEW OUTPUT TYPE                      
         OI    RGINDS,RGICHNG      SET REQUEST CHANGED                          
                                                                                
REQGLO46 TM    RGDESTI,RGDESTIF    TEST 'FROM' DESTINATION ID INPUT             
         BZ    REQGLO48                                                         
         TM    RGDESTI,RGDESTIA    TEST 'CHANGE ALL'                            
         BNZ   REQGLO48                                                         
         CLC   RQHDEST,RGDESTF     MATCH 'FROM' DESTINATION ID                  
         BE    REQGLO48                                                         
         TM    RGDESTI,RGDESTIT    TEST 'TO' OUTPUT TYPE GIVEN                  
         BZ    REQGLO30            NO - IT IS A FILTER                          
         B     REQGLO50            YES - DON'T CHANGE THIS ONE                  
                                                                                
REQGLO48 TM    RGDESTI,RGDESTIT    TEST 'TO' DESTINATION ID INPUT               
         BZ    REQGLO50                                                         
         CLC   RQHDEST,RGDESTT     TEST 'CHANGE TO' VALUE IS CURRENT            
         BE    REQGLO50                                                         
         MVC   RQHDEST,RGDESTT     MOVE IN NEW DESTINATION ID                   
         OI    RGINDS,RGICHNG      SET REQUEST CHANGED                          
                                                                                
REQGLO50 TM    RGRSTAI,RGRSTAIF    TEST RUN STATUS CHANGE                       
         BZ    REQGLO52                                                         
         TM    RGRSTAI,RGRSTAIA    TEST 'CHANGE ALL'                            
         BNZ   REQGLO52                                                         
         CLC   RFPVREQD,RGRSTAF    MATCH 'FROM' RUN STATUS                      
         BE    REQGLO52                                                         
         TM    RGRSTAI,RGRSTAIT    TEST 'TO' RUN STATUS INPUT                   
         BZ    REQGLO30            YES - IT IS A FILTER                         
         B     REQGLO54            NO - DON'T CHANGE THIS ONE                   
                                                                                
REQGLO52 TM    RGRSTAI,RGRSTAIT    TEST 'TO' RUN STATUS INPUT                   
         BZ    REQGLO54                                                         
         CLC   RFPVREQD,RGRSTAT    TEST 'CHANGE TO' VALUE IS CURRENT            
         BE    REQGLO54                                                         
         MVC   RFPVREQD,RGRSTAT    MOVE IN NEW RUN STATUS                       
         OI    RGINDS,RGICHNG      SET REQUEST CHANGED                          
                                                                                
REQGLO54 TM    RGINDS,RGICHNG      TEST ANY CHANGES MADE                        
         BZ    REQGLO30            NO - GET NEXT RECORD                         
                                                                                
         MVC   RFPFRQID,RFPVRQID   SET KEY FIELDS FOR REQUEST CHANGE            
         MVC   RFPFSORT,RFPVSORT                                                
         MVC   RFPFSEQN,RFPVSEQN                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPCHARQ    UPDATE THE REQUEST                           
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXIT                                                             
         IC    R1,RGREQC           BUMP N'REQUESTS CHANGED                      
         AHI   R1,1                                                             
         STC   R1,RGREQC                                                        
         B     REQGLO30                                                         
                                                                                
REQGLO56 LA    R0,RGLGRPH                                                       
         ST    R0,FVADDR                                                        
         CLI   RGREQN,0            TEST ANY REQUESTS IN THIS GROUP              
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$NRITG)                                           
         B     REQGLOX                                                          
                                                                                
         SR    R0,R0                                                            
         ICM   R0,1,RGREQC         R0=N'CHANGES MADE                            
         BZ    REQGLO58                                                         
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$TRCEN)                                           
         TM    CSINDSL1,CSIUSELC   SET APPROPRIATE MESSAGE                      
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(GI$TRCEP)                                           
         CVD   R0,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,DIGIT                                            
         UNPK  FVXTRA(2),PCDUB     OUTPUT N'REQUESTS CHANGED                    
         CLI   FVXTRA,C'0'                                                      
         BNE   REQGLOX                                                          
         MVC   FVXTRA(1),FVXTRA+1                                               
         MVI   FVXTRA+1,SPACE                                                   
         OI    TWARLI1,TWARLIRL    SET REFRESH LIST PENDING                     
         B     REQGLOX                                                          
                                                                                
REQGLO58 LA    R0,RGLSRCHH         NO CHANGES MADE                              
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GE$NCMCP)                                           
                                                                                
REQGLOX  J     EXIT                                                             
         DROP  RB,RC                                                            
                                                                                
RGWORKD  DSECT                     ** REQUEST/GLOBAL LOCAL W/S **               
RGREQN   DS    X                   N'REQUESTS IN THIS GROUP                     
RGREQC   DS    X                   N'REQUESTS CHANGED THIS TIME                 
                                                                                
RGINDS   DS    X                   TRANSACTION INDICATOR                        
RGICHNG  EQU   X'40'               REQUEST CHANGED THIS TIME                    
                                                                                
RGSRCHI  DS    X                   SEARCH FIELD INDICATOR                       
RGSRCHIF EQU   X'80'               SEARCH STRING INPUT                          
RGSRCHIK EQU   X'40'               SEARCH FOR STRING IN REQUEST CARDS           
RGSRCHL  DS    X                   SEARCH STRING LENGTH                         
RGSRCHV  DS    CL(L'RGLSRCH)       SEARCH STRING VALUE                          
                                                                                
RGREQRI  DS    X                   REQUESTOR FIELD INDICATOR                    
RGREQRIF EQU   X'80'               CHANGE FROM REQUESTOR INPUT                  
RGREQRIT EQU   X'40'               CHANGE TO REQUESTOR INPUT                    
RGREQRIA EQU   X'20'               CHANGE ALL                                   
RGREQRIW EQU   X'10'               WILD CARD USED                               
RGREQRFL DS    X                   CHANGE FROM LENGTH                           
RGREQRF  DS    CL(L'REQLREQR)      CHANGE REQUESTOR FROM                        
RGREQRT  DS    CL(L'REQLREQR)      CHANGE REQUESTOR TO                          
                                                                                
RGOTYPI  DS    X                   OUTPUT TYPE FIELD INDICATOR                  
RGOTYPIF EQU   X'80'               CHANGE FROM OUTPUT TYPE INPUT                
RGOTYPIT EQU   X'40'               CHANGE TO OUTPUT TYPE INPUT                  
RGOTYPIA EQU   X'20'               CHANGE ALL                                   
RGOTYPF  DS    CL(L'REQLOTYP)      CHANGE OUTPUT TYPE FROM                      
RGOTYPT  DS    CL(L'REQLOTYP)      CHANGE OUTPUT TYPE TO                        
                                                                                
RGDESTI  DS    X                   DESTINATION ID FIELD INDICATOR               
RGDESTIF EQU   X'80'               CHANGE FROM DESTINATION ID INPUT             
RGDESTIT EQU   X'40'               CHANGE TO DESTINATION ID INPUT               
RGDESTIA EQU   X'20'               CHANGE ALL                                   
RGDESTF  DS    CL(L'REQLDEST)      CHANGE DESTINATION ID FROM                   
RGDESTT  DS    CL(L'REQLDEST)      CHANGE DESTINATION ID TO                     
                                                                                
RGRSTAI  DS    X                   RUN STATUS FIELD INDICATOR                   
RGRSTAIF EQU   X'80'               CHANGE FROM RUN STATUS INPUT                 
RGRSTAIT EQU   X'40'               CHANGE TO RUN STATUS INPUT                   
RGRSTAIA EQU   X'20'               CHANGE ALL                                   
RGRSTAF  DS    CL(L'REQLRSTA)      CHANGE RUN STATUS FROM                       
RGRSTAT  DS    CL(L'REQLRSTA)      CHANGE RUN STATUS TO                         
RLP02    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LIST REQUEST RECORDS                                                *         
***********************************************************************         
                                                                                
REQLST   BASE  ,                                                                
         L     RC,AOVERWRK                                                      
         USING RLWORKD,RC          RC=A(LOCAL W/S)                              
                                                                                
RLKEYVAL MVI   RLFLAG,0            RESET LIST CONTROL FLAG                      
         CLI   TWASCRN,REQLSTSQ    TEST LIST SCREEN IS LOADED                   
         BE    RLKEYV02                                                         
         XC    TWARLIND,TWARLIND                                                
         NI    CSINDSL1,CSIUSELC                                                
         GOTOR AOVRSCR,PCPARM,('REQLSTSQ',RLPOLY1H)                             
         JNE   EXIT                                                             
         CLI   ASSYSL,CONLETQ      TEST CONNECTED TO CONTROL SYSTEM             
         BE    RLKEYV02                                                         
         XC    RLISYSN,RLISYSN     NO - CLEAR SYSTEM FIELDS                     
         OI    RLISYSH+(FVATRB-FVIHDR),FVAPROT                                  
                                                                                
RLKEYV02 TM    CSINDSL1,CSIUSELC   TEST NESTED (LIST) CALL                      
         BNZ   RLKEYV08                                                         
         TM    RLISYSH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   RLKEYV04                                                         
         TM    RLIGRPH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   RLKEYV04                                                         
         CLI   GROUP.LSTTRTYP,RECGRP                                            
         BE    RLKEYV09                                                         
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   RLKEYV04                                                         
         LA    R0,RLIGRPH                                                       
         TM    RLISYSH+(FVATRB-FVIHDR),FVAPROT                                  
         BNZ   *+8                                                              
         LA    R0,RLISYSH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EKEY)                                            
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK                                                
         J     EXIT                                                             
                                                                                
RLKEYV04 MVC   CUSYSL,ASSYSL       SET DEFAULT SYSTEM                           
         TM    RLISYSH+(FVATRB-FVIHDR),FVAPROT                                  
         BNZ   RLKEYV06                                                         
         GOTOR AVALSYS,RLISYSH                                                  
         JNE   EXIT                                                             
         MVC   CUSYSL,PCWORK                                                    
                                                                                
RLKEYV06 GOTOR AFVAL,RLIGRPH       VALIDATE GROUP CODE                          
         JNE   EXIT                                                             
         MVC   CUUSER,TWAUSRID                                                  
         MVC   CUAALF,TWAAGY                                                    
         LA    R1,FVIFLD                                                        
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXIT                                                             
                                                                                
         GOTOR ANTRSES,=AL1(RECGRP,ACTBLD,6,0,0,0)                              
                                                                                
RLKEYV08 OI    RLIGRPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RLISYSH+(FVATRB-FVIHDR),FVAPROT                                  
                                                                                
RLKEYV09 OC    GROUP.GRPLGRP,GROUP.GRPLGRP                                      
         JZ    XITSES                                                           
         MVC   CUUSER,GROUP.GRPLUSER                                            
         MVC   CUAALF,GROUP.GRPLAGY                                             
         MVC   CUSYSL,GROUP.GRPLSYST                                            
         LA    R1,GROUP.GRPLGRP                                                 
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP                                                          
         BE    RLKEYV10                                                         
         MVC   RLIGRP,GROUP.GRPLGRP                                             
         OI    RLIGRPH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R0,RLIGRPH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXIT                                                             
                                                                                
RLBLDRET DS    0H                                                               
RLKEYV10 CLC   RLIGRP,RFPVGRP      OUTPUT GROUP CODE                            
         BE    *+14                                                             
         MVC   RLIGRP,RFPVGRP                                                   
         OI    RLIGRPH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
         CLI   ASSYSL,CONLETQ      TEST CONTROL SYSTEM                          
         BNE   RLKEYV12                                                         
         GOTOR AGETSYS,CUSYSL      OUTPUT SYSTEM CODE                           
         CLC   RLISYS,PCWORK                                                    
         BE    RLKEYV12                                                         
         MVC   RLISYS,PCWORK                                                    
         OI    RLISYSH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
RLKEYV12 CLC   RLIDESC,RFPVDESC    OUTPUT DESCRIPTION                           
         BE    *+14                                                             
         MVC   RLIDESC,RFPVDESC                                                 
         OI    RLIDESCH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
         LA    R0,RLIGRPH          POINT TO GROUP CODE                          
         ST    R0,FVADDR                                                        
         TM    RFPVGSTA,RFPVGSDQ   EXIT NOW IF DELETED GROUP                    
         JNZ   EXIT                                                             
                                                                                
         CLC   RLLGRPC,RFPVGRP     TEST CHANGE OF GROUP CODE                    
         BNE   *+14                                                             
         CLC   RLLGNUMR,RFPVNREQ   OR NUMBER OF REQUESTS                        
         BE    RLOPTVAL                                                         
         OI    CSLTINDS,CSLTIFST   YES - SET FIRST FOR LIST                     
         MVC   RLLGRPC,RFPVGRP                                                  
         MVC   RLLGNUMR,RFPVNREQ                                                
         XC    RLSAVE(RLSAVEL),RLSAVE                                           
         XC    TWARLIND,TWARLIND   RESET LIST INDICATOR                         
                                                                                
RLOPTVAL LA    R5,RLOPTS                                                        
         ST    R5,AOVEROUT         SAVE A(OUTPUT AREA FOR OPTIONS)              
         USING RLVALS,R5           R5=A(REQUEST LIST OPTIONS)                   
                                                                                
         BASR  R1,0                                                             
         AHI   R1,RLOVAL-*                                                      
         ST    R1,AOVERVAL         SAVE A(OPTION VALIDATION ROUTINES)           
         GOTOR AFVAL,RLPOPTH                                                    
         GOTOR AVALOPT,RLOTAB      VALIDATE OPTIONS                             
         JNE   EXIT                                                             
         DROP  R5,RB                                                            
         EJECT                                                                  
***********************************************************************         
* HANDLE LIST SUB-ACTIONS                                             *         
***********************************************************************         
                                                                                
RLSELECT MVI   FVINDX,0                                                         
         NI    CSLTINDS,FF-(CSLTIHLD+CSLTIANY)                                  
         XC    RLRHSVAL,RLRHSVAL   CLEAR SAVED RHS VALUE                        
                                                                                
         CLI   PCPFKEY,PFKRFSHQ    TEST REFRESH LIST                            
         JNE   *+12                                                             
         OI    CSLTINDS,CSLTIFST   SET FIRST FOR LIST                           
         MVI   PCPFKEY,0           CLEAR PFKEY                                  
                                                                                
         LA    R0,RLOPTS                                                        
         LHI   R1,RLOKOPTL                                                      
         LA    RE,RLOKOPT                                                       
         LHI   RF,RLOKOPTL                                                      
         CLCL  R0,RE                                                            
         JE    *+8                                                              
         OI    CSLTINDS,CSLTIFST   SET FIRST FOR LIST IF KEY CHANGED            
                                                                                
         LA    R0,RLVALS                                                        
         LHI   R1,RLVALSL                                                       
         LA    RE,RLOPTS                                                        
         LHI   RF,RLVALSL                                                       
         MVCL  R0,RE               MOVE IN NEW FILTERS/OPTIONS                  
                                                                                
         TM    CSLTINDS,CSLTIFST   TEST CHANGE OF KEY FIELDS                    
         JNZ   RLSCROLL            YES - GET SOME RECORDS                       
                                                                                
RLSELE02 SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LIST ENTRIES                    
         JZ    RLSCROLL                                                         
         MVC   LSTTRECN,CSPAG#LO                                                
         LA    R6,RLIACTNH                                                      
         USING LSTD,R6                                                          
                                                                                
RLSELE04 BASE  ,                                                                
         NI    RLFLAG,FF-(RLFINPT+RLFIPFK+RLFSPLT+RLFCHNG)                      
         CLI   LSTACTH+(FVILEN-FVIHDR),0                                        
         BE    RLSELE06                                                         
         CLI   LSTACT,ACTAOKQ      TEST NULL ACTION                             
         BE    RLSELE06                                                         
         OI    RLFLAG,RLFINPT      SET USER INPUT                               
         B     RLSELE10                                                         
                                                                                
RLSELE06 CLC   CSCURDSP,LSTD+(FVABSA-FVIHDR)                                    
         BL    RLSELE08                                                         
         CLC   CSCURDSP,LSTNEXT+(FVABSA-FVIHDR)                                 
         BNL   RLSELE08                                                         
         CLI   PCPFKEY,0           TEST PFKEY WAS ENTERED                       
         BE    RLSELE08                                                         
         OI    RLFLAG,RLFIPFK      SET TO MATCH ON PFKEY VALUE                  
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         B     RLSELE10                                                         
                                                                                
RLSELE08 TM    CSLTINDS,CSLTIEOL+CSLTIEOP                                       
         BZ    RLSELE10                                                         
         CLC   LSTTRECN,CSSEL#LO                                                
         BL    RLSELE10                                                         
         CLC   LSTTRECN,CSSEL#HI                                                
         BH    RLSELE10                                                         
         SR    R4,R4                                                            
         ICM   R4,3,CSSELMUL                                                    
         A     R4,AOVERSEL                                                      
         USING SELTABD,R4          R4=A(SELECT TABLE ENTRY)                     
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPM                                                    
         LA    RE,WORKD(RE)                                                     
         XC    LSTACT,LSTACT                                                    
         MVC   LSTACT(ACTNAMLQ),0(RE)                                           
         OI    RLFLAG,RLFINPT      SET USER INPUT                               
                                                                                
RLSELE10 GOTOR ATSARIO,TSAGET      GET REQUEST LIST RECORD                      
                                                                                
         MVC   CUUSER,REQLUSER                                                  
         MVC   CUAALF,REQLAGY                                                   
         MVC   CUSYSL,REQLSYST                                                  
         LA    R1,REQLGRP                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP                                                          
         ORG   *-2                                                              
         CLC   RFPVGRP,REQLGRP     VALIDATE GROUP IF NECESSARY                  
         BE    RLSELE11                                                         
         GOTOR (RF)                                                             
         BE    RLSELE11                                                         
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXIT                                                             
                                                                                
RLSELE11 MVC   RFPFRQID,REQLRQID   GET REQUEST INTO STORAGE                     
         MVC   RFPFSORT,REQLSORT                                                
         MVC   RFPFSEQN,REQLSEQN                                                
         OI    RFPFFLAG,RFPF1STR                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPRETRQ                                                 
         GOTOR VRFPIO,PCPARM,RFPD                                               
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXIT                                                             
                                                                                
         TM    LSTREQRH+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    RLSELE12                                                         
         OI    RLFLAG,RLFCHNG                                                   
         GOTOR AFVAL,LSTREQRH      VALIDATE REQUESTOR NAME                      
         SR    RE,RE                                                            
         ICM   RE,1,RFPVRQSN                                                    
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         LHI   RF,L'RFPVREQC                                                    
         MR    RE,RE                                                            
         LA    RF,RFPVREQC-1(RF)                                                
         IC    RE,RFPVRQSC                                                      
         AR    RF,RE                                                            
         LHI   RE,L'REQLREQR-1                                                  
         TM    RFPVSTAT,RFPVSPOF                                                
         BZ    *+8                                                              
         LHI   RE,3-1                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),FVIFLD      SET REQUESTOR NAME IN REQUEST                
         MVC   REQLREQR,PCSPACES                                                
         EX    RE,*+8                                                           
         B     RLSELE12                                                         
         MVC   REQLREQR(0),FVIFLD  AND SET IT IN THE LIST TOO                   
                                                                                
RLSELE12 TM    LSTOTYPH+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    RLSELE14                                                         
         OI    RLFLAG,RLFCHNG                                                   
         OI    LSTDESTH+(FVIIND-FVIHDR),FVITHIS                                 
         XC    RQHOUT,RQHOUT       VALIDATE OUTPUT TYPE                         
         XC    REQLOTYP,REQLOTYP                                                
         GOTOR AVALOUT,LSTOTYPH                                                 
         BL    RLSELE14                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IOUT)                                            
         J     EXIT                                                             
         MVC   RQHOUT,FVIFLD                                                    
         MVC   REQLOTYP,FVIFLD                                                  
                                                                                
RLSELE14 TM    LSTDESTH+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    RLSELE16                                                         
         OI    RLFLAG,RLFCHNG                                                   
         XC    RQHDEST,RQHDEST                                                  
         XC    RFPVFXID,RFPVFXID                                                
         XC    REQLDEST,REQLDEST                                                
         XC    REQLFXID,REQLFXID                                                
         LA    R1,LSTDESTH         VALIDATE DESTINATION ID                      
         TM    RFPVSTAT,RFPVSPOF                                                
         BZ    *+8                                                              
         ICM   R1,8,=AL1(RFPXSYMS) PASS X'80' IN HOB OF R1 FOR SPOOF            
         GOTOR AVALDST,(R1)                                                     
         JNE   EXIT                                                             
         MVC   RQHDEST,PCWORK                                                   
         MVC   RFPVFXID,PCWORK+L'RQHDEST                                        
         MVC   REQLDEST,PCWORK                                                  
         MVC   REQLFXID,PCWORK+L'RQHDEST                                        
         OC    REQLFXID,REQLFXID   SET OUTPUT TYPE IF FAX ID PRESENT            
         BZ    RLSELE16                                                         
         MVC   RQHOUT,PCUFAX                                                    
         MVC   REQLOTYP,PCUFAX                                                  
         MVC   LSTOTYP,PCUFAX                                                   
         OI    LSTOTYPH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
RLSELE16 TM    LSTDSCH+(FVIIND-FVIHDR),FVITHIS                                  
         BZ    RLSELE18                                                         
         OI    RLFLAG,RLFCHNG                                                   
         GOTOR AFVAL,LSTDSCH       VALIDATE DESCRIPTION                         
         MVC   RFPVRDSC,FVIFLD                                                  
         MVC   REQLRDSC,FVIFLD                                                  
                                                                                
RLSELE18 TM    LSTSTATH+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    RLSELE20                                                         
         OI    RLFLAG,RLFCHNG                                                   
         GOTOR AVALRUN,LSTSTATH    VALIDATE RUN OPTION                          
         JH    EXIT                                                             
         MVC   RFPVREQD,PCWORK     VALIDATE RUN?                                
         MVC   REQLRSTA,PCWORK                                                  
                                                                                
RLSELE20 TM    RLFLAG,RLFCHNG      TEST ANY CHANGES THIS LINE                   
         BZ    RLSELE22                                                         
         NI    RLFLAG,FF-(RLFCHNG)                                              
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPCHARQ    UPDATE THE REQUEST                           
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXIT                                                             
         OI    CSLTINDS,CSLTIANY   SET LINE PROCESSED THIS SCREEN               
         GOTOR ATSARIO,TSAPUT      UPDATE THE LIST ENTRY                        
                                                                                
RLSELE22 GOTOR AFVAL,LSTACTH       VALIDATE THE ACTION FIELD                    
         BE    RLSELE24                                                         
         TM    RLFLAG,RLFINPT+RLFIPFK                                           
         JZ    RLSETAOK                                                         
                                                                                
RLSELE24 NI    RLFLAG,FF-(RLFNEXT)                                              
                                                                                
         TM    RLFLAG,RLFIPFK      PROCESS PFKEY IF ENTERED                     
         BNZ   *+12                                                             
         CLI   FVIFLD,ACTAOKQ      TEST INPUT IS ACTION COMPLETED               
         JE    RLSETAOK                                                         
                                                                                
         CLI   FVIFLD,ACTNOTQ      OR MULTIPLE ACTION END                       
         BNE   *+12                                                             
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         J     RLSETAOK                                                         
                                                                                
         LA    RF,FVIFLD+1                                                      
         SR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         BZ    RLSELE30                                                         
         SR    R1,R1                                                            
RLSELE26 CLI   0(RF),SPACE         LOOK FORWARD FOR A SPACE CHARACTER           
         BE    RLSELE28                                                         
         AHI   RF,1                                                             
         AHI   R1,1                                                             
         BCT   RE,RLSELE26                                                      
         B     RLSELE30                                                         
                                                                                
RLSELE28 OI    RLFLAG,RLFSPLT      INDICATE FIELD IS SPLIT                      
         CLI   0(RF),SPACE                                                      
         BNE   *+12                                                             
         AHI   RF,1                                                             
         BCT   RE,*-12                                                          
         BCTR  RE,0                                                             
         MVC   RLRHSVAL,PCSPACES                                                
         CHI   RE,L'RLRHSVAL-1                                                  
         BNH   *+8                                                              
         LHI   RE,L'RLRHSVAL-1                                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RLRHSVAL(0),0(RF)                                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),PCSPACES                                                 
         STC   R1,FVXLEN           SET LHS INPUT LENGTH                         
         AHI   R1,1                                                             
         STC   R1,FVILEN                                                        
                                                                                
RLSELE30 SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RE,FVIFLD(RF)       POINT TO END OF INPUT FIELD                  
         CLI   0(RE),ACTEOSQ                                                    
         BE    *+12                                                             
         CLI   0(RE),ACTEOLQ                                                    
         BNE   RLSELE32                                                         
         MVC   CSSEL#LO,LSTTRECN   SET LOW RECORD NUMBER                        
         MVC   CSSEL#HI,PCEFFS     SET DEFAULT HIGH VALUE                       
         LHI   R1,CSLTIEOL                                                      
         CLI   0(RE),ACTEOLQ                                                    
         BE    *+14                                                             
         MVC   CSSEL#HI,CSPAG#HI                                                
         LHI   R1,CSLTIEOP                                                      
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         OI    CSLTINDS,0                                                       
         BCTR  RF,0                                                             
         STC   RF,FVXLEN                                                        
                                                                                
RLSELE32 L     R4,AOVERSEL         R4=A(SELECT TABLE)                           
         XC    RLASEL,RLASEL       RESET A(UNIQUE ACTION)                       
                                                                                
RLSELE34 TM    RLFLAG,RLFINPT      TEST FIELD INPUT                             
         BNZ   RLSELE36                                                         
         TM    RLFLAG,RLFIPFK      TEST PFKEY INPUT                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   PCPFKEY,SELTPFK     MATCH PFKEY TO SELECT TABLE                  
         BE    RLSELE42                                                         
         B     RLSELE38                                                         
                                                                                
RLSELE36 SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,WORKD(RE)        RE=A(CURRENT ACTION WORD)                    
         EX    RF,*+8                                                           
         BE    RLSELE37                                                         
         CLC   FVIFLD(0),0(RE)     MATCH INPUT TO ACTION WORD                   
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPM                                                    
         LA    RE,WORKD(RE)        RE=A(CURRENT ACTION WORD)                    
         EX    RF,*+8                                                           
         BNE   RLSELE38                                                         
         CLC   FVIFLD(0),0(RE)     MATCH INPUT TO ACTION WORD                   
RLSELE37 TM    SELTIND2,SELTIDEF   TEST THIS IS THE DEFAULT ENTRY               
         BZ    *+12                                                             
         STCM  R4,15,RLASEL        YES - USE IT REGARDLESS                      
         B     RLSELE39                                                         
                                                                                
         OC    RLASEL,RLASEL       TEST A(ACTION)                               
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IACT)                                            
         J     EXIT                                                             
         STCM  R4,15,RLASEL        SET A(ACTION)                                
                                                                                
RLSELE38 AHI   R4,SELTABL          BUMP TO NEXT TABLE ENTRY                     
         CLI   SELTABD,EOT         TEST END OF TABLE REACHED                    
         BNE   RLSELE34                                                         
                                                                                
RLSELE39 ICM   R4,15,RLASEL        R4=A(UNIQUE ACTION) OR 0                     
         BZ    RLSELE48                                                         
         TM    SELTIND2,SELTIRHS   TEST RHS REQUIRED                            
         BZ    RLSELE40                                                         
         TM    RLFLAG,RLFSPLT      TEST SPLIT FIELD                             
         BNZ   RLSELE40                                                         
         CLC   RLRHSVAL,PCSPACES   TEST VALUE SAVED                             
         BNH   *+12                                                             
         TM    CSLTINDS,CSLTIEOL+CSLTIEOP TEST MULTIPLE SELECT ACTIVE           
         BNZ   RLSELE40                                                         
         MVC   FVMSGNO,=AL2(GE$MIF)                                             
         SR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         LA    RF,1(RE)                                                         
         LA    RE,LSTACT(RE)                                                    
         MVI   0(RE),SPACE         SET 'ACTION ?' IN INPUT FIELD                
         MVC   1(L'PCQUEST,RE),PCQUEST                                          
         STC   RF,FVERRNDX         SET CURSOR TO QUESTION MARK                  
         OI    LSTACTH+(FVOIND-FVIHDR),FVOXMT                                   
         J     EXIT                                                             
                                                                                
RLSELE40 TM    CSLTINDS,CSLTIEOL   TEST SELECT TO END OF LIST                   
         BZ    *+12                                                             
         TM    SELTIND1,SELTIEOL                                                
         BZ    RLSELE48                                                         
         TM    CSLTINDS,CSLTIEOP   TEST SELECT TO END OF PAGE                   
         BZ    *+12                                                             
         TM    SELTIND1,SELTIEOP                                                
         BZ    RLSELE48                                                         
                                                                                
RLSELE42 CLC   SELTREC(L'SELTREC+L'SELTACT),BZEROES                             
         BE    RLSELE44                                                         
         GOTOR ATSTMIX,SELTRECA    VALIDATE RECORD/ACTION                       
         BNE   RLSELE48                                                         
                                                                                
RLSELE44 CLC   LSTTRECN,CSSEL#LO   TEST SELECT MULTIPLE INPUT LINE              
         BNE   RLSELE46                                                         
         L     RE,AOVERSEL                                                      
         LA    RF,SELTABD                                                       
         SR    RF,RE                                                            
         STCM  RF,3,CSSELMUL       SET DISPLACEMENT TO SELTAB ENTRY             
                                                                                
RLSELE46 GOTOR ATSARIO,TSAGET      GET REQUEST LIST RECORD                      
         B     RLSELE50                                                         
                                                                                
RLSELE48 TM    RLFLAG,RLFIPFK      TEST PFKEY INPUT                             
         BNZ   RLNXTREQ                                                         
         NI    CSLTINDS,FF-(CSLTIEOL+CSLTIEOP)                                  
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         J     EXIT                                                             
                                                                                
RLSELE50 MVC   OVWORK1(L'RLRHSVAL),RLRHSVAL                                     
         LA    RE,LSTD                                                          
         ST    RE,OVADDR1          PASS VALUE AND A(LINE) TO ACTION RTN         
         AHI   RE,LSTACTH-LSTD                                                  
         ST    RE,OVADDR2          SAVE A(ACTION FIELD)                         
                                                                                
         OI    CSLTINDS,CSLTIANY   SET LINE PROCESSED THIS SCREEN               
         CLC   SELTREC(L'SELTREC+L'SELTACT),BZEROES                             
         BE    RLSELE52                                                         
         L     RE,ATWA                                                          
         LA    RF,LSTD                                                          
         SR    RF,RE                                                            
         STCM  RF,3,CSSELACT       SET DISPLACEMENT TO FIELD HEADER             
         L     RE,AOVERSEL                                                      
         LA    RF,SELTABD                                                       
         SR    RF,RE                                                            
         STCM  RF,3,CSSELCUR       SET DISPLACEMENT TO SELTAB ENTRY             
         CLC   CSSEL#LO,LSTTRECN                                                
         BNE   *+8                                                              
         STCM  RF,3,CSSELMUL       SET DISPLACEMENT TO MULTI-ENTRY              
         STC   R0,CSSELREM         SET NUMBER OF LINES REMAINING                
                                                                                
         GOTOR ANTRSES,SELTPARM                                                 
                                                                                
RLSELE52 L     RF,OVBASE1                                                       
         ICM   RF,8,SELTRTN                                                     
         GOTOR (RF)                                                             
         LHI   R1,1                SET TO RE-DISPLAY THIS LINE                  
         J     RLACTAOK                                                         
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* LINE ACTION COMPLETED                                               *         
***********************************************************************         
                                                                                
RLDISRET SR    R1,R1               DON'T WANT TO RE-DISPLAY THIS LINE           
         J     RLACTR02                                                         
                                                                                
RLDELRET CLC   FVMSGNO,=AL2(FVFOK) TEST FOR ERRORS                              
         JE    *+14                                                             
         MVC   FVADDR,OVADDR2      YES - POSITION CURSOR TO FIELD               
         J     EXIT                                                             
         SR    R1,R1               DON'T WANT TO RE-DISPLAY THIS LINE           
         J     RLACTR02                                                         
                                                                                
RLCALRET DS    0H                  GROUP/CALENDAR RETURNS HERE                  
RLCHARET CLC   FVMSGNO,=AL2(FVFOK)                                              
         JE    *+14                                                             
         MVC   FVADDR,OVADDR2      YES - POSITION CURSOR TO FIELD               
         J     EXIT                                                             
         LHI   R1,1                SET TO RE-DISPLAY THIS LINE                  
         J     RLACTR02                                                         
                                                                                
RLGLORET L     RC,AOVERWRK         RESTORE RC VALUE                             
         OI    CSLTINDS,CSLTIFST   SET FIRST FOR LIST                           
         J     RLSCROLL                                                         
                                                                                
RLACTR02 L     RC,AOVERWRK                                                      
         MVI   PCPFKEY,0           RESET PFKEY VALUE                            
         TM    TWARLI1,TWARLIRN    TEST REFRESH NOW (FOR ERRORS)                
         JZ    *+16                                                             
         NI    TWARLI1,FF-(TWARLIRN)                                            
         OI    CSLTINDS,CSLTIFST                                                
         J     RLSCROLL                                                         
         SR    R6,R6                                                            
         ICM   R6,3,CSSELACT                                                    
         A     R6,ATWA             R6=A(ACTION LINE)                            
         SR    R4,R4                                                            
         ICM   R4,3,CSSELCUR                                                    
         A     R4,AOVERSEL         R4=A(SELECT TABLE ENTRY)                     
         SR    R0,R0                                                            
         ICM   R0,1,CSSELREM       R0=NUMBER OF LINES REMAINING                 
         JNZ   RLACTAOK                                                         
         DC    H'0'                                                             
                                                                                
RLACTAOK XC    LSTACT,LSTACT                                                    
         MVI   LSTACT,ACTAOKQ                                                   
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPM                                                    
         LA    RE,WORKD(RE)                                                     
         MVC   LSTACT+1(ACTNAMLQ),0(RE)                                         
         OI    LSTACTH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
RLACTA02 LTR   R1,R1               TEST IF RE-DISPLAY OF LINE REQUIRED          
         JZ    RLACTA04                                                         
         GOTOR ATSARIO,TSAPUT                                                   
         GOTOR BLDLIN,LSTD                                                      
         JH    EXIT                                                             
         JE    RLACTA04                                                         
         OI    CSLTINDS,CSLTIFST   CAN'T GET REQUEST - REFRESH DISPLAY          
         J     RLSCROLL                                                         
                                                                                
RLACTA04 CLI   PCPFKEY,PFKQUITQ                                                 
         JNE   RLNXTREQ                                                         
         J     RLNXTR06                                                         
                                                                                
RLSETAOK XC    LSTACT,LSTACT                                                    
         OI    LSTACTH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
RLNXTREQ LA    R6,LSTNEXT          BUMP TO NEXT ACTION FIELD                    
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN       BUMP TO NEXT RECORD NUMBER                   
         AHI   R1,1                                                             
         STCM  R1,3,LSTTRECN                                                    
         BRCT  R0,RLSELE04         DO FOR NUMBER OF ENTRIES ON SCREEN           
                                                                                
         BCTR  R1,0                                                             
         STCM  R1,3,LSTTRECN                                                    
         TM    CSLTINDS,CSLTIEOF   TEST END OF FILE REACHED                     
         JZ    RLNXTR02                                                         
         CLC   LSTTRECN,CSHIRECN   TEST THIS IS LAST RECORD                     
         JNE   RLNXTR02                                                         
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         DROP  R6                                                               
                                                                                
RLNXTR02 TM    CSLTINDS,CSLTIANY   TEST ANY SELECTIONS THIS PAGE                
         JZ    RLSCROLL                                                         
         TM    TWARLI1,TWARLIRL    SET REFRESH LIST PENDING                     
         JZ    RLNXTR04                                                         
         TM    CSLTINDS,CSLTIEOL   TEST SELECT TO END OF LIST                   
         JNZ   RLNXTR04                                                         
         NI    TWARLI1,FF-(TWARLIRL)                                            
         OI    CSLTINDS,CSLTIFST   TURN OFF REFRESH/TURN ON FIRST               
         XC    RFPVGRP,RFPVGRP     RE-VALIDATE THE GROUP                        
         J     RLSCROLL                                                         
                                                                                
RLNXTR04 TM    CSLTINDS,CSLTIEOL   TEST PROCESS TO END OF LIST                  
         JNZ   RLSCROLL                                                         
                                                                                
RLNXTR06 XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         LA    R0,RLIACT1H                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$ACENR)                                           
         MVI   FVOMTYP,GTMINF                                                   
         J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* HANDLE SCROLLING                                                    *         
***********************************************************************         
                                                                                
RLSCROLL BASE  ,                                                                
         MVC   RLPAG#LO,CSPAG#LO   SAVE LAST LOW & HIGH RECORDS                 
         MVC   RLPAG#HI,CSPAG#HI                                                
         XC    CSPAG#LO,CSPAG#LO   CLEAR LOW & HIGH VALUES FOR PAGE             
         XC    CSPAG#HI,CSPAG#HI                                                
         TM    CSLTINDS,CSLTIFST   TEST FIRST FOR LIST                          
         BZ    RLSCRO02                                                         
         MVI   CSLTINDS,0          CLEAR LIST INDICATORS                        
         MVI   CSLSTNUM,0          CLEAR NUMBER OF LIST ENTRIES                 
         MVC   CSHIRECN,CSPSRECN   RESET HIGH RECORD NUMBER                     
         XC    RLSAVE(RLSAVEL),RLSAVE CLEAR KEY                                 
         XC    RFPFRQID,RFPFRQID   CLEAR REQUEST KEY FIELDS                     
         XC    RFPFSORT,RFPFSORT                                                
         XC    RFPFSEQN,RFPFSEQN                                                
         B     RLSCRO16                                                         
                                                                                
RLSCRO02 MVC   RLSCRNUM,PCSCRNUM   SET TUSER SCROLL AMOUNT                      
         TM    CSLTINDS,CSLTIHLD   TEST HOLD PAGE IF DISPLAY CHANGED            
         BZ    RLSCRO04                                                         
         MVC   CSPAG#LO,RLPAG#LO   RESTORE LAST LOW & HIGH RECORDS              
         MVC   CSPAG#HI,RLPAG#HI                                                
         B     RLDISPLY                                                         
                                                                                
RLSCRO04 TM    PCSCROLL,PFKIUPDN   TEST SCROLL UP OR DOWN                       
         BZ    RLSCRO08                                                         
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,CSPSRECN                                                    
         TM    RLSCRNUM,PFKIMAXN   TEST SCROLL FIRST                            
         BNZ   RLSCRO06                                                         
         LHI   RF,RLISTMAX         SCROLL UP (BACKWARDS)                        
         TM    RLSCRNUM,PFKIHALF                                                
         BZ    *+8                                                              
         SRL   RF,1                                                             
         TM    RLSCRNUM,X'0F'                                                   
         BZ    *+8                                                              
         IC    RF,RLSCRNUM                                                      
         AHI   RF,1                                                             
         SR    RE,RE                                                            
         ICM   RE,3,RLPAG#LO                                                    
         SR    RE,RF               BACK-UP TO RECORD NUMBER-1                   
         BM    *+12                                                             
         CLM   RE,3,CSPSRECN       TEST NOT < LOW RECORD FOR SESSION            
         BNL   RLSCRO06                                                         
         SR    RE,RE               SET TO START FROM LOW RECORD                 
         ICM   RE,3,CSPSRECN                                                    
                                                                                
RLSCRO06 STCM  RE,3,LSTTRECN                                                    
         MVI   CSLSTNUM,0                                                       
         B     RLSCRO12                                                         
                                                                                
RLSCRO08 SR    R1,R1                                                            
         ICM   R1,1,CSLSTNUM       PICK UP NUMBER OF ENTRIES IN PAGE            
         BZ    REQLSTX                                                          
         MVI   CSLSTNUM,0                                                       
         TM    RLSCRNUM,X'0F'      TEST SCROLL AMOUNT SPECIFIED                 
         BZ    *+16                                                             
         CLM   R1,1,RLSCRNUM       TEST SCROLL EXCEEDS ACTUAL AMOUNT            
         BL    *+8                                                              
         IC    R1,RLSCRNUM                                                      
         TM    RLSCRNUM,PFKIHALF   TEST HALF PAGE SCROLL                        
         BZ    *+8                                                              
         SRL   R1,1                                                             
         AHI   R1,-1                                                            
         BM    RLSCRO10                                                         
         SR    R0,R0                                                            
         ICM   R0,3,RLPAG#LO                                                    
         AR    R1,R0                                                            
         STCM  R1,3,LSTTRECN                                                    
         TM    CSLTINDS,CSLTIEOF   TEST END OF FILE ENCOUNTERED                 
         BZ    RLSCRO12                                                         
         CLC   LSTTRECN,CSHIRECN   TEST LAST RECORD DISPLAYED                   
         BL    RLSCRO12                                                         
                                                                                
RLSCRO10 MVC   LSTTRECN,CSPSRECN   SET TO DISPLAY FIRST PAGE                    
                                                                                
RLSCRO12 SR    RE,RE               BUMP TO NEXT RECORD                          
         ICM   RE,3,LSTTRECN                                                    
         AHI   RE,1                                                             
         STCM  RE,3,LSTTRECN                                                    
         CLC   LSTTRECN,CSHIRECN   TEST IN TSAR BUFFER                          
         BH    RLSCRO14            NO - GET NEXT REQUEST RECORD                 
         GOTOR LSTADD,1            ADD ENTRY TO LSTTAB                          
         BE    RLSCRO12                                                         
         B     RLDISPLY                                                         
                                                                                
RLSCRO14 TM    CSLTINDS,CSLTIEOF   TEST EOF ENCOUNTERED                         
         BNZ   RLDISPLY                                                         
         MVC   RFPFRQID,RLFRQID                                                 
         MVC   RFPFSORT,RLSORT                                                  
         MVC   RFPFSEQN,RLSEQN                                                  
                                                                                
RLSCRO16 OI    RFPFFLAG,RFPF1STR                                                
                                                                                
RLSCRO18 MVC   CUUSER,REQLUSER                                                  
         MVC   CUAALF,REQLAGY                                                   
         MVC   CUSYSL,REQLSYST                                                  
         LA    R1,GROUP.GRPLGRP                                                 
                                                                                
         TM    CSINDSL1,CSIUSELC   TEST USING A LIST                            
         BNZ   RLSCRO20                                                         
                                                                                
         MVC   CUUSER,TWAUSRID                                                  
         MVC   CUAALF,TWAAGY                                                    
         MVC   CUSYSL,ASSYSL                                                    
         LA    R1,RLLGRPC                                                       
                                                                                
RLSCRO20 ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP                                                          
         ORG   *-2                                                              
         CLC   RFPVGRP,0(R1)       VALIDATE GROUP IF NECESSARY                  
         BE    RLSCRO22                                                         
         GOTOR (RF)                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXIT                                                             
         OI    RFPFFLAG,RFPF1STR                                                
                                                                                
RLSCRO22 MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPRETRQ    SET TO RETURN REQUEST CARDS                  
         GOTOR VRFPIO,PCPARM,RFPD                                               
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXIT                                                             
                                                                                
         CLI   RFPRET,RFPEOF       TEST END OF REQUESTS                         
         BNE   *+12                                                             
         OI    CSLTINDS,CSLTIEOF   SET END OF FILE                              
         B     RLDISPLY                                                         
                                                                                
         CLI   RLORUNF,0           TEST RUN FILTER SET                          
         BE    RLSCRO24                                                         
         CLC   RFPVREQD,RLORUNF    MATCH FILTER VALUE TO REQUEST                
         BNE   RLSCRO18                                                         
                                                                                
RLSCRO24 XC    LSTTABD(LSTTABL),LSTTABD                                         
         GOTOR LSTBLD              BUILD LIST ENTRY/TEST SECURITY               
         BNE   RLSCRO18                                                         
         SR    RE,RE                                                            
         ICM   RE,3,CSHIRECN                                                    
         AHI   RE,1                                                             
         STCM  RE,3,LSTTRECN                                                    
         GOTOR LSTADD,0            ADD ENTRY TO LIST                            
         BE    RLSCRO18                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY PAGE OF DATA                                                *         
***********************************************************************         
                                                                                
RLDISPLY LA    R6,RLIACTNH                                                      
         USING LSTD,R6             R6=A(SCREEN LINE)                            
         LHI   R0,RLISTMAX         R0=N'REQUESTS ON SCREEN                      
RLDISP02 XC    LSTACT,LSTACT                                                    
         OI    LSTACTH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    LSTREQR,LSTREQR                                                  
         OI    LSTREQRH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    LSTOTYP,LSTOTYP                                                  
         OI    LSTOTYPH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    LSTDEST,LSTDEST                                                  
         OI    LSTDESTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    LSTREQ1,LSTREQ1                                                  
         OI    LSTREQ1H+(FVOIND-FVIHDR),FVOXMT                                  
         XC    LSTREQ2,LSTREQ2                                                  
         OI    LSTREQ2H+(FVOIND-FVIHDR),FVOXMT                                  
         XC    LSTDSC,LSTDSC                                                    
         OI    LSTDSCH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    LSTSTAT,LSTSTAT                                                  
         OI    LSTSTATH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R6,LSTNEXT                                                       
         BCT   R0,RLDISP02                                                      
         DROP  R6                                                               
                                                                                
RLDISP04 CLC   CSHIRECN,CSPSRECN   TEST ANY RECORDS FOUND                       
         BNE   RLDISP06                                                         
         MVC   FVMSGNO,=AL2(GI$NLENR)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R0,RLIGRPH                                                       
         ST    R0,FVADDR                                                        
         J     EXIT                                                             
                                                                                
RLDISP06 MVC   LSTTRECN,CSPAG#LO                                                
         CLC   LSTTRECN,CSSEL#HI   TEST > HIGH MULTIPLE SELECT                  
         BNH   *+14                                                             
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LINES TO DISPLAY                
         BNZ   *+6                                                              
         DC    H'0'                DIE FOR NOW IF NO DISPLAY LINES              
         LA    R6,RLIACTNH                                                      
         USING LSTD,R6             R6=A(SCREEN LINE)                            
RLDISP08 GOTOR ATSARIO,TSAGET      GET REQUEST LIST RECORD                      
         GOTOR BLDLIN,LSTD                                                      
         JH    EXIT                                                             
         BE    *+12                CAN'T GET RECORD - REFRESH DISPLAY           
         OI    CSLTINDS,CSLTIFST                                                
         J     RLSCROLL                                                         
         MVI   LSTACTH+(FVILEN-FVIHDR),0                                        
         LA    R6,LSTNEXT                                                       
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN                                                    
         AHI   R1,1                                                             
         STCM  R1,3,LSTTRECN                                                    
         BCT   R0,RLDISP08                                                      
         BCTR  R1,0                                                             
         STCM  R1,3,LSTTRECN                                                    
         CLC   LSTTRECN,CSSEL#LO   TEST < LOW MULTIPLE SELECT                   
         BNL   *+14                                                             
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         TM    CSLTINDS,CSLTIEOL   TEST SELECT TO END OF LIST                   
         JNZ   RLSELE02                                                         
         DROP  R6                                                               
                                                                                
REQLSTX  LA    R0,RLIACT1H                                                      
         ST    R0,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$LDSOE)                                           
         TM    CSLTINDS,CSLTIEOF   TEST END-OF-FILE ENCOUNTERED                 
         JZ    EXIT                                                             
         CLC   LSTTRECN,CSHIRECN   TEST LAST RECORD DISPLAYED                   
         JNE   EXIT                                                             
         MVC   FVMSGNO,=AL2(GI$ELSEF)                                           
         J     EXIT                                                             
         DROP  RB,RC                                                            
                                                                                
RLISTMAX EQU   4                   N'LINES ON LIST SCREEN                       
                                                                                
RLWORKD  DSECT                     ** REQLST LOCAL W/S **                       
RLBYTE1  DS    XL1                 WORK BYTE 1                                  
RLBYTE2  DS    XL1                 WORK BYTE 2                                  
RLSCRNUM DS    XL1                 SCROLL MAGNITUDE                             
RLOPTS   DS    (RLVALSL)X          KEY & OPTIONS                                
RLFLAG   DS    XL1                 FLAG BYTE                                    
RLFINPT  EQU   X'80'               USER INPUT IN SELECT FIELD                   
RLFIPFK  EQU   X'40'               USER INPUT PFKEY ON THIS LINE                
RLFNEXT  EQU   X'20'               NOT FIRST TIME FOR CURRENT RECORD            
RLFSPLT  EQU   X'10'               SPLIT INPUT FIELD                            
RLFCHNG  EQU   X'08'               REQUEST LINE HAS CHANGED                     
RLPAG#LO DS    XL2                 LOW RECORD ON CURRENT PAGE                   
RLPAG#HI DS    XL2                 HIGH RECORD ON CURRENT PAGE                  
RLMASK   DS    XL(L'LSTTMASK)      VALID ACTION MASK WORK AREA                  
RLASEL   DS    XL4                 A(UNIQUE SELECT TABLE ACTION)                
RLP02    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CREATE A REQUEST LIST TABLE ELEMENT                      *         
*                                                                     *         
* NTRY - R1=ZERO TO CREATE AN ENTRY, NON-ZERO TO POST AN ENTRY        *         
*        IOKEY CONTAINS REQUEST RECORD KEY (IF R1=ZERO)               *         
*        LSTTRECN IS RECORD NUMBER OR LIST ENTRY (IF R1=NON-ZERO)     *         
*        CSLSTNUM IS NUMBER OF ENTRIES IN PAGE SO FAR                 *         
* EXIT - CC=NOT EQUAL IF PAGE FULL - RLNXTKEY IS NEXT REQUEST KEY     *         
***********************************************************************         
                                                                                
LSTADD   NTR1  BASE=*,LABEL=*                                                   
         LTR   R1,R1               SAVE REQUEST KEY IF ADDING TO LIST           
         BNZ   LSTADD02                                                         
         MVC   RLFRQID,RFPVRQID                                                 
         MVC   RLSORT,RFPVSORT                                                  
         MVC   RLSEQN,RFPVSEQN                                                  
                                                                                
LSTADD02 SR    RE,RE                                                            
         ICM   RE,1,CSLSTNUM                                                    
         LA    R0,1(RE)                                                         
         CHI   R0,RLISTMAX         TEST TABLE FULL                              
         BH    LSTADDN                                                          
         STC   R0,CSLSTNUM                                                      
         LTR   R1,R1               TEST CREATE/POST                             
         BNZ   LSTADD04                                                         
         GOTOR ATSARIO,TSAADD      ADD LIST ENTRY TO TSAR                       
                                                                                
LSTADD04 OC    CSPAG#LO,CSPAG#LO   SET LOW & HIGH RECORDS FOR PAGE              
         BNZ   *+10                                                             
         MVC   CSPAG#LO,LSTTRECN                                                
         MVC   CSPAG#HI,LSTTRECN                                                
                                                                                
LSTADDY  J     EXITY               SET CC=EQUAL                                 
                                                                                
LSTADDN  J     EXITN               SET CC=NOT EQUAL FOR FULL PAGE               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD LIST TABLE ENTRY                                   *         
***********************************************************************         
                                                                                
LSTBLD   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   LSTTRTYP,RECREQ                                                  
         MVC   REQLAGY,RFPVAGY                                                  
         MVC   REQLUSER,RFPVUSER                                                
         MVC   REQLSYST,RFPVSYST                                                
         MVC   REQLGRP,RFPVGRP                                                  
         MVC   REQLRQID,RFPVRQID                                                
         MVC   REQLSORT,RFPVSORT                                                
         MVC   REQLSEQN,RFPVSEQN                                                
         MVC   REQLRQSC,RFPVRQSC                                                
         MVC   REQLRQSN,RFPVRQSN                                                
         MVC   REQLRSTA,RFPVREQD                                                
         MVC   REQLSTAT,RFPVSTAT                                                
         MVC   REQLOTYP,RQHOUT                                                  
         MVC   REQLDEST,RQHDEST                                                 
         MVC   REQLNUMC,RFPVNUMC                                                
         MVC   REQLRDSC,RFPVRDSC                                                
         MVC   REQLFXID,RFPVFXID                                                
                                                                                
LSTBLD02 MVC   REQLREQR,PCSPACES                                                
         SR    RE,RE                                                            
         ICM   RE,1,RFPVRQSN                                                    
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         LHI   RF,L'RFPVREQC                                                    
         MR    RE,RE                                                            
         LA    RF,RFPVREQC-1(RF)                                                
         IC    RE,RFPVRQSC                                                      
         AR    RF,RE                                                            
         LHI   RE,L'REQLREQR-1                                                  
         TM    RFPVSTAT,RFPVSPOF                                                
         BZ    *+8                                                              
         LHI   RE,3-1                                                           
         EX    RE,*+8                                                           
         B     LSTBLDY                                                          
         MVC   REQLREQR(0),0(RF)                                                
                                                                                
LSTBLDY  J     EXITY                                                            
                                                                                
LSTBLDN  J     EXITN                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A DISPLAY LINE                                     *         
***********************************************************************         
                                                                                
         USING LSTD,R6                                                          
BLDLIN   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   CUUSER,REQLUSER                                                  
         MVC   CUAALF,REQLAGY                                                   
         MVC   CUSYSL,REQLSYST                                                  
         LA    R1,REQLGRP                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP                                                          
         ORG   *-2                                                              
         CLC   RFPVGRP,0(R1)                                                    
         BE    BLDLIN02                                                         
         GOTOR (RF)                                                             
         BE    BLDLIN02                                                         
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXITH                                                            
                                                                                
BLDLIN02 LA    R0,RFPVREQH                                                      
         LHI   R1,L'RFPVREQH+(RFPVMAXC*L'RFPVREQC)                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   RFPFRQID,REQLRQID                                                
         MVC   RFPFSORT,REQLSORT                                                
         MVC   RFPFSEQN,REQLSEQN                                                
         OI    RFPFFLAG,RFPF1STR                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPRETRQ                                                 
         GOTOR VRFPIO,PCPARM,RFPD                                               
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         J     EXITH                                                            
                                                                                
         CLI   RFPRET,RFPEOF       TEST END OF REQUESTS                         
         JE    EXITL                                                            
                                                                                
         MVC   LSTREQR,REQLREQR                                                 
         OI    LSTREQRH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
         MVC   LSTOTYP,REQLOTYP                                                 
         OI    LSTOTYPH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
         XC    LSTDEST,LSTDEST                                                  
         OI    LSTDESTH+(FVOIND-FVIHDR),FVOXMT                                  
         OC    REQLDEST,REQLDEST                                                
         BZ    BLDLIN06                                                         
         OC    REQLFXID,REQLFXID                                                
         BZ    BLDLIN04                                                         
         MVC   LSTDEST(L'PCUFAXP),PCUFAXP                                       
         MVC   LSTDEST+L'PCUFAXP(L'REQLFXID),REQLFXID                           
         B     BLDLIN06                                                         
                                                                                
BLDLIN04 GOTOR AGETUID,REQLDEST                                                 
         MVC   LSTDEST,PCWORK+(GIDCODE-GIDTABD)                                 
                                                                                
BLDLIN06 OI    LSTDESTH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   LSTDSC,REQLRDSC                                                  
                                                                                
         SR    R0,R0                                                            
         TM    RFPVSTAT,RFPVSPOF   TEST SPOOF REQUEST                           
         BZ    *+12                                                             
         LHI   R0,X'40'                                                         
         B     BLDLIN08                                                         
         TM    RFPVSTAT,RFPVRUNR   TEST RUNNER REQUEST                          
         BZ    *+12                                                             
         LHI   R0,X'10'                                                         
         B     BLDLIN08                                                         
                                                                                
         CLI   RLODATE,0           TEST DATES=REAL SPECIFIED                    
         BE    BLDLIN08                                                         
         GOTOR AGETNXT             TEST NEXT RUN DATE FOR GROUP SET             
         BNE   BLDLIN08                                                         
         GOTOR VDATCON,PCPARM,(6,PCWORK),(0,PCDUB)                              
         GOTOR VREQRFP,PCPARM,(X'20',RFPVREQH),(RFPFSYS,ACOM),         *        
               (CULANG,0),(2,0),(X'FF',ARFPBUFF),PCDUB                          
                                                                                
BLDLIN08 GOTOR VREQRFP,PCPARM,((R0),RFPVREQH),(RFPFSYS,ACOM),          *        
               (CULANG,0),(2,0),(X'FF',ARFPBUFF)                                
         OI    LSTREQ1H+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   LSTREQ1,RFPVREQC                                                 
         OI    LSTREQ2H+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   LSTREQ2,RFPVREQC+L'RFPVREQC                                      
                                                                                
         OI    LSTSTATH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   LSTSTAT(L'PCUYES),PCMYES                                         
         CLI   RFPVREQD,REQLRSRD                                                
         BNE   BLDLINX                                                          
         MVC   LSTSTAT(L'PCUNO),PCMNO                                           
                                                                                
BLDLINX  J     EXITY                                                            
         DROP  R6,RB                                                            
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE REQUEST OPTIONS                                            *         
***********************************************************************         
                                                                                
RLOVAL   NTR1  BASE=*,LABEL=*                                                   
         BASR  R7,0                                                             
         AHI   R7,GLOBALS-*                                                     
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
                                                                                
         B     RLOVRUNF            RUN?=YES/NO                                  
         B     RLOVDATE            DATES=REAL                                   
                                                                                
RLOVALX  J     EXIT                                                             
                                                                                
***********************************************************************         
* VALIDATE RUN?=YES/NO                                                *         
***********************************************************************         
                                                                                
RLOVRUNF GOTOR AVALRUN,FVIHDR      VALIDATE RUN OPTION                          
         B     RLOVALX                                                          
                                                                                
***********************************************************************         
* VALIDATE DATES=REAL                                                 *         
***********************************************************************         
                                                                                
RLOVDATE SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    RLOVDAT2                                                         
         CLC   FVIFLD(0),PCUREAL                                                
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     RLOVALX                                                          
                                                                                
RLOVDAT2 MVI   PCWORK,1                                                         
         B     RLOVALX                                                          
         DROP  RB                                                               
         EJECT                                                                  
CONLETQ  EQU   C'C'                CONTROL SYSTEM LETTER                        
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
                                                                                
BZEROES  DC    XL4'00'                                                          
                                                                                
RLOTAB   DS    0X                  ** REQUEST LIST OPTION TABLE **              
                                                                                
         DC    AL2(PCURUNQ-WORKD,PCURUNQ-WORKD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,L'PCUYES,L'RLORUNF)                              
         DC    AL1(1)                                                           
         DC    AL2(1,RLORUNF-RLVALS)                                            
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(PCUDATES-WORKD,PCUDATES-WORKD)                               
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,L'PCUREAL,L'RLODATE)                             
         DC    AL1(2)                                                           
         DC    AL2(2,RLODATE-RLVALS)                                            
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
                                                                                
RLOTABX  DC    AL1(EOT)                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* GERLPWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE GERLPWORK                                                      
         PRINT ON                                                               
                                                                                
TWAD     DSECT                     ** TWAD DEFINITIONS **                       
         ORG   RLPOLY1H                                                         
       ++INCLUDE GERLPFBD          REQUEST/LIST SCREEN                          
         ORG   RLPOLY1H                                                         
       ++INCLUDE GERLPFAD          REQUEST/GLOBAL SCREEN                        
                                                                                
         ORG   OSVALS                                                           
                                                                                
RLLGNUMR DS    XL(L'RFPVNREQ)      LAST TIME GROUP NUMBER OF REQUESTS           
RLLGRPC  DS    CL(L'GRPKGRP)       LAST TIME GROUP CODE (LIST)                  
RGLGRPC  DS    CL(L'GRPKGRP)       LAST TIME GROUP CODE (GLOBAL)                
                                                                                
RLSAVE   DS    0X                  ** LIST KEY **                               
RLFRQID  DS    CL(L'RFPFRQID)      REQUEST ID                                   
RLSORT   DS    CL(L'RFPFSORT)      SORT SEQUENCE                                
RLSEQN   DS    XL(L'RFPFSEQN)      SEQUENCE NUMBER                              
RLRHSVAL DS    XL(L'GRPKGRP)       RIGHT HAND SIDE VALUE                        
RLSAVEL  EQU   *-RLSAVE                                                         
                                                                                
RLVALS   DS    0X                  ** REQUEST/LIST VALUES **                    
                                                                                
RLOKOPT  DS    0X                  ** KEY OPTIONS **                            
                                                                                
RLORUNF  DS    XL1                 RUN FILTER                                   
                                                                                
RLODATE  DS    XL1                 DATE DISPLAY                                 
                                                                                
RLOKOPTL EQU   *-RLVALS                                                         
                                                                                
RLODOPT  DS    0X                  ** DISPLAY OPTIONS **                        
                                                                                
RLODOPTL EQU   *-RLODOPT                                                        
                                                                                
RLVALSL  EQU   *-RLVALS                                                         
                                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
                                                                                
LSTD     DSECT                     ** DSECT TO COVER TWA LIST LINE **           
         DS    CL(L'RLIACTNH)                                                   
         DS    CL(L'RLIACTN)                                                    
LSTACTH  DS    CL(L'RLIACT1H)                                                   
LSTACT   DS    CL(L'RLIACT1)       ACTION FIELD                                 
         DS    CL(L'RLIACT1X)                                                   
         DS    CL(L'RLIRQRNH)                                                   
         DS    CL(L'RLIRQRN)                                                    
LSTREQRH DS    CL(L'RLIRQR1H)                                                   
LSTREQR  DS    CL(L'RLIRQR1)       REQUESTOR FIELD                              
         DS    CL(L'RLIRQR1X)                                                   
         DS    CL(L'RLIOUTNH)                                                   
         DS    CL(L'RLIOUTN)                                                    
LSTOTYPH DS    CL(L'RLIOUT1H)                                                   
LSTOTYP  DS    CL(L'RLIOUT1)       OUTPUT TYPE FIELD                            
         DS    CL(L'RLIOUT1X)                                                   
         DS    CL(L'RLIDSTNH)                                                   
         DS    CL(L'RLIDSTN)                                                    
LSTDESTH DS    CL(L'RLIDST1H)                                                   
LSTDEST  DS    CL(L'RLIDST1)       DESTINATION ID FIELD                         
         DS    CL(L'RLIDST1X)                                                   
LSTREQ1H DS    CL(L'RLIREQ1H)                                                   
LSTREQ1  DS    CL(L'RLIREQ1)       REQUEST DISPLAY LINE#1                       
LSTREQ2H DS    CL(L'RLIREQ2H)                                                   
LSTREQ2  DS    CL(L'RLIREQ2)       REQUEST DISPLAY LINE#2                       
         DS    CL(L'RLIDSCNH)                                                   
         DS    CL(L'RLIDSCN)                                                    
LSTDSCH  DS    CL(L'RLIDSC1H)                                                   
LSTDSC   DS    CL(L'RLIDSC1)       DESCRIPTION FIELD                            
         DS    CL(L'RLIDSC1X)                                                   
         DS    CL(L'RLIRUNNH)                                                   
         DS    CL(L'RLIRUNN)                                                    
LSTSTATH DS    CL(L'RLIRUN1H)                                                   
LSTSTAT  DS    CL(L'RLIRUN1)       RUN STATUS                                   
         DS    CL(L'RLIRUN1X)                                                   
LSTNEXT  EQU   *                                                                
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010GERLP02   06/22/07'                                      
         END                                                                    
