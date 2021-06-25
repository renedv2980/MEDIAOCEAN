*          DATA SET REVIEW01   AT LEVEL 013 AS OF 05/01/02                      
*          DATA SET REVIEW01   AT LEVEL 011 AS OF 01/07/98                      
*&&      SET   NOP=N                                                            
*PHASE T81701A                                                                  
REVIEW01 TITLE '- FILE PROGRAM ROUTINES'                                        
REVIEW01 CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL RTWORKL,REVIEW01,R7,CLEAR=YES,RR=RE                              
         USING RTWORKD,RC                                                       
         ST    RE,RTRELO                                                        
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         ST    R1,RTPARMA          SAVE A(CALLERS R1)                           
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0A                                                               
         B     DSTN                DISPLAY STATION NAME                         
         B     VSTN                VALIDATE STATION NAME                        
         B     VGRP                VALIDATE GROUP                               
         B     DOFF                DISPLAY OFFICE                               
         B     VOFF                VALIDATE OFFICE                              
         B     NXKEY               NEXT IN LIST                                 
         DC    10AL4(0)                                                         
*                                                                               
EXITL    MVI   GCDUB1,0            SET CC LOW                                   
         B     EXITCC                                                           
EXITH    MVI   GCDUB1,2            SET CC HIGH                                  
         B     EXITCC                                                           
EXITOK   MVI   GCDUB1,1            SET CC EQUAL                                 
EXITCC   CLI   GCDUB1,1                                                         
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY STATION NAME INTO BOWORK1                                   *         
*                                                                     *         
* NTRY: P1       = STATION CODE                                       *         
* EXIT: BOWORK1  = STATION NAME                                       *         
***********************************************************************         
         SPACE 1                                                                
DSTN     L     R2,0(R1)                                                         
         MVC   BOWORK1,BCSPACES                                                 
         CLC   0(5,R2),BCEFFS                                                   
         BNE   *+14                                                             
         MVC   BOWORK1(L'ALL),ALL                                               
         B     EXITOK                                                           
*                                                                               
         MVC   BOWORK1(4),0(R2)                                                 
*&&NOP                                                                          
         MVI   BOWORK1+4,C'-'                                                   
*                                                                               
         LA    RF,MEDTAB                                                        
DSTN02   CLI   0(RF),0                                                          
         BE    DSTNX                                                            
         CLC   4(1,R2),0(RF)                                                    
         BE    *+12                                                             
         LA    RF,2(RF)                                                         
         B     DSTN02                                                           
*                                                                               
         MVC   BOWORK1+5(2),0(RF)                                               
*&&                                                                             
DSTNX    B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE STATION NAME INTO BOWORK1                                  *         
*                                                                     *         
* NTRY: FVIFLD   = STATION NAME                                       *         
* EXIT: BOWORK1  = STATION CODE                                       *         
***********************************************************************         
         SPACE 1                                                                
VSTN     XC    BOWORK2,BOWORK2                                                  
         MVC   BOWORK1,BCSPACES                                                 
         CLI   FVILEN,3                                                         
         BNE   VSTA01                                                           
         CLC   =C'ALL',FVIFLD                                                   
         BNE   VSTA01                                                           
         MVC   BOWORK1(L'REK3STN),BCEFFS                                        
         B     EXITOK                                                           
*                                                                               
VSTA01   GOTOX VSCANNER,RTPARM,(C'C',FVIFLD),(2,BOWORK2),C',=,-'                
         LA    R4,BOWORK2                                                       
         USING SCANBLKD,R4                                                      
         CLI   SC1STLEN,3                                                       
         BL    VSTAN                                                            
         CLI   SC1STLEN,6                                                       
         BH    VSTAN                                                            
         LA    R5,3                                                             
         CLI   SC1STLEN,3                                                       
         BNE   *+6                                                              
         BCTR  R5,0                                                             
         EX    R5,STAMOVE          SAVE CALL LETTERS                            
*                                                                               
         LA    R5,BOWORK1+4                                                     
         CLI   SC1STLEN,4          SET STATION?                                 
         BNH   VSTA02              NO                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         SH    RF,=H'5'                                                         
         LA    R1,SC1STFLD+4                                                    
         BAS   RE,VALMED                                                        
         CLI   SC2NDLEN,0                                                       
         BE    VSTA04                                                           
         B     VSTAN                                                            
*                                                                               
STAMOVE  MVC   BOWORK1(0),SC1STFLD                                              
*                                                                               
VSTA02   CLI   SC2NDLEN,0          DEFINED MEDIA                                
         BNE   *+12                YES                                          
         LA    R6,=C'TV'                                                        
         B     VSTA04              DEFAULT TO TV                                
*                                                                               
         CLI   SC2NDLEN,2          MUST BE 2 CHARS OR LESS                      
         BH    VSTAN                                                            
         XR    RF,RF                                                            
         IC    RF,SC2NDLEN                                                      
         BCTR  RF,0                                                             
         LA    R1,SC2NDFLD                                                      
         BAS   RE,VALMED                                                        
*                                                                               
VSTA04   MVC   0(1,R5),0(R6)       SET MEDIA                                    
         CLC   =C'MR',CUAALF       SUPER-COMPANY CONNECTION?                    
         BE    VSTA06              YES                                          
*                                                                               
         XC    IOKEY,IOKEY         LOOK AT THIS - IT'S HORRIBLE!                
         MVI   IOKEY,2                                                          
         MVC   IOKEY+20(2),CUAALF                                               
         MVC   IOKEY+22(4),BOWORK1                                              
         CLI   IOKEY+25,C'-'                                                    
         BNE   *+8                                                              
         MVI   IOKEY+25,C' '                                                    
         MVC   IOKEY+26(1),0(R6)                                                
         CLI   0(R6),C'T'                                                       
         BNE   *+8                                                              
         MVI   IOKEY+26,C' '                                                    
         L     R1,=AL4(XOHIGH+XOREPDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   VSTAN               READ HIGH ERROR                              
         CLC   IOKEY(27),IOKEYSAV                                               
         BNE   VSTAN                                                            
         B     EXITOK              NO  - DON'T GET EXPANSION                    
*                                                                               
VSTA06   LA    R2,CMPTAB           REALLY SHODDY WAY TO DO THINGS...            
*                                                                               
VSTA08   CLI   0(R2),0             EOT                                          
         BE    VSTAN                                                            
*                                                                               
         XC    IOKEY,IOKEY         LOOK AT THIS - IT'S HORRIBLE!                
         MVI   IOKEY,2                                                          
         MVC   IOKEY+20(2),0(R2)   TRY FOR ALL COMPANIES                        
         MVC   IOKEY+22(4),BOWORK1                                              
         CLI   IOKEY+25,C'-'                                                    
         BNE   *+8                                                              
         MVI   IOKEY+25,C' '                                                    
         MVC   IOKEY+26(1),0(R6)                                                
         CLI   0(R6),C'T'                                                       
         BNE   *+8                                                              
         MVI   IOKEY+26,C' '                                                    
         L     R1,=AL4(XOHIGH+XOREPDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   VSTAN               READ HIGH ERROR                              
         CLC   IOKEY(27),IOKEYSAV                                               
         BE    EXITOK                                                           
         LA    R2,2(R2)                                                         
         B     VSTA08              NEXT COMPANY IN TABLE                        
*                                                                               
VSTAN    MVC   FVMSGNO,=AL2(RR#ISTTN)                                           
         B     EXITL                                                            
*                                                                               
VALMED   LA    R6,MEDTAB                                                        
*                                                                               
VALMED2  CLI   0(R6),0            END OF TABLE?                                 
         BE    VSTAN              YES - INVALID                                 
         EX    RF,*+6                                                           
         BER   RE                                                               
         CLC   0(0,R1),0(R6)                                                    
         LA    R6,2(R6)                                                         
         B     VALMED2                                                          
*                                                                               
CMPTAB   DC    CL2'AM'            MR PROVIDES SUPERSET FOR THESE                
         DC    CL2'CQ'                                                          
         DC    CL2'NK'                                                          
         DC    X'00'                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE GROUP CODE                                                 *         
*                                                                     *         
* NTRY: FVIFLD   = GROUP CODE                                         *         
***********************************************************************         
         SPACE 1                                                                
VGRP     XC    BOWORK1,BOWORK1                                                  
         CLI   FVILEN,3            GROUP=ALL?                                   
         BNE   VGRP01              NO                                           
         CLC   =C'ALL',FVIFLD                                                   
         BNE   VGRPN                                                            
         MVC   BOWORK1(2),BCEFFS                                                
         B     EXITOK                                                           
*                                                                               
VGRP01   CLI   FVILEN,1            LENGTH 1 GROUP?                              
         BNE   VGRP02              NO                                           
         MVC   BOWORK1(2),FVIFLD                                                
         CLI   BOWORK1,C'T'                                                     
         BE    EXITOK                                                           
         CLI   BOWORK1,C'R'                                                     
         BE    EXITOK                                                           
         CLI   BOWORK1,C'A'        INTNY                                        
         BE    EXITOK                                                           
         CLI   BOWORK1,C'I'        INTNY                                        
         BE    EXITOK                                                           
         CLI   BOWORK1,C'P'        PETRY                                        
         BE    EXITOK                                                           
         B     VGRPN               ERROR                                        
*                                                                               
VGRP02   CLI   FVILEN,2            LENGTH 2 GROUP?                              
         BNE   VGRPN               NO - ERROR                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
X        USING RGRPRECD,IOKEY      READ GROUP RECORD                            
         MVI   X.RGRPKTYP,7                                                     
         MVC   X.RGRPKREP,CUAALF                                                
         MVC   X.RGRPKGRP,FVIFLD                                                
         L     R1,=AL4(XOREAD+XOREPDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   VGRPN               RECORD NOT FOUND                             
         MVC   BOWORK1(L'RGRPKGRP),FVIFLD                                       
         B     EXITOK                                                           
         DROP  X                                                                
*                                                                               
VGRPN    MVC   FVMSGNO,=AL2(RR#IGRP)                                            
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE FILTER                                             *         
*                                                                     *         
* NTRY: FLTIFLD  = OFFICE CODE FILTER                                 *         
* EXIT: FVIFLD   = OFFICE CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFF     CLC   FLTIFLD(L'ROFFKOFF),BCEFFS                                       
         BNE   *+14                                                             
         MVC   FVIFLD(3),=C'ALL'                                                
         B     EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'ROFFKOFF),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE CODE AS A FILTER                                    *         
*                                                                     *         
* NTRY: FVIFLD   = OFFICE CODE                                        *         
* EXIT: BOWORK1  = OFFICE CODE FILTER FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
VOFF     CLC   =C'O=',TWAACCS      OFFICE ACCESS LIMITATION                     
         BNE   VOFF02                                                           
         TM    TWAAUTH,X'80'       OVERRIDE AUTHORISATION                       
         BO    VOFF02                                                           
*                                                                               
         CLI   FVILEN,0            SET DEFAULT                                  
         BNE   *+14                                                             
         MVI   FVILEN,2                                                         
         MVC   FVIFLD(2),TWAACCS+2                                              
*                                                                               
         CLI   FVILEN,2                                                         
         BNE   VOFFN                                                            
         CLC   FVIFLD(2),TWAACCS+2                                              
         BNE   VOFFN                                                            
*                                                                               
         MVC   BOWORK1(2),FVIFLD   SET VALUE TO FILTER                          
         OI    GCINDS1,GCIREDIS    SET TO REDISPLAY                             
         B     EXITOK                                                           
*                                                                               
VOFF02   CLI   FVILEN,3            OFFICE MUST BE 2 CHARACTER OR 'ALL'          
         BNE   VOFF06                                                           
         CLI   CSREC,X'16'         K6 REPORT ONLY HAS 'ALL' VALID               
         BE    VOFF04                                                           
         CLI   CSREC,X'21'         ALL 'W' REPORTS ALLOW OFFICE 'ALL'           
         BE    VOFF04                                                           
         CLI   CSREC,X'22'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'23'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'24'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'25'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'26'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'27'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'28'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'29'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'2A'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'2B'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'2C'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'2D'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'2E'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'2F'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'30'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'31'                                                      
         BE    VOFF04                                                           
         CLI   CSREC,X'32'                                                      
         BE    VOFF04                                                           
         B     VOFFN                                                            
VOFF04   CLC   =C'ALL',FVIFLD                                                   
         BNE   VOFFN                                                            
         MVC   BOWORK1(L'ROFFKOFF),BCEFFS                                       
         B     EXITOK                                                           
*                                                                               
VOFF06   CLI   FVILEN,L'ROFFKOFF   OFFICE MUST BE 2 CHARACTER                   
         BNE   VOFFN                                                            
*                                                                               
         XC    IOKEY,IOKEY                                                      
X        USING ROFFRECD,IOKEY      READ OFFICE RECORD                           
         MVI   X.ROFFKTYP,4        THIS IS AWFUL                                
         MVC   X.ROFFKREP,CUAALF                                                
         MVC   X.ROFFKOFF,FVIFLD                                                
         L     R1,=AL4(XOREAD+XOREPDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   VOFFN               RECORD NOT FOUND                             
         MVC   BOWORK1(L'ROFFKOFF),FVIFLD                                       
         B     EXITOK                                                           
         DROP  X                                                                
*                                                                               
VOFFN    MVC   FVMSGNO,=AL2(RR#IOFF)                                            
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* KEY INTERLEAVE FOR LIST BUILD                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING NXKEYD,RTWORK                                                    
*                                                                               
NXKEY    MVC   NXOLDKEY,IOKEY                                                   
         LR    R2,R1                                                            
         USING FILTABD,R2                                                       
*                                                                               
NEWK02   CLC   FILTNUM,=AL2(EOT)   END OF TABLE                                 
         BE    NXKEYX              YES                                          
*                                                                               
         XR    RF,RF               SEE IF THIS FILTER HAS BEEN SET              
         ICM   RF,3,FILTNUM                                                     
         GOTOX AGEN,RTPARM,OFILT,FFFLT,(RF)                                     
         BE    NEWK06                                                           
*                                                                               
NEWK04   LA    R2,FILTABLQ(R2)                                                  
         B     NEWK02                                                           
*                                                                               
NEWK06   GOTOX VHELLO,RTPARM,(C'G',CORETAB),('FLTELQ',AFLTELSV),       *        
               (L'FILTNUM,FILTNUM)                                              
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                NO WAY HOSE                                  
*                                                                               
         ICM   R3,15,12(R1)        GET THE CURRENT FLTEL                        
         USING FLTELD,R3                                                        
         CLI   FLTCNT,1            LIST OF FILTERS?                             
         BNE   NEWK22              YES                                          
*                                                                               
         TM    FLTIND2,FDRF2V1+FDRF2V2                                          
         BZ    NEWK10              NOT A RANGE                                  
*                                                                               
         XC    FLTIFLD,FLTIFLD     SET FIRST PART OF RANGE                      
         LA    R4,FLTDATA                                                       
         XR    RE,RE                                                            
         IC    RE,FLT1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FLTIFLD(0),0(R4)                                                 
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,FLTNUM                                                      
         GOTOX APRG,RTPARM,('GCBOVER',ODATA),(RF),DFDO,IOKEY,0,0                
         BE    *+6                                                              
         DC    H'0'                WHERE FILTER ROUTINE?                        
*                                                                               
         CLI   0(R1),DFLTX         DEFINATELY NOT WANTED FOR THE LIST           
         BNE   *+6                                                              
         DC    H'0'                NOT ALLOWED FOR SKIP READ                    
*                                                                               
         CLI   0(R1),DFLTE         RETURN CODE FROM APP. FILTER                 
         BL    NEWK16              TOO LOW FOR RANGE                            
*                                                                               
         XC    FLTIFLD,FLTIFLD     SET SECOND PART OF RANGE                     
         XR    RE,RE                                                            
         IC    RE,FLT1LEN                                                       
         LA    R4,0(RE,R4)                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FLTIFLD(0),0(R4)                                                 
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,FLTNUM                                                      
         GOTOX APRG,RTPARM,('GCBOVER',ODATA),(RF),DFDO,IOKEY,0,0                
         BE    *+6                                                              
         DC    H'0'                WHERE FILTER ROUTINE?                        
*                                                                               
         CLI   0(R1),DFLTX         DEFINATELY NOT WANTED FOR THE LIST           
         BNE   *+6                                                              
         DC    H'0'                NOT ALLOWED FOR SKIP READ                    
*                                                                               
         CLI   0(R1),DFLTE         RETURN CODE FROM APP. FILTER                 
         BL    NEWK04              WITHIN RANGE                                 
         BE    NEWK04              WITHIN RANGE                                 
*                                                                               
         LA    RF,IOKEY            IF HIGH - SET TO FIRST VALUE,                
         USING REK1DSCT,RF         ADD 1 TO BYTE IN FRONT,                      
         XR    RE,RE               CLEAR REMAINDER OF KEY                       
         IC    RE,FILTDSP                                                       
         LA    R1,0(RE,RF)                                                      
         BCTR  R1,0                BYTE IN FRONT OF THIS FILTER                 
*                                                                               
NEWK08   XR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(R1)            SET BYTE IN FRONT TO +1                      
         CLI   0(R1),0             SEE IF IT WAS X'FF'                          
         BNE   *+8                 NO                                           
         BCT   R1,NEWK08           PROPAGATE CARRY                              
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FILTDSP                                                       
         LA    R1,0(RE,RF)                                                      
*                                                                               
         IC    RE,FILTLEN          MOVE IN CURRENT FILTER                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),FLTDATA                                                  
*                                                                               
         LA    R1,1(RE,R1)                                                      
         LA    RE,IOKEY+L'IOKEY-1                                               
         CR    R1,RE               SET REMAINDER OF KEY TO X'00'                
         BH    NEWK04                                                           
         MVI   0(RE),0                                                          
         BCT   RE,*-10                                                          
         DC    H'0'                                                             
*                                                                               
NEWK10   XC    FLTIFLD,FLTIFLD     SET THIS DATA IN FILTER FIELD                
         XR    RE,RE                                                            
         IC    RE,FLT1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FLTIFLD(0),FLTDATA                                               
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,FLTNUM                                                      
         GOTOX APRG,RTPARM,('GCBOVER',ODATA),(RF),DFDO,IOKEY,0,0                
         BE    *+6                                                              
         DC    H'0'                WHERE FILTER ROUTINE?                        
*                                                                               
         CLI   0(R1),DFLTX         DEFINITELY NOT WANTED FOR THE LIST           
         BNE   *+6                                                              
         DC    H'0'                NOT ALLOWED FOR SKIP READ                    
*                                                                               
         TM    FLTIND1,FDRF1NOT                                                 
         BZ    NEWK14                                                           
*                                                                               
         CLI   0(R1),DFLTE         IGNORE 'NOT' FILTER UNLESS EQUAL             
         BL    NEWK04                                                           
         BH    NEWK04                                                           
*                                                                               
         XR    R1,R1               SET TO FILTER VALUE+1, OR HIGH               
         IC    R1,FILTDSP          VALUE+1 IF RANGE,                            
         LA    R1,IOKEY(R1)        CLEAR REMAINDER OF KEY                       
         XR    RE,RE                                                            
         IC    RE,FILTLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),FLTDATA                                                  
         LA    R1,0(RE,R1)                                                      
*                                                                               
NEWK12   XR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    RE,1(RE)            SET BYTE = BYTE+1                            
         STC   RE,0(R1)                                                         
         CLI   0(R1),0             WAS BYTE = X'FF'                             
         BNE   *+8                 NO                                           
         BCT   R1,NEWK12           PROPAGATE CARRY                              
*                                                                               
         XR    R1,R1               SET TO FILTER VALUE+1, OR HIGH               
         IC    R1,FILTDSP          VALUE+1 IF RANGE,                            
         LA    R1,IOKEY(R1)        CLEAR REMAINDER OF KEY                       
         XR    RE,RE                                                            
         IC    RE,FILTLEN                                                       
         LA    R1,0(RE,R1)         FIRST BYTE AFTER CURRENT FILTER              
         LA    RE,IOKEY+L'IOKEY-1  END OF KEY                                   
*                                                                               
         CR    R1,RE               SET REMAINDER OF KEY TO X'00'                
         BH    NEWK04                                                           
         MVI   0(RE),0                                                          
         BCT   RE,*-10                                                          
         B     NEWK04                                                           
*                                                                               
NEWK14   CLI   0(R1),DFLTE         RETURN CODE FROM APP. FILTER                 
         BL    NEWK16                                                           
         BE    NEWK04              IGNORE FILTER IF MATCH                       
         BH    NEWK18                                                           
*                                                                               
NEWK16   XR    R1,R1               IF LOW - SET TO FILTER VALUE, OR             
         IC    R1,FILTDSP                   FIRST VALUE IF RANGE,               
         LA    R1,IOKEY(R1)                 CLEAR REMAINDER OF KEY              
         XR    RE,RE                                                            
         IC    RE,FILTLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),FLTDATA                                                  
*                                                                               
         LA    R1,1(RE,R1)         FIRST BYTE AFTER CURRENT FILTER              
         LA    RE,IOKEY+L'IOKEY-1  END OF KEY                                   
*                                                                               
         CR    R1,RE               SET REMAINDER OF KEY TO X'00'                
         BH    NEWK04                                                           
         MVI   0(RE),0                                                          
         BCT   RE,*-10                                                          
*                                                                               
NEWK18   XR    R1,R1               IF HIGH - SET BYTE IN FRONT OF THIS          
         IC    R1,FILTDSP                    FILTER TO +1 & PROPAGATE           
         LA    R1,IOKEY(R1)                  ANY REQUIRED CARRY,                
         BCTR  R1,0                          SET TO FILTER VALUE, OR            
*                                            FIRST VALUE IF RANGE,              
NEWK20   XR    RE,RE                         CLEAR REMAINDER OF KEY             
         IC    RE,0(R1)                                                         
         LA    RE,1(RE)            SET BYTE = BYTE+1                            
         STC   RE,0(R1)                                                         
         CLI   0(R1),0             WAS BYTE = X'FF'                             
         BNE   *+8                 NO                                           
         BCT   R1,NEWK20           PROPAGATE CARRY                              
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FILTDSP                                                       
         LA    R1,IOKEY(RE)        R1= CURRENT FILTER IN KEY                    
         IC    RE,FILTLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),FLTDATA     MOVE IN CURRENT FILTER                       
*                                                                               
         LA    R1,1(RE,R1)                                                      
         LA    RE,IOKEY+L'IOKEY-1                                               
*                                                                               
         CR    R1,RE               SET REMAINDER OF KEY TO X'00'                
         BH    NEWK04                                                           
         MVI   0(RE),0                                                          
         BCT   RE,*-10                                                          
         DC    H'0'                                                             
*                                                                               
NEWK22   XR    R0,R0           *** LIST OF VALUES ***                           
         IC    R0,FLTCNT                                                        
         LA    R4,FLTINFO                                                       
FLT      USING FLTINFO,R4                                                       
*                                                                               
NEWK24   XC    FLTIFLD,FLTIFLD     SET NEXT DATA IN FILTER FIELD                
         XR    RE,RE                                                            
         IC    RE,FLT1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FLTIFLD(0),FLT.FLTDATA                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,FLTNUM                                                      
         GOTOX APRG,RTPARM,('GCBOVER',ODATA),(RF),DFDO,IOKEY,0,0                
         BE    *+6                                                              
         DC    H'0'                WHERE FILTER ROUTINE?                        
*                                                                               
         CLI   0(R1),DFLTX         DEFINITELY NOT WANTED FOR THE LIST           
         BNE   *+6                 NOT ALLOWED FOR SKIP READ                    
         DC    H'0'                                                             
*                                                                               
         TM    FLT.FLTIND2,FDRF2V1+FDRF2V2                                      
         BZ    NEWK32              NOT A RANGE - EASY THEN                      
         TM    FLT.FLTIND1,FDRF1NOT                                             
         BZ    NEWK26              'NOT' FILTER                                 
*                                                                               
         CLI   0(R1),DFLTE         RETURN CODE FOR 'NOT' FILTER                 
         BL    NEWK04              RETURNED LOW - THAT'S FINE                   
         B     NEWK28                                                           
*                                                                               
NEWK26   CLI   0(R1),DFLTE         RETURN CODE FOR NORMAL FILTER                
         BL    NEWK36              RETURNED LOW - CONTINUE                      
*                                                                               
NEWK28   XC    FLTIFLD,FLTIFLD     SET SECOND PART OF RANGE                     
         XR    RE,RE                                                            
         IC    RE,FLT1LEN                                                       
         LA    RF,FLT.FLTDATA(RE)  SECOND PART OF RANGE                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FLTIFLD(0),0(RF)                                                 
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,FLTNUM                                                      
         GOTOX APRG,RTPARM,('GCBOVER',ODATA),(RF),DFDO,IOKEY,0,0                
*                                                                               
         CLI   0(R1),DFLTX         DEFINITELY NOT WANTED FOR THE LIST           
         BNE   *+6                 NOT ALLOWED FOR SKIP READ                    
         DC    H'0'                                                             
*                                                                               
         TM    FLT.FLTIND1,FDRF1NOT                                             
         BZ    NEWK30              'NOT' FILTER                                 
*                                                                               
         CLI   0(R1),DFLTE         SECOND FILTER RETURN FOR 'NOT'               
         BH    NEWK04              RETURNED HIGH - THAT'S FINE                  
         B     NEWK36                                                           
*                                                                               
NEWK30   CLI   0(R1),DFLTE         RETURN CODE FROM SECOND FILTER               
         BH    NEWK36              RETURNED HIGH - CONTINUE                     
         B     NEWK04              WITHIN RANGE OF VALUES                       
*                                                                               
NEWK32   TM    FLT.FLTIND1,FDRF1NOT                                             
         BZ    NEWK34              NOT 'NOT' FILTER                             
*                                                                               
         CLI   0(R1),DFLTE         SINGLE 'NOT' FILTER                          
         BNE   NEWK04              RETURNED NOT EQUAL - IGNORE                  
         B     NEWK36                                                           
*                                                                               
NEWK34   CLI   0(R1),DFLTE         SINGLE FILTER                                
         BE    NEWK04              RETURNED EQUAL - IGNORE                      
*                                                                               
NEWK36   XR    RE,RE                                                            
         IC    RE,FLT1LEN                                                       
         TM    FLT.FLTIND2,FDRF2V1+FDRF2V2                                      
         BZ    *+8                                                              
         SLL   RE,1                TWICE AS BIG IF A RANGE                      
         LA    R4,L'FLTIND1+L'FLTIND2(RE,R4)                                    
         BCT   R0,NEWK24           DO FOR ALL WITHIN LIST                       
         DROP  FLT                                                              
*                                                                               
         XR    R0,R0               FIND CLOSEST VALUE ABOVE CURRENT             
         IC    R0,FLTCNT           NUMBER IN LIST                               
         XR    R5,R5               R5=CURRENT NEXT HIGHEST FILTER               
FLTSV    USING FLTINFO,R5                                                       
         LA    R4,FLTINFO          R4=CURRENT FILTER FROM LIST                  
FLT      USING FLTINFO,R4                                                       
         XR    RE,RE                                                            
         IC    RE,FILTDSP                                                       
         LA    R1,IOKEY(RE)                                                     
         B     NEWK38                                                           
*                                                                               
NXHILST  CLC   FLT.FLTDATA(0),FLTSV.FLTDATA                                     
NXHIKEY  CLC   0(0,R1),FLT.FLTDATA COMPARE KEY DATA WITH CURRENT FILTER         
*                                                                               
NEWK38   IC    RE,FLT1LEN          LENGTH OF A FILTER                           
         BCTR  RE,0                                                             
         EX    RE,NXHIKEY          SEE IF DATA > CURRENT KEY                    
         BH    NEWK40              KEY VALUE IS HIGHER THAN DATA                
*                                                                               
         LTR   R5,R5               PREVIOUS HIGHER FILTER?                      
         BNZ   *+6                 YES                                          
         LR    R5,R4                                                            
*                                                                               
         EX    RE,NXHILST          SEE IF THIS IS BETTER THAN CURRENT           
         BH    *+6                 NO                                           
         LR    R5,R4               NEW BEST MATCH                               
*                                                                               
NEWK40   IC    RE,FLT1LEN                                                       
         TM    FLT.FLTIND2,FDRF2V1+FDRF2V2                                      
         BZ    *+8                                                              
         SLL   RE,1                TWICE AS BIG IF A RANGE                      
         LA    R4,L'FLTIND1+L'FLTIND2(RE,R4)                                    
         BCT   R0,NEWK38                                                        
         DROP  FLT                                                              
*                                                                               
         LTR   R5,R5               HIGHER MATCH?                                
         BNZ   NEWK46              YES                                          
*                                                                               
         XR    RF,RF               FIND SMALLEST FILTER                         
         IC    RF,FLTCNT                                                        
         BCTR  RF,0                                                             
         LA    R4,FLTINFO                                                       
FLT      USING FLTINFO,R4                                                       
         LR    R5,R4                                                            
         BCTR  RE,0                                                             
*                                                                               
NEWK42   XR    RE,RE                                                            
         IC    RE,FLT1LEN                                                       
         EX    RE,NXHILST          LOOK FOR SMALLEST VALUE IN LIST              
         BH    *+6                                                              
         LR    R5,R4               NEW LOW VALUE                                
*                                                                               
         LA    RE,1(RE)                                                         
         TM    FLT.FLTIND2,FDRF2V1+FDRF2V2                                      
         BZ    *+8                                                              
         SLL   RE,1                TWICE AS BIG IF A RANGE                      
         LA    R4,L'FLTIND1+L'FLTIND2(RE,R4)                                    
         BCT   RF,NEWK42                                                        
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FILTDSP                                                       
         LA    R1,IOKEY(RE)                                                     
         BCTR  R1,0                BYTE IN FRONT OF THIS FILTER                 
*                                                                               
NEWK44   XR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(R1)            SET BYTE IN FRONT TO +1                      
         CLI   0(R1),0             IF THIS BYTE WAS X'FF'                       
         BNE   *+8                 NO                                           
         BCT   R1,NEWK44           PROPAGATE THE CARRY                          
         DROP  FLT                                                              
*                                                                               
FLTS     USING FLTINFO,R5                                                       
NEWK46   XR    RE,RE                                                            
         IC    RE,FILTDSP                                                       
         LA    R1,IOKEY(RE)        THIS FILTER                                  
*                                                                               
         IC    RE,FILTLEN          MOVE IN NEXT HIGHEST VALUE                   
         BCTR  RE,0                OR FIRST PART OF NEXT RANGE                  
         EX    RE,*+4                                                           
         MVC   0(0,R1),FLTS.FLTDATA                                             
*                                                                               
         LA    R1,1(RE,R1)                                                      
         LA    RE,IOKEY+L'IOKEY-1                                               
         CR    R1,RE               SET REMAINDER OF KEY TO X'00'                
         BH    NEWK04                                                           
         MVI   0(RE),0                                                          
         BCT   RE,*-10                                                          
         DC    H'0'                                                             
         DROP  FLTS                                                             
*                                                                               
NXKEYX   CLC   NXOLDKEY,IOKEY      KEY CHANGED?                                 
         BNE   EXITOK              YES                                          
*                                                                               
         LA    RF,IOKEY            SET TO GO TO NEXT LINE NUMBER                
         USING REK1DSCT,RF                                                      
         XR    RE,RE                                                            
         ICM   RE,15,REK1LINE                                                   
         LA    RE,1(RE)                                                         
         STCM  RE,15,REK1LINE                                                   
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
NXKEYD   DSECT                                                                  
NXFLAG   DS    XL1                                                              
NXOLDKEY DS    CL(L'IOKEY)                                                      
*                                                                               
REVIEW01 CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
*                                                                               
ALL      DC    CL7'All'                                                         
*                                                                               
CORETAB  DC    CL8'CORETAB'                                                     
*                                                                               
MEDTAB   DC    CL2'TV'             VALID MEDIA FOR STATIONS                     
         DC    CL2'AM'                                                          
         DC    CL2'FM'                                                          
         DC    CL2'CM'                                                          
         DC    XL1'00'                                                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
RTWORKD  DSECT                                                                  
RTRELO   DS    A                                                                
RTPARMA  DS    A                   A(INCOMING PARAMETER LIST)                   
*                                                                               
RTPARMS  DS    0XL24               * PARAMETER SAVE AREA *                      
RTPARMS1 DS    A                                                                
RTPARMS2 DS    A                                                                
RTPARMS3 DS    A                                                                
RTPARMS4 DS    A                                                                
RTPARMS5 DS    A                                                                
RTPARMS6 DS    A                                                                
*                                                                               
RTPARM   DS    0XL24               * PARAMETERS 1-6 *                           
RTPARM1  DS    A                                                                
RTPARM2  DS    A                                                                
RTPARM3  DS    A                                                                
RTPARM4  DS    A                                                                
RTPARM5  DS    A                                                                
RTPARM6  DS    A                                                                
*                                                                               
RTWORK   DS    XL128               FOR ALL TO USE                               
RTWORKL  EQU   *-RTWORKD                                                        
         SPACE 1                                                                
* REVIEWWRK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REVIEWWRK                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
REVIEW01 CSECT                                                                  
         ORG   REVIEW01+(((*-REVIEW01)/2048)+1)*2048                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013REVIEW01  05/01/02'                                      
         END                                                                    
