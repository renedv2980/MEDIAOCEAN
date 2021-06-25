*          DATA SET CTSFM21    AT LEVEL 019 AS OF 10/23/13                      
*PHASE TA0A21A                                                                  
*INCLUDE SCINKEY                                                                
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM21 -- IP SUBNET RECORD MAINTENANCE/LIST/REPORT  *         
*                                                                     *         
*  COMMENTS:     MAINTAINS IP SUBNET RECORDS ON CTFILE                *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFM95 (MAINTENANCE)                        *         
*                        CTSFM96 (LIST)                               *         
*                        CTSFM97 (REPORT)                             *         
*                                                                     *         
*  OUTPUTS:      UPDATED IP SUBNET RECORDS, OR LIST.                  *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A21 - IP SUBNET RECORD MAINTENANCE/LIST/REPORT'              
TA0A21   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWORKLNQ,**0A21**,R7,RR=R3                                       
         LR    R4,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         ST    R4,ABIGWORK                                                      
*                                                                               
         OI    GENSTAT4,CONFDEL+NODELLST                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       CLI   ACTNUM,ACTREP       NO KEY VALIDATION FOR LIST / REPORT          
         BE    VKX                                                              
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKX                                                              
*                                                                               
         BRAS  RE,CHKSRE           CHECK FOR SPECIAL RESTRICTION                
*                                                                               
         LA    R2,IPMSNIDH                                                      
         CLI   IPMSNIDH+5,0        ANY DATA?                                    
         BNE   *+12                YES                                          
         MVI   GERROR1,MISSING     REQUIRED FOR ALL BUT LIST                    
         B     VSFMERR                                                          
*                                                                               
         BRAS  RE,VALSNID                                                       
*                                                                               
         LA    R4,KEY              BUILD IP SUBNET KEY                          
         USING CTIPREC,R4                                                       
         XC    KEY,KEY                                                          
         MVI   CTIPKTYP,CTIPKTEQ   RECORD TYPE C'3'                             
         MVI   CTIPKTY2,CTIPKT2E   SUB RECORD TYPE C'I'                         
         MVC   CTIPKSID,SNID       SUBNET ID (1S COMPLEMENT)                    
         DROP  R4                                                               
*                                                                               
VKX      B     XIT                                                              
***********************************************************************         
* CHECK FOR SPECIAL RESTRICTION                                                 
***********************************************************************         
CHKSRE   NTR1                                                                   
         CLI   ACTNUM,ACTADD                                                    
         BE    CSREX20                                                          
         CLI   ACTNUM,ACTCHA                                                    
         BE    CSREX20                                                          
         CLI   ACTNUM,ACTDEL                                                    
         BE    CSREX20                                                          
         CLI   ACTNUM,ACTREST                                                   
         BE    CSREX20                                                          
         CLI   ACTNUM,ACTSEL                                                    
         BNE   CSREX                                                            
         CLI   THISLSEL,C'C'                                                    
         BE    CSREX20                                                          
         CLI   THISLSEL,C'D'                                                    
         BNE   CSREX                                                            
CSREX20  CLC   AGENCY,=C'**'       TEST SPECIAL AGENCY ALPHA                    
         BE    CSREX                                                            
         MVI   GERROR1,SECLOCK     SECURITY LOCKOUT                             
         B     VSFMERR                                                          
CSREX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUBNET ID                                                            
***********************************************************************         
VALSNID  NTR1                                                                   
         LA    RE,5                CHECK FIRST 5 CHAR FOR '.' OR ':'            
         LA    RF,IPMSNID                                                       
VSNID10  CLI   0(RF),C'.'                                                       
         BE    VSNID20             V4 SUBNET ID                                 
         CLI   0(RF),C':'                                                       
         BE    VSNID40             V6 SUBNET ID                                 
         AHI   RF,1                                                             
         BCT   RE,VSNID10                                                       
         B     VSNIDBAD            INVALID SUBNET ID                            
*                                                                               
VSNID20  LA    R2,IPMSNIDH                                                      
         BRAS  RE,VSNV4                                                         
         B     VSNID60                                                          
*                                                                               
VSNID40  LA    R2,IPMSNIDH                                                      
         BRAS  RE,VSNV6                                                         
*                                                                               
VSNID60  XC    SNID,=16X'FF'       1'S COMPLEMENT                               
         B     XIT                                                              
*                                                                               
VSNIDBAD MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE V4 SUBNET ID                                                         
***********************************************************************         
* NTRY: R2 = A(FIELD HEADER)                                                    
* EXIT: SNID = 128-BIT SUBNET ID                                                
VSNV4    NTR1                                                                   
         ZIC   RE,5(R2)            SUBNET ID LENGTH                             
         LA    RF,8(R2)                                                         
         XC    IPWORK(8*4),IPWORK                                               
         XC    FULL,FULL                                                        
*                                                                               
VSNV405  CLI   0(RF),C'.'          VALID CHAR ARE 0..9 OR '.'                   
         BE    VSNV410                                                          
         CLI   0(RF),C'0'                                                       
         BL    VSNV4BAD                                                         
         CLI   0(RF),C'9'                                                       
         BH    VSNV4BAD                                                         
VSNV410  AHI   RF,1                                                             
         BCT   RE,VSNV405                                                       
*                                                                               
         ZIC   RE,5(R2)            SUBNET ID LENGTH                             
         LA    RF,8(R2)                                                         
         LA    R3,IPWORK                                                        
*                                                                               
         LA    R4,0(RF,RE)                                                      
         BCTR  R4,0                                                             
         CLI   0(R4),C'.'                                                       
         BE    VSNV4BAD            LAST CHAR CAN'T BE '.'                       
*                                                                               
VSNV420  LA    R1,0                                                             
         LR    R4,RF                                                            
VSNV422  CLI   0(RF),C'.'                                                       
         BE    VSNV430                                                          
         AHI   RF,1                                                             
         AHI   R1,1                                                             
         BCT   RE,VSNV422                                                       
VSNV430  LTR   R1,R1               1-3 DIGIT FOR EACH NUMBER                    
         BZ    VSNV4BAD                                                         
         CHI   R1,3                                                             
         BH    VSNV4BAD                                                         
*                                  RIGHT-ALIGNED INTO THE FIELD                 
         LA    R5,4(R3)                                                         
         SR    R5,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)                                                    
*                                                                               
         AHI   RF,1                                                             
         AHI   R3,L'IPWORK                                                      
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         BCT   RE,VSNV420                                                       
*                                                                               
         LA    RF,IPWORK                                                        
         SR    R3,RF               MUST BE 4 FIELDS FOR V4 SUBNET ID            
         CHI   R3,4*L'IPWORK                                                    
         BNE   VSNV4BAD                                                         
*                                  NOW 4 NUMBERS ARE IN 4 FULLWORD              
         LA    R3,IPWORK                                                        
         LA    R5,FULL                                                          
         LA    RE,4                                                             
*                                                                               
VSNV450  PACK  DUB,0(4,R3)         PACK, CONVERT TO BINARY                      
         CVB   RF,DUB                                                           
         CHI   RF,X'FF'                                                         
         BH    VSNV4BAD            CAN'T > 255                                  
         STC   RF,0(R5)                                                         
         AHI   R3,4                                                             
         AHI   R5,1                                                             
         BCT   RE,VSNV450                                                       
*                                                                               
         MVC   SNID(12),V4PREFIX                                                
         MVC   SNID+12(4),FULL                                                  
         B     XIT                                                              
*                                                                               
VSNV4BAD MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
*                                                                               
V4PREFIX DC    X'00000000000000000000FFFF'    IP V4 ADDRESS PREFIX              
***********************************************************************         
* VALIDATE V6 SUBNET ID                                                         
***********************************************************************         
* NTRY: R2 = A(FIELD HEADER)                                                    
* EXIT: SNID = 128-BIT SUBNET ID                                                
VSNV6    NTR1                                                                   
         ZIC   RE,5(R2)            SUBNET ID LENGTH                             
         LA    RF,8(R2)                                                         
*                                                                               
VSNV605  CLI   0(RF),C':'          VALID CHAR ARE 0..9, A..F OR ':'             
         BE    VSNV610                                                          
         CLI   0(RF),C'A'                                                       
         BL    VSNIDBAD                                                         
         CLI   0(RF),C'F'                                                       
         BNH   VSNV610                                                          
         CLI   0(RF),C'0'                                                       
         BL    VSNIDBAD                                                         
         CLI   0(RF),C'9'                                                       
         BH    VSNIDBAD                                                         
VSNV610  AHI   RF,1                                                             
         BCT   RE,VSNV605                                                       
*                                                                               
         LA    RF,8(R2)            SUBNET ID                                    
         CLI   0(RF),C':'                                                       
         BNE   *+12                                                             
         CLI   1(RF),C':'                                                       
         BNE   VSNIDBAD            CAN'T HAVE LEADING SINGLE ':'                
*                                                                               
         ZIC   RE,5(R2)            SUBNET ID LENGTH                             
         BCTR  RE,0                                                             
         AR    RF,RE               LAST CHAR                                    
         CLI   0(RF),C':'                                                       
         BNE   *+14                                                             
         BCTR  RF,0                                                             
         CLI   0(RF),C':'          2ND CHAR                                     
         BNE   VSNIDBAD            CAN'T HAVE TRAILING SINGLE ':'               
*                                                                               
         ZIC   RE,5(R2)            SUBNET ID LENGTH                             
         BCTR  RE,0                                                             
         LA    RF,8(R2)                                                         
*                                                                               
         SR    R0,R0               # OF FIELDS                                  
         SR    R1,R1               # OF '::'                                    
         CLC   0(2,RF),=C'::'                                                   
         BE    *+8                                                              
         AHI   R0,1                                                             
*                                                                               
VSNV620  CLC   0(2,RF),=C'::'                                                   
         BNE   *+12                                                             
         AHI   R1,1                                                             
         B     VSNV625                                                          
*                                                                               
         CLI   0(RF),C':'                                                       
         BNE   VSNV625                                                          
         AHI   R0,1                : FOLLOWED BY A CHAR, 1 FIELD                
*                                                                               
VSNV625  AHI   RF,1                                                             
         BCT   RE,VSNV620                                                       
*                                                                               
         LTR   R1,R1                                                            
         BNZ   *+16                                                             
         CHI   R0,8                                                             
         BNE   VSNV6BAD            MUST BE 8 FIELD WHEN NO '::'                 
         B     VSNV630                                                          
*                                                                               
         CHI   R1,1                                                             
         BH    VSNV6BAD            CAN'T HAVE MORE THAN 1 '::'                  
*                                  '::' = AT LEAST 2 FIELDS OF 0'S              
         MHI   R1,2                                                             
         AR    R1,R0                                                            
         CHI   R1,8                                                             
         BH    VSNV6BAD            CAN'T HAVE MORE THAN 8 FIELDS                
*                                                                               
*                                  R0 = # FIELD EXCLUDE '::'                    
VSNV630  LR    R6,R0                                                            
         LA    R3,IPWORK                                                        
         ZIC   R4,5(R2)            SUBNET ID LENGTH                             
         LA    R5,8(R2)            SUBNET ID                                    
         MVC   0(32,R3),=32C'0'    PADDED W/ 0'S                                
*                                                                               
VSNV635  CLC   0(2,R5),=C'::'                                                   
         BE    VSNV650                                                          
         CLI   0(R5),C':'                                                       
         BNE   *+10                                                             
         AHI   R5,1                                                             
         BCTR  R4,0                                                             
*                                                                               
         LR    RE,R5                                                            
VSNV640  CLI   0(R5),C':'                                                       
         BE    VSNV643                                                          
         AHI   R5,1                                                             
         BCT   R4,VSNV640                                                       
*                                                                               
VSNV643  LR    RF,R5                                                            
         SR    RF,RE                                                            
         LTR   RF,RF               1-4 DIGIT FOR EACH NUMBER                    
         BZ    VSNV6BAD                                                         
         CHI   RF,4                                                             
         BH    VSNV6BAD                                                         
*                                  RIGHT-ALIGNED INTO THE FIELD                 
         LA    R1,4(R3)                                                         
         SR    R1,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)                                                    
*                                                                               
         AHI   R3,L'IPWORK                                                      
         BCTR  R6,0                                                             
         LTR   R4,R4                                                            
         BZ    VSNV6X                                                           
         B     VSNV635                                                          
*                                                                               
VSNV650  LTR   R6,R6                                                            
         BZ    VSNV6X              NO MORE INPUT FIELD LEFT                     
         AHI   R5,2                BUMP PASS '::'                               
         AHI   R4,-2                                                            
         LA    R3,IPWORK+32                                                     
         LR    RE,R6                                                            
         MHI   RE,L'IPWORK                                                      
         SR    R3,RE               NEXT NON-ZERO FIELDS                         
         B     VSNV635                                                          
*                                                                               
VSNV6X   GOTO1 HEXIN,DMCB,IPWORK,SNID,32                                        
         B     XIT                                                              
*                                                                               
VSNV6BAD MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
         BRAS  RE,CHKSRE           CHECK FOR SPECIAL RESTRICTION                
         BRAS  RE,VRPID                                                         
         BRAS  RE,VRCID                                                         
         BRAS  RE,VRMASK                                                        
         BRAS  RE,VRDES                                                         
         BRAS  RE,VRPUID                                                        
         BRAS  RE,VRRNAM                                                        
VRX      B     DR                  REDISPLAY RECORD                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRINCIPLE ID AND COMPATIBLE ID'S                                     
***********************************************************************         
VRPID    NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
         MVI   FLAG,0              SET FLAGS FOR WHAT WAS INPUT                 
         XC    SVAGYAP,SVAGYAP                                                  
         XC    SVAGYAI,SVAGYAI                                                  
         MVI   ELCODE,CTPIDELQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,IPMPIDH                                                       
         CLI   IPMPIDH+5,0                                                      
         BE    VRPIDX              PRINCIPAL ID NOT INPUT                       
*                                                                               
         CLI   IPMPIDH+5,3                                                      
         BNL   *+12                                                             
         MVI   GERROR1,INVALID     INPUT TOO SHORT                              
         B     VSFMERR                                                          
*                                                                               
         MVC   AIO,AIO2            READ PRINCIPLE ID RECORD                     
         L     R3,AIO                                                           
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         OC    IPMPID,SPACES                                                    
         MVC   CTIKID,IPMPID                                                    
         MVC   KEY(L'CTIKEY),CTIKEY                                             
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
         TM    DMCB+8,X'40'                                                     
         BZ    *+12                                                             
         MVI   GERROR1,DISKERR     DISK ERROR                                   
         B     VSFMERR                                                          
         TM    DMCB+8,X'10'                                                     
         BZ    *+12                                                             
         MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     VSFMERR                                                          
*                                                                               
         BRAS  RE,GETAGID          GET AGENCY ALPHA ID AND TEST ID              
         MVC   SVAGYAP,AGYID                                                    
         MVI   FLAG,1              SET COMPATIBLE ID LIST BUILT                 
         DROP  R3                                                               
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTPIDD,R3                                                        
         MVI   CTPIDEL,CTPIDELQ    BUILD PRINCIPAL ID ELEMENT                   
         MVI   CTPIDLEN,X'0C'                                                   
         MVC   CTPID,IPMPID                                                     
         GOTO1 ADDELEM                                                          
VRPIDX   B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMPATIBLE ID'S LIST                                                 
***********************************************************************         
VRCID    NTR1                                                                   
         MVI   ELCODE,CTIDELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,IPMIDSH                                                       
         CLI   IPMIDSH+5,0                                                      
         BE    VRCIDX                                                           
         CLI   IPMIDSH+5,3                                                      
         BNE   VRCID10                                                          
         CLC   IPMIDS(3),=C'ALL'                                                
         BNE   VRCID10                                                          
         CLI   TWAOFFC,C'*'         ALL ONLY VALID FOR DDS TERMS                
         BE    *+12                                                             
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ                                                   
         MVI   CTIDLEN,X'0C'                                                    
         OC    IPMIDS,SPACES                                                    
         MVC   CTID,IPMIDS                                                      
         GOTO1 ADDELEM                                                          
         DROP  R3                                                               
         B     VRCIDX                                                           
*                                                                               
VRCID10  MVI   LINECNT,3                                                        
*                                                                               
VRCID20  TM    1(R2),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BE    VRCID100                                                         
         GOTO1 SCANNER,DMCB,(R2),(20,BLOCK1)                                    
         CLI   4(R1),0                                                          
         BNE   *+12                                                             
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
*                                                                               
         LA    R4,BLOCK1           R4=A(SCAN BLOCK ENTRY)                       
         MVC   FLDCNT,4(R1)                                                     
         MVI   FLINDX,1                                                         
         MVC   GINDEX,FLINDX                                                    
*                                                                               
VRCID30  CLC   FLINDX,FLDCNT                                                    
         BH    VRCID100                                                         
         CLI   1(R4),0                                                          
         BNE   VRCID40                                                          
         CLI   0(R4),3             VALIDATE USER-ID                             
         BNL   *+12                                                             
         MVI   GERROR1,INVALID     INPUT TOO SHORT                              
         B     VSFMERR                                                          
*                                                                               
         CLI   0(R4),10                                                         
         BNH   *+12                                                             
         MVI   GERROR1,INVALID     INPUT TOO LONG                               
         B     VSFMERR                                                          
*                                                                               
         BAS   RE,WILDCARD                                                      
         BE    VRCID80                                                          
         L     R3,AIO2             SWITCH I/O AREAS                             
         MVC   AIO,AIO2                                                         
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY       BUILD ID KEY                                 
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,12(R4)                                                    
         MVC   KEY(L'CTIKEY),CTIKEY                                             
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
         TM    DMCB+8,X'40'                                                     
         BZ    *+12                                                             
         MVI   GERROR1,DISKERR     DISK ERROR                                   
         B     VSFMERR                                                          
*                                                                               
         TM    DMCB+8,X'10'                                                     
         BZ    *+12                                                             
         MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     VSFMERR                                                          
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         DROP  R3                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ      BUILD ID ELEMENT                             
         MVI   CTIDLEN,X'0C'                                                    
         MVC   CTID,12(R4)                                                      
         B     VRCID50                                                          
         DROP  R3                                                               
*                                                                               
VRCID40  CLI   0(R4),0             VALIDATE LIST-ID                             
         BNE   *+12                                                             
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
*                                                                               
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'LIST'   CHECK FOR VALID KEYWORD                      
         BE    VRCID47                                                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'AGY'    CHECK FOR VALID KEYWORD 'AGY'                
         BE    *+12                                                             
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
*                                                                               
         CLI   1(R4),2                                                          
         BE    *+12                MUST BE 2 CHARS                              
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
*                                                                               
         CLI   FLAG,0              FIRST LIST ENTRY?                            
         BNE   *+12                CAN'T BE THE FIRST ENTRY                     
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
*                                  AGY=  AGY-UID LIST                           
         LA    R1,AGYLIST                                                       
         LA    RE,AGYLISTX                                                      
VRCID41  CR    R1,RE                                                            
         BNL   VRCID42             END OF VALID AGY LIST                        
         OC    0(2,R1),0(R1)                                                    
         BZ    VRCID42             ALSO END OF VALID AGY LIST                   
         CLC   0(2,R1),22(R4)      SAME AS AGY=                                 
         BE    VRCID46             YES - DONE CHECKING                          
         AHI   R1,2                                                             
         B     VRCID41             NO - CHECK NEXT ONE                          
*                                                                               
VRCID42  L     R3,AIO2             SWITCH I/O AREAS                             
         MVC   AIO,AIO2                                                         
         USING CT9BREC,R3                                                       
         XC    CT9BKEY,CT9BKEY     BUILD LIST KEY                               
         MVI   CT9BKTYP,CT9BKTYQ                                                
         MVI   CT9BKSUB,CT9BKS01                                                
         MVC   CT9BKAGY,22(R4)                                                  
         MVC   KEY(L'CT9BKEY),CT9BKEY                                           
         GOTO1 HIGH                                                             
         TM    DMCB+8,X'40'                                                     
         BZ    *+12                                                             
         MVI   GERROR1,DISKERR     DISK ERROR                                   
         B     VSFMERR                                                          
*                                                                               
         CLC   CT9BKAGY,22(R4)     SAME AGENCY?                                 
         BE    *+12                NO - NOT FOUND                               
         MVI   GERROR1,NOTFOUND                                                 
         B     VSFMERR                                                          
*                                                                               
VRCID43  LA    R1,CT9BDATA                                                      
VRCID44  CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE X'02' ELEMENT                      
         CLI   0(R1),X'02'                                                      
         BE    VRCID45                                                          
         SR    RF,RF                                                            
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     VRCID44                                                          
*                                                                               
VRCID45  MVC   WORK(10),2(R1)      SAVE THIS USER ID                            
         BAS   RE,VALID            VALIDATE IT                                  
         BNE   *+12                                                             
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
*                                                                               
         GOTO1 SEQ                 NEXT RECORD                                  
         CLC   CT9BKAGY,22(R4)     STILL THE SAME AGENCY?                       
         BE    VRCID43                                                          
         DROP  R3                                                               
*                                                                               
VRCID46  MVC   KEY,SAVEKEY                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ      BUILD THE ELEMENT                            
         MVI   CTIDLEN,X'06'                                                    
         MVC   CTID(2),=X'0001'    AGY= SUBCODE                                 
         MVC   CTIDAGY(2),22(R4)                                                
         B     VRCID90                                                          
         DROP  R3                                                               
*                                                                               
VRCID47  CLI   1(R4),6                                                          
         BNH   *+12                                                             
         MVI   GERROR1,INVALID     INPUT TOO LONG                               
         B     VSFMERR                                                          
*                                                                               
         L     R3,AIO2             SWITCH I/O AREAS                             
         MVC   AIO,AIO2                                                         
         USING CTWREC,R3                                                        
         XC    CTWKEY,CTWKEY       BUILD LIST KEY                               
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'I'                                                     
         MVC   CTWKID,22(R4)                                                    
         MVC   KEY(L'CTWKEY),CTWKEY                                             
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
         TM    DMCB+8,X'40'                                                     
         BZ    *+12                                                             
         MVI   GERROR1,DISKERR     DISK ERROR                                   
         B     VSFMERR                                                          
*                                                                               
         TM    DMCB+8,X'10'                                                     
         BZ    *+12                                                             
         MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     VSFMERR                                                          
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         DROP  R3                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ      BUILD ID ELEMENT                             
         MVI   CTIDLEN,X'0C'                                                    
         XC    CTID(2),CTID                                                     
         MVC   CTID+2(8),22(R4)                                                 
*                                                                               
VRCID50  CLI   FLAG,0              FIRST LIST ENTRY                             
         BNE   VRCID60                                                          
         OC    CTID(2),CTID        MUST BE AN ID                                
         BNZ   *+12                                                             
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
*                                                                               
         L     R3,AIO2                                                          
         BRAS  RE,GETAGID                                                       
         MVC   SVAGYAI,AGYID                                                    
         MVI   FLAG,1                                                           
         B     VRCID90                                                          
*                                                                               
VRCID60  MVC   WORK(10),CTID       OTHER LIST ENTRIES MUST BE IN FIRST          
         DROP  R3                                                               
         OC    WORK(2),WORK        LIST ENTRY'S COMPATIBLE ID LIST              
         BZ    VRCID65                                                          
         BAS   RE,VALID                                                         
         BNE   VRCID90                                                          
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
*                                                                               
VRCID65  L     R3,AIO2                                                          
         USING CTWREC,R3                                                        
         LA    R3,CTWDATA                                                       
         SR    R1,R1                                                            
*                                                                               
VRCID70  CLI   0(R3),0             IF ENTRY IS A LIST DO FOR ALL                
         BE    VRCID90             ENTRIES IN LIST RECORD                       
         CLI   0(R3),X'A4'                                                      
         BNE   VRCID72                                                          
         MVC   WORK(10),3(R3)                                                   
         BAS   RE,VALID                                                         
         BNE   *+12                                                             
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
*                                                                               
VRCID72  EQU   *                                                                
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     VRCID70                                                          
*                                                                               
         DROP  R3                                                               
VRCID80  EQU   *                   PROCESS WILDCARD ID XX*                      
         CLI   FLAG,0              FIRST LIST ENTRY                             
         BNE   *+12                                                             
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR             MUST BE A SINGLE USERID                      
*                                                                               
         MVC   WORK(10),12(R4)                                                  
         BRAS  RE,VALID                                                         
         BNE   *+12                                                             
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTIDD,R3                                                         
         MVI   CTIDEL,CTIDELQ      BUILD ID ELEMENT                             
         MVI   CTIDLEN,X'0C'                                                    
         MVC   CTID,12(R4)                                                      
*                                  ADD ID ELEMENT TO TERM REC                   
         DROP  R3                                                               
VRCID90  LA    R3,ELEM                                                          
         MVC   DMCB+4,AIO1                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'CTFILE '),,(R3),=C'ADD=CODE'                 
         CLI   12(R1),0                                                         
         BE    *+12                                                             
         MVI   GERROR1,57          RECORD TOO BIG  ?                            
         B     VSFMERR                                                          
*                                                                               
         ZIC   R1,FLINDX           BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FLINDX                                                        
         MVC   GINDEX,FLINDX                                                    
         LA    R4,32(R4)                                                        
         B     VRCID30                                                          
*                                                                               
VRCID100 ZIC   R1,0(R2)            BUMP TO NEXT TWA LINE                        
         AR    R2,R1                                                            
         ZIC   RF,LINECNT                                                       
         BCT   RF,*+8                                                           
         B     VRCIDX                                                           
         STC   RF,LINECNT                                                       
         B     VRCID20                                                          
*                                                                               
VRCIDX   B     XIT                                                              
         EJECT                                                                  
* ROUTINE CHECK ID IS IN COMPATIBLE ID LIST OF MASTER ID                        
*                                                                               
VALID    L     RF,ABIGWORK        A(EXTENDED GETID BUFFER)                      
VALID2   CLC   0(10,RF),WORK                                                    
         BE    VALIDX                                                           
*                                 CHECK FOR WILD CARD IN CID LIST               
         LA    R1,9(RF)                                                         
         LA    R0,10                                                            
*                                                                               
VALID3   EQU   *                                                                
         CLI   0(R1),C'*'                                                       
         BE    VALID4                                                           
         CLI   0(R1),C' '                                                       
         BNE   VALID5                                                           
         BCTR  R1,0                                                             
         BCT   R0,VALID3                                                        
         B     VALID5                                                           
*                                                                               
VALID4   EQU   *                                                                
         BCTR  R0,0                                                             
         LR    R1,R0              CHECK FOR WILD CARD MATCH                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),WORK                                                     
         BE    VALIDX                                                           
*                                                                               
VALID5   LA    RF,12(RF)                                                        
         CLI   0(RF),X'FF'                                                      
         BNE   VALID2                                                           
*                                                                               
VALIDX   CLI   0(RF),X'FF'        CC=EQ IF NOT IN LIST                          
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO SET AGENCY ALPHA ID FROM ID RECORD AT R3                           
* AND BUILD COMPATIBLE ID LIST                                                  
*                                                                               
GETAGID  NTR1                                                                   
         SR    R0,R0                                                            
         LA    RF,CTIDATA-CTIREC(R3)                                            
GETAGID1 CLI   0(RF),0                                                          
         BNE   *+12                                                             
         LA    RF,=X'06040000'     POINT TO DUMMY ELEMENT                       
         B     GETAGID2                                                         
         CLI   0(RF),X'06'         TEST AGENCY ALPHA ID EL                      
         BE    GETAGID2                                                         
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     GETAGID1                                                         
*                                                                               
GETAGID2 MVC   AGYID,2(RF)         SAVE AGENCY ALPHA ID                         
         GOTO1 CALLOV,DMCB,0,X'D9000AFA',0                                      
         L     RF,0(R1)            RF=A(GETIDS)                                 
         GOTO1 (RF),DMCB,(C'C',(R3)),ABIGWORK,(C'W',DATAMGR)                    
         CLI   0(R1),0                                                          
         BNE   *+12                                                             
         MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
         CLI   0(R1),X'FF'                                                      
         BNE   GAGID30                                                          
         MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     VSFMERR                                                          
*                                                                               
GAGID30  EQU   *                   SAVE VALID AGENCY LIST                       
         XC    AGYLIST(AGYLISTQ),AGYLIST                                        
         LA    R1,AGYLIST                                                       
         LA    RE,AGYLISTX                                                      
         LA    RF,CTIDATA-CTIREC(R3)                                            
GAGID310 CLI   0(RF),0                                                          
         BE    GAGID380            EOR                                          
         CLI   0(RF),X'20'         COMP ID LIST ELEMENT                         
         BH    GAGID380            PASS IT ALREADY, EXIT                        
         CLC   0(2,RF),=X'2006'    AGY= ELEMENT                                 
         BNE   GAGID320            NO - SKIP THIS                               
         CR    R1,RE                                                            
         BL    *+6                                                              
         DC    H'0'                TOO MANY AGY=, INCR AGYLIST SIZE             
         MVC   0(2,R1),4(RF)       YES - SAVE                                   
         AHI   R1,2                                                             
GAGID320 IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     GAGID310                                                         
GAGID380 EQU   *                                                                
         B     XIT                                                              
         SPACE 2                                                                
* ROUTINE TO CHECK FOR WILD CARD STYLE USERID IN COMPATIBLE ID LIST             
* R4=A(SCANNER CONTROL BLOCK)                                                   
* IF WILDCARD (I.E. STRING ENDS WITH '*') RETURN IN LIDSAVE                     
* AND RETURN CC .EQ.                                                            
*                                                                               
WILDCARD NTR1                                                                   
         ZIC   RF,0(R4)                                                         
         LA    R1,11(RF,R4)                                                     
WCAR010  CLI   0(R1),C'*'                                                       
         BE    WCAR020                                                          
         CLI   0(R1),C' '                                                       
         BE    WCAR012                                                          
         B     WCARNO                                                           
WCAR012  BCTR  R1,0                                                             
         BCT   RF,WCAR010                                                       
         B     WCARNO                                                           
WCAR020  EQU   *                                                                
         BCTR  RF,0                                                             
         STC   RF,WILDCLEN                                                      
         B     WCAROK                                                           
WCAROK   SR    RC,RC                                                            
WCARNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUBNET MASK                                                          
***********************************************************************         
VRMASK   NTR1                                                                   
         LA    R2,IPMMASKH                                                      
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   GERROR1,MISSING                                                  
         B     VSFMERR                                                          
*                                                                               
         LA    RE,5                CHECK FIRST 5 CHAR FOR '.' OR ':'            
         LA    RF,IPMMASK                                                       
VMASK10  CLI   0(RF),C'.'                                                       
         BE    VMASK20             V4 SUBNET MASK                               
         CLI   0(RF),C':'                                                       
         BE    VMASK40             V6 SUBNET MASK                               
         AHI   RF,1                                                             
         BCT   RE,VMASK10                                                       
         B     VMASKBAD            INVALID SUBNET MASK                          
*                                                                               
VMASK20  CLC   CTIPKSID-CTIPKEY+KEY(12),KEYV4PF                                 
         BNE   VMASKBAD            THIS IS NOT A V4 IP SUBNET                   
         BRAS  RE,VSNV4                                                         
         B     VMASK60                                                          
*                                                                               
VMASK40  BRAS  RE,VSNV6                                                         
*                                                                               
VMASK60  MVI   ELCODE,CTIPMELQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTIPMD,R3                                                        
         MVI   CTIPMEL,CTIPMELQ    BUILD SUBNET MASK ELEMENT                    
         MVI   CTIPMLEN,18                                                      
         MVC   CTIPM,SNID                                                       
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
VMASKBAD MVI   GERROR1,INVALID                                                  
         B     VSFMERR                                                          
         EJECT                                                                  
KEYV4PF  DC    X'FFFFFFFFFFFFFFFFFFFF0000'    V4 IP PREFIX (1S COMPL)           
***********************************************************************         
* VALIDATE DESCRIPTION                                                          
***********************************************************************         
VRDES    NTR1                                                                   
         MVI   ELCODE,CTDSCELQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTDSCD,R3                                                        
         MVI   CTDSCEL,CTDSCELQ    BUILD SUBNET MASK ELEMENT                    
         MVI   CTDSCLEN,62                                                      
         MVC   CTDSC,IPMDES                                                     
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         DROP  R3                                                               
***********************************************************************         
* VALIDATE PU ID                                                                
***********************************************************************         
VRPUID   NTR1                                                                   
*                                                                               
         MVI   ELCODE,CTIPPELQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,IPMPUIDH                                                      
         CLI   IPMPUIDH+5,0                                                     
         BE    VRPUIDX                                                          
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTIPPD,R3                                                        
         MVI   CTIPPEL,CTIPPELQ    BUILD IP PUID ELEMENT                        
         ZIC   R1,IPMPUIDH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTIPPID(0),IPMPUID                                               
         AH    R1,=Y(CTIPPLNQ+1)                                                
         STC   R1,CTIPPLEN                                                      
         MVI   CTIPPFLG,0                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
VRPUIDX  B     XIT                                                              
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE RESOURCE NAME                                                        
***********************************************************************         
VRRNAM   NTR1                                                                   
*                                                                               
         MVI   ELCODE,CTIPRELQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   IPMRNAMH+5,0                                                     
         BE    VRRNAMX                                                          
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING CTIPRD,R3                                                        
         MVI   CTIPREL,CTIPRELQ    BUILD IP RESOURCE NAME ELEMENT               
         ZIC   R1,IPMRNAMH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTIPRNAM(0),IPMRNAM                                              
         AH    R1,=Y(CTIPRLNQ+1)                                                
         STC   R1,CTIPRLEN                                                      
         GOTO1 ADDELEM                                                          
*                                                                               
VRRNAMX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
         BRAS  RE,CHKSRE           CHECK FOR SPECIAL RESTRICTION                
         XC    IPMPUID,IPMPUID                                                  
         OI    IPMPUIDH+6,X'80'                                                 
         XC    IPMRNAM,IPMRNAM                                                  
         OI    IPMRNAMH+6,X'80'                                                 
         XC    IPMDES,IPMDES                                                    
         OI    IPMDESH+6,X'80'                                                  
         XC    IPMSMIN,IPMSMIN                                                  
         OI    IPMSMINH+6,X'80'                                                 
         XC    IPMSMAX,IPMSMAX                                                  
         OI    IPMSMAXH+6,X'80'                                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CTPIDELQ      PRINCIPLE ID ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   DR20                                                             
         USING CTPIDD,R6                                                        
         MVC   IPMPID,CTPID                                                     
         OI    IPMPIDH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
DR20     BRAS  RE,DISIDS                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CTIPMELQ      SUBNET MASK ELEMENT                         
         BRAS  RE,GETEL                                                         
         BE    *+6                  MUST HAVE SUBNET MASK                       
         DC    H'0'                                                             
*                                                                               
         USING CTIPMD,R6                                                        
         MVC   SNID,CTIPM                                                       
         MVC   SVMASK,CTIPM                                                     
         DROP  R6                                                               
         LA    R2,IPMMASKH                                                      
         BRAS  RE,DISIPA                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CTDSCELQ      DESCRIPTION ELEMENT                         
         BRAS  RE,GETEL                                                         
         BNE   DR30                                                             
         USING CTDSCD,R6                                                        
         MVC   IPMDES,CTDSC                                                     
         OI    IPMDESH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
DR30     L     R6,AIO                                                           
         MVI   ELCODE,CTIPPELQ      IP PUID ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   DR40                                                             
         USING CTIPPD,R6                                                        
         ZIC   R1,CTIPPLEN                                                      
         SH    R1,=Y(CTIPPLNQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IPMPUID(0),CTIPPID                                               
         OI    IPMPUIDH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DR40     L     R6,AIO                                                           
         MVI   ELCODE,CTIPRELQ      IP RESOURCE NAME ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   DR90                                                             
         USING CTIPRD,R6                                                        
         ZIC   R1,CTIPRLEN                                                      
         SH    R1,=Y(CTIPRLNQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IPMRNAM(0),CTIPRNAM                                              
         OI    IPMRNAMH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DR90     L     R6,AIO                                                           
         USING CTIPREC,R6                                                       
         MVC   SNID,CTIPKSID       SUBNET ID (1S COMP)                          
         DROP  R6                                                               
         XC    SNID,=16X'FF'       INVERSE SUBNET ID                            
         MVC   SVSNID,SNID         SAVE SUBNET ID                               
         NC    SNID,SVMASK         SUBNET && MASK = SUBNET MIN                  
         LA    R2,IPMSMINH                                                      
         BRAS  RE,DISIPA                                                        
*                                                                               
         MVC   SNID,SVSNID         RESTORE SUBNET ID                            
         XC    SVMASK,=16X'FF'     INVERSE THE MASK                             
         OC    SNID,SVMASK         SUBNET || -MASK = SUBNET MAX                 
         LA    R2,IPMSMAXH                                                      
         BRAS  RE,DISIPA                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT (X'F1')                     
         BRAS  RE,GETEL                                                         
         BNE   DRX                                                              
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(11,LUPDATE)                            
         XC    DMCB,DMCB                                                        
         GOTO1 GETTXT,DMCB,28,0,(C'I',0),(11,LUPDATE)                           
         OI    GENSTAT2,USMYOK                                                  
         DROP  R6                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISIDS - DISPLAY COMPATIBLE ID'S                                              
***********************************************************************         
DISIDS   NTR1                                                                   
         XC    IPMIDS,IPMIDS                                                    
         OI    IPMIDSH+6,X'80'                                                  
         XC    IPMID2,IPMID2                                                    
         OI    IPMID2H+6,X'80'                                                  
         XC    IPMID3,IPMID3                                                    
         OI    IPMID3H+6,X'80'                                                  
*                                                                               
         XC    IDCNT,IDCNT                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,CTIDELQ                                                   
*                                                                               
         BRAS  RE,GETEL                                                         
         BNE   DIDS90                                                           
         B     DIDS20                                                           
*                                                                               
DIDS10   BRAS  RE,NEXTEL                                                        
         BNE   DIDS90                                                           
*                                  ADD ID ELEMENT TO ID BLOCK                   
DIDS20   SR    R1,R1                                                            
         IC    R1,IDCNT            BUMP BLOCK COUNT                             
         LR    R4,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,IDCNT                                                         
         MH    R4,=H'20'                                                        
         LA    R4,BLOCK1(R4)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R4),C' '          AND CLEAR IT                                 
         MVC   1(19,R4),0(R4)                                                   
         IC    R1,1(R6)                                                         
         LA    RF,2(R6)                                                         
*                                                                               
         OC    2(2,R6),2(R6)                                                    
         BNZ   DIDS40                                                           
         MVC   0(2,R4),=C'L='                                                   
         AHI   R1,-2                                                            
         LA    R4,2(R4)                                                         
         LA    RF,2(RF)                                                         
         B     DIDS50                                                           
*                                                                               
DIDS40   CLC   2(2,R6),=X'0001'                                                 
         BNZ   DIDS50                                                           
         MVC   0(2,R4),=C'A='                                                   
         AHI   R1,-2                                                            
         LA    R4,2(R4)                                                         
         LA    RF,2(RF)                                                         
         B     DIDS50                                                           
*                                                                               
DIDS50   SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(RF)       MOVE IN ID                                   
         B     DIDS10                                                           
*                                                                               
DIDS90   CLI   IDCNT,0             VALID ID'S                                   
         BE    XIT                                                              
         ZIC   R0,IDCNT                                                         
         GOTO1 =V(SCINKEY),DMCB,(3,IPMIDSH),(20,BLOCK1),(R0),RR=RELO            
*                                                                               
         OI    IPMIDSH+6,X'80'                                                  
         LA    R2,IPMIDSH                                                       
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
***********************************************************************         
* DISIPA                                                                        
***********************************************************************         
*NTRY: SNID=128-BIT IP ADDRESS                                                  
*      R2  =A(SCREEN HEADER)                                                    
*EXIT: DISPLAY V4/V6 IP ADDRESS INTO SCREEN FIELD.                              
*                                                                               
DISIPA   NTR1                                                                   
         XC    8(L'IPMSNID,R2),8(R2)    CLEAR THE FIELD                         
*                                                                               
         L     R4,AIO                                                           
         USING CTIPREC,R4                                                       
         CLC   CTIPKSID(12),KEYV4PF     V4?                                     
         BNE   DIPAV6                                                           
*                                                                               
*WHEN NOT DK MODE, IF USER ENTER SNID IN V6, DISPLAY MASK IN V6 ALSO.           
DIPAV4   CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DIPAV420                                                         
         LA    RE,5                                                             
         LA    RF,IPMSNID                                                       
DIPAV410 CLI   0(RF),C':'                                                       
         BE    DIPAV6                                                           
         AHI   RF,1                                                             
         BCT   RE,DIPAV410                                                      
*                                                                               
DIPAV420 LA    RE,SNID+L'SNID-4                                                 
         LHI   RF,4                                                             
         LA    R3,8(R2)                                                         
         SR    R4,R4                                                            
*                                                                               
DIPAV450 IC    R4,0(RE)                                                         
         EDIT  (R4),(3,(R3)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R3,R0                                                            
         MVI   0(R3),C'.'                                                       
         AHI   R3,1                                                             
         AHI   RE,1                                                             
         BCT   RF,DIPAV450                                                      
         BCTR  R3,0                CLEAR UP LAST '.'                            
         MVI   0(R3),C' '                                                       
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
*                                                                               
DIPAV6   GOTO1 HEXOUT,DMCB,SNID,WORK,L'SNID,0                                   
         OC    DMCB+16,DMCB+16                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,7                                                             
         LA    R1,8(R2)                                                         
         LA    R3,WORK                                                          
DIPAV620 MVC   0(4,R1),0(R3)                                                    
         MVI   4(R1),C':'                                                       
         AHI   R1,5                                                             
         AHI   R3,4                                                             
         BCT   R0,DIPAV620                                                      
         MVC   0(4,R1),0(R3)                                                    
*                                                                               
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
***********************************************************************         
* GETIPA                                                                        
***********************************************************************         
*NTRY: SNID=128-BIT IP ADDRESS                                                  
*EXIT: WORK=39 CHARS IP ADDRESS FOR DISPLAY                                     
*                                                                               
GETIPA   NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R4,KEY                                                           
         USING CTIPREC,R4                                                       
         CLC   CTIPKSID(12),KEYV4PF     V4?                                     
         BNE   GIPAV6                                                           
         DROP  R4                                                               
*                                                                               
GIPAV4   LA    RE,SNID+L'SNID-4                                                 
         LHI   RF,4                                                             
         LA    R3,WORK                                                          
         SR    R4,R4                                                            
*                                                                               
GIPAV450 IC    R4,0(RE)                                                         
         EDIT  (R4),(3,(R3)),ALIGN=LEFT,ZERO=NOBLANK,WRK=IPWORK                 
         AR    R3,R0                                                            
         MVI   0(R3),C'.'                                                       
         AHI   R3,1                                                             
         AHI   RE,1                                                             
         BCT   RF,GIPAV450                                                      
         BCTR  R3,0                CLEAR UP LAST '.'                            
         MVI   0(R3),C' '                                                       
         B     XIT                                                              
*                                                                               
GIPAV6   GOTO1 HEXOUT,DMCB,SNID,IPWORK,L'SNID,0                                 
         OC    DMCB+16,DMCB+16                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,7                                                             
         LA    R1,WORK                                                          
         LA    R3,IPWORK                                                        
GIPAV620 MVC   0(4,R1),0(R3)                                                    
         MVI   4(R1),C':'                                                       
         AHI   R1,5                                                             
         AHI   R3,4                                                             
         BCT   R0,GIPAV620                                                      
         MVC   0(4,R1),0(R3)                                                    
         B     XIT                                                              
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                                                               
         BRAS  RE,CHKSRE           CHECK FOR SPECIAL RESTRICTION                
         L     R4,AIO                                                           
         USING CTIPREC,R4                                                       
         MVC   SNID,CTIPKSID       1'S COMPLEMENT                               
         XC    SNID,=16X'FF'       INVERSE IT                                   
         DROP  R4                                                               
         LA    R2,IPMSNIDH                                                      
         BRAS  RE,DISIPA                                                        
         B     XIT                                                              
***********************************************************************         
* ONLINE LIST                                                                   
***********************************************************************         
LR       DS    0H                                                               
         LA    R4,KEY                                                           
         USING CTIPREC,R4                                                       
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                NO                                           
*                                                                               
         MVI   CTIPKTYP,CTIPKTEQ   RECORD TYPE C'3'                             
         MVI   CTIPKTY2,CTIPKT2E   SUB RECORD TYPE C'I'                         
         LA    R2,IPMSNIDH                                                      
         CLI   IPMSNIDH+5,0        ANY DATA?                                    
         BE    LR10                NO                                           
         BRAS  RE,VALSNID                                                       
         MVC   CTIPKSID,SNID       SUBNET ID (1S COMPLEMENT)                    
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         L     R4,AIO                                                           
         B     LR30                                                             
*                                                                               
LR20     GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
LR30     CLI   CTIPKTYP,CTIPKTEQ   IP SUBNET RECORD?                            
         BNE   LRX                                                              
         CLI   CTIPKTY2,CTIPKT2E                                                
         BNE   LRX                 NO MORE RECORDS TO LIST                      
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   SNID,CTIPKSID                                                    
         XC    SNID,=16X'FF'       INVERSE IT                                   
         BRAS  RE,GETIPA                                                        
         MVC   LSTSNID,WORK                                                     
*                                                                               
         CLC   CTIPKSID(12),KEYV4PF     V4?                                     
         BNE   LR70                                                             
         DROP  R4                                                               
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,CTIPMELQ     SUBNET MASK                                  
         BAS   RE,GETEL                                                         
         BNE   LR70                                                             
*                                                                               
         USING CTIPMD,R6                                                        
         MVC   SNID,CTIPM                                                       
         BRAS  RE,GETIPA                                                        
         MVC   LSTSMASK,WORK                                                    
         DROP  R6                                                               
*                                                                               
LR70     LR    R6,R4                                                            
         MVI   ELCODE,CTDSCELQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   LR90                                                             
*                                                                               
         USING CTDSCD,R6                                                        
         MVC   LSTDES,CTDSC                                                     
         DROP  R6                                                               
*                                                                               
LR90     GOTO1 LISTMON                                                          
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PR       DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIPREC,R4                                                       
         MVI   CTIPKTYP,CTIPKTEQ   RECORD TYPE C'3'                             
         MVI   CTIPKTY2,CTIPKT2E   SUB RECORD TYPE C'I'                         
         LA    R2,IPRSNIDH                                                      
         CLI   IPRSNIDH+5,0        ANY DATA?                                    
         BE    PR10                NO                                           
         BRAS  RE,VALSNID                                                       
         MVC   CTIPKSID,SNID       SUBNET ID (1S COMPLEMENT)                    
*                                                                               
PR10     GOTO1 HIGH                FIRST RECORD                                 
         L     R4,AIO                                                           
         B     PR30                                                             
*                                                                               
PR20     GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
PR30     CLI   CTIPKTYP,CTIPKTEQ   IP SUBNET RECORD?                            
         BNE   PRX                                                              
         CLI   CTIPKTY2,CTIPKT2E                                                
         BNE   PRX                 NO MORE RECORDS TO LIST                      
*                                                                               
         XC    P,P                 CLEAR PRINT LINE                             
         MVC   SNID,CTIPKSID                                                    
         XC    SNID,=16X'FF'       INVERSE IT                                   
         BRAS  RE,GETIPA                                                        
         MVC   PRSNID,WORK                                                      
         DROP  R4                                                               
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,CTIPMELQ     SUBNET MASK                                  
         BAS   RE,GETEL                                                         
         BNE   PR60                                                             
*                                                                               
         USING CTIPMD,R6                                                        
         MVC   SNID,CTIPM                                                       
         BRAS  RE,GETIPA                                                        
         MVC   PRMASK,WORK                                                      
         DROP  R6                                                               
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,CTDSCELQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   PR60                                                             
*                                                                               
         USING CTDSCD,R6                                                        
         MVC   PRDES,CTDSC                                                      
         DROP  R6                                                               
*                                                                               
PR60     GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BRAS  RE,PRTIDS           VALID IDS LIST                               
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,CTIPPELQ      IP PUID ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   PR80                                                             
*                                                                               
         MVC   PRPUH,=C'PU='                                                    
*                                                                               
         USING CTIPPD,R6                                                        
         ZIC   R1,CTIPPLEN                                                      
         SH    R1,=Y(CTIPPLNQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRPU(0),CTIPPID                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PR80     LR    R6,R4                                                            
         MVI   ELCODE,CTIPRELQ      IP RESOURCE NAME ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   PR90                                                             
*                                                                               
         MVC   PRRESH,=C'RESOURCE='                                             
*                                                                               
         USING CTIPRD,R6                                                        
         ZIC   R1,CTIPRLEN                                                      
         SH    R1,=Y(CTIPRLNQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRRES(0),CTIPRNAM                                                
         DROP  R6                                                               
*                                                                               
PR90     GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR20                                                             
*                                                                               
PRX      B     XIT                                                              
***********************************************************************         
* PRTIDS - PRINT COMPATIBLE ID'S                                                
***********************************************************************         
PRTIDS   NTR1                                                                   
         XC    IDCNT,IDCNT                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,CTIDELQ                                                   
*                                                                               
         BRAS  RE,GETEL                                                         
         BNE   PIDS70                                                           
         B     PIDS20                                                           
*                                                                               
PIDS10   BRAS  RE,NEXTEL                                                        
         BNE   PIDS70                                                           
*                                  ADD ID ELEMENT TO ID BLOCK                   
PIDS20   SR    R1,R1                                                            
         IC    R1,IDCNT            BUMP BLOCK COUNT                             
         LR    R4,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,IDCNT                                                         
         MHI   R4,20                                                            
         LA    R4,BLOCK1(R4)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R4),C' '          AND CLEAR IT                                 
         MVC   1(19,R4),0(R4)                                                   
         IC    R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),2(R6)       MOVE IN ID                                   
         OC    2(2,R6),2(R6)       ID LIST ELEMENT                              
         BNZ   *+10                                                             
         MVC   0(2,R4),=C'L='                                                   
         B     PIDS10                                                           
*                                                                               
PIDS70   CLI   IDCNT,0             VALID ID'S                                   
         BE    XIT                                                              
         ZIC   R5,IDCNT                                                         
         LA    R3,BLOCK1                                                        
         LA    R4,PRVLIDS                                                       
*                                                                               
PIDS80   MVC   0(20,R4),0(R3)      COPY USERID TO PRINT LINE                    
*                                                                               
         AHI   R4,20               PT TO THE LAST NON-BLANK CHAR                
         BCTR  R4,0                                                             
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         B     *-10                                                             
*                                                                               
         MVI   1(R4),C','                                                       
         AHI   R4,2                                                             
*                                                                               
         LA    RE,PRVLIDS+L'PRVLIDS                                             
         CR    R4,RE               EXCESS THE PRINT LINE LENGTH?                
         BNH   PIDS85              NO                                           
*                                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),C' '          REMOVE LAST ","                              
         GOTO1 SPOOL,DMCB,(R8)     PRINT THIS LINE, START NEW LINE              
         LA    R4,PRVLIDS                                                       
*                                                                               
PIDS85   AHI   R3,20                                                            
         BCT   R5,PIDS80                                                        
*                                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),C' '          REMOVE LAST ","                              
PIDS90   GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         B     XIT                                                              
*                                                                               
VSFMERR  GOTO1 SFMERR                                                           
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM95D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM96D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM97D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
K        EQU   1024                                                             
LWORKLNQ EQU   30*K                                                             
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
ABIGWORK DS    A                                                                
SAVEKEY  DS    XL25                CTFILE KEY                                   
*                                                                               
IPWORK   DS    8F                                                               
SNID     DS    XL16                SUBNET ID                                    
SVSNID   DS    XL16                SAVED SUBNET ID                              
SVMASK   DS    XL16                SAVED MASK                                   
FLAG     DS    X                                                                
FLINDX   DS    X                                                                
FLDCNT   DS    X                                                                
LINECNT  DS    X                                                                
*                                                                               
LUPDATE  DS    CL11                BUFFER FOR LAST UPDATED DATE                 
*                                                                               
SVAGYAP  DS    CL2                                                              
SVAGYAI  DS    CL2                                                              
AGYID    DS    CL2                                                              
AGYLIST  DS    30CL2               VALID AGENCY LIST FROM PRINCIPAL ID          
AGYLISTX EQU   *                                                                
AGYLISTQ EQU   AGYLISTX-AGYLIST                                                 
WILDCLEN DS    X                                                                
*                                                                               
IDCNT    DS    X                                                                
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
*                                                                               
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTSNID  DS    0CL39               IP SUBNET ID                                 
LSTSID   DS    CL15                V4 IP SUBNET ID                              
         DS    5C                                                               
LSTSMASK DS    CL15                V4 IP SUBNET MASK                            
         DS    4C                                                               
         DS    C                                                                
LSTDES   DS    CL30                IP SUBNET DESCRIPTION                        
         DS    C                                                                
*                                                                               
* PRINT LINE                                                                    
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    C                                                                
PRSNID   DS    CL39                SUBNET ID                                    
         DS    CL2                                                              
PRMASK   DS    CL39                SUBNET MASK                                  
         DS    CL2                                                              
PRPID    DS    CL10                PRINCIPAL ID                                 
         DS    CL2                                                              
PRDES    DS    CL30                DESCRIPTION                                  
*                                                                               
         ORG   P                                                                
         DS    CL42                                                             
PRVLIDS  DS    CL65                VALID ID LIST                                
         DS    CL11                FOR 1 USERID OVERFLOW                        
*                                                                               
         ORG   P                                                                
         DS    CL42                                                             
PRPUH    DS    CL3                 PU=                                          
PRPU     DS    CL60                PU                                           
*                                                                               
         ORG   P                                                                
         DS    CL42                                                             
PRRESH   DS    CL9                 RESOURCE=                                    
PRRES    DS    CL60                RESOURCE                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019CTSFM21   10/23/13'                                      
         END                                                                    
