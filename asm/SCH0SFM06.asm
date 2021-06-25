*          DATA SET SCH0SFM06  AT LEVEL 086 AS OF 11/11/98                      
*PHASE T21706                                                                   
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21706  -- CLIENT MAINTENANCE                        *         
*                T21707  -- CLIENT LIST                               *         
*                                                                     *         
*  COMMENTS:     MAINTAINS CLIENTS RECORDS                            *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SPSFM76 (MAINT) & SPSFM77 (LIST)              *         
*                                                                     *         
*  OUTPUTS:                                                           *         
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
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21706 - CLIENT MAINTENANCE'                                    
T21706   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1706**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
*        CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
*        BE    DK                                                               
*        CLI   MODE,DISPREC        DISPLAY RECORD                               
*        BE    DR                                                               
*        CLI   MODE,LISTRECS                                                    
*        BE    LR                  LIST RECORDS                                 
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTRECD,R6                                                       
         LA    R2,CLTMEDH           MEDIA                                       
         GOTO1 VALIMED                                                          
         FOUT  CLTMEDNH,MEDNM                                                   
         MVC   CKEYAM,BAGYMD        AGENCY/MEDIA CODE                           
*                                                                               
         LA    R2,CLTCLTH           CLIENT                                      
         GOTO1 VALICLT                                                          
         FOUT  CLTCLTNH,CLTNM                                                   
         MVC   CKEYCLT,BCLT         BINARY CLIENT CODE                          
*                                                                               
VKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0X                                                               
*                                                                               
         LA    R4,OPTNTAB                                                       
         USING OPTTABD,R4                                                       
VR10     LH    R2,SCRDISD                                                       
         CH    R2,=X'FFFF'         SEE IF AT END OF TABLE                       
         BE    VRX                                                              
         AR    R2,RA               POINT R2 TO THE HEADER OF ENTRY              
         LH    R3,RECDISD                                                       
         A     R3,AIO              POINT R3 TO THE TARGET IN AIO                
         L     RF,AVALD            VALIDATE FIELD ON SCREEN                     
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         LA    R4,OPTTABQ(R4)      BUMP THE TABLE POINTER                       
         B     VR10                                                             
VRX      L     R6,AIO                                                           
         B     XIT                                                              
*        B     DR                  REDISPLAY RECORD                             
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0X                                                               
*                                                                               
         MVC   BACKKEY(L'KEY),KEY  ***********                                  
*                                                                               
         LA    R4,OPTNTAB                                                       
         USING OPTTABD,R4                                                       
DR10     LH    R2,SCRDISD                                                       
         CH    R2,=X'FFFF'         SEE IF AT END OF TABLE                       
         BE    DRX                                                              
         AR    R2,RA               POINT R2 TO THE HEADER OF ENTRY              
         LH    R3,RECDISD                                                       
         A     R3,AIO              POINT R3 TO THE TARGET IN AIO                
         L     RF,ADISD            DISPLAY FIELD ON SCREEN                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         LA    R4,OPTTABQ(R4)      BUMP THE TABLE POINTER                       
         B     DR10                                                             
*RX      MVC   KEY,BACKKEY                                                      
*        GOTO1 READ                                                             
*        GOTO1 GETREC                                                           
*        LA    R5,KEY                                                           
*        LA    R7,BACKKEY                                                       
*        LA    R9,KEYSAVE                                                       
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
*                                                                               
DKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       DS    0X                                                               
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        TABLES, CONSTANTS                                            *         
***********************************************************************         
RATETABU DC    CL3'NSI',CL1'0'     RATING SERVICE TABLE FOR USA                 
         DC    CL3'ARB',CL1'1'                                                  
RATETABC DC    CL3'CSI',CL1'0'     RATING SERVICE TABLE FOR CAN                 
         DC    CL3'BBM',CL1'1'                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
                                                                                
PROFTAB1 DC    AL1(05),AL1(01),C'012'           BRAND/POL TRNDS                 
         DC    AL1(03),AL1(02),X'FF'            LOCK BOX NUM                    
PROFTAB3 DC    AL1(06),AL1(03),C'0123'          MKT/STA TRNDS                   
         DC    AL1(04),AL1(04),C'01'            RATING SERVICE                  
         DC    AL1(09),AL1(05),C'0123459'       BILL FORMULA CNTRL              
         DC    AL1(05),AL1(06),C'012'           BILL ESTIMATE CNTRL             
         DC    AL1(08),AL1(07),C'0123YN'   Y IS ONLY REAL VALUE                 
         DC    AL1(13),AL1(08),C'0123456789*'   PRINT EST SERIES NM             
         DC    AL1(04),AL1(09),C'01'            GOALS CPP OVERRIDE              
         DC    AL1(05),AL1(10),C'012'           PROGRAM ADJ CNTRL               
         DC    AL1(05),AL1(11),C'012'           POL TIMESHEET DEMOS             
         DC    AL1(06),AL1(12),C'01YN'     Y IS ONLY REAL VALUE                 
         DC    AL1(05),AL1(13),C'0YN'           PRD REQ FOR TRUE POL            
         DC    AL1(03),AL1(14),X'FF'            EXCL GROUP CODE                 
         DC    AL1(14),AL1(15),C'0123456789Y*'  CLIENT RATE CNTRL               
*                                                                               
XTRATAB  DC    AL1(05),AL1(16),C'0UC'           CANADIAN DEMO OPTION            
         DC    AL1(04),AL1(17),C'01'            CANADIAN NET TAX                
KPROF18  DC    AL1(KPROF18X-KPROF18),AL1(18)    BUY ID REQ                      
         DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ*'                                   
KPROF18X EQU   *                                                                
         DC    AL1(04),AL1(19),C'NY'            EST FILTERS REQ                 
         DC    AL1(04),AL1(20),C'0E'            CAMPAIGNS                       
         DC    AL1(05),AL1(21),C'NYD'           US SPILL                        
         DC    AL1(04),AL1(22),C'NY'            EST=NO EST NAME                 
         DC    AL1(04),AL1(23),C'NY'            MKGDS IN MISSED MTH             
         DC    AL1(04),AL1(24),C'NY'            GOAL REQD FOR BUY               
         DC    AL1(05),AL1(25),C'0CU'           COUNTRY                         
         DC    AL1(04),AL1(26),C'NY'            OUT-OF-WEEK                     
         DC    AL1(06),AL1(27),C'SUXZ'          GST CODE                        
         DC    AL1(04),AL1(28),C'NY'            SPECIAL DEMO ADJ                
         DC    AL1(04),AL1(29),C'NY'            PRD REQ FOR ADDS SEND           
         DC    X'0000'                                                          
         SPACE 2                                                                
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'00'                    
         EJECT                                                                  
*                                                                               
OPTNTAB  DS    0F                                                               
         DC    AL2(CLTONUMH-T217FFD),AL2(COFFICE-CKEY)                          
         DC    A(VALOFF),A(DISOFF)                                              
         DC    AL2(CLTAOFCH-T217FFD),AL2(CACCOFC-CKEY)                          
         DC    A(VALACC),A(DISACC)                                              
         DC    AL2(CLTIFCDH-T217FFD),AL2(CCLTIFC-CKEY)                          
         DC    A(VALIFC),A(DISIFC)                                              
         DC    AL2(CLTOP1AH-T217FFD),AL2(CPOLONLY-CKEY)                         
         DC    A(VALYN),A(DISYN)                                                
         DC    AL2(CLTOPTSH-T217FFD),AL2(COPT1-CKEY)                            
         DC    A(VALOPTS),A(DISOPTS)                                            
         DC    AL2(CLTOP1H-T217FFD),AL2(CPROF+0-CKEY)                           
         DC    A(VALPROF),A(DISSAME)                                            
         DC    AL2(CLTOP3H-T217FFD),AL2(CPROF+2-CKEY)                           
         DC    A(VALMST),A(DISMST)                                              
         DC    AL2(CLTOP8H-T217FFD),AL2(CPROF+7-CKEY)                           
         DC    A(VALPROF),A(DISYN)                                              
         DC    AL2(CLTOP4H-T217FFD),AL2(CPROF+3-CKEY)                           
         DC    A(VALRATE),A(DISRATE)                                            
         DC    AL2(CLTOP11H-T217FFD),AL2(CPROF+10-CKEY)                         
         DC    A(VALPROF),A(DISYN)                                              
         DC    AL2(CLTOP7H-T217FFD),AL2(CPROF+6-CKEY)                           
         DC    A(VALPROF),A(DISYN)                                              
         DC    AL2(CLTOP9H-T217FFD),AL2(CPROF+8-CKEY)                           
         DC    A(VALPROF),A(DISYN)                                              
         DC    AL2(CLTOP10H-T217FFD),AL2(CPROF+9-CKEY)                          
         DC    A(VALPROF),A(DISSAME)                                            
         DC    AL2(CLTOP12H-T217FFD),AL2(CPROF+11-CKEY)                         
         DC    A(VALPROF),A(DISYN)                                              
         DC    AL2(CLTOP13H-T217FFD),AL2(CPROF+12-CKEY)                         
         DC    A(VALPROF),A(DISYN)                                              
         DC    AL2(CLTOP15H-T217FFD),AL2(CPROF+14-CKEY)                         
         DC    A(VALPROF),A(DISSAME)                                            
         DC    AL2(CLTEX1H-T217FFD),AL2(CEXTRA+0-CKEY)                          
         DC    A(VALEXTRA),A(DISYN)                                             
         DC    AL2(CLTEX3H-T217FFD),AL2(CEXTRA+2-CKEY)                          
         DC    A(VALEXTRA),A(DISYN)                                             
         DC    AL2(CLTEX4H-T217FFD),AL2(CEXTRA+3-CKEY)                          
         DC    A(VALEXTRA),A(DISYN)                                             
         DC    AL2(CLTEX6H-T217FFD),AL2(CEXTRA+5-CKEY)                          
         DC    A(VALEXTRA),A(DISYN)                                             
         DC    AL2(CLTEX7H-T217FFD),AL2(CEXTRA+6-CKEY)                          
         DC    A(VALEXTRA),A(DISYN)                                             
         DC    AL2(CLTEX9H-T217FFD),AL2(CEXTRA+8-CKEY)                          
         DC    A(VALEXTRA),A(DISYN)                                             
         DC    AL2(CLTEX10H-T217FFD),AL2(CEXTRA+9-CKEY)                         
         DC    A(VALCTRY),A(DISCTRY)                                            
         DC    AL2(CLTEX12H-T217FFD),AL2(CEXTRA+11-CKEY)                        
         DC    A(VALGST),A(DISGST)                                              
         DC    AL2(CLTEX13H-T217FFD),AL2(CEXTRA+12-CKEY)                        
         DC    A(VALEXTRA),A(DISYN)                                             
         DC    AL2(CLTDLYH-T217FFD),AL2(CDAILY-CKEY)                            
         DC    A(VALDLY),A(DISDLY)                                              
         DC    AL2(CLTOP14H-T217FFD),AL2(CPROF+13-CKEY)                         
         DC    A(VALPROF),A(DISSAME)                                            
         DC    AL2(CLTEX11H-T217FFD),AL2(CEXTRA+10-CKEY)                        
         DC    A(VALEXTRA),A(DISYN)                                             
         DC    AL2(CLTEX14H-T217FFD),AL2(CEXTRA+13-CKEY)                        
         DC    A(VALEXTRA),A(DISYN)                                             
         DC    AL2(CLTTPRDH-T217FFD),AL2(CMCLTPRD-CKEY)                         
         DC    A(VALPRD),A(DISPRD)                                              
         DC    AL2(CLTTTLEH-T217FFD),AL2(CTITLE-CKEY)                           
         DC    A(VALTTL),A(DISTTL)                                              
         DC    AL2(CLTTCLTH-T217FFD),AL2(CMCLTCOD-CKEY)                         
         DC    A(VALCLTSQ),A(DISCLTSQ)                                          
         DC    AL2(CLTMDNMH-T217FFD),AL2(CMEDNAME-CKEY)                         
         DC    A(VALTTL),A(DISTTL)                                              
         DC    AL2(CLTPSTH-T217FFD),AL2(CPST-CKEY)                              
         DC    A(VALPST),A(DISPST)                                              
         DC    X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VALNAME  NTR1                                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         GOTO1 ANY                                                              
         MVC   CNAME,WORK                                                       
         XIT1                                                                   
         DROP  R6                                                               
***********************************************************************         
VALOFF   NTR1                                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         CLI   SVAPROF+13,C'Y'      IS OFFICE NUMBER REQUIRED?                  
         BE    OFF10                                                            
         CLI   5(R2),0                                                          
         BE    OFFX                                                             
OFF10    GOTO1 ANY                                                              
*        TM    AGYFLAG1,X'10'       OFF=HEX OPTION IN USE?                      
*        BO    OFF20                YES                                         
*        CLI   8(R2),C'A'                                                       
*        BL    ERRINV                                                           
OFF20    CLI   WORK,C'='            INVALID OFFICE NUMBERS                      
         BE    ERRINV                                                           
         CLI   WORK,C','                                                        
         BE    ERRINV                                                           
         CLI   WORK,C'-'                                                        
         BE    ERRINV                                                           
         MVC   COFFICE,WORK                                                     
OFFX     XIT1                                                                   
***********************************************************************         
VALACC   NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALRATE  NTR1                                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         CLI   8(R2),C'0'                                                       
         BE    VRATE05                                                          
         CLI   8(R2),C'1'                                                       
         BE    VRATE05                                                          
         CLI   SVAPROF+7,C'C'                                                   
         BE    VRATE01                                                          
*                                                                               
         LA    R1,RATETABU                                                      
         LA    RE,2                                                             
RTLOOP1A CLC   8(3,R2),0(R1)        SEE IF INPUT WAS AN US SERVICE              
         BNE   RTLOOP1                                                          
         MVC   CPROF+3(1),3(R1)                                                 
         B     VRATE05A                                                         
RTLOOP1  LA    R1,4(R1)                                                         
         BCT   RE,RTLOOP1A                                                      
*                                                                               
VRATE01  LA    R1,RATETABC                                                      
         LA    RE,2                                                             
RTLOOP2A CLC   8(3,R2),0(R1)        SEE IF INPUT WAS A CANADIAN SERVICE         
         BNE   RTLOOP2                                                          
         MVC   CPROF+3(1),3(R1)                                                 
         B     VRATE05A                                                         
RTLOOP2  LA    R1,4(R1)                                                         
         BCT   RE,RTLOOP2A                                                      
*                                                                               
         B     ERRINV               NEITHER, THEN ERROR                         
*                                                                               
VRATE05  MVC   CPROF+3(1),8(R2)                                                 
VRATE05A CLI   CLTMED,C'R'          FOR MEDIA R                                 
         BNE   VRATE10               RATING SERVICE=1 ONLY                      
         MVI   CPROF+3,C'1'                                                     
VRATE10  CLI   CLTMED,C'T'          FOR MEDIA T                                 
         BNE   VRATEX                AND "NOT" CANADIAN AGY                     
         CLI   SVAPROF+7,C'C'        RATING SERVICE=0 ONLY                      
         BE    VRATEX                                                           
         MVI   CPROF+3,C'0'                                                     
VRATEX   XIT1                                                                   
         DROP  R6                                                               
***********************************************************************         
VALMST   NTR1                                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         LA    R1,PROFTAB3         MKT/STA TURNAROUND IS PROFILE #3             
         ZIC   R5,0(R1)                                                         
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         LA    R1,2(R1)                                                         
MSTLOOP1 CLC   8(1,R2),0(R1)                                                    
         BE    VMST20                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,MSTLOOP1                                                      
         B     VMST30                                                           
VMST20   MVC   CPROF+3(1),8(R2)                                                 
VMST30   XIT1                                                                   
***********************************************************************         
VALEXTRA NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALPROF  NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALIFC   NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALDLY   NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALYN    NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALOPTS  NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALOZ    NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALCTRY  NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALPRD   NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALTTL   NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALCLTSQ NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALGST   NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
VALPST   NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
DISOFF   NTR1                                                                   
         FOUT  (R2),(R3),1                                                      
         XIT1                                                                   
***********************************************************************         
DISACC   NTR1                                                                   
         FOUT  (R2),(R3),5                                                      
         XIT1                                                                   
***********************************************************************         
DISIFC   NTR1                                                                   
         FOUT  (R2),(R3),8                                                      
         XIT1                                                                   
***********************************************************************         
DISYN    NTR1                                                                   
         CLI   0(R3),C'Y'                                                       
         BE    DISYES                                                           
         CLI   0(R3),C'1'                                                       
         BE    DISYES                                                           
         FOUT  (R2),=C'N'                                                       
         XIT1                                                                   
DISYES   FOUT  (R2),=C'Y'                                                       
         XIT1                                                                   
***********************************************************************         
DISSAME  NTR1                                                                   
         FOUT  (R2),(R3),1                                                      
         XIT1                                                                   
***********************************************************************         
DISRATE  NTR1                                                                   
         CLI   0(R3),C'0'                                                       
         BNE   RATE20                                                           
         CLI   SVAPROF+7,C'C'                                                   
         BNE   RATE10                                                           
         FOUT  (R2),=C'CSI',3                                                   
         B     RATEX                                                            
RATE10   FOUT  (R2),=C'NSI',3                                                   
         B     RATEX                                                            
RATE20   CLI   SVAPROF+7,C'C'                                                   
         BNE   RATE30                                                           
         FOUT  (R2),=C'BBM'                                                     
         B     RATEX                                                            
RATE30   FOUT  (R2),=C'ARB'                                                     
RATEX    XIT1                                                                   
***********************************************************************         
DISOPTS  NTR1                                                                   
         XIT1                                                                   
***********************************************************************         
DISPRD   NTR1                                                                   
         XIT1                                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         CLI   CMCLTPRD,0                                                       
         BE    PRDCLR                                                           
         OC    CMCLTCOD,CMCLTCOD                                                
         BZ    PRDCLR                                                           
*                                                                               
         FOUT  (R2),SPACES,3                                                    
         MVC   KEYSAVE,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),KEYSAVE+1                                               
         MVC   KEY+2(2),CMCLTCOD                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R4,CLIST                                                         
DISPRD10 CLC   3(1,R4),0(R3)                                                    
         BE    DISPRD20                                                         
         LA    R4,4(R4)                                                         
         CLI   0(R4),C'A'                                                       
         BNL   DISPRD10                                                         
         DC    H'0'                                                             
DISPRD20 MVC   8(L'CLTTPRD,R2),0(R4)                                            
DISPRDX  XIT1                                                                   
PRDCLR   FOUT  (R2),SPACES,3                                                    
         B     DISPRDX                                                          
***********************************************************************         
DISDLY   NTR1                                                                   
         FOUT  (R2),=C'N',1                                                     
         CLI   0(R3),0                                                          
         BE    XITDLY                                                           
         FOUT  (R2),(R3),1                                                      
XITDLY   XIT1                                                                   
***********************************************************************         
DISTTL   NTR1                                                                   
         FOUT  (R2),(R3),10                                                     
         XIT1                                                                   
***********************************************************************         
DISMST   NTR1                           MARKET/STATION TURNAROUND               
         CLI   0(R3),C'0'                                                       
         BNE   MST10                                                            
         FOUT  (R2),=C'U5M',3                                                   
         B     MSTX                                                             
MST10    CLI   0(R3),C'1'                                                       
         BNE   MST20                                                            
         FOUT  (R2),=C'U5S',3                                                   
         B     MSTX                                                             
MST20    CLI   0(R3),C'2'                                                       
         BNE   MST20                                                            
         FOUT  (R2),=C'U3M',3                                                   
         B     MSTX                                                             
MST30    CLI   0(R3),C'3'                                                       
         BNE   MST30                                                            
         FOUT  (R2),=C'U3S',3                                                   
MSTX     XIT1                                                                   
***********************************************************************         
DISCLTSQ NTR1                                                                   
         XIT1                           ***********88                           
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         OC    CMCLTCOD,CMCLTCOD        TEST SPCL TRAFFIC CLIENT                
         BZ    SQCLEAR                                                          
*                                                                               
         FOUT  (R2),SPACES,6                                                    
         MVC   DMCB+4(4),=X'D9000A15'   GET CLUNPK ADDRESS                      
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'              CHECK FOR ERROR                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                  LOAD A(CLUNPK)                          
         GOTO1 (RF),(R1),(C'Y',CMCLTCOD),8(R2)                                  
*                                       UNPACK CLT CODE INTO FLD                
         MVI   10(R2),C'='              PUT = INTO 3RD CHAR IN FIELD            
*                                       MOVE THE CLT SEQ INTO 5TH CHAR          
         GOTO1 HEXOUT,DMCB,CMCLTUNQ,11(R2),1,=C'TOG'                            
*                                                                               
DISCLTX  XIT1                                                                   
SQCLEAR  FOUT  (R2),SPACES,6                                                    
         B     DISCLTX                                                          
         DROP  R6                                                               
***********************************************************************         
DISCTRY  NTR1                                                                   
         FOUT  (R2),(R3),1                                                      
         XIT1                                                                   
***********************************************************************         
DISGST   NTR1                                                                   
         FOUT  (R2),(R3),1                                                      
         XIT1                                                                   
***********************************************************************         
DISPST   NTR1                                                                   
                                                                                
         FOUT  (R2),SPACES,45                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         OC    CPST,CPST           IS THERE ANYTHING TO DISPLAY                 
         BZ    DISPSTX                                                          
                                                                                
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,CPST                                                          
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
                                                                                
         MVI   DMCB+7,QPSTVAL      CALL PSTVAL TO VALIDATE PST CODES            
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   8(45,R2),PSTOUT       OUTPUT                                     
                                                                                
DISPSTX  XIT1                                                                   
         DROP  R6                                                               
         DROP  R4                                                               
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
******** ***** ********************************************************         
OPTTABD  DSECT                                                                  
SCRDISD  DS    AL2                  DISPLACEMENT INTO THE SCREEN                
RECDISD  DS    AL2                  DISPLACEMENT INTO THE RECORD                
AVALD    DS    A                    ADDRESS OF VALIDATION ROUTINES              
ADISD    DS    A                    ADDRESS OF DISPLAY ROUTINES                 
OPTTABQ  EQU   *-OPTTABD                                                        
*                                                                               
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFM76D          MAINTENACE SCREEN                            
         EJECT                                                                  
*        ORG   CONTAGH                                                          
*      ++INCLUDE SPSFM77D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAPY          AUTOPAY RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE SPGENAGY          AGENCY PROFILES                              
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE DDFLDIND          FOR FOUT                                     
         EJECT                                                                  
       ++INCLUDE DDPSTBLK          FOR PSTVAL                                   
         EJECT                                                                  
       ++INCLUDE DDCOREQUS         FOR PSTVAL                                   
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
BACKKEY  DS    CL48                BACK UP KEY                                  
FAKEFLD  DS    XL11                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
*                                                                               
PSTOUT   DS    CL64                PSTVAL OUTPUT FIELD                          
APSTVAL  DS    A                   A(PSTVAL)                                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
LSMED    DS    CL1                                                              
         DS    CL3                                                              
LSCLT    DS    CL3                                                              
         DS    CL1                                                              
LSPRD    DS    CL3                                                              
         DS    CL1                                                              
LSPRD2   DS    CL3                                                              
         DS    CL1                                                              
LSEST    DS    CL3                                                              
         DS    CL1                                                              
LSSTA    DS    CL8                                                              
         DS    CL1                                                              
LSMOY    DS    CL6                                                              
         DS    CL1                                                              
LSSREP   DS    CL4                                                              
         DS    CL2                                                              
LSPAYDT  DS    CL8                                                              
         DS    CL2                                                              
LSINV    DS    CL11                                                             
         DS    CL1                                                              
LSDATE   DS    CL8                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086SCH0SFM06 11/11/98'                                      
         END                                                                    
