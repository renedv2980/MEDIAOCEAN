*          DATA SET DELMXR1    AT LEVEL 006 AS OF 05/08/17                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMXR1A                                                                 
***********************************************************************         
* THIS VERSION PRODUCES BOTH PGM LINEUP, AND PROGRAMS BROKEN UP BY              
* QUARTER HOURS.  IT WAS DECIDED WE WON'T NEED THE INDIVIDUAL PGM               
* LINEUP, SO I SAVED THIS VERSION AND CREATED ANOTHER ONE TO ONLY               
* PRODUCE PROGRAMS BROKEN UP BY QUARTER HOUR.                                   
***********************************************************************         
*INCLUDE REGSAVE                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE NSIWEEK                                                                
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE LMTIME                                                                 
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DELMICETPA, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.         *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* NOTE: CALLS TO EXTERNAL SUBROUTINES *MUST* BE MADE VIA              *         
*       CALL_DDS_SUBRTN, BECAUSE THIS EXIT'S USE OF REGISTERS IS      *         
*       NON-DDS-CONFORMING.                                           *         
*                                                                     *         
***********************************************************************         
DELMXR1  CSECT                                                                  
*                                                                               
         ENTRY E15                 MUST BE "E15" (FOR DFSORT)                   
*                                                                               
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         USING E15,RC              RC = PROGRAM BASE REGISTER                   
E15      STM   RE,RC,12(RD)        SAVE ALL REGS EXCEPT RD                      
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         BC    0,MAIN10            *** SELF-MODIFYING CODE ***                  
         MVI   *-3,X'F0'           *** ONLY DO THIS ONCE   ***                  
*                                                                               
         MVC   VADDAY,=V(ADDAY)                                                 
         MVC   VGETDAY,=V(GETDAY)                                               
         MVC   VDATCON,=V(DATCON)                                               
         XC    PREV_WK,PREV_WK                                                  
         MVI   WK#,1                                                            
         MVI   NEW_REC,C'Y'                                                     
*                                                                               
MAIN10   DS    0H                                                               
*                                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BZ    EOF                 YES: DO NOT RETURN                           
         USING LMDSECT,R3                                                       
*                                                                               
         CLC   W_KCODE,=C'07'                                                   
         BE    QH01                                                             
         CLC   W_KCODE,=C'08'                                                   
         BE    QH01                                                             
         CLC   W_KCODE,=C'09'                                                   
         BE    QH01                                                             
         CLC   W_KCODE,=C'10'                                                   
         BE    QH01                                                             
         CLC   W_KCODE,=C'11'                                                   
         BE    DELREC                                                           
         CLC   W_KCODE,=C'21'                                                   
         BE    DELREC                                                           
         CLC   W_KCODE,=C'12'                                                   
         BE    PL01                                                             
         CLC   W_KCODE,=C'22'                                                   
         BE    PL01                                                             
         B     KR10                                                             
*                                                                               
************************************************************                    
* .   CALCULATE THE WEEK NUMBER FOR THE CURRENT RECORD                          
* ..  STORE DDS QHR IN KEY FOR SORTING                                          
* ... CHANGE DATE ON RECORD TO MATCH 24 HOUR SAMPLE DATE                        
************************************************************                    
QH01     DS    0H                                                               
*                                                                               
         LA    RE,W_KCALDT+11      QUARTER HOUR                                 
         XC    DUB,DUB                                                          
         MVC   DUB(2),0(RE)                                                     
         MVC   DUB+2(3),3(RE)                                                   
         GOTOR ,DMCB,(1,DUB),QHR                                                
         L     RF,=V(HRTOQH2)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         GOTOR ,DMCB,(0,W_KCALDT),DUB                                           
         L     RF,=V(DATVAL)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         CLI   QHR,X'60'           12A-445A MINUS A DAY                         
         BNL   QH10                                                             
         CLI   QHR,X'4C'                                                        
         BL    QH10                                                             
         LHI   R4,-1                                                            
         GOTOR ,DMCB,DUB,DUB,(R4)                                               
         L     RF,=V(ADDAY)                                                     
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
QH10     GOTOR ,DMCB,DUB,(4,VGETDAY),VADDAY,VDATCON                             
         L     RF,=V(NSIWEEK)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
         CLC   PREV_WK,0(R1)                                                    
         BE    QH40                                                             
*                                                                               
         CLC   W_KSEQ,PREV_SEQ                                                  
         BNE   QH20                                                             
         CLC   W_KCODE,PREV_REC                                                 
         BNE   QH20                                                             
         CLC   W_KDISC,PREV_STA                                                 
         BNE   QH20                                                             
         CLC   W_KSAMTY,PREV_SAM                                                
         BE    QH30                                                             
*                                                                               
QH20     XC    PREV_WK,PREV_WK                                                  
         MVI   WK#,0                                                            
         MVC   PREV_SEQ,W_KSEQ                                                  
         MVC   PREV_REC,W_KCODE                                                 
         MVC   PREV_SAM,W_KSAMTY                                                
         MVC   PREV_STA,W_KDISC                                                 
*                                                                               
QH30     MVC   PREV_WK,0(R1)                                                    
         ZIC   R1,WK#                                                           
         AHI   R1,1                                                             
         STC   R1,WK#                                                           
*                                                                               
QH40     MVC   W_KWEEK,WK#                                                      
         MVC   W_KQHR,QHR                                                       
*                                                                               
         GOTOR ,DMCB,(0,DUB),DUB2                                               
         L     RF,=V(GETDAY)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         MVC   W_KDAY,0(R1)                                                     
         OI    W_KDAY,X'F0'                                                     
*                                                                               
         GOTOR ,DMCB,(0,DUB),(20,DUB)                                           
         L     RF,=V(DATCON)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         MVC   W_KYEAR,DUB                                                      
         MVC   W_KDATE(2),DUB+4                                                 
         MVC   W_KDATE+3(2),DUB+6                                               
         B     KEEPREC                                                          
*                                                                               
************************************************************                    
* .    CREATE INDIVIDUAL QTR HOUR RECORDS FOR EACH PROGRAM                      
* ..   CALCULATE THE WEEK NUMBER FOR THE CURRENT RECORD                         
* ...  STORE DDS QHR IN KEY FOR SORTING                                         
* .... CHANGE DATE ON RECORD TO MATCH 24 HOUR SAMPLE DATE                       
************************************************************                    
PL01     DS    0H                                                               
*                                                                               
         CLC   W_KCODE,=C'22'                                                   
         BNE   *+12                                                             
         MVI   W_RECTYPE,RECTYPE_PUP                                            
         B     *+8                                                              
         MVI   W_RECTYPE,RECTYPE_PQR                                            
*                                                                               
         CLI   NEXT_REC,C'Y'                                                    
         BE    PL90                                                             
         CLI   NEW_REC,C'Y'                                                     
         BNE   PL20                                                             
*                                                                               
         LA    RE,W_PNRQHSDT+11    START QHR                                    
         XC    DUB,DUB                                                          
         MVC   DUB(2),0(RE)                                                     
         MVC   DUB+2(3),3(RE)                                                   
         GOTOR ,DMCB,(1,DUB),SQHR                                               
         L     RF,=V(HRTOQH2)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
         MVC   PGM_SQHR,SQHR                                                    
*                                                                               
         GOTOR ,DMCB,(0,W_PNRQHSDT),SDATE START DATE                            
         L     RF,=V(DATVAL)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         LA    RE,W_PNRQHEDT+11    END QHR                                      
         XC    DUB,DUB                                                          
         MVC   DUB(2),0(RE)                                                     
         MVC   DUB+2(3),3(RE)                                                   
         GOTOR ,DMCB,(1,DUB),EQHR                                               
         L     RF,=V(HRTOQH2)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
         CLI   EQHR,0                                                           
         BNE   *+8                                                              
         MVI   EQHR,X'60'                                                       
*                                                                               
         GOTOR ,DMCB,(0,W_PNRQHEDT),EDATE END DATE                              
         L     RF,=V(DATVAL)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         CLI   SQHR,X'4C'          12A-445A MINUS A DAY                         
         BL    PL15                                                             
         LHI   R4,-1                                                            
         GOTOR ,DMCB,SDATE,SDATE,(R4)                                           
         L     RF,=V(ADDAY)                                                     
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
PL15     CLI   EQHR,X'4C'          12A-445A MINUS A DAY                         
         BL    PL17                                                             
         LHI   R4,-1                                                            
         GOTOR ,DMCB,EDATE,EDATE,(R4)                                           
         L     RF,=V(ADDAY)                                                     
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
PL17     ZIC   RE,EQHR             THE END QHR OF 6A IS 545A.                   
         SHI   RE,1                                                             
         STC   RE,EQHR                                                          
         MVC   PGM_EQHR,EQHR                                                    
*                                                                               
PL20     CLC   SDATE,EDATE                                                      
         BNE   PL30                                                             
         CLC   SQHR,EQHR                                                        
         BNE   PL30                DONE FOR THIS PROGRAM LINEUP                 
         MVI   NEXT_REC,C'Y'       AFTER ADDING THIS QTR RECORD                 
*                                  PUT OUT ORIGINAL PROGRAM LINEUP              
*                                  THEN GO GET THE NEXT LINEUP                  
*                                                                               
PL30     GOTOR ,DMCB,(0,SDATE),(20,DUB)                                         
         L     RF,=V(DATCON)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         MVC   W_KYEAR,DUB                                                      
         MVC   W_KDATE(2),DUB+4                                                 
         MVC   W_KDATE+3(2),DUB+6                                               
         MVC   W_KQHR,SQHR                                                      
*                                                                               
         MVC   W_PNRSQH,PGM_SQHR                                                
         MVC   W_PNREQH,PGM_EQHR                                                
         CLC   PGM_SQHR,PGM_EQHR   PROGRAM CROSSES DAY?                         
         BNH   *+8                 GOING TO CREATE TWO RECORDS                  
         MVI   W_PNREQH,X'5F'                                                   
         BNE   PL35                PGM_SQH = PGM_EQH?                           
         CLC   SDATE,EDATE         SAME DAY : MEANS PGM HAS 1/4HR DUR           
         BE    PL35                DIFF DAY : 24 HOUR PGM? EEEKS...             
         MVI   W_PNREQH,X'5F'                                                   
*                                                                               
PL35     ZIC   RE,W_PNREQH                                                      
         ZIC   R1,W_PNRSQH                                                      
         SR    RE,R1                                                            
         AHI   RE,1                                                             
         STCM  RE,3,W_PNRLQH                                                    
*                                                                               
         GOTOR ,DMCB,SDATE,(4,VGETDAY),VADDAY,VDATCON                           
         L     RF,=V(NSIWEEK)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
         CLC   PREV_WK,0(R1)       SAME WEEK?                                   
         BE    PL70                                                             
*                                                                               
         CLC   W_KSEQ,PREV_SEQ     SAME FILE?                                   
         BNE   PL50                                                             
         CLC   W_KCODE,PREV_REC    SAME RECORD?                                 
         BNE   PL50                                                             
         CLC   W_KDISC,PREV_STA    SAME STATION?                                
         BNE   PL50                                                             
         CLC   W_KSAMTY,PREV_SAM   SAME SAMPLE?                                 
         BE    PL60                                                             
*                                                                               
PL50     XC    PREV_WK,PREV_WK     RESTART THE WEEK COUNT                       
         MVI   WK#,0                                                            
         MVC   PREV_SEQ,W_KSEQ                                                  
         MVC   PREV_REC,W_KCODE                                                 
         MVC   PREV_SAM,W_KSAMTY                                                
         MVC   PREV_STA,W_KDISC                                                 
         MVC   TELEID,=H'01'       TELECAST ID                                  
         CLI   W_RECTYPE,RECTYPE_PUP                                            
         BNE   *+10                                                             
         MVC   TELEID,=H'5001'     PROGRAM UPDATES STARTS AT 5000               
*                                                                               
PL60     MVC   PREV_WK,0(R1)                                                    
         ZIC   R1,WK#                                                           
         AHI   R1,1                                                             
         STC   R1,WK#                                                           
*                                                                               
PL70     MVC   W_KWEEK,WK#                                                      
*                                                                               
         GOTOR ,DMCB,(0,SDATE),DUB2                                             
         L     RF,=V(GETDAY)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         MVC   W_KDAY,0(R1)                                                     
         OI    W_KDAY,X'F0'                                                     
         LH    RE,TELEID                                                        
         EDIT  (RE),W_PNRSEQ,FILL=0                                             
*                                                                               
         ZIC   R1,SQHR             FOR NEXT ITERATION                           
         AHI   R1,1                                                             
         STC   R1,SQHR                                                          
*                                                                               
         CLI   SQHR,X'60'          X'60' IS 5A OF NEXT DAY                      
         BL    PL72                                                             
         MVI   SQHR,0                                                           
         LH    RE,TELEID           BUMP TELECAST ID                             
         AHI   RE,1                                                             
         STCM  RE,3,TELEID                                                      
         MVI   PGM_SQHR,0                                                       
*                                                                               
         LHI   R4,1                ADD A DAY                                    
         GOTOR ,DMCB,SDATE,SDATE,(R4)                                           
         L     RF,=V(ADDAY)                                                     
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
PL72     CLI   NEW_REC,C'Y'        ON NEW RECORDS, SAVE DATE INFO               
         BNE   PL75                                                             
         MVC   SV_SYEAR,W_KYEAR    THESE FIELDS ARE SAVED TO PRODUCE            
         MVC   SV_SDATE(6),W_KDATE PROGRAM LINEUP RECORDS THAT                  
         MVC   SV_SQHR,W_KQHR      CROSSES THE DAY BOUNDARY                     
         MVC   SV_SWK#,W_KWEEK                                                  
         MVC   SV_SKDAY,W_KDAY                                                  
         MVI   NEW_REC,C'N'                                                     
*                                                                               
PL75     CLI   NEXT_REC,C'Y'       IF INDICATOR IS TURNED ON                    
         BNE   ADDREC                                                           
         MVC   SV_EYEAR,W_KYEAR    IT MEANS CURRENT RECORD IS THE               
         MVC   SV_EDATE(6),W_KDATE LAST QUARTER HOUR OF PROGRAM                 
         MVC   SV_EQHR,W_KQHR                                                   
         MVC   SV_EWK#,W_KWEEK                                                  
         MVC   SV_EKDAY,W_KDAY                                                  
         B     ADDREC                                                           
*                                                                               
PL90     DS    0H                                                               
         CLC   SV_SDATE,SV_EDATE   SPLIT CROSS DAY PROGRAM?                     
         BE    PL100                                                            
         MVC   W_PNRSQH,SV_SQHR    ON PROGRAM THAT CROSSES 5AM                  
         MVI   W_PNREQH,X'5F'      BOUNDARY, CREATE TWO PROGRAM                 
         ZIC   RE,W_PNREQH         LINEUP RECORD, ONE ENDING AT                 
         ZIC   R1,W_PNRSQH         445AM, ANOTHER STARTING AT 5AM               
         SR    RE,R1               AND ENDS AT THE PROGRAM END TIME             
         AHI   RE,1                                                             
         STCM  RE,3,W_PNRLQH                                                    
         LH    RE,TELEID                                                        
         SHI   RE,1                                                             
         EDIT  (RE),W_PNRSEQ,FILL=0                                             
         MVC   W_KDAY,SV_SKDAY                                                  
         MVC   W_KDATE,SV_SDATE                                                 
         MVC   W_KYEAR,SV_SYEAR                                                 
         MVC   W_KWEEK,SV_SWK#                                                  
         MVI   W_RECTYPE,RECTYPE_PNR                                            
         MVC   SV_SDATE,SV_EDATE   PREPARE FIELDS TO CREATE 2ND                 
         MVC   SV_SYEAR,SV_EYEAR   HALF OF THE PROGRAM.                         
         MVC   SV_SWK#,SV_EWK#                                                  
         MVC   SV_SKDAY,SV_EKDAY                                                
         MVI   SV_SQHR,0           5AM                                          
         B     ADDREC              ADD THE SPLITTED LINEUP                      
*                                                                               
PL100    MVC   W_PNRSQH,SV_SQHR                                                 
         MVC   W_PNREQH,SV_EQHR                                                 
         ZIC   RE,W_PNREQH                                                      
         ZIC   R1,W_PNRSQH                                                      
         SR    RE,R1                                                            
         AHI   RE,1                                                             
         STCM  RE,3,W_PNRLQH                                                    
         LH    RE,TELEID                                                        
         EDIT  (RE),W_PNRSEQ,FILL=0                                             
         MVC   W_KDAY,SV_SKDAY                                                  
         MVC   W_KDATE,SV_SDATE                                                 
         MVC   W_KYEAR,SV_SYEAR                                                 
         MVC   W_KWEEK,SV_SWK#                                                  
         MVI   W_RECTYPE,RECTYPE_PNR                                            
         B     KEEPREC             KEEP THE LINEUP                              
*                                                                               
************************************************************                    
KEEPREC  DS    0H                                                               
         BAS   RE,ACTIVEWK                                                      
         MVI   NEW_REC,C'Y'        TURN ON NEW RECORD INDICATOR                 
         LH    RE,TELEID                                                        
         AHI   RE,1                                                             
         STCM  RE,3,TELEID         BUMP TELECAST ID                             
         MVI   NEXT_REC,C'N'                                                    
         XC    SV_PARMS(SV_LENQ),SV_PARMS                                       
*                                                                               
KR10     DS    0H                                                               
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         SGR   GR1,GR1                                                          
         LR    R1,R3               SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LGHI  GRF,4               SET RC=4:  DELETE RECORD                     
         B     GOBACK                                                           
*                                                                               
ADDREC   DS    0H                                                               
         BAS   RE,ACTIVEWK                                                      
         SGR   GR1,GR1                                                          
         LR    R1,R3                                                            
         LGHI  GRF,12              SET RC=12: ADD RECORD                        
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
         LGHI  GRF,8               SET RC=8:EOF                                 
*                                                                               
GOBACK   DS    0H                                                               
         LMH   GR0,GR0,DFSORT_HIGH_HALVES                                       
         LMH   GR2,GRE,DFSORT_HIGH_HALVES+8                                     
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE REGS                                 
         BSM   0,RE                RETURN                                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
ACTIVEWK NTR1                                                                   
         MVC   W_KAWKS,=C'    '                                                 
         LA    R1,W_KAWKS                                                       
         ZIC   R2,W_KWEEK                                                       
         AR    R1,R2                                                            
         SHI   R1,1                                                             
         STC   R2,0(R1)                                                         
         OI    0(R1),X'F0'                                                      
         OI    W_KWEEK,X'F0'                                                    
*                                                                               
         MVC   W_KDAYMAP,=28C' '                                                
         LA    R1,W_KDAYMAP                                                     
         LA    R0,X'F0'                                                         
         ZIC   RE,W_KWEEK                                                       
         XR    RE,R0                                                            
         SHI   RE,1                                                             
         MHI   RE,7                                                             
         AR    R1,RE                                                            
         ZIC   RE,W_KDAY                                                        
         XR    RE,R0                                                            
         SHI   RE,1                                                             
         AR    R1,RE                                                            
*                                                                               
         LA    R2,DAYTAB           TRANSLATE DAY CODE                           
AW10     CLC   W_KDAY,0(R2)                                                     
         BE    AW20                                                             
         LA    R2,L'DAYTAB(R2)                                                  
         CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     AW10                                                             
AW20     MVC   0(L'W_KDAY,R1),1(R2)                                             
         MVC   W_KDAY,1(R2)                                                     
*                                                                               
AWX      XIT1                                                                   
DAYTAB   DS    0CL2                                                             
         DC    C'1',C'1'                                                        
         DC    C'2',C'2'                                                        
         DC    C'3',C'3'                                                        
         DC    C'4',C'4'                                                        
         DC    C'5',C'5'                                                        
         DC    C'6',C'7'                                                        
         DC    C'7',C'8'                                                        
         DC    X'FF'                                                            
*----------------------------------------------------------------------         
*----------------------------------------------------------------------         
*                                                                               
* CALL A DDS SUBROUTINE. THIS REQUIRES ESTABLISHING A NEW RD CHAIN,             
* BECAUSE THIS PROGRAM IS A DFSORT EXIT, AND DOES NOT CONFORM TO DDS            
* STANDARD REGISTER USAGE.                                                      
*                                                                               
* INPUT REGISTERS ARE STANDARD:                                                 
*   R1 = A(PARAMETER LIST)                                                      
*   RE = RETURN ADDRESS                                                         
*   RF = A(ROUTINE TO CALL)                                                     
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
CALL_DDS_SUBRTN DS 0H                                                           
         STM   RE,RC,GPRSAVE       SAVE CALLER'S REGISTERS                      
         LR    R0,RD               SAVE CALLER'S RD LOCALLY                     
         DROP  RC                                                               
*                                                                               
         BASR  RB,0                                                             
         AHI   RB,-2                                                            
         USING *-6,RB              MAKE THIS ROUTINE ADDRESSABLE                
         L     RD,=V(REGSAVE)      DDS WORKING STORAGE                          
*                                                                               
         BASR  RE,RF               CALL EXTERNAL SUBROUTINE                     
*                                                                               
         LR    RD,R0               RESTORE CALLER'S RD                          
         LM    RE,RC,GPRSAVE       RESTORE CALLER'S REGISTERS                   
         BSM   0,RE                EXIT                                         
         DROP  RB                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         ORG   DELMXR1+(((*-DELMXR1)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
GPRSAVE  DS    15F                 INTERNAL CALLER'S SAVED RE-RC                
DFSORT_HIGH_HALVES DS 16F                                                       
DMCB     DS    6F                  PARAMETERS TO CALL_DDS_SUBRTN                
HALF     DS    H                                                                
ELEM     DS    CL128                                                            
         SPACE 3                                                                
*                                                                               
WORK     DS    XL17                FOR EDIT                                     
DUB      DS    D                                                                
DUB2     DS    D                                                                
VADDAY   DS    V                                                                
VGETDAY  DS    V                                                                
VDATCON  DS    V                                                                
QHR      DS    XL1                                                              
SQHR     DS    XL1                                                              
EQHR     DS    XL1                                                              
SDATE    DS    XL6                                                              
EDATE    DS    XL6                                                              
DUR      DS    XL1                                                              
WK#      DS    CL1                                                              
PREV_SEQ DS    XL1                                                              
PREV_REC DS    XL2                                                              
PREV_STA DS    XL4                                                              
PREV_SAM DS    XL1                                                              
PREV_DAY DS    XL1                                                              
PREV_QHR DS    XL1                                                              
PREV_WK  DS    XL1                                                              
NEW_REC  DS    CL1                                                              
NEXT_REC DS    CL1                 DONE WITH THIS PROGRAM. GET ANOTHER          
TELEID   DS    H                                                                
*                                                                               
SV_PARMS DS    0H                                                               
SV_SDATE DS    XL6                                                              
SV_SYEAR DS    CL4                                                              
SV_SQHR  DS    XL1                                                              
SV_SWK#  DS    CL1                                                              
SV_SKDAY DS    XL1                                                              
*                                                                               
SV_EDATE DS    XL6                                                              
SV_EYEAR DS    CL4                                                              
SV_EQHR  DS    XL1                                                              
SV_EWK#  DS    CL1                                                              
SV_EKDAY DS    XL1                                                              
SV_LENQ  EQU   *-SV_PARMS                                                       
*                                                                               
PGM_SQHR DS    XL1                                                              
PGM_EQHR DS    XL1                                                              
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DELMDSECT                                                      
         PRINT ON                                                               
         EJECT                                                                  
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DELMXR1   05/08/17'                                      
         END                                                                    
