*          DATA SET NEGETUNIT  AT LEVEL 087 AS OF 05/01/02                      
*PHASE T00A19,+0                                                                
*INCLUDE MSUNPK                                                                 
*INCLUDE GETPROF                                                                
         TITLE 'T00A19 - NETWORK UNIT MONITOR'                                  
T00A19   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 10,**UNIT**                                                      
         USING MYD,R7                                                           
         LR    R7,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T21DFFD,RA                                                       
         SPACE 1                                                                
         L     R9,ANBLOCK                                                       
         USING NEBLOCKD,R9                                                      
         EJECT                                                                  
*              SELECT I/O ROUTINE                                               
         SPACE 3                                                                
         CLI   ESTWTS,0            COMPLETE WEIGHTED DEMO DETAILS               
         BE    NET4                                                             
         CLI   ESTWTS+1,0                                                       
         BNE   NET4                                                             
         LA    R2,ESTWTS+2                                                      
         LA    R3,13                                                            
         SR    R4,R4                                                            
         SPACE 2                                                                
NET2     ZIC   R1,1(R2)                                                         
         AR    R4,R1                                                            
         LA    R2,2(R2)                                                         
         BCT   R3,NET2                                                          
         STC   R4,ESTWTS+1                                                      
         SPACE 2                                                                
NET4     CLI   READMODE,C'I'                                                    
         BE    INIT                                                             
         MVC   NBESTOPT,EDEMOPT                                                 
         MVC   NBREVOPT,RDEMOPT                                                 
         MVC   NBACTOPT,ADEMOPT                                                 
         B     HANDLIO                                                          
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              INITIALIZATION ROUTINES - PROFILES                               
         SPACE 2                                                                
INIT     DS    0H                                                               
         XC    KEY,KEY             SET UP KEY FOR PROFILES                      
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+4(2),REP        (AGENCY)                                     
         MVI   KEY+6,C'N'                                                       
         MVI   ALLCLI,C'Y'                                                      
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),ALLOFF                                                 
         XC    EFFCLI,EFFCLI                                                    
         OC    BCLIENT,BCLIENT                                                  
         BZ    INITB                                                            
         MVI   ALLCLI,C'N'                                                      
         GOTO1 CLUNPK,DMCB,BCLIENT,KEY+7                                        
         MVC   KEY+11(1),CLTOFF                                                 
         SPACE 1                                                                
INITB    GOTO1 =V(GETPROF),DMCB,KEY,USERPROF,DATAMGR,RR=RB                      
         MVC   KEY+2(2),=C'00'     NOW GET THE SPOT PROFILES                    
         LA    R2,SPOTPROF                                                      
         ST    R2,DMCB+4                                                        
         BASR  RE,RF                                                            
         MVC   KEY+2(2),RCPROG+2   THEN THE SPECIFIC PROFILE FOR PROG           
         LA    R2,PROGPROF                                                      
         ST    R2,DMCB+4                                                        
         BASR  RE,RF                                                            
         SPACE 2                                                                
         LA    R2,USERPROF+2       DO WE NEED CALENDAR OR BROADCAST             
         CLI   DTMONTYP,C'A'       A/M                                          
         BE    *+8                                                              
         LA    R2,USERPROF+3                                                    
         MVC   DTMONTYP,0(R2)      BECOMES B/C                                  
         EJECT                                                                  
*              INITIALIZE NETBLOCK                                              
         SPACE 3                                                                
         MVC   NBSELAM,BAGYMED     SELECTION FIELDS                             
         MVC   NBSELCL2,BCLIENT                                                 
         MVC   NBSELPRD,BPRODUCT                                                
         GOTO1 =V(MSUNPK),DMCB,BMARKET,WORK,WORK+4,RR=RB                        
         MVC   NBSELNET,WORK+4                                                  
         MVC   NBSELEST,BEST                                                    
         MVC   NBSELESE,BESTEND                                                 
         MVC   NBSELPAK,BPACKAGE                                                
         SPACE 1                                                                
         MVC   NBUSER,USERPROF                                                  
         MVC   NBDEMOS,DEMOS                                                    
         MVC   NBDEMOS,NDEMOS                                                   
         LA    R1,IO                                                            
         ST    R1,NBAIO                                                         
         MVC   NBACOM,ACOMFACS                                                  
         EJECT                                                                  
*              INITIALIZATION ROUTINES - WEEKS                                  
         SPACE 3                                                                
INIT1    LA    R2,WEEKLIST         BUILD LIST OF WEEKS                          
         LA    R3,1                                                             
         XC    WEEKLIST,WEEKLIST                                                
         MVC   WORK(6),USERQSTR                                                 
         GOTO1 DATCON,DMCB,(0,USERQSTR),(2,(R2))                                
         GOTO1 GETDAY,DMCB,USERQSTR,DUB                                         
         ZIC   R5,DMCB                                                          
         IC    R6,USERPROF+4       START OF WEEK                                
         SLL   R6,28                                                            
         SRL   R6,28                                                            
         LTR   R6,R6                                                            
         BNZ   *+8                                                              
         LA    R6,1                                                             
         LA    R6,6(R6)            END OF WEEK 7-13                             
         SR    R6,R5                                                            
         CH    R6,=H'7'                                                         
         BE    INIT6                                                            
         BL    INIT4                                                            
         SH    R6,=H'7'                                                         
         B     INIT4                                                            
         SPACE 2                                                                
INIT2    LA    R6,6                                                             
         SPACE 2                                                                
INIT4    GOTO1 ADDAY,DMCB,WORK,WORK+6,(R6)                                      
         MVC   WORK(6),WORK+6                                                   
         SPACE 2                                                                
INIT6    CLC   WORK(6),USERQEND                                                 
         BL    INIT8                                                            
         SPACE 2                                                                
INIT7    GOTO1 DATCON,DMCB,(0,USERQEND),(2,2(R2))                               
         STC   R3,NWEEKS                                                        
         B     INIT10                                                           
         SPACE 2                                                                
INIT8    GOTO1 DATCON,DMCB,(0,WORK),(2,2(R2))                                   
         CH    R3,=H'16'                                                        
         BNE   INIT9                                                            
         CLI   DTSTATUS,C'N'                                                    
         BE    INIT7                                                            
         MVI   DTSTATUS,C'M'                                                    
         B     INIT20                                                           
         SPACE 2                                                                
INIT9    LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(2,(R2))                                    
         B     INIT2                                                            
         EJECT                                                                  
*              INITIALIZATION ROUTINES - MONTHS                                 
         SPACE 3                                                                
INIT10   LA    R2,MNTHLIST                                                      
         LA    R4,NMONTHS                                                       
         LA    R5,4                                                             
         XC    MNTHLIST,MNTHLIST                                                
         BAS   RE,MINT                                                          
         CLI   DTSTATUS,C'M'                                                    
         BE    INIT20                                                           
         B     INIT30                                                           
         SPACE 2                                                                
MINT     NTR1                                                                   
         LA    R3,1                                                             
         MVC   WORK(6),USERQSTR                                                 
         GOTO1 DATCON,DMCB,(0,USERQSTR),(2,(R2))                                
         CLI   DTMONTYP,C'B'                                                    
         BE    BR2                                                              
         SPACE 2                                                                
INIT12   MVC   WORK+4(2),=C'15'    BUMP TO NEXT MONTH                           
         GOTO1 ADDAY,DMCB,WORK,WORK+6,30                                        
         MVC   WORK+6+4(2),=C'01'                                               
         MVC   DMCB+8(4),=F'-1'    BACK UP TO END OF LAST MONTH                 
         GOTO1 ADDAY,DMCB,WORK+6,WORK                                           
         CLC   WORK(6),USERQEND                                                 
         BL    INIT16                                                           
         SPACE 2                                                                
INIT14   GOTO1 DATCON,DMCB,(0,USERQEND),(2,2(R2))                               
         STC   R3,0(R4)                                                         
         B     XIT                                                              
         SPACE 2                                                                
INIT16   GOTO1 DATCON,DMCB,(0,WORK),(2,2(R2))                                   
         CR    R3,R5                                                            
         BE    INIT14                                                           
         SPACE 2                                                                
INIT18   LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(2,(R2))                                    
         B     INIT12                                                           
         EJECT                                                                  
*              INITIALIZATION ROUTINES - MONTHS/QUARTERS                        
         SPACE 2                                                                
INIT20   DS    0H                  REQUESTED PERIOD TOO LONG                    
         LA    R2,WEEKLIST         SO BUILD MONTHS IN PLACE OF WEEKS            
         XC    WEEKLIST,WEEKLIST                                                
         LA    R4,NWEEKS                                                        
         LA    R5,16                                                            
         BAS   RE,MINT                                                          
         XC    MNTHLIST,MNTHLIST   AND QUARTERS INSTEAD OF MONTHS               
         LA    R2,WEEKLIST                                                      
         LA    R3,MNTHLIST                                                      
         LA    R4,4                                                             
         ZIC   R6,NWEEKS           (REALLY MONTHS)                              
         SR    R5,R5                                                            
         SPACE 2                                                                
INIT22   MVC   0(2,R3),0(R2)       START OF QUARTER                             
         LA    R5,1(R5)                                                         
         SPACE 2                                                                
INIT24   MVC   2(2,R3),2(R2)       ASSUME END                                   
         GOTO1 DATCON,DMCB,(2,2(R2)),(0,WORK)                                   
         CLC   WORK+2(2),=C'03'    QUARTERS END IN MARCH                        
         BE    INIT28                                                           
         CLC   WORK+2(2),=C'06'                    JUNE                         
         BE    INIT28                                                           
         CLC   WORK+2(2),=C'12'                    DEC                          
         BE    INIT28                                                           
         CLC   WORK+2(2),=C'09'                    AND SEP                      
         BNE   INIT26                                                           
         CH    R5,=H'1'            EXCEPT IF THIS IS FIRST QUARTER              
         BNE   INIT28                                                           
         CLC   USERQSTR+2(2),=C'09'                                             
         BNE   INIT28              AND IT STARTS IN SEP                         
         SPACE 2                                                                
INIT26   LA    R2,4(R2)                                                         
         BCT   R6,INIT24                                                        
         B     INIT29                                                           
         SPACE 2                                                                
INIT28   LA    R3,4(R3)                                                         
         LA    R2,4(R2)                                                         
         BCT   R4,INIT22                                                        
         SPACE 2                                                                
INIT29   STC   R5,NMONTHS          (N'QUARTERS)                                 
         SPACE 2                                                                
INIT30   LA    R4,KEY              SET UP KEY FOR AGENCY UNIVERSE               
         USING NUNKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   NUNKTYP,=X'0D22'                                                 
         MVC   NUNKAGY,REP                                                      
         GOTO1 DATCON,DMCB,USERQSTR,(2,NUNKEND)                                 
         GOTO1 NETUNIV,DMCB,(0,BLOCK),(R4),DATAMGR                              
         XC    LASTUNCD,LASTUNCD                                                
         CLI   NDEMOS,0            FORCE IN VIEWERS                             
         BNE   XIT                                                              
         MVI   NDEMOS,1                                                         
         MVI   ESTDEMOS+1,C'T'                                                  
         MVI   ESTDEMOS+2,127                                                   
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES - BROADCAST MONTHS                       
         SPACE 3                                                                
BR2      DS    0H                                                               
         GOTO1 GETDAY,DMCB,USERQSTR,DUB                                         
         ZIC   R1,DMCB                                                          
         IC    R6,USERPROF+4       START OF WEEK                                
         SLL   R6,28                                                            
         SRL   R6,28                                                            
         LTR   R6,R6                                                            
         BNZ   *+8                                                              
         LA    R6,1                                                             
         LA    R6,6(R6)            END OF WEEK 7-13                             
         SR    R6,R1                                                            
         CH    R6,=H'7'                                                         
         BE    BR4                                                              
         BL    *+8                                                              
         SH    R6,=H'7'                                                         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R6)                                      
         MVC   WORK(6),WORK+6      (SUNDAY OF FIRST WEEK)                       
         SPACE 2                                                                
BR4      GOTO1 DATCON,DMCB,(0,WORK),(2,2(R2))                                   
         CLC   WORK(6),USERQEND                                                 
         BNL   INIT14                                                           
         GOTO1 ADDAY,DMCB,WORK,WORK+6,7                                         
         CLC   WORK(4),WORK+6      IS THIS IN SAME BROADCAST MONTH              
         BNE   BR6                                                              
         MVC   WORK(6),WORK+6                                                   
         B     BR4                                                              
         SPACE 2                                                                
BR6      CR    R3,R5                                                            
         BE    INIT14                                                           
         GOTO1 ADDAY,DMCB,WORK,DUB,1                                            
         LA    R3,1(R3)                                                         
         LA    R2,4(R2)                                                         
         GOTO1 DATCON,DMCB,(0,DUB),(2,0(R2))                                    
         MVC   WORK(6),WORK+6                                                   
         B     BR4                                                              
         EJECT                                                                  
*              ROUTINES TO CONTROL NETIO AND NETVALUE                           
         SPACE 3                                                                
HANDLIO  GOTO1 VNETIO,DMCB,(R9)                                                 
         MVC   MODE,NBMODE                                                      
         CLI   NBMODE,NBPROCUN                                                  
         BE    UNIT2                                                            
         CLI   NBMODE,NBPROCPK                                                  
         BE    PACK2                                                            
         B     XIT                                                              
         SPACE 1                                                                
UNIT2    MVC   AFFDATE,NBACTDAT                                                 
         MVC   AFFTIME,NBAFFTIM                                                 
         MVC   PPPROGNO,NBNTI                                                   
         MVC   SPOTDATE,NBACTDAT                                                
         MVC   SPOTASS,NBASSIGN                                                 
         MVC   SPOTACT,NBACTUAL                                                 
         MVC   SPOTINT,NBINTEG                                                  
         MV1   SPOTTYPE,0                                                       
         OC    NBMGBDAT,NBMGBDAT                                                
         BZ    *+8                                                              
         OI    SPOTTYPE,X'02'                                                   
         OC    NBMGFDAT,NBMGFDAT                                                
         BZ    *+8                                                              
         OI    SPOTTYPE,X'01'                                                   
         MVC   SPOTPROG,NBPROGNM                                                
         MVC   SPOTIME,NBTIME                                                   
         MVC   SPOTPDAY,NBDAY                                                   
         MVC   SPOTPTIM,NBTIME                                                  
         MVC   SPOTPCOD,NBACTPRG                                                
         MVC   SPOTDP,NBACTDP                                                   
         MVC   SPOTPRD,NBPRD                                                    
         MVC   SPOTSHR,NBP1SHR                                                  
         MVC   SPOTPRD2,NBPRD2                                                  
         MVI   SPOTNPRD,1                                                       
         CLI   SPOTPRD2,0                                                       
         BE    *+8                                                              
         MVI   SPOTNPRD,2                                                       
         MVC   SPOTLEN,NBLEN                                                    
         MVC   SPOTMGNM,NBMGBNM                                                 
         MVC   SPOTMGDT,NBMGBDT                                                 
         MVC   SPOTEUN(6),NBESTUN                                               
         MVC   SPOTESPT,SPOTEUN                                                 
         MVC   SPOTIME                                                          
         OC    SPOTEUN,SPOTEUN                                                  
         BZ    UNIT4                                                            
         MVC   SPOTEACT,SPOTACT                                                 
         MVC   SPOTEASS,SPOTASS                                                 
         MVC   SPOTECUT,SPOTCUT                                                 
         MVC   SPOTEINT,SPOTINT                                                 
         SPACE 1                                                                
UNIT4    MVC   SPOTEHOM(120),NBESTHOM                                           
         MVC   SPOTAUN(6),NBACTUN                                               
         MVC   SPOTASPT,SPOTAUN                                                 
         OC    SPOTAUN,SPOTAUN                                                  
         BZ    UNIT6                                                            
         MVC   SPOTAACT,SPOTACT                                                 
         MVC   SPOTAASS,SPOTASS                                                 
         MVC   SPOTACUT,SPOTCUT                                                 
         MVC   SPOTAINT,SPOTINT                                                 
         SPACE 1                                                                
UNIT6    MVC   SPOTAHOM(120),NBACTHOM                                           
         MVI   SPOTTIMP,C'N'                                                    
         OC    NBPAYTGR,NBPAYTGR                                                
         BZ    *+8                                                              
         MVI   SPOTTIMP,C'Y'                                                    
         MVI   SPOTINTP,C'N'                                                    
         OC    NBPAYIGR,NBPAYIGR                                                
         BZ    *+8                                                              
         MVI   SPOTINTP,C'Y'                                                    
         MVI   SPOTTIMB,C'N'                                                    
         OC    NBBILTGR,NBBILTGR                                                
         BZ    *+8                                                              
         MVI   SPOTTIMB,C'Y'                                                    
         MVI   SPOTINTB,C'N'                                                    
         OC    NBBILIGR,NBBILIGR                                                
         BZ    *+8                                                              
         MVI   SPOTINTB,C'Y'                                                    
         MVC   SPOTACTD,NBDAYNAM                                                
         MVC   SPOTMSNM,NBMGFNAM                                                
         MVC   SPOTSIGN,NBMGFDAT                                                
         MVC   SPOTSTAT,NBUNITST                                                
         MVC   BUYIMPCT,NBIMPACT                                                
         MVC   BUYFEED,NBFEED                                                   
         MVC   BUYUNIV,NBUNIV                                                   
         MVC   EFFCLI,NBEFFCLI                                                  
         LA    R2,NWEEKS           LOOK UP WEEK                                 
         LA    R3,SPOTWEEK                                                      
         BAS   RE,SVALDT                                                        
         LA    R2,NMONTHS                  AND MONTH NUMBERS                    
         LA    R3,SPOTMNTH                                                      
         BAS   RE,SVALDT                                                        
         B     XIT                                                              
         SPACE 1                                                                
SVALDT   NTR1                                                                   
         LA    R1,1                ROUTINE TO ESTABLISH DATE NUMBER             
         ZIC   R0,0(R2)                                                         
         LA    R2,1(R2)                                                         
         SPACE 2                                                                
SVALDT2  CLC   SPOTDATE,2(R2)                                                   
         BH    SVALDT4                                                          
         STC   R1,0(R3)                                                         
         B     XIT                                                              
         EJECT                                                                  
*              TRANSLATE PACKAGE FIELDS                                         
         SPACE 3                                                                
PACK2    MVC   PACKNAME,NBPAKNAM                                                
         MVC   PACKDP,NBACTDP                                                   
         MVC   PACKDPNM,NBDPNAM                                                 
         MVC   PACKCOST,NBPAKCST                                                
         MVC   PACKSTAT,NBPACKST                                                
         MVC   PACKNUM,NBPACK                                                   
         B     XIT                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         B     *+6                                                              
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
MYD      DSECT                                                                  
NETIO    DS    V                                                                
         PRINT OFF                                                              
DUMMYD   DSECT                                                                  
       ++INCLUDE SPSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE SPGENUNIV                                                      
         PRINT ON                                                               
NEBLOCKD DSECT                                                                  
       ++INCLUDE NEBLOCK                                                        
         SPACE 2                                                                
T21DFFD  DSECT                                                                  
         DS    CL64                                                             
CONHEADH DS    0C                                                               
       ++INCLUDE SPSPLTWAD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087NEGETUNIT 05/01/02'                                      
         END                                                                    
