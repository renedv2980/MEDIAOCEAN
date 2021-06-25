*          DATA SET NEGOL04    AT LEVEL 004 AS OF 08/24/10                      
*PHASE T31404A                                                                  
                                                                                
***********************************************************************         
* Brand Allocation Record                                                       
***********************************************************************         
                                                                                
         TITLE 'NEGOL04 -  BARULES RECORD'                                      
         PRINT NOGEN                                                            
T31404   CSECT                                                                  
         NMOD1 0,T31404                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING T31404,RB,R6                                                     
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTOR VSETSPT                                                          
                                                                                
         CLI   MODE,VALKEY         Validate key                                 
         JE    VK                                                               
         CLI   MODE,VALREC         Validate record                              
         JE    VR                                                               
         CLI   MODE,DISPKEY        Display key                                  
         JE    DK                                                               
         CLI   MODE,DISPREC        Display record                               
         JE    DR                                                               
         CLI   MODE,LISTRECS       List record                                  
         JE    LR                                                               
                                                                                
EXIT     GOTOR VSETSPT                                                          
         XIT1                                                                   
                                                                                
***********************************************************************         
* Validate key                                                        *         
***********************************************************************         
                                                                                
VK       XC    TMPFLDH(TMPFLDLQ),TMPFLDH                                        
                                                                                
         MVI   TMPFLDH,9           Validate media                               
         MVI   TMPFLDH+5,1                                                      
         MVI   TMPFLD,C'N'                                                      
         LA    R2,TMPFLDH                                                       
         GOTOR VALIMED                                                          
                                                                                
         XC    BCLT,BCLT           Default to agency level                      
         CLI   ACTNUM,ACTLIST      Test list                                    
         JE    VK02                                                             
                                                                                
         LA    R2,BRUCLIH          Validate client                              
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         CLC   =C'***',8(R2)       Test agency level                            
         JE    VK02                                                             
         GOTOR VALICLT                                                          
                                                                                
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING BRULESD,R4                                                       
                                                                                
         MVI   BRULKTYP,BRULKTYQ                                                
         MVI   BRULKSTY,BRULKSTQ                                                
         MVC   BRULAGY,BAGYMD                                                   
         GOTOR HIGH                                                             
         CLC   KEY(L'BRULKEY),KEYSAVE  Test agency level record exists          
         JNE   AGYERR                                                           
                                                                                
VK02     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING BRULESD,R4                                                       
                                                                                
         MVI   BRULKTYP,BRULKTYQ                                                
         MVI   BRULKSTY,BRULKSTQ                                                
         MVC   BRULAGY,BAGYMD                                                   
         MVC   BRULCLT,BCLT                                                     
         DROP  R4                                                               
                                                                                
VKX      MVC   SAVEKEY,KEY                                                      
         J     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* Validate record                                                     *         
***********************************************************************         
                                                                                
VR       XC    SVVALS(SVVALSL),SVVALS                                           
                                                                                
         L     R4,AIO                                                           
         USING BRULESD,R4                                                       
         MVC   BRULKEY,KEY         Build record from scratch                    
         MVC   BRULAGYA,AGENCY                                                  
         MVC   BRULRLEN,=X'0018'                                                
         DROP  R4                                                               
                                                                                
         GOTOR HELLO,DMCB,(C'D',=C'SPTFILE'),(X'01',AIO)                        
         GOTOR HELLO,DMCB,(C'D',=C'SPTFILE'),(X'02',AIO)                        
                                                                                
         LA    R2,BRUUADH          Use assigned dollars                         
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         MVI   SVUAD,NOQ                                                        
         CLI   BRUUAD,NOQ                                                       
         JE    VR02                                                             
         CLI   BRUUAD,YESQ                                                      
         JNE   INVLFLD                                                          
         MVC   SVUAD,BRUUAD                                                     
                                                                                
VR02     LA    R2,BRURSDH          Round split dollars                          
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         CLI   BRURSD,C'P'         Test round to nearest penny                  
         JE    *+12                                                             
         CLI   BRURSD,C'$'         Test round to nearest dollar                 
         JNE   INVLFLD                                                          
         MVC   SVRSD,8(R2)                                                      
                                                                                
         LA    R2,BRUDWPH          Dollar weight percentage                     
         BAS   RE,GETHEX                                                        
         BAS   RE,TSTINCR                                                       
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'100'                                                     
         JH    INVLFLD                                                          
         MVC   SVDWP,FULL                                                       
                                                                                
         LA    R2,BRUNTPH          Network percentage                           
         BAS   RE,GETHEX                                                        
         BAS   RE,TSTINCR                                                       
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'100'                                                     
         JH    INVLFLD                                                          
         MVC   SVNTP,FULL                                                       
                                                                                
         LA    R2,BRUPGPH          Program percentage                           
         BAS   RE,GETHEX                                                        
         BAS   RE,TSTINCR                                                       
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'100'                                                     
         JH    INVLFLD                                                          
         MVC   SVPGP,FULL                                                       
                                                                                
         LA    R2,BRUTBLH          Total budget lower                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR04                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR04     MVC   SVTBL,FULL                                                       
                                                                                
         LA    R2,BRUTBUH          Total budget upper                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR06                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR06     MVC   SVTBU,FULL                                                       
                                                                                
         CLC   SVTBL,SVTBU         Test lower < upper                           
         JH    UPPERR                                                           
                                                                                
         LA    R2,BRUGBLH          Total goal lower                             
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
         MVC   SVGBL,FULL                                                       
                                                                                
         LA    R2,BRUGBUH          Total goal upper                             
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR08                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR08     MVC   SVGBU,FULL                                                       
                                                                                
         CLC   SVGBL,SVGBU         Test lower < upper                           
         JH    UPPERR                                                           
                                                                                
         LA    R2,BRUWBLH          Total weekly lower                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
         MVC   SVWBL,FULL                                                       
                                                                                
         LA    R2,BRUWBUH          Total weekly upper                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR10                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR10     MVC   SVWBU,FULL                                                       
                                                                                
         CLC   SVWBL,SVWBU         Test lower < upper                           
         JH    UPPERR                                                           
                                                                                
         LA    R2,BRUIZUH          Include $0 units                             
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         MVI   SVIZU,NOQ                                                        
         CLI   BRUIZU,NOQ                                                       
         JE    VR12                                                             
         CLI   BRUIZU,YESQ                                                      
         JNE   INVLFLD                                                          
         MVC   SVIZU,BRUIZU                                                     
                                                                                
VR12     LA    R2,BRUDW1H          Demo target weight percentage 1              
         BAS   RE,GETHEX                                                        
         BAS   RE,TSTINCR                                                       
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'100'                                                     
         JH    INVLFLD                                                          
         MVC   SVDW1,FULL                                                       
                                                                                
         LA    R2,BRUDW2H          Demo target weight percentage 2              
         BAS   RE,GETHEX                                                        
         BAS   RE,TSTINCR                                                       
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'100'                                                     
         JH    INVLFLD                                                          
         MVC   SVDW2,FULL                                                       
                                                                                
         LA    R2,BRUDW3H          Demo target weight percentage 3              
         BAS   RE,GETHEX                                                        
         BAS   RE,TSTINCR                                                       
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'100'                                                     
         JH    INVLFLD                                                          
         MVC   SVDW3,FULL                                                       
                                                                                
         ICM   R3,15,SVDWP                                                      
         ICM   RF,15,SVNTP                                                      
         AR    R3,RF                                                            
         ICM   RF,15,SVPGP                                                      
         AR    R3,RF                                                            
         ICM   RF,15,SVDW1                                                      
         AR    R3,RF                                                            
         ICM   RF,15,SVDW2                                                      
         AR    R3,RF                                                            
         ICM   RF,15,SVDW3                                                      
         AR    R3,RF                                                            
         C     R3,=F'100'          Test $weight + demo1 weight +                
         JNE   WGHTERR             demo2 weight + demo3 weight = 100            
                                                                                
         LA    R2,BRUTL1H          Total demo lower 1                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
         MVC   SVTL1,FULL                                                       
                                                                                
         LA    R2,BRUTU1H          Total demo upper 1                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR14                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR14     MVC   SVTU1,FULL                                                       
                                                                                
         CLC   SVTL1,SVTU1         Test lower < upper                           
         JH    UPPERR                                                           
                                                                                
         LA    R2,BRUTL2H          Total demo lower 2                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
         MVC   SVTL2,FULL                                                       
                                                                                
         LA    R2,BRUTU2H          Total demo upper 2                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR16                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR16     MVC   SVTU2,FULL                                                       
                                                                                
         CLC   SVTL2,SVTU2         Test lower < upper                           
         JH    UPPERR                                                           
                                                                                
         LA    R2,BRUTL3H          Total demo lower 3                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
         MVC   SVTL3,FULL                                                       
                                                                                
         LA    R2,BRUTU3H          Total demo upper 3                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR18                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR18     MVC   SVTU3,FULL                                                       
                                                                                
         CLC   SVTL3,SVTU3         Test lower < upper                           
         JH    UPPERR                                                           
                                                                                
         LA    R2,BRUBL1H          Brand goal lower 1                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
         MVC   SVBL1,FULL                                                       
                                                                                
         LA    R2,BRUBU1H          Brand goal upper 1                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR20                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR20     MVC   SVBU1,FULL                                                       
                                                                                
         CLC   SVBL1,SVBU1         Test lower < upper                           
         JH    UPPERR                                                           
                                                                                
         LA    R2,BRUBL2H          Brand goal lower 2                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
         MVC   SVBL2,FULL                                                       
                                                                                
         LA    R2,BRUBU2H          Brand goal upper 2                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR22                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR22     MVC   SVBU2,FULL                                                       
                                                                                
         CLC   SVBL2,SVBU2         Test lower < upper                           
         JH    UPPERR                                                           
                                                                                
         LA    R2,BRUBL3H          Brand goal lower 3                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
         MVC   SVBL3,FULL                                                       
                                                                                
         LA    R2,BRUBU3H          Brand goal upper 3                           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR24                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR24     MVC   SVBU3,FULL                                                       
                                                                                
         CLC   SVBL3,SVBU3         Test lower < upper                           
         JH    UPPERR                                                           
                                                                                
         LA    R2,BRUWL1H          Weekly brand lower 1                         
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
         MVC   SVWL1,FULL                                                       
                                                                                
         LA    R2,BRUWU1H          Weekly brand upper 1                         
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR26                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR26     MVC   SVWU1,FULL                                                       
                                                                                
         CLC   SVWL1,SVWU1         Test lower < upper                           
         JH    UPPERR                                                           
                                                                                
         LA    R2,BRUWL2H          Weekly brand lower 2                         
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
         MVC   SVWL2,FULL                                                       
                                                                                
         LA    R2,BRUWU2H          Weekly brand upper 2                         
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR28                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR28     MVC   SVWU2,FULL                                                       
                                                                                
         CLC   SVWL2,SVWU2         Test lower < upper                           
         JH    UPPERR                                                           
                                                                                
         LA    R2,BRUWL3H          Weekly brand lower 3                         
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
         MVC   SVWL3,FULL                                                       
                                                                                
         LA    R2,BRUWU3H          Weekly brand upper 3                         
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR30                                                             
         CLC   FULL,=F'0'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'999'                                                     
         JH    INVLFLD                                                          
VR30     MVC   SVWU3,FULL                                                       
                                                                                
         CLC   SVWL3,SVWU3         Test lower < upper                           
         JH    UPPERR                                                           
                                                                                
         LA    R2,BRUNTIH          Network - time relaxation interval           
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test valid interval                          
         JE    VR32                                                             
         CLC   FULL,=F'30'                                                      
         JE    VR32                                                             
         CLC   FULL,=F'60'                                                      
         JE    VR32                                                             
         CLC   FULL,=F'90'                                                      
         JE    VR32                                                             
         CLC   FULL,=F'120'                                                     
         JNE   INVLFLD                                                          
VR32     MVC   SVNTI,FULL                                                       
                                                                                
         LA    R2,BRUCTIH          Cable - time relaxation interval             
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'          Test valid interval                          
         JE    VR34                                                             
         CLC   FULL,=F'30'                                                      
         JE    VR34                                                             
         CLC   FULL,=F'60'                                                      
         JE    VR34                                                             
         CLC   FULL,=F'90'                                                      
         JE    VR34                                                             
         CLC   FULL,=F'120'                                                     
         JNE   INVLFLD                                                          
VR34     MVC   SVCTI,FULL                                                       
                                                                                
         LA    R2,BRUMDSH          Maximum double spotting allowed              
         BAS   RE,GETHEX                                                        
         CLC   FULL,=F'0'                                                       
         JE    VR36                                                             
         CLC   FULL,=F'1'          Test range                                   
         JL    INVLFLD                                                          
         CLC   FULL,=F'50'                                                      
         JH    INVLFLD                                                          
VR36     MVC   SVMDS,FULL                                                       
                                                                                
         L     R2,AIO              Add elements to record                       
         USING BRULESD,R2                                                       
         MVI   BR01ID,BR01IDQ                                                   
         MVI   BR01LN,BR01LNQN                                                  
         GOTOR DATCON,DMCB,(5,0),(2,BR01ACT)                                    
         MVC   BR01UAD,SVUAD                                                    
         MVC   BR01RSD,SVRSD                                                    
         MVC   BR01DWP,SVDWP                                                    
         MVC   BR01NTP,SVNTP                                                    
         MVC   BR01PGP,SVPGP                                                    
         MVC   BR01TBL,SVTBL                                                    
         MVC   BR01TBU,SVTBU                                                    
         MVC   BR01GBL,SVGBL                                                    
         MVC   BR01GBU,SVGBU                                                    
         MVC   BR01WBL,SVWBL                                                    
         MVC   BR01WBU,SVWBU                                                    
         MVC   BR01IZU,SVIZU                                                    
         MVC   BR01NTI,SVNTI                                                    
         MVC   BR01CTI,SVCTI                                                    
         MVC   BR01MDS,SVMDS                                                    
         DROP  R2                                                               
                                                                                
         GOTOR HELLO,DMCB,(C'P',=C'SPTFILE'),(X'01',AIO),ELEM,0                 
                                                                                
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         USING BDEMD,R2                                                         
         MVI   BDEMID,BDEMIDQ                                                   
         MVI   BDEMLN,BDEMLNQ                                                   
         MVI   BDEMNUM,1                                                        
         MVC   BDEMTWP,SVDW1                                                    
         MVC   BDEMTDL,SVTL1                                                    
         MVC   BDEMTDU,SVTU1                                                    
         MVC   BDEMBGL,SVBL1                                                    
         MVC   BDEMBGU,SVBU1                                                    
         MVC   BDEMWBL,SVWL1                                                    
         MVC   BDEMWBU,SVWU1                                                    
         DROP  R2                                                               
                                                                                
         GOTOR HELLO,DMCB,(C'P',=C'SPTFILE'),(X'02',AIO),ELEM,0                 
                                                                                
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         USING BDEMD,R2                                                         
         MVI   BDEMID,BDEMIDQ                                                   
         MVI   BDEMLN,BDEMLNQ                                                   
         MVI   BDEMNUM,2                                                        
         MVC   BDEMTWP,SVDW2                                                    
         MVC   BDEMTDL,SVTL2                                                    
         MVC   BDEMTDU,SVTU2                                                    
         MVC   BDEMBGL,SVBL2                                                    
         MVC   BDEMBGU,SVBU2                                                    
         MVC   BDEMWBL,SVWL2                                                    
         MVC   BDEMWBU,SVWU2                                                    
         DROP  R2                                                               
                                                                                
         GOTOR HELLO,DMCB,(C'P',=C'SPTFILE'),(X'02',AIO),ELEM,0                 
                                                                                
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         USING BDEMD,R2                                                         
         MVI   BDEMID,BDEMIDQ                                                   
         MVI   BDEMLN,BDEMLNQ                                                   
         MVI   BDEMNUM,3                                                        
         MVC   BDEMTWP,SVDW3                                                    
         MVC   BDEMTDL,SVTL3                                                    
         MVC   BDEMTDU,SVTU3                                                    
         MVC   BDEMBGL,SVBL3                                                    
         MVC   BDEMBGU,SVBU3                                                    
         MVC   BDEMWBL,SVWL3                                                    
         MVC   BDEMWBU,SVWU3                                                    
         DROP  R2                                                               
                                                                                
         GOTOR HELLO,DMCB,(C'P',=C'SPTFILE'),(X'02',AIO),ELEM,0                 
                                                                                
VRX      J     DR                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Display record                                                      *         
***********************************************************************         
                                                                                
DR       L     R4,AIO                                                           
         USING BRULESD,R4                                                       
                                                                                
         MVC   BRUUAD,BR01UAD                                                   
         OI    BRUUADH+6,X'80'                                                  
                                                                                
         MVC   BRURSD,BR01RSD                                                   
         OI    BRURSDH+6,X'80'                                                  
                                                                                
         EDIT  BR01DWP,BRUDWP,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUDWPH+6,X'80'                                                  
                                                                                
         CLI   BR01LN,BR01LNQ      Test old style                               
         JE    DR02                                                             
         EDIT  BR01NTP,BRUNTP,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUNTPH+6,X'80'                                                  
                                                                                
         EDIT  BR01PGP,BRUPGP,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUPGPH+6,X'80'                                                  
                                                                                
DR02     EDIT  BR01TBL,BRUTBL,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUTBLH+6,X'80'                                                  
                                                                                
         EDIT  BR01TBU,BRUTBU,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUTBUH+6,X'80'                                                  
                                                                                
         EDIT  BR01GBL,BRUGBL,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUGBLH+6,X'80'                                                  
                                                                                
         EDIT  BR01GBU,BRUGBU,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUGBUH+6,X'80'                                                  
                                                                                
         EDIT  BR01WBL,BRUWBL,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUWBLH+6,X'80'                                                  
                                                                                
         EDIT  BR01WBU,BRUWBU,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUWBUH+6,X'80'                                                  
                                                                                
         MVC   BRUIZU,BR01IZU                                                   
         OI    BRUIZUH+6,X'80'                                                  
                                                                                
         EDIT  BR01NTI,BRUNTI,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUNTIH+6,X'80'                                                  
                                                                                
         EDIT  BR01CTI,BRUCTI,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUCTIH+6,X'80'                                                  
                                                                                
         EDIT  BR01MDS,BRUMDS,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUMDSH+6,X'80'                                                  
         DROP  R4                                                               
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,BDEMIDQ                                                   
         BAS   RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING BDEMD,R4                                                         
                                                                                
         EDIT  BDEMTWP,BRUDW1,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUDW1H+6,X'80'                                                  
                                                                                
         EDIT  BDEMTDL,BRUTL1,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUTL1H+6,X'80'                                                  
                                                                                
         EDIT  BDEMTDU,BRUTU1,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUTU1H+6,X'80'                                                  
                                                                                
         EDIT  BDEMBGL,BRUBL1,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUBL1H+6,X'80'                                                  
                                                                                
         EDIT  BDEMBGU,BRUBU1,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUBU1H+6,X'80'                                                  
                                                                                
         EDIT  BDEMWBL,BRUWL1,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUWL1H+6,X'80'                                                  
                                                                                
         EDIT  BDEMWBU,BRUWU1,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUWU1H+6,X'80'                                                  
                                                                                
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         CLI   0(R4),BDEMIDQ                                                    
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         EDIT  BDEMTWP,BRUDW2,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUDW2H+6,X'80'                                                  
                                                                                
         EDIT  BDEMTDL,BRUTL2,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUTL2H+6,X'80'                                                  
                                                                                
         EDIT  BDEMTDU,BRUTU2,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUTU2H+6,X'80'                                                  
                                                                                
         EDIT  BDEMBGL,BRUBL2,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUBL2H+6,X'80'                                                  
                                                                                
         EDIT  BDEMBGU,BRUBU2,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUBU2H+6,X'80'                                                  
                                                                                
         EDIT  BDEMWBL,BRUWL2,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUWL2H+6,X'80'                                                  
                                                                                
         EDIT  BDEMWBU,BRUWU2,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUWU2H+6,X'80'                                                  
                                                                                
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         CLI   0(R4),BDEMIDQ                                                    
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         EDIT  BDEMTWP,BRUDW3,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUDW3H+6,X'80'                                                  
                                                                                
         EDIT  BDEMTDL,BRUTL3,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUTL3H+6,X'80'                                                  
                                                                                
         EDIT  BDEMTDU,BRUTU3,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUTU3H+6,X'80'                                                  
                                                                                
         EDIT  BDEMBGL,BRUBL3,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUBL3H+6,X'80'                                                  
                                                                                
         EDIT  BDEMBGU,BRUBU3,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUBU3H+6,X'80'                                                  
                                                                                
         EDIT  BDEMWBL,BRUWL3,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUWL3H+6,X'80'                                                  
                                                                                
         EDIT  BDEMWBU,BRUWU3,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    BRUWU3H+6,X'80'                                                  
         DROP  R4                                                               
                                                                                
DRX      J     EXIT                                                             
         EJECT                                                                  
                                                                                
DK       DS    0H                                                               
         L     R4,AIO                                                           
         USING BRULESD,R4                                                       
         OI    BRUCLIH+6,X'80'                                                  
         OC    BRULCLT,BRULCLT     Test agency level                            
         JNZ   *+14                                                             
         MVC   BRUCLI,=C'***'                                                   
         J     EXIT                                                             
                                                                                
         GOTOR CLUNPK,DMCB,BRULCLT,BRUCLI                                       
DKX      J     EXIT                                                             
                                                                                
LR       DS    0H                                                               
         MVI   NLISTS,15                                                        
                                                                                
         OC    KEY,KEY                                                          
         JNZ   *+10                                                             
         MVC   KEY,SAVEKEY                                                      
         GOTOR HIGH                                                             
         J     LR02                                                             
                                                                                
LRSEQ    GOTOR SEQ                                                              
LR02     LA    R4,KEY                                                           
         USING BRULESD,R4                                                       
                                                                                
         CLI   BRULKTYP,BRULKTYQ                                                
         JNE   EXIT                                                             
         CLI   BRULKSTY,BRULKSTQ                                                
         JNE   EXIT                                                             
                                                                                
         CLC   KEY(BRULCLT-BRULKEY),KEYSAVE                                     
         JNE   EXIT                                                             
                                                                                
         GOTOR GETREC                                                           
                                                                                
         L     R4,AIO                                                           
                                                                                
         LA    R5,LISTAR                                                        
         OC    BRULCLT,BRULCLT     Test agency level                            
         JNZ   *+14                                                             
         MVC   0(3,R5),=C'***'                                                  
         J     LR04                                                             
                                                                                
         GOTOR CLUNPK,DMCB,BRULCLT,0(R5)                                        
LR04     GOTOR LISTMON                                                          
         J     LRSEQ                                                            
                                                                                
LRX      J     EXIT                                                             
                                                                                
***********************************************************************         
* Get hex value of field                                              *         
* On entry - R2 points to field to validate on screen                 *         
* On exit  - FULL = value in hex                                      *         
***********************************************************************         
                                                                                
GETHEX   NTR1                                                                   
         XC    FULL,FULL                                                        
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         TM    1(R2),X'10'         Test numeric                                 
         JZ    INVLFLD                                                          
                                                                                
         CLI   8(R2),C'.'          Test decimal entered                         
         JE    INVLFLD                                                          
         CLI   9(R2),C'.'                                                       
         JE    INVLFLD                                                          
         CLI   10(R2),C'.'                                                      
         JE    INVLFLD                                                          
                                                                                
         ZIC   R1,5(R2)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
         ST    R3,FULL                                                          
         J     EXIT                                                             
                                                                                
TSTRNG   NTR1                                                                   
         CLI   FULL+3,0            Test between 0 and 100                       
         JL    INVLFLD                                                          
         CLI   FULL+3,100                                                       
         JH    INVLFLD                                                          
         J     EXIT                                                             
         EJECT                                                                  
                                                                                
TSTINCR  NTR1                                                                   
         ZIC   R1,5(R2)                                                         
         SHI   R1,1                                                             
         LA    R3,8(R2)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),C'0'          Test in 5% increments                        
         JE    *+12                                                             
         CLI   0(R3),C'5'                                                       
         JNE   INVLFLD                                                          
         J     EXIT                                                             
                                                                                
                                                                                
         GETEL R4,24,ELCODE                                                     
                                                                                
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
                                                                                
MISSERR  MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
                                                                                
TRAPERR  GOTOR ERREX                                                            
                                                                                
INVLFLD  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(10),=C'*Error* - '                                       
         MVC   CONHEAD+10(32),=C'Input is not an acceptable value'              
         FOUT  CONHEADH                                                         
         GOTOR ERREX2                                                           
                                                                                
UPPERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(10),=C'*Error* - '                                       
         MVC   CONHEAD+10(32),=C'Upper bound is less than lower  '              
         FOUT  CONHEADH                                                         
         GOTOR ERREX2                                                           
                                                                                
AGYERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(10),=C'*Error* - '                                       
         MVC   CONHEAD+10(33),=C'Agency level record must be added'             
         FOUT  CONHEADH                                                         
         GOTOR ERREX2                                                           
                                                                                
WGHTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(10),=C'*Error* - '                                       
         MVC   CONHEAD+10(24),=C'Weights must add to 100%'                      
         FOUT  CONHEADH                                                         
         LA    R2,BRUDWPH                                                       
         GOTOR ERREX2                                                           
                                                                                
         EJECT                                                                  
       ++INCLUDE SPGENBARU                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NEGOLFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEGOLF5D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEGOLF6D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE NEGENUSER                                                      
       ++INCLUDE NEGOLWORK                                                      
         ORG   SYSSPARE                                                         
                                                                                
*        Work Area                                                              
                                                                                
WORKAREA DS    0CL500                                                           
SAVEKEY  DS    XL13                                                             
                                                                                
TMPFLDH  DS    XL8                                                              
TMPFLD   DS    XL1                                                              
TMPFLDLQ EQU   *-TMPFLDH                                                        
                                                                                
SVVALS   DS    0X                  ** Saved record values **                    
SVUAD    DS    C                                                                
SVRSD    DS    C                                                                
SVDWP    DS    XL4                                                              
SVNTP    DS    XL4                                                              
SVPGP    DS    XL4                                                              
SVTBL    DS    XL4                                                              
SVTBU    DS    XL4                                                              
SVGBL    DS    XL4                                                              
SVGBU    DS    XL4                                                              
SVWBL    DS    XL4                                                              
SVWBU    DS    XL4                                                              
SVIZU    DS    C                                                                
SVDW1    DS    XL4                                                              
SVDW2    DS    XL4                                                              
SVDW3    DS    XL4                                                              
SVTL1    DS    XL4                                                              
SVTU1    DS    XL4                                                              
SVBL1    DS    XL4                                                              
SVBU1    DS    XL4                                                              
SVWL1    DS    XL4                                                              
SVWU1    DS    XL4                                                              
SVTL2    DS    XL4                                                              
SVTU2    DS    XL4                                                              
SVBL2    DS    XL4                                                              
SVBU2    DS    XL4                                                              
SVWL2    DS    XL4                                                              
SVWU2    DS    XL4                                                              
SVTL3    DS    XL4                                                              
SVTU3    DS    XL4                                                              
SVBL3    DS    XL4                                                              
SVBU3    DS    XL4                                                              
SVWL3    DS    XL4                                                              
SVWU3    DS    XL4                                                              
SVNTI    DS    XL4                                                              
SVCTI    DS    XL4                                                              
SVMDS    DS    XL4                                                              
SVVALSL  EQU   *-SVVALS                                                         
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NEGOL04   08/24/10'                                      
         END                                                                    
